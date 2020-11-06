package apigen

import cats.data.{ NonEmptyList, State }
import cats.instances.list._
import cats.syntax.all._
import higherkindness.droste._
import higherkindness.droste.data.Fix
import higherkindness.droste.syntax.all._
import higherkindness.skeuomorph.openapi.JsonSchemaF
import org.http4s.HttpRoutes

import scala.meta._

object SchemaBuilder {
  import Utils._

  /**
   * This type represent a function that converts Path into a Type, while producing a Type Definition Registry
   * It takes in Path as input, because we want to build Path when traverse the tree from top to bottom,
   * but we only want to collapse from bottom to top, making it a function allow us to attach information
   * from top to bottom.
   * The intuition is that by making it a function, the caller will provide the path, and the caller is actually
   * the layer above, which knows more about the path, and recursively this will reach root, which knows how things start
   *
   * It returns a State monad because we want to capture both state and a return Type
   */
  type CodeBuilder = NonEmptyList[SchemaPath] => State[TypeDefRegistry, Type]

  /**
   * This ADT models the path of an arbitrary object in OpenApi's schema
   * Normally the top level path is always a NewType, that is the name of the schema
   *
   */
  sealed trait SchemaPath
  case class NewType(typeName: String)                                extends SchemaPath
  case class FieldPath(fieldName: String)                             extends SchemaPath
  case object ArrayItem                                               extends SchemaPath
  case class CoproductBranch(coproductName: String, branchId: String) extends SchemaPath

  def primitiveTreeBuilder(primTpe: Type): CodeBuilder =
    path =>
      path.last match {
        case NewType(newTypeName) =>
          val tpeName = Type.Name(newTypeName)
          addTypeDef(tpeName, q"type $tpeName = $primTpe").map(_ => primTpe)
        case _: FieldPath | ArrayItem | _: CoproductBranch => State.pure(primTpe)
      }

  /**
   * High level use case:
   * Given a Json Schema, produce corresponding scala model code (as scala.meta.Tree)
   * There are 2 types of output:
   * 1. Type references, eg. `Int`, `MyTrait` etc, these are references that can be use as type
   * 2. Type definition, eg. `case class MyClass(...)` or `sealed trait ADT`, these are primarily Type definition, but they also
   *    provides Type references for free
   *
   * All primitives and types from Std Library are type references, as they are implemented by language or std library, we only
   * need to define custom type (ie. Sum type and Product type)
   *
   * In each we layer, we need to produce a Type Reference, and potentially a Type Definition for custom type,
   * Type reference is needed to compose types into composite type, eg. A + B = C
   * Type Definition is needed to support Type reference
   *
   * This is modeled by `State[Map[Type, Tree], Type]`, the return type `Type` is the type reference, and `Map[Type, Tree]`
   * capture all type definitions required
   *
   * However, our Algebra cannot simple return `State[Map[Type, Tree], Type]`, because it does not have
   * enough information to produce type reference and definition for all cases:
   *
   * The issue is mainly because we wish to express schema in idiomatic Scala, ie. case class for Product and sealed trait
   * for Sum type, both of these type can only be defined with a name, which comes from the context. Thus our algebra
   * produces a function `TypeContext => State[Map[Type, Tree], Type]`
   *
   * Note it is possible to make type not to depends on outer context by modeling them using Shapeless Record and Coproduct
   * I decided not to, we can consider it if we move to Scala 3
   *
   */
  val schemaToTreeBuiler: Algebra[JsonSchemaF, CodeBuilder] =
    Algebra[JsonSchemaF, CodeBuilder] {
      case JsonSchemaF.IntegerF()  => primitiveTreeBuilder(t"Int")
      case JsonSchemaF.LongF()     => primitiveTreeBuilder(t"Long")
      case JsonSchemaF.FloatF()    => primitiveTreeBuilder(t"Float")
      case JsonSchemaF.DoubleF()   => primitiveTreeBuilder(t"Double")
      case JsonSchemaF.StringF()   => primitiveTreeBuilder(t"String")
      case JsonSchemaF.ByteF()     => primitiveTreeBuilder(t"Byte")
      case JsonSchemaF.BinaryF()   => primitiveTreeBuilder(t"Array[Byte]")
      case JsonSchemaF.BooleanF()  => primitiveTreeBuilder(t"Boolean")
      case JsonSchemaF.DateF()     => primitiveTreeBuilder(t"java.time.LocalDate")
      case JsonSchemaF.DateTimeF() => primitiveTreeBuilder(t"java.time.LocalDateTime")
      case JsonSchemaF.PasswordF() => primitiveTreeBuilder(t"String")
      case JsonSchemaF.ObjectF(properties, required) =>
        path =>
          val className = Type.Name(determineTypeName(path))
          val fieldStringWithState: State[TypeDefRegistry, List[Term.Param]] =
            properties
              .traverse[State[TypeDefRegistry, *], Term.Param] {
                case JsonSchemaF.Property(fieldName, tpeBuilder) =>
                  val isOptional = !required.contains(fieldName)
                  val nextPath   = path.append(FieldPath(fieldName))
                  val innerTpeStr: State[TypeDefRegistry, Term.Param] =
                    tpeBuilder(nextPath).map { tpeStr =>
                      val paramName = Name(fieldName)
                      if (isOptional) {
                        param"$paramName: Option[${tpeStr}]"
                      } else {
                        param"$paramName: $tpeStr"
                      }
                    }
                  innerTpeStr
              }

          for {
            fieldsString <- fieldStringWithState
            classDef     = q"""case class $className(..$fieldsString)"""
            _            <- addTypeDef(className, classDef)
          } yield {
            className
          }

      case JsonSchemaF.ArrayF(tpeBuilder) =>
        path =>
          path.last match {
            case NewType(aliasName) =>
              for {
                tpeStr   <- tpeBuilder(path.append(ArrayItem))
                aliasTpe = Type.Name(aliasName)
                _        <- addTypeDef(aliasTpe, q"type $aliasTpe = List[$tpeStr]")
              } yield {
                aliasTpe
              }
            case _: FieldPath | ArrayItem | _: CoproductBranch =>
              for {
                tpeStr <- tpeBuilder(path.append(ArrayItem))
              } yield {
                Type.Apply(Type.Name("List"), tpeStr :: Nil)
              }
          }
      case JsonSchemaF.EnumF(cases) =>
        path =>
          // we can only handle string enum now, to fix it we need to fix the
          // parser and the JsonSchema to capture all valid input
          enumSealedTrait(Type.Name(determineTypeName(path)), cases)

      case JsonSchemaF.SumF(cases) =>
        /**
         * Sum type not supported now, challenges include
         * 1. cases does not
         */
        path =>
          val adtName = determineTypeName(path)
          val stateWithTypeRefs: State[TypeDefRegistry, List[Type]] =
            cases.zipWithIndex.traverse[State[TypeDefRegistry, *], Type] {
              case (builder, idx) =>
                builder(path.append(CoproductBranch(adtName, idx.toString)))
            }
          nestedEitherCoproduct(stateWithTypeRefs)

      case JsonSchemaF.ReferenceF(ref) =>
        _ =>
          /**
           * Todo: should we consider validation?
           * Todo: Make this support inter-file references
           */
          State.pure {
            val tpeNameStr = ref.split("/").last
            Type.Name(tpeNameStr)
          }
    }

  /**
   * This method is not comprehensive, it does not guarantee the name produced will be unique
   * It is simply a best effort to produce a name that makes sense
   */
  def determineTypeName(p: NonEmptyList[SchemaPath]): String =
    p.reverse match {
      case NonEmptyList(NewType(typeName), _)    => typeName.capitalize
      case NonEmptyList(FieldPath(fieldName), _) => fieldName.capitalize
      case NonEmptyList(ArrayItem, rest) =>
        rest
          .collectFirst {
            case NewType(typeName)    => s"${typeName}Item"
            case FieldPath(fieldName) => s"${fieldName}Item"
          }
          .getOrElse(throw new Error("Invariant violation: Path only contains ArrayItem"))
      case NonEmptyList(CoproductBranch(coproductName, branchId), _) =>
        // todo: this looks horrible, can we improve it?
        s"${coproductName}Branch${branchId}"

    }

  def nestedEitherCoproduct(
    stateWithTypeRefs: State[TypeDefRegistry, List[Type]]
  ): State[TypeDefRegistry, Type] =
    for {
      tpeRefs <- stateWithTypeRefs
    } yield {
      tpeRefs.reduceLeft[Type] {
        case (acc, tpeRef) => t"Either[$acc, $tpeRef]"
      }
    }

  def enumSealedTrait(baseName: Type.Name, cases: List[String]): State[TypeDefRegistry, Type] = {

    val baseTrait = q"""sealed trait ${baseName}"""
    val caseObjectsStr = cases
      .map(s => Term.Name(s))
      .map(cs => q"case object $cs extends ${baseTrait.templ}")
    val allStats = NonEmptyList.of[Defn](baseTrait, caseObjectsStr: _*)

    addTypeDefs(baseName, allStats).map(_ => baseName)

  }

  def modelCode(schemaName: String, schema: JsonSchemaF.Fixed): ModelCode = {
    val schemaToCode = scheme.cata(schemaToTreeBuiler)
    schemaToCode(schema)(NonEmptyList.one(NewType(schemaName)))
  }

  def produceModelCode(
    openApiSchemas: Map[String, JsonSchemaF.Fixed],
    packageName: String,
    moduleName: String
  ): Source = {
    val allDefns = openApiSchemas.flatMap {
      case (objKey, jsonSchema) =>
        val (typeRegistry, _) = modelCode(objKey, jsonSchema).run(Map.empty).value

        val defns = typeRegistry.values.flatMap(_.toList).toList
        defns
    }.toList

    val packageRef    = Term.Name(packageName)
    val moduleNameRef = Term.Name(moduleName)
    val sourceCode    = source"""
        package $packageRef
        object $moduleNameRef {
          ..$allDefns
        }
       """
    sourceCode
  }
}

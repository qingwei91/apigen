package apigen

import cats.data.{ NonEmptyList, State }
import cats.instances.list._
import cats.syntax.all._
import higherkindness.droste._
import higherkindness.droste.data.Fix
import higherkindness.droste.syntax.all._
import higherkindness.skeuomorph.openapi.JsonSchemaF

import scala.meta._

object SchemaBuilder {

  type TypeDefRegistry = Map[Type, NonEmptyList[Defn]]
  type TreeBuilder     = TypeContext => State[TypeDefRegistry, Type]

  sealed trait SchemaPath
  case object Root                        extends SchemaPath
  case class FieldPath(fieldName: String) extends SchemaPath
  case object ArrayItem                   extends SchemaPath

  /**
   * ADT to model the context used by type, this is needed for
   * - generate type alias when type used for aliasing
   * - generate ADT for enum, we use the name from context as ADT's name
   * -
   */
  sealed trait TypeContext
  case class RecordField(fieldName: String)                extends TypeContext
  case class TypeAlias(aliasName: String)                  extends TypeContext
  case class IsNestedTypeOfAlias(closestAliasName: String) extends TypeContext
  case class IsNestedTypeOfField(closestFieldName: String) extends TypeContext

  def primitiveTreeBuilder(primTpe: Type): TreeBuilder = {
    case TypeAlias(aliasName) =>
      val tpeName = Type.Name(aliasName)
      addTypeDef(tpeName, q"type $tpeName = $primTpe").map(_ => primTpe)
    case _: RecordField         => State.pure(primTpe)
    case _: IsNestedTypeOfAlias => State.pure(primTpe)
    case _: IsNestedTypeOfField => State.pure(primTpe)
  }

  def addTypeDef(tpeAlias: Type, definition: Defn): State[TypeDefRegistry, Unit] =
    addTypeDefs(tpeAlias, NonEmptyList.one(definition))

  def addTypeDefs(tpeAlias: Type, definitions: NonEmptyList[Defn]): State[TypeDefRegistry, Unit] =
    State.modify[TypeDefRegistry] { registry =>
      registry.get(tpeAlias) match {
        case Some(value) =>
          throw new Exception(
            s"Unexpected state, $tpeAlias type alias has already been defined as $value"
          )
        case None => registry.updated(tpeAlias, definitions)
      }
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
  val schemaToTreeBuiler: Algebra[JsonSchemaF, TreeBuilder] =
    Algebra[JsonSchemaF, TreeBuilder] {
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
      case JsonSchemaF.ObjectF(properties, required) => {
        case TypeAlias(aliasName) =>
          val className = Type.Name(aliasName)
          val fieldStringWithState: State[TypeDefRegistry, List[Term.Param]] =
            properties
              .traverse[State[TypeDefRegistry, *], Term.Param] {
                case JsonSchemaF.Property(fieldName, tpeBuilder) =>
                  val isOptional = !required.contains(fieldName)

                  val innerTpeStr: State[TypeDefRegistry, Term.Param] =
                    tpeBuilder(RecordField(fieldName)).map { tpeStr =>
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

        case _ =>
          throw new Exception(
            "Object must be a type alias, or else we dont know how to name the object!"
          )
      }
      case JsonSchemaF.ArrayF(tpeBuilder) => {
        case TypeAlias(aliasName) =>
          for {
            tpeStr   <- tpeBuilder(IsNestedTypeOfAlias(aliasName))
            aliasTpe = Type.Name(aliasName)
            _        <- addTypeDef(aliasTpe, q"type $aliasTpe = List[$tpeStr]")
          } yield {
            aliasTpe
          }
        case RecordField(fieldName) =>
          for {
            tpeStr <- tpeBuilder(IsNestedTypeOfField(fieldName))
          } yield {
            Type.Apply(Type.Name("List"), tpeStr :: Nil)
          }

        case _: IsNestedTypeOfField | _: IsNestedTypeOfAlias =>
          throw new Exception("Does not support nested array!")

      }
      case JsonSchemaF.EnumF(cases) =>
        // we can only handle string enum now, to fix it we need to fix the
        // parser and the JsonSchema to capture all valid input
        {
          case TypeAlias(aliasName)                  => enumSealedTrait(Type.Name(aliasName), cases)
          case RecordField(fieldName)                => enumSealedTrait(Type.Name(fieldName), cases)
          case IsNestedTypeOfAlias(closestAliasName) =>
            // this case means enum is
            enumSealedTrait(Type.Name(s"${closestAliasName}Enum"), cases)
          case IsNestedTypeOfField(closestFieldName) =>
            enumSealedTrait(Type.Name(closestFieldName), cases)

        }

      case JsonSchemaF.SumF(cases) =>
        /**
         * Sum type not supported now, challenges include
         * 1. cases does not
         */
        {
          case TypeAlias(aliasName) =>
            val stateWithTypeRefs: State[TypeDefRegistry, List[Type]] =
              cases.traverse[State[TypeDefRegistry, *], Type](
                builder => builder(IsNestedTypeOfAlias(aliasName))
              )
            nestedEitherCoproduct(stateWithTypeRefs)

          case RecordField(fieldName) =>
            val stateWithTypeRefs: State[TypeDefRegistry, List[Type]] =
              cases.traverse[State[TypeDefRegistry, *], Type](
                builder => builder(IsNestedTypeOfField(fieldName))
              )
            nestedEitherCoproduct(stateWithTypeRefs)

          case IsNestedTypeOfField(fieldName) =>
            val stateWithTypeRefs: State[TypeDefRegistry, List[Type]] =
              cases.traverse[State[TypeDefRegistry, *], Type](
                builder => builder(IsNestedTypeOfField(fieldName))
              )
            nestedEitherCoproduct(stateWithTypeRefs)

          case IsNestedTypeOfAlias(aliasName) =>
            val stateWithTypeRefs: State[TypeDefRegistry, List[Type]] =
              cases.traverse[State[TypeDefRegistry, *], Type](
                builder => builder(IsNestedTypeOfField(aliasName))
              )
            nestedEitherCoproduct(stateWithTypeRefs)
        }
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

  def produceModelCode(openApiSchemas: Map[String, JsonSchemaF.Fixed]): Map[String, Defn.Object] =
    openApiSchemas
      .map {
        case (objKey, jsonSchema) =>
          val schemaToCode      = scheme.cata(schemaToTreeBuiler)
          val (typeRegistry, _) = schemaToCode(jsonSchema)(TypeAlias(objKey)).run(Map.empty).value

          val defns = typeRegistry.values.flatMap(_.toList).toList

          objKey -> q"""
             object ${Term.Name(objKey)} {
               ..${defns}
             }
           """
      }

}

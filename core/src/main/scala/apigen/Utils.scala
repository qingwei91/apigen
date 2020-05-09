package apigen

import cats.data.{ NonEmptyList, State }

import scala.meta.{ Defn, Type }

object Utils {
  type TypeDefRegistry = Map[Type.Name, NonEmptyList[Defn]]
  type CodeProducer[A] = State[TypeDefRegistry, A]
  type ModelCode       = CodeProducer[Type]

  // this is kleisli
  type TypeProducer = SchemaPosition => ModelCode

  // each model produce a number of defs and 1 type that refers to the model

  def addTypeDef(tpeAlias: Type.Name, definition: Defn): State[TypeDefRegistry, Unit] =
    addTypeDefs(tpeAlias, NonEmptyList.one(definition))

  def addTypeDefs(
    tpeAlias: Type.Name,
    definitions: NonEmptyList[Defn]
  ): State[TypeDefRegistry, Unit] =
    State.modify[TypeDefRegistry] { registry =>
      registry.get(tpeAlias) match {
        case Some(value) =>
          throw new Exception(
            s"Unexpected state, $tpeAlias type alias has already been defined as $value"
          )
        case None => registry.updated(tpeAlias, definitions)
      }
    }

}

package apigen
import higherkindness.skeuomorph.openapi.schema

sealed trait SchemaPosition

case class ApiPath(name: String)                     extends SchemaPosition
case class MethodNode(parent: ApiPath, name: String) extends SchemaPosition

sealed trait MediaTypeParent extends SchemaPosition
sealed trait HeaderParent    extends SchemaPosition
case class PathParam(parent: MethodNode, paramName: String, location: schema.Location)
    extends SchemaPosition

case class PathRequest(parent: MethodNode) extends MediaTypeParent
case class PathResponse(parent: MethodNode, responseCode: Either[Int, DefaultCode.type])
    extends MediaTypeParent
    with HeaderParent

case class HeaderPath(parent: HeaderParent, headerName: String)      extends SchemaPosition
case class MediaTypePath(parent: MediaTypeParent, mediaType: String) extends SchemaPosition

case class Schema(name: String)            extends SchemaPosition
case class ComponentResponse(name: String) extends HeaderParent with MediaTypeParent
case class ComponentRequest(name: String)  extends MediaTypeParent
case class ComponentParam(name: String, paramName: String, location: schema.Location)
    extends SchemaPosition

case object DefaultCode

package apigen

import cats.Functor
import cats.syntax.all._
import higherkindness.skeuomorph.openapi.schema
import higherkindness.skeuomorph.openapi.schema.{
  Callback,
  Components,
  Header,
  MediaType,
  Parameter,
  Path,
  Reference,
  Request,
  Response
}

object implicits {
  implicit val mediaFunctor: Functor[MediaType] = new Functor[MediaType] {
    override def map[A, B](fa: MediaType[A])(f: A => B): MediaType[B] = {
      fa.copy(schema = fa.schema.map(f), encoding = Map.empty)
    }
  }
  implicit val headerFunctor: Functor[Header] = new Functor[Header] {
    override def map[A, B](fa: Header[A])(f: A => B): Header[B] = {
      fa.copy(schema = f(fa.schema))
    }
  }

  implicit val requestFunctor: Functor[Request] = new Functor[Request] {
    override def map[A, B](fa: Request[A])(f: A => B): Request[B] = {
      fa.copy(content = fa.content.mapValues(_.map(f)))
    }
  }
  implicit val responseFunctor: Functor[Response] = new Functor[Response] {
    override def map[A, B](fa: Response[A])(f: A => B): Response[B] = {
      val headerB = fa.headers.mapValues {
        case Left(value)  => Left(value.map(f))
        case Right(value) => Right(value)
      }
      fa.copy(content = fa.content.mapValues(_.map(f)), headers = headerB)
    }
  }
  implicit val paramFunctor: Functor[Parameter] = new Functor[Parameter] {
    override def map[A, B](fa: Parameter[A])(f: A => B): Parameter[B] = {
      fa match {
        case p: Parameter.Path[A]   => p.copy(schema = f(fa.schema))
        case q: Parameter.Query[A]  => q.copy(schema = f(fa.schema))
        case h: Parameter.Header[A] => h.copy(schema = f(fa.schema))
        case c: Parameter.Cookie[A] => c.copy(schema = f(fa.schema))

      }
    }
  }
  implicit val componentFunctor: Functor[Components] = new Functor[Components] {
    override def map[A, B](fa: Components[A])(f: A => B): Components[B] = {
      val schemasB  = fa.schemas.mapValues(f)
      val responseB = fa.responses.mapValues(_.left.map(_.map(f)))
      val paramsB   = fa.parameters.mapValues(_.left.map(_.map(f)))
      val requestsB = fa.requestBodies.mapValues(_.left.map(_.map(f)))
      fa.copy(
        schemas = schemasB,
        responses = responseB,
        parameters = paramsB,
        requestBodies = requestsB
      )
    }
  }

  implicit val operationFunctor: Functor[Path.Operation] = new Functor[Path.Operation] {
    override def map[A, B](fa: Path.Operation[A])(f: A => B): Path.Operation[B] = {
      val paramsB    = fa.parameters.map(_.left.map(_.map(f)))
      val reqsB      = fa.requestBody.map(_.left.map(_.map(f)))
      val responseB  = fa.responses.mapValues(_.left.map(_.map(f)))
      val callbacksB = Map.empty[String, Either[Callback[B], Reference]]
      fa.copy(
        parameters = paramsB,
        requestBody = reqsB,
        responses = responseB,
        callbacks = callbacksB
      )
    }
  }
  implicit val pathItem: Functor[schema.Path.ItemObject] = new Functor[Path.ItemObject] {
    override def map[A, B](fa: Path.ItemObject[A])(f: A => B): Path.ItemObject[B] = {
      fa.copy(
        get = fa.get.map(_.map(f)),
        put = fa.put.map(_.map(f)),
        post = fa.post.map(_.map(f)),
        delete = fa.delete.map(_.map(f)),
        options = fa.options.map(_.map(f)),
        head = fa.head.map(_.map(f)),
        patch = fa.patch.map(_.map(f)),
        trace = fa.trace.map(_.map(f)),
        parameters = fa.parameters.map(_.left.map(_.map(f)))
      )
    }
  }
}

package io.github.maxkar
package http.server.toolkit

import fun.typeclass.Monad

import http.headers.Header
import http.headers.HeaderFormatException

import http.server.api.Route
import http.server.api.Response
import http.server.api.NegotiableErrors


/**
 * An implementation of some Route methods that are easily expressed
 * via the others. Actual server implementations may extend this class
 * and will have to implement only the minimal set of methods.
 *
 * The minimal set of methods is:
 * * `path` (could be implemented using doRoute)
 * * `continue`
 * * `getMethod`
 * * `getHeaders`
 * * `getHeaderNames`
 * * `getCookies`
 * * `getParameters`
 * * `getParameterNames`
 * * `getBodyAsBytes`
 *
 * Utility methods provided:
 *  * `doRoute` - routing logic implementation
 *  * `raiseXXX` set of methods
 *
 * @tparam M execution monad.
 */
abstract class BaseRoute[M[_]: Monad]
    extends Route[M]:

  /** Name of the header used for content negotiation. */
  protected final val contentNegotiationHeader = "Accept"

  /** Error handlers for the standard errors. */
  protected val errors: NegotiableErrors


  /** List of standard (server-wide) methods known by the route. */
  protected val knownMethods: Iterable[String]


  /**
   * Aborts processing of the response. This method is similar to
   * `Processing.abort` but moved into the base route to reduce amount of
   * dependency (only the `abort` method is used by this implementation).
   */
  protected def abort[T](response: Response): M[T]


  override def method[T](fn: PartialFunction[String, M[T]]): M[T] =
    getMethod() flatMap { meth =>
      if fn.isDefinedAt(meth) then
        fn(meth)
      else
        raiseBadMethod(meth, knownMethods.filter(fn.isDefinedAt))
    }


  override def customMethod[T](
        customMethods: Iterable[String]
      )(
        fn: PartialFunction[String, M[T]]
      ): M[T] =
    getMethod() flatMap { meth =>
      if fn.isDefinedAt(meth) then
        fn(meth)
      else
        raiseBadMethod(meth, (customMethods ++ knownMethods).filter(fn.isDefinedAt))
    }


  override def getHeader(name: String): M[String] =
    getHeaders(name) flatMap { maybeValues =>
      if maybeValues.isEmpty then
        raiseMissingHeader(name)
      else if maybeValues.length == 1 then
        Monad.pure(maybeValues.head)
      else
        Monad.pure(maybeValues.mkString(","))
    }


  override def getHeader[T](header: Header[T]): M[T] =
    getHeaders(header.name) flatMap { values =>
      try
        Monad.pure(header.decodeFromString(values))
      catch
        case HeaderFormatException(msg) =>
          if values.isEmpty then
            raiseMissingHeader(header.name)
          else
            raiseInvalidHeader(header.name, values, msg)
      end try
    }


  override def getOptHeader(name: String, defaultValue: String): M[String] =
    getHeaders(name) map { values =>
      if values.isEmpty then defaultValue
      else if values.length == 1 then values.head
      else values.mkString(",")
    }


  override def getOptHeader[T](header: Header[T], defaultValue: T): M[T] =
    getHeaders(header.name) flatMap { values =>
      if values.isEmpty then
        Monad.pure(defaultValue)
      else
        try
          Monad.pure(header.decodeFromString(values))
        catch
          case HeaderFormatException(msg) =>
            raiseInvalidHeader(header.name, values, msg)
        end try
      end if
    }


  override def getOptHeader(name: String): M[Option[String]] =
    getHeaders(name) map { values =>
      if values.isEmpty then None
      else if values.length == 1 then Some(values.head)
      else Some(values.mkString(","))
    }


  override def getOptHeader[T](header: Header[T]): M[Option[T]] =
    getHeaders(header.name) flatMap { values =>
      if values.isEmpty then
        Monad.pure(None)
      else
        try
          Monad.pure(Some(header.decodeFromString(values)))
        catch
          case HeaderFormatException(msg) =>
            raiseInvalidHeader(header.name, values, msg)
        end try
      end if
    }


  override def getParameter(name: String): M[String] =
    getParameters(name) flatMap { params =>
      if params.nonEmpty then
        Monad.pure(params.head)
      else
        raiseMissingParameter(name)
    }


  override def getOptParameter(name: String, defaultValue: String): M[String] =
    getParameters(name) map { params =>
      if params.nonEmpty then
        params.head
      else
        defaultValue
    }


  override def getOptParameter(name: String): M[Option[String]] =
    getParameters(name) map { params =>
      if params.nonEmpty then
        Some(params.head)
      else
        None
    }


  override def negotiate[H: Ordering, T](
        header: Header[Seq[H]],
        fn: PartialFunction[H, M[T]],
        default: => M[T],
      ): M[T] =
    getHeader(header) flatMap { headerValues =>
      headerValues.sorted.find(fn.isDefinedAt) match
        case Some(v) => fn(v)
        case None => default
      end match
    }


  override def negotiateBy[H, W: Ordering, T](
        header: Header[Seq[H]],
        weight: H => W,
        fn: PartialFunction[H, M[T]],
        default: => M[T],
      ): M[T] =
    getHeader(header) flatMap { headerValues =>
      headerValues.sortBy(weight).find(fn.isDefinedAt) match
        case Some(v) => fn(v)
        case None => default
      end match
    }


  /** Performs routing using the given function and path segments. */
  protected def doRoute[T](
        fullPath: List[String],
        unconsumedPath: List[String],
        fn: PartialFunction[List[String], M[T]],
      ): M[T] =
    if fn.isDefinedAt(unconsumedPath) then
      fn(unconsumedPath)
    else
      raisePathNotFound(fullPath, unconsumedPath)
  end doRoute


  /** Raises the "path not found" error. */
  protected def raisePathNotFound[T](fullPath: List[String], unconsumedPath: List[String]): M[T] =
    getHeaders(contentNegotiationHeader) flatMap { acc =>
      abort(errors.pathNotFound(acc, fullPath, unconsumedPath))
    }


  /** Raises the "invalid method" error. */
  protected def raiseBadMethod[T](method: String, supportedMethods: Iterable[String] = Seq.empty): M[T] =
    getHeaders(contentNegotiationHeader) flatMap { acc =>
      abort(errors.unsupportedMethod(acc, method, supportedMethods))
    }


  /** Raises the "missig header" error. */
  protected def raiseMissingHeader[T](header: String): M[T] =
    getHeaders(contentNegotiationHeader) flatMap { acc =>
      abort(errors.missingHeader(acc, header))
    }


  /** Rasises the "invalid header value" error. */
  protected def raiseInvalidHeader[T](
        headerName: String,
        values: Seq[String],
        message: String,
      ): M[T] =
    getHeaders(contentNegotiationHeader) flatMap { acc =>
      abort(errors.invalidHeader(acc, headerName, values, message))
    }


  /** Raises "missing required parameter" error. */
  protected def raiseMissingParameter[T](parameter: String): M[T] =
    getHeaders(contentNegotiationHeader) flatMap { acc =>
      abort(errors.missingParameter(acc, parameter))
    }


  /** Raises "byte body length exceeded". */
  protected def raiseByteLengthExceeded[T](length: Long): M[T] =
    getHeaders(contentNegotiationHeader) flatMap { acc =>
      abort(errors.byteLengthExceeded(acc, length))
    }
end BaseRoute

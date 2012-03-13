/*
 * Copyright 2012 Typesafe Inc.
 *
 * Unless otherwise agreed, training materials may only be used for
 * educational and reference purposes by individual named participants
 * in a training course offered by Typesafe or a Typesafe training partner.
 * Unauthorized reproduction, redistribution, or use of this material is prohibited.
 */
package models.github

import play.api.libs.json.Json._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.libs.ws.Response
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.Akka

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

trait Github {
  val logger = LoggerFactory.getLogger("application");

  val baseUrl = "https://api.github.com"

  lazy val timeParser = org.joda.time.format.ISODateTimeFormat.dateTimeNoMillis

  private def calculateExpirationInSecs(response : Response) : Int = {
      if (response.status == 404) return 10
      val remainingAllowed = response.header("X-RateLimit-Remaining").map { _.toInt }
      remainingAllowed match {
        case Some(r) if r < 100 => 7 * 24 * 60 * 60
        case Some(r) if r < 2000 => 24 * 60 * 60
        case _ => 60
      }     
  }

  //Cached imports
  import play.api.cache._
  import play.api.Play.current //Current Play application

  private def responsePromise(url : String) =
    WS.url(url).get()

  def find[T](url: String)(implicit reads: Reads[T], manifest: Manifest[T]): Promise[Option[T]] = {
    val cacheKey = url
    Cache.getAs[Option[T]](cacheKey) map { value => Akka.future(value) } getOrElse {
      responsePromise(url).map { response =>
        val result = if (response.status == 404) None else Some(response.json.as[T])
        Cache.set(cacheKey, result, calculateExpirationInSecs(response))
        result
      }
    }
  }

  /**
   * A DSL to download jsons from a url using
   * Note: We could have cached the web pages as well, but for this training we will cache only the results
   */
  def get(url: String) = new {
    def promiseOf[T](implicit reads: Reads[T], manifest: Manifest[T]) : Promise[T] = {
      val cacheKey = url
      Cache.getAs[T](cacheKey) map { value => Akka.future(value) } getOrElse { //return a Promise with value if found in cache, or....
        logger.debug("fetching " + cacheKey)
        responsePromise(url).map { response => //and map the result to the type you expect
          val result = response.json.as[T]
          Cache.set(cacheKey, result, calculateExpirationInSecs(response))  
          result
        }
      }
    }

    def paginatedPromiseOf[T](implicit reads: Reads[T], manifest: Manifest[T]): Promise[Seq[T]] = {

      val LinkRegExp = """.*\<(https://.*?)\>;\W*rel="next",.*?""".r
      
      /**
       * Find the next link from the response header for pagination
       */
      def nextLink(response: Response) : Option[String] = {
        response.header("Link") match {
          case Some(LinkRegExp(next)) => Some(next)
          case _ => None
        }
      }

      /**
       * Recursively get the next promises without blocking
       * Note: Uses Vector as Seq because we are appending
       */
      def nextPromisesFrom(promises : Promise[Vector[Response]]) : Promise[Vector[Response]] = promises flatMap { responses =>
        nextLink(responses.last) match {
          case Some(nextUrl) => {
            val nextPromises = responsePromise(nextUrl) map { nextResponse =>
              responses :+ nextResponse
            }
            nextPromisesFrom(nextPromises)
          }
          case _ => Akka.future(responses)
        }
      }
      
      import scala.collection.mutable.Builder
      import scala.collection.generic.CanBuildFrom
      /**
       * Addition to Promise API to convert any Traversable of Promise to Promise of Traversable
       */
      def sequence[A, M[_] <: Traversable[_]](in: M[Promise[A]])(implicit cbf: CanBuildFrom[M[Promise[A]], A, M[A]]): Promise[M[A]] = { 
        in.foldLeft(Promise.pure(cbf(in)): Promise[Builder[A, M[A]]])((fr, fa) => for (r <- fr; a <- fa.asInstanceOf[Promise[A]]) yield (r += a)).map(_.result)
      }
      
      val cacheKey = url
      Cache.getAs[Seq[T]](cacheKey) map { value => Akka.future(value) } getOrElse { //return a Promise with value if found in cache, or....
        val first = sequence(Vector(responsePromise(url))) 
        nextPromisesFrom(first) map { responses =>
          val jsons = responses map (_.json) 
          val result = jsons.flatMap(_.as[Seq[T]])
          Cache.set(cacheKey, result, calculateExpirationInSecs(responses.last))
          result
        }
      }
    }
  }

}

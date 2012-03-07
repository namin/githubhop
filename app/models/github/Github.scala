package models.github

import play.api.libs.json.Json._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.libs.ws.Response
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.Akka

trait Github {
  val baseUrl = "https://api.github.com"

  lazy val timeParser = org.joda.time.format.ISODateTimeFormat.dateTimeNoMillis

  private def calculateExpirationInSecs(response : Response) : Int = {
      if (response.status == 404) return 10
      val remainingAllowed = response.header("X-RateLimit-Remaining").map { _.toInt }
      remainingAllowed match {
        case Some(r) if r < 100 => 60 * 60 //We have only 100 requests left. Keep this value for an hour to let system recover
        case Some(r) if r < 2000 => 5 * 60 //We have less than 2000 left. Scale back to 1 req/ 5 mins
        case _ => 30 //We are looking good, keep the data quite fresh 
      }     
  }

  //Cached imports
  import play.api.cache._
  import play.api.Play.current //Current Play application

  private def responsePromise(url : String) = WS.url(url).get()

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

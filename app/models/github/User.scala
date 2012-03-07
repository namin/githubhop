package models.github

import play.api.libs.json._
import play.api.libs.concurrent.Promise

import models._

case class User(login: String, avatar_url: String)

object Users extends UsersBase with Github

private[github] trait UsersBase { this : Github =>
  implicit object UserFormat extends Reads[User] {
    def reads(json: JsValue): User = User(
      login = (json \ "login").as[String],
      avatar_url = (json \ "avatar_url").as[String]
    )
  }

  def authenticate(login: String) =
    find[User](baseUrl + "/users/" + login)

}

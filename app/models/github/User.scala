/*
 * Copyright 2012 Typesafe Inc.
 *
 * Unless otherwise agreed, training materials may only be used for
 * educational and reference purposes by individual named participants
 * in a training course offered by Typesafe or a Typesafe training partner.
 * Unauthorized reproduction, redistribution, or use of this material is prohibited.
 */
package models.github

import play.api.libs.json._
import play.api.libs.concurrent.Promise

import models._

case class User(login: String, avatar_url: String) {
  def id = login
}

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
      
  def watching(repo: Repo) =
    get(baseUrl + "/repos/" + repo.owner.login + "/" + repo.name + "/watchers").promiseOf[Seq[User]]
}

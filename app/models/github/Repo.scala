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

/**
 * Represents a Github Repo(sitory)
 */
case class Repo(name: String, description: String, homepage: String, owner: User, num_watchers: Int) {
  def id = owner.id + "/" + name
}

/**
 * Convenience object used with the running Play application
 */
object Repos extends ReposBase with Github

/**
 * This base trait contains the format of Repo. 
 * We need such a Base trait to be able to mock easily
 */
private[github] trait ReposBase  { this : Github => //we use a self type here to be able to easily mock it 
  import Users._
  implicit object RepoFormat extends Reads[Repo] { //only read, if writes and reads: Format
    def reads(json: JsValue): Repo = Repo(
      name = (json \ "name").as[String],
      description = (json \ "description").asOpt[String].getOrElse(""),
      homepage = (json \ "homepage").asOpt[String].getOrElse(""),
      owner = (json \ "owner").as[User],
      num_watchers = (json \ "watchers").as[Int]
    )
  }

  def authenticate(id: String) =
    find[Repo](baseUrl + "/repos/" + id)
    
  def watched(user: User) =
    get(baseUrl + "/users/" + user.login + "/watched").promiseOf[Seq[Repo]]
  
  def of(user : User) =
    get(baseUrl + "/users/" + user.login + "/repos").paginatedPromiseOf[Repo]
}

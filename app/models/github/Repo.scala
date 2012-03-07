package models.github

import play.api.libs.json._
import play.api.libs.concurrent.Promise

import models._

/**
 * Represents a Github Repo(sitory)
 */
case class Repo(name: String, description: String, url: String, owner: User)

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
      description = (json \ "description").as[String],
      url = (json \ "url").as[String],
      owner = (json \ "owner").as[User]
    )
  }
  
  def of(user : User) : Promise[Seq[Repo]] =
    get(baseUrl + "/users/" + user.login + "/repos").paginatedPromiseOf[Repo]
}

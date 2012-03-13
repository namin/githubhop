/*
 * Copyright 2012 Typesafe Inc.
 *
 * Unless otherwise agreed, training materials may only be used for
 * educational and reference purposes by individual named participants
 * in a training course offered by Typesafe or a Typesafe training partner.
 * Unauthorized reproduction, redistribution, or use of this material is prohibited.
 */
package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.concurrent.Promise

import models._

object Application extends Controller {

  val topN = 200
  
  val loginForm = Form("login" -> nonEmptyText)

  def index = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      loginForm => Ok(views.html.login("", loginForm)),
      login =>
        login.count(_ == '/') match {
          case 0 => recommendUser(login, loginForm.bindFromRequest)
          case 1 => recommendRepo(login, loginForm.bindFromRequest)
          case _ => Ok(views.html.login("Not a valid Github username or repository", loginForm.bindFromRequest))
        })
  }

  // TODO: requests to github should be async.

  def rest(user: String, repo: String) = Action {
	repo match {
	  case "" => recommendUser(user, loginForm.bind(Map("login" -> user)))
	  case _ => val login = user+"/"+repo; recommendRepo(login, loginForm.bind(Map("login" -> login)))
	}
  }
  
  def recommendUser(login: String, loginFormFromRequest: Form[String]) = {
    github.Users.authenticate(login).value.get match {
      case None => Ok(views.html.login("Not a valid Github username", loginFormFromRequest))
      case Some(user) => Ok(views.html.users(user, github.Recommender.user.similar(topN)(user), loginFormFromRequest))
    }
  }
  
  def recommendRepo(login: String, loginFormFromRequest: Form[String]) = {
    github.Repos.authenticate(login).value.get match {
      case None => Ok(views.html.login("Not a valid Github repository (i.e. 'username/repository_name')", loginFormFromRequest))
      case Some(repo) => Ok(views.html.repos(repo, github.Recommender.repo.similar(topN)(repo), loginFormFromRequest))
    }
  }  

  def handleTimeout(promise: Promise[Result]) = {
    promise orTimeout("Timed out while waiting for response", 30, java.util.concurrent.TimeUnit.SECONDS) map { _.fold (
      page => page,
      errorMsg => InternalServerError(views.html.error(errorMsg))  
    )}
  }
}

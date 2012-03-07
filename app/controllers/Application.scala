package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.concurrent.Promise

import models._

object Application extends Controller {

  val loginForm = Form("login" -> nonEmptyText)

  def index = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      loginForm => Ok(views.html.login("", loginForm)),
      login =>
        AsyncResult { handleTimeout { github.Users.authenticate(login) map { userIfOk =>
          userIfOk match {
            case None => Ok(views.html.login("Not a valid Github username", loginForm.bindFromRequest))
            case Some(user) => AsyncResult { handleTimeout { github.Repos.of(user) map { repos =>
                Ok(views.html.dashboard(user, repos, loginForm.bindFromRequest))
  }}}}}}})}

  def handleTimeout(promise: Promise[Result]) = {
    promise orTimeout("Timed out while waiting for response", 30, java.util.concurrent.TimeUnit.SECONDS) map { _.fold (
      page => page,
      errorMsg => InternalServerError(views.html.error(errorMsg))  
    )}
  }
}

package controllers

import play.api._
import play.api.data.Forms._
import play.api.data._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._

object Application extends Controller {

  def index = Action { implicit request =>

    print(request)

    Ok(views.html.main("Schedule Planner"))
  }

//  def handle = Action { implicit request =>
//
//    Ok()
//  }
}
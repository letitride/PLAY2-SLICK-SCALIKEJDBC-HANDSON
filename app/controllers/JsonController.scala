package controllers

import controllers.UserController.UserForm
import javax.inject.Inject
import models.Users
import play.api.mvc.{AbstractController, ControllerComponents}
import play.api.libs.json._
import scalikejdbc._

class JsonController @Inject()(components: ControllerComponents) extends AbstractController(components) {


  implicit val usersWritesFormat = new Writes[Users]{
    def writes(user: Users): JsValue = {
      Json.obj(
        "id"-> user.id,
        "name"      -> user.name,
        "companyId" -> user.companyId
      )
    }
  }

  implicit val userFormFormat = new Reads[UserForm]{
    def reads(json: JsValue): JsResult[UserForm] = {
      JsSuccess(UserForm(
        id = (json \ "id").asOpt[Long],
        name = (json \ "name").as[String],
        companyId = (json \ "companyId").asOpt[Int]
      ))
    }
  }

  /**
   * 一覧表示
   */
  def list = Action{ implicit request =>
    val u = Users.syntax("u")
    DB.readOnly{implicit session =>
      val users = withSQL{
        select.from(Users as u).orderBy(u.id.asc)
      }.map(Users(u.resultName)).list.apply()

      Ok(Json.obj("users" -> users))
    }

  }

  /**
   * ユーザ登録
   */
  def create = Action(parse.json){ implicit request =>
    request.body.validate[UserForm].map { form =>
      DB.localTx{ implicit session =>
        Users.create(form.name, form.companyId)
        Ok(Json.obj("result" -> "success"))
      }
    }.recoverTotal( e => {
      BadRequest(Json.obj(
        "result" -> "failure",
        "error" -> JsError.toJson(e)))
    })
  }

  /**
   * ユーザ更新
   */
  def update = Action(parse.json){implicit request =>
    request.body.validate[UserForm].map{ form =>
      DB.localTx( implicit session => {
        Users.find(form.id.get).foreach(user => {
          Users.save(user.copy(name = form.name, companyId = form.companyId))
        })
        Ok(Json.obj("result"->"success"))
      })
    }.recoverTotal( e => {
      BadRequest(Json.obj("result"->"failure", "error"->JsError.toJson(e)))
    })
  }

  /**
   * ユーザ削除
   */
  def remove(id: Long) = Action( implicit request => {
    DB.localTx { implicit session =>
      Users.find(id).foreach { user =>
        Users.destroy(user)
      }
      Ok(Json.obj("result" -> "success"))
    }
  })
}

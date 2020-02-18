package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import javax.inject.Inject
import scalikejdbc._
import models._

object UserController {
  // フォームの値を格納するケースクラス
  case class UserForm(id: Option[Long], name: String, companyId: Option[Int])
  // formから送信されたデータ ⇔ ケースクラスの変換を行う
  val userForm = Form(
    mapping(
      "id"        -> optional(longNumber),
      "name"      -> nonEmptyText(maxLength = 20),
      "companyId" -> optional(number)
    )(UserForm.apply)(UserForm.unapply)
  )
}

class UserController @Inject()(components: MessagesControllerComponents)
  extends MessagesAbstractController(components){

  import UserController._

  private val c = Companies.syntax("c")

  def list = Action{ implicit request => {
    val u = Users.syntax("u")

      DB.readOnly { implicit session => {
        val users = withSQL {
            select.from(Users as u).orderBy(u.id.asc)
          }.map(Users(u.resultName)).list().apply()
          Ok(views.html.user.list(users))
      }}
    }
  }

  def edit(id: Option[Long]) = Action { implicit request =>
    DB.readOnly { implicit session =>
      // リクエストパラメータにIDが存在する場合
      val form = id match {
        // IDが渡されなかった場合は新規登録フォーム
        case None => userForm
        // IDからユーザ情報を1件取得してフォームに詰める
        case Some(id) => Users.find(id) match {
          case Some(user) => userForm.fill(UserForm(Some(user.id), user.name, user.companyId))
          case None => userForm
        }
      }

      // プルダウンに表示する会社のリストを取得
      val companies = withSQL {
        select.from(Companies as c).orderBy(c.id.asc)
      }.map(Companies(c.resultName)).list().apply()

      Ok(views.html.user.edit(form, companies))
    }
  }
  def create = TODO

  def update = TODO

  def remove(id:Long) = TODO

}

package io.tyoras.cards.domain.user

import cats.Show
import cats.implicits.toShow

import java.time.ZonedDateTime
import java.util.UUID

sealed abstract class User extends Product with Serializable {
  protected type ThisType <: User

  def name: String
  def withUpdatedName(newName: String, updateDate: ZonedDateTime): ThisType
  def about: String
  def withUpdatedAbout(newAbout: String, updateDate: ZonedDateTime): ThisType
}

object User {
  final case class Existing(id: UUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data) extends User {
    override protected type ThisType = Existing

    override def name: String = data.name

    override def withUpdatedName(newName: String, updateDate: ZonedDateTime): ThisType =
      copy(data = data.withUpdatedName(newName, updateDate), updatedAt = updateDate)

    override def about: String = data.about

    override def withUpdatedAbout(newAbout: String, updateDate: ZonedDateTime): ThisType =
      copy(data = data.withUpdatedAbout(newAbout, updateDate), updatedAt = updateDate)
  }
  object Existing {
    implicit val shwo: Show[Existing] = e => s"id = ${e.id} | created_at = ${e.createdAt} | updated_at = ${e.updatedAt} | ${e.data.show}"
  }

  final case class Data(name: String, about: String) extends User {
    override protected type ThisType = Data

    override def withUpdatedName(newName: String, updateDate: ZonedDateTime): ThisType = copy(name = newName)

    override def withUpdatedAbout(newAbout: String, updateDate: ZonedDateTime): ThisType = copy(about = newAbout)
  }
  object Data {
    implicit val show: Show[Data] = d => s"""name = ${d.name} | about = ${d.about}"""
  }
}
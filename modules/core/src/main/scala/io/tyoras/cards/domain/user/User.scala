package io.tyoras.cards.domain.user

import cats.Show
import cats.implicits.toShow
import io.chrisdavenport.fuuid.FUUID

import java.time.ZonedDateTime

sealed abstract class User extends Product with Serializable:
  protected type ThisType <: User

  def name: String
  def withUpdatedName(newName: String, updateDate: ZonedDateTime): ThisType
  def about: String
  def withUpdatedAbout(newAbout: String, updateDate: ZonedDateTime): ThisType

object User:
  final case class Existing(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data) extends User:
    override protected type ThisType = Existing

    override def name: String = data.name

    override def withUpdatedName(newName: String, updateDate: ZonedDateTime): ThisType =
      copy(data = data.withUpdatedName(newName, updateDate), updatedAt = updateDate)

    override def about: String = data.about

    override def withUpdatedAbout(newAbout: String, updateDate: ZonedDateTime): ThisType =
      copy(data = data.withUpdatedAbout(newAbout, updateDate), updatedAt = updateDate)
  object Existing:
    given Show[Existing] = e => s"id = ${e.id} | created_at = ${e.createdAt} | updated_at = ${e.updatedAt} | ${e.data.show}"

  final case class Data(name: String, about: String) extends User:
    override protected type ThisType = Data

    override def withUpdatedName(newName: String, updateDate: ZonedDateTime): ThisType = copy(name = newName)

    override def withUpdatedAbout(newAbout: String, updateDate: ZonedDateTime): ThisType = copy(about = newAbout)
  object Data:
    given Show[Data] = d => s"""name = ${d.name} | about = ${d.about}"""

package io.tyoras.cards.domain.user.model

import cats.Show
import cats.implicits.toShow
import io.chrisdavenport.fuuid.FUUID
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import java.time.ZonedDateTime

sealed abstract class User extends Product with Serializable:
  protected type ThisType <: User

  def name: User.Name
  def withUpdatedName(newName: User.Name, updateDate: ZonedDateTime): ThisType
  def about: User.About
  def withUpdatedAbout(newAbout: User.About, updateDate: ZonedDateTime): ThisType

object User:
  type Name = Name.T
  object Name
      extends RefinedSubtype[String, DescribedAs[Not[
        Blank
      ] & Trimmed & MaxLength[100], "User name must be a non-blank string with a maximum length of 100 characters."]]

  type About = About.T
  object About extends RefinedSubtype[String, DescribedAs[Not[Blank] & Trimmed, "User description must be a non-blank string."]]

  final case class Existing(id: FUUID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data) extends User:
    override protected type ThisType = Existing

    override def name: User.Name = data.name

    override def withUpdatedName(newName: User.Name, updateDate: ZonedDateTime): ThisType =
      copy(data = data.withUpdatedName(newName, updateDate), updatedAt = updateDate)

    override def about: User.About = data.about

    override def withUpdatedAbout(newAbout: User.About, updateDate: ZonedDateTime): ThisType =
      copy(data = data.withUpdatedAbout(newAbout, updateDate), updatedAt = updateDate)
  object Existing:
    given Show[Existing] = e => s"id = ${e.id} | created_at = ${e.createdAt} | updated_at = ${e.updatedAt} | ${e.data.show}"

  final case class Data(name: User.Name, about: User.About) extends User:
    override protected type ThisType = Data

    override def withUpdatedName(newName: User.Name, updateDate: ZonedDateTime): ThisType = copy(name = newName)

    override def withUpdatedAbout(newAbout: User.About, updateDate: ZonedDateTime): ThisType = copy(about = newAbout)
  object Data:
    given Show[Data] = d => s"""name = ${d.name} | about = ${d.about}"""

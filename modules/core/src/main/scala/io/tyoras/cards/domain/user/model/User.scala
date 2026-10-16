package io.tyoras.cards.domain.user.model

import cats.{Order, Show}
import cats.implicits.toShow
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import pl.iterators.kebs.opaque.Opaque

import java.time.ZonedDateTime
import java.util.UUID

sealed abstract class User extends Product with Serializable:
  protected type ThisType <: User

  def name: User.Name
  def withUpdatedName(newName: User.Name): ThisType
  def about: User.About
  def withUpdatedAbout(newAbout: User.About): ThisType

object User:
  opaque type ID = UUID
  object ID extends Opaque[ID, UUID]:
    given Show[ID]  = Show.fromToString
    given Order[ID] = Order.fromOrdering
  type Name = Name.T
  object Name
      extends RefinedSubtype[String, DescribedAs[Not[
        Blank
      ] & Trimmed & MaxLength[100], "User name must be a non-blank string with a maximum length of 100 characters."]]

  type About = About.T
  object About extends RefinedSubtype[String, DescribedAs[Not[Blank] & Trimmed, "User description must be a non-blank string."]]

  final case class Existing(id: User.ID, createdAt: ZonedDateTime, updatedAt: ZonedDateTime, data: Data) extends User:
    override protected type ThisType = Existing

    override def name: User.Name = data.name

    override def withUpdatedName(newName: User.Name): ThisType =
      copy(data = data.withUpdatedName(newName))

    override def about: User.About = data.about

    override def withUpdatedAbout(newAbout: User.About): ThisType =
      copy(data = data.withUpdatedAbout(newAbout))
  object Existing:
    given Show[Existing] = e => s"id = ${e.id} | created_at = ${e.createdAt} | updated_at = ${e.updatedAt} | ${e.data.show}"

  final case class Data(name: User.Name, about: User.About) extends User:
    override protected type ThisType = Data

    override def withUpdatedName(newName: User.Name): ThisType = copy(name = newName)

    override def withUpdatedAbout(newAbout: User.About): ThisType = copy(about = newAbout)
  object Data:
    given Show[Data] = d => s"""name = ${d.name} | about = ${d.about}"""

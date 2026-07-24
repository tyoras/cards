package io.tyoras.cards.cli.tui

import io.tyoras.cards.cli.tui.Size.{Big, Small}
import io.tyoras.cards.domain.card.Rank.*
import io.tyoras.cards.domain.card.*
import io.tyoras.cards.domain.card.Suit.{Club, Diamond, Heart, Spade}
import layoutz.*
import layoutz.Border.Round
import layoutz.Color.{Black, BrightWhite}

val cardWidth  = 12
val cardHeight = 11

enum Size:
  case Big
  case Small

def renderCard(card: Card, size: Size = Size.Big, title: Option[String] = None): Element =
  size match
    case Big   => renderBigCard(card, title)
    case Small => renderSmallCard(card, title)

def renderCardBack(size: Size = Size.Big, title: Option[String] = None): Element =
  size match
    case Big   => renderBigCardBack(title)
    case Small => renderSmallCardBack(title)

private def renderSmallCard(c: Card, title: Option[String] = None): Element =
  renderSmallCard(c.toString, title)

private def renderSmallCardBack(title: Option[String] = None): Element =
  renderSmallCard("? ", title)

private def renderSmallCard(value: String, title: Option[String]): Element =
  box(title.getOrElse(""))(value.leftAlign(5), empty, value.rightAlign(5)).border(Round).color(Black).colorBg(BrightWhite)

private def renderBigCard(card: Card, title: Option[String] = None): Element =
  val suit   = card.suit.toString
  val inside = card.rank match
    case Ace(_) =>
      Seq(
        "".leftAlign(cardWidth - 1),
        empty,
        empty,
        suit.center(),
        empty,
        empty,
        "".rightAlign(cardWidth - 1)
      )
    case King(_) =>
      Seq(
        "".leftAlign(cardWidth - 1),
        empty,
        empty,
        suit.center(),
        empty,
        empty,
        "".rightAlign(cardWidth - 1)
      )
    case Queen(_) =>
      Seq(
        "".leftAlign(cardWidth - 1),
        empty,
        empty,
        suit.center(),
        empty,
        empty,
        "".rightAlign(cardWidth - 1)
      )
    case Jack(_) =>
      Seq(
        "".leftAlign(cardWidth - 1),
        empty,
        empty,
        suit.center(),
        empty,
        empty,
        "".rightAlign(cardWidth - 1)
      )
    case Ten(_) =>
      Seq(
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        suit.center(),
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        suit.center(),
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1))
      )
    case Nine(_) =>
      Seq(
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        suit.center(),
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1))
      )
    case Eight(_) =>
      Seq(
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1))
      )
    case Seven(_) =>
      Seq(
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        suit.center(),
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        empty,
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1))
      )
    case Six(_) =>
      Seq(
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1))
      )
    case Five(_) =>
      Seq(
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        empty,
        suit.center(),
        empty,
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1))
      )
    case Four(_) =>
      Seq(
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1)),
        empty,
        empty,
        empty,
        empty,
        empty,
        row(suit.leftAlign((cardWidth - 2) / 2), suit.rightAlign((cardWidth - 2) / 2 - 1))
      )
    case Three(_) =>
      Seq(
        suit.center(),
        empty,
        empty,
        suit.center(),
        empty,
        empty,
        suit.center()
      )
    case Two(_) =>
      Seq(
        suit.center(),
        empty,
        empty,
        empty,
        empty,
        empty,
        suit.center()
      )

  box(title.getOrElse(""))(
    card.rank.toString.leftAlign(cardWidth),
    layout(inside*).center(),
    card.rank.toString.rightAlign(cardWidth)
  ).border(Round).color(Black).colorBg(BrightWhite)

private def renderBigCardBack(title: Option[String] = None): Element =
  box(title.getOrElse(""))(
    row(Diamond.toString.leftAlign((cardWidth - 1) / 2), Club.toString.rightAlign(cardWidth / 2)),
    layout(
      "".leftAlign(cardWidth - 1),
      empty,
      row(Spade.toString, Diamond.toString, Club.toString, Heart.toString).center(),
      row(Heart.toString, Spade.toString, Diamond.toString, Club.toString).center(),
      row(Club.toString, Heart.toString, Spade.toString, Diamond.toString).center(),
      empty,
      "".rightAlign(cardWidth - 1)
    ).center(),
    row(Spade.toString.leftAlign((cardWidth - 1) / 2), Heart.toString.rightAlign(cardWidth / 2))
  ).border(Round).color(Black).colorBg(BrightWhite)

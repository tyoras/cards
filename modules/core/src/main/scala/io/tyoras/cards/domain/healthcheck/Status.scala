package io.tyoras.cards.domain.healthcheck

import cats.data.NonEmptyList
import io.tyoras.cards.domain.healthcheck.Status.*

enum Status:
  case UP
  case DEGRADED
  case DOWN

sealed trait HealthStatus:
  def name: String
  def status: Status
  def components: List[HealthStatus]

object HealthStatus:
  case class ComponentHealth(name: String, status: Status) extends HealthStatus:
    override val components: List[HealthStatus] = List.empty
  object ComponentHealth:
    def down(name: String): HealthStatus =
      ComponentHealth(name, DOWN)

    def up(name: String): HealthStatus =
      ComponentHealth(name, UP)

  case class CompositeHealth(name: String, comps: NonEmptyList[HealthStatus]) extends HealthStatus:
    override val components: List[HealthStatus] = comps.toList
    override lazy val status: Status            =
      if components.exists(_.status == DOWN)
      then DOWN
      else if components.exists(_.status == DEGRADED)
      then DEGRADED
      else UP

  def of(name: String, components: List[HealthStatus] = List.empty, defaultStatus: Status = UP): HealthStatus =
    NonEmptyList.fromList(components).fold(ComponentHealth(name, defaultStatus))(CompositeHealth(name, _))

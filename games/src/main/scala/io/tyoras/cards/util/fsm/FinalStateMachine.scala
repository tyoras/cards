package io.tyoras.cards.util.fsm

trait FinalStateMachine[F[_], A] {
  def getCurrentState: F[A]

  def transition(f: A => F[A]): F[A]
}

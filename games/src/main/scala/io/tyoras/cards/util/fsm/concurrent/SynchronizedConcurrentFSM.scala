package io.tyoras.cards.util.fsm.concurrent

import cats.effect._
import cats.effect.concurrent._
import cats.implicits._
import io.tyoras.cards.util.fsm.FinalStateMachine

object SynchronizedConcurrentFSM {
  def create[F[_] : Concurrent, A](a: A): F[FinalStateMachine[F, A]] =
    (Ref.of[F, A](a), Semaphore(1)).mapN { (state, semaphore) =>
      new FinalStateMachine[F, A] {
        override def getCurrentState: F[A] = state.get

        override def transition(f: A => F[A]): F[A] = semaphore.withPermit {
          getCurrentState.flatMap(f).flatMap { newA =>
            state.updateAndGet(_ => newA)
          }
        }
      }
    }
}

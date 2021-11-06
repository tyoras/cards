package io.tyoras.cards.util.fsm.concurrent

import cats.effect._
import cats.effect.std.Semaphore
import cats.syntax.all._
import io.tyoras.cards.util.fsm.FinalStateMachine

object SynchronizedConcurrentFSM:
  def create[F[_] : Concurrent, A](a: A): F[FinalStateMachine[F, A]] =
    (Ref.of[F, A](a), Semaphore(1)).mapN { (state, semaphore) =>
      new FinalStateMachine[F, A] {
        override def getCurrentState: F[A] = state.get

        override def transition(f: A => F[A]): F[A] = semaphore.permit.surround {
          getCurrentState.flatMap(f).flatMap { newA =>
            state.updateAndGet(_ => newA)
          }
        }
      }
    }

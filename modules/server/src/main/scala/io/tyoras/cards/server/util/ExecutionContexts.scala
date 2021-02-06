package io.tyoras.cards.server.util

import cats.effect.{Resource, Sync}
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext

object ExecutionContexts {

  def fixedThreadPool[F[_] : Sync](size: Int): Resource[F, ExecutionContext] =
    initEC(F.delay(Executors.newFixedThreadPool(size)))

  def cachedThreadPool[F[_] : Sync]: Resource[F, ExecutionContext] =
    initEC(F.delay(Executors.newCachedThreadPool))

  private def initEC[F[_] : Sync](acquireES: F[ExecutorService]): Resource[F, ExecutionContext] = {
    val release = (es: ExecutorService) => F.delay(es.shutdown())
    Resource.make(acquireES)(release).map(ExecutionContext.fromExecutor)
  }

}

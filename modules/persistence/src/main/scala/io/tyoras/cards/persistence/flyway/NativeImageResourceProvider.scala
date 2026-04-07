package io.tyoras.cards.persistence.flyway

import org.flywaydb.core.api.ResourceProvider
import org.flywaydb.core.api.resource.LoadableResource

import java.io.{InputStreamReader, Reader}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*

class NativeImageResourceProvider extends ResourceProvider {

  private val basePath    = "db/migration"
  private val classLoader = Thread.currentThread().getContextClassLoader.nn

  private def listFiles(): Seq[String] =
    val stream = classLoader.getResourceAsStream(basePath)
    if stream == null then Seq.empty
    else
      try
        val bytes = stream.nn.readAllBytes()
        String(bytes, StandardCharsets.UTF_8).linesIterator.filter(line => line.nonEmpty && line.endsWith(".sql")).toSeq
      finally stream.nn.close()

  private def makeResource(filename: String): LoadableResource = new LoadableResource {
    override def read(): Reader =
      val stream = classLoader.getResourceAsStream(s"$basePath/$filename")
      if stream == null then throw new RuntimeException(s"Migration resource not found: $basePath/$filename")
      InputStreamReader(stream.nn, StandardCharsets.UTF_8)

    override def getAbsolutePath: String       = s"$basePath/$filename"
    override def getAbsolutePathOnDisk: String = s"$basePath/$filename"
    override def getFilename: String           = filename
    override def getRelativePath: String       = filename
  }

  override def getResource(name: String): LoadableResource | Null =
    val probe = classLoader.getResourceAsStream(s"$basePath/$name")
    if probe == null then null
    else
      probe.nn.close()
      makeResource(name)

  override def getResources(prefix: String, suffixes: Array[String]): java.util.Collection[LoadableResource] =
    listFiles().filter(f => f.startsWith(prefix) && suffixes.exists(s => s != null && f.endsWith(s))).map(makeResource).asJava
}

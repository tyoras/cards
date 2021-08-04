import sbt._

object Dependencies {

  case object ch {
    case object qos {
      case object logback {
        val `logback-classic` = "ch.qos.logback" % "logback-classic" % "1.2.5"
      }
    }
  }

  case object com {
    case object github {
      case object pureconfig {
        val pureconfigVersion = "0.16.0"
        val pureconfig = "com.github.pureconfig" %% "pureconfig"                           % pureconfigVersion
        val `pureconfig-cats-effect` = "com.github.pureconfig" %% "pureconfig-cats-effect" % pureconfigVersion
      }
    }
    case object monovore {
      val declineVersion = "2.1.0"
      val decline = "com.monovore" %% "decline"                 % declineVersion
      val `decline-effect` = "com.monovore" %% "decline-effect" % declineVersion
    }

    case object olegpy {
      val `better-monadic-for` = "com.olegpy" %% "better-monadic-for" % "0.3.1"
    }
  }

  case object io {
    case object chrisdavenport {
      val `cats-effect-time` = "io.chrisdavenport" %% "cats-effect-time" % "0.2.0"
      val fuuidVersion = "0.8.0-M2"
      val fuuid = "io.chrisdavenport" %% "fuuid"                 % fuuidVersion
      val `fuuid-circe` = "io.chrisdavenport" %% "fuuid-circe"   % fuuidVersion
      val `fuuid-http4s` = "io.chrisdavenport" %% "fuuid-http4s" % fuuidVersion
    }

    case object circe {
      val circeVersion = "0.14.1"
      val `circe-core` = dep("core")
      val `circe-generic` = dep("generic")
      private def dep(artifact: String): ModuleID = "io.circe" %% s"circe-$artifact" % circeVersion
    }
  }

  case object org {
    case object augustjune {
      val `context-applied` = "org.augustjune" %% "context-applied" % "0.1.4"
    }
    case object flywaydb {
      val `flyway-core` = "org.flywaydb" % "flyway-core" % "7.11.4"
    }
    case object http4s {
      val http4sVersion = "0.23.0"
      val `http4s-blaze-server` = dep("blaze-server")
      val `http4s-circe` = dep("circe")
      val `http4s-dsl` = dep("dsl")

      private def dep(artifact: String): ModuleID = "org.http4s" %% s"http4s-$artifact" % http4sVersion
    }
    case object postgresql {
      val postgresql = "org.postgresql" % "postgresql" % "42.2.23"
    }
    case object scalacheck {
      val scalacheck = "org.scalacheck" %% "scalacheck" % "1.15.4"
    }
    case object scalatest {
      val scalatest = "org.scalatest" %% "scalatest" % "3.2.9"
    }
    case object scalatestplus {
      val `scalacheck-1-14` = "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0"
    }

    case object typelevel {
      val `cats-core` = "org.typelevel" %% "cats-core"           % "2.6.1"
      val `cats-effect` = "org.typelevel" %% "cats-effect"       % "3.2.0"
      val `kind-projector` = "org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full
      val `log4cats-slf4j` = "org.typelevel" %% "log4cats-slf4j" % "2.1.1"
    }
  }

  case object tpolecat {
    val `skunk-core` =
      "org.tpolecat" %% "skunk-core" % "0.2.0"
  }

  lazy val coreDeps = Seq(
    io.chrisdavenport.`cats-effect-time`,
    io.chrisdavenport.fuuid,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`,
    org.typelevel.`log4cats-slf4j`
  )

  lazy val coreTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest,
    org.scalatestplus.`scalacheck-1-14`
  ).map(_ % Test)

  lazy val configDeps = Seq(
    com.github.pureconfig.pureconfig,
    com.github.pureconfig.`pureconfig-cats-effect`,
    org.typelevel.`cats-effect`
  )

  lazy val configTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest
  ).map(_ % Test)

  lazy val persistenceDeps = Seq(
    io.chrisdavenport.`cats-effect-time`,
    io.chrisdavenport.fuuid,
    org.flywaydb.`flyway-core`,
    org.postgresql.postgresql,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`,
    org.typelevel.`log4cats-slf4j`,
    tpolecat.`skunk-core`
  )

  lazy val persistenceTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest,
    org.scalatestplus.`scalacheck-1-14`
  ).map(_ % Test)

  lazy val cliDeps = Seq(
    ch.qos.logback.`logback-classic`,
    com.monovore.decline,
    com.monovore.`decline-effect`,
    io.chrisdavenport.`cats-effect-time`,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`,
    org.typelevel.`log4cats-slf4j`
  )

  lazy val cliTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest
  ).map(_ % Test)

  lazy val serverDeps = Seq(
    ch.qos.logback.`logback-classic`,
    io.chrisdavenport.fuuid,
    io.chrisdavenport.`fuuid-circe`,
    io.chrisdavenport.`fuuid-http4s`,
    io.circe.`circe-core`,
    io.circe.`circe-generic`,
    org.http4s.`http4s-blaze-server`,
    org.http4s.`http4s-circe`,
    org.http4s.`http4s-dsl`,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`
  )

  lazy val serverTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest
  ).map(_ % Test)

}

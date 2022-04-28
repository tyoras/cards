import sbt._

object Dependencies {

  case object ch {
    case object qos {
      case object logback {
        val `logback-classic` = "ch.qos.logback" % "logback-classic" % "1.2.11"
      }
    }
  }

  case object com {
//    case object github {
//      case object pureconfig {
//        val pureconfigVersion = "0.17.0"
//        val pureconfig = "com.github.pureconfig" %% "pureconfig"                           % pureconfigVersion
//        val `pureconfig-cats-effect` = "com.github.pureconfig" %% "pureconfig-cats-effect" % pureconfigVersion
//      }
//    }

    case object monovore {
      val declineVersion = "2.2.0"
      val decline = "com.monovore" %% "decline"                 % declineVersion
      val `decline-effect` = "com.monovore" %% "decline-effect" % declineVersion
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
    case object flywaydb {
      val `flyway-core` = "org.flywaydb" % "flyway-core" % "8.5.10"
    }

    case object http4s {
      val http4sVersion = "0.23.11"
      val `http4s-blaze-server` = dep("blaze-server")
      val `http4s-circe` = dep("circe")
      val `http4s-dsl` = dep("dsl")

      private def dep(artifact: String): ModuleID = "org.http4s" %% s"http4s-$artifact" % http4sVersion
    }

    case object postgresql {
      val postgresql = "org.postgresql" % "postgresql" % "42.3.4"
    }

    case object scalacheck {
      val scalacheck = "org.scalacheck" %% "scalacheck" % "1.16.0"
    }

    case object scalatest {
      val scalatest = "org.scalatest" %% "scalatest" % "3.2.12"
    }

    case object scalatestplus {
      val `scalacheck-1-15` = "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0"
    }

    case object tpolecat {
      val skunkVersion = "0.3.1"
      val `skunk-core` =
        "org.tpolecat" %% "skunk-core" % skunkVersion
      val `skunk-circe` =
        "org.tpolecat" %% "skunk-circe" % skunkVersion
    }

    case object typelevel {
      val `cats-core` = "org.typelevel" %% "cats-core"           % "2.7.0"
      val `cats-effect` = "org.typelevel" %% "cats-effect"       % "3.3.11"
      val `kind-projector` = "org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full
      val `log4cats-slf4j` = "org.typelevel" %% "log4cats-slf4j" % "2.2.0"
    }
  }

  lazy val coreDeps = Seq(
    io.chrisdavenport.`cats-effect-time`,
    io.chrisdavenport.fuuid,
    io.circe.`circe-core`,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`,
    org.typelevel.`log4cats-slf4j`
  )

  lazy val coreTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest,
    org.scalatestplus.`scalacheck-1-15`
  ).map(_ % Test)

  lazy val configDeps = Seq(
//    com.github.pureconfig.pureconfig,
//    com.github.pureconfig.`pureconfig-cats-effect`,
    org.typelevel.`cats-effect`
  )

  lazy val configTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest
  ).map(_ % Test)

  lazy val persistenceDeps = Seq(
    io.chrisdavenport.`cats-effect-time`,
    io.chrisdavenport.fuuid,
    io.circe.`circe-core`,
    org.flywaydb.`flyway-core`,
    org.postgresql.postgresql,
    org.tpolecat.`skunk-core`,
    org.tpolecat.`skunk-circe`,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`,
    org.typelevel.`log4cats-slf4j`
  )

  lazy val persistenceTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest,
    org.scalatestplus.`scalacheck-1-15`
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

import sbt.*

object Dependencies {

  case object ch {
    case object qos {
      case object logback {
        val `logback-classic` = "ch.qos.logback" % "logback-classic" % "1.5.34"
      }
    }
  }

  case object com {
    case object github {
      case object `jwt-scala` {
        val jwtScalaVersion = "11.0.4"
        val `jwt-core`      = "com.github.jwt-scala" %% "jwt-core"  % jwtScalaVersion
        val `jwt-circe`     = "com.github.jwt-scala" %% "jwt-circe" % jwtScalaVersion

      }
      case object pureconfig {
        val pureconfigVersion        = "0.17.10"
        val `pureconfig-core`        = "com.github.pureconfig" %% "pureconfig-core"        % pureconfigVersion
        val `pureconfig-cats-effect` = "com.github.pureconfig" %% "pureconfig-cats-effect" % pureconfigVersion
        val `pureconfig-http4s`      = "com.github.pureconfig" %% "pureconfig-http4s"      % pureconfigVersion
      }
    }

    case object monovore {
      val declineVersion   = "2.6.2"
      val decline          = "com.monovore" %% "decline"        % declineVersion
      val `decline-effect` = "com.monovore" %% "decline-effect" % declineVersion
    }
  }

  case object dev {
    case object profunktor {
      val http4sJwtAuthVersion = "2.0.15"
      val `http4s-jwt-auth`    = "dev.profunktor" %% "http4s-jwt-auth" % http4sJwtAuthVersion
    }
  }

  case object io {
    case object chrisdavenport {
      val `cats-effect-time` = "io.chrisdavenport" %% "cats-effect-time" % "0.2.1"
      val fuuidVersion       = "0.8.0-M2"
      val fuuid              = "io.chrisdavenport" %% "fuuid"            % fuuidVersion
      val `fuuid-circe`      = "io.chrisdavenport" %% "fuuid-circe"      % fuuidVersion
      val `fuuid-http4s`     = "io.chrisdavenport" %% "fuuid-http4s"     % fuuidVersion
    }

    case object circe {
      val `circe-core` = "io.circe" %% s"circe-core" % "0.14.14"
    }

    case object github {
      case object iltotore {
        val ironVersion       = "3.3.1"
        val iron              = "io.github.iltotore" %% "iron"            % ironVersion
        val `iron-cats`       = "io.github.iltotore" %% "iron-cats"       % ironVersion
        val `iron-circe`      = "io.github.iltotore" %% "iron-circe"      % ironVersion
        val `iron-pureconfig` = "io.github.iltotore" %% "iron-pureconfig" % ironVersion
      }
    }

    case object scalaland {
      val chimney = "io.scalaland" %% "chimney" % "1.10.0"
    }
  }

  case object org {
    case object flywaydb {
      val `flyway-database-postgresql` = "org.flywaydb" % "flyway-database-postgresql" % "12.7.0"
    }

    case object http4s {
      val http4sVersion            = "0.23.33"
      val `http4s-ember-server`    = dep("ember-server")
      val `http4s-circe`           = dep("circe")
      val `http4s-dsl`             = dep("dsl")
      val `http4s-jdk-http-client` = "org.http4s" %% "http4s-jdk-http-client" % "0.10.0"

      private def dep(artifact: String): ModuleID = "org.http4s" %% s"http4s-$artifact" % http4sVersion
    }

    case object postgresql {
      val postgresql = "org.postgresql" % "postgresql" % "42.7.11"
    }

    case object scalacheck {
      val scalacheck = "org.scalacheck" %% "scalacheck" % "1.19.0"
    }

    case object scalatest {
      val scalatest = "org.scalatest" %% "scalatest" % "3.2.20"
    }

    case object scalatestplus {
      val `scalacheck-1-15` = "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0"
    }

    case object tpolecat {
      val skunkVersion = "1.0.0"
      val `skunk-core` =
        "org.tpolecat" %% "skunk-core" % skunkVersion
      val `skunk-circe` =
        "org.tpolecat" %% "skunk-circe" % skunkVersion
    }

    case object typelevel {
      val `cats-core`                     = "org.typelevel" %% "cats-core"                     % "2.13.0"
      val `cats-effect`                   = "org.typelevel" %% "cats-effect"                   % "3.7.0"
      val `cats-effect-testing-scalatest` = "org.typelevel" %% "cats-effect-testing-scalatest" % "1.8.0"
      val `cats-parse`                    = "org.typelevel" %% "cats-parse"                    % "1.1.0"
      val `log4cats-slf4j`                = "org.typelevel" %% "log4cats-slf4j"                % "2.8.0"
    }
  }

  case object xyz {
    case object matthieucourt {
      val layoutz = "xyz.matthieucourt" %% "layoutz" % "0.7.0"
    }
  }

  lazy val coreDeps = Seq(
    com.github.`jwt-scala`.`jwt-core`,
    com.github.`jwt-scala`.`jwt-circe`,
    dev.profunktor.`http4s-jwt-auth`,
    io.chrisdavenport.`cats-effect-time`,
    io.chrisdavenport.fuuid,
    io.chrisdavenport.`fuuid-circe`,
    io.circe.`circe-core`,
    io.github.iltotore.iron,
    io.github.iltotore.`iron-circe`,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`,
    org.typelevel.`cats-parse`,
    org.typelevel.`log4cats-slf4j`
  )

  lazy val coreTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest,
    org.scalatestplus.`scalacheck-1-15`,
    org.typelevel.`cats-effect-testing-scalatest`
  ).map(_ % Test)

  lazy val persistenceDeps = Seq(
    io.chrisdavenport.`cats-effect-time`,
    io.chrisdavenport.fuuid,
    io.circe.`circe-core`,
    org.flywaydb.`flyway-database-postgresql`,
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
    com.github.pureconfig.`pureconfig-core`,
    com.github.pureconfig.`pureconfig-cats-effect`,
    com.github.pureconfig.`pureconfig-http4s`,
    com.monovore.decline,
    com.monovore.`decline-effect`,
    io.chrisdavenport.`cats-effect-time`,
    org.http4s.`http4s-circe`,
    org.http4s.`http4s-jdk-http-client`,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`,
    org.typelevel.`log4cats-slf4j`,
    xyz.matthieucourt.layoutz
  )

  lazy val cliTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest,
    org.typelevel.`cats-effect-testing-scalatest`
  ).map(_ % Test)

  lazy val serverDeps = Seq(
    ch.qos.logback.`logback-classic`,
    com.github.`jwt-scala`.`jwt-core`,
    com.github.`jwt-scala`.`jwt-circe`,
    com.github.pureconfig.`pureconfig-core`,
    com.github.pureconfig.`pureconfig-cats-effect`,
    dev.profunktor.`http4s-jwt-auth`,
    io.chrisdavenport.fuuid,
    io.chrisdavenport.`fuuid-circe`,
    io.chrisdavenport.`fuuid-http4s`,
    io.scalaland.chimney,
    io.circe.`circe-core`,
    org.http4s.`http4s-ember-server`,
    org.http4s.`http4s-circe`,
    org.http4s.`http4s-dsl`,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`
  )

  lazy val serverTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest
  ).map(_ % Test)

  lazy val sharedDeps = Seq(
    io.chrisdavenport.fuuid,
    io.chrisdavenport.`fuuid-circe`,
    io.circe.`circe-core`,
    io.scalaland.chimney,
    org.typelevel.`cats-core`
  )

  lazy val sharedTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest
  ).map(_ % Test)
}

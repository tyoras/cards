import sbt._

object Dependencies {

  case object ch {
    case object qos {
      case object logback {
        val `logback-classic` = "ch.qos.logback" % "logback-classic" % "1.2.3"
      }
    }
  }

  case object com {
    case object monovore {
      val declineVersion = "1.4.0"
      val decline = "com.monovore" %% "decline"                 % declineVersion
      val `decline-effect` = "com.monovore" %% "decline-effect" % declineVersion
    }

    case object olegpy {
      val `better-monadic-for` = "com.olegpy" %% "better-monadic-for" % "0.3.1"
    }
  }

  case object dev {
    case object profunktor {
      val console4cats = "dev.profunktor" %% "console4cats" % "0.8.1"
    }
  }

  case object io {
    case object chrisdavenport {
      val `cats-effect-time` = "io.chrisdavenport" %% "cats-effect-time" % "0.1.2"
      val fuuid = "io.chrisdavenport" %% "fuuid"                         % "0.5.0"
    }
  }

  case object org {
    case object augustjune {
      val `context-applied` = "org.augustjune" %% "context-applied" % "0.1.4"
    }
    case object scalacheck {
      val scalacheck = "org.scalacheck" %% "scalacheck" % "1.15.3"
    }
    case object scalatest {
      val scalatest = "org.scalatest" %% "scalatest" % "3.2.7"
    }
    case object scalatestplus {
      val `scalacheck-1-14` = "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0"
    }

    case object typelevel {
      val `cats-core` = "org.typelevel" %% "cats-core"           % "2.5.0"
      val `cats-effect` = "org.typelevel" %% "cats-effect"       % "2.5.0"
      val `log4cats-slf4j` = "org.typelevel" %% "log4cats-slf4j" % "1.2.2"
    }
  }

  lazy val coreTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest,
    org.scalatestplus.`scalacheck-1-14`
  ).map(_ % Test)

  lazy val gamesDeps = Seq(
    io.chrisdavenport.`cats-effect-time`,
    io.chrisdavenport.fuuid,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`,
    org.typelevel.`log4cats-slf4j`
  )

  lazy val gamesTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest,
    org.scalatestplus.`scalacheck-1-14`
  ).map(_ % Test)

  lazy val cliDeps = Seq(
    ch.qos.logback.`logback-classic`,
    com.monovore.decline,
    com.monovore.`decline-effect`,
    dev.profunktor.console4cats,
    io.chrisdavenport.`cats-effect-time`,
    org.typelevel.`cats-core`,
    org.typelevel.`cats-effect`,
    org.typelevel.`log4cats-slf4j`
  )

  lazy val cliTestDeps = Seq(
    org.scalacheck.scalacheck,
    org.scalatest.scalatest
  ).map(_ % Test)

}

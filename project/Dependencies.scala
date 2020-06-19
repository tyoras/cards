import sbt._

object Dependencies {
  lazy val cats = "org.typelevel" %% "cats-core"             % "2.1.1"
  lazy val catEffect = "org.typelevel" %% "cats-effect"      % "2.1.3"
  lazy val console4cats = "dev.profunktor" %% "console4cats" % "0.8.1"
  val declineVersion = "1.2.0"
  lazy val decline = "com.monovore" %% "decline"              % declineVersion
  lazy val declineEffect = "com.monovore" %% "decline-effect" % declineVersion
  lazy val fuuid = "io.chrisdavenport" %% "fuuid"             % "0.4.0"
  lazy val log4cats = "io.chrisdavenport" %% "log4cats-slf4j" % "1.1.1"
  lazy val logback = "ch.qos.logback"                         % "logback-classic" % "1.2.3"
  lazy val scalaTest = "org.scalatest" %% "scalatest"         % "3.2.0"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck"      % "1.14.3"

  lazy val betterFor = addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

  lazy val coreDeps = Seq(
    scalaTest  % Test,
    scalaCheck % Test
  )

  lazy val gamesDeps = Seq(
    cats,
    catEffect,
    fuuid,
    log4cats,
    scalaTest  % Test,
    scalaCheck % Test
  )

  lazy val cliDeps = Seq(
    cats,
    catEffect,
    console4cats,
    decline,
    declineEffect,
    log4cats,
    logback,
    scalaTest  % Test,
    scalaCheck % Test
  )
}

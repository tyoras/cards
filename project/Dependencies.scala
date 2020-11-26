import sbt._

object Dependencies {
  lazy val cats = "org.typelevel" %% "cats-core"                      % "2.3.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect"              % "2.2.0"
  lazy val catsEffectTime = "io.chrisdavenport" %% "cats-effect-time" % "0.1.2"
  lazy val console4cats = "dev.profunktor" %% "console4cats"          % "0.8.1"
  val declineVersion = "1.3.0"
  lazy val decline = "com.monovore" %% "decline"                            % declineVersion
  lazy val declineEffect = "com.monovore" %% "decline-effect"               % declineVersion
  lazy val fuuid = "io.chrisdavenport" %% "fuuid"                           % "0.4.0"
  lazy val log4cats = "io.chrisdavenport" %% "log4cats-slf4j"               % "1.1.1"
  lazy val logback = "ch.qos.logback"                                       % "logback-classic" % "1.2.3"
  lazy val scalaTest = "org.scalatest" %% "scalatest"                       % "3.2.3"
  lazy val scalaCheckIntegration = "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck"                    % "1.15.1"

  lazy val betterFor = addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

  lazy val coreDeps = Seq(
    scalaTest             % Test,
    scalaCheckIntegration % Test,
    scalaCheck            % Test
  )

  lazy val gamesDeps = Seq(
    cats,
    catsEffect,
    catsEffectTime,
    fuuid,
    log4cats,
    scalaTest             % Test,
    scalaCheckIntegration % Test,
    scalaCheck            % Test
  )

  lazy val cliDeps = Seq(
    cats,
    catsEffect,
    catsEffectTime,
    console4cats,
    decline,
    declineEffect,
    log4cats,
    logback,
    scalaTest  % Test,
    scalaCheck % Test
  )
}

import Dependencies._
import scoverage.ScoverageKeys.coverageMinimum

lazy val commonSettings = Seq(
  organization := "io.tyoras",
  scalaVersion := "2.13.2",
  version := "0.1.0-SNAPSHOT",
  betterFor,
  scalacOptions in Scapegoat += "-P:scapegoat:overrideLevels:UnsafeTraversableMethods=Warning"
)

ThisBuild / scapegoatVersion := "1.4.4"
ThisBuild / scapegoatDisabledInspections := Seq("IncorrectlyNamedExceptions")
ThisBuild / coverageMinimum := 75
ThisBuild / coverageFailOnMinimum := false

lazy val root = (project in file("."))
  .settings(name := "cards", commonSettings)
  .aggregate(core, games, cli)

lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    scalaOpts,
    libraryDependencies ++= coreDeps,
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "io.tyoras.cards",
    buildInfoOptions += BuildInfoOption.BuildTime
  )
  .enablePlugins(BuildInfoPlugin)

lazy val games = (project in file("games"))
  .settings(commonSettings, scalaOpts, libraryDependencies ++= gamesDeps)
  .dependsOn(core)

lazy val cli = (project in file("cli"))
  .settings(commonSettings, packagingSettings, scalaOpts, libraryDependencies ++= cliDeps)
  .enablePlugins(JavaAppPackaging)
  .dependsOn(games)

lazy val scalaOpts = scalacOptions := Seq(
  "-Yrangepos",
  "-Xlint",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  //"-Xfatal-warnings", si possible
  "-Ywarn-unused",
  "-Ydelambdafy:method",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-encoding",
  "UTF-8"
)

lazy val packagingSettings = Seq(
  mainClass in Compile := Some("io.tyoras.cards.cli.Launcher")
)

import Dependencies._
import scoverage.ScoverageKeys.coverageMinimum

lazy val commonSettings = Seq(
  organization := "io.tyoras",
  scalaVersion := "2.13.2",
  version := "0.1.0-SNAPSHOT",
  betterFor,
  scalacOptions in Scapegoat += "-P:scapegoat:overrideLevels:UnsafeTraversableMethods=Warning",
  test in assembly := {}
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
    buildInfoOptions += BuildInfoOption.BuildTime,
    coverageExcludedPackages := ".*BuildInfo.scala"
  )
  .enablePlugins(BuildInfoPlugin)

lazy val games = (project in file("games"))
  .settings(
    commonSettings,
    scalaOpts,
    libraryDependencies ++= gamesDeps
  )
  .dependsOn(core)

lazy val cli = (project in file("cli"))
  .settings(
    commonSettings,
    packagingSettings,
    scalaOpts,
    libraryDependencies ++= cliDeps
  )
  .enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)
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
  mainClass in (Compile, assembly) := Some("io.tyoras.cards.cli.Launcher"),
  assemblyJarName in assembly := "cards.jar",
  graalVMNativeImageOptions ++= Seq(
    "--verbose",
    "--no-server",
    "--no-fallback",
    "--static",
    "--enable-http",
    "--enable-https",
    "--enable-all-security-services",
    "--report-unsupported-elements-at-runtime",
    "--allow-incomplete-classpath",
    "-H:+ReportExceptionStackTraces",
    "-H:+ReportUnsupportedElementsAtRuntime",
    "-H:+TraceClassInitialization",
    "-H:+PrintClassInitialization",
    "-H:+RemoveSaturatedTypeFlows",
    "-H:+StackTrace",
    "-H:+JNI",
    "-H:-SpawnIsolates",
    "-H:-UseServiceLoaderFeature",
    "-H:UseMuslC=../../../bundle/",
    "--install-exit-handlers",
    "--initialize-at-build-time=scala.runtime.Statics$VM,ch.qos.logback.core.boolex.JaninoEventEvaluatorBase"
  )
)

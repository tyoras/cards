import Dependencies._
import scoverage.ScoverageKeys.coverageMinimum

ThisBuild / organization := "io.tyoras"
ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalacOptions ++= Seq(
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

lazy val commonSettings = Seq(
  update / evictionWarningOptions := EvictionWarningOptions.empty,
  addCompilerPlugin(com.olegpy.`better-monadic-for`),
  addCompilerPlugin(org.augustjune.`context-applied`),
  Scapegoat / scalacOptions += "-P:scapegoat:overrideLevels:UnsafeTraversableMethods=Warning",
  assembly / test := {}
)

ThisBuild / scapegoatVersion := "1.4.9"
ThisBuild / scapegoatDisabledInspections := Seq("IncorrectlyNamedExceptions")
ThisBuild / coverageMinimum := 75
ThisBuild / coverageFailOnMinimum := false

Global / lintUnusedKeysOnLoad := false

lazy val cards = (project in file("."))
  .aggregate(core, games, cli)

lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    libraryDependencies ++= coreTestDeps,
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "io.tyoras.cards",
    buildInfoOptions += BuildInfoOption.BuildTime,
    coverageExcludedPackages := ".*BuildInfo.scala"
  )
  .enablePlugins(BuildInfoPlugin)

lazy val games = (project in file("games"))
  .settings(
    commonSettings,
    libraryDependencies ++= gamesDeps ++ gamesTestDeps
  )
  .dependsOn(core)

lazy val cli = (project in file("cli"))
  .settings(
    commonSettings,
    packagingSettings,
    libraryDependencies ++= cliDeps ++ cliTestDeps
  )
  .enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)
  .dependsOn(games)



lazy val packagingSettings = Seq(
  Compile / assembly / mainClass := Some("io.tyoras.cards.cli.Launcher"),
  assembly / assemblyJarName := "cards.jar",
  graalVMNativeImageOptions ++= Seq(
    "--verbose",
    "--no-server",
    "--no-fallback",
    "--static",
    "--libc=musl",
    "--enable-http",
    "--enable-https",
    "--enable-all-security-services",
    "--report-unsupported-elements-at-runtime",
    "--allow-incomplete-classpath",
    "-H:+ReportExceptionStackTraces",
    "-H:+ReportUnsupportedElementsAtRuntime",
    "-H:+PrintClassInitialization",
    "-H:+RemoveSaturatedTypeFlows",
    "-H:ReflectionConfigurationFiles=/build/reflect-config.json",
    "-H:+StackTrace",
    "-H:+JNI",
    "-H:-SpawnIsolates",
    "-H:-UseServiceLoaderFeature",
    "--install-exit-handlers",
    "--initialize-at-build-time=scala.runtime.Statics$VM,ch.qos.logback.core.boolex.JaninoEventEvaluatorBase"
  )
)

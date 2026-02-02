import Dependencies.*

ThisBuild / organization := "io.tyoras"
ThisBuild / scalaVersion := "3.7.4"

ThisBuild / tlBaseVersion    := "0.1"
ThisBuild / scapegoatVersion := "3.2.4"
ThisBuild / tlFatalWarnings  := false // no need for fatal warnings in this project

lazy val commonSettings = Seq(
  update / evictionWarningOptions := EvictionWarningOptions.empty,
  scalafmtPrintDiff               := true
)

ThisBuild / coverageMinimumStmtTotal := 75
ThisBuild / coverageFailOnMinimum    := false

Global / lintUnusedKeysOnLoad := false

lazy val cards = (project in file(".")).aggregate(core, persistence, cli, server)

lazy val core = (project in file("modules/core"))
  .settings(
    commonSettings,
    libraryDependencies ++= coreDeps ++ coreTestDeps,
    buildInfoKeys    := Seq[BuildInfoKey](version),
    buildInfoPackage := "io.tyoras.cards",
    buildInfoOptions += BuildInfoOption.BuildTime,
    coverageExcludedPackages := ".*BuildInfo.scala"
  )
  .enablePlugins(BuildInfoPlugin)

lazy val persistence = (project in file("modules/persistence"))
  .settings(
    commonSettings,
    libraryDependencies ++= persistenceDeps ++ persistenceTestDeps
  )
  .dependsOn(core)

lazy val cli = (project in file("modules/cli"))
  .settings(
    commonSettings,
    cliPackagingSettings,
    libraryDependencies ++= cliDeps ++ cliTestDeps
  )
  .enablePlugins(NativeImagePlugin)
  .dependsOn(core)

lazy val server = (project in file("modules/server"))
  .settings(
    commonSettings,
    serverPackagingSettings,
    libraryDependencies ++= serverDeps ++ serverTestDeps
  )
  .enablePlugins(NativeImagePlugin)
  .dependsOn(core, persistence)

lazy val cliPackagingSettings = Seq(
  Compile / mainClass := Some("io.tyoras.cards.cli.Launcher")
) ++ graalVMPackagingSettings

lazy val serverPackagingSettings = Seq(
  Compile / mainClass := Some("io.tyoras.cards.server.Main")
) ++ graalVMPackagingSettings

lazy val graalVMPackagingSettings = Seq(
  nativeImageOptions ++= Seq(
    "-H:+ReportExceptionStackTraces",
    "--initialize-at-run-time=scala.util.Random",
    "--initialize-at-run-time=org.slf4j.LoggerFactory",
    "--initialize-at-run-time=org.slf4j.MDC",
    "--initialize-at-build-time=scala.runtime.Statics$VM",
    "--no-fallback",
    "--static",
    "--enable-http",
    "--enable-https"
  ),
  nativeImageVersion := "22.3.3"
)

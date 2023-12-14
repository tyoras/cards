import Dependencies.*

ThisBuild / organization := "io.tyoras"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version      := "0.1.0-SNAPSHOT"

ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  // "-Xfatal-warnings", si possible
  "-language:higherKinds",
  "-language:implicitConversions",
  "-encoding",
  "UTF-8"
)

lazy val commonSettings = Seq(
  update / evictionWarningOptions := EvictionWarningOptions.empty
)

ThisBuild / coverageMinimumStmtTotal := 75
ThisBuild / coverageFailOnMinimum    := false

Global / lintUnusedKeysOnLoad := false

lazy val cards = (project in file(".")).aggregate(core, persistence, cli, config, server)

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

lazy val config = (project in file("modules/config")).settings(
  commonSettings,
  libraryDependencies ++= configDeps ++ configTestDeps
)

lazy val persistence = (project in file("modules/persistence"))
  .settings(
    commonSettings,
    libraryDependencies ++= persistenceDeps ++ persistenceTestDeps
  )
  .dependsOn(core, config)

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
  .dependsOn(core, config, persistence)

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

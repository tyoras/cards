import Dependencies.*

ThisBuild / organization := "io.tyoras"
ThisBuild / scalaVersion := "3.8.3"

ThisBuild / tlBaseVersion    := "0.1"
ThisBuild / scapegoatVersion := "3.3.4"
ThisBuild / tlFatalWarnings  := false // no need for fatal warnings in this project
ThisBuild / tlJdkRelease     := Some(25)

lazy val commonSettings = Seq(
  update / evictionWarningOptions := EvictionWarningOptions.empty,
  scalafmtPrintDiff               := true
)

ThisBuild / coverageMinimumStmtTotal := 75
ThisBuild / coverageFailOnMinimum    := false

Global / lintUnusedKeysOnLoad := false

lazy val cards = (project in file(".")).aggregate(core, persistence, cli, server, shared)

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
    run / connectInput := true,
    cliPackagingSettings,
    libraryDependencies ++= cliDeps ++ cliTestDeps
  )
  .enablePlugins(NativeImagePlugin)
  .dependsOn(core, shared)

lazy val server = (project in file("modules/server"))
  .settings(
    commonSettings,
    serverPackagingSettings,
    libraryDependencies ++= serverDeps ++ serverTestDeps
  )
  .enablePlugins(NativeImagePlugin)
  .dependsOn(core, persistence, shared)

lazy val shared = (project in file("modules/shared"))
  .settings(
    commonSettings,
    libraryDependencies ++= sharedDeps ++ sharedTestDeps
  ).dependsOn(core)

lazy val cliPackagingSettings = Seq(
  Compile / mainClass := Some("io.tyoras.cards.cli.remote.Launcher")
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
    "--initialize-at-build-time=ch.qos.logback,ch.qos.logback.classic.Logger",
    "--no-fallback",
    "--static",
    "--libc=musl",
    "--enable-http",
    "--enable-https",
    "-march=compatibility" //so it is compatible with low end machines architecture
  ),
  nativeImageVersion := "25.0.1",
  nativeImageJvm := "graalvm-java25"
)

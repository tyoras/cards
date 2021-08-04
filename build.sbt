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
  "-Xlint:-byname-implicit",
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
  .aggregate(core, persistence, cli, config, server)

lazy val core = (project in file("modules/core"))
  .settings(
    commonSettings,
    libraryDependencies ++= coreDeps ++ coreTestDeps,
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "io.tyoras.cards",
    buildInfoOptions += BuildInfoOption.BuildTime,
    coverageExcludedPackages := ".*BuildInfo.scala"
  )
  .enablePlugins(BuildInfoPlugin)

lazy val config = (project in file("modules/config"))
  .settings(
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
  .enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)
  .dependsOn(core)

lazy val server = (project in file("modules/server"))
  .settings(
    commonSettings,
    serverPackagingSettings,
    libraryDependencies ++= serverDeps ++ serverTestDeps
  )
  .enablePlugins(JavaAppPackaging, GraalVMNativeImagePlugin)
  .dependsOn(core, config, persistence)


lazy val cliPackagingSettings = Seq(
  Compile / assembly / mainClass := Some("io.tyoras.cards.cli.Launcher"),
  assembly / assemblyJarName := "cards-cli.jar",
) ++ graalVMPackagingSettings

lazy val serverPackagingSettings = Seq(
  Compile / assembly / mainClass := Some("io.tyoras.cards.server.Main"),
  assembly / assemblyJarName := "cards-server.jar",
) ++ graalVMPackagingSettings

lazy val graalVMPackagingSettings = Seq(
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
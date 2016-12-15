import sbt._
import Keys._

object MyBuild extends Build {
  lazy val root = project.in(file(".")).aggregate(macros, rules)

  lazy val dotty = RootProject(uri("git://github.com/dotty-linker/dotty"))

  lazy val macros = project.settings(
    scalaVersion := "2.11.8",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2",
    libraryDependencies += "org.scalameta" %% "scalameta" % "1.1.0",
    addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M5" cross CrossVersion.full)
    )

  lazy val rules = project.settings(
    scalaVersion := "2.11.8",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2",
    libraryDependencies += "org.scalameta" %% "scalameta" % "1.1.0",
    addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M5" cross CrossVersion.full),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "200", "-workers", "1", "-verbosity", "1")
  ).dependsOn(macros).dependsOn(dotty)

  parallelExecution in Test in rules := false
}

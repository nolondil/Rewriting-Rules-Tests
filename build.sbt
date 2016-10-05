name := "Tests Rules"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2"
libraryDependencies += "org.scalameta" %% "scalameta" % "1.1.0"

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M5" cross CrossVersion.full)


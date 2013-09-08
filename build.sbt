scalaVersion := "2.10.2"

lazy val core = project

lazy val gen = project.dependsOn(core)
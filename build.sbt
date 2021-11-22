import Dependencies._

ThisBuild / organization  := "org.pchapin"
ThisBuild / version       := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion  := "2.13.7"   // Think about upgrading to Scala 3.1.0. Check libs!
ThisBuild / scalacOptions :=
  Seq("-encoding", "UTF-8", // Encoding of the source files.
      "-feature",
      "-deprecation",       // Tell us about deprecated things.
      "-unchecked",
      "-Wunused:nowarn",    // Warn if the nowarn annotation doesn't actually suppress a warning.
      "-Xsource:3",         // Help us migrate to Scala 3 by forbidding somethings and allowing others.
      "-Ywarn-dead-code",
      "-Ywarn-value-discard")

Test / logBuffered := false

lazy val nessie = (project in file("."))
  .settings(
    name := "Nessie",
    libraryDependencies ++= nessieDeps,
  )

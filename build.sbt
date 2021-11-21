import Dependencies._

ThisBuild / organization  := "org.pchapin"
ThisBuild / version       := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion  := "2.11.12"   // Think about upgrading to Scala 3.1.0. Check libs!
ThisBuild / scalacOptions :=
  Seq("-encoding", "UTF-8", // Encoding of the source files.
      "-feature",
      "-deprecation",       // Tell us about deprecated things.
      "-unchecked",
      "-Ywarn-dead-code",
      "-Ywarn-value-discard")

Test / logBuffered := false

lazy val nessie = (project in file("."))
  .settings(
    name := "Nessie",
    libraryDependencies ++= nessieDeps,
  )

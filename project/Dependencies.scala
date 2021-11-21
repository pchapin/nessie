import sbt._

// See: https://www.scala-sbt.org/1.x/docs/Organizing-Build.html
object Dependencies {

  // Versions
  lazy val scalaTestVersion = "2.1.7"
  // lazy val scalaGraphCoreVersion = "1.13.2"

  // Kiama and ScalaZ don't appear to have binaries for Scala 2.13.x.
  // Fortunately, they are not needed at this time.
  //
  // lazy val kiamaVersion = "2.2.0"
  // lazy val scalazVersion = "7.3.0-SNAPSHOT"

  // Libraries
  // val scalactic = "org.scalactic" %% "scalactic" % scalaTestVersion
  val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  // val scalaGraphCore = "org.scala-graph" %% "graph-core" % scalaGraphCoreVersion
  // val kiama = "org.bitbucket.inkytonik.kiama" %% "kiama" % kiamaVersion
  // val scalaz = "org.scalaz" %% "scalaz" % scalazVersion

  // Projects
  val nessieDeps = Seq(scalaTest % Test)
}

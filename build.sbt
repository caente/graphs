scalaVersion := "2.11.8"

organization := "caente"

resolvers ++= Seq(
  Resolver.bintrayRepo("stanch", "maven"),
  Resolver.bintrayRepo("drdozer", "maven")
)

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.14.0",
  "org.typelevel" %% "cats" % "0.7.2",
  "com.github.mpilquist" %% "simulacrum" % "0.8.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "com.lihaoyi" % "ammonite" % "0.7.8" % "test" cross CrossVersion.full,
  "org.stanch" %% "reftree" % "0.7.2"
)


initialCommands in (Test,console) := s"""
 ammonite.Main().run(
   "a" -> 1
)
"""

initialCommands in console := """
  import cats._
  import cats.syntax.eq._
  import cats.syntax.functor._
  import cats.instances.all._
  import common.graph2._
  import common.graph.smallerAndEven
  """

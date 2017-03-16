scalaVersion := "2.11.8"

organization := "caente"

resolvers ++= Seq(
  Resolver.bintrayRepo("stanch", "maven"),
  Resolver.bintrayRepo("drdozer", "maven")
)


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.14.0",
  "com.github.mpilquist" %% "simulacrum" % "0.8.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "com.lihaoyi" % "ammonite" % "0.7.8" % "test" cross CrossVersion.full,
  "com.slamdata" %% "matryoshka-core" % "0.18.0",
  "org.scalaz" %% "scalaz-core" % "7.2.9"
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
val nodes = Map(
    1 -> List(3),
    2 -> List(3),
    3 -> List(5),
    4 -> List(6),
    5 -> List(6),
    6 -> List()
  )
  implicit val relation: Relation[Int] = (x, y) => nodes(x).contains(y)
  """

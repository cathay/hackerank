name := "algorithm"

version := "1.0"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-deprecation")

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.typesafeRepo("releases")

libraryDependencies ++=Seq(
  "junit" % "junit" % "4.10" % "test",
  "org.mockito" % "mockito-all" % "1.10.19" % "test",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  "org.scala-graph" % "graph-core_2.11" % "1.12.0"
)
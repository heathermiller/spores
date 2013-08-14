scalaVersion := "2.10.2"

organization := "org.scala-lang"

name := "scala-spores"

version := "0.1-SNAPSHOT"

libraryDependencies <++= (scalaVersion) {
  sv => Seq(
    "org.scala-lang" % "scala-reflect" % sv % "provided",
    "org.scala-lang" % "scala-compiler" % sv % "provided"
  )
}
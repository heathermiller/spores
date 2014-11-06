package scala.spores

import sbt._

object Dependencies {

  val scalaVersion = "2.11.2"

  val scalaReflect = "org.scala-lang" % "scala-reflect" % scalaVersion % "provided"
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scalaVersion % "test"

  val junit = "junit" % "junit" % "4.10" % "test"
  val junitIntf = "com.novocode" % "junit-interface" % "0.8" % "test"

  val scalaPickling = "org.scala-lang" %% "scala-pickling" % "0.9.1-SNAPSHOT"

  val core = Seq(scalaReflect, scalaCompiler, junit, junitIntf)

  val pickling = core ++ Seq(scalaPickling)

}

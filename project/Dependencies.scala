package scala.spores

import sbt._

object Dependencies {

  val scalaVersion = "2.11.7"

  val scalaReflect = "org.scala-lang" % "scala-reflect" % scalaVersion % "provided"
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scalaVersion % "test"

  val junit = "junit" % "junit" % "4.12" % "test"
  val junitIntf = "com.novocode" % "junit-interface" % "0.11" % "test"

  val scalaPickling = "org.scala-lang.modules" %% "scala-pickling" % "0.11.0-M1"

  val core = Seq(scalaReflect, scalaCompiler, junit, junitIntf)

  val pickling = core ++ Seq(scalaPickling)

}

package scala.spores

import sbt._
import Keys._

object SporesBuild extends Build {

  lazy val buildSettings = Seq(
    organization := "org.scala-lang.modules",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := "2.11.2"
  )

  lazy val root = Project(
    id = "spores",
    base = file(".")
  )

  lazy val core = Project(
    id = "spores-core",
    base = file("core")
  )

  lazy val pickling = Project(
    id = "spores-pickling",
    base = file("spores-pickling")
  ) dependsOn(core)

  override lazy val settings =
    super.settings ++
    buildSettings

  lazy val defaultSettings = Seq(
    scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-target:jvm-1.6", "-deprecation", "-feature", "-unchecked", "-Xlog-reflective-calls", "-Xlint")
  )

}

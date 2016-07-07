package scala.spores

import sbt._
import Keys._

object SporesBuild extends Build {

  lazy val buildSettings = Seq(
    organization := "org.scala-lang.modules",
    organizationName := "LAMP/EPFL",
    organizationHomepage := Some(new URL("http://lamp.epfl.ch")),
    version      := "0.2.4",
    scalaVersion := "2.11.7"
  )

  lazy val publishSettings = Seq(
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/heathermiller/spores</url>
      <inceptionYear>2013</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html
          </url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/heathermiller/spores.git</url>
        <connection>scm:git:git://github.com/heathermiller/spores.git</connection>
      </scm>
      <developers>
        <developer>
          <id>heathermiller</id>
          <name>Heather Miller</name>
          <timezone>+1</timezone>
          <url>http://github.com/heathermiller</url>
        </developer>
        <developer>
          <id>phaller</id>
          <name>Philipp Haller</name>
          <timezone>+1</timezone>
          <url>http://github.com/phaller</url>
        </developer>
      </developers>
    ),
    credentials ++= loadCredentials()
  )

  def loadCredentials(): List[Credentials] = {
    val mavenSettingsFile = System.getProperty("maven.settings.file")
    if (mavenSettingsFile != null) {
      println("Loading Sonatype credentials from " + mavenSettingsFile)
      try {
        import scala.xml._
        val settings = XML.loadFile(mavenSettingsFile)
        def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
        List(Credentials(
          "Sonatype Nexus Repository Manager",
          "oss.sonatype.org",
          readServerConfig("username"),
          readServerConfig("password")
        ))
      } catch {
        case ex: Exception =>
          println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
          Nil
      }
    } else {
      Nil
    }
  }

  lazy val root = Project(
    id = "spores",
    base = file(".")
  )

  lazy val core = Project(
    id = "spores-core",
    base = file("core"),
    settings = buildSettings ++ publishSettings
  )

  lazy val pickling = Project(
    id = "spores-pickling",
    base = file("spores-pickling"),
    settings = buildSettings ++ publishSettings
  ) dependsOn(core)

  override lazy val settings =
    super.settings ++
    buildSettings

  lazy val defaultSettings = Seq(
    scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-target:jvm-1.6", "-deprecation", "-feature", "-unchecked", "-Xlog-reflective-calls", "-Xlint")
  )

}

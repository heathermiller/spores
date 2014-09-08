import scala.spores.{SporesBuild, Dependencies}

SporesBuild.defaultSettings

libraryDependencies ++= Dependencies.core

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

parallelExecution in Test := false

import scala.spores.{SporesBuild, Dependencies}

SporesBuild.defaultSettings

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Dependencies.pickling

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

parallelExecution in Test := false

// scalacOptions in Test ++= Seq("-Xlog-implicits")

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

libraryDependencies += "junit" % "junit-dep" % "4.10" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M2" % "test"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

scalacOptions in Test ++= Seq("-Xlog-implicits"/*, "-Ybrowse:typer"*/)

scalaVersion := "2.11.1"

organization := "org.scala-lang"

name := "scala-spores"

version := "0.1-SNAPSHOT"

libraryDependencies <++= (scalaVersion) {
  sv => Seq(
    "org.scala-lang" % "scala-reflect" % sv % "provided",
    "org.scala-lang" % "scala-compiler" % sv % "provided"
  )
}

libraryDependencies += "junit" % "junit-dep" % "4.11" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11-RC1" % "test"

libraryDependencies += "org.scala-lang" %% "scala-pickling" % "0.8.0"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s")

parallelExecution in Test := false

scalacOptions in Compile ++= Seq("-Xexperimental")

scalacOptions in Test ++= Seq("-Xlog-implicits"/*, "-Ybrowse:typer"*/)

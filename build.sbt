ThisBuild / scalaVersion := "3.5.2"

scalacOptions ++= Seq("-experimental")
libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value
libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.19" % "test")

lazy val lms = (project in file("lms3"))
lazy val tutorial = (project in file(".")).dependsOn(lms)

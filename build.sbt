name := "matrices"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % "2.12.4")

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.0.0" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
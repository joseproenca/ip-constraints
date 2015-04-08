// import AssemblyKeys._

// assemblySettings

test in assembly := {}

name := "picc"

version := "1.0"

scalaVersion := "2.10.5" // needed for z3

// exportJars := true

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9"

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.5"

//libraryDependencies += "org.scala-lang" % "scala-library" % "2.10.5"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies += "junit" % "junit" % "4.12"


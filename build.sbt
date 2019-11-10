name := "Unification"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"


val scalaTestVersion = "3.0.8"


val scalaTest = List(
  "org.scalactic" %% "scalactic"
).map(_ % scalaTestVersion)

libraryDependencies ++= scalaTest

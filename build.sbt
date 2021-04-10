name := "cats"

version := "0.1"

scalaVersion := "2.13.5"

libraryDependencies +=
  "org.typelevel" %% "cats-core" % "2.1.0"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
//  "-Ypartial-unification"
)

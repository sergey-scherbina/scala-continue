organization := "sergiy.shcherbyna"

name := "scala-continue"

version := "0.2"

lazy val scala2 = (project in file("scala2")).settings(
  scalaVersion := "2.13.2",
  crossScalaVersions := Seq("2.12.8"),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

lazy val scala3 = (project in file("scala3")).settings(
  scalaVersion := "0.25.0-RC2",
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
)

lazy val root = (project in file("."))
  .aggregate(scala2, scala3)
  .settings(
    crossScalaVersions := Nil,
    publish / skip := true
  )

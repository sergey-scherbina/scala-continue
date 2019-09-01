name := "scala-continue"

version := "0.2"

scalaVersion := "0.18.1-RC1"

// libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

// use the %%% operator for Scala.js
libraryDependencies += "com.eed3si9n.verify" %% "verify" % "0.1.0" % Test

testFrameworks += new TestFramework("verify.runner.Framework")

scalacOptions ++= {
  if (isDotty.value) Seq("-language:Scala2", "-Xignore-scala2-macros") else Nil
}

libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))

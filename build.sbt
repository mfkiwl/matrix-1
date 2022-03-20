// See README.md for license details.

ThisBuild / scalaVersion     := "2.12.9"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "SIST"

val chiselVersion = "3.5.1"

lazy val root = (project in file("."))
  .settings(
    name := "matrix",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chisel-iotesters" % "2.5.0",
      "edu.berkeley.cs" %% "rocketchip" % "1.2.6",
      "org.scalatest" %% "scalatest" % "3.0.5",
      "edu.berkeley.cs" %% "hardfloat" % "1.2.4",
      "edu.berkeley.cs" %% "chiseltest" % "0.5.0" % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  )


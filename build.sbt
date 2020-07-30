version := "0.0.1-SNAPSHOT"

organization := "mlgruby"

scalaVersion := "2.13.3"

watchTriggeredMessage := Watch.clearScreenOnTrigger

initialCommands in console := "import native.collections._"

addCommandAlias("testc", ":clean;coverage;test;coverageReport")

libraryDependencies ++=
  Seq(
    "org.scalatest" %% "scalatest" % "3.0.8" % "test"
  )

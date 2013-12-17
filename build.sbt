seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

name := """text-cleaner"""

version := "1.0"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.apache.commons" % "commons-lang3" % "3.1"
)

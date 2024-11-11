ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "untitled4",
    libraryDependencies += "com.github.samtools" % "htsjdk" % "2.24.1" // Add HTSJDK dependency
  )

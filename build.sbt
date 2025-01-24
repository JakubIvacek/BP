ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.4"
//fork := true
lazy val root = (project in file("."))
  .settings(
    name := "untitled4",
    javaOptions += "-Xmx8G",
    libraryDependencies += "com.github.samtools" % "htsjdk" % "2.24.1", // Add HTSJDK dependency
    libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.5.0",
    libraryDependencies += "com.lihaoyi" %% "ujson" % "2.0.0" ,
    libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.41.2.2",
    libraryDependencies += "commons-net" % "commons-net" % "3.9.0",
    libraryDependencies += "org.rogach" %% "scallop" % "4.1.0"


  )

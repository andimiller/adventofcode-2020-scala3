val dottyVersion = "3.0.0-M2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= List(
      "org.typelevel" % "cats-parse_2.13" % "0.1.0",
      "co.fs2" % "fs2-io_2.13" % "2.4.6",
    ),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )

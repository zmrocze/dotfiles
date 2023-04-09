val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "resolution",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"
    // libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.8.0",
    
  )

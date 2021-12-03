
ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "smirkle",
    showCursor := true,
    title := "Smirkle",
    gameAssetsDirectory := "assets",
    windowStartWidth := 1280, // Width of Electron window, used with `indigoRun`.
    windowStartHeight := 720, // Height of Electron window, used with `indigoRun`.
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.2" % "test",
      "com.beachape" %%% "enumeratum" % "1.6.1",
      "org.typelevel" %% "cats-core" % "2.1.1",
      "io.indigoengine" %%% "indigo" % "0.6.0",
      "io.indigoengine" %%% "indigo-json-circe" % "0.6.0"
    )
  )
  .enablePlugins(ScalaJSPlugin, SbtIndigo)

addCommandAlias("buildGame", ";compile;fastOptJS;indigoBuild")
addCommandAlias("runGame", ";compile;fastOptJS;indigoRun")
addCommandAlias("buildGameFull", ";compile;fullOptJS;indigoBuildFull")
addCommandAlias("runGameFull", ";compile;fullOptJS;indigoRunFull")

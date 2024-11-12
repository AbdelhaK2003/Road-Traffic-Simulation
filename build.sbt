import Dependencies._

ThisBuild / scalaVersion     := "2.13.15" 
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "Road_Traffic_Simulation",
    libraryDependencies ++= Seq(
      "org.scalanlp" %% "breeze" % "2.1.0",
      "org.scalafx" %% "scalafx" % "16.0.0-R24",
      "org.openjfx" % "javafx-controls" % "19" classifier "win", 
      "org.openjfx" % "javafx-fxml" % "19" classifier "win",
      "org.openjfx" % "javafx-media" % "19" classifier "win"
    )
  )

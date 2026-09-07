lazy val main = project
  .in(file("."))
  .settings(name    := "artificial-bacteria",
            version := "0.0.0",
            scalaVersion := "3.9.0",
            organization := "com.github.mbuzdalov",
            scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  )

import sbt.Keys.libraryDependencies

ThisBuild / scalaVersion := "3.6.4"

val project_name = "scalajs-shell"
val project_version = "1.0.0-SNAPSHOT"

val artifactPrefix = "target/scala-2.11/" + project_name + "-" + project_version

scalacOptions ++= Seq("-feature", "-deprecation")

lazy val root = (project in file("."))
  .settings(
    organization := "org.enricobn",
    name := project_name,
    version := project_version,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    libraryDependencies += "org.enricobn" %%% "scalajs-vfs" % "1.0.0-SNAPSHOT" changing(),
    libraryDependencies += "org.enricobn" %%% "scalajs-terminal" % "1.0.0" changing(),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
    libraryDependencies += "org.scalamock" %%% "scalamock" % "7.3.0" % Test
  )
  .enablePlugins(ScalaJSPlugin)

scalacOptions ++= Seq(
  "-deprecation"
)
    
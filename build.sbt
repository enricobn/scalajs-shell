import sbt.Keys.libraryDependencies

ThisBuild / scalaVersion := "2.13.16"

val project_name = "scalajs-shell"
val project_version = "1.0.0-SNAPSHOT"

val artifactPrefix = "target/scala-2.11/" + project_name + "-" + project_version

scalacOptions ++= Seq("-feature", "-deprecation")

/*
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

 */

lazy val root = (project in file("."))
  .settings(
    organization := "org.enricobn",
    name := project_name,
    version := project_version,
    //artifactPath in(Compile, fullOptJS) := baseDirectory.value / (artifactPrefix + ".min.js"),
    //&&artifactPath in(Compile, packageJSDependencies) := baseDirectory.value / (artifactPrefix + "-jsdeps.js"),
    //artifactPath in(Compile, packageMinifiedJSDependencies) := baseDirectory.value / (artifactPrefix + "-jsdeps.min.js"),
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    libraryDependencies += "org.enricobn" %%% "scalajs-vfs" % "1.0.0-SNAPSHOT" changing(),
    libraryDependencies += "org.enricobn" %%% "scalajs-terminal" % "1.0.0" changing(),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
    libraryDependencies += "org.scalamock" %%% "scalamock" % "7.3.0" % Test
  )
  .enablePlugins(ScalaJSPlugin)

scalacOptions ++= Seq(
  "-Xsource:3",
  "-deprecation"
)
    
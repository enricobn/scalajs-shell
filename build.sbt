val project_name = "scalajs-shell"
val project_version = "1.0.0"

val artifactPrefix = "target/scala-2.11/" + project_name + "-" + project_version


lazy val root = (project in file("."))
    .settings(
      scalaVersion := "2.11.8",
      organization := "org.enricobn",
      name := project_name,
      version := project_version,
      artifactPath in (Compile, fullOptJS) := baseDirectory.value / (artifactPrefix + ".min.js"),
      artifactPath in (Compile, packageJSDependencies) := baseDirectory.value / (artifactPrefix + "-jsdeps.js"),
      artifactPath in (Compile, packageMinifiedJSDependencies) := baseDirectory.value / (artifactPrefix + "-jsdeps.min.js"),
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1",
      libraryDependencies += "org.enricobn" %%% "scalajs-vfs" % "1.0.0",
      libraryDependencies += "org.enricobn" %%% "scalajs-terminal" % "1.0.0",
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
      libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.3.0" % "test"
    )
    .enablePlugins(ScalaJSPlugin)
    
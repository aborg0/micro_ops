name := "micro_ops"

version := "0.1"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.7",
  organization := "com.github.aborg0"
)
lazy val scalaReflect = Def.setting { "org.scala-lang" % "scala-reflect" % scalaVersion.value }
lazy val scalaCompiler = Def.setting { "org.scala-lang" % "scala-compiler" % scalaVersion.value }

lazy val core = (project in file("."))
  .dependsOn(macroSub)
  .settings(
    commonSettings,
    // other settings here
  )

lazy val macroSub = (project in file("macro"))
  .settings(
    commonSettings,
    libraryDependencies += scalaReflect.value,
//    libraryDependencies += scalaCompiler.value,
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.0.0"
    // other settings here
  )


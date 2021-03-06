import sbt._
import sbt.Keys._

object MyBuild extends Build {

  // Dependencies

  lazy val machinist = "org.typelevel" %% "machinist" % "0.3.0"
  lazy val discipline = "org.typelevel" %% "discipline" % "0.2.1"
  lazy val scalaMeter = "com.storm-enroute" %% "scalameter" % "0.6"

  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.12.2"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "2.2.4"
  lazy val spireScalaCheckBindings = "org.spire-math" %% "spire-scalacheck-binding" % "0.10.1"

  lazy val spire = "org.spire-math" %% "spire" % "0.10.1"
  lazy val jscience = "org.jscience" % "jscience" % "4.3.1"
  lazy val commonsMath = "org.apache.commons" % "commons-math3" % "3.4.1"

  lazy val noPublish = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

  // Settings

  override lazy val settings = super.settings ++ Seq(
    organization := "com.faacets",

    scalaVersion := "2.11.7",

    licenses := Seq("BSD-style" -> url("http://opensource.org/licenses/MIT")),
    homepage := Some(url("https://github.com/denisrosset/qalg")),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),

    scalacOptions ++= Seq(
      //"-no-specialization", // use this to build non-specialized jars
      "-Yinline-warnings",
      "-deprecation",
      "-unchecked",
//      "-optimize",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-feature"
    ),

    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += "bintray/non" at "http://dl.bintray.com/non/maven",

    // re-enable to check imports, or once scala's REPL manages to not
    // be useless under -Ywarn-unused-import.

    // scalacOptions in Compile := {
    //   CrossVersion.partialVersion(scalaVersion.value) match {
    //     case Some((2, 10)) =>
    //       scalacOptions.value
    //     case Some((2, n)) if n >= 11 =>
    //       scalacOptions.value ++ Seq("-Ywarn-unused-import")
    //   }
    // },

    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          libraryDependencies.value

        // in Scala 2.10, quasiquotes are provided by macro-paradise
        case Some((2, 10)) =>
          libraryDependencies.value ++ Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
            "org.scalamacros" %% "quasiquotes" % "2.0.1")
      }
    }
  )

  // Main

  lazy val qalg = Project("qalg", file(".")).
    aggregate(macros, indup, core, scalacheckBinding, tests, benchmark).
    settings(qalgSettings: _*)

  lazy val qalgSettings = Seq(
    name := "qalg-aggregate"
  ) ++ noPublish

    // Macros

  lazy val macros = Project("macros", file("macros")).
    settings(macroSettings: _*)

  lazy val macroSettings = Seq(
    name := "qalg-macros",
    libraryDependencies ++= Seq(machinist, spire, scalaTest % "test", scalaCheck % "test"),
    unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / s"scala_${scalaBinaryVersion.value}"
  )

  // Indup

  lazy val indup = Project("indup", file("indup")).
    settings(indupSettings: _*).
    dependsOn(macros)

  lazy val indupSettings = Seq(
    name := "qalg-indup",
    libraryDependencies ++= Seq(
      spire,
      machinist,
      scalaCheck % "test",
      scalaTest % "test"
    )
  )

  // Core

  lazy val core = Project("core", file("core")).
    settings(coreSettings: _*).
    dependsOn(indup)

  lazy val coreSettings = Seq(
    name := "qalg-core",
    libraryDependencies ++= Seq(
      spire,
      jscience,
      commonsMath,
      scalaCheck % "test",
      scalaTest % "test"
    ),
    unmanagedBase := baseDirectory.value / "../lib"
  )

  // Extras

  lazy val extras = Project("extras", file("extras")).
    settings(extrasSettings: _*)

  lazy val extrasSettings = Seq(
    name := "qalg-extras",
    libraryDependencies ++= Seq(
      spire,
      jscience,
      commonsMath,
      scalaCheck % "test",
      scalaTest % "test"
    ),
    unmanagedBase := baseDirectory.value / "../lib"
  )

  // Scalacheck binding

  lazy val scalacheckBinding = Project("scalacheck-binding", file("scalacheck-binding")).
    settings(scalacheckSettings: _*).
    dependsOn(core)

  lazy val scalacheckSettings = Seq(
    name := "qalg-scalacheck-binding",
    libraryDependencies ++= Seq(
      discipline,
      scalaCheck,
      scalaTest,
      spireScalaCheckBindings
    )
  )

  // Tests

  lazy val tests = Project("tests", file("tests")).
    settings(testsSettings: _*).
    dependsOn(indup, core) //, scalacheckBinding)

  lazy val testsSettings = Seq(
    name := "qalg-tests",
    libraryDependencies ++= Seq(
      scalaTest % "test",
      spireScalaCheckBindings
    ),
    unmanagedBase := baseDirectory.value / "../lib"
  ) ++ noPublish


  // Benchmark

  lazy val benchmark: Project = Project("benchmark", file("benchmark")).
    settings(benchmarkSettings: _*).
    dependsOn(indup, core)

  lazy val benchmarkSettings = Seq(
    name := "qalg-benchmark",

    // raise memory limits here if necessary
    // TODO: this doesn't seem to be working with caliper at the moment :(
  
    javaOptions in run += "-Xmx4G",

    libraryDependencies ++= Seq(
      scalaMeter
    ),

    // enable forking in run
    fork in run := true
  ) ++ noPublish

}

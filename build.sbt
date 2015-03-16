name := "qalg"

organization := "com.faacets"

version := "0.9.1-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.spire-math" %% "spire" % "0.9.1",
  "org.jscience" % "jscience" % "4.3.1"
)

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation") 

initialCommands in console := """
import com.faacets.qalg._; import algebra._; import math._
import spire.implicits._; import spire.math._; import syntax.all._; import std.any._
import org.jlinalg.{Vector => JVector, Matrix => JMatrix, IRingElement, IRingElementFactory}
import org.jlinalg.rational.{Rational => JRational}
import org.jlinalg.doublewrapper.DoubleWrapper
"""

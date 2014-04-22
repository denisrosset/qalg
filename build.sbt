name := "Polyta"

version := "0.1"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.1" % "test",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "org.spire-math" %% "spire" % "0.7.3"
)

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation") 

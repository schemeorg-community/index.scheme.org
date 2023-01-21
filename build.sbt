ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.4"
libraryDependencies += "org.apache.solr" % "solr-solrj" % "8.11.1"
libraryDependencies += "org.apache.solr" % "solr-core" % "8.11.1"
libraryDependencies += "com.lihaoyi" %% "cask" % "0.8.3"
libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.14.1"
libraryDependencies += "ch.qos.logback" % "logback-core" % "1.2.6"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.6"


val http4sVersion = "1.0.0-M37"
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "io.circe" %% "circe-generic" % "0.14.3"
)


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

scalacOptions ++= Seq("-Xmax-inlines", "1000")


lazy val root = (project in file("."))
  .settings(
    name := "r7rs-index-site"
  )

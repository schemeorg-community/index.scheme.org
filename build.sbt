ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.7"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

libraryDependencies += "org.apache.solr" % "solr-solrj" % "8.11.1"
libraryDependencies += "org.apache.solr" % "solr-core" % "8.11.1"

val http4sVersion = "0.23.10"
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion
)



lazy val root = (project in file("."))
  .settings(
    name := "r7rs-index-site"
  )

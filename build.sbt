ThisBuild / version := "0.0.3"

ThisBuild / scalaVersion := "3.2.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.4"
libraryDependencies += "org.apache.solr" % "solr-solrj" % "8.11.1"
libraryDependencies += "org.apache.solr" % "solr-core" % "8.11.1"
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
    name := "r7rs-index-site",
    assembly / mainClass := Some("scmindex.Main")
  )

ThisBuild / assemblyMergeStrategy := {
  case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
  case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
  case "module-info.class" => MergeStrategy.discard
  case s if s.matches("^META-INF/.*\\.SF$") => MergeStrategy.discard
  case s if s.matches("^META-INF/.*\\.DSA$") => MergeStrategy.discard
  case s if s.matches("^META-INF/.*\\.RSA$") => MergeStrategy.discard
  case PathList("META-INF", xs @ _*) =>
      (xs map {_.toLowerCase}) match {
          case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) => MergeStrategy.discard
              case _ => MergeStrategy.last
      }
  case _  => MergeStrategy.last
}

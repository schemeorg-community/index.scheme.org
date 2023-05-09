ThisBuild / version := "0.0.3"

ThisBuild / scalaVersion := "3.2.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.4"

val solrVersion = "8.11.1"
libraryDependencies += ("org.apache.solr" % "solr-solrj" % solrVersion)
  .exclude("org.apache.logging.log4j", "log4j-slf4j-impl")
  .exclude("org.apache.logging.log4j", "log4j-core")
libraryDependencies += ("org.apache.solr" % "solr-core" % solrVersion)
  .exclude("org.apache.logging.log4j", "log4j-slf4j-impl")
  .exclude("org.apache.logging.log4j", "log4j-core")

val logbackVersion = "1.2.6"
libraryDependencies += "ch.qos.logback" % "logback-core" % logbackVersion
libraryDependencies += "ch.qos.logback" % "logback-classic" % logbackVersion

val http4sVersion = "1.0.0-M37"
libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
)

libraryDependencies += "io.circe" %% "circe-generic" % "0.14.3"
libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.14.1"
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.41.2.1"
libraryDependencies += "com.zaxxer" % "HikariCP" % "5.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

scalacOptions ++= Seq("-Xmax-inlines", "1000")

lazy val root = (project in file("."))
  .settings(
    name := "scheme-index",
    assemblyJarName in assembly := "scheme-index.jar",
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

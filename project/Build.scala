import sbt._
import Keys._

object FPInScalaBuild extends Build {

  val scalatest = "org.scalatest" %% "scalatest" % "2.0.M5b"
  val slf4j = "org.slf4j" % "slf4j-api" % "1.7.2"
  val logback = "ch.qos.logback" % "logback-classic" % "1.0.9"
  val mockito = "org.mockito" % "mockito-all" % "1.9.5"
  val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.1.0"


  val opts = Project.defaultSettings ++ Seq(
    scalaVersion := "2.10.0",
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    libraryDependencies ++= Seq(
        akkaActor,
        scalatest % "test",
        slf4j,
        logback,
        mockito % "test"
      )
  )

  lazy val root =
    Project(id = "fpinscala",
            base = file("."),
            settings = opts) aggregate (chapterCode, exercises, answers)
  lazy val chapterCode =
    Project(id = "chapter-code",
            base = file("chaptercode"),
            settings = opts)
  lazy val exercises =
    Project(id = "exercises",
            base = file("exercises"),
            settings = opts)
  lazy val answers =
    Project(id = "answers",
            base = file("answers"),
            settings = opts)
}


lazy val root = project
  .in(file("."))
  .settings(
    name := "CoqRepr",
    version := "1.0.0",
    organization := "Caballero Software Inc.",
    scalaVersion :=  "2.13.1",
    libraryDependencies ++= Seq(
    "org.apache.spark" %% "spark-core" % "3.2.1",
    "org.apache.spark" %% "spark-sql" % "3.2.1"
    )
  )
name := "dnadb_plot"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies += "io.suzaku" %% "boopickle" % "1.3.0"

libraryDependencies ++= Seq(
  "com.typesafe.slick"  %% "slick" % "3.2.3",
  "org.slf4j" % "slf4j-nop" % "1.7.25",
  "org.xerial" %  "sqlite-jdbc" % "3.25.2"
)
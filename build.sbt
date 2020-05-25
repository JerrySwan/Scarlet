lazy val root = (project in file(".")).
  settings(
    name := "Scarlet",
    version := "0.1.0",
    scalaVersion := "2.12.10"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "spire" % "0.14.1",
  "org.scalanlp" %% "breeze" % "1.0"
)

// End ///////////////////////////////////////////////////////////////

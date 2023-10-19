val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalablur",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-language:implicitConversions", "-deprecation"),
    libraryDependencies ++= Seq(
      ("com.storm-enroute" %% "scalameter-core" % "0.21")
        .cross(CrossVersion.for3Use2_13),
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
      "org.scalameta" %% "munit" % "0.7.26" % Test
    )
  )

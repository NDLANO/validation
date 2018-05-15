val Scalaversion = "2.12.2"
val ScalaTestVersion = "3.0.1"

lazy val commonSettings = Seq(
  organization := "ndla",
  scalaVersion := Scalaversion
)

lazy val validation = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "validation",
    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
    scalacOptions := Seq("-target:jvm-1.8", "-deprecation"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % ScalaTestVersion % "test",
      "org.jsoup" % "jsoup" % "1.10.3",
      "org.json4s" %% "json4s-native" % "3.5.0",
      "com.netaporter" %% "scala-uri" % "0.4.16"
    )
  )

val checkfmt = taskKey[Boolean]("Check for code style errors")
checkfmt := {
  val noErrorsInMainFiles = (Compile / scalafmtCheck).value
  val noErrorsInTestFiles = (Test / scalafmtCheck).value
  val noErrorsInSbtConfigFiles = (Compile / scalafmtSbtCheck).value

  noErrorsInMainFiles && noErrorsInTestFiles && noErrorsInSbtConfigFiles
}

Test / test := ((Test / test).dependsOn(Test / checkfmt)).value

val fmt = taskKey[Unit]("Automatically apply code style fixes")
fmt := {
  (Compile / scalafmt).value
  (Test / scalafmt).value
  (Compile / scalafmtSbt).value
}

publishTo := {
  val nexus = sys.props.getOrElse("nexus.host", "")
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/ndla-snapshots")
  else
    Some("releases" at nexus + "content/repositories/ndla-releases")
}

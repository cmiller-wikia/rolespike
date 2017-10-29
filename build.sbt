lazy val root = (project in file("."))
  .settings(CommonSettings.compiler)
  .settings(CommonSettings.console)
  .settings(Seq(
    organization := "fandom.com",
    name := "rolespike",
    description := "Polyglot Service Spike",
		fork := true
  ))
  .settings(Seq(
    libraryDependencies ++=
			CommonDeps.http4s ++
			CommonDeps.http4s_client ++
      CommonDeps.cats ++
			CommonDeps.logging ++
			CommonDeps.circe ++
			Seq(
				"org.typelevel" %% "cats-effect" % "0.3",
				"org.tpolecat" %% "doobie-core-cats" % "0.4.4",
				"com.h2database" % "h2" % "1.4.195"
			) ++
      CommonDeps.scalatest,
			addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  ))

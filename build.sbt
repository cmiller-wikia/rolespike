
lazy val AllTests = config("alltests") extend(Test)
def standaloneTestsFilter(name: String): Boolean = !(name contains "MySql")
def allTestsFilter(name: String): Boolean = true

lazy val root = (project in file("."))
	.configs(AllTests)
  .settings(CommonSettings.compiler)
  .settings(CommonSettings.console)
  .settings(Seq(
    organization := "fandom.com",
    name := "rolespike",
    description := "Polyglot Service Spike",
    fork := true
  ))
  .settings(
    libraryDependencies ++=
      CommonDeps.http4s ++
      CommonDeps.http4s_client ++
      CommonDeps.cats ++
      CommonDeps.logging ++
      CommonDeps.circe ++
      Seq(
        "org.typelevel" %% "cats-effect" % "0.3",
        "org.tpolecat" %% "doobie-core-cats" % "0.4.4",
        "com.h2database" % "h2" % "1.4.195",
				"mysql" % "mysql-connector-java" % "6.0.6"
      ) ++
      CommonDeps.scalatest,
		inConfig(AllTests)(Defaults.testTasks),
		testOptions in Test := Seq(Tests.Filter(standaloneTestsFilter)),
		testOptions in AllTests := Seq(Tests.Filter(allTestsFilter)),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )

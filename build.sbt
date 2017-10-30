import scalariform.formatter.preferences._

lazy val AllTests = config("alltests") extend(Test)
def standaloneTestsFilter(name: String): Boolean = !(name contains "MySql")
def allTestsFilter(name: String): Boolean = true

lazy val DefaultSettings =
  Seq(
    organization := "fandom.com",
    description := "Polyglot Service Spike",
    fork := true
  ) ++
  CommonSettings.compiler ++
  CommonSettings.console ++
  Seq(
    libraryDependencies ++=
    CommonDeps.http4s ++
    CommonDeps.cats ++
    CommonDeps.circe ++
    CommonDeps.scalatest ++
    Seq(
      "org.tpolecat" %% "doobie-core-cats" % "0.4.4",
      "com.h2database" % "h2" % "1.4.195",
      "mysql" % "mysql-connector-java" % "6.0.6"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    scalariformPreferences := scalariformPreferences.value
      .setPreference(RewriteArrowSymbols, true)
      .setPreference(DanglingCloseParenthesis, Force)
  )

lazy val base = (project in file("base-libs"))
  .settings(DefaultSettings)
  .settings(Seq(
    name := "non-specific shared code"
  ))

lazy val rolesvc = (project in file("role-service"))
  .configs(AllTests)
  .settings(DefaultSettings)
  .settings(
    Seq(
      name := "toy role service"
    ),
    inConfig(AllTests)(Defaults.testTasks),
    testOptions in Test := Seq(Tests.Filter(standaloneTestsFilter)),
    testOptions in AllTests := Seq(Tests.Filter(allTestsFilter))
  )
  .dependsOn(base % "compile->compile;test->test")

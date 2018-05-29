name := "thesis"

version := "0.1"

scalaVersion := "2.12.5"


addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full)

//autoCompilerPlugins := true

val monocleVersion = "1.5.0" // 1.5.0-cats based on cats 1.0.x
val circeVersion = "0.9.3"
val http4sVersion = "0.18.11"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.scala-graph" %% "graph-core" % "1.12.3",
  "org.scala-graph" %% "graph-constrained" % "1.12.3",
  "org.scala-graph" %% "graph-dot" % "1.12.1",
  "org.scala-graph" %% "graph-json" % "1.12.1",
  "org.typelevel" %% "cats-core" % "1.0.1",
  "org.typelevel" %% "cats-free" % "1.0.1",
  "org.typelevel" %% "cats-effect" % "0.10",
  "org.typelevel" %% "cats-mtl-core" % "0.2.1",
  "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-law"   % monocleVersion % "test",
  "com.kailuowang" %% "henkan-convert" % "0.6.1",
  "com.kailuowang" %% "henkan-optional" % "0.6.1",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.0",
  "org.scalanlp" %% "breeze" % "0.13.2",
  "org.scalanlp" %% "breeze-natives" % "0.13.2",
  "org.scalanlp" %% "breeze-viz" % "0.13.2",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.http4s"      %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"      %% "http4s-circe"        % http4sVersion,
  "org.http4s"      %% "http4s-dsl"          % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-generic-extras"
).map(_ % circeVersion)

libraryDependencies ++= Seq(

  // Start with this one
  "org.tpolecat" %% "doobie-core"      % "0.5.2",

  // And add any of these as needed
  "org.tpolecat" %% "doobie-h2"        % "0.5.2", // H2 driver 1.4.197 + type mappings.
  "org.tpolecat" %% "doobie-hikari"    % "0.5.2", // HikariCP transactor.
  "org.tpolecat" %% "doobie-postgres"  % "0.5.2", // Postgres driver 42.2.2 + type mappings.
  "org.tpolecat" %% "doobie-specs2"    % "0.5.2", // Specs2 support for typechecking statements.
  "org.tpolecat" %% "doobie-scalatest" % "0.5.2"  // ScalaTest support for typechecking statements.

)


scalacOptions += "-Ypartial-unification"

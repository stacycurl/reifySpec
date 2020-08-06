import sbt.Keys.resolvers

name := "reifySpec"

version := "0.1"

scalaVersion := "2.12.8"

val derivingVersion = "1.0.0"

val derivingVersionDependencies = Seq(
  // the @deriving and @xderiving plugin and macro
  "org.scalaz" %% "deriving-macro" % derivingVersion,
  compilerPlugin("org.scalaz" %% "deriving-plugin" % derivingVersion),
  // the scalaz-deriving Altz / Decidablez / Deriving API and macros
  "org.scalaz" %% "scalaz-deriving" % derivingVersion,
  // instances for Show and Arbitrary
  "org.scalaz" %% "scalaz-deriving-magnolia" % derivingVersion
)

/*
lazy val reify = (project in file("reify")
  settings (resolvers ++= Seq(
    "jcenter" at "http://jcenter.bintray.com"
  ))

  settings(libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  ) ++ derivingVersionDependencies)
)

lazy val echoSpec = (project in file("echoSpec")
  settings(libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic"  % "3.0.8",
    "org.scalatest" %% "scalatest"  % "3.0.8",
    "org.scalameta" %% "scalameta"  % "4.2.3",
    "com.lihaoyi"   %% "sourcecode" % "0.1.9"
  ) ++ derivingVersionDependencies)

  dependsOn(reify)
)
*/

lazy val reifySpec = (project in file(".")
  settings(libraryDependencies ++= Seq(
    "org.scalactic" %% "scalactic" % "3.0.8"  % "test",
    "org.scalatest" %% "scalatest" % "3.0.8"  % "test",
    "org.scalameta" %% "scalameta" % "4.2.3"  % "test"
  ) ++ derivingVersionDependencies)

  // Both reify and echoSpec need publishing
  // dependsOn(reify, echoSpec)
)

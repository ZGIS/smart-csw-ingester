/*
 * Copyright (c) 2011-2017 Interfaculty Department of Geoinformatics, University of
 * Salzburg (Z_GIS) & Institute of Geological and Nuclear Sciences Limited (GNS Science)
 * in the SMART Aquifer Characterisation (SAC) programme funded by the New Zealand
 * Ministry of Business, Innovation and Employment (MBIE)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.packager.docker._
import com.sksamuel.scapegoat.sbt._
import com.sksamuel.scapegoat.sbt.ScapegoatSbtPlugin.autoImport._
import scoverage.ScoverageKeys._
import java.util.Date

name := """smart-csw-ingester"""

version := "1.0-SNAPSHOT"

// new sbt-site 1.0.0 config SiteScaladocPlugin incompatible with activator sbt-site bundle 0.8.1
lazy val root = (project in file(".")).enablePlugins(PlayScala, BuildInfoPlugin, SiteScaladocPlugin, JavaAppPackaging, DockerPlugin)

scalaVersion := "2.11.8"

val luceneVersion = "6.4.0"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  filters,

//  "com.gilt" % "lib-lucene-sugar_2.11" % "0.2.3",

  "org.apache.lucene" % "lucene-core" % luceneVersion,
  "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
  "org.apache.lucene" % "lucene-queryparser" % luceneVersion,
  "org.apache.lucene" % "lucene-spatial" % luceneVersion,
  "org.apache.lucene" % "lucene-spatial-extras" % luceneVersion,
//  "com.vividsolutions" % "jts" 	% "1.14",
  "org.locationtech.spatial4j" % "spatial4j" % "0.6",

  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
  specs2 % Test
)

scalacOptions in ThisBuild ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation", // warning and location for usages of deprecated APIs
  "-feature", // warning and location for usages of features that should be imported explicitly
  "-unchecked", // additional warnings where generated code depends on assumptions
  "-Xlint:_", // recommended additional warnings
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
  "-Ywarn-unused-import", // Warn when imports are unused
  "-Ywarn-unused", // Warn when local and private vals, vars, defs, and types are unused
  "-Ywarn-numeric-widen", // Warn when numerics are widened, Int and Double, for instance
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-dead-code", // Warn when dead code is identified
  "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`
  "-Ywarn-nullary-override", //  Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit
  "-language:reflectiveCalls"
)

fork in run := true

// -----------------
// coverage, style and dependency checks

val genSiteDir = "src/site/generated"

// Scala style task for compile, config file is scalastyle-config.xml
lazy val compileScalastyle = taskKey[Unit]("compileScalastyle")
compileScalastyle := org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Compile).toTask("").value
(compile in Compile) <<= (compile in Compile) dependsOn compileScalastyle

// Scala style task to run with tests
lazy val testScalastyle = taskKey[Unit]("testScalastyle")
testScalastyle := org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Test).toTask("").value
(test in Test) <<= (test in Test) dependsOn testScalastyle

scapegoatVersion := "1.2.1"

scapegoatOutputPath := genSiteDir + "/scapegoat"

// scalacOptions only for the scapegoat task
scalacOptions in Scapegoat ++= Seq("-P:scapegoat:overrideLevels:TraversableHead=Warning:OptionGet=Warning")

// disabling coverage for standard tasks, call explicit in test runs / publish site
// coverageEnabled := true

lazy val coverageCopyTask = TaskKey[Unit]("copy-coverage")

coverageCopyTask := {
  println(s"Copying: ./target/scala-2.11/scoverage-report/ to $genSiteDir")
  val result = Seq("cp", "-r", "./target/scala-2.11/scoverage-report", genSiteDir + "/scoverage-report") !!
}

dependencyCheckOutputDirectory := Some(file(genSiteDir + "/dep-sec"))

// Use e.g. yEd to format the graph
dependencyGraphMLFile := file(genSiteDir + "/dep-sec/dependencies.graphml")

// Use e.g.graphviz to render
dependencyDotFile := file(genSiteDir + "/dep-sec/dependencies.dot")

// -----------------
// publish docs on github
// new sbt-site 1.0.0 config incompatible with activator sbt-site bundle 0.8.1
includeFilter in makeSite := "*.txt" | "*.html" | "*.md" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js"

// Puts ScalaDoc output in `target/site/latest/api`, will automatically be included with makeSite
siteSubdirName in SiteScaladoc := "latest/api"

previewLaunchBrowser := false

ghpages.settings

git.remoteRepo := "git@github.com:ZGIS/smart-csw-ingester.git"

// -----------------
// packaging options

val buildNumber = sys.props.getOrElse("buildNumber", default = System.currentTimeMillis().toString)

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, "buildNumber" -> buildNumber)

buildInfoPackage := "utils"

buildInfoObject := "BuildInfo"

version in Docker := version.value

maintainer in Docker := "allixender@googlemail.com"

dockerBaseImage in Docker := "java:8-jre"

dockerBaseImage := "java:8-jre"

sources in (Compile, doc) := Seq.empty

publishArtifact in (Compile, packageDoc) := false

javaOptions in Universal ++= Seq(
  // others will be added as app parameters
 // "-DapplyEvolutions.default=true",
  "-Dconfig.resource=application.conf",
  "-Dlogger.resource=logback-stdout.xml"
  //"-Dapplication.base_url=http://test.smart-project.info/"
)

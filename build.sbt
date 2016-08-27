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
import scoverage.ScoverageKeys._
import com.markatta.sbttaglist.TagListPlugin.tagListSettings

name := """smart-csw-ingester"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala).enablePlugins(JavaAppPackaging, DockerPlugin)

scalaVersion := "2.11.7"

val luceneVersion = "6.1.0"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,

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

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in ThisBuild ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation", // warning and location for usages of deprecated APIs
  "-feature", // warning and location for usages of features that should be imported explicitly
  "-unchecked", // additional warnings where generated code depends on assumptions
  "-Xlint", // recommended additional warnings
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
  "-Ywarn-inaccessible",
  "-Ywarn-dead-code",
  "-language:reflectiveCalls"
)

scapegoatVersion := "1.1.0"

coverageEnabled := true

target in Compile in doc := baseDirectory.value / "docs/api"

version in Docker := version.value
maintainer in Docker := "allixender@googlemail.com"
dockerBaseImage in Docker := "java:8-jre"
dockerBaseImage := "java:8-jre"

javaOptions in Universal ++= Seq(
  // others will be added as app parameters
 // "-DapplyEvolutions.default=true",
  "-Dconfig.resource=application.conf"
  //"-Dapplication.base_url=http://test.smart-project.info/"
)

fork in run := true

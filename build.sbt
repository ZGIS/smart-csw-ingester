import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import de.johoop.findbugs4sbt.FindBugs._
import com.typesafe.sbt.packager.docker._

name := """smart-csw-ingester"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala).enablePlugins(JavaAppPackaging, DockerPlugin)

scalaVersion := "2.11.7"

val luceneVersion = "4.7.2"
// val luceneVersion = "6.1.0"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,

//  "com.gilt" % "lib-lucene-sugar_2.11" % "0.2.3",

  "org.apache.lucene" % "lucene-core" % luceneVersion,
  "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
  "org.apache.lucene" % "lucene-queryparser" % luceneVersion,
//  "org.apache.lucene" % "lucene-spatial" % luceneVersion,
//  "org.apache.lucene" % "lucene-spatial-extras" % luceneVersion,
//  "com.vividsolutions" % "jts" 	% "1.13",
  "org.locationtech.spatial4j" % "spatial4j" % "0.6",

  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
  specs2 % Test
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

// resolvers += "Official Maven Repo" at "http://repo1.maven.org/maven2/"

// resolvers += "maven2 central" at "http://central.maven.org/maven2/"

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

findbugsSettings

findbugsExcludeFilters := Some(
  <FindBugsFilter>
    <!-- See docs/examples at http://findbugs.sourceforge.net/manual/filter.html -->
    <Match>
      <Class name="~views\.html\..*"/>
    </Match>
    <Match>
      <Class name="~Routes.*"/>
    </Match>
    <Match>
      <Class name="~controllers\.routes.*"/>
    </Match>
  </FindBugsFilter>
)

version in Docker := version.value

maintainer in Docker := "allixender@googlemail.com"

dockerBaseImage in Docker := "java:8-jre"

dockerBaseImage := "java:8-jre"

javaOptions in Universal ++= Seq(
  // others will be added as app parameters
 // "-DapplyEvolutions.default=true",
  "-Dconfig.resource=prod.application.conf"
  //"-Dapplication.base_url=http://test.smart-project.info/"
)

fork in run := true
import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "iteratees"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "org.pegdown" % "pegdown" % "1.1.0"
    )

    def customLessEntryPoints(base: File): PathFinder = (
        (base / "app" / "assets" / "stylesheets" / "bootstrap" * "bootstrap.less")
        +++ (base / "app" / "assets" / "stylesheets" * "*.less")
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      
      lessEntryPoints <<= baseDirectory(customLessEntryPoints)
    )

}

name := "imageJhairCountingOverRoot"
organization := ""
version := "0.0.1"

scalaVersion := "2.11.4"

libraryDependencies += "net.imagej" % "ij" % "1.49k"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"


fork := true

// Enable and customize `sbt-imagej` plugin
enablePlugins(SbtImageJ)
ijRuntimeSubDir := "sandbox"
ijPluginsSubDir := "ij-plugins"
ijCleanBeforePrepareRun := true
ijExclusions += """nativelibs4java\S*"""
// Instruct `clean` to delete created plugins subdirectory created by `ijRun`/`ijPrepareRun`.
cleanFiles += ijPluginsDir.value

name := "skew bionomial heap"
organization := "org.myproject"
version := "0.1.0"
scalaVersion := "2.12.6"

enablePlugins(JmhPlugin)

sourceDirectory in Jmh := (sourceDirectory in Runtime).value
classDirectory in Jmh := (classDirectory in Runtime).value
dependencyClasspath in Jmh := (dependencyClasspath in Runtime).value
// rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail)
compile in Jmh := (compile in Jmh).dependsOn(compile in Runtime).value
run in Jmh := (run in Jmh).dependsOn(Keys.compile in Jmh).evaluated

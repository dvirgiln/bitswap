organization := "com.david"
version := "0.1-SNAPSHOT"
name := "bitswap"
scalaVersion := "2.12.0"

val commonDependencies: Seq[ModuleID] = Seq(
  "org.scalatest" %% "scalatest" % "3.0.4",
  "org.slf4j" % "slf4j-log4j12" % "1.7.10",
  "log4j" % "log4j" % "1.2.17"
)

val meta = """META.INF(.)*""".r
val assemblySettings=Seq(assemblyMergeStrategy in assembly := {
  case PathList("javax", "servlet", xs@_*) => MergeStrategy.first
  case PathList(ps@_*) if ps.last endsWith ".html" => MergeStrategy.first
  case n if n.startsWith("reference.conf") => MergeStrategy.concat
  case n if n.endsWith(".conf") => MergeStrategy.concat
  case meta(_) => MergeStrategy.discard
  case x => MergeStrategy.first

})

val root = (project in file(".")).
  settings(
    libraryDependencies ++= commonDependencies,
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-Ypartial-unification",
      "-language:_"
    )
  )
//mainClass in Compile := Some("com.david.crawler.Main")

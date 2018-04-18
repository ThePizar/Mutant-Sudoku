name := "sudoku-variant-solver"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.10" % Test,
  "org.choco-solver" % "choco-solver" % "4.0.5"
)
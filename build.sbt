name := "maltese"

version := "1.0"

//scalaVersion := "2.11.7"
scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "edu.arizona.sista" % "processors" % "3.3",
  "edu.arizona.sista" % "processors" % "3.3" classifier "models"
)

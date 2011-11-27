
name                                    := "minei"

version                                 := "0.1"

scalaVersion                            := "2.9.1"

scalaSource       in Compile            := file("Minei.scala")

scalacOptions                           ++= Seq("-deprecation", "-unchecked")

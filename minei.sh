#!/bin/sh

scalac -classpath war/WEB-INF/classes:../gwt-2.0.4/gwt-servlet.jar Minei.scala

mkdir -p war/WEB-INF/classes/org/godfat/minei/
mv org/godfat/minei/*.class war/WEB-INF/classes/org/godfat/minei/
rm -r org

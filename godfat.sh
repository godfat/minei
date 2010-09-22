#!/bin/sh

scalac -classpath war/WEB-INF/classes:../gwt-2.0.4/gwt-servlet.jar Godfat.scala

mkdir -p war/WEB-INF/classes/org/godfat/mine/
mv org/godfat/mine/Godfat.class war/WEB-INF/classes/org/godfat/mine/
rm -r org

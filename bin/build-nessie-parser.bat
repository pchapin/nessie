@echo off

cd src\edu\vtc\nessie
java -Xmx1536M -cp ..\..\..\..\lib\antlr-4.2.2.jar org.antlr.v4.Tool -visitor Nessie.g4
cd ..\..\..\..

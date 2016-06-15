#!/bin/bash

cd src/edu/vtc/nessie
java -Xmx2048M -cp ../../../../lib/antlr-4.2.2.jar org.antlr.v4.Tool -visitor Nessie.g4
cd ../../../..

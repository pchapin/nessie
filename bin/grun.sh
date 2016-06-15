#
# This is the testing tool described in "Definitive ANTRL 4."
#

cd build/production/Nessie
java -cp .:../../../lib/antlr-4.2.2.jar org.antlr.v4.runtime.misc.TestRig edu.vtc.nessie.Nessie $1 $2 $3 $4
cd ../../..

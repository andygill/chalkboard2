#
# Trivial testing framework, just uses diff
#
# This must match version number
CHALKBOARD_SERVER=../../dist/build/chalkboard-server-1_9_0_16/chalkboard-server-1_9_0_16

test::
	rm -fR *png testout
	../../dist/build/chalkboard-tests-test1/chalkboard-tests-test1
	mkdir -p testout
	mv *png testout
	diff -x .svn master testout 
# Testing Server
	env CHALKBOARD_SERVER=$(CHALKBOARD_SERVER) ../../dist/build/chalkboard-tests-test1/chalkboard-tests-test1 server
	sleep 3
	mkdir -p serverout
	mv *png serverout



clean::
	rm *.png

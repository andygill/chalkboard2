boot::
	( cd ../.. ; cabal configure -fvideo1 ; cabal build )
	( cd tests ; rm -f testIn1.jpeg testIn2.jpeg testIn3.jpeg testOut.mp4 testBothOut.mp4 test1MultipleInOut.mp4 test2MultipleInOut.mp4 testGLSLOut.mp4 )
	../../dist/build/chalkboard-tests-video1/chalkboard-tests-video1
	mv testIn1.jpeg testIn2.jpeg testIn3.jpeg testOut.mp4 testBothOut.mp4 test1MultipleInOut.mp4 test2MultipleInOut.mp4 testGLSLOut.mp4 tests
	diff masters tests

clean::
	( cd tests ; rm -f testIn1.jpeg testIn2.jpeg testIn3.jpeg testOut.mp4 testBothOut.mp4 test1MultipleInOut.mp4 test2MultipleInOut.mp4 testGLSLOut.mp4 )

test::
	( cd ../.. ; cabal configure -ffont1 ; cabal build )
	rm -f testFont1.jpeg
	../../dist/build/chalkboard-tests-font1/chalkboard-tests-font1
	diff masterFont1.jpeg testFont1.jpeg

clean::
	rm -f testFont1.jpeg

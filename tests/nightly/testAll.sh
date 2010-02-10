#Script to run all of the individual ChalkBoard regression tests
#  -  Assumes starting from the 'nightly' directory (for nightly tests)
#  -  All future regression tests should be added here


( cd ../test1 ; make )
( cd ../video1 ; make )
( cd ../font1 ; make )


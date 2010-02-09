echo -e "Running Nightly Tests:\n\n"

cd /local/kmatlage/chalkboard/chalkboard/tests/nightly
git clone git://gitsldg.ittc.ku.edu/chalkboard/chalkboard.git
cd chalkboard
( cd tests/test1 ; make )
( cd tests/video1 ; make )
cd ..
rm -rf chalkboard

echo -e "\n\nFinished Nightly Tests.\n"

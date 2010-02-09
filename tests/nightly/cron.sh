#nightlyTestDir=/local/kmatlage/chalkboard/chalkboard/tests/nightly/

echo -e "Running Nightly Tests...\n\n" > /tmp/nightlyEmail

cd /tmp
git clone git://gitsldg.ittc.ku.edu/chalkboard/chalkboard.git
cd chalkboard/tests/nightly

rm -f nightly.log
./testAll.sh >> nightly.log 2>&1
cp nightly.log /tmp/nightly.log

echo -e "Diff:\n" >> /tmp/nightlyEmail
diff expected.log nightly.log >> /tmp/nightlyEmail

cd ../../..
rm -rf chalkboard

echo -e "\n\nFinished Nightly Tests.\n" >> /tmp/nightlyEmail

mutt -s "ChalkBoard Nightly Test" -a /tmp/nightly.log -- kmatlage@ittc.ku.edu < /tmp/nightlyEmail

rm -f /tmp/nightlyEmail /tmp/nightly.log


nightlyTestDir=/local/kmatlage/chalkboard/chalkboard/tests/nightly/

$nightlyTestDir/testAll.sh >> $nightlyTestDir/nightly.log 2>&1
diff expected.log nightly.log

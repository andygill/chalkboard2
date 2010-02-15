# Nightly Testing for ChalkBoard
#  -  Can simply run this script manually or set it up as a cron job
#  -  If running as a cron job, be sure to set "DISPLAY=:0.0" in the crontab and make sure the system has set "xhost +" to allow access to the display
#  -  New recipients can be added by inserting additional email addresses into the string below, separated by a space.


recipients="kmatlage@ittc.ku.edu andygill@ittc.ku.edu"

nightlyEmail=/tmp/nightlyEmail
nightlyLog=/tmp/nightly.log
nightlyDiff=/tmp/nightly.diff


echo -e "Running Nightly Tests...\n\n" > $nightlyEmail

cd /tmp
git clone git://gitsldg.ittc.ku.edu/chalkboard/chalkboard.git > $nightlyLog
cd chalkboard/tests/nightly

./testAll.sh >> $nightlyLog 2>&1

echo -e "Diff:\n" >> $nightlyEmail
diff expected.log $nightlyLog > $nightlyDiff
cat $nightlyDiff >> $nightlyEmail

cd ../../..
rm -rf chalkboard

echo -e "\n\nFinished Nightly Tests.\n" >> $nightlyEmail

mutt -s "ChalkBoard Nightly Test" -a $nightlyLog $nightlyDiff -- $recipients < $nightlyEmail

rm -f $nightlyEmail $nightlyLog $nightlyDiff


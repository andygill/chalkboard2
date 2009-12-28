# perl showme.pl > hack.html
while (<STDIN>) {
	if (/Binary files (\S+.png) and (\S+.png) differ/) {
		print "<table><tr><th>$1</th><th>$2</th></tr>\n";
		print "<tr><td><img src=\"$1\"></td><td><img src=\"$2\"></td></td>\n";
		print "</table><br><hr>\n\n";
	}
}
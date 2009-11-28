$infile = $ARGV[0];

foreach(@ARGV[1..$#ARGV]) {
	/(\d+)_(.*).txt/;
	$table[$1] = "$1_$2";
}	

$infile =~ /^(\d+)_/;
$count = $1;

#print "\n($count)\n";
print "<br><hr>";
print ("<a href=\"" . $table[$count-1] . ".html\">back ($table[$count-1])</a>  ");
print ("<a href=\"index.html\">up   (index)</a>  ");
print ("<a href=\"" . $table[$count+1] . ".html\">next ($table[$count+1])</a>  ");
print "<br><hr><br>";

while (<STDIN>) {
	print "$_";
}

#print "\n($count)\n";
print "<br><hr>";
print ("<a href=\"" . $table[$count-1] . ".html\">back ($table[$count-1])</a>  ");
print ("<a href=\"index.html\">up   (index)</a>  ");
print ("<a href=\"" . $table[$count+1] . ".html\">next ($table[$count+1])</a>  ");
print "<br><hr><br>";
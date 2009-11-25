# perl script
print ('<html><body>');
foreach (@ARGV) {
    /^(\d+_)(.*).txt$/;
    print ('<a href="' . "$1$2" . '.html' . '">' . $2 . '</a>');
    print (' (<a href="' . "$1$2" . '.txt' . '">txt</a>)');
    print "<BR>\n";
}
print ('</body></html>');

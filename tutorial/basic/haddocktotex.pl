# turn a haddock document into latex.


sub cmd {
	local ($c,$r,@rest) = @_;
	push(@state,$c);
	$extra = '';
	if ($r =~ /CLASS=\"([^"]*)\"/) {
		push(@state,"$c:$1");
		$extra = $1;
	}

	$count{$c}++;
#	push(@state,":hide") if ($c eq "TABLE" && ($count{'TABLE'} == 4 || $count{'TABLE'} == 5));
#	push(@state,":hide") if ($c eq "TD" && ($count{'TD'} == 14));
	if ($c eq 'A' 
		|| $c eq "SCRIPT" 
		|| $c eq "HEAD" 
		|| $c eq "HTML"
		|| $c eq "META"
		|| $c eq "LINK"
		|| $c eq "BODY"
		|| $c eq "IMG"
		|| $c eq "FONT"
		|| $c eq "SPAN"
		|| $c eq "DIV"
		) {
		# nothing 
	} elsif ($c eq 'TT') {
		print ("{\\tt " . "");
		push(@state,"CLOSE:}");
	} elsif ($c eq 'DL') {
		print ("\n\\begin\{itemize\}\n");
		print ("\\setlength\{\\itemsep\}\{0in\}\n");
		push(@state,"NEWLINE:");
		push(@state,"CLOSE:\\end\{itemize\}");
		push(@state,"NEWLINE:");
	} elsif ($c eq 'DT') {
		print ("\n\\item ");
	} elsif ($c eq 'TR') {
		@tabs = grep {$_ eq "TABLE"} @state;
		if (grep {$_ eq ":hide"} @state) {
		} elsif ($#tabs == 0) {
			push(@state,"NEWLINE:");
			push(@state,"NEWLINE:");
		} else {
			push(@state,"NEWLINE:");
			push(@state,"CLOSEROW:");
			$therow = '';
		}
	} elsif ($c eq 'TABLE') {
		if ($count{'TABLE'} == 1) {
#			push(@state,":notable");
			# top level table, do nothing
		} elsif (grep {$_ eq "TD:topbar"} @state) {
			push(@state,":hide");
		} elsif (grep {$_ eq "TD:modulebar"} @state) {
			push(@state,":hide");
		} else {
			print ("\n\\begin\{tabular\}{p{0.95\\linewidth}}");
#			print ("\n\\hline\n");
#			print ("\n\\verb|" . join(";",@state) . "|\\\\\n");
			push(@state,"NEWLINE:");
			push(@state,"CLOSE:\\end\{tabular\}");
			push(@state,"NEWLINE:");
#			push(@state,"CLOSE:\\hline");
#			push(@state,"NEWLINE:");
		}
	} elsif ($c eq 'TD') {
		if ($extra eq 's8') {
			#
		} elsif ($extra eq 'section1') {
#			print "%";  # HACK HACK HACK
		 	print "\\subsubsection{";
			push(@state,"CLOSE:}");
		} elsif ($extra eq 'section4') {
			print "\{\\bf ";
			push(@state,"CLOSE:}");
		} elsif ($extra eq 'decl') {
			print "{";  # for now
			push(@state,"CLOSE:}");
		} elsif ($extra eq 'doc') {
			print ("\\hspace"."{"."0.05\\textwidth" ."}");  # for now
			print ("\\begin"."{"."minipage" ."}{" . "0.9\\textwidth" . "}");  # for now
			push(@state,"CLOSE:\\end" . "{" . minipage . "}");
		} elsif ($extra eq 'section1') { #  && $count{'TD'} != 14) {
#			print ("\\subsubsection"."{");  # for now
#			push(@state,"CLOSE:}");
		} else {
			print " ";
		}
	} elsif ($c eq 'B') {
		print ("{\\bf " . "");
		push(@state,"CLOSE:}");
	} elsif ($c eq 'EM') {
		print ("{\\em " . "");
		push(@state,"CLOSE:}");
	} elsif ($c eq "TITLE") {
		print ("\\subsection"."{");
		push(@state,"CLOSE:}");
	} else {
		print ("\n## $c" . "{\n");
		push(@state,"CLOSE:###}");
	}
}

sub closecmd { 
	local ($c,@rest) = @_;	
	if ($c =~ /CLOSE:(.*)$/) {
		# nothing
		print "$1";
	}
	if ($c =~ /NEWLINE:$/) {
		# nothing
		print "\n";
	}
	if ($c =~ /CLOSEROW:$/ && length($therow) > 0) {
		# nothing
		print "\\\\ % (" . length($therow) . ")\n";
	}
}

sub findstate {
  

	
}

sub issue {
	local ($c,@rest) = @_;	
	return if (grep {$_ eq "SCRIPT"} @state);
	return if (grep {$_ eq "TD:topbar"} @state);
	return if (grep {$_ eq "TD:modulebar"} @state);
	return if (grep {$_ eq "TD:botbar"} @state);
	return if (grep {$_ eq ":hide"} @state);
#	return if (grep {$_ eq "TD:section4"} @state);
#	return if (grep {$_ eq "TD:section1"} @state);
#	@x = grep {$_ eq "TABLE:vanilla"} @state;
#	return if ($#x == 1);
#	if (grep {$_ eq "TITLE"} @state) {
		
	
#	print ("\n(" . join(";",@state) . ")\n");

#	$c =~ s/\-\&gt;/->/g;
	$c =~ s/\&gt;/{\\tt >}/g;
#	$c =~ s/\&lt;-/\\verb\|<-|/g;
	$c =~ s/\&lt;/{\\tt <}/g;
	$c =~ s/\$/\\\$/g;
	$c =~ s/_/\\\_/g;
	if (grep {$_ eq ":hide"} @state) {
		print "(($c))";
	} else {
		print "$c";
	}
	$therow .= $c;
}

@state = ('HTML');
while(<STDIN>) {
  next if (/^<\!--/);
  $state = '' if ($state eq 'NEXT');
  next if (/^<\!DOCTYPE/);

#  print "## $_";

  if (/^>(.*)<\/(\w+)/) {
	issue($1);
	$text = $1;
	$find = $2;
	do {
		$v = pop(@state);
		closecmd($v);
#		print "{$v} $#state\n";
	} while($v ne $find && $#state > -1);
	die "opps, bad nesting" if ($#state == -1);
  } elsif (/^<\/(\w+)/) {
	$find = $1;
	do {
		$v = pop(@state);	
		closecmd($v);
#		print "{$v} $#state\n";
	} while($v ne $find && $#state > -1);
	die "opps, bad nesting" if ($#state == -1);

  } elsif (/^>(.*)<(\w+)(.*)/) {
	issue($1);
	cmd($2,$3);
  } elsif (/^(.*)<(\w+)(.*)/) {
	issue($1);
	cmd($2,$3);
  } elsif (/^>(.*)/) {
	issue($1);
  } else {
	issue($_);
  }

  $state = join(':',@state);

#  print "($state) $_";
	
}
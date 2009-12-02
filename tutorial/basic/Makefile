FILES=	\
	Graphics-ChalkBoard-Board.tex		\
	Graphics-ChalkBoard-O.tex		\
	Graphics-ChalkBoard-Shapes.tex		\
	Graphics-ChalkBoard-Types.tex		\
	Graphics-ChalkBoard-Main.tex		\
	Graphics-ChalkBoard-Utils.tex		\
	Graphics-ChalkBoard-Options.tex

SRC = ../../dist/doc/html/chalkboard/

all:: $(FILES)
#	perl haddocktotex.pl < ../../dist/doc/html/chalkboard/Graphics-ChalkBoard-Board.html > test.tex


%.tex: $(SRC)/%.html
	perl haddocktotex.pl < $<  > $@

clean::
	rm $(FILES)
	
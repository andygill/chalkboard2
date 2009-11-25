MARKDOWN=perl ../scripts/Markdown.pl
INDEX=perl ../scripts/Index.pl
all: $(patsubst %.txt,%.html,$(wildcard *.txt))
	$(INDEX) $(wildcard *.txt) > index.html

%.html: %.txt 
	$(MARKDOWN) $< > $@

clean:
	rm *.html
	

MARKDOWN=perl ../scripts/Markdown.pl
all: $(patsubst %.txt,%.html,$(wildcard *.txt))
%.html: %.txt 
	$(MARKDOWN) $< > $@

clean:
	rm *.html
	

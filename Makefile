
SUBS = 1.0 1.1 1.2
SUBS_IDX = $(addsuffix /index.html, $(SUBS))

all: index.html $(SUBS_IDX)

%.html: %.adoc
	asciidoc $< > $@


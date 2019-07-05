

.PHONY: all

all: whitespace.html whitespace.pdf

whitespace.html: whitespace.md whitespace.html.yaml
	pandoc whitespace.md whitespace.html.yaml -s --toc --output whitespace.html

whitespace.pdf: whitespace.md whitespace.pdf.yaml
	pandoc whitespace.md whitespace.pdf.yaml -s --toc --output whitespace.pdf

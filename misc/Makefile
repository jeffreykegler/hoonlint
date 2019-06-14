

.PHONY: all

all: whitespace.html whitespace.pdf

whitespace.html: whitespace.md whitespace.yaml
	pandoc whitespace.md whitespace.yaml -s --toc --output whitespace.html

whitespace.pdf: whitespace.md whitespace.yaml
	pandoc whitespace.md whitespace.yaml --toc --output whitespace.pdf

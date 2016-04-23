TEXS=$(notdir $(wildcard tex/*.tex)) # LaTeX files in tex/
DOCS=$(TEXS:%.tex=tmp/%.pdf)         # Output PDF files go in tmp/
SVGS=$(notdir $(wildcard img/*.svg)) # SVG files in img/
PDFTEXS=$(SVGS:%.svg=tmp/%.pdf_tex)  # Output pdf_tex files go in tmp/

all: $(DOCS) graphs
	cp tmp/presentation.pdf tmp/report.pdf out/
	@echo Success! See out/ for output.

tmp/%.pdf: tex/%.tex graphs pdftexs dirs
	latexmk -f $< > /dev/null 2>&1

.PHONY: graphs
graphs: src/main.R dirs
	Rscript src/main.R

.PHONY: clean
clean:
	rm -rf tmp out

.PHONY: pdftexs
pdftexs: $(PDFTEXS) dirs

tmp/%.pdf_tex: img/%.svg dirs
	inkscape -D -z --file=$< --export-pdf=$(basename $@).pdf --export-latex

readme.pdf: readme.md
	pandoc readme.md

.PHONY: dirs
dirs:
	mkdir -p tmp out

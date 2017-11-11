BODYTEXT:=\
	01_introduction.md \
	02_methods.md \
	03_results.md \
	04_discussion.md \
	05_conclusions.md

MDFILES:=00_index.md ${BODYTEXT}

PANDOC_HEAD:=pandoc \
	--verbose \
	--filter pandoc-include \
	--filter pandoc-crossref \
	--filter pandoc-citeproc

PANDOC:=${PANDOC_HEAD} ${MDFILES} -s -o

all:
	Rscript -e "bookdown::render_book('index.Rmd', output_format = 'bookdown::pdf_document2')"

clean:
	rm -rf _bookdown_files

paper.pdf: ${MDFILES}
	${PANDOC} $@

odtpaper.odt: ${MDFILES}
	${PANDOC} $@

docxpaper.docx: ${MDFILES}
	${PANDOC} $@

%.md: %.Rmd
	Rscript -e 'knitr::knit("$<")'

#02_methods.md: 02_methods.Rmd
#03_results.md: 03_results.Rmd

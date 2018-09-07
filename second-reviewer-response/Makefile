all: pdf

pdf:
	Rscript -e  'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_document2")'

doc:
	Rscript -e  'bookdown::render_book("index.Rmd", output_format = "bookdown::word_document2")'

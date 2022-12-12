pdffiles := $(shell git ls-files pdf/*.pdf)
txtfiles := $(shell git ls-files txt/*.txt)
artistsfiles := $(shell git ls-files index/*.artists.txt)
recfiles := $(patsubst txt/%.txt,csv/%.rec.csv,$(txtfiles))

print-%:
	@echo $($*)

help:
	@echo 'Usage:                                                                    '
	@echo 'make records — split txt into CSV: one record in a row'

all: csv/authors_joined.rec.csv

dataset: dataset/editions.csv

csv/19%.rec.csv: txt/19%.txt scripts/split_records.py
	python3 scripts/split_records.py $< $@

results/ill/%_general_.csv: index/%.artists.txt index/%.authors.txt scripts/illustrators_network.R
	Rscript scripts/illustrators_network.R -i $< -a index/$*.authors.txt -o $(@D) -y $*

csv/authors_disamb.csv: csv/all.rec.csv scripts/authors.R
	Rscript scripts/authors.R

csv/authors_joined.rec.csv: csv/all.rec.csv
	Rscript scripts/disamb_authors.R

dataset/editions.csv: csv/authors_joined.rec.csv
	Rscript scripts/export_editions.R -i $< -o $@

records: $(recfiles) csv/all.rec.csv

convert: $(txtfiles)

csv/all.rec.csv: $(recfiles)
	head -1 $< > $@
	tail -q -n +2 $^ >> $@

data: csv/all.rec.csv $(recfiles)
	zip records.zip $^

illustrators-net: $(patsubst index/%.artists.txt, results/ill/%_general_.csv, $(artistsfiles))



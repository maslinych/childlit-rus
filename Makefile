pdffiles := $(shell git ls-files pdf/*.pdf)
txtfiles := $(shell git ls-files txt/*.txt)
artistsfiles := $(shell git ls-files index/*.artists.txt)
recfiles := $(patsubst txt/%.txt,csv/%.rec.csv,$(txtfiles))

print-%:
	@echo $($*)

help:
	@echo 'Usage:                                                                    '
	@echo 'make records — split txt into CSV: one record in a row'

all: csv/all.rec.csv

dataset: dataset/editions.csv

csv/19%.rec.csv: txt/19%.txt scripts/split_records.py
	python3 scripts/split_records.py $< $@

results/ill/%_general_.csv: index/%.artists.txt index/%.authors.txt scripts/illustrators_network.R
	Rscript scripts/illustrators_network.R -i $< -a index/$*.authors.txt -o $(@D) -y $*

## do not run this rule, because authors_disamb has been manually edited A LOT
## this rule only documents how the file was initially created
#csv/authors_disamb.csv: csv/all.rec.csv scripts/authors.R
#	Rscript scripts/authors.R

csv/authors_joined.rec.csv: csv/all.rec.csv
	Rscript scripts/disamb_authors.R

dataset/editions.csv: csv/all.rec.csv csv/authors_disamb.csv csv/genres.csv csv/normalized_fields.csv scripts/export_editions.R
	Rscript scripts/export_editions.R -i $< -o $@ -a csv/authors_disamb.csv -g csv/genres.csv -n csv/normalized_fields.csv

records: $(recfiles) csv/all.rec.csv

convert: $(txtfiles)

csv/all.rec.csv: $(recfiles)
	head -1 $< > $@
	tail -q -n +2 $^ >> $@

test-dataset: dataset/tests/test.py dataset/editions.csv dataset/bibliography.csv
	python3 $<


data: csv/all.rec.csv $(recfiles)
	zip records.zip $^

illustrators-net: $(patsubst index/%.artists.txt, results/ill/%_general_.csv, $(artistsfiles))



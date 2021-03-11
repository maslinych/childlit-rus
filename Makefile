pdffiles := $(shell git ls-files pdf/*.pdf)
txtfiles := $(shell git ls-files txt/*.txt)
artistsfiles := $(shell git ls-files index/*.artists.txt)

print-%:
	@echo $($*)

help:
	@echo 'Usage:                                                                    '
	@echo 'make records — split txt into CSV: one record in a row'

all: convert

csv/%.rec.csv: txt/%.txt scripts/split_records.py
	python3 scripts/split_records.py $< $@

results/ill/%_general_.csv: index/%.artists.txt index/%.authors.txt scripts/illustrators_network.R
	Rscript scripts/illustrators_network.R -i $< -a index/$*.authors.txt -o $(@D) -y $*

records: $(patsubst txt/%.txt,csv/%.rec.csv,$(txtfiles))

convert: $(txtfiles)

illustrators-net: $(patsubst index/%.artists.txt, results/ill/%_general_.csv, $(artistsfiles))


pdffiles := $(shell git ls-files pdf/*.pdf)
txtfiles := $(shell git ls-files txt/*.txt)
artistsfiles := $(shell git ls-files index/*.artists.txt)
recfiles := $(patsubst txt/%.txt,csv/%.rec.csv,$(txtfiles))

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

records: $(recfiles)

convert: $(txtfiles)

csv/all.rec.csv: $(recfiles)
	head -1 $< > $@
	tail -q -n +2 $^ >> $@

data: csv/all.rec.csv $(recfiles)
	zip records.zip $^

illustrators-net: $(patsubst index/%.artists.txt, results/ill/%_general_.csv, $(artistsfiles))



pdffiles := $(shell git ls-files pdf/*.pdf)
txtfiles := $(shell git ls-files txt/*.txt)

print-%:
	@echo $($*)

help:
	@echo 'Usage:                                                                    '
	@echo 'make records — split txt into CSV: one record in a row'

all: convert

csv/%.rec.csv: txt/%.txt scripts/split_records.py
	python3 scripts/split_records.py $< $@

records: $(patsubst txt/%.txt,csv/%.rec.csv,$(txtfiles))

convert: $(txtfiles)


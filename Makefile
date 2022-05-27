# Building and deployment of the package

# This file assumes that the user has bump2version installed:
# https://pypi.org/project/bump2version/

version := patch
ROOT_DIR := $(shell basename $(dir $(abspath $$PWD)))
PKG := $(shell find . -type f -name "$(ROOT_DIR)*.tar.gz")
BRANCH:= $(shell git rev-parse --symbolic-full-name --abbrev-ref HEAD)

print:
	echo $(ROOT_DIR)

all: clean test docs build check 

clean:
	- rm -r $(ROOT_DIR).Rcheck $(PKG)

docs: clean
	Rscript -e 'roxygen2::roxygenise("$(ROOT_DIR)")'

build: docs
	R CMD build $(ROOT_DIR)

check: build
	R CMD check $(PKG)

install: build check
	R CMD install .

bump:
	git checkout $(BRANCH)
	git pull origin $(BRANCH)
	bumpversion $(version)
	git push origin $(BRANCH) --follow-tags

.PHONY=clean bump

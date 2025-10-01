PKG=Rfuzzycoco

rox:
	Rscript --no-save -e 'devtools::document()'

build: rox
	Rscript --no-save -e 'devtools::build()'

compile:
	Rscript --no-save -e 'pkgbuild::compile_dll(debug = FALSE)'

check: rox
	_R_CHECK_SYSTEM_CLOCK_=0 Rscript --no-save -e 'devtools::check(".", check_dir = ".checks")'

check/novignettes:
	_R_CHECK_SYSTEM_CLOCK_=0 Rscript --no-save -e 'devtools::check(".", check_dir = ".checks", vignettes = FALSE)'

run_examples: rox
	Rscript --no-save -e 'devtools::run_examples(run_donttest = TRUE)'

build_vignettes: rox
	Rscript --no-save -e 'devtools::build_vignettes()'

FILTER=
test: rox
	Rscript --no-save -e 'devtools::test(filter="$(FILTER)")'

manual: rox
	rm -f $(PKG).pdf
	R CMD Rd2pdf -o $(PKG).pdf .

clean:
	rm -rf .checks* .Rd2* src/*.o src/*.so src/*.a src/fuzzycoco/src/*.o
	
pkgdown: rox
	Rscript --no-save -e 'pkgdown::build_site()'

zero-coverage:
	Rscript -e 'library(covr); suppressWarnings(zero_coverage(package_coverage()))'

covr:
	Rscript -e 'library(covr); suppressWarnings(print(package_coverage()))'

covr/fast:
	Rscript -e 'library(covr); suppressWarnings(print(package_coverage(type = "tests", clean=FALSE, pre_clean=FALSE)))'

COVR_REPORT=.tmp/cov.html
report_covr:
	Rscript -e 'library(covr); suppressWarnings(print(report(package_coverage(), "$(COVR_REPORT)")))'

report_covr/fast:
	Rscript -e 'library(covr); suppressWarnings(print(report(package_coverage(type = "tests", clean=FALSE, pre_clean=FALSE), "$(COVR_REPORT)")))'
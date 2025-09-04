
# Rfuzzycoco

<!-- badges: start -->
<!-- badges: end -->

Rfuzzycoco provides the FuzzyCoCo algorithm by wrapping the [fuzzycoco](https://github.com/Lonza-RND-Data-Science/fuzzycoco) C++ library, and extending its possibilities.

## License

This fuzzycoco software is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0).  

## Installation

You can install the development version of Rfuzzycoco from github.
Because the Rfuzzycoco uses a git submodule, the usual easy ways to install
packages, like `remotes::install_github()` and `pak::pak()` do not work.

The easiest way is thus to clone the github repository, fetch the submodule and install the local package source from 
the terminal:


```bash
git clone https://github.com/Lonza-RND-Data-Science/Rfuzzycoco.git
cd Rfuzzycoco/
git submodule update --init

# install devtools from CRAN if needed
R -e "install.packages('devtools')"

# install the local package
R -e "devtools::install('.')"
```

## Local setup

- clone the repo
- get the submodule: `git submodule update --init`


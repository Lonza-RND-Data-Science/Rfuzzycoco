
# Rfuzzycoco

<!-- badges: start -->

[![R-CMD-check](https://github.com/Lonza-RND-Data-Science/Rfuzzycoco/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/Lonza-RND-Data-Science/Rfuzzycoco/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/Lonza-RND-Data-Science/Rfuzzycoco/graph/badge.svg?token=DNSmWrcvFd)](https://codecov.io/gh/Lonza-RND-Data-Science/Rfuzzycoco)
[![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-success)](https://www.gnu.org/licenses/agpl-3.0.html)
<!-- badges: end -->

Rfuzzycoco provides the FuzzyCoCo algorithm by wrapping the [fuzzycoco](https://github.com/Lonza-RND-Data-Science/fuzzycoco) C++ library, and extending its possibilities.

## Fuzzy Coco

The Fuzzy CoCo (Fuzzy Cooperative Coevolution) algorithm — by Carlos A. Peña-Reyes (2000) — is an evolutionary 
fuzzy modeling method designed to automatically generate interpretable fuzzy rule-based systems from data.

Unlike traditional evolutionary approaches that evolve full rule bases as single entities, Fuzzy CoCo uses a cooperative coevolutionary strategy:

  - Each fuzzy rule (or rule component) is evolved in a separate subpopulation.
  - The membership functions parameters are evolved in a second subpopulation.

Both rule structures and membership functions are optimized jointly to balance accuracy and interpretability.

This decomposition enables **Fuzzy CoCo** to efficiently handle complex systems and produce transparent, linguistically meaningful models.

### Reference

Peña-Reyes, C. A., & Sipper, M. (2001).  
*Fuzzy CoCo: A cooperative-coevolutionary approach to fuzzy modeling.*  
**IEEE Transactions on Fuzzy Systems, 9**(5), 727–737.  
DOI: [10.1109/91.963759](https://doi.org/10.1109/91.963759)  
[📄 PDF (moshesipper.com)](https://www.moshesipper.com/pubs/fuzzy_coco.pdf)

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

## Synopsis

cf the [Getting Started](https://lonza-rnd-data-science.github.io/Rfuzzycoco/articles/getting_started.html) vignette:

```R
library(Rfuzzycoco)

pms <- params(
  nb_rules = 2, nb_max_var_per_rule = 3,        # structural parameters
  rules.pop_size = 100, mfs.pop_size = 100,     # coevolution population sizes
  ivars.nb_sets = 3, , ivars.nb_bits_pos = 8,   # input vars: 3 fuzzy sets, and 8 bits to discretize the values 
  ovars.nb_sets = 3, ovars.nb_bits_pos = 8,     # output vars: 3 fuzzy sets, and 8 bits to discretize the values 
  metricsw.sensitivity = 0, metricsw.specificity = 0, metricsw.rmse = 1, # we just use RMSE (root mean square error)
  output_vars_defuzz_thresholds = 17            # threshold for the qsec output variable
)

x <- mtcars[c("mpg", "hp", "wt")]
y <- mtcars["qsec"]
df <- cbind(x, y)

fit <- fit(model, qsec ~ ., df, engine = "rcpp", seed = 456, max_generations = 20)

res <- evaluate(fit, df)

y2 <- predict(fit, x)

```

## Documentation

The package documentation, including the reference manual and the vignettes,  is available online here: 
[https://lonza-rnd-data-science.github.io/Rfuzzycoco/](https://lonza-rnd-data-science.github.io/Rfuzzycoco/).

The underlying `fuzzycoco` C++ library and its documentation is
available here: [https://github.com/Lonza-RND-Data-Science/fuzzycoco](https://github.com/Lonza-RND-Data-Science/fuzzycoco)


## Local setup

- clone the repo
- get the submodule: `git submodule update --init`


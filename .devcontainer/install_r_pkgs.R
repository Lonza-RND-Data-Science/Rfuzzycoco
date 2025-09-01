#! /usr/bin/env Rscript
# a R script to install a list of packages from a file

#- If you are in a container environment, please consider adding the
#   following to your configuration to silence this warning:
options(bspm.sudo = TRUE)

### read the file from STDIN
pkgs <- readLines("stdin")
# remove comments
pkgs <- grep('^\\s*#', pkgs, value = TRUE, invert = TRUE)
# remove empty lines
pkgs <- grep('^\\s*$', pkgs, value = TRUE, invert = TRUE)

message("requested to install ", length(pkgs), " packages")

install.packages(pkgs)


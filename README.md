To install:

if (!requireNamespace("remotes", quietly = TRUE)) {
  # If not installed, install the package
  install.packages("remotes")
}

remotes::install_github("kzeglinski/rladiesITASpkg")
library(rladiesmelbITAS)

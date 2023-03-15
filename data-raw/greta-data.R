# This script consolidates the data generating scripts described in R/data-raw.
# It sources all scripts and writes them to the internal data storage
# R/sysdata.rda. For requirements on the execution of this script, refer to the
# sourced scripts below.
library(usethis)

# Create an empty list. Each data-raw file creates a list called output 
# containing all relevant objects. The lists are appended to sysdata after
# each source call and stored in R/sysdata.R at the end of the file.
sysdata <- list()

# First source the data generating scripts for the data explorer
source("data-raw/bounds.R")
sysdata <- append(sysdata, output) # boundary data
source("data-raw/georef.R")
sysdata <- append(sysdata, output) # aggregated survey data + codebook

# Then source the data generating scripts for the case studies
source("data-raw/coopernico.R")
sysdata <- append(sysdata, output)


do.call(use_data, sysdata)

# This script consolidates the data generating scripts described in R/data-raw.
# It sources all scripts and writes them to the internal data storage
# R/sysdata.rda. For requirements on the execution of this script, refer to the
# sourced scripts below.
library(usethis)

old_ws <- ls()

# Create an empty list. Each data-raw file creates a list called output 
# containing all relevant objects. The lists are appended to sysdata after
# each source call and stored in R/sysdata.R at the end of the file.
sysdata <- list()

# First source the data generating scripts for the data explorer
cat("Collecting boundary data from GISCO...\n")
capture.output(source("data-raw/bounds.R", local = TRUE))
sysdata <- append(sysdata, output) # boundary data
cat("Georeferencing multinational survey data...\n")
capture.output(source("data-raw/georef.R", local = TRUE))
sysdata <- append(sysdata, output) # aggregated survey data + codebook

# Then source the data generating scripts for the case studies
cat("Preparing Coopernico data...\n")
capture.output(source("data-raw/coopernico.R", local = TRUE))
sysdata <- append(sysdata, output)

# Prepare data for saving
sysdata <- lapply(sysdata, as.name)
sysdata$internal <- TRUE
sysdata$overwrite <- TRUE
sysdata$compress <- "bzip2"

# Save data
do.call(use_data, sysdata)

# Clean workspace
rm(list = setdiff(ls(), old_ws))
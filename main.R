# 1) SETUP  ------------------------------------------------------------------

closeAllConnections() #Remove all the data
rm(list=ls()) #remove all variables

# a) Libraries --------------------------------------------------------------------

source("scripts/library-manager.R",encoding = "ISO-8859-1", echo=TRUE)

# b) Paths -------------------------------------------------------------------

#Root of database file
data.base =  str_c(getwd(),"/data/") 

# c) Functions ---------------------------------------------------------------
source(str_c("scripts/utils.R"), encoding = "ISO-8859-1", echo=TRUE)

# 2) EXECUTION -----------------------------------------------------------------------

# a) Data processing and feature engineering -------------------------------------------------

source("scripts/data-processing.R", echo=TRUE,encoding = "ISO-8859-1") 
source("scripts/feature-engineering.R", echo=TRUE,encoding = "ISO-8859-1")

# Note: these models are not included in the TRR paper

# b) Descriptive analyses
source("scripts/descriptive-analyses.R", echo=TRUE,encoding = "ISO-8859-1") 

# c) Statistics and Predictions -------------------------------------------
source("scripts/statistical-tests.R", echo=TRUE,encoding = "ISO-8859-1") 

# d) Regression models ---------------------------------------------------------------
# source("scripts/regression-models.R", echo=TRUE,encoding = "ISO-8859-1") 

# e) Exports ---------------------------------------------------------------

#Tables with regression models
# source("scripts/tables.R", echo=TRUE,encoding = "ISO-8859-1") 

#The tables exported from this script can be used to be read in tableau
source("scripts/exports-tableau.R", echo=TRUE,encoding = "ISO-8859-1")

# f) Extra 
source("scripts/brtdata-scraping.R", echo=TRUE,encoding = "ISO-8859-1")






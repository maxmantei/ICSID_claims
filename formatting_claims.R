################################################################################
## File name: formatting_claims.R                                             ##
## Author:    Maximillian Mantei                                              ##
## Date:      2015-08-04                                                      ##
## Purpose:   taking claims data and put it in panel format                   ##
################################################################################

## Loading packages
library(dplyr)
library(foreign)

## Set working directory
wd <- "C:/GitHub/ICSID_claims/"
setwd(wd)

## RUN cleaning_claims.R FIRST IF cases.csv DOES NOT EXIST!
# source("pulling_claims.R")
cases <- read.csv("data/cases.csv", stringsAsFactors = FALSE)

## generate large data frame for the final merge
cases <- cases %>% 
      select(host, source, reg_year, out_year, bit_claim, nafta_claim, other_claim) %>%
      distinct()
# generate a grid of data
data <- expand.grid(host = unique(cases$host), 
                    source = unique(cases$source), 
                    year = 1975:2012, 
                    stringsAsFactors = FALSE)
# get the sum of registered claims per year for each host-source pair
data <- cases %>% 
      group_by(host, source, reg_year) %>% 
      summarise(bit_claims_brought = sum(bit_claim, na.rm = TRUE), 
                nafta_claims_brought = sum(nafta_claim, na.rm = TRUE),
                other_claims_brought = sum(other_claim, na.rm = TRUE)) %>%
      rename(year = reg_year) %>%
      right_join(data)
# get the sum of concluded claims per year for each host-source pair
data <- cases %>% 
      group_by(host, source, out_year) %>% 
      summarise(bit_claims_concluded = sum(bit_claim, na.rm = TRUE), 
                nafta_claims_concluded = sum(nafta_claim, na.rm = TRUE),
                other_claims_concluded = sum(other_claim, na.rm = TRUE)) %>%
      rename(year = out_year) %>%
      right_join(data)
# set missing values to zero (no record = no case) in order to calc. cumul. sums
data[is.na(data)] <- 0
# generate a whole lot of summary variables
data <- data %>%
      arrange(year) %>%
      group_by(host, source) %>% 
      mutate(bit_claims_brought_total = cumsum(bit_claims_brought),
             nafta_claims_brought_total = cumsum(nafta_claims_brought),
             other_claims_brought_total = cumsum(other_claims_brought),
             bit_claims_concluded_total = cumsum(bit_claims_concluded),
             nafta_claims_concluded_total = cumsum(nafta_claims_concluded),
             other_claims_concluded_total = cumsum(other_claims_concluded),
             bit_claims_pending_total = bit_claims_brought_total - bit_claims_concluded_total,
             nafta_claims_pending_total = nafta_claims_brought_total - nafta_claims_concluded_total,
             other_claims_pending_total = other_claims_brought_total - other_claims_concluded_total,
             bit_claim = as.numeric(bit_claims_pending_total > 0),
             nafta_claim = as.numeric(nafta_claims_pending_total > 0),
             other_claim = as.numeric(nafta_claims_pending_total > 0))
# filter the relevant years
data <- data %>% filter(year >= 1980 & year <= 2012)

# save file
file_name <- paste0("data/ICSID_claims_", Sys.Date(), ".csv")
write.csv(data, file_name, append = FALSE)

## Export STATA file
file_name <- paste0("data/ICSID_claims_", Sys.Date(), ".dta")
write.dta(data, file_name)

## END
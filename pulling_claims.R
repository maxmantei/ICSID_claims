################################################################################
## File name: pulling_claims.R                                                ##
## Author:    Maximillian Mantei                                              ##
## Date:      2015-08-04                                                      ##
## Purpose:   Pulling data on ICSID claims from https://icsid.worldbank.org/  ##
################################################################################

## Loading packages
library(rvest)
library(dplyr)
library(RCurl)
library(RSelenium)

## Set working directory
wd <- "C:/GitHub/ICSID_claims/"
setwd(wd)

## Get full list of claims on the website
# start Selenium server
RSelenium::startServer()
# initiate a firefox browser
remDr <- remoteDriver(browserName = "firefox")
# open the browser without showing it
remDr$open(silent = TRUE)
# navigate to website
remDr$navigate("https://icsid.worldbank.org/apps/ICSIDWEB/cases/Pages/AdvancedSearch.aspx")
# get the buttton element that must be clicked to view all cases
PageSizeALL <- remDr$findElement("css selector",
                                 "#pagesize > option:nth-child(3)")
# click the element
PageSizeALL$clickElement()
# get the case IDs
PageHTML <- remDr$getPageSource()[[1]] %>% XML::htmlParse()
# getting the table
cases <- PageHTML %>%
      html_node('#ctl00_m_g_ba040fcb_44f7_44fa_92d0_d088c5679794_ctl00_gvCasedetails') %>%
      html_table() %>% 
      as_data_frame()
# remove last row
cases <- head(cases, n = -1)
# replace spaces with %20 in each case ID (for the web address)
cases[, 1] <- sapply(cases[, 1], function(x) {gsub(" ", "%20", x)})
# close remote drive
remDr$close()

## Set some stuff for the loop
# set static part of URL
URL <- "https://icsid.worldbank.org/apps/ICSIDWEB/cases/Pages/casedetail.aspx?CaseNo="
## set css selectors for html nodes
# Claimant data
ClaimantNode <- "#ctl00_m_g_39b2e503_4cae_4c82_a505_66099a6ff48d_ctl00_gvProceedingtab_ctl02_Label5"
# Outcome of proceedings data (concluded or discontinued...)
ProcOutNode <- "#ctl00_m_g_39b2e503_4cae_4c82_a505_66099a6ff48d_ctl00_gvProceedingtab_ctl02_lblOut"
# Date registered
RegNode <- "#ctl00_m_g_39b2e503_4cae_4c82_a505_66099a6ff48d_ctl00_gvProceedingtab_ctl02_Label2"
# BIT node
BITNode <- "#ctl00_m_g_39b2e503_4cae_4c82_a505_66099a6ff48d_ctl00_dvCaseDetails_lblBIT"
# Other instruments invoked
InstNode <- "#ctl00_m_g_39b2e503_4cae_4c82_a505_66099a6ff48d_ctl00_dvCaseDetails_lblInstrumentInvk"

## Loop trough cases and pull data
# set static part of URL
URL <- "https://icsid.worldbank.org/apps/ICSIDWEB/cases/Pages/casedetail.aspx?CaseNo="
# start loop
for (i in seq_len(nrow(cases))){
      # set specific case URL
      caseURL <- paste0(URL, cases[i, 1])
      # get the specific case html file
      casePage <- getURL(caseURL, ssl.verifypeer = FALSE) %>% 
            XML::htmlParse()
      # get variables from html file using css selectors as nodes
      cases$claimants[i] <- casePage %>% 
            html_nodes(ClaimantNode) %>% 
            html_text()
      cases$outcome[i] <- casePage %>% 
            html_nodes(ProcOutNode) %>% 
            html_text()
      cases$registered[i] <- casePage %>%
            html_nodes(RegNode) %>%
            html_text()
      cases$bit[i] <- casePage %>%
            html_nodes(BITNode) %>%
            html_text()
      cases$instrument[i] <- casePage %>%
            html_nodes(InstNode) %>%
            html_text()
      cases$claim_id[i] <- (nrow(cases):1)[i] # claim_id; oldest gets id == 1
      
      ## add a random wait time (in secs) to be polite towards the WB servers...
      # wait time is a random draw from a uniform distribution [2, 3]
      Sys.sleep(runif(1, min = 1, max = 2))
}

# save pulled data
write.csv(cases, "data/cases_fresh.csv", append = FALSE)

# To continue run cleaning_claims.R
source("cleaning_claims.R")
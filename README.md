# Scraping and formatting ICSID case data
Maximillian Mantei  
4 August 2015  

# Pulling the data
## Start
This is the beginning of ``pulling_claims.R``. First the required packages are loaded. *RSelenium* is used to start a remote browser to load the ICSID website and retrieve all case numbers. *rvest* is used to scrape the individual case details websites. *dplyr* is used for data manipulation and the handy ``%>%`` infix operator (takes everything that is left from it and makes it the first argument of a function to the right of it). ``setwd`` sets the working directory on the local machine.

```r
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
```

## Get case IDs
RSelenium is usd to remotely browse to the [ICSID cases]("https://icsid.worldbank.org/apps/ICSIDWEB/cases/Pages/AdvancedSearch.aspx"). Then the button to show all results is clicked to get the complete list of cases, which is then retrieved. The case ID is later used to loop through the case detail pages, which are all static so the remote browser is no longer needed.


```r
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
```

## Get case data
The list of case IDs is used to loop through every individual case website. Before starting the loop some CSS selectors are defined. They identify the bits of data in the html file that we want to extract. The results are saved in ``data/cases_fresh.csv``.


```r
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
```

# Cleaning the data
## Start


```r
################################################################################
## File name: cleaning_claims.R                                               ##
## Author:    Maximillian Mantei                                              ##
## Date:      2015-08-04                                                      ##
## Purpose:   cleaning claims data                                            ##
################################################################################

## Loading packages
library(dplyr)
library(xlsx)

## Set working directory
wd <- "C:/GitHub/ICSID_claims/"
setwd(wd)

## RUN pulling_claims.R FIRST IF cases_fresh.csv DOES NOT EXIST!
# source("pulling_claims.R")

## Load the data generated by pulling_claims.R
cases <- read.csv("data/cases_fresh.csv", stringsAsFactors = FALSE)
```

## Extracting data from text strings
A lot of data is stored in chuncks of text. The following functions exploit some systematicity of the text strings to get the valuable parts. Also the structure of the data is changed to pairs of countries.


```r
## extract info and reformat variables
# year the claim was registered
cases$reg_year <- sapply(cases$registered, function(x){
      y <- strsplit(x, ", ")[[1]][2] # splits string at the ", "
      substr(y, 1,  4)} # cuts off everything that might come after the year
) %>% as.numeric() # converts string to number
# year the original case was concluded (or discontinued)
cases$out_year <- sapply(cases$outcome, function(x){
      y <- strsplit(x, ", ")[[1]][2] # splits string at the ", "
      substr(y, 1,  4)} # cuts off everything that might come after the year
) %>% as.numeric() # converts string to number
# creates a dummy; = 1 if there is a BIT invoked
cases$bit_claim <- as.numeric(cases$bit != "")
# creates a dummy; = 1 if instruments contains NAFTA
cases$nafta_claim <- as.numeric(grepl("NAFTA",cases$instrument))
# creates a dummy; = 1 if the claim is neither BIT nor NAFTA
cases$other_claim <- as.numeric((cases$bit_claim + cases$nafta_claim) == 0)

## get claimant natinalities (add every claimant in new row)
# extract whats between brackets in the claimants variable
claimants_list <- regmatches(cases$claimants, 
                             gregexpr("(?=\\().*?(?<=\\))",
                                      cases$claimants, perl=T))
# remove remaining brackets on the new variable
claimants_list <- lapply(claimants_list, function(x){
      gsub("[\\(\\)]", "",x)}
)
# get possibly more nationalities (i.e. when 2 countries are in one pair of brackets)
claimants_list <- lapply(claimants_list, function(x){
      x <- unlist(strsplit(x, split = ", "))
      unique(x) # get the unique values (when more than 2 companies are from same country)
})
# append the data by the number of claimants per case
cases <- cases[rep(1:nrow(cases), sapply(claimants_list, function(x){length(x)})),]
# add all claimants to the data
cases$claimants_nat <- unlist(claimants_list)
```

## Correcting errors
Remove artifacts or meaningless results. Create a dummy when the respondent to the claim is a state owned company.


```r
# correcting quirks in the claimants nationalities
cases$claimants_nat[cases$claimants_nat == "nationality not available"] <- NA
cases$claimants_nat[cases$claimants_nat == "Pty"] <- NA
cases$claimants_nat[cases$claimants_nat == "Panamá"] <- "Panama"
cases$claimants_nat[cases$claimants_nat == "Tallinn"] <- "Estonia"
cases$claimants_nat[cases$claimants_nat == "Indian Ocean"] <- NA
cases$claimants_nat[cases$claimants_nat == "Block A1"] <- NA
cases$claimants_nat[cases$claimants_nat == "Block A4"] <- NA
cases$claimants_nat[cases$claimants_nat == "G.P."] <- NA
cases$claimants_nat[cases$claimants_nat == "Lithuanian"] <- "Lithuania"
cases$claimants_nat[cases$claimants_nat == "G.P."] <- NA
cases$claimants_nat[cases$claimants_nat == "Group"] <- NA
cases$claimants_nat[cases$claimants_nat == "08-10"] <- NA
cases$claimants_nat[cases$claimants_nat == "Holding"] <- NA
cases$claimants_nat[cases$claimants_nat == "Pigap II"] <- NA
cases$claimants_nat[cases$claimants_nat == "Private"] <- NA
cases$claimants_nat[cases$claimants_nat == "Uruguayan"] <- "Uruguay"
cases$claimants_nat[cases$claimants_nat == "Private"] <- NA
cases$claimants_nat[cases$claimants_nat == "Swiss"] <- "Switzerland"
cases$claimants_nat[cases$claimants_nat == "Argentine"] <- "Argentina"
cases$claimants_nat[cases$claimants_nat == "Gambian"] <- "Gambia"
cases$claimants_nat[cases$claimants_nat == "ICRS"] <- NA
cases$claimants_nat[cases$claimants_nat == "PHC"] <- NA
cases$claimants_nat[cases$claimants_nat == "Wynne"] <- NA
cases$claimants_nat[cases$claimants_nat == "Services"] <- NA
cases$claimants_nat[cases$claimants_nat == "formerly Capote Farms"] <- NA
cases$claimants_nat[cases$claimants_nat == " Inc."] <- NA
cases$claimants_nat[cases$claimants_nat == "EMELEC"] <- NA
cases$claimants_nat[cases$claimants_nat == "Junior"] <- NA
cases$claimants_nat[cases$claimants_nat == "s"] <- NA
cases$claimants_nat[cases$claimants_nat == "Deutschland"] <- "Germany"
cases$claimants_nat[cases$claimants_nat == "formerly Enron Corporation"] <- NA
cases$claimants_nat[cases$claimants_nat == "Tanzanian"] <- "Tanzania"
cases$claimants_nat[cases$claimants_nat == "Middle East"] <- NA
cases$claimants_nat[cases$claimants_nat == "El Furrial"] <- "Venezuela"
# deleting NAs (can's be merged later) and duplicates
cases <- cases %>% filter(!is.na(claimants_nat)) %>% distinct()

# create "respondent is state owned company" dummy
cases$resp_state_comp <- 0 
cases$resp_state_comp[cases$Respondent.s. == "Boru Hatlari ile Petrol Tasima Anonim Sirketi"] <- 1
cases$resp_state_comp[cases$Respondent.s. == "Perupetro S.A."] <- 1
cases$resp_state_comp[cases$Respondent.s. == "Tanzania Electric Supply Company Limited"] <- 1
cases$resp_state_comp[cases$Respondent.s. == "Bangladesh Petroleum Exploration and Production Company Limited (\"Bapex\") and Bangladesh Oil Gas and Mineral Corporation (\"Petrobangla\")"] <- 1
cases$resp_state_comp[cases$Respondent.s. == "Bangladesh Petroleum Exploration & Production Company Limited (\"Bapex\") and Bangladesh Oil Gas and Mineral Corporation"] <- 1
cases$resp_state_comp[cases$Respondent.s. == "Empresa Estatal Petróleos del Ecuador (Petroecuador)"] <- 1

# correcting quirks in the respondents nationalities & code state comps as their hosts
cases$respondents_nat <- cases$Respondent.s.
cases$respondents_nat[cases$Respondent.s. == "Boru Hatlari ile Petrol Tasima Anonim Sirketi"] <- "Turkey"
cases$respondents_nat[cases$Respondent.s. == "Caravelí Cotaruse Transmisora de Energía S.A.C."] <- NA # private company
cases$respondents_nat[cases$Respondent.s. == "Perupetro S.A."] <- "Peru"
cases$respondents_nat[cases$Respondent.s. == "CMS Energy Corporation and others"] <- NA # private company
cases$respondents_nat[cases$Respondent.s. == "Tanzania Electric Supply Company Limited"] <- "Tanzania"
cases$respondents_nat[cases$Respondent.s. == "Tanzania Electric Supply Company Limited"] <- "Tanzania"
cases$respondents_nat[cases$Respondent.s. == "Bangladesh Petroleum Exploration and Production Company Limited (\"Bapex\") and Bangladesh Oil Gas and Mineral Corporation (\"Petrobangla\")"] <- "Bangladesh"
cases$respondents_nat[cases$Respondent.s. == "Bangladesh Petroleum Exploration & Production Company Limited (\"Bapex\") and Bangladesh Oil Gas and Mineral Corporation"] <- "Bangladesh"
cases$respondents_nat[cases$Respondent.s. == "Republic of Ecuador and Empresa Estatal Petróleos del Ecuador (Petroecuador)"] <- "Ecuador"
cases$respondents_nat[cases$Respondent.s. == "Republic of Ecuador and Consejo Nacional de Electricidad"] <- "Ecuador"
cases$respondents_nat[cases$Respondent.s. == "Empresa Estatal Petróleos del Ecuador (Petroecuador)"] <- "Ecuador"
cases$respondents_nat[cases$Respondent.s. == "Democratic Republic of the Congo and Générale des Carrières et des Mines"] <- "Congo, Democratic Republic"
cases$respondents_nat[cases$Respondent.s. == "Independent Power Tanzania Limited"] <- NA # private company
cases$respondents_nat[cases$Respondent.s. == "Bangladesh and Bangladesh Oil, Gas and Mineral Corporation"] <- "Bangladesh"
cases$respondents_nat[cases$Respondent.s. == "Arab Republic of Egypt and General Authority for Investment and Free Zones"] <- "Egypt"
cases$respondents_nat[cases$Respondent.s. == "Société Serete S.A."] <- NA # private company
cases$respondents_nat[cases$Respondent.s. == "PT Kaltim Prima Coal and others"] <- NA # private company
cases$respondents_nat[cases$Respondent.s. == "United Republic of Cameroon and Société Camerounaise des Engrais"] <- "Cameroon"

# deleting NAs (can's be merged later) and duplicates
cases <- cases %>% filter(!is.na(respondents_nat)) %>% distinct()
```

## Match WorldBank codes
The countries are assigned WB codes using a reference file. Results are saved (also as XLSX file for future references).


```r
## Set up WB codes for respondents and claimants
# load a dataset with references to the WB codes
country.codes <- read.csv("country codes/country codes.csv", sep=";", stringsAsFactors=FALSE)
# code respondents as their WB code and save them as host
# code claimants as their WB codes and save them as source
cases <- cases %>% 
      mutate(host = country.codes$WBcode[match(respondents_nat, country.codes$ICSIDname)],
             source = country.codes$WBcode[match(claimants_nat, country.codes$ICSID2name)])
# also check the alternative ICSID country names and add them
cases$host <- replace(cases$host, 
                      is.na(cases$host), 
                      country.codes$WBcode[match(
                            cases$respondents_nat[is.na(cases$host)], country.codes$ICSIDalt)]
)
cases$host <- replace(cases$host, 
                      is.na(cases$host), 
                      country.codes$WBcode[match(
                            cases$respondents_nat[is.na(cases$host)], country.codes$country)]
)
cases$source <- replace(cases$source, 
                        is.na(cases$source), 
                        country.codes$WBcode[match(
                              cases$claimants_nat[is.na(cases$source)], country.codes$country)]
)
# remove cases with missings on either host or source variable
cases <- cases %>% filter(!is.na(host) & !is.na(source) & host != source) %>% 
      distinct()
# save data
write.csv(cases, "data/cases.csv", append = FALSE)

## Export XLSX file
file_name <- paste0("data/ICSID_cases_reference_", Sys.Date(), ".xlsx")
write.xlsx(cases, file_name, sheetName = "ICSID", 
           col.names = TRUE, row.names = FALSE, append=FALSE, showNA = FALSE)

# To continue run formatting_claims.R
source("formatting_claims.R")
```

# Formatting the data
## Start


```r
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
```

## Generating summary variables 
First a grid of all possible combinations of **host**, **source** and all **year**s from 1975 to 2015 is generated. The case data is inserted and the summary variables are generated. HEAVY USE OF THE *dplyr* PACKAGE! Results are saved as CSV and DTA file (for a seamless merge later).


```r
## generate large data frame for the final merge
cases <- cases %>% 
      select(host, source, reg_year, out_year, bit_claim, nafta_claim, other_claim, resp_state_comp) %>%
      distinct()
# generate a grid of data
data <- expand.grid(host = unique(cases$host), 
                    source = unique(cases$source), 
                    year = 1975:2015, 
                    stringsAsFactors = FALSE)
# get the sum of registered claims per year for each host-source pair
data <- cases %>%
      select(-out_year) %>%
      group_by(host, source, reg_year) %>% 
      summarise(bit_claims_brought = sum(bit_claim, na.rm = TRUE), 
                nafta_claims_brought = sum(nafta_claim, na.rm = TRUE),
                other_claims_brought = sum(other_claim, na.rm = TRUE)) %>%
      rename(year = reg_year) %>%
      right_join(data)
# get the sum of concluded claims per year for each host-source pair
data <- cases %>%
      select(-reg_year) %>%
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
```


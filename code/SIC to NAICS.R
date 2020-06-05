#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#
# CoRisk: SEC SIC 1987 codes to NAICS 2017
# 2020-05-13 Fabian Braesemann
#
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%
# Loading packages
#%#%#%#%#%
library(tidyverse)    # numerous data wrangling packages
library(data.table)   # quick data loading
library(lubridate)    # Time series data wrangling
library(zoo)          # moving average
library(RColorBrewer) # Nice colours
library("readxl")
'%!in%' <- function(x,y)!('%in%'(x,y)) # opposite of %in% command

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Load data
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Merging tables from https://www.census.gov/eos/www/naics/concordances/concordances.html

sic1987naics2002 <- readxl::read_xls(paste(getwd(), "/1987_SIC_to_2002_NAICS.xls", sep =""))
naics2002naics2007 <- readxl::read_xls(paste(getwd(), "/2002_to_2007_NAICS.xls", sep =""))
naics2007naics2012 <- readxl::read_xls(paste(getwd(), "/2007_to_2012_NAICS.xls", sep =""))
naics2012naics2017 <- readxl::read_xlsx(paste(getwd(), "/2012_to_2017_NAICS.xlsx", sep =""))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Data Wrangling
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

sic1987naics2007 <- merge(sic1987naics2002, naics2002naics2007, by = c("NAICS2002","NAICS2002Title"))
sic1987naics2012 <- merge(sic1987naics2007, naics2007naics2012, by = c("NAICS2007","NAICS2007Title"))
sic1987naics2017 <- merge(sic1987naics2012, naics2012naics2017, by = c("NAICS2012","NAICS2012Title"))

sic1987naics2017 <- sic1987naics2017 %>% dplyr::select(SIC1987, NAICS2017, NAICS2017Title)
sic1987naics2017 <- sic1987naics2017[!duplicated(sic1987naics2017),]

#%#%#%#%
# Introduce bigger sections
#%#%#%#%
sic1987naics2017$NAICS2017TwoDigits <- substr(sic1987naics2017$NAICS2017,1,2)

# Sections from: https://www.census.gov/cgi-bin/sssd/naics/naicsrch?chart=2017

sic1987naics2017$Section <- ifelse(sic1987naics2017$NAICS2017TwoDigits == "11", "Agriculture",
       ifelse(sic1987naics2017$NAICS2017TwoDigits == "21", "Mining",
                     ifelse(sic1987naics2017$NAICS2017TwoDigits == "23", "Construction",
                            ifelse(sic1987naics2017$NAICS2017TwoDigits %in% c(31,32,33), "Manufacturing",
                                          ifelse(sic1987naics2017$NAICS2017TwoDigits %in% c(42, 44,45), "Wholesale & Retail",
                                                 ifelse(sic1987naics2017$NAICS2017TwoDigits %in% c(22, 48,49),	"Transportation & Utilities",
                                                        ifelse(sic1987naics2017$NAICS2017TwoDigits == "51", "Information",
                                                               ifelse(sic1987naics2017$NAICS2017TwoDigits == "52", "Finance",
                                                                             ifelse(sic1987naics2017$NAICS2017TwoDigits %in% c(53, 54, 55, 56), "Professional & Business Services",
                                                                                                  ifelse(sic1987naics2017$NAICS2017TwoDigits %in% c(61, 62), "Education & Health Services",
                                                                                                                ifelse(sic1987naics2017$NAICS2017TwoDigits %in% c(71,72), "Leisure & Hospitality",
                                                                                                                              ifelse(sic1987naics2017$NAICS2017TwoDigits == "81", "Other Services",
                                                                                                                                     ifelse(sic1987naics2017$NAICS2017TwoDigits == "92", "Public Administration", "NONE")))))))))))))

# Change SIC1987 column to character and add a leading 0 for 3-digit codes
sic1987naics2017$SIC1987 <- as.character(sic1987naics2017$SIC1987)
sic1987naics2017$SIC1987 <- ifelse(nchar(sic1987naics2017$SIC1987) == 3, paste("0",sic1987naics2017$SIC1987, sep = ""), sic1987naics2017$SIC1987)
# Just focus on first two digits
sic1987naics2017$SIC <- substr(sic1987naics2017$SIC1987, 1, 2)
sic_naicsSection <- sic1987naics2017 %>% dplyr::select(SIC, Section)
# Identify most common section per SIC
sic_naicsSection <- sic_naicsSection %>% group_by(SIC) %>% count(Section) %>% top_n(1) %>% dplyr::select(-n)


write.csv(sic_naicsSection,"sic1987naics2017.csv", row.names = FALSE)


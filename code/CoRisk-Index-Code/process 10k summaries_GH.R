##############################################################
# Process 10k report and sentence summaries
# Created: April 20, 2020 | Fabian Stephany
# Amended: SIC to NAICS, May 13, 2020 | Fabian Braesemann
##############################################################
##############################################################
# Load Packages
library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)
library(RColorBrewer)
library(scales)
library("rjson")
library(ggrepel)
library(readxl)
require(svMisc)
library(dplyr)
'%!in%' <- function(x,y)!('%in%'(x,y))
##############################################################
##############################################################
# Read relevant files
## Read industry classification dictornary (SIC 1987 to NAICS 2017)
sic_naics <- fread(paste(getwd(),"/sic1987naics2017.csv", sep = ""))
sic_naics$SIC <- ifelse(nchar(sic_naics$SIC) == 1, paste("0",sic_naics$SIC, sep = ""), sic_naics$SIC)
## Read sentence summaries
sentence_stats <- read_csv("10x_report_sentences.csv")
##############################################################
##############################################################
# Calculate count statistics
## Count number of words in sentence
sentence_stats$sentence_word_count <- str_count(sentence_stats$sentence_text, '\\w+')
## Count keywords
### Negative words (https://sraf.nd.edu/textual-analysis/resources/)
negative_words <- as.data.frame(read_excel("LoughranMcDonald_SentimentWordLists_2018.xlsx",sheet = "Negative", col_names = F))
#### Iterate over 2355 negative words in chunks of 500 words
sentence_stats$sentence_negative_count <- 0
n_words <- paste(str_to_lower(negative_words[,1])[0:500], collapse="|")
sentence_stats$sentence_negative_count <- sentence_stats$sentence_negative_count + str_count(sentence_stats$sentence_text, n_words)
#####
n_words <- paste(str_to_lower(negative_words[,1])[501:1000], collapse="|")
sentence_stats$sentence_negative_count <- sentence_stats$sentence_negative_count + str_count(sentence_stats$sentence_text, n_words)
#####
n_words <- paste(str_to_lower(negative_words[,1])[1001:1500], collapse="|")
sentence_stats$sentence_negative_count <- sentence_stats$sentence_negative_count + str_count(sentence_stats$sentence_text, n_words)
#####
n_words <- paste(str_to_lower(negative_words[,1])[1501:2000], collapse="|")
sentence_stats$sentence_negative_count <- sentence_stats$sentence_negative_count + str_count(sentence_stats$sentence_text, n_words)
#####
n_words <- paste(str_to_lower(negative_words[,1])[2001:2355], collapse="|")
sentence_stats$sentence_negative_count <- sentence_stats$sentence_negative_count + str_count(sentence_stats$sentence_text, n_words)


### Topical words
production_words <- c("business operation","business disruption", "product", "work stoppage","labor disruption","labor","work", 
                      "manufacturing operation","labor shortage", "employee productivity", "product development", "business activity")
supply_words = c("manufacturing facility","manufacture facility", "contract manufacturer","service provider", "logistic provider", 
                 "supply disruption", "party manufacturer","supply disruption","facility","supply","transportation delay", "delivery delay",
                 "supplier", "business partner", "supply chain", "material shortage")
travel_words = c("air travel","travel", "travel restriction", "airline industry","travel disruption")
demand_words = c("store closure","distribution channel", "market condition", "consumer spend", "market acceptance", "consumer confidence",
                 "consumer demand","consume","store", "customer", "store traffic")
finance_words = c("operating result", "cash flow", "stock price", "estate value","credit availability", "performance problem")

sentence_stats$sentence_production_count <- str_count(sentence_stats$sentence_text, paste(production_words, collapse="|"))
sentence_stats$sentence_supply_count <- str_count(sentence_stats$sentence_text, paste(supply_words, collapse="|"))
sentence_stats$sentence_travel_count <- str_count(sentence_stats$sentence_text, paste(travel_words, collapse="|"))
sentence_stats$sentence_demand_count <- str_count(sentence_stats$sentence_text, paste(demand_words, collapse="|"))
sentence_stats$sentence_finance_count <- str_count(sentence_stats$sentence_text, paste(finance_words, collapse="|"))

##############################################################
##############################################################
#### 10-k or 10-Q or both - this is the question #### 
df <- sentence_stats
sentence_stats <- df %>% filter(form == "10-K")
##############################################################
##############################################################
# Process 10k report data
## Read report summaries
report_stats <- sentence_stats[,c("date","sic","cik","report_corona_count")]
report_stats <- report_stats %>% group_by(date,sic,cik) %>% dplyr::summarise(report_corona_count = mean(report_corona_count))
## Give date
report_stats$date <- as.Date(as.character(report_stats$date), format = "%Y-%m-%d")
## Merge reports with industry dictonary
report_stats$SIC <- str_sub(report_stats$sic, 1,2)
#report_stats <- left_join(report_stats, isic_dict)
report_stats <- merge(report_stats, sic_naics, by.x = "SIC", by.y = "SIC")

#%#%#%#%#%#%#%#%#%
# INTERLUDE: Check distribution of NAICS Sections
#%#%#%#%#%#%#%#%#%

report_stats %>% group_by(Section) %>% summarise(count = n()) %>% arrange(desc(count))

#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%
# Deine major level industries
report_stats <- report_stats %>% filter(Section %in% c("Finance", "Manufacturing", "Professional & Business Services", "Transportation & Utilities", 
                                                       "Mining", "Wholesale & Retail", "Information", "Leisure & Hospitality"))

# Process 10k sentence data
## Keep sentences with corona count > 0
sentence_stats <- sentence_stats %>% filter(report_corona_count > 0)

## Give date
sentence_stats$date <- as.Date(as.character(sentence_stats$date), format = "%Y-%m-%d")
## Merge reports with industry dictonary
sentence_stats$SIC <- str_sub(sentence_stats$sic, 1,2)
#sentence_stats <- left_join(sentence_stats, isic_dict)
sentence_stats <- merge(sentence_stats, sic_naics, by.x = "SIC", by.y = "SIC")
# Deine major level industries
sentence_stats <-  sentence_stats %>% filter(Section %in% c("Finance", "Manufacturing", "Professional & Business Services", "Transportation & Utilities", 
                                        "Mining", "Wholesale & Retail", "Information", "Leisure & Hospitality"))
##############################################################
##############################################################
# Build main measure for figures
## Binary report corona count
report_stats$corona_binary <- ifelse(report_stats$report_corona_count > 0 , 1, 0)
## Report corona share, report corona count, report negative share
report_sections <- report_stats %>% group_by(Section, date) %>%
  summarise(no_reports = n(), report_corona_share = sum(corona_binary) / no_reports, 
            report_corona_count = mean(report_corona_count),
            report_corona_binary_count = sum(corona_binary)) %>%
  mutate(no_reports_sum = rollsum(no_reports, 10,fill = list(NA, NULL, NA), align = "right"))
## Sentence negative share and sentence topic share
sentence_stats$sentence_word_count <- as.numeric(sentence_stats$sentence_word_count)
sentence_stats$sentence_negative_count <- as.numeric(sentence_stats$sentence_negative_count)
sentence_stats$sentence_production_count <- as.numeric(sentence_stats$sentence_production_count)
sentence_stats$sentence_travel_count <- as.numeric(sentence_stats$sentence_travel_count)
sentence_stats$sentence_supply_count <- as.numeric(sentence_stats$sentence_supply_count)
sentence_stats$sentence_demand_count <- as.numeric(sentence_stats$sentence_demand_count)
sentence_stats$sentence_finance_count <- as.numeric(sentence_stats$sentence_finance_count)

sentence_sections <- sentence_stats %>% group_by(Section, date) %>%
  summarise(sentence_negative_share = mean(sentence_negative_count / sentence_word_count),
            sentence_production_share = mean(sentence_production_count / sentence_word_count),
            sentence_travel_share = mean(sentence_travel_count / sentence_word_count),
            sentence_supply_share = mean(sentence_supply_count / sentence_word_count),
            sentence_demand_share = mean(sentence_demand_count / sentence_word_count),
            sentence_finance_share = mean(sentence_finance_count / sentence_word_count))
# Bring report and sentence stats together
corisk_stats <- left_join(report_sections,sentence_sections)
## Set share to 0 if missing
corisk_stats$sentence_negative_share <- ifelse(is.na(corisk_stats$sentence_negative_share), 0, corisk_stats$sentence_negative_share)
corisk_stats$sentence_travel_share <- ifelse(is.na(corisk_stats$sentence_travel_share), 0, corisk_stats$sentence_travel_share)
corisk_stats$sentence_production_share <- ifelse(is.na(corisk_stats$sentence_production_share), 0, corisk_stats$sentence_production_share)
corisk_stats$sentence_demand_share <- ifelse(is.na(corisk_stats$sentence_demand_share), 0, corisk_stats$sentence_demand_share)
corisk_stats$sentence_supply_share <- ifelse(is.na(corisk_stats$sentence_supply_share), 0, corisk_stats$sentence_supply_share)
corisk_stats$sentence_finance_share <- ifelse(is.na(corisk_stats$sentence_finance_share), 0, corisk_stats$sentence_finance_share)

##############################################################
##############################################################
# Build CoRisk Index and Senitment Index
## CORISK is geometric mean of report corona count, report_corona_share, and corona sentence sentiment
corisk_stats <- corisk_stats %>% mutate(CoRisk_Index = (report_corona_count * sentence_negative_share)^(1/2))

smoother <- 14

corisk_stats <- corisk_stats %>% group_by(Section) %>% mutate(CoRisk_Index_ma = rollmean(CoRisk_Index, smoother,fill = list(NA, NULL, NA), align = "right"),
                                                              CoRisk_Index_ma_left = rollmean(CoRisk_Index, 7 ,fill = list(NA, NULL, NA), align = "right"))
## Moving average of negative sentence share
corisk_stats <- corisk_stats %>% group_by(Section) %>% mutate(sentence_negative_share_ma = rollmean(sentence_negative_share, smoother,fill = list(NA, NULL, NA), align = "right"),
                                                              sentence_negative_share_ma_left = rollmean(sentence_negative_share, 7,fill = list(NA, NULL, NA), align = "right"))

##############################################################
##############################################################
# Rearrange Tables and Calculate "All Industry" Average
## Keep only measure relevant for figures
corisk_stats <- corisk_stats[,c("date","Section","CoRisk_Index","CoRisk_Index_ma","CoRisk_Index_ma_left","no_reports","report_corona_count","report_corona_binary_count",
                                "report_corona_share","sentence_negative_share","sentence_negative_share_ma","sentence_negative_share_ma_left",
                                "sentence_production_share","sentence_travel_share","sentence_demand_share","sentence_supply_share","sentence_finance_share")]
## Calculate mean across all industries
corisk_mean <- corisk_stats %>% group_by(date) %>% summarise(CoRisk_Index = mean(CoRisk_Index, na.rm = T),
                                                             report_corona_count = mean(report_corona_count, na.rm = T),
                                                             report_corona_binary_count = sum(report_corona_binary_count, na.rm = T),
                                                             no_reports = sum(no_reports, na.rm = T),
                                                             report_corona_share = mean(report_corona_share, na.rm = T),
                                                             sentence_negative_share = mean(sentence_negative_share,na.rm = T),
                                                             sentence_production_share = mean(sentence_production_share, na.rm = T),
                                                             sentence_travel_share = mean(sentence_travel_share, na.rm = T),
                                                             sentence_demand_share = mean(sentence_demand_share,na.rm = T),
                                                             sentence_supply_share = mean(sentence_supply_share, na.rm = T),
                                                             sentence_finance_share = mean(sentence_finance_share,na.rm = T))

corisk_mean <- corisk_mean %>% mutate(CoRisk_Index_ma = rollmean(CoRisk_Index, smoother,fill = list(NA, NULL, NA), align = "right"),
                                      CoRisk_Index_ma_left = rollmean(CoRisk_Index, 10,fill = list(NA, NULL, NA), align = "right"))
corisk_mean$CoRisk_Index_ma <- ifelse(is.na(corisk_mean$CoRisk_Index_ma), corisk_mean$CoRisk_Index_ma_left, corisk_mean$CoRisk_Index_ma)
## Moving average of negative sentence share
corisk_mean <- corisk_mean %>% mutate(sentence_negative_share_ma = rollmean(sentence_negative_share, smoother,fill = list(NA, NULL, NA), align = "right"),
                                      sentence_negative_share_ma_left = rollmean(sentence_negative_share, 10, fill = list(NA, NULL, NA), align = "right"))
corisk_mean$sentence_negative_share_ma <- ifelse(is.na(corisk_mean$sentence_negative_share_ma), corisk_mean$sentence_negative_share_ma_left, corisk_mean$sentence_negative_share_ma)

corisk_mean$Section <- "All Industries"
## Create final WIIIIDE table
corisk_stats <- rbind(data.frame(corisk_stats),data.frame(corisk_mean))
## Gather measures in LOOOOONG table
corisk_stats <- gather(corisk_stats, key, value, -date, -Section)
## Remove Section "Other Industries"
corisk_stats <- corisk_stats %>% filter(Section != "Other Industries")
##############################################################
##############################################################
# Store final spreadsheet for CoRisk Dashboard
write.csv(corisk_stats, paste(dirname(getwd()), "/shiny/CoRisk_data.csv", sep ="") )
##############################################################
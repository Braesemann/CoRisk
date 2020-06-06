#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#
# CoRisk: CoRisk-Index Supplement Figures
# 2020-06-05 Fabian Braesemann
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
library(ggpubr)       # Arrange plots
library(scales)       # Extra axis scales for log and %
library(broom)        # Package for groupwise regressions
library("pracma")     # Package for groupwise regressions
'%!in%' <- function(x,y)!('%in%'(x,y)) # opposite of %in% command

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figures S 1, S 2, S 3, S 4, S 5, S 8, S 14, and S 15
# show screenshot, explorative data views from other programs or simple data aggregations.
# For these plots, software code is not availablbe.
#
# Note, for the reproduction of Figures S 9 and S 12 you need to download
# 10-k report sentiment history data from:
# https://sraf.nd.edu/textual-analysis/resources/#LM_10X_Summaries
# 
# All other figures should be reproducible with this script and with Supplement_Figures_Data.zip
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figure S 6 Dotplot
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Load raw data on the report (sentence) level
sentence_stats <- fread(paste(getwd(), "/sentence_stats.csv", sep = ""))

# Compute number of corona mentions per report 
dotplot_data <- sentence_stats %>% group_by(Section,report_link) %>% summarise(report_corona_count = mean(report_corona_count))

# For the plot, abbreviate some sectors
dotplot_data$Section <- ifelse(dotplot_data$Section == "Professional & Business Services", "Professional &\nBusiness Services", 
                               dotplot_data$Section)
dotplot_data$Section <- ifelse(dotplot_data$Section == "Transportation & Utilities", "Transportation &\nUtilities", 
                               dotplot_data$Section)
dotplot_data$Section <- ifelse(dotplot_data$Section == "Leisure & Hospitality", "Leisure &\nHospitality", 
                               dotplot_data$Section)
dotplot_data$Section <- ifelse(dotplot_data$Section == "Wholesale & Retail", "Wholesale &\nRetail", 
                               dotplot_data$Section)
# Arrange them
dotplot_data$Section <- factor(dotplot_data$Section, levels = c("Mining","Transportation &\nUtilities",
                                                                "Finance", 
                                                                "Leisure &\nHospitality",
                                                                "Information", 
                                                                "All Industries", 
                                                                "Professional &\nBusiness Services",
                                                                "Manufacturing","Wholesale &\nRetail"))

# Plot
dotplot_data %>% filter(report_corona_count< 100) %>%
  ggplot(aes(x = Section, y = report_corona_count)) +
  geom_point(position = position_jitter(0.2,0.2), aes(fill = Section), 
             shape = 21, size = 2.5, alpha = 0.7, stroke = 0.2, colour = "black") +
  geom_boxplot(fill = "NA",lwd = 0.5, width = 0.5, colour = "black", outlier.alpha = 0,coef = 1) +
  scale_y_log10() + annotation_logticks(sides = "lr", colour = "grey") + 
  scale_fill_manual(values = c(brewer.pal(9,"RdBu")[9],brewer.pal(9,"RdBu")[8],
                               brewer.pal(9,"RdBu")[7],brewer.pal(9,"RdBu")[6],
                               brewer.pal(9,"Reds")[1],brewer.pal(9,"RdBu")[3],
                               brewer.pal(9,"RdBu")[2],brewer.pal(9,"RdBu")[1])) + 
  labs(x = "", y = "No. of 'corona' mentions per report", fill = "")+
  theme_bw() + theme(text = element_text(size = 18),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x = element_blank(), legend.position = "none", 
                     axis.text.x = element_text(angle = 90, hjust = 1))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figure S 7 Stocks per industry
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Load stocks data
stocks <- fread(paste(getwd(), "/df_stocks_15_05.csv", sep = ""), header = T)

# Wrangle stock data
stocks <- gather(stocks, CIK, value, -Date)
stocks$date <- stocks$Date # rename date column
stocks <- stocks %>% dplyr::select(-Date) # delete 'Date' column
stocks$date <- as.Date(stocks$date, format = "%Y-%m-%d") # date format

# Load CIK data (containing SIC per CIK) to match with sector level
ciks <- readxl::read_xlsx(paste(getwd(),"/cik_results.xlsx", sep = ""))
# We only need one value per CIK
ciks <- ciks %>% group_by(CIK) %>% slice(which.max(Date))

# Load SIC to NAICS file which contains Sector information
sic_naics <- fread(paste(getwd(),"/sic1987naics2017.csv", sep = ""))
sic_naics$SIC <- ifelse(nchar(sic_naics$SIC) == 1, paste("0",sic_naics$SIC, sep = ""), sic_naics$SIC)

## Merge CIK data with industry dictonary
ciks$SIC <- str_sub(ciks$SIC, 1,2)
ciks <- merge(ciks, sic_naics, by.x = "SIC", by.y = "SIC")
ciks <- ciks %>% dplyr::select(CIK, Section) # Keep relevant columns

# Merge stocks and CIK data
stocks <- merge(stocks, ciks, by = "CIK")
# Calculate daily average stock price per industry
stocks <- stocks %>% group_by(Section, date) %>% summarise(Diff = mean(value, na.rm = T))
# Filter only relevant sectors
stocks <- stocks %>% filter(Section %in% c("Mining", "Manufacturing", "Information", "Wholesale & Retail",
                                           "Finance", "Transportation & Utilities", 
                                           "Professional & Business Services", "Leisure & Hospitality"))

# Load CoRisk data for lower panel of each plot
corisk <- fread(paste(getwd(), "/CoRisk_data.csv", sep = ""))
# Keep only relevant columns and rows
corisk <- corisk %>% filter(key == "sentence_negative_share_ma") %>% dplyr::select(date, Section, negativity = value)
corisk$date <- as.Date(corisk$date, format = "%Y-%m-%d") # date format

# Merge CoRisk and stock market data
corisk_stocks <- merge(corisk, stocks, by = c("Section","date"))

# Wide to long format for plotting
corisk_stocks <- corisk_stocks %>% gather(key, value, - date, - Section)


# Plot all eight sectors individually (they have different y axes) and store as plots
a <- corisk_stocks %>% filter(date != "2020-05-01", Section == "Finance") %>% 
  ggplot(aes(x = date, y = value, col = key)) + geom_line() +
  facet_grid(key~Section, scales = "free") +
  theme(legend.position = "none")

b <- corisk_stocks %>% filter(date != "2020-05-01", Section == "Manufacturing") %>% 
  ggplot(aes(x = date, y = value, col = key)) + geom_line() +
  facet_grid(key~Section, scales = "free") +
  theme(legend.position = "none")

c <- corisk_stocks %>% filter(date != "2020-05-01", Section == "Information") %>% 
  ggplot(aes(x = date, y = value, col = key)) + geom_line() +
  facet_grid(key~Section, scales = "free") +
  theme(legend.position = "none")

d <- corisk_stocks %>% filter(date != "2020-05-01", Section == "Wholesale & Retail") %>% 
  ggplot(aes(x = date, y = value, col = key)) + geom_line() +
  facet_grid(key~Section, scales = "free") +
  theme(legend.position = "none")

e <- corisk_stocks %>% filter(date != "2020-05-01", Section == "Mining") %>% 
  ggplot(aes(x = date, y = value, col = key)) + geom_line() +
  facet_grid(key~Section, scales = "free") +
  theme(legend.position = "none")

f <- corisk_stocks %>% filter(date != "2020-05-01", Section == "Transportation & Utilities") %>% 
  ggplot(aes(x = date, y = value, col = key)) + geom_line() +
  facet_grid(key~Section, scales = "free") +
  theme(legend.position = "none")

g <- corisk_stocks %>% filter(date != "2020-05-01", Section == "Professional & Business Services") %>% 
  ggplot(aes(x = date, y = value, col = key)) + geom_line() +
  facet_grid(key~Section, scales = "free") +
  theme(legend.position = "none")

h <- corisk_stocks %>% filter(date != "2020-05-01", Section == "Leisure & Hospitality") %>% 
  ggplot(aes(x = date, y = value, col = key)) + geom_line() +
  facet_grid(key~Section, scales = "free") +
  theme(legend.position = "none")

# Arrange all plots
ggarrange(a,b,c,d,e,f,g,h, ncol = 2, nrow = 4)

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figure S 12 Negativity and Recessions
# Figure S  9 Calendar representativeness
#
# Both require the same dataset.
#
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Load historical sentiment data

# Sentiment history
history <- fread(paste(getwd(),"/LM_10X_Summaries_2018.csv", sep =""))

#%#%#%#%
# 10-k report sentiment history from:
# https://sraf.nd.edu/textual-analysis/resources/#LM_10X_Summaries
#
# --> YOU NEED TO DOWNLOAD THE FILE FROM THE WEBSITE, IF YOU WANT
#     TO REPRODUCE FIGURE S 9 AND S 12
#
#%#%#%#%

# GDP and unemployment history
unrate <- fread(paste(getwd(), "/unrate.csv", sep =""))

#%
# Quarterly GDP and Unemployment data from:
# https://research.stlouisfed.org/econ/mccracken/fred-databases/
#%

# Create date column
history$FILING_DATE <- as.Date(as.character(history$FILING_DATE), format = "%Y%m%d")

# Calculate share or negative and positive words
history <- history %>% mutate(S_Negative = N_Negative / N_Words)

# Get quarterly averages from daily data
negativity_q <- history %>% group_by(month = floor_date(FILING_DATE, "quarter")) %>% 
  summarise(S_Negative = mean(S_Negative,na.rm = T), count = n()) %>% filter(month > "1996-09-01")
# Calculate smoothed four-quarter moving averages
negativity_q <- negativity_q %>% 
  mutate(m.av.S_Negative = rollmean(S_Negative, 4,fill = list(NA, NULL, NA), align = "right"))


# GDP Time series and differentiated
gdp <- ts(unrate$gdp,start = c(1996,4), frequency = 4)
diff_gdp <- diff(log(gdp), lag =4)

# Unemployment rate to TS
unr <- ts(unrate$unrate,start = c(1996,4), frequency = 4)

negativity_q <- ts(negativity_q$m.av.S_Negative, start = c(1996,4), frequency = 4)
#negativity_q <- diff(negativity_q, lag =4)

# One joint time-series of all three variables
df4 <- ts(data.frame(GDP_change = diff_gdp, # Change in GDP
                     unr = unr[5:length(unr)], # Unemployment rate
                     Negativity = negativity_q[5:length(negativity_q)]),
          #Negativity = negativity_q$m.av.S_Negative[5:nrow(negativity_q)]), # negativity
          start = c(1997,4), frequency = 4)

# For plotting: TS to data frame
df_new <- data.frame(Y=as.matrix(df4), date=time(df4))

# Multiply decimal values * 100 for better labels
df_new$Y.GDP_change <- df_new$Y.GDP_change *100
df_new$Y.Negativity <- df_new$Y.Negativity *100

# Wide to long DF
df_new <- gather(df_new, key, value, -date)

# Rename the factors and arrange factors
df_new$key <- ifelse(df_new$key == "Y.GDP_change", "Change in GDP (%)",
                     ifelse(df_new$key == "Y.unr", "Unemployment rate (%)","10k-report negativity (%)"))
df_new$key <- factor(df_new$key, levels = c("Change in GDP (%)","Unemployment rate (%)", "10k-report negativity (%)"))


# Plot Figure S 12
ggplot()  +
  # Include rectangle for first recession
  geom_rect(data = data.frame(xmin = decimal_date(as.Date(c("2001-03-01"))),
                              xmax = decimal_date(as.Date(c("2001-08-01"))),
                              ymin = -Inf,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  # Include rectangle for second recession
  geom_rect(data = data.frame(xmin = decimal_date(as.Date(c("2007-12-01"))),
                              xmax = decimal_date(as.Date(c("2009-06-01"))),
                              ymin = -Inf,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  # Line plot
  geom_line(data = df_new,aes(x = date, y = value, col = key), lwd = 0.9) + 
  # Facets
  facet_wrap(~key, ncol = 1, scales = "free_y") + 
  # X-scale and colours
  scale_x_continuous(limits = c(2000,2018), expand = c(0, 0)) +
  scale_colour_manual(values = c(brewer.pal(11,"RdBu")[2], brewer.pal(11,"RdBu")[3], brewer.pal(11,"RdBu")[9])) +
  # Axis labels and titles
  labs(x = "", y = "", caption = "NBER recessions highlighted as grey bars",
       subtitle = "Macroeconomic development and 10-k report text negativity") +
  # Edit layout
  theme_bw() +
  theme(legend.position = "none", text = element_text(size = 16), panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

#%
# Recessions from NBER:
# https://www.nber.org/cycles.html
#%

#%#%#%#
# Figure S 9
# Calendar Representativeness
#%#%#%#

## Read industry classification dictornary (SIC 1987 to NAICS 2017)
sic_naics <- fread(paste(getwd(),"/sic1987naics2017.csv", sep = ""))
sic_naics$SIC <- ifelse(nchar(sic_naics$SIC) == 1, paste("0",sic_naics$SIC, sep = ""), sic_naics$SIC)

## Merge with Sections
## Merge reports with industry dictonary
history$SIC <- str_sub(history$SIC, 1,2)
history <- merge(history, sic_naics, by.x = "SIC", by.y = "SIC")

# Filter relevant Sections
history <- history %>% filter(Section %in% c("Professional & Business Services",
                                             "Information", "Leisure & Hospitality",
                                             "Manufacturing", "Finance", "Wholesale & Retail",
                                             "Mining", "Transportation & Utilities"))

# Keep relevant columns and calculate no. of reports per quarter
history <- history %>% dplyr::select(Section, FILING_DATE) %>% 
  group_by(Section, quarter = floor_date(FILING_DATE, "quarter")) %>% summarise(count = n())

# Axis lables function
format_quarters <- function(x) {
  x <- as.yearqtr(x)
  year <- as.integer(x)
  quart <- as.integer(format(x, "%q"))
  
  paste(c("Jan-Mar","Apr-Jun","Jul-Sep","Oct-Dec")[quart], 
        year)
}

# Plot S 9
history %>% filter(quarter > "2013-12-31") %>%
  ggplot(aes(x = quarter, y = count, fill = Section)) + geom_area(position='fill') +
  scale_x_date(expand = c(0, 0), breaks = date_breaks("3 months"), label = format_quarters) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  scale_fill_manual(values = c(brewer.pal(9,"RdBu")[9],brewer.pal(9,"RdBu")[8],
                               brewer.pal(9,"RdBu")[7],brewer.pal(9,"RdBu")[6],
                               brewer.pal(9,"Greys")[4],brewer.pal(9,"RdBu")[4],
                               brewer.pal(9,"RdBu")[3],brewer.pal(9,"RdBu")[2],
                               brewer.pal(9,"RdBu")[1])) + 
  labs(x = "", y = "Share of reports per industry", fill = "") +
  theme(text = element_text(size = 12), legend.position = "bottom", axis.text.x = element_text(angle=45, vjust = 0.5))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figure S 10 Trends in Unemployment data
# Figure S 11 Spurious regression
#
# Both require the same dataset.
#
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Load unemployment initial claims from Andrew Van Dam's github data set
ui <- read.csv("https://raw.githubusercontent.com/Economic/ui_state_detailed/master/output/state_ui_industry_recoded.csv")


# Calculate weekly sums of the initial claims and emplyoment for all states together
ui <- ui %>% group_by(sector,sectorname, endweek) %>% summarise(ic = sum(ic), emp = sum(emp))

# Have a look at the different sectors
ui %>% group_by(sector,sectorname) %>% summarise(count = n()) %>% print(n = 50)

# Map the NAICS sectors to the nomenclature we are using in the paper (WE WILL NEED A TABLE IN THE APPENDIX ABOUT IT)
ui$Section <- ifelse(ui$sector == "11", "Agriculture",
                     ifelse(ui$sector == "21", "Mining",
                            ifelse(ui$sector %in% c("22-23","23"), "Construction",
                                   ifelse(ui$sector %in% "31-33", "Manufacturing",
                                          ifelse(ui$sector %in% c("42", "44-45"), "Wholesale & Retail",
                                                 ifelse(ui$sector %in% c("22", "48-49", "1021"),	"Transportation & Utilities",
                                                        ifelse(ui$sector == "51", "Information",
                                                               ifelse(ui$sector %in% c("52","1023"), "Finance",
                                                                      ifelse(ui$sector %in% c("53", "54", "55", "56", "1024"), "Professional & Business Services",
                                                                             ifelse(ui$sector %in% c("61", "62"), "Education & Health Services",
                                                                                    ifelse(ui$sector %in% c("71","72","1026"), "Leisure & Hospitality",
                                                                                           ifelse(ui$sector == "81", "Other Services",
                                                                                                  ifelse(ui$sector == "92", "Public Administration", "NONE")))))))))))))

# Calculate the initial claims and employment for "all industries" in the US
ui_all <- ui %>% group_by(endweek) %>% summarise(ic = sum(ic), emp = sum(emp)) %>% 
  mutate(ic_share = ic / emp, ic_sum = cumsum(ic_share))
ui_all$Section <- "All Industries" # Give it a name
ui_all <- data.frame(ui_all %>% dplyr::select(Section, endweek, ic, emp, ic_share, ic_sum)) # Make it an ordered DF

# For each industry section (our nomenclature) calculate the sum of the initial claims and employment
ui <- data.frame(ui %>% group_by(Section, endweek) %>% summarise(ic = sum(ic), emp = sum(emp)) %>% 
                   mutate(ic_share = ic / emp, ic_sum = cumsum(ic_share))) # Then, calculate the share of ic's per employment and the sum
# Combine with "all industries"
ui <- rbind(ui, ui_all)

# CoRisk data (from oxford.berlin/corisk)
corisk <- fread(paste(getwd(), "/CoRisk_data.csv", sep = ""))

corisk <- corisk %>% filter(key == "CoRisk_Index_ma") # Just keep the CoRisk-MA variable
corisk$date <- as.Date(corisk$date, format = "%Y-%m-%d") # date column
# Calculate the weekly averaage
corisk <- corisk %>% group_by(Section, week = floor_date(date, "week", week_start = 7)) %>% 
  summarise(CoRisk_Index = mean(value, na.rm = T))
# Make the week column coincide with the 'endweek' column from AVD
corisk$week <- corisk$week + 6

# Merge CoRisk and UI data
corisk <- merge(corisk, ui, by.x = c("Section", "week"), by.y = c("Section", "endweek"))

# New DF for the plot
corisk2 <- corisk %>% dplyr::select(-ic, -emp) # remove redundant columns
corisk2 <- corisk2 %>% gather(key, value, -Section, -week, -ic_share, -ic_sum) # To long format

corisk_all <- corisk %>% group_by(Section) %>% mutate(ic_sum_Diff = ic_sum / lag(ic_sum),
                                                      CoRisk_Diff = CoRisk_Index / lag(CoRisk_Index),
                                                      CoRisk_Diff_lag = lag(CoRisk_Diff))

# Groupwise regression of log initial claims ~ log CoRisk-Index
dfHour = corisk_all %>% 
  group_by(Section) %>%
  do(fitHour = lm(ic_sum ~ CoRisk_Index, data = .))

# Get R2
dfHourCoef = glance(dfHour, fitHour)
dfHourCoef <- dfHourCoef[,c(1:2)] # Just extract the R2 column
dfHourCoef <- dfHourCoef %>% mutate(r.squared = round(r.squared,2))

# Get estimates
dfHourCoef2 = tidy(dfHour, fitHour)  
dfHourCoef2 <- dfHourCoef2 %>% filter(term == "CoRisk_Index") %>% dplyr::select(Section, estimate) %>%
  mutate(estimate = round(estimate, 2)) # Just extract the estimate column

# Merge CoRisk and R2 column
corisk_all <- merge(corisk_all, dfHourCoef, by = "Section")
# Merge CoRisk and estimate column
corisk_all <- merge(corisk_all, dfHourCoef2, by = "Section")
# Display only once per facet (avoid overplotting)
corisk_all$r.squared_label <- ifelse(corisk_all$week == "2020-03-14", 
                                     paste("R2 =", corisk_all$r.squared), NA)
# Display only once per facet (avoid overplotting)
corisk_all$estimate_label <- ifelse(corisk_all$week == "2020-03-14", 
                                    paste("b =", corisk_all$estimate), NA)

corisk_all$Section_label <- ifelse(corisk_all$Section == "Professional & Business Services", 
                                   "Prof. & Business Services", corisk2$Section)

# Reorder facets by b value
corisk_all$Section_label <- factor(corisk_all$Section_label, levels = c("Leisure & Hospitality", "Manufacturing", "Wholesale & Retail",
                                                                        "All Industries","Information", "Prof. & Business Services",
                                                                        "Transportation & Utilities","Finance","Mining"))

# Plot the spurious correlation
corisk_all %>%
  ggplot(aes(x = CoRisk_Index, y = ic_sum*100, col = Section_label, fill = Section_label)) + facet_wrap(~Section_label) + 
  geom_point(shape = 21, size = 2.5, col = "black", stroke = 0.3) +        # Points
  geom_smooth(se = F, span = 2, lwd = 1, show.legend = F, method = "lm") + # Trend
  # Display R2, b, and week
  geom_text(aes(x = 1.05, y = 9, label = r.squared_label), color = 'black') +
  geom_text(aes(x = 1.05, y = 5, label = estimate_label), color = 'black') +
  scale_fill_brewer(palette = "RdBu") + scale_colour_brewer(palette = "RdBu") +
  # Theme and axes
  theme_bw() +
  labs(x = "CoRisk-Index (weekly average)", y = "Sum of weekly unemployment initial claims\n(% of total employment)", col = "",
       title = "CoRisk-Index and unemployment initial claims (SPURIOUS CORRELATION") +
  theme(legend.position = "none", text = element_text(size = 14), legend.text = element_text(size = 9),
        panel.grid.minor = element_blank(), strip.background =element_rect(fill=brewer.pal(9,"Greys")[2]))


#%#%#%#%
# Figure S 10 Trends in the unemployment data
#%#%#%#%

# Keep only relevant columns
corisk_all2 <- corisk_all %>% dplyr::select(Section,Section_label, week, ic_share, 
                                            CoRisk_Diff, CoRisk_Diff_lag, CoRisk_Index, ic_sum)

# Wide to long
corisk_all_long <- gather(corisk_all2, key, value, - Section, -week, - Section_label)

# Plot # There are trends in the data!
corisk_all_long %>%
  ggplot(aes(x = week, y = value, col = key)) + facet_wrap(~Section_label, scales = "free") +
  geom_line(aes(group = key),lwd = 1.1, col = "black") +
  geom_line(lwd = 1) +
  scale_fill_brewer(palette = "RdBu") + scale_colour_brewer(palette = "RdBu") +
  # Theme and axes
  theme_bw() +
  labs(x = "Week", y = "Value", col = "",
       title = "Trends of CoRisk and Unemployment initial claims") +
  theme(legend.position = "bottom", text = element_text(size = 14), legend.text = element_text(size = 9),
        panel.grid.minor = element_blank(), strip.background =element_rect(fill=brewer.pal(9,"Greys")[2]))


#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Figure S 13 US-China Trade War
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%


# Read prepared data
dfs <- fread(paste(getwd(), "/china_sentiment_data.csv", sep = ""))
# Calculate negativity share
dfs <- dfs %>% mutate(negativity_share = sentence_negative_count / sentence_word_count)
# Get date column
dfs$file_id <- as.character(dfs$file_id)
dfs$date <- sapply(dfs$file_id, function(x) strsplit(x,"_")[[1]][1])
dfs$date <- as.Date(dfs$date, format = "%Y%m%d")

# Calculate the negativity share per day and the MA
negativity <- dfs %>% group_by(date) %>% summarise(negativity_share = mean(negativity_share)) %>% 
  filter(negativity_share < 0.1) %>%
  mutate(negativity_m.av = rollmean(negativity_share, 14,fill = list(NA, NULL, NA), align = "right"),
         # For the left end, calculate shorter 7 day MA
         negativity_m.av_left = rollmean(negativity_share, 7,fill = list(NA, NULL, NA), align = "right"))

negativity$negativity_m.av <- ifelse(is.na(negativity$negativity_m.av), 
                                     negativity$negativity_m.av_left, negativity$negativity_m.av)

# Time plot with events highlighted
ggplot()  +
  # Jan, 22 - Trump: tariffs on solar panels
  geom_rect(data = data.frame(xmin = as.Date(c("2018-01-22")),
                              xmax = as.Date(c("2018-01-24")),
                              ymin = -Inf,
                              ymax = 0.021),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = brewer.pal(9,"RdYlGn")[1], alpha = 0.5) +
  annotate(geom="text",x=as.Date("2018-01-24"),
           y=0.017,label="Jan, 22:\nTrump: tariffs\non solar panels",fontface="bold", size = 3.5, hjust = 0) +
  # Mar, 01 - Steel tariffs
  geom_rect(data = data.frame(xmin = as.Date(c("2018-03-01")),
                              xmax = as.Date(c("2018-03-03")),
                              ymin = -Inf,
                              ymax = 0.0265),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = brewer.pal(9,"RdYlGn")[1], alpha = 0.5) +
  annotate(geom="text",x=as.Date("2018-03-03"),
           y=0.021,label="Mar, 01:\nTrump: tariffs\non steel",fontface="bold", size = 3.5, hjust = 0) +
  # May 15 - Trade talks in DC
  geom_rect(data = data.frame(xmin = as.Date(c("2018-05-14")),
                              xmax = as.Date(c("2018-05-16")),
                              ymin = 0.0292,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = brewer.pal(9,"RdYlGn")[9], alpha = 0.5) +
  annotate(geom="text",x=as.Date("2018-05-16"),
           y=0.035,label="May, 15:\nTrade talks\nin Washington",fontface="bold", size = 3.5, hjust = 0) +
  # May 29 - higher tariffs
  geom_rect(data = data.frame(xmin = as.Date(c("2018-05-29")),
                              xmax = as.Date(c("2018-05-31")),
                              ymin = -Inf,
                              ymax = 0.0277),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = brewer.pal(9,"RdYlGn")[1], alpha = 0.5) +
  annotate(geom="text",x=as.Date("2018-05-31"),
           y=0.017,label="May, 29:\nUS announce\nhigher tariffs",fontface="bold", size = 3.5, hjust = 0) +
  # Jul 6 - tariffs start
  geom_rect(data = data.frame(xmin = as.Date(c("2018-07-06")),
                              xmax = as.Date(c("2018-07-08")),
                              ymin = -Inf,
                              ymax = 0.0261),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = brewer.pal(9,"RdYlGn")[1], alpha = 0.5) +
  annotate(geom="text",x=as.Date("2018-07-08"),
           y=0.021,label="Jul, 06:\nUS tariffs\nstart",fontface="bold", size = 3.5, hjust = 0) +
  # Aug 14 - WTO
  geom_rect(data = data.frame(xmin = as.Date(c("2018-08-14")),
                              xmax = as.Date(c("2018-08-16")),
                              ymin = 0.031,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = brewer.pal(9,"RdYlGn")[9], alpha = 0.5) +
  annotate(geom="text",x=as.Date("2018-08-16"),
           y=0.035,label="Aug, 14:\nChinese\nWTO complaint",fontface="bold", size = 3.5, hjust = 0) +
  # Sep 24 - More tariffs
  geom_rect(data = data.frame(xmin = as.Date(c("2018-09-23")),
                              xmax = as.Date(c("2018-09-25")),
                              ymin = -Inf,
                              ymax = 0.027),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = brewer.pal(9,"RdYlGn")[1], alpha = 0.5) +
  annotate(geom="text",x=as.Date("2018-09-25"),
           y=0.017,label="Sep, 24:\nMore US\ntariffs start",fontface="bold", size = 3.5, hjust = 0) +
  # Dec 02 - Postponed tariffs
  geom_rect(data = data.frame(xmin = as.Date(c("2018-12-02")),
                              xmax = as.Date(c("2018-12-04")),
                              ymin = 0.0295,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = brewer.pal(9,"RdYlGn")[9], alpha = 0.5) +
  annotate(geom="text",x=as.Date("2018-12-04"),
           y=0.035,label="Dec, 02:\nTrade talks\nafter G-20",fontface="bold", size = 3.5, hjust = 0) +
  # Line plot
  geom_line(data = negativity,aes(x = date, y = negativity_m.av), lwd = 0.5, col = "black") +
  # Smoother
  geom_smooth(data = negativity,aes(x = date, y = negativity_m.av),  lwd = 0.7, col = "darkgrey", span = 0.25, se = F, lty = 4) +
  theme_bw() +
  labs(x = "", y = "Share of negative words in 'china'-sentences") +
  scale_y_continuous(limits = c(0.015,0.037), expand = c(0,0), labels = percent_format()) +
  theme(legend.position = "none", text = element_text(size = 14), panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())


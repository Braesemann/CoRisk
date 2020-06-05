#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#
# CoRisk: CoRisk-Index Paper Figures
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
# Figure 1 A and C
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

sup <- fread(paste(getwd(),"/SuP1200_200601.csv", sep = ""))
sup <- sup %>% dplyr::select(date = `Effective date`, sup = `S&P GLOBAL 1200`)
sup$date <- as.Date(sup$date, format = "%d.%m.%y")

corisk <- fread(paste(getwd(), "/CoRisk_data.csv", sep = ""))
corisk <- corisk %>% filter(key == "sentence_negative_share_ma", Section == "All Industries") %>% dplyr::select(date, negativity = value)
corisk$date <- as.Date(corisk$date, format = "%Y-%m-%d")

sup <- merge(sup, corisk, by = "date")

sup2 <- sup %>% gather(key, value, -date)

sup2$key <- ifelse(sup2$key == "sup", "S&P Global 1200 index", "Share of negative words in 10K-report 'corona' sentences")

sup2 %>% #filter(key == "Share of negative words in 10K-report 'corona' sentences") %>%
  ggplot() + 
  # Jan, 30 - WHO declares COVID-19 a health emergency
  geom_rect(data = data.frame(xmin = as.Date(c("2020-01-30")),
                              xmax = as.Date(c("2020-01-31")),
                              ymin = -Inf,
                              ymax = Inf, key = "Share of negative words in 10K-report 'corona' sentences"),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_text(data = data.frame(x=as.Date("2020-01-30"),key = "Share of negative words in 10K-report 'corona' sentences",
           y=0.015,label="Jan, 30: WHO\ndeclares public\nhealth emergency"), aes(x = x, y = y, label = label),fontface="bold", size = 3.5, hjust = 1) +
  # Jan, 30 - WHO declares COVID-19 pandemic
  geom_rect(data = data.frame(xmin = as.Date(c("2020-02-19")),
                              xmax = as.Date(c("2020-02-20")),
                              ymin = -Inf,
                              ymax = Inf, key = "S&P Global 1200 index"),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_text(data = data.frame(x=as.Date("2020-02-19"), key = "S&P Global 1200 index",
                              y=2350,label="Feb, 20:\nStart of stock\nmarket crash"), aes(x = x, y = y, label = label),
            fontface="bold", size = 3.5, hjust = 1) +
  # Jan, 30 - WHO declares COVID-19 pandemic
  geom_rect(data = data.frame(xmin = as.Date(c("2020-03-08")),
                              xmax = as.Date(c("2020-03-09")),
                              ymin = -Inf,
                              ymax = Inf, key = "S&P Global 1200 index"),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_text(data = data.frame(x=as.Date("2020-03-10"), key = "S&P Global 1200 index",
           y=2350,label="Mar, 09:\n'Black Monday'\n"), aes(x = x, y = y, label = label),fontface="bold", size = 3.5, hjust = 0) +
  # stay at home released
  geom_rect(data = data.frame(xmin = as.Date(c("2020-04-26")),
                              xmax = as.Date(c("2020-05-04")),
                              ymin = -Inf,
                              ymax = Inf, key = "Share of negative words in 10K-report 'corona' sentences"),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_text(data = data.frame(x=as.Date("2020-04-26"),key = "Share of negative words in 10K-report 'corona' sentences",
                              y=0.015,label="Apr, 26 - May, 04:\n15 States abandon\n stay-at-home orders"),aes(x = x, y = y, label = label),
            fontface="bold", size = 3.5,hjust = 1) +
  geom_line(aes(x = date, y = value, col = key, fill = key), lwd = 0.75) + #geom_point(shape = 21, stroke = 0.2, size = 2.5, alpha = 0.8, col = "black") +
  facet_wrap(~key, scales = "free", ncol = 1) + 
  scale_colour_manual(values = c(brewer.pal(11,"RdBu")[3],brewer.pal(11,"RdBu")[9])) +
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[3],brewer.pal(11,"RdBu")[9])) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size = 16), panel.grid.major = element_blank(),
        strip.background = element_blank(), strip.text.x = element_blank(),
        panel.spacing = unit(4, "lines"),
        panel.grid.minor = element_blank())

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Fig 1 B share of reports over time
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Load CoRisk data
corisk <- fread(paste(getwd(), "/CoRisk_data.csv", sep = ""))
corisk$date <- as.Date(corisk$date, format = "%Y-%m-%d") # date format

# Filter only relevant variables and sections
report_stats <- corisk %>% filter(key %in% c("no_reports","report_corona_binary_count"), Section == "All Industries") %>% dplyr::select(-Section,-V1)

# Long to wide
report_stats <- spread(report_stats, key, value)

# Calculate share of 'corona' reports over time
time_data <- report_stats %>% mutate(CoronaRel=report_corona_binary_count/no_reports)

# Two week moving average
time_data <- time_data %>% mutate(m.av = rollmean(CoronaRel, 14,fill = list(NA, NULL, NA), align = "right"))

# Plot from End of January
time_data %>% filter(date > "2020-01-29") %>%
  ggplot(aes(x = date, y = m.av)) + 
  geom_line(colour = c(brewer.pal(11,"RdBu")[9]), lwd = 0.75) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  labs(x = "", y = "Share of 10-K reports mentioning 'corona' (%)") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size = 16), panel.grid.major.y = element_blank(),
        strip.background = element_blank(), strip.text.x = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.grid.minor.y = element_blank())


#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Fig 1 D share of firms reporting per industry
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Load CoRisk data
corisk <- fread(paste(getwd(), "/CoRisk_data.csv", sep = ""))
corisk$date <- as.Date(corisk$date, format = "%Y-%m-%d") # date format

# Filter only relevant variables and sections
corisk_bar_mean <- corisk%>% filter(key %in% c("report_corona_binary_count","no_reports")) %>% group_by(Section) %>% 
  summarise(value_mean = sum(value[key == "report_corona_binary_count"]) / sum(value[key == "no_reports"]))

# Abbreviate and adjust sector titles
corisk_bar_mean$Section <- ifelse(corisk_bar_mean$Section == "Professional & Business Services", "Professional &\nBusiness Services", 
                                  corisk_bar_mean$Section)
corisk_bar_mean$Section <- ifelse(corisk_bar_mean$Section == "Transportation & Utilities", "Transportation &\nUtilities", 
                                  corisk_bar_mean$Section)
corisk_bar_mean$Section <- ifelse(corisk_bar_mean$Section == "Leisure & Hospitality", "Leisure &\nHospitality", 
                                  corisk_bar_mean$Section)
corisk_bar_mean$Section <- ifelse(corisk_bar_mean$Section == "Wholesale & Retail", "Wholesale &\nRetail", 
                                  corisk_bar_mean$Section)

# Generate dotplot 
corisk_bar_mean %>% 
  ggplot(aes(x=reorder(Section, value_mean), y=value_mean, fill=reorder(Section, value_mean), col = reorder(Section, value_mean)))+
  geom_point(shape = 21, size = 4.5, stroke = 0.2, colour = "black") +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8),labels = function(value) paste0(value*100, "%")) +
  expand_limits(y = 0) +
  scale_fill_brewer(palette = "RdBu", direction = -1) + 
  scale_colour_brewer(palette = "RdBu", direction = -1) + 
  labs(x = "", y = "Firms reporting\nabout COVID-19", colour = "", fill = "") +
  theme_bw() + theme(text = element_text(size = 18), legend.position = "none", panel.grid.minor = element_blank(),
                     axis.text.x = element_text(angle = 90, hjust = 1, size = 14))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Fig 2 A CoRisk-Index
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Load CoRisk data
corisk <- fread(paste(getwd(), "/CoRisk_data.csv", sep = ""))
corisk$date <- as.Date(corisk$date, format = "%Y-%m-%d") # date format

# Filter only relevant variables and sections
corisk_stats <- corisk %>% filter(key %in% c("CoRisk_Index_ma"))

corisk_stats$Section <- factor(corisk_stats$Section, levels = c("Wholesale & Retail", "Manufacturing",
                                                                "Professional & Business Services",
                                                                "All Industries", 
                                                                "Information", "Leisure & Hospitality",
                                                                "Finance", "Transportation & Utilities","Mining"))


corisk_stats %>% filter(key == "CoRisk_Index_ma", date > "2020-01-20") %>%
  ggplot(aes(x = date, y = value*100, # Multiply by 100 to get rounded index values
             colour = Section, group = Section)) + 
  geom_line(lwd = 1, show.legend = T) +
  scale_colour_manual(values = c(brewer.pal(9,"RdBu")[1],brewer.pal(9,"RdBu")[2], # Define colours manually
                                 brewer.pal(9,"RdBu")[3],brewer.pal(9,"RdBu")[4],
                                 brewer.pal(9,"Greys")[4],brewer.pal(9,"RdBu")[6],
                                 brewer.pal(9,"RdBu")[7],brewer.pal(9,"RdBu")[8],
                                 brewer.pal(9,"RdBu")[9])) + 
  scale_size_discrete(range = c(1, 0.25)) + 
  scale_y_continuous(breaks = c(0,25,50,75,100,125)) +
  labs(x = "", y = "CoRisk-Index", colour = "", fill = "", lty = "")+
  theme_bw() + theme(text = element_text(size = 18), legend.position = c(0.25,0.79), legend.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x = element_blank()) 

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Fig 2 B Topic-specific keywords line chart
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

# Keep only relevant variables
corisk1 <- corisk %>% dplyr::select(-V1) %>% mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(Section %in% c("Manufacturing", "Wholesale & Retail"), key %in% c("sentence_travel_share", 
                                                                           "sentence_supply_share",
                                                                           "sentence_demand_share"))
# Long to wide format
corisk1 <- spread(corisk1, key, value)

# Calculate two-week moving averages of topic-specific keyword mentions for sector MANUFACTURING
corisk1a <- corisk1 %>% filter(Section == "Manufacturing") %>% 
  mutate(sentence_travel_share = rollmean(sentence_travel_share, 14,fill = list(NA, NULL, NA), align = "right"),
         sentence_supply_share = rollmean(sentence_supply_share, 14,fill = list(NA, NULL, NA), align = "right"),
         sentence_demand_share = rollmean(sentence_demand_share, 14,fill = list(NA, NULL, NA), align = "right"))

# Calculate two-week moving averages of topic-specific keyword mentions for sector WHOLESALE & RETAIL
corisk1b <- corisk1 %>% filter(Section == "Wholesale & Retail") %>% 
  mutate(sentence_travel_share = rollmean(sentence_travel_share, 14,fill = list(NA, NULL, NA), align = "right"),
         sentence_supply_share = rollmean(sentence_supply_share, 14,fill = list(NA, NULL, NA), align = "right"),
         sentence_demand_share = rollmean(sentence_demand_share, 14,fill = list(NA, NULL, NA), align = "right"))

# Combine both the Manufacturing and Wholesale & Retail datasets
corisk1 <- rbind(corisk1a, corisk1b)

# Bring to long format
corisk1 <- corisk1 %>% gather(key, value, -date,-Section)

# Proper names for subplots
corisk1$key <- ifelse(corisk1$key == "sentence_supply_share", "Supply",
                             ifelse(corisk1$key == "sentence_demand_share", "Demand",
                                           ifelse(corisk1$key == "sentence_travel_share", "Travel","NONE")))

# Order labels
corisk1$key <- factor(corisk1$key, levels = c("Demand","Supply", "Travel"))

# Find last day in the dataset
end <- max(corisk1$date)

# Create individual data frames for plotting
corisk2 <- corisk1 %>% filter(key == "Travel")
corisk3 <- corisk1 %>% filter(key == "Supply")
corisk5 <- corisk1 %>% filter(key == "Demand")

#----
# Individual plots for each topic
#---

# Travel
plot1 <- ggplot() +
  # Jan 31 - travel restrictions announced
  geom_rect(data = data.frame(xmin = as.Date(c("2020-01-31")),
                              xmax = as.Date(c("2020-02-01")),
                              ymin = -Inf,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  annotate(geom="text",x=as.Date("2020-02-01"),
           y=0.001,label="Jan, 31:\nTrump announces\n travel restrictions",fontface="bold", size = 4.5,hjust = 0) +
  geom_line(data = corisk2, aes(x = date, y = value, col = Section), lwd = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c(brewer.pal(11,"RdBu")[3], brewer.pal(11,"RdBu")[9])) +
  theme_bw() + 
  scale_x_date(limits = c(as.Date("2020-01-25"),end)) +
  labs(col = "",x = "", y = "", title = "Travel") +
  theme(legend.position = "bottom", legend.text = element_text(size = 16), text = element_text(size = 14), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Supply
plot2 <- ggplot() +
  # Jan 31 - Supply restrictions anticipated
  geom_rect(data = data.frame(xmin = as.Date(c("2020-01-31")),
                              xmax = as.Date(c("2020-02-01")),
                              ymin = -Inf,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  annotate(geom="text",x=as.Date("2020-02-01"),
           y=0.008,label="Jan, 31:\nAnalysts fear\nsupply delays",fontface="bold", size = 4.5,hjust = 0) +
  geom_line(data = corisk3, aes(x = date, y = value, col = Section), lwd = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c(brewer.pal(11,"RdBu")[3], brewer.pal(11,"RdBu")[9])) +
  theme_bw() + 
  scale_x_date(limits = c(as.Date("2020-01-25"),end)) +
  labs(col = "",x = "", y = "", title = "Supply") +
  theme(legend.position = "bottom", legend.text = element_text(size = 16), text = element_text(size = 14), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Demand
plot4 <- ggplot() +
  # Stay-at-home orders in 45 states
  geom_rect(data = data.frame(xmin = as.Date(c("2020-03-15")),
                              xmax = as.Date(c("2020-03-19")),
                              ymin = -Inf,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  annotate(geom="text",x=as.Date("2020-03-15"),
           y=0.0075,label="Mar, 15 - 19:\n First states announce\n stay-at-home orders",fontface="bold", size = 4.5,hjust = 1) +
  geom_line(data = corisk5, aes(x = date, y = value, col = Section), lwd = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c(brewer.pal(9,"RdBu")[1], brewer.pal(9,"RdBu")[9])) +
  theme_bw() + 
  scale_x_date(limits = c(as.Date("2020-01-25"),end)) +
  labs(col = "",x = "", y = "", title = "Demand") +
  theme(legend.position = "bottom", legend.text = element_text(size = 16), text = element_text(size = 14), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Arrange all plots with ggarrange
figure <- ggarrange(plot1, plot2, plot4, nrow = 3, common.legend = T, legend = "bottom")

# Annotate the figure by adding a common label and plot
annotate_figure(figure, left = text_grob("Share of topic-specific keywords in corona-sentences", rot = 90, size = 18))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Fig 3 CoRisk and Unemployment
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%


#%#%#%#%#%
# Load Data
#%#%#%#%#%

# Load unemployment initial claims from Andrew Van Dam's github data set
ui <- read.csv("https://raw.githubusercontent.com/Economic/ui_state_detailed/master/output/state_ui_industry_recoded.csv")

#%#%#%#%#%
# Data Wrangling
#%#%#%#%#%

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

#%#%#%#%#%
# Data Wrangling
#%#%#%#%#%

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

corisk <- corisk %>% mutate(ic_sum_detrend = detrend(log10(ic_sum), tt = 'linear', bp = c()),
                            CoRisk_Index_detrend = detrend(log10(CoRisk_Index), tt = 'linear'))


#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Update the plot and go to stock markets

corisk_all <- corisk %>% group_by(Section) %>% mutate(ic_sum_Diff = ic_sum / lag(ic_sum),
                                                      CoRisk_Diff = CoRisk_Index / lag(CoRisk_Index),
                                                      CoRisk_Diff_lag = lag(CoRisk_Diff))


# Groupwise regression of log initial claims ~ log CoRisk-Index
dfHour = corisk_all %>% 
  group_by(Section) %>%
  do(fitHour = lm(ic_share ~ CoRisk_Diff_lag, data = .))

# Get R2
dfHourCoef = glance(dfHour, fitHour)
dfHourCoef <- dfHourCoef[,c(1:2)] # Just extract the R2 column
dfHourCoef <- dfHourCoef %>% mutate(r.squared = round(r.squared,2))

# Get estimates
dfHourCoef2 = tidy(dfHour, fitHour)  
dfHourCoef2 <- dfHourCoef2 %>% filter(term == "CoRisk_Diff_lag") %>% dplyr::select(Section, estimate) %>%
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


corisk_all %>%
  ggplot(aes(x = CoRisk_Diff_lag, y = ic_share*100, col = Section_label, fill = Section_label)) + facet_wrap(~Section_label) + 
  geom_point(shape = 21, size = 2.5, col = "black", stroke = 0.3) +        # Points
  geom_smooth(se = F, span = 2, lwd = 1, show.legend = F, method = "lm") + # Trend
  # Display R2, b, and week
  geom_text(aes(x = 1.05, y = 9, label = r.squared_label), color = 'black') +
  geom_text(aes(x = 1.05, y = 7, label = estimate_label), color = 'black') +
  scale_fill_brewer(palette = "RdBu") + scale_colour_brewer(palette = "RdBu") +
  # Theme and axes
  theme_bw() +
  labs(x = "CoRisk-Index t-1 / CoRisk-Index t-2", y = "Weekly unemployment initial claims\n(% of total employment)", col = "",
       title = "CoRisk-Index and unemployment initial claims") +
  theme(legend.position = "none", text = element_text(size = 14), legend.text = element_text(size = 9),
        panel.grid.minor = element_blank(), strip.background =element_rect(fill=brewer.pal(9,"Greys")[2]))

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

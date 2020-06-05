################################################################
# Create Online Observatory Dashboard
################################################################

#%#%#%#%#%#%#%#%#%#%#%#%
# Load Packages
#%#%#%#%#%#%#%#%#%#%#%#%
library(shiny)
library(tidyverse)
library(plotly)
library(readr)
library(data.table)
library(scales)
library(RColorBrewer)
library(zoo)
library(lubridate)
library(htmlwidgets)
library(leaflet)
library(raster)
library(rsconnect)
'%!in%' <- function(x,y)!('%in%'(x,y))



#%#%#%#%#%#%#%#%#%#%#%#%
# Generate Dashboard user interface
#%#%#%#%#%#%#%#%#%#%#%#%

# Define UI for random distribution app ----
ui <- fluidPage(
  # App title ----
  mainPanel(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "right",
    # Sidebar panel for inputs ----
    sidebarPanel(dateRangeInput("dates", "Customise Date Range:", start = "2020-01-15", end = Sys.Date()),
                 selectizeInput('industry', 'Select Industry:', 
                                choices = c("Finance", "Manufacturing", "Professional & Business Services", "Transportation & Utilities", 
                                            "Mining", "Wholesale & Retail", "Information", "Leisure & Hospitality","All Industries"),
                                selected = c("Manufacturing","Wholesale & Retail","All Industries","Finance"), multiple = TRUE),
                  downloadButton("download_corisk", "Download CoRisk Data")),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("CoRisk Index", plotlyOutput("index")),
                  tabPanel("Text Sentiment", plotlyOutput("sentiment")),
                  tabPanel("Industry View", plotlyOutput("bar")),
                  tabPanel("Topic Heatmap", plotlyOutput("heatmap"))
                  )
    )
  )
)

#%#%#%#%#%#%#%#%#%#%#%#%
# Load Dashboard data
#%#%#%#%#%#%#%#%#%#%#%#%

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # CoRisk Index
  corisk_select <- reactive({
    
    corisk_index <- read.csv("CoRisk_data.csv")
    corisk_index$date <- as.Date(as.character(corisk_index$date), format = "%Y-%m-%d")
    corisk_index %>% filter(as.character(date) > input$dates[1] & as.character(date) < input$dates[2]) %>%
      filter(Section %in% input$industry)
    
  })
  
  
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%  
# First plot - CoRisk index
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
  
  # Generate a plot of the data
  output$index <- renderPlotly({
    
    # Calculate total number of reports displayed
    total_reports <- sum(corisk_select()$value[corisk_select()$key=="no_reports" & corisk_select()$Section == "All Industries"])
    
    corisk_plot <- corisk_select() %>% filter(key == "CoRisk_Index_ma", value > 0) %>% filter(Section %in% input$industry) %>%
      ggplot(aes(x = date, y = value*100, colour = Section, group = Section, 
                 text=sprintf("CoRisk Index: %s<br>Industry: %s <br>Week: %s", round(value*100,2), Section, date))) + 
      #geom_smooth(se=FALSE, lwd = 0.5, show.legend = T, text = "") + 
      #geom_point(aes(fill = factor(Section), text=sprintf("CoRisk Index: %s<br>Industry: %s <br>Week: %s", round(value,2), Section, date)), 
      #           shape = 21, size = 1.5, stroke = 0.2, alpha = 0.7, colour = "black") +
      geom_line(lty = 1, lwd = 1, show.legend = F) +
      scale_fill_brewer(palette = "RdBu", direction = -1) + 
      scale_colour_brewer(palette = "RdBu", direction = -1) + 
      scale_size_discrete(range = c(1, 0.25)) + 
      labs(x = "", y = "CoRisk-Index", colour = "", fill = "")+
      theme_bw() + theme(text = element_text(size = 18), legend.position = "bottom", panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank())
    
    ggplotly(corisk_plot, tooltip="text") %>% layout(showlegend = FALSE, 
                                                     annotations = list(x = 1, y = 0, text = paste("Reports displayed: ",total_reports,sep=""), 
                                                            showarrow = F, xref='paper', yref='paper', 
                                                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                                            font=list(size=15)))
      
        })
  
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Second plot - barchart
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

    # Generate a plot of the data ----
  output$bar <- renderPlotly({
    
  corisk_bar_mean <- corisk_select() %>% filter(key %in% c("report_corona_binary_count","no_reports")) %>% group_by(Section) %>% 
    summarise(value_mean = sum(value[key == "report_corona_binary_count"]) / sum(value[key == "no_reports"]))
    
  corisk_bar <- corisk_bar_mean %>% 
      ggplot(aes(x=reorder(Section, value_mean), y=value_mean, fill=Section, text=sprintf("Industry: %s<br>Share of reporting firms: %s%%", Section, round(value_mean, 2)*100)))+
      geom_bar(stat="identity", show.legend = F, text = "") + 
      scale_y_continuous(labels = function(value) paste0(value*100, "%")) +
      scale_fill_brewer(palette = "RdBu", direction = -1) + 
      scale_colour_brewer(palette = "RdBu", direction = -1) + 
      labs(x = "", y = "Firms reporting about COVID-19", colour = "", fill = "") +
      theme_bw() + theme(text = element_text(size = 18), legend.position = "bottom", panel.grid.minor = element_blank(),
                         panel.grid.major.y = element_blank()) + coord_flip()
  
  ggplotly(corisk_bar, tooltip = "text") %>% layout(showlegend = FALSE)
    
  })
  
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Third plot - heatmap
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
  
  # Generate a Heatmap-
  output$heatmap <- renderPlotly({
    
    hm_df <- corisk_select() %>% group_by(Section, key) %>% summarise(value = mean(value)) %>% 
      filter(key %in% c("sentence_production_share","sentence_supply_share","sentence_demand_share",
                        "sentence_finance_share","sentence_travel_share"))
  
    hm_df$key <- ifelse(hm_df$key == "sentence_production_share", "Production",
                        ifelse(hm_df$key=="sentence_supply_share", "Supply",
                               ifelse(hm_df$key=="sentence_demand_share", "Demand",
                                      ifelse(hm_df$key=="sentence_finance_share", "Finance","Travel"))))
    
    hm_df$key <- factor(hm_df$key, levels = c("Travel", "Supply", "Production", "Demand", "Finance"))
    
    # Heatmap 
    corisk_heatmap <- ggplot(hm_df, aes(x = key, y = Section, fill= value)) + 
      geom_raster(interpolate = F, aes(text=sprintf("Industry: %s<br>Topic relevance: %s per 1.000", Section, round(value*1000,2)))) +
      scale_fill_gradient(low = brewer.pal(9,"Reds")[1],high = brewer.pal(9,"Reds")[9]) +
      labs(x = "", y = "", fill = "Topic relevance") +
      theme_bw() + theme(panel.grid = element_blank(), text = element_text(size = 18),
                         legend.position = "none")
    
    ggplotly(corisk_heatmap, tooltip = "text", showscale = F) %>% layout(showlegend = FALSE,  
                                                                         #xaxis = list(title = list(font = list(size = 1))),
                                                                         xaxis = list(tickfont = list(size = 12)),
                                                                         yaxis = list(tickfont = list(size = 12)))
    
  })
  
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Fourth plot - Sentiment
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
  
  #
  output$sentiment <- renderPlotly({  
  
  corisk_sentiment <- corisk_select() %>% 
    #filter(Section == "All Industries") %>% 
    filter(key == "sentence_negative_share_ma")# & Section == "All Industries") 
    
  corisk_sentiment$key <- ifelse(corisk_sentiment$key=="sentence_negative_share_ma", "'Corona Sentences'", "None")
  
  corisk_sentiment$value <- round(corisk_sentiment$value*100, 2)
  
  corisk_sentiment_plot <- corisk_sentiment %>%
    ggplot(aes(x = date, y = value, colour = Section, group = Section, 
               text=sprintf("Date: %s<br>Source: %s<br>Share of 'Negative Words': %s%%", date, Section, value))) + 
    #geom_smooth(se=FALSE, lwd = 1, show.legend = F) + 
    #facet_wrap(~key, scales = "free",ncol=1) +
    geom_line(lty = 1, lwd = 1, show.legend = F) +
    #geom_point(aes(fill = factor(Section)), shape = 21, size = 2, stroke = 0.2, alpha = 0.7, colour = "black") +
    scale_fill_brewer(palette = "RdBu", direction = -1) + 
    scale_colour_brewer(palette = "RdBu", direction = -1) + 
    scale_size_discrete(range = c(1, 0.25)) + 
    labs(x = "", y = "Share of negative words (%)", colour = "Sentiment", fill = "Sentiment")+
    theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom", panel.grid.minor = element_blank(),
                       panel.grid.major.x = element_blank(), axis.text.y = element_text(size = 10))

  ggplotly(corisk_sentiment_plot, tooltip = "text", showscale = F) %>% layout(showlegend = FALSE)
     
  })
  
#%#%#%#%#%#%#%#%#%#%#%#%
# Download function
#%#%#%#%#%#%#%#%#%#%#%#%

  # Downloadable csv of selected dataset
  output$download_corisk <- downloadHandler(
    
    filename = function() {
      paste("corisk_",input$dates[1],"_to_",input$dates[2],".csv", sep = "")
    },
    content = function(file) {
      write.csv(corisk_select(), file, row.names = FALSE, quote = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)

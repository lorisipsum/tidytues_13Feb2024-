library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(shinyjs)
library(png)
library(plotly)
library(viridis)
library(smplot2)
library(ggpubr)


groundhogs <- read_csv('www/groundhogs.csv')
groundhogs <- groundhogs[,-1]
predictions <- read_csv('www/predictions.csv')
predictions <- predictions[,-1]

merged_df <- merge(groundhogs, predictions)

#Climate Data downloaded from IMF [https://climatedata.imf.org/pages/climatechange-data]

climate_df <- read_csv("www/Annual_Surface_Temperature_Change.csv")
  climate_df %<>% filter(ISO2 == "US")  %>% pivot_longer(!c(ObjectId, Country, ISO2, ISO3, Indicator, Unit, 
                                                           Source, CTS_Code, CTS_Name, CTS_Full_Descriptor),
                                                         names_to = "Year", values_to = "Value")
  climate_df$Year <- gsub("F", "", climate_df$Year)
  climate_df$Year <- as.numeric(climate_df$Year)
  

number_choices <- c(1,2,3,4,5,6,7,8,9,10)

title = tags$div(tags$strong("Groundhogs!"))

header <- dashboardHeader(title = title)

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))

body <- dashboardBody(
  shinyjs::useShinyjs(),
  uiOutput("bodypanel"),
  tags$head(tags$style(HTML('
  /* logo */
     .skin-green .main-header .logo {
     background-color: #6d90b3;
     }
   
    /* navbar (rest of the header) */
      .skin-green .main-header .navbar {
      background-color: #6d90b3;
      }
     
     /* main sidebar */
      .skin-green .main-sidebar {
      background-color: #6d90b3;
     }
 
    /* body */
    .content-wrapper, .right-side {
      background-color: #8fa2b5;
    }
  ')))
)



ui<-dashboardPage(header, sidebar, body, skin = "green")

server <- function(input, output, session) {
  
  output$bodypanel <- renderUI({tabItems(
    tabItem(
      tabName ="graphs", class = "active",
      br(), br(),
      plotlyOutput('graph_1', height = "800px"),
      br(), br(),
      plotOutput('graph_2', height = "1900px"),
      br(), br(), br(),
      align = "center",
      "Another silly Shiny by: ",
      span(strong("lorisipsum"), style = "color:blue"),
      br(),
      tags$img(src = 'photo.png', height = 75, width = 100),
      br()
    ),
    tabItem(
      tabName ="climate_graphs", class = "active",
      column(6,
             selectInput("chosen_number", tags$label("Graph of choice", style = "color: #fcfcfc;"), choices = number_choices)
      ),
      br(), br(),
      plotOutput('graph_3', height = "600px"),
      br(), br(), br(),
      plotOutput('graph_4', height = "600px"),
      br(), br(), br(),
      align = "center",
      "Another silly Shiny by: ",
      span(strong("lorisipsum"), style = "color:blue"),
      br(),
      tags$img(src = 'photo.png', height = 75, width = 100),
      br()
    ),
    tabItem(
      tabName ="data", class = "active",
      br(), br(),
      downloadButton('download',"Download the Groundhog data"),
      br(), br(),
      downloadButton('download_2',"Download the Climate data"),
      br(), br(), br(),
      align = "center",
      "Another silly Shiny by: ",
      span(strong("lorisipsum"), style = "color:blue"),
      br(),
      tags$img(src = 'photo.png', height = 75, width = 100),
      br()
      )
    )
  })

    output$sidebarpanel <- renderUI({
      sidebarMenu(
        menuItem(tags$label("Groundhog Graphs", style = "Color: #fcfcfc;"), tabName = "graphs", icon=icon("chart-simple")),
        menuItem(tags$label("Groundhog-Climate Graphs", style = "Color: #fcfcfc;"), tabName = "climate_graphs", icon=icon("temperature-half")),
        menuItem(tags$label("Data Download", style = "Color: #fcfcfc;"), tabName = "data", icon=icon("download"))
      )
    })
    
    
  output$graph_1 <- renderPlotly({
    merged_df %>% group_by(region) %>% count() %>% ungroup() %>%
      plot_ly(x = ~n, y = ~region, type = 'scatter', mode = 'markers') %>%
      layout(title = 'Number of Groundhog Predictions per State',
             xaxis = list(title = 'State'),
             yaxis = list(title = 'Number of Groundhog Predictions'),
             showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
  })

  output$graph_2 <- renderPlot({
  merged_df %>% filter(!is.na(shadow)) %>% rename(Shadow = shadow) %>%
    mutate(year = as.factor(year)) %>%
    ggplot() +
    geom_bar(aes(x = fct_rev(year), fill = Shadow)) +
    scale_fill_viridis_d() +
    coord_flip()+
    theme_bw() +
    theme(text = element_text(size=20), title = element_text(size=14)) +
    xlab("") +
    ylab("")
  })

  output$graph_3 <- renderPlot({
  merged_df %>% filter(!is.na(shadow)) %>% group_by(year) %>%  filter(n() >= as.numeric(input$chosen_number)) %>%
    select(id, year, shadow) %>%
    mutate(Perc_shadow = mean(shadow)*100) %>% slice(1) %>% ungroup() %>%
    ggplot(aes(year, Perc_shadow)) +
      geom_bar(data = climate_df, aes(x = Year, y = Value * 50), stat = "identity", fill = "red", alpha = 0.4) +
      sm_statCorr(data = climate_df, mapping = aes(x = Year, y = Value * 50), corr_method = 'pearson',
                  fit.params = list(color = 'red',  linetype = 'longdash'), inherit.aes = FALSE,
                  show_text = FALSE) +
      stat_cor(data = climate_df, mapping = aes(x = Year, y = Value * 50), inherit.aes = FALSE,
               label.x = 2000, label.y = -15, color = "red") +
      labs(title = "Groundhog Predictions and Mean Surface Temperature Change",
           subtitle = "Groundhog Prediction Trends in Purple, Mean Annual Surface Temperature Trends in Red",
           caption = "Climate data source: IMF") +
      geom_point(size = 2, color = "#440154FF") + theme_bw() +
      theme(text = element_text(size=20), title = element_text(size=14)) +
      xlab("Year") +
      ylab("% of Shadow Seen") +
      sm_statCorr(corr_method = 'pearson',
                fit.params = list(color = '#440154FF',  linetype = 'longdash')) + 
      scale_y_continuous(sec.axis = sec_axis(~./50, name = "Mean Surface Temperature Change (Compared with 1951-1980 mean)"))
  })
  
  output$graph_4 <- renderPlot({
  pred <- merged_df %>% filter(!is.na(shadow)) %>% group_by(year) %>%  filter(n() >= input$chosen_number) %>%
      select(id, year, shadow) %>%
      mutate(Perc_shadow = mean(shadow)*100) %>% slice(1) %>% ungroup()  
  
  climate_df <- climate_df %>% rename(year = Year)
  
  pred_climate <- merge(pred, climate_df, by = "year", all = FALSE) 
    
  ggscatter(pred_climate, x = "Perc_shadow", y = "Value", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "% of Shadow Seen ", ylab = "Mean Surface Temperature Change (Compared with 1951-1980 mean)") +
    labs(title = "Groundhog Predictions vs Mean Surface Temperature Change",
         caption = "Climate sata source: IMF") +
    geom_point(size = 2, color = "#440154FF") + theme_bw() +
    theme(text = element_text(size=20), title = element_text(size=14,  face = "bold"), 
          axis.title = element_text(size=14, face = "plain"))
  })
  

  thedata <- reactive(merged_df)
  output$download <- downloadHandler(
    filename = function(){"Groundhog_data.csv"},
    content = function(fname){
      write.csv(thedata(), fname)
    }
    )
  
  thedata_1 <- reactive(climate_df)
  output$download_2 <- downloadHandler(
    filename = function(){"Climate_data.csv"},
    content = function(fname){
      write.csv(thedata_1(), fname)
    }
  )
    
  
} 

shinyApp(ui = ui, server = server)



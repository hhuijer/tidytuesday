---
title: "Untitled"
output: html_document
date: "2026-06-10"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(DT)

tuesdata <- tidytuesdayR::tt_load('2026-06-09')

game_films <- tuesdata$game_films

colnames(game_films)
```

```{r}
#basic data summaries
total_films <- nrow(game_films)
avg_rotten_tomatoes <- mean(game_films$rotten_tomatoes, na.rm = TRUE)
avg_metacritic <- mean(game_films$metacritic, na.rm = TRUE)
avg_cinema_score <- mean(game_films$cinema_score, na.rm = TRUE)
total_box_office <- sum(game_films$worldwide_box_office, na.rm = TRUE)

# Get unique values for filters
unique_categories <- unique(game_films$category)
unique_directors <- unique(game_films$director)
unique_publishers <- unique(game_films$original_game_publisher)
```

```{r}
#create variable for game series for major game series
game_films <- game_films %>%
  mutate(series = case_when(
    str_detect(tolower(title), "pokemon") ~ "Pokémon",
    str_detect(tolower(title), "resident evil") ~ "Resident Evil",
    str_detect(tolower(title), "sonic") ~ "Sonic",
    str_detect(tolower(title), "super mario") ~ "Super Mario",
    str_detect(tolower(title), "uncharted") ~ "Uncharted",
    str_detect(tolower(title), "tomb raider") ~ "Tomb Raider",
    str_detect(tolower(title), "street fighter") ~ "Street Fighter",
    str_detect(tolower(title), "mortal kombat") ~ "Mortal Kombat",
    str_detect(tolower(title), "halo") ~ "Halo",
    str_detect(tolower(title), "castlevania") ~ "Castlevania",
    str_detect(tolower(title), "five nights at freddy's") ~ "Five Nights At Freddy's",
    str_detect(tolower(title), "tekken") ~ "Tekken",
    str_detect(tolower(title), "detective pikachu") ~ "Pokémon",
    TRUE ~ "Other"
  ))
```



```{r}
#create shiny app dashboard page
ui <- dashboardPage(
  dashboardHeader(title = "Video Game Films Dashboard"),
  
  #create dashboard sidebar
  dashboardSidebar(
    # create filters for series, director, publisher, and year range
    selectInput("series", "Game Series:", 
                choices = c("All", unique(game_films$series))),
    selectInput("director", "Director:", 
                choices = c("All", unique(game_films$director))),
    selectInput("publisher", "Game Publisher:", 
                choices = c("All", unique(game_films$original_game_publisher))),
    sliderInput("year_range", "Release Years:",
                min = min(year(game_films$release_date), na.rm=TRUE),
                max = max(year(game_films$release_date), na.rm=TRUE),
                value = c(min(year(game_films$release_date), na.rm=TRUE), 
                         max(year(game_films$release_date), na.rm=TRUE)),
                step = 1,
                sep = "") 
    ),
  
  #create dashboard body
  dashboardBody(
    fluidRow(
      valueBox(nrow(game_films), "Total Films", icon = icon("film"), color = "blue"),
      valueBox(round(mean(game_films$rotten_tomatoes, na.rm=T), 1), "Avg Rotten Tomatoes", icon = icon("star"), color = "red"),
      valueBox(round(mean(game_films$metacritic, na.rm=T), 1), "Avg Metacritic", icon = icon("star"), color = "yellow")
    ),
    
    #create plotly plots    
    fluidRow(
      box(plotlyOutput("timeSeriesPlot"), width = 6),
      box(plotlyOutput("ratingsPlot"), width = 6)
    )
  )
)

server <- function(input, output, session) {
  
  # filter data
  filtered_films <- reactive({
    df <- game_films %>%
        mutate(year = year(release_date))
        
    if(input$series != "All") df <- df %>% filter(series == input$series)
  df <- df %>% filter(year >= input$year_range[1] & year <= input$year_range[2])
    if(input$director != "All") df <- df %>% filter(director == input$director)
    if(input$publisher != "All") df <- df %>% filter(original_game_publisher == input$publisher)
    return(df)
  })
  
  # create time series plot
  output$timeSeriesPlot <- renderPlotly({
    data <- filtered_films() %>%
      group_by(release_date) %>%
      summarise(box_office = sum(worldwide_box_office, na.rm=TRUE),
                count = n(),
                .groups = 'drop')
    
    plot_ly(data, x = ~release_date, y = ~box_office, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Box Office Over Time", 
             xaxis = list(title = "Release Date"),
             yaxis = list(title = "Worldwide Box Office ($)"))
  })
  
  # create ratings comparison
  output$ratingsPlot <- renderPlotly({
    data <- filtered_films() %>%
      summarise(
        Rotten_Tomatoes = mean(rotten_tomatoes, na.rm=TRUE),
        Metacritic = mean(metacritic, na.rm=TRUE),
        Cinema_Score = mean(cinema_score, na.rm=TRUE)
      ) %>%
      pivot_longer(everything(), names_to = "Source", values_to = "Score")
    
    plot_ly(data, x = ~Source, y = ~Score, type = 'bar') %>%
      layout(title = "Average Ratings by Source")
  })
}

shinyApp(ui, server)
  
```

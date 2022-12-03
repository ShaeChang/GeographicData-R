
# setup ---------------------------------------------------------------

library(shiny)
library(lubridate)
library(tmap)
library(sf)
library(tidyverse)

# Options:

filter <- dplyr::filter

# starting data -------------------------------------------------------

trout <-
  read_csv('data/va_trout.csv')

# variables -----------------------------------------------------------

trout_species <-
  trout %>% 
  pull(common_name) %>% 
  unique() %>% 
  sort()

# user interface ------------------------------------------------------

ui <-
  fluidPage(
    
    # Title panel:
    
    titlePanel("Virginia trout observations, iNaturalist"),
    
    tags$br(),
    
    
    # Set formatting of div:
    
    sidebarLayout(
      
      # Sidebar panel
      
      sidebarPanel(
        
        # Species input:
        
        selectInput(
          inputId = 'spp',
          label = 'Species',
          choices = trout_species),
      ),
      
      # Main panel
      
      mainPanel(
        plotOutput('my_plot')
      )
    )
  )
  

# server --------------------------------------------------------------

server <-
  function(input, output) {
    
    # Plot:
    
    output$my_plot <-
      renderPlot({
        trout %>% 
          filter(common_name == input$spp) %>% 
          group_by(
            year = year(date)) %>% 
          summarize(n = n()) %>% 
          ggplot(
            aes(x = year,
                y = n)) +
          geom_bar(stat = 'identity') +
          labs(title = 'Trout by year') +
          theme_bw()
      })
    
    # Summary table:
    
    
    # Map:
    
  }


# knit ----------------------------------------------------------------

shinyApp(ui, server)

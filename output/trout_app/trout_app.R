
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
        
        # add tabs to our panel
        
        tabsetPanel(
          
          # the plot tab
          
          tabPanel(
            'Plot',
            plotOutput('my_plot')
          ),
          
          # the summary table (the data is in server section)
          
          tabPanel(
            'Summary',
            dataTableOutput(
              'summary'
            )
          ),
          
          tabPanel(
            'Map',
            tmapOutput('my_map')
          )
        )
      )
    )
  )
  

# server --------------------------------------------------------------

server <-
  function(input, output) {
    
    # create two reactive objects
    
    trout_filtered <-
      reactive({
        trout %>% 
          filter(common_name == input$spp)
      })
    
    trout_summary <-
      reactive({
        trout_filtered() %>% 
          group_by(
            year = year(date)) %>% 
          summarize(n = n())
      })
    
    # Plot:
    
    output$my_plot <-
      renderPlot({
        trout_summary()
      })
    
    # Summary table:
    
    output$summary <-
      renderDataTable({
        trout %>% 
          filter(common_name == input$spp) %>% 
          group_by(
            year = year(date)) %>% 
          summarize(n = n()) 
      })
    
    # Map:
    
    output$my_map <-
      renderTmap({
        trout_filtered() %>% # it's a function
          st_as_sf(
            coords = c('longitude', 'latitude'),
            crs = 4326) %>% 
          tm_shape() +
          tm_dots()
      })
    
  }


# knit ----------------------------------------------------------------

shinyApp(ui, server)

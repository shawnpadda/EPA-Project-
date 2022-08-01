# Load all of the essential libraries.
library(shiny)
library(ggplot2)
library(bslib)
library(tidyverse)
library(shinydashboard)
library(ISLR)
library(htmltools)
library(car)
library(plotly)
library(shinythemes)
library(leaflet)


# Define UI for application that draws a histogram
ui <- dashboardPage( skin = "purple",
  
      dashboardHeader(title = "Environmental Protection Agency Dashboard"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Introduction", tabName = "H4",icon = icon("far fa-file-image")),
          menuItem("Objectives",tabName = "H1", icon = icon("fas fa-server")),
          menuItem("Data Set", tabName = "H2",icon = icon("bar-chart-o")),
          menuItem("Findings", tabName = "H3",icon = icon("list-alt")),
          menuItem("References", tabName = "H5",icon = icon("table"))
          
        )
      ),
      
      dashboardBody(
        tabItems(
          
          # 1st tab
          tabItem(tabName = "H1",
                  navbarPage("Research Objectives",theme = shinytheme("sandstone")),
                  tabPanel("Key Objectives", htmlOutput("object"))
                ),
          
          # 2nd tab
          tabItem(tabName = "H2",
                  navbarPage("Environmental Protection Agency Data Set"),
                  fluidRow(
                    box(
                      title = "Input Controls", width = 6, solidHeader = TRUE,
                      status = "primary",
                      background = "purple",
                      selectInput(inputId = "sample",
                                  label = "Select a dataset",
                                  choices = c("EPA"),
                                  selected = "EPA"), height = 450
                    ),
                    tabBox(tabPanel("Data Information",htmlOutput("txt")),
                           tabPanel("Plot1",plotlyOutput("plot")),
                           tabPanel("Plot2", leafletOutput("zone")),
                           tabPanel("Plot3", plotlyOutput("zone1")),
                           tabPanel("Plot4", plotOutput("zone2")),
                           tabPanel("Plot5", plotOutput("zone3")),
                           tabPanel("Summary", verbatimTextOutput("summary1")
                    )
                  )
                )
               ),
          tabItem(tabName = "H3",
                  navbarPage("Key Findings"),
                  fluidRow( htmlOutput("object2")
                  )
          ),
          tabItem(tabName = "H4",
                  navbarPage("About EPA"),
                  fluidRow( htmlOutput("object1")
                  )
          ),
          tabItem(tabName = "H5",
                  navbarPage("References",theme = shinytheme("flatly")),
                  fluidRow(htmlOutput("object3")
                  ),
                  fluidRow(
                    tags$b(tags$a("Link1",href = "https://www.epa.gov/aboutepa"))
                    ),
                  fluidRow(
                    tags$b(tags$a("Link2",href = "https://ericwfox.github.io/data/nrsa.rds"))
                    )
                  )
          
      )
    )
  )


    
  

# Define server logic required to draw a histogram
server <- function(input, output,session) {
         
           #streams <- readRDS(url("https://ericwfox.github.io/data/streams.rds"))
           #streams %>%
            # drop_na()
          nrsa <- readRDS(url("https://ericwfox.github.io/data/nrsa.rds"))
    
         
         output$summary1 <- renderPrint({
           nrsa %>% mutate(cond = as.factor(cond))%>%summary()
         })
         
         output$plot <- renderPlotly(
           #maps::map("state") %>% 
           ggplot(nrsa,aes(x = lon,y = lat, fill = cond)) + geom_point() + theme_bw()
         )
         
         output$zone1 <- renderPlotly(
           #maps::map("state") %>% 
           ggplot(nrsa,aes(x = factor(cond))) + geom_bar(aes(fill = cond)) + theme() + labs(x = "Conditions", y = "Count")
         )
         
         output$zone2 <- renderPlot(
           #maps::map("state") %>% 
           ggplot(nrsa,aes(x = lon)) + geom_boxplot() + theme() + labs(x = "Longitude")
         )
         
         output$zone3 <- renderPlot(
           #maps::map("state") %>% 
           ggplot(nrsa,aes(x = lat)) + geom_boxplot() + theme() +labs(x = "Latitude")
         )
         
         output$zone <- renderLeaflet(
           leaflet(data = nrsa) %>%
             addTiles() %>%
             addProviderTiles(providers$OpenStreetMap, group = "Open Street Maptile",
                              options = providerTileOptions(noWrap = TRUE))%>%
             addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Satellite Maptile",
                              options = providerTileOptions(noWrap = TRUE)) %>%
             addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo Maptile",
                              options = providerTileOptions(noWrap = TRUE)) %>%
              addLayersControl(
               baseGroups = c("Open Street Maptile", "Satellite Maptile", "Topo Maptile"),
               options = layersControlOptions(collapsed = TRUE))%>%
             addCircleMarkers(~lon, ~lat, popup = ~as.factor(cond), label = ~as.factor(cond), color = c("red",  "orange", "blue"),
                              opacity = 0.3,radius = 0.9)%>%
             setView(lng  = -95.712891 , lat =  37.09024, zoom = 3.8) %>%
             addLegend("bottomright", colors =c("red",  "orange", "blue"), labels = c("poor","fair","good"),
                       values = ~cond,
                       title = "Stream conditions",
                       opacity = 1
             )
             
         )
         
         output$txt <- renderText({
           HTML(paste0("The data set is obtained from the environmental protection agency (EPA). The Environmental 
           Protection Agency (EPA) sampled nearly 2000 stream sites across the conterminous US during the summer 
                       months of 2008/09. This was part of a larger environmental monitoring program called
           the National Rivers and Stream Assessment (NRSA).  The condition of the stream sites were evaluated as Good, Fair, or
            Poor according to an aquatic health index."))
         })
         
         output$object <- renderText({
           HTML(paste0("In this project, I addressed the following Interesting research questions:
                       <li> Visualize outliers in the data set using a box plot approach.
                       <li> Show the poor, fair, and good stream conditions using static and dynamic data visualizations.
                       <li> Present the descriptive summary statistics of the EPA data set."))
         })
         
         output$object1 <- renderText({
           HTML(paste0("The Environmental Protection Agency (EPA) is an independent executive agency of the United States federal 
                       government tasked with environmental protection matters. President Richard Nixon proposed the establishment 
                       of EPA on July 9, 1970; it began operation on December 2, 1970, after Nixon signed an executive order. The 
                       EPA has its headquarters in Washington, D.C., regional offices for each of the agency's ten regions and 27 
                       laboratories.The agency conducts environmental assessment, research, and education. It has the responsibility 
                       of maintaining and enforcing national standards under a variety of environmental laws, in consultation with state, 
                       tribal, and local governments. It delegates some permitting, monitoring, and enforcement responsibility to U.S. 
                       states and the federally recognized tribes. EPA enforcement powers include fines, sanctions, and other measures. 
                       The agency also works with industries and all levels of government in a wide variety of voluntary pollution prevention 
                       programs and energy conservation efforts."))
         })
         output$object2 <- renderText({
           HTML(paste0("The project's key findings are listed below:
           <li> No extreme outliers were detected in the data set.
           <li> Poor stream conditions showed the most contribution in the data.
           <li> Data showed more poor stream conditions on the east coast of the United states.
                       "))
         })
         
         
         output$object3 <- renderText({
           HTML(paste0("The references are provided below:
                       "))
         })
         
}


options = list(width = 900, height = 900)

# Run the application 
shinyApp(ui, server)

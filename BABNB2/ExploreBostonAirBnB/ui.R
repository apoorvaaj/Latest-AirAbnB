library(shiny)
library(ggmap)
library(ggplot2)
shinyServer(
  ui <- fluidPage(
    
    headerPanel("Boston Air BnB Listing Visualizations"),
    
    sidebarPanel
    (
      selectInput("Attributes", "Select Attribute to Visualize", 
                  choices = c("Property Type","Location","Rate")),
      conditionalPanel(condition = "input.Attributes == 'Rate'",
                       sliderInput("InputRate", "Select range for Rates (USD)", 
                                   min = 0, max = 500, value = 10, step = 1)),
      conditionalPanel(condition = "input.Attributes == 'Location'",
                       selectInput("InputLocation", "Select Location", 
                                   choices = c("Allston",
                                               "Back Bay",
                                               "Bay Village",
                                               "Beacon Hill",
                                               "Brighton",
                                               "Charlestown",
                                               "Chinatown",
                                               "Dorchester",
                                               "Downtown",
                                               "East Boston",
                                               "Fenway",
                                               "Hyde Park",
                                               "Jamaica Plain",
                                               "Leather District",
                                               "Longwood Medical Area",
                                               "Mattapan",
                                               "Mission Hill",
                                               "North End",
                                               "Roslindale",
                                               "Roxbury",
                                               "South Boston",
                                               "South Boston Waterfront",
                                               "South End",
                                               "West End",
                                               "West Roxbury"))),
      conditionalPanel(condition = "input.Attributes == 'Property Type'",
                       selectInput("InputPropertyType", "Select Type of Property", 
                                   choices = c("Apartment",
                                               "Bed & Breakfast",
                                               "Boat",
                                               "Camper/RV",
                                               "Condominium",
                                               "Dorm",
                                               "Entire Floor",
                                               "Guesthouse",
                                               "House",
                                               "Loft",
                                               "Other",
                                               "Townhouse",
                                               "Villa")))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map", plotOutput("listingplot")),
                  tabPanel("Summary",verbatimTextOutput("summarytable")),
                  tabPanel("Review",plotOutput("ReviewOut"))
      )
    )
  )
)

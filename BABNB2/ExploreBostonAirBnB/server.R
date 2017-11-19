library(shiny)
library(ggmap)
library(ggplot2)
library(plotly)
library(RSQLite)
library(sqldf)
library(readr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
sqlitePath <- "C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/Project.sqlite"
shinyServer(
  function(input,output,session)
  {
    listings <- read.csv(file="C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/listings_transformed.csv",
                         header=TRUE, sep=",",stringsAsFactors = FALSE)
    output$listingplot <- renderPlot(
      {
        if(input$Attributes == "Location")
        {
          listings <- read.csv(file="C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/listings_transformed.csv",
                               header=TRUE, sep=",",stringsAsFactors = FALSE)
          newlistings <- subset(listings,
                                listings$neighbourhood_cleansed == input$InputLocation,
                                select = c("latitude",
                                           "longitude",
                                           "property_type",
                                           "transformed_price",
                                           "neighbourhood_cleansed"))
          NLdataframe <- data.frame(newlistings)
          bostonmap <- get_map(location = c(lon = -71.0589, 
                                            lat = 42.3601), 
                               zoom = 15, 
                               maptype = "roadmap",
                               source = "google")
          displaymap <- ggmap(bostonmap,extent = "normal", maprange = TRUE) + geom_point(data= NLdataframe,
                                                                                         aes(x=longitude, 
                                                                                             y=latitude),
                                                                                         colour = "blue")
          displaymap
        }
        else if(input$Attributes == "Property Type")
        {
          listings <- read.csv(file="C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/listings_transformed.csv",
                               header=TRUE, sep=",",stringsAsFactors = FALSE)
          newlistings <- subset(listings,
                                listings$property_type == input$InputPropertyType,
                                select = c("latitude",
                                           "longitude",
                                           "property_type",
                                           "transformed_price",
                                           "neighbourhood_cleansed"))
          NLdataframe <- data.frame(newlistings)
          bostonmap <- get_map(location = c(lon = -71.0589, 
                                            lat = 42.3601), 
                               zoom = 15, 
                               maptype = "roadmap",
                               source = "google")
          displaymap <- ggmap(bostonmap,extent = "normal", maprange = TRUE) + geom_point(data= NLdataframe,
                                                                                         aes(x=longitude, 
                                                                                             y=latitude),
                                                                                         colour = "red")

          displaymap
         }
        else if(input$Attributes == "Rate")
        {
          listings <- read.csv(file="C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/listings_transformed.csv",
                               header=TRUE, sep=",",stringsAsFactors = FALSE)
          newlistings <- subset(listings,
                                listings$transformed_price <= input$InputRate,
                                select = c("latitude",
                                           "longitude",
                                           "property_type",
                                           "transformed_price",
                                           "neighbourhood_cleansed"))
          NLdataframe <- data.frame(newlistings)
          bostonmap <- get_map(location = c(lon = -71.0589, 
                                            lat = 42.3601), 
                               zoom = 15, 
                               maptype = "roadmap",
                               source = "google")
          displaymap <- ggmap(bostonmap, extent = "normal", maprange = TRUE) + geom_point(data= NLdataframe,
                                                                                          aes(x=longitude, 
                                                                                              y=latitude),
                                                                                          colour = "green")
          displaymap
          #plot(newlistings$longitude,newlistings$latitude,col="green")
          #hist(newlistings$transformed_price)
        }
      }
    )
    output$summarytable <- renderPrint(
      {
        if(input$Attributes == "Location")
        {
          listings <- read.csv(file="C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/listings_transformed.csv",
                               header=TRUE, sep=",",stringsAsFactors = FALSE)
          newlistings <- subset(listings,
                                listings$neighbourhood_cleansed == input$InputLocation,
                                select = c("transformed_price",
                                           "bedrooms",
                                           "bathrooms",
                                           "number_of_reviews"))
          summary(newlistings)
        }
        else if(input$Attributes == "Property Type")
        {
          listings <- read.csv(file="C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/listings_transformed.csv",
                               header=TRUE, sep=",",stringsAsFactors = FALSE)
          newlistings <- subset(listings,
                                listings$property_type == input$InputPropertyType,
                                select = c("transformed_price",
                                           "bedrooms",
                                           "bathrooms",
                                           "number_of_reviews"))
          summary(newlistings)
        }
        else if(input$Attributes == "Rate")
        {
          listings <- read.csv(file="C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/listings_transformed.csv",
                               header=TRUE, sep=",",stringsAsFactors = FALSE)
          newlistings <- subset(listings,
                                listings$transformed_price <= input$InputRate,
                                select = c("transformed_price",
                                           "bedrooms",
                                           "bathrooms",
                                           "number_of_reviews"))
          summary(newlistings)
        }
      }
    )
    output$ReviewOut <- renderPlot(
      {
        if(input$Attributes == "Location")
        {
          listings <- read.csv(file="C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/listings_transformed.csv",
                               header=TRUE, sep=",",stringsAsFactors = FALSE)
          reviewdf <- subset(listings,
                             listings$neighbourhood_cleansed == input$InputLocation,
                             select = c("summary"))
          wordscleanvs <- VectorSource(reviewdf)
          wordsclean <- SimpleCorpus(wordscleanvs, control = list(language = "en"))
          wordsclean <- tm_map(wordsclean, stripWhitespace)
          wordsclean <- tm_map(wordsclean, tolower)
          wordsclean <- tm_map(wordsclean, removeWords, stopwords("english"))
          wordsclean <- tm_map(wordsclean, removeWords, c("boston","kitchen","bed","apartment","view"))
          wordsclean <- tm_map(wordsclean, removeNumbers)
          wordsclean <- tm_map(wordsclean, removePunctuation)
          wordsclean <- tm_map(wordsclean, stemDocument)
          wordcloud(wordsclean, scale=c(5,0.5), max.words=100,min.freq=3, 
                    random.order=FALSE, rot.per=0.35, 
                    use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
          
          #data.frame(reviewdf)
        }
        else if(input$Attributes == "Property Type")
        {
          listings <- read.csv(file="C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/listings_transformed.csv",
                               header=TRUE, sep=",",stringsAsFactors = FALSE)
          reviewdf <- subset(listings,
                             listings$property_type == input$InputPropertyType,
                             select = c("summary"))
          wordscleanvs <- VectorSource(reviewdf)
          wordsclean <- SimpleCorpus(wordscleanvs, control = list(language = "en"))
          wordsclean <- tm_map(wordsclean, stripWhitespace)
          wordsclean <- tm_map(wordsclean, tolower)
          wordsclean <- tm_map(wordsclean, removeWords, stopwords("english"))
          wordsclean <- tm_map(wordsclean, removeWords, c("boston","kitchen","bed","apartment","view"))
          wordsclean <- tm_map(wordsclean, removeNumbers)
          wordsclean <- tm_map(wordsclean, removePunctuation)
          wordsclean <- tm_map(wordsclean, stemDocument)
          wordcloud(wordsclean, scale=c(5,0.5), max.words=100,min.freq=3, 
                    random.order=FALSE, rot.per=0.35, 
                    use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
        }
        else if(input$Attributes == "Rate")
        {
          listings <- read.csv(file="C:/B14 - Applied Statistical Analysis with R/Projects/BABNB2/ExploreBostonAirBnB/listings_transformed.csv",
                               header=TRUE, sep=",",stringsAsFactors = FALSE)
          reviewdf <- subset(listings,
                             listings$transformed_price <= input$InputRate,
                             select = c("summary"))
          wordscleanvs <- VectorSource(reviewdf)
          wordsclean <- SimpleCorpus(wordscleanvs, control = list(language = "en"))
          wordsclean <- tm_map(wordsclean, stripWhitespace)
          wordsclean <- tm_map(wordsclean, tolower)
          wordsclean <- tm_map(wordsclean, removeWords, stopwords("english"))
          wordsclean <- tm_map(wordsclean, removeWords, c("boston","kitchen","bed","apartment","view"))
          wordsclean <- tm_map(wordsclean, removeNumbers)
          wordsclean <- tm_map(wordsclean, removePunctuation)
          wordsclean <- tm_map(wordsclean, stemDocument)
          wordcloud(wordsclean, scale=c(5,0.5), max.words=100,min.freq=3, 
                    random.order=FALSE, rot.per=0.35, 
                    use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
        }
      }
    )
  }
)


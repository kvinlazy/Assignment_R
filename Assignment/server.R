#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(rattle)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    weatherstationlat <- reactive({ lat <- as.numeric(locationsAUS$latitude[ locationsAUS$name == input$city ])
    })
    
    weatherstationlng <- reactive({ as.numeric(locationsAUS$longitude[ locationsAUS$name == input$city ])
    })
    
    prepdata <- reactive({
        weatherAUS %>%
            filter(weatherAUS$Location == input$city & between(weatherAUS$Date, as.Date(input$date1), 
                                                               as.Date(input$date2)) & 
                       !is.na(input$Factor)) %>%
            select("Date",input$Factor)
    })
    
    
    output$text <- renderPlot({
        test <- weatherAUS %>%
            filter(weatherAUS$Location == input$city & 
                       between(weatherAUS$Date, as.Date(input$date1), as.Date(input$date2))) %>%
            select(input$Colfactor1,input$Colfactor2)
        test <- data.frame(test)
        test <- test[complete.cases(test),]
        colnames(test) <- c("Factor1","Factor2")
        ggplot(test, aes(Factor1, Factor2)) + 
            geom_point(color="#69b3a2",alpha = 0.5 ) +
            geom_smooth(color="red", fill="#be79df", se=TRUE)
    })
    
    output$distPlot <- renderPlot({
        
        plot(prepdata())
    })
    
    output$distPlot1 <- renderLeaflet({
        
        leaflet() %>% 
            setView(lng = weatherstationlng(), lat = weatherstationlat(), zoom = 12)  %>% 
            addTiles() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(lat = locationsAUS$latitude,
                       lng = locationsAUS$longitude,popup = locationsAUS$stnID, 
                       label = locationsAUS$name)
        
    })
    
})

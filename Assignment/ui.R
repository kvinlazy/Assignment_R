#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rattle)
library(shinythemes)
library(leaflet)
library(dplyr)
vars <- setdiff(names(weatherAUS),"Date")
vars <- setdiff(vars,"Location")
s <- range(weatherAUS$Date)


# Define UI for application that draws a histogram
shinyUI(fluidPage( theme = shinytheme("slate"),
                   
                   # Application title
                   titlePanel("Weather Station of Australia"),
                   
                   # Sidebar with a slider input for number of bins
                   sidebarLayout(
                       sidebarPanel(
                           selectInput("city","City of Austrailia", locationsAUS$name, selected = "Albury"),
                           selectInput("Factor","Factor", vars, selected = "MinTemp"),
                           dateInput("date1", "Start Date:", value = s[1], min = s[1], max=s[2]),
                           dateInput("date2", "Stop Date:", value = s[2], min = s[1], max=s[2]),
                           selectInput("Colfactor1","Coleration Factor", vars, selected = "MinTemp"),
                           selectInput("Colfactor2","Coleration Factor 2", vars, selected = "MaxTemp")
                       ),
                       mainPanel(
                           tabsetPanel(
                               tabPanel("Stations",leafletOutput("distPlot1")),
                               tabPanel("Plot",plotOutput("distPlot")),
                               tabPanel("GG Plot",plotOutput("text"))
                           )
                       ),
                   )
))

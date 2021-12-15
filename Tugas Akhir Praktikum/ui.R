library(ggplot2)
library(ggpubr)
library(shiny)
library(leaflet)
library(sf)
library(ggthemes)
library(DT)

ui <- fluidPage(
  
  leafletOutput("mymap"),
  
  br(),
  
  DT::dataTableOutput("mytable", width = "100%"),
  
  br(),
  
  plotOutput("gambar_ku"),
  
  br(),

  br()
  
)
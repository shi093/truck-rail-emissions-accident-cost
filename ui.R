library(shiny)
library(leaflet)
library(dplyr)
library(shinycssloaders)
library(rgdal)
library(plotly)
library(htmltools)
library(DT)
library(shinyjs)


ui<-fluidPage(
    tags$head(HTML("<title> Freight Transportation Costs for Truck and Rail</title>")),
    
    useShinyjs(),
    br(),
    span(style = "font-weight: 600; font-size: 25px; width: 100%; color: #022DB7;", 
         "Freight Transportation Costs for Truck and Rail"),
    
    br(),
    fluidRow(
      column(8, 
             
             leafletOutput("Zone", height = "550px")%>% withSpinner(color="#0dc5c1")),
      column(4, 
             span("Select "), span( style="color:green", "Origin"), span(" and "), span( style="color:red", "Destination"), 
             span(" from the map:"),
             br(),br(),
             htmlOutput("od_info")%>% withSpinner(color="#0dc5c1"),
             hr(),
             htmlOutput("od_total")%>% withSpinner(color="#0dc5c1"),
             plotlyOutput("od_pie", width = "100%", height = "350px")%>% withSpinner(color="#0dc5c1")
             
      )
    ),
    h4("Compare Rail vs. Truck"),
    br(),
    fluidRow(column(5, DT::dataTableOutput("od_vol"))),
    br(),
    tabsetPanel(
      
      tabPanel("Emission Costs",
               br(),
               fluidRow(
                 column(3,wellPanel(
                   
                   h5("Emissions Rate (grams / ton-mile)"),
                   fluidRow(
                     column(6,
                            
                            numericInput("co2",  div(style = "font-size:12px","CO2 Rail:"), 22.33927, min = 0),
                            numericInput("nox", div(style = "font-size:12px","NOx Rail:"), 0.23614, min = 0),
                            numericInput("pm25", div(style = "font-size:12px","PM2.5 Rail:"), 0.00573, min = 0),
                            numericInput("voc", div(style = "font-size:12px","VOC Rail:"), 0.00967, min = 0),
                            numericInput("so2", div(style = "font-size:12px","SO2 Rail:"), 0.00411, min = 0)
                     ),
                     column(6,
                            numericInput("co2t", div(style = "font-size:12px","CO2 Truck:"), 98.1366, min = 0),
                            numericInput("noxt", div(style = "font-size:12px","NOx Truck:"), 0.2698, min = 0),
                            numericInput("pm25t", div(style = "font-size:12px","PM2.5 Truck:"), 0.0138, min = 0),
                            numericInput("voct", div(style = "font-size:12px","VOC Truck:"), 0.0131, min = 0),
                            numericInput("so2t", div(style = "font-size:12px","SO2 Truck:"), 0.0009, min = 0)
                            
                     )
                   ),
                   
                   hr(),
                   h5("Emission Costs ($/ton)"),
                   fluidRow(
                     column(6,
                            numericInput("co2c", div(style = "font-size:12px","CO2 Cost:"), 1.01742437964137, min = 0),
                            numericInput("noxc", div(style = "font-size:12px","NOx Cost:"), 8749.84966491578, min = 0),
                            numericInput("pm25c", div(style = "font-size:12px","PM2.5 Cost:"), 394048.462235102, min = 0)
                     ),
                     column(6,
                            numericInput("vocc", div(style = "font-size:12px","VOC Cost:"), 2136.59119724688, min = 0),
                            numericInput("so2c", div(style = "font-size:12px","SO2 Cost:"), 50972.9614200326, min = 0)
                            
                     )
                   ),
                   actionButton("resetEmission", "Reset Emissions Rate")
                 )
                 ),
                 column(9, 
                        br(),
                        fluidRow(
                          column(6, div(DT::dataTableOutput("emission_info"),  width = "100%", style = "font-size:90%"))
                        ), 
                        br(),br(),
                        fluidRow(
                          plotlyOutput("emission_chart", width = "100%", height = "550px")%>% withSpinner(color="#0dc5c1")
                        )
                 )  
               )),
      
      tabPanel("Accident Costs",
               br(),
               fluidRow(
                 column(3,wellPanel(
                   
                   h5("Fatality Event Rate (events/billion ton-miles)"),
                   fluidRow(
                     column(6,
                            
                            numericInput("rail_fatality",  div(style = "font-size:12px","Rail:"), 0.39, min = 0)
                     ),
                     column(6,
                            numericInput("truck_fatality", div(style = "font-size:12px","Truck:"), 2.54, min = 0)
                     )
                   ),
                   
                   h5("Injury Event Rate (events/billion ton-miles)"),
                   fluidRow(
                     column(6,
                            numericInput("rail_injury",  div(style = "font-size:12px","Rail:"), 3.32, min = 0)
                     ),
                     column(6,
                            numericInput("truck_injury", div(style = "font-size:12px","Truck:"), 55.98, min = 0)
                     )
                   ),
                   
                   hr(),
                   h5("Accident Cost"),
                   fluidRow(
                     column(6,
                            numericInput("cost_fatality", div(style = "font-size:12px","Fatality Cost:"), 9767274.045, min = 0)
                     ),
                     column(6,
                            numericInput("cost_injury", div(style = "font-size:12px","Injury Cost:"), 177031.8421, min = 0)
                     )
                   ),
                   actionButton("resetSafety", "Reset Safety Rate")
                 )
                 ),
                 column(9, 
                        br(),
                        fluidRow(
                          column(6, div(DT::dataTableOutput("safety_info"),  width = "100%", style = "font-size:90%"))
                        ), 
                        br(),br(),
                        fluidRow(
                          column(1,),
                          column(9,
                                 plotlyOutput("safety_chart", width = "100%", height = "550px")%>% withSpinner(color="#0dc5c1")
                          )
                        )
                 )  
               )    
      ),
      
      tabPanel("Pavement Damage Cost",
               br(),
               fluidRow(
                 column(3,wellPanel(
                   numericInput("pavement_cost",  div(style = "font-size:12px","Pavement Damage Cost ($/mile): "), 0.385498385, min = 0),
                   hr(),
                   h5("Rate for calculating truck shipments:"),
                   numericInput("carload_ton", div(style = "font-size:12px","Tons/Carload:"), 80.54, min = 1),
                   numericInput("truck_per_carload",  div(style = "font-size:12px","Trucks per Carload: "), 3.5, min = 0),
                   actionButton("resetPavement", "Reset Damage Rate")
                 )
                 ),
                 column(9, 
                        br(),
                        fluidRow(
                          column(6,div(DT::dataTableOutput("pavement_info"),  width = "100%", style = "font-size:90%")),
                          column(1, )
                        )
                 )  
               )    
      ),
      
      tabPanel("Efficiency Benefits",
               br(),
               fluidRow(
                 column(3,wellPanel(
                   
                   h5("Revenue per Ton-Mile"),
                   fluidRow(
                     column(6,
                            
                            numericInput("rail_tm",  div(style = "font-size:12px","Rail:"), 0.04190727, min = 0)
                     ),
                     column(6,
                            numericInput("truck_tm", div(style = "font-size:12px","Truck:"), 0.19157565, min = 0)
                     )
                   ),
                   actionButton("resetEfficiency", "Reset Revenue Rate")
                 )
                 ),
                 column(9, 
                        br(),
                        fluidRow(
                          column(6,div(DT::dataTableOutput("efficiency_info"),  width = "100%", style = "font-size:90%")),
                          column(1)
                        )
                 )  
               )    
      )
    )
    
  )
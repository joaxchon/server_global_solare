
library(shiny, quietly = TRUE)
library(dbConnect)
library(tidyverse)
library("shinythemes")
library("FitAR")
library("plotly")
navbarPage("PAX",
           theme = shinytheme("cerulean"),
           #theme = shinytheme("united"),
           #theme = "business-casual.css",
           #theme = "template-style.css",
           tabPanel("Home", 
                    includeHTML("www/index.html")
           ),
           tabPanel("Pronóstico de MTR",
                    
                    # App title ----
                    titlePanel("Modelos de pronósticos del s"),
                    
                    # Sidebar layout with input and output definitions ----
                    sidebarLayout(
                      # Sidebar panel for inputs ----
                      sidebarPanel(
                        textInput("node", "Calcular precios para:",
                                  "01AAN-85"),
                        
                        dateInput("date_1", "Fecha para el pronostico:", value = Sys.Date()+1, format = "dd/mm/yyyy"),
                        
                        dateInput("date_2", "Fecha al menos un mes atras:", value = Sys.Date()-30, format = "dd/mm/yyyy"),
                        selectInput("decision", "Determino que se parecera a:",
                                    choices = c("Dia previo","Dia Anterior Similar", "Promedio 4 dias similares", "Promedio Semana","Pronostico ARIMA","Promedio 2 dias anteriores", "Promedio 2 dias similares")),
                        p("En este hay que escoger la decision"),
                        downloadButton("PreciosChidos", "Download"),
                        sliderInput("perc", "Agregar porcentaje:",
                                    min = -40, max = 40, value = 0
                        )
                        
                        
                      ),
                      
                      
                      
                      # Main panel for displaying outputs ----
                      mainPanel(
                        
                        # Output: Tabset w/ plot, summary, and table ----
                        tabsetPanel(type = "tabs",
                                    tabPanel("Gráficas", plotOutput("plot"),plotOutput("TEST"),plotOutput("residuales"))
                                    
                        )
                        
                      )
                    )
           ),
           tabPanel("Datos duros",
                    verbatimTextOutput("summary"),tableOutput("tabla")
           ),
           navbarMenu("Checar",
                      tabPanel("Otras opciones",
                               dataTableOutput("table")
                      ),
                      tabPanel("Nosotros",
                               fluidRow(
                                 column(6,
                                        
                                        "nothing" ),
                                 column(3,
                                        img(class="img-polaroid",
                                            src=paste0("http://upload.wikimedia.org/",
                                                       "wikipedia/commons/9/92/",
                                                       "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                        tags$small(
                                          "Source: Photographed at the Bay State Antique ",
                                          "Automobile Clubs July 10, 2005 show at the ",
                                          "Endicott Estate in Dedham, MA by ",
                                          a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                            "User:Sfoskett")
                                        )
                                 )
                               )
                      )
           )
)

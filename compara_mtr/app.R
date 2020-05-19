library(shiny, quietly = TRUE)
library(dbConnect)
library(tidyverse)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("MAPE"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    ################################## ESTE INPUT DEBE SER ESCRITO EL NODO 
    sidebarPanel(
      textInput("nodo", "Comparar pronosticos para el nodo:", "01AAN-85"),
      
      dateInput("date_1", "Fecha inicial", value = Sys.Date()-2, format = "dd/mm/yyyy"),
      
      dateInput("date_2", "Fecha final:", value = Sys.Date()-1, format = "dd/mm/yyyy"),
      downloadButton("PreciosChidos", "Download")
      #uiOutput("clientes")
      
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Resultado", plotOutput("plot"),tableOutput("table"))
                  
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  matriz_pml <- reactive({
    fecha_1 <- paste('\"',input$date_1, '\"', sep = "")
    fecha_2 <- paste('\"',input$date_2, '\"', sep = "")
    
    nodo_base <- paste('\"',input$nodo,'\"',sep = "")
    
    con <- dbConnect(MySQL(), user='admin', password='joaxon1234', 
                     dbname='mem_data', host='database-1.cm4ctfesbule.us-east-2.rds.amazonaws.com',
                     port=3306)
    rs <- dbSendQuery(con, paste('select fecha, hora, clave_nodo as nodo, pml as precio_mtr from pml_mtr ',
                                 'where fecha between ',fecha_1,' and ',fecha_2,' and clave_nodo = ', nodo_base,' ;'  ,sep=''))
    
    mtr_pml<- fetch(rs, n=-1)
    
    #dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)
    mtr_pml
  })
  
  matriz_pronos <- reactive({
    #print("entro")
    fecha_1 <- paste('\"',input$date_1, '\"', sep = "")
    fecha_2 <- paste('\"',input$date_2, '\"', sep = "")
    nodo_base <- paste('\"',input$nodo,'\"',sep = "")
    
    
    
    con <- dbConnect(MySQL(), user='admin', password='joaxon1234', 
                     dbname='Info_interna', host='database-1.cm4ctfesbule.us-east-2.rds.amazonaws.com',
                     port=3306)
    rs <- dbSendQuery(con, paste('select * from pronostico_mtr where fecha between ',fecha_1, ' and ', fecha_2, ' and nodo = ', nodo_base,' ;',sep = ""))  
    pml_pronos<- fetch(rs, n=-1)
    dbDisconnect(con)
    #print(paste('select * from pronostico_mtr where fecha between ',fecha_1, ' and ', fecha_2, ' and nodo = ', nodo_base,' ;',sep = ""))
    #print(pml_pronos)
    pml_pronos
  })
  
  DISPLAY <- reactive({ 
    if(length(matriz_pml()$hora) == length(matriz_pronos()$hora)){ 
      data.frame(matriz_pronos(),matriz_pml())
    }
  })
  
  
  output$plot <- renderPlot({
    if(length(matriz_pml()$hora) == length(matriz_pronos()$hora)){
      ggplot(DISPLAY(), aes(x = seq(1,length(DISPLAY()$`hora`)))) + geom_line(aes(y = precio_mtr), color = "blue", size = 1)+
        geom_line(aes(y = precio_pronosticado),col = "red", size = 1, alpha  = 0.5)+ 
        geom_ribbon( aes(ymin=precio_mtr,ymax=precio_pronosticado), fill="green", alpha=0.1)+
        labs(x = "Horas", y = "MXN")
    }
  })
  
  
  
  output$table <- renderTable({ 
    if(length(matriz_pml()$hora) == length(matriz_pronos()$hora)){
      DISPLAY() %>% 
        dplyr::select(fecha,hora,precio_mtr,precio_pronosticado) %>%
        mutate(precio_mtr-precio_pronosticado) %>%
        mutate(porcentaje_error = (precio_mtr-precio_pronosticado)/precio_pronosticado * 100)
      
    }
    
    
  }, digits = 4)
  
  output$PreciosChidos <- downloadHandler(
    filename = function() {
      paste("MAPEMAPE", ".csv", sep = "")
    },
    content = function(file) {
      df <- data.frame(DISPLAY()) 
      write.csv(df, file, row.names = FALSE)
    }
    ,
    contentType = "csv"
  )
  
}
#app <- shinyApp(ui, server)
shinyApp(ui,server)
#runApp(app, host = "0.0.0.0", port = 5050)
# Create Shiny app -------

library(shiny, quietly = TRUE)
############# shiny es el paquete que nos permite hacer hacer la pagina web
library(dbConnect)
############# paquete que nos permite hacer consultas y enviar cosas a bases
############# paquete que nos permite mandar mails
library(tidyverse)

sube_a_base <- function(DATA){
  con <- dbConnect(MySQL(), user='admin', password='joaxon1234', 
                   dbname='Info_interna', host='database-1.cm4ctfesbule.us-east-2.rds.amazonaws.com',
                   port=3306)
  dbWriteTable(con, "pronostico_mtr",DATA, append = TRUE,row.names = FALSE)
  dbDisconnect(con)
  
}



ui <- fluidPage(
  titlePanel("Upload pronosticos MTR"),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      textInput("nodo_pronos", "Nodo del pronostico","01AAN-85"),
      dateInput("date_1", "Fecha DEL pronostico ", value = Sys.Date(), format = "dd/mm/yyyy"),
      uiOutput("prueba_dinamica"),
      fileInput("archivo","Cargarlo"),
      actionButton("subir","SUBIR")
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Datos",tableOutput("datos"))
                  
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  
  output$datos <- renderTable({  
    DATA()
  })
  
  output$prueba_dinamica<- renderUI({
    
  })
  
  
  
  
  
  DATA <- reactive({
    req(input$archivo)
    datos_temp <- c()
    
    inFile <- input$archivo
    datos_temp <-  read.csv(inFile$datapath,check.names = FALSE)
    
    
    DATOS <- c()
    datos_temp <- data.frame(datos_temp)
    fecha_rep <- as.Date(rep(input$date_1,length(datos_temp[,1])))
    clave_rep <- rep(input$nodo_pronos,length(datos_temp[,1]))
    hora <- seq(1,length(datos_temp[,1]))
    precios <- datos_temp[,1]
    DATOS <- data.frame(fecha_rep,hora,clave_rep,precios)
    if(ncol(datos_temp) > 1){
      hora <- seq(1,length(datos_temp[,2]))
      fecha_rep <- as.Date(rep(input$date_1,length(datos_temp[,2])))
      clave_rep <- rep(names(datos_temp[2]),length(datos_temp[,2]))
      precios <- datos_temp[,2]
      DATOS <- rbind(DATOS,data.frame(fecha_rep,hora,clave_rep,precios))
      if(ncol(datos_temp > 2)){ 
        hora <- seq(1,length(datos_temp[,3]))
        fecha_rep <- as.Date(rep(input$date_1,length(datos_temp[,3])))
        clave_rep <- rep(names(datos_temp[3]),length(datos_temp[,3]))
        precios <- datos_temp[,3]
        DATOS <- rbind(DATOS,data.frame(fecha_rep,hora,clave_rep,precios))
        
      } 
      
    }
    names(DATOS) <- c("fecha","hora","nodo","precio_pronosticado")
    if(input$subir){ 
      sube_a_base(DATOS)
    }
    DATOS
    
  })  
  
  
  
  
  
  
}


#app <- shinyApp(ui, server)
shinyApp(ui, server)

#runApp(app, host = "0.0.0.0", port = 5050) 
# Create Shiny app -------------

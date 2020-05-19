library(shiny, quietly = TRUE)
############# shiny es el paquete que nos permite hacer hacer la pagina web
library(dbConnect)
library("e1071")
############# paquete que nos permite mandar mails
library(tidyverse)
library(quantmod)

sube_a_base <- function(DATA){
  con <-  dbConnect(MySQL(), user='admin', password='joaxon1234', 
                    dbname='mem_data', host='database-1.cm4ctfesbule.us-east-2.rds.amazonaws.com',
                    port=3306)
  dbWriteTable(con, "pronostico_mda",DATA, append = TRUE,row.names = FALSE)
  dbDisconnect(con)
  
}



ui <- fluidPage(
  titlePanel("Generador de Pronosticos"),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      #textInput("nodo_pronos", "Nodo del cliente","01-AAN85"),
      
      dateInput("date_1", "Fecha DEL pronostico ", value = Sys.Date()+1, format = "dd/mm/yyyy"),
      uiOutput("clientes"),
      fileInput("archivo","Cargar datos anteriores"),
      #actionButton("subir","SUBIR"), 
      selectInput("decision", "Utilizar: ", c("Soporte Vectorial", "Lineal Multiple","Dia Anterior","Promedio Ingresado")),
      numericInput("threshold", "Threshold sobre-sub",100),
      radioButtons("riesgo","Tipo de estrategia", choices = c("Flat","Tranquila","Media","Agresiva")),
      #actionButton("generar", "generar")
      downloadButton("downloadData","Generar csv")
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Datos",plotOutput("Grafica"),plotOutput("GraficaTrading"),plotOutput("GraficaCvar"),plotOutput("GraficaSmooth"),plotOutput("GraficaDens"),tableOutput("datos"))
                  
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  
  clientes_opc <-  reactive({ 
    
    
    con <- dbConnect(MySQL(), user='admin', password='joaxon1234', 
                     dbname='Info_interna', host='database-1.cm4ctfesbule.us-east-2.rds.amazonaws.com',
                     port=3306)
    #rs <- dbSendQuery(con, 'set character set "utf8"')
    rs <- dbSendQuery(con, 'select cliente from clientes ; ' ) 
    
    
    clientes_opc_1<- fetch(rs, n=-1)
    
    dbDisconnect(con)
    clientes_opc_1
  })
  
  output$datos <- renderTable({  
    #DATA()
    #DATA_precios()  
    #DATA_cvar()
    DATA_final()
    })
  
  output$Grafica <- renderPlot({
    
    ####################### convertir columnas en filas   
    TABLA <-  gather(data.frame(DATA()),"Modelo","valor",-c("hora"))
    ggplot(TABLA)  + geom_line(aes(x = hora, y = valor, col = Modelo)) + xlab("Hora") + ylab("Mwh") + ggtitle("Pronósticos de Generacion") +
      theme(legend.position="bottom")
    ############## No sirvió el piping, no tengo ni idea porque. 
  })
  
  output$GraficaSmooth <- renderPlot({
    req(input$archivo)
    
    precios<- data.frame(DATA_precios())
    precios <- mutate(precios,"fecha_hora" = paste(fecha,hora,sep= " "))
    ggplot(precios,aes(x=as.POSIXct(fecha_hora))) + geom_smooth(aes(y=pml_mda),col="blue") + geom_smooth(aes(y=pml_mtr),col="red")
    
  })
  
  output$GraficaTrading <- renderPlot({
    req(input$archivo)
    
    precios<- data.frame(DATA_precios()) 
    barChart(ts(precios$spread),name="Spread MDA - MTR")
    addEMA()
    addMACD()
    
  })
  
  
  output$GraficaCvar <- renderPlot({
    
    req(input$archivo)
    
    cvars <- data.frame(DATA_cvar()) 
    cvars <- cvars %>% gather(key = "Nivel",value = "valor",-c("hora","estrat"))
    ggplot(cvars) + geom_tile(aes(x=hora,y=Nivel,fill=valor))+ scale_fill_gradient(low="light green",high="red")+ facet_wrap(~estrat,scales="free")+
      theme(element_blank(),panel.background = element_blank()) + ggtitle("Tail value at risk MDA-MTR") 
    
  })
  
  output$GraficaDens <- renderPlot({
    req(input$archivo)
    
    precios <-data.frame(DATA_precios())
    ggplot(precios) + geom_density(aes(x=pml_mda),col="blue",fill="blue", alpha=0.5)+ geom_density(aes(x=pml_mtr),col="red",fill="red", alpha=0.1) + facet_wrap(~hora)
    
  })
  
  
  DATA <- reactive({
    req(input$archivo)
    datos_temp <- c()
    
    ################################## leo el archivo CSV  
    inFile <- input$archivo
    datos_temp <-  read.csv(inFile$datapath,check.names = FALSE)
    ########################################################## checo que el archivo tenga fechas y datos
    fecha_min <- min(as.Date(datos_temp[,"fecha"]))
    fecha_max <- max(as.Date(datos_temp[,"fecha"]))+1
    ################################# consulto temperaturas
    cliente_temp <- paste('"',input$cliente,'"',sep="")
    ########## query intermedia vieja: select * from mem_data.weather where
    fecha_pronos <- as.character(as.Date(input$date_1))
    query <- paste('SELECT * 
FROM mem_data.weather WHERE (fecha,fecha_consulta) IN 
( SELECT fecha, MAX(fecha_consulta)
  FROM mem_data.weather
  GROUP BY fecha
) and (fecha between ',paste('"',as.character(fecha_min),'"',sep="") ,' and ',paste('"',as.character(fecha_max),'"',sep=""),' or fecha =',paste('"',fecha_pronos,'")',sep="") ,' and ciudad = (select lugar from Info_interna.clientes where cliente = ',cliente_temp,');')
    
    con <- dbConnect(MySQL(), user='admin', password='joaxon1234', 
                     dbname='mem_data', host='database-1.cm4ctfesbule.us-east-2.rds.amazonaws.com',
                     port=3306)
    rs <- dbSendQuery(con, query) 
    
    
    temperaturas<- fetch(rs, n=-1)
    
    dbDisconnect(con)
    
    ################################# modelos 
    
    temperaturas[,"conditions"] <- as.numeric(as.factor(temperaturas[,"conditions"]))
    
    datos_fechas_archivo <- merge(datos_temp,temperaturas[which(temperaturas$fecha != as.Date(fecha_pronos)),], by=c("hora","fecha"))
    
    x_gorro <- temperaturas[which(temperaturas$fecha == as.Date(fecha_pronos)),]
    
    
    
    
    
    ############## Regresión lineal con temperaturas
    modelo_lineal_mult <- lm(formula = energia ~ hora + temp_f + cloud_cover_porcentaje + conditions, data=datos_fechas_archivo )
    resumen <-summary(modelo_lineal_mult)
    #print(resumen)
    energia_modelo_1 <- pmax(0,predict(modelo_lineal_mult,x_gorro[,c("hora","temp_f","cloud_cover_porcentaje","conditions")]))
    #print(x_gorro[,"hora"])
    #print(length(energia_modelo_1))
    modelo_msv <- svm(energia ~ hora + temp_f + cloud_cover_porcentaje + conditions, data=datos_fechas_archivo)
    
    energia_modelo_2 <- pmax(0,predict(modelo_msv,x_gorro[,c("hora","temp_f","cloud_cover_porcentaje","conditions")]))
    
    
    ############## Datos anteriores
    energia_modelo_3 <- datos_fechas_archivo[which(as.Date(as.character(datos_fechas_archivo$fecha)) == fecha_max-1),"energia"]
    ############## Regresion Lineal sin temperaturas
    promedio <-summarise(group_by(datos_fechas_archivo,hora), "promedio_observado" = mean(energia))
    energia_promedio <- promedio$promedio_observado
    ################################# graficas y eleccion  
    
    #data.frame("hora" = seq(1,24,1),"modelo_1" = modelo_1,"modelo_2" = modelo_2,"modelo_3" = modelo_3,"modelo_4" = modelo_4 )
    data.frame("hora"= seq(0,23,1), "MLM" = energia_modelo_1, "MSV" = energia_modelo_2, "UltimoDia" = energia_modelo_3, "PromedioIngresado" = energia_promedio )
  })
  
  
  DATA_precios <- reactive({
    
    fecha <- as.Date(input$date_1)
    fecha_1 <- paste('"',as.character(fecha-20),'"',sep="")
    fecha_2 <- paste('"',as.character(fecha),'"',sep="")
    cliente <- paste('"',input$cliente,'"',sep="")
    query <- paste('select * from pml_mda where fecha between ',fecha_1, ' and ', fecha_2, ' and clave_nodo = (SELECT nodo from Info_interna.clientes where cliente = ',cliente,");",sep="")
    query2 <- paste('select * from pml_mtr where fecha between ',fecha_1, ' and ', fecha_2, ' and clave_nodo = (SELECT nodo from Info_interna.clientes where cliente = ',cliente,");",sep="")
    
    con <- dbConnect(MySQL(), user='admin', password='joaxon1234', 
                     dbname='mem_data', host='database-1.cm4ctfesbule.us-east-2.rds.amazonaws.com',
                     port=3306)
    rs <- dbSendQuery(con, query) 
    
    
    precios_mda<- fetch(rs, n=-1)
    
    dbDisconnect(con)
    
    con <- dbConnect(MySQL(), user='admin', password='joaxon1234', 
                     dbname='mem_data', host='database-1.cm4ctfesbule.us-east-2.rds.amazonaws.com',
                     port=3306)
    rs <- dbSendQuery(con, query2) 
    
    
    precios_mtr<- fetch(rs, n=-1)
    
    dbDisconnect(con)
    names(precios_mda)[4] <- "pml_mda"
    names(precios_mtr)[4]<- "pml_mtr"
    precios <- merge(precios_mda,precios_mtr,by = c("fecha","hora"))
    precios <- precios %>% mutate("spread" = pml_mda-pml_mtr)
    
  })
  
  DATA_cvar <- reactive({
    req(input$archivo)
    precios <- data.frame(DATA_precios())
    precios <- mutate(precios, "spread" = pml_mda-pml_mtr)
    
    var_sob_1<- rep(0,24)
    var_sob_2<- rep(0,24)
    var_sob_3<- rep(0,24)
    cvar_sob_1<- rep(0,24) 
    cvar_sob_2<- rep(0,24) 
    cvar_sob_3<- rep(0,24) 
    
    var_sub_1<- rep(0,24)
    var_sub_2<- rep(0,24)
    var_sub_3<- rep(0,24)
    cvar_sub_1<- rep(0,24) 
    cvar_sub_2<- rep(0,24) 
    cvar_sub_3<- rep(0,24) 
    
    for( i in seq(1,24,1)){
      temp <- precios$spread[which(precios$hora == i)]
      var_sob_1[i] <- quantile(temp,0.6)
      cvar_sob_1[i] <- sum(precios$spread[which(precios$hora == i & precios$spread > var_sob_1[i])])/length(precios$spread[which(precios$hora == i & precios$spread > var_sob_1[i])])
      var_sob_2[i] <- quantile(temp,0.75)
      cvar_sob_2[i] <- sum(precios$spread[which(precios$hora == i & precios$spread > var_sob_2[i])])/length(precios$spread[which(precios$hora == i & precios$spread > var_sob_2[i])])
      var_sob_3[i] <- quantile(temp,0.9)
      cvar_sob_3[i] <- sum(precios$spread[which(precios$hora == i & precios$spread > var_sob_3[i])])/length(precios$spread[which(precios$hora == i & precios$spread > var_sob_3[i])])
      var_sub_1[i] <- quantile(-temp,0.6)
      #cvar_sub_1[i] <- -sum(precios$spread[which(precios$hora == i & precios$spread < var_sub_1[i])])/length(precios$spread[which(precios$hora == i & precios$spread < var_sub_1[i])])
      #var_sub_2[i] <- quantile(temp,0.25)
      #cvar_sub_2[i] <- -sum(precios$spread[which(precios$hora == i & precios$spread < var_sub_2[i])])/length(precios$spread[which(precios$hora == i & precios$spread < var_sub_2[i])])
      #var_sub_3[i] <- quantile(temp,0.1)
      #cvar_sub_3[i] <- -sum(precios$spread[which(precios$hora == i & precios$spread < var_sub_3[i])])/length(precios$spread[which(precios$hora == i & precios$spread < var_sub_3[i])])
      cvar_sub_1[i] <- -sum(precios$spread[which(precios$hora == i & -precios$spread < var_sub_1[i])])/length(precios$spread[which(precios$hora == i & -precios$spread < var_sub_1[i])])
      var_sub_2[i] <- quantile(-temp,0.75)
      cvar_sub_2[i] <- -sum(precios$spread[which(precios$hora == i & -precios$spread < var_sub_2[i])])/length(precios$spread[which(precios$hora == i & -precios$spread < var_sub_2[i])])
      var_sub_3[i] <- quantile(-temp,0.9)
      cvar_sub_3[i] <- -sum(precios$spread[which(precios$hora == i & -precios$spread < var_sub_3[i])])/length(precios$spread[which(precios$hora == i & -precios$spread < var_sub_3[i])])
      
    }
    
    cvars_sob <- data.frame("hora"= seq(1,24,1),"60_porciento" = cvar_sob_1,"75_porciento" = cvar_sob_2,"90_porciento" = cvar_sob_3,"estrat" ="sobre")
    cvars_sub <- data.frame("hora"= seq(1,24,1),"60_porciento" = cvar_sub_1,"75_porciento" = cvar_sub_2,"90_porciento" = cvar_sub_3,"estrat" ="sub")
    
    cvars <-rbind(cvars_sob,cvars_sub)
    
  })
  
  
  
  DATA_final <- reactive({
    
    modelos <- data.frame(DATA())
    cvar <- data.frame(DATA_cvar())
    
    seleccion <- switch(input$decision,
                       "Soporte Vectorial" = modelos$MSV ,
                       "Lineal Multiple"=modelos$MLM,
                       "Dia Anterior"=modelos$UltimoDia,
                       "Promedio Ingresado"=modelos$PromedioIngresado )
    
    
    r_1 <- 1.15
    r_2 <- 1/r_1  
    
    cvar_comparador_sobre <- switch(input$riesgo,
                              "Flat" = rep(Inf,24),
                              "Tranquila" = cvar$X90_porciento[which(cvar$estrat=="sobre")],
                              "Media" = cvar$X75_porciento[which(cvar$estrat=="sobre")],
                              "Agresiva" = cvar$X60_porciento[which(cvar$estrat=="sobre")] )
                              
    cvar_comparador_sub <- switch(input$riesgo,
                                    "Flat" = rep(Inf,24),
                                    "Tranquila" = cvar$X90_porciento[which(cvar$estrat=="sub")],
                                    "Media" = cvar$X75_porciento[which(cvar$estrat=="sub")],
                                    "Agresiva" = cvar$X60_porciento[which(cvar$estrat=="sub")] )
    
    
    
  nominado <- rep(0,24)
  for(i in seq(1,24,1)){
    if(cvar_comparador_sobre[i] < input$threshold){
      nominado[i] <- seleccion[i]*r_1
    }
    if(cvar_comparador_sub[i] < -input$threshold){
      nominado[i] <- seleccion[i]*r_2
    }
    else{
    nominado[i] <- seleccion[i]
    }
  }
  # data.frame("Modelo_Seleccionado" = seleccion,"Nominacion"=nominado)
   
   miniquery <- paste('select clave_cenace from Info_interna.clientes where cliente =',paste('"',input$cliente,'"',sep=""),";",sep="")
   con <- dbConnect(MySQL(), user='admin', password='joaxon1234', 
                    dbname='mem_data', host='database-1.cm4ctfesbule.us-east-2.rds.amazonaws.com',
                    port=3306)
   rs <- dbSendQuery(con, miniquery) 
   
   
   clave_cenace_bd<- fetch(rs, n=-1)
   
   dbDisconnect(con)
   clave_cenace_bd
   data.frame("Generador"=rep(as.character(clave_cenace_bd),24),"Dia de Flujo" = rep(format(input$date_1,"%d/%m/%Y" ),24) , "Hora"=seq(100,2400,100), "Timezone"= rep("CST.MX",24), "Disponibilidad (MW)" = nominado, check.names = FALSE)
  })
  
  output$clientes <- renderUI({
    selectInput("cliente", "Generar oferta para:", choices = clientes_opc())
  }) 
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(DATA_final()$Generador[1],"_",input$date_1," ",input$cliente, ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(DATA_final(),check.names = F), file,row.names=FALSE)
    }
  )
  
  
}


#app <- shinyApp(ui, server)
shinyApp(ui, server)

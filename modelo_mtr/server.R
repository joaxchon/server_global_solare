server <- function(input, output) {
  
  
  library(shiny, quietly = TRUE)
  library(dbConnect)
  library(tidyverse)
  library("shinythemes")
  library("FitAR")
  library("plotly")
  data <- reactiveValues()
  get_data <-  reactive({ 
    req(nchar(input$node) > 7)
    
    fecha_1 <- paste('\"',input$date_1,'\"',sep = "")
    fecha_2 <- paste('\"',input$date_2,'\"',sep = "")
    dias_sim <- 'and weekday(fecha) in (0,1,2,3,4,5,6)'
    
    
    con <- dbConnect(MySQL(), user='admin', password='joaxon1234', 
                     dbname='mem_data', host='database-1.cm4ctfesbule.us-east-2.rds.amazonaws.com',
                     port=3306)
    rs <- dbSendQuery(con, paste('select fecha, hora, clave_nodo as nodo,pml as precio_mtr from pml_mtr ',
                                 'where fecha between ',fecha_2,' and ',fecha_1,' and clave_nodo = ', paste('"',as.character(input$node),'"', sep = ""),';'  ,sep=''))
    
    mtr_pnd<- fetch(rs, n=-1)
    
    dbDisconnect(con)
    mtr_pnd
  })
  
  
  df_final <- reactive({
    
    df <- data.frame(get_data()) 
    
    df_aux <- df[which(df$nodo == input$node),]
    fechas_aux <- as.Date(df_aux$fecha)
    simi_index <- which(weekdays(fechas_aux) == weekdays(input$date_1))
    
    dia <- tail(df_aux, 24)
    df_aux_sim <- df_aux[simi_index,]
    dia_2_a <- tail(df_aux,48)
    dia_1_sim <- tail(df_aux_sim,24)
    dia_4_sim <- tail(df_aux_sim,96)
    dia_2_sim <- tail(df_aux[simi_index,],48)
    sem <- tail(df_aux,168)
    
    
    prom_4_sim <- c()
    prom_2_sim <- c()
    prom_2_a <- c()
    prom_sem <- c()
    for(i in 1:24){
      prom_4_sim  <- c(prom_4_sim,mean(dia_4_sim$precio_mtr[which(dia_4_sim$hora == i)]))
      prom_2_sim  <- c(prom_2_sim,mean(dia_2_sim$precio_mtr[which(dia_2_sim$hora == i)]))
      prom_2_a  <- c(prom_2_a,mean(dia_2_a$precio_mtr[which(dia_2_a$hora == i)]))
      prom_sem <- c(prom_sem, mean(sem$precio_mtr[which(sem$hora == i)]))
    }
    
    
    
    
    if(input$decision == "Dia previo"){
      dia$precio_mtr <- dia$precio_mtr*(1+input$perc/100)
      precio <- dia$precio_mtr
    }
    if(input$decision == "Dia Anterior Similar"){
      dia_1_sim$precio_mtr <- dia_1_sim$precio_mtr*(1+input$perc/100)
      precio <- dia_1_sim$precio_mtr
    }
    if(input$decision == "Promedio Semana"){
      prom_sem<- prom_sem*(1+input$perc/100)
      precio <- prom_sem
    }
    if(input$decision == "Promedio 2 dias similares"){
      prom_2_sim <- prom_2_sim*(1+input$perc/100)
      precio <- prom_2_sim
    }
    if(input$decision == "Promedio 4 dias similares"){
      prom_4_sim <- prom_4_sim*(1+input$perc/100)
      precio <- prom_4_sim
    }
    if(input$decision == "Promedio 2 dias anteriores"){
      prom_2_a <- prom_2_a*(1+input$perc/100)
      precio <- prom_2_a
    }
    
    
    
    residuales_vec <- c()
    hora_vec <- c()
    
    prediccion <- rep(0,24)
    for(j in 1:24){
      data_hour <- df_aux[which(df_aux$hora == j),]
      time_series <- data_hour$precio_mtr
      ff <- ar(time_series)
      orden <- ff$order
      fit <- FitAR(time_series,max(orden,1))
      a <- predict(fit,1)
      prediccion[j] <- a$Forecasts
      residuales_vec <- c(residuales_vec,fit$res)
      hora_vec <- c(hora_vec,rep(j, length(fit$res)))
      
      
    }
    residuales <- data.frame(residuales_vec, hora_vec)
    
    if(input$decision == "Pronostico ARIMA"){
      prediccion <- prediccion*(1+input$perc/100)
      precio <- prediccion
    }
    
    
    
    df_final <- data.frame("Anterior" = dia$precio_mtr,"Anterior similar" = dia_1_sim$precio_mtr,"Promedio 4 similares" = prom_4_sim,"Promedio 2 similares" = prom_2_sim,"Promedio 2 anteriores" =prom_2_a,"Promedio semanal" = prom_sem, "Autoregresivo" = prediccion, "hora" = seq(1,24,1))
    #df_final <- data.frame("Anterior" = dia$precio_mtr,"Anterior similar" = dia_1_sim$precio_mtr,"Promedio 4 similares" = prom_4_sim,"Promedio 2 similares" = prom_2_sim,"Promedio 2 anteriores" =prom_2_a,"Promedio semanal" = prom_sem, "Autoregresivo" = prediccion, "Pronostico" = precio ,"hora" = seq(1,24,1))
    df_final <- df_final %>% gather(key = "serie", value = "precio", -c("hora"))
    
    
    list(df_final, precio, residuales)
    
    
  })  
  
  output$plot <- renderPlot({
    
    
    
    data.frame(df_final()[[1]]) %>% ggplot() + geom_line(aes(x = hora, y = as.numeric(precio), col = serie)) + xlab("Hora") + ylab("MXN/Mwh") + ggtitle("Precios observados")+
      theme(legend.position="bottom") 
    
  })
  
  output$TEST <- renderPlot({
    data.frame(df_final()[[1]]) %>% ggplot() + geom_smooth(aes(x = hora, y = as.numeric(precio), col = serie), method = 'loess') + xlab("Hora") + ylab("MXN/Mwh") + ggtitle("Interpolación de observados")+
      theme(legend.position="none") 
    
  })
  
  
  output$residuales <- renderPlot({
    data.frame(df_final()[[3]]) %>% ggplot(aes( sample = residuales_vec)) + facet_wrap(vars(hora_vec),scales = "free") + stat_qq() +
      xlab("Cuantil teórico") + ylab("Cuantil observado") + ggtitle("Comprobación de Residuales")
  })
  output$PreciosChidos <- downloadHandler(
    filename = function() {
      paste("Precios chidos chidos", ".csv", sep = "")
    },
    content = function(file) {
      
      
      precio <- data.frame(df_final()[[2]])
      names(precio) <- substr(input$node,3,5)
      write.csv(precio, file, row.names = FALSE)
    }
    ,
    contentType = "csv"
  )
  
  
  output$summary <- renderPrint({
    summary(spread(df_final()[[1]],serie,precio)[,-c(1)]  )
  })
  
  output$tabla <- renderTable({
    df_final()[[1]]
  })
  
  
  output$table <- renderDataTable({
    datatable(cars)
  })
}

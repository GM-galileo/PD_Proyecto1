library(shiny)
library(dplyr)
library(ggplot2)
library(reticulate)
library(DT)
library(maps)
library(mapproj)
source("helpers.R")
library(reshape2)
library(treemapify)

###  DATA ---------------------------------------------------------------
data <-  USArrests
data <- cbind(State= rownames(data), data)

rownames(data) <- 1:nrow(data)
data$porMurder <- (data$Murder)/data$UrbanPop
data$porRape <- (data$Rape)/data$UrbanPop
data$porAssault <- (data$Assault)/data$UrbanPop

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



###  SERVER -------------------------------------------------------------
shinyServer(function(input, output, session) {
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[['TipoCrimen']])) {
      
      updateTextInput(session, "TipoCrimen", value = query[['TipoCrimen']])
      if(query[['TipoCrimen']]=="Murder")
        updateRadioButtons(session, inputId = "Crime", selected = "Murder")
      
      if(query[['TipoCrimen']]=="Rape")
        updateRadioButtons(session, inputId = "Crime", selected = "Rape")
      
      if(query[['TipoCrimen']]=="Assault")
        updateRadioButtons(session, inputId = "Crime", selected = "Assault")
    }
  })
  
  observeEvent(input$btn_Limpiar, {
    updateSliderInput(inputId = "range", value = c(0,100))
  })

  ###--TAB - POBLACION -------------------------------------------------  
  
  ###--MAPA ------------------------------------------------------------
  output$USmap <- renderPlot({
    Mdata <- switch(input$Crimen, 
                    "UrbanPop" = USArrests$UrbanPop
    )
    
    color <- switch(input$Crimen, 
                    "UrbanPop" = "lightskyblue",
    )
    
    legend <- switch(input$Crimen, 
                     "UrbanPop" = "% Poblacion EEUU en 1973",
    )
    
    percent_map(Mdata, color, legend, input$range[1], input$range[2])
  })
  
  ###--TABLA ------------------------------------------------------------
  output$UStable = DT::renderDataTable({
    InfoTabla <- filter(USArrests, USArrests$UrbanPop>=input$range[1] & USArrests$UrbanPop<=input$range[2])
  })
  
  DatosSeleccion <- reactive({data.frame(filter(USArrests, USArrests$UrbanPop>=input$range[1] & USArrests$UrbanPop<=input$range[2])[input$UStable_rows_selected, ])})  
  
  output$select_table<-DT::renderDataTable({
    if(nrow(DatosSeleccion()) > 0) {
      df <- tibble::rownames_to_column(data.frame(state.x77), var = "State")
      df1 <- tibble::rownames_to_column(data.frame(transform(DatosSeleccion())), var = "State")

      v <- as.vector(df1$State,)
      print(df1[, "State", drop=FALSE])
      
      Info <- df[df$State %in%  v, ] 
    }
  })
  

  ###------------------------------------------------------------------------
  
  output$distPlot <- renderPlot({
    x1    <- input$Crime
    
    
      
    ggplot(data, aes( x = reorder(data$State,-as.numeric((factor(data[[input$Crime]])))) ,  y = factor(data[[input$Crime]])))+
      geom_bar( stat="identity"              )+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title = "Cantidad Crimenes por estado")+
      xlab("Estado")+
      ylab("Cantidad")+
      scale_colour_manual(values=cbp1)
   
    
  })
  
  output$TopdistPlot <- renderPlot({
    x1    <- input$Crime
    
    data %>% 
      mutate(valor=as.numeric(factor(data[[input$TipoCrimen]]))) %>% 
      arrange(desc(as.numeric(valor))) %>% 
      slice(1:10) %>% 
      ggplot(., aes( x = reorder(State, as.numeric(-valor)) ,  y = as.numeric(valor), fill=State))+
      geom_bar(stat="identity")+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title = "Top 10 Mayor Cantidad Crimenes por estado")+
      xlab("Estado")+
      ylab("Cantidad")+
      scale_colour_manual(values=cbp1)
    
  })
  
  output$TopNdistPlot <- renderPlot({
    x1    <- input$Crime
    
    data %>% 
      mutate(valor=as.numeric(factor(data[[input$TipoCrimen]]))) %>% 
      arrange(as.numeric(valor)) %>% 
      slice(1:10) %>% 
      ggplot(., aes( x = reorder(State, as.numeric(valor)) ,  y = as.numeric(valor), fill=State))+
      geom_bar(stat="identity")+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title = "Top Menor 10 Cantidad Crimenes por estado")+
      xlab("Estado")+
      ylab("Cantidad")
 
    
  })  
  
  
  output$TopdistPlotpor <- renderPlot({
    
    valorinput <- input$TipoCrimen
    data_por <- gsub(" ", "", paste("por",valorinput)) 
    data %>% 
      mutate(valor=as.numeric(factor(data[[data_por]]))) %>% 
      arrange(desc(as.numeric(valor))) %>% 
      slice(1:10) %>% 
      ggplot(., aes( x = reorder(State, as.numeric(-valor)) ,  y = as.numeric(valor), fill=State))+
      geom_bar(stat="identity")+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title = "Top Mayor 10 Cantidad Crimenes por estado % Poblacion")+
      xlab("Estado")+
      ylab("Cantidad")
    
  })  
  
  
  output$TopNdistPlotpor <- renderPlot({
    valorinput <- input$TipoCrimen
    data_por <- gsub(" ", "", paste("por",valorinput)) 
    
    data %>% 
      mutate(valor=as.numeric(factor(data[[data_por]]))) %>% 
      arrange(as.numeric(valor)) %>% 
      slice(1:10) %>% 
      ggplot(., aes( x = reorder(State, as.numeric(valor)) ,  y = as.numeric(valor), fill=State))+
      geom_bar(stat="identity")+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title = "Top Menor 10 Cantidad Crimenes por estado % Poblacion")+
      xlab("Estado")+
      ylab("Cantidad")
    
  })  
  
  output$EstadodistPlot <- renderPlot({
    Val_estado <- input$Estado
    df <- melt(data, id.vars='State')
    df<-df[(df$variable=="Assault" | df$variable=="Rape" | df$variable=="Murder"),]
    df %>% 
      filter(State==Val_estado) %>% 
      ggplot(., aes( x = variable ,  y = value, fill=variable))+
      geom_bar(stat="identity")+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title = "Resumen por Estado")+
      xlab(cat("Estado:" ,Val_estado))+
      ylab("Cantidad")+
      coord_flip()+
      theme_minimal()
    
  })
  
  output$TreePlot <- renderPlot({
    x1    <- input$TipoCrimen
    
    ggplot(data, aes( fill = data$State,  area = as.numeric(factor(data[[input$TipoCrimen]]))), label=data$State)+
      geom_treemap()+
      labs(title = "Participacion por Estado")+
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 15,
                        label=data$State)
    
  })
  ###------------------------------------------------------------------------
  
})
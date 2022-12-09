############################
#   Contenido del server   # 
############################


server <- function(input, output, session) {
  
  
  ############################################################
  #         Análisis de las transacciones en la CCSS         #
  ############################################################
  
  #################################################
  #        Presentación de las transferencias     #
  #################################################
  
  
#  output$picture.ingresos <- renderImage({
#    return(list(src = "C:/Users/regla.fiscal/App.R/Ingresos/Dashboard/Scripts_tablas/www/ingresos.png",contentType = "image/png",alt = "R"))
#  }, 
#  deleteFile = FALSE
#  )
  

  ##############################################
  #         Indicadores de los ingresos        #
  ##############################################
  
    output$tabla.indicadores <- renderReactable({
      
      Indicadores <- Indicadores
      

      reactable(Indicadores)
      
              
    })

    #################################################
    #         Evolución de las transferencias       #
    #################################################
    
    #############################
    # Acumuladas - anuales      #
    #############################
    
    #############
    #  General  #
    #############
    
    output$E_A_1 <- renderHighchart({
      
      CCSS_general_total <- CCSS_general_total
      

      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_general_total$Año) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_general_total$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    })
    
    #############
    #  Título   #
    #############
    
    output$E_A_2 <- renderHighchart({
      
      CCSS_titulo_total <- CCSS_titulo_total %>%
                           dplyr::filter(`Título`==input$clasi.E.A.titulo)
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_titulo_total$Año) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_titulo_total$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    })
    
    ##############
    #  Partida   #
    ##############
  
    output$E_A_3 <- renderHighchart({
      
      CCSS_Partida_total <- CCSS_Partida_total %>%
        dplyr::filter(`Partida`==input$clasi.E.A.partida)
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_Partida_total$Año) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_Partida_total$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    })
    
    ##################
    #  Contribución  #
    ##################
    
    output$E_A_4 <- renderHighchart({
      
      CCSS_contribucion_total <- CCSS_contribucion_total %>%
                       dplyr::filter(`Contribución`==input$clasi.E.A.contribucion)
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_contribucion_total$Año) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_contribucion_total$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    }) 
    
    ##################
    #  Regimen       #
    ##################
    
    output$E_A_5 <- renderHighchart({
      
      CCSS_regimen_total <- CCSS_regimen_total %>%
                     dplyr::filter(`Regimen`==input$clasi.E.A.regimen)
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_regimen_total$Año) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_regimen_total$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    }) 
    
    output$E_A_6 <- renderHighchart({
      
      CCSS_detalle_total <- CCSS_detalle_total %>%
        dplyr::filter(`Detalle`==input$clasi.E.A.detalle)
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_detalle_total$Año) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_detalle_total$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    }) 
    
    ##################
    #    Detalle     #
    ##################
    
    #############################
    #         Mensuales         #
    #############################
    
    #############
    #  General  #
    #############
    
    output$E_M_1 <- renderHighchart({
      
      CCSS_general_mensual <- CCSS_general_mensual  %>%  
                                                      dplyr::mutate(
                                                                     Fecha = paste(Año,mes,sep ="-")
                                      )
      
      
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_general_mensual$Fecha) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_general_mensual$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    })
    
    #############
    #  Título   #
    #############
    
    output$E_M_2 <- renderHighchart({
      
      CCSS_titulo_mensual <- CCSS_titulo_mensual %>%
                                        dplyr::filter(`Título`==input$clasi.E.M.titulo) %>%  
                                                                                        dplyr::mutate(
                                                                                          Fecha = paste(Año,mes,sep ="-")
                                                                                        )
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_titulo_mensual$Fecha) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_titulo_mensual$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    })
    
    ##############
    #  Partida   #
    ##############
    
    output$E_M_3 <- renderHighchart({
      
      CCSS_Partida_mensual <- CCSS_Partida_mensual %>%
        dplyr::filter(`Partida`==input$clasi.E.M.partida) %>%  
        dplyr::mutate(
          Fecha = paste(Año,mes,sep ="-")
        )
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_Partida_mensual$Fecha) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_Partida_mensual$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    })
    
    ##################
    #  Contribución  #
    ##################
    
    output$E_M_4 <- renderHighchart({
      
      CCSS_contribucion_mensual <- CCSS_contribucion_mensual %>%
        dplyr::filter(`Contribución`==input$clasi.E.M.contribucion) %>%  
        dplyr::mutate(
          Fecha = paste(Año,mes,sep ="-")
        )
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_contribucion_mensual$Fecha) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_contribucion_mensual$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    })
  
    
    ##################
    #  Regimen       #
    ##################
    
    output$E_M_5 <- renderHighchart({
      
      CCSS_regimen_mensual <- CCSS_regimen_mensual %>%
        dplyr::filter(`Regimen`==input$clasi.E.M.regimen) %>%  
        dplyr::mutate(
          Fecha = paste(Año,mes,sep ="-")
        )
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_regimen_mensual$Fecha) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_regimen_mensual$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    })
    
    ##################
    #    Detalle     #
    ##################
    
    output$E_M_6 <- renderHighchart({
      
      CCSS_detalle_mensual <- CCSS_detalle_mensual %>%
        dplyr::filter(`Detalle`==input$clasi.E.M.detalle) %>%  
        dplyr::mutate(
          Fecha = paste(Año,mes,sep ="-")
        )
      
      Transferencias  <- highchart() %>% 
        hc_title(text = "",
                 margin = 20, align = "center",
                 style = list(color = "#129", useHTML = TRUE)) %>% 
        hc_subtitle(text = "",
                    align = "right",
                    style = list(color = "#634", fontWeight = "bold")) %>%
        hc_credits(enabled = TRUE, # add credits
                   text = "" # href = "www.cgr.go.cr"
        ) %>%
        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical", x = 0, y = 100) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5) %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_xAxis(categories = CCSS_detalle_mensual$Fecha) %>% 
        hc_add_series(name = "Transferencias", data = CCSS_detalle_mensual$`Transferencia devengado`) %>% 
        hc_chart(zoomType = "xy")
      
      Transferencias
      
    }) 
    
}





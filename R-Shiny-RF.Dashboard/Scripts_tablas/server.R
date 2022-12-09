############################
#   Contenido del server   # 
############################


server <- function(input, output, session) {
  
  ##############################################
  #         Presentation de la RF 2020         #
  ##############################################
  
  
  
  ##############################################
  #   Graficos de los tipos de presupuestos    #
  ##############################################
  
  output$picture <- renderImage({
    return(list(src = "C:/Users/oscar/Desktop/Regla fiscal/RF --- REVOLUTION/RF_WEB/Scripts_tablas/www/ppt-rf.png",contentType = "image/png",alt = "R"))
  }, 
  deleteFile = FALSE
  )
  
  
  ##############################
  #  Presupuesto GC - anuales  #
  ##############################
  
  ###############
  #  Nominales  #
  ###############
  
  output$HCGC_A0 <- renderHighchart({
    
    Egresos_GC_GC_anual <- tabla_0_T
    
    Egresos_GC_GC_anual <- Egresos_GC_GC_anual %>%
      mutate(
        `Inicial` = round(Inicial/Millones,1),
        `Ajustado`= round(Ajustado/Millones,1),
        `Ejecutado` = round(Ejecutado/Millones,1)
      ) 
    
    Egresos_GC_GC_anual  <- highchart() %>% 
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
      hc_xAxis(categories = Egresos_GC_GC_anual$Año) %>% 
      hc_add_series(name = "Inicial", data = Egresos_GC_GC_anual$Inicial) %>% 
      hc_add_series(name = "Ajustado", data = Egresos_GC_GC_anual$Ajustado) %>% 
      hc_add_series(name = "Ejecutado", data = Egresos_GC_GC_anual$Ejecutado, color="red") %>% 
      hc_chart(zoomType = "xy")
    
    Egresos_GC_GC_anual
    
  })
  
  
  output$HCGC_A1 <- renderHighchart({
    
    Egresos_GC_GC_anual <- tabla_0_F %>%
      filter(`CLA ECO 1` == input$eco1.1) 
    
    Egresos_GC_GC_anual <- Egresos_GC_GC_anual %>%
      mutate(
        `Inicial` = round(Inicial/Millones,1),
        `Ajustado`= round(Ajustado/Millones,1),
        `Ejecutado` = round(Ejecutado/Millones,1)
      ) 
    
    Egresos_GC_GC_anual  <- highchart() %>% 
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
      hc_xAxis(categories = Egresos_GC_GC_anual$Año) %>% 
      hc_add_series(name = "Inicial", data = Egresos_GC_GC_anual$Inicial) %>% 
      hc_add_series(name = "Ajustado", data = Egresos_GC_GC_anual$Ajustado) %>% 
      hc_add_series(name = "Ejecutado", data = Egresos_GC_GC_anual$Ejecutado, color="red" ) %>% 
      hc_chart(zoomType = "xy")
    
    Egresos_GC_GC_anual
    
  })
  
  #  
  ####################
  #  %_Varianciones  #
  ####################
  
  output$HCGC_A2 <- renderHighchart({
    
    
    Egresos_GC_GC_anual_variacion <- tabla_0_F %>%
      filter(`CLA ECO 1` == input$eco1.1)
    
    Egresos_GC_GC_anual_variacion <-  tabla_0_F %>%
      filter(`CLA ECO 1` == input$eco1.1) %>%
      mutate(
        var.Inicial  = round((Inicial/lag(Inicial)-1)*100,1), 
        var.Ajustado = round((Ajustado/lag(Ajustado)-1)*100,1),
        var.Ejecutado = round((Ejecutado/lag(Ejecutado)-1)*100,1)
        
      )
    
    Egresos_GC_GC_anual_variacion  <- highchart() %>% 
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
      hc_xAxis(categories = Egresos_GC_GC_anual_variacion$Año) %>% 
      hc_add_series(name = "Inicial", data = round(Egresos_GC_GC_anual_variacion$var.Inicial,1)) %>% 
      hc_add_series(name = "Ajustado", data = round(Egresos_GC_GC_anual_variacion$var.Ajustado,1)) %>% 
      hc_add_series(name = "Ejecutado", data = round(Egresos_GC_GC_anual_variacion$var.Ejecutado,1), color="red")  %>% 
      hc_chart(zoomType = "xy")
    
    Egresos_GC_GC_anual_variacion
    
  })
  
  ################################
  #  Presupuesto GC - Mensuales  #
  ################################
  
  ###############
  #  Nominales  #
  ###############
  
  output$HCGC_B0 <- renderHighchart({
    
    Egresos_GC_GC_mensual <- tabla_2_T
    
    Egresos_GC_GC_mensual <- Egresos_GC_GC_mensual %>%
      dplyr::mutate(
        Ejecutado = round(Ejecutado/Millones,1)
      )
    
    Egresos_GC_GC_mensual  <- highchart() %>% 
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
      hc_exporting(enabled = TRUE) %>% 
      hc_xAxis(categories = Egresos_GC_GC_mensual$Fecha) %>% 
      hc_add_series(name = "Ejecutado", data = Egresos_GC_GC_mensual$Ejecutado) %>% 
      hc_chart(zoomType = "xy")
    
    Egresos_GC_GC_mensual
    
  })
  
  output$HCGC_B1 <- renderHighchart({
    
    Egresos_GC_GC_mensual <- tabla_2_F %>%
      filter(CE1 == input$eco1.2) 
    
    
    Egresos_GC_GC_mensual <- Egresos_GC_GC_mensual %>%
      dplyr::mutate(
        Ejecutado = round(Ejecutado/Millones,1)
      )
    
    Egresos_GC_GC_mensual  <- highchart() %>% 
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
      hc_exporting(enabled = TRUE) %>% 
      hc_xAxis(categories = Egresos_GC_GC_mensual$Fecha) %>% 
      hc_add_series(name = "Ejecutado", data = Egresos_GC_GC_mensual$Ejecutado) %>% 
      hc_chart(zoomType = "xy")
    
    Egresos_GC_GC_mensual
    
  })
  
  
  ####################
  #  %_Varianciones  #
  ####################
  
  output$HCGC_B2 <- renderHighchart({
    
    Egresos_GC_GC_mensual <- tabla_2_F %>%
      filter(CE1 == input$eco1.2)
    
    
    Egresos_GC_GC_mensual_variacion <-  mutate(Egresos_GC_GC_mensual,
                                               var.Ejecutado = round((Ejecutado/lag(Ejecutado,n = 12)-1)*100,1),
                                               Fecha = paste(Año,mes,sep ="-")
    )
    
    Egresos_GC_GC_mensual_variacion <- Egresos_GC_GC_mensual_variacion %>%
      mutate(
        acum_12 = roll_sum(Egresos_GC_GC_mensual_variacion$Ejecutado, 12, align = "right", fill = NA),
        var.acum_12  = round((acum_12/lag(acum_12,n = 12)-1)*100,1)
      )
    
    Egresos_GC_GC_mensual_variacion  <- highchart() %>% 
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
      hc_exporting(enabled = TRUE) %>% 
      hc_xAxis(categories = Egresos_GC_GC_mensual_variacion$Fecha) %>% 
      hc_add_series(name = "Var.Ejecutado.%", data = Egresos_GC_GC_mensual_variacion$var.Ejecutado) %>% 
      hc_add_series(name = "var.acum_12.%", data = Egresos_GC_GC_mensual_variacion$var.acum_12) %>% 
      hc_chart(zoomType = "xy")
    
    Egresos_GC_GC_mensual_variacion
    
  })
  
  #################################
  #  Presupuesto Titulo - anuales #
  #################################
  
  ###############
  #  Nominales  #
  ###############
  
  
  output$HCGC_C1 <- renderHighchart({
    
    Egresos_GC_GC_anual <- tabla_3_F   %>%
      filter(`Título` == input$titulo.1) %>%  
      filter(`CLA ECO 1` == input$eco1.3)
    
    Egresos_GC_GC_anual <-  Egresos_GC_GC_anual %>%
      mutate(
        `Inicial` = round(Inicial/Millones,1),
        `Ajustado`= round(Ajustado/Millones,1),
        `Ejecutado` = round(Ejecutado/Millones,1)
      )  
    
    
    Egresos_GC_GC_anual  <- highchart() %>% 
      hc_title(text = "",
               margin = 20, align = "center",
               style = list(color = "#129", useHTML = TRUE)) %>% 
      hc_subtitle(text = paste(""),
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
      hc_xAxis(categories = Egresos_GC_GC_anual$Año) %>% 
      hc_add_series(name = "Inicial", data = Egresos_GC_GC_anual$Inicial) %>% 
      hc_add_series(name = "Ajustado", data = Egresos_GC_GC_anual$Ajustado) %>% 
      hc_add_series(name = "Ejecutado", data = Egresos_GC_GC_anual$Ejecutado, color="red")  %>% 
      hc_chart(zoomType = "xy")
    
    Egresos_GC_GC_anual
    
  })
  
  ####################
  #  %_Varianciones  #
  ####################
  
  output$HCGC_C2 <- renderHighchart({
    
    Egresos_GC_GC_anual <- tabla_3_F   %>%
      filter(`Título` == input$titulo.1) %>%  
      filter(`CLA ECO 1` == input$eco1.3)
    
    Egresos_GC_GC_anual_variacion <- mutate(Egresos_GC_GC_anual,
                                            var.Inicial  = (Inicial/lag(Inicial)-1)*100, 
                                            var.Ajustado = (Ajustado/lag(Ajustado)-1)*100,
                                            var.Ejecutado = (Ejecutado/lag(Ejecutado)-1)*100
                                            
    )
    
    Egresos_GC_GC_anual_variacion  <- highchart() %>% 
      hc_title(text = "",
               margin = 20, align = "center",
               style = list(color = "#129", useHTML = TRUE)) %>% 
      hc_subtitle(text = paste(""),
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
      hc_xAxis(categories = Egresos_GC_GC_anual$Año) %>% 
      hc_add_series(name = "Inicial", data = round(Egresos_GC_GC_anual_variacion$var.Inicial,1)) %>% 
      hc_add_series(name = "Ajustado", data = round(Egresos_GC_GC_anual_variacion$var.Ajustado,1)) %>% 
      hc_add_series(name = "Ejecutado", data = round(Egresos_GC_GC_anual_variacion$var.Ejecutado,1), color="red")  %>% 
      hc_chart(zoomType = "xy")
    
    Egresos_GC_GC_anual_variacion
    
  })
  
  
  ###################################
  #  Presupuesto Titulo - Mensuales #
  ###################################
  
  ###############
  #  Nominales  #
  ###############
  
  output$HCGC_D1 <- renderHighchart({
    
    Egresos_GC_GC_mensual <- tabla_4_F %>%
      filter(`Título` == input$titulo.2) %>%  
      filter(CE1 == input$eco1.4)  
    
    
    Egresos_GC_GC_mensual <- Egresos_GC_GC_mensual %>%
      mutate(
        Ejecutado = round(Ejecutado/Millones,1)
      )
    
    Egresos_GC_GC_mensual  <- highchart() %>% 
      hc_title(text = "",
               margin = 20, align = "center",
               style = list(color = "#129", useHTML = TRUE)) %>% 
      hc_subtitle(text = paste(""),
                  align = "right",
                  style = list(color = "#634", fontWeight = "bold")) %>%
      hc_credits(enabled = TRUE, # add credits
                 text = "" # href = "www.cgr.go.cr"
      ) %>%
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_xAxis(categories = Egresos_GC_GC_mensual$Fecha) %>% 
      hc_add_series(name = "Ejecutado", data = Egresos_GC_GC_mensual$Ejecutado) %>% 
      hc_chart(zoomType = "xy")
    
    Egresos_GC_GC_mensual
    
  })
  
  
  ####################
  #  %_Varianciones  #
  ####################
  
  output$HCGC_D2 <- renderHighchart({
    
    Egresos_GC_GC_mensual <- tabla_4_F %>%
      filter(`Título` == input$titulo.2) %>%  
      filter(CE1 == input$eco1.4)  
    
    Egresos_GC_GC_mensual_variacion <-  mutate(Egresos_GC_GC_mensual,
                                               var.Ejecutado = round((Ejecutado/lag(Ejecutado,n = 12)-1)*100,1),
                                               Fecha = paste(Año,mes,sep ="-")
                                               
    )
    
    Egresos_GC_GC_mensual_variacion <- Egresos_GC_GC_mensual_variacion %>%
      mutate(
        acum_12 = roll_sum(Egresos_GC_GC_mensual_variacion$Ejecutado, 12, align = "right", fill = NA),
        var.acum_12  = round((acum_12/lag(acum_12,n = 12)-1)*100,1)
      )
    
    
    Egresos_GC_GC_mensual_variacion  <- highchart() %>% 
      hc_title(text = "",
               margin = 20, align = "center",
               style = list(color = "#129", useHTML = TRUE)) %>% 
      hc_subtitle(text = paste(""),
                  align = "right",
                  style = list(color = "#634", fontWeight = "bold")) %>%
      hc_credits(enabled = TRUE, # add credits
                 text = "" # href = "www.cgr.go.cr"
      ) %>%
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_xAxis(categories = Egresos_GC_GC_mensual_variacion$Fecha) %>% 
      hc_add_series(name = "Var.Ejecutado.%", data = Egresos_GC_GC_mensual_variacion$var.Ejecutado) %>% 
      hc_add_series(name = "var.acum_12.%", data = Egresos_GC_GC_mensual_variacion$var.acum_12) %>% 
      hc_chart(zoomType = "xy")
    
    Egresos_GC_GC_mensual_variacion
    
  })
  
  ###########################################################
  #            Analisis por clasificador                    #
  ###########################################################
  
  ######################
  #      TOTAL COG     #
  ######################
  
  output$COG_TOTAL_A_G <- renderHighchart({
    
    COG_T_A <- tabla.10.1
    
    COG_T_A  <- highchart() %>% 
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
      hc_xAxis(categories = COG_T_A$Año) %>% 
      hc_add_series(name = "Remuneraciones", data = COG_T_A$`Remuneraciones`) %>% 
      hc_add_series(name = "Servicios", data = COG_T_A$`Servicios`) %>% 
      hc_add_series(name = "Materiales y suministros", data = COG_T_A$`Materiales y suministros`) %>% 
      hc_add_series(name = "Intereses y comisiones", data = COG_T_A$`Intereses y comisiones`) %>%
      hc_add_series(name = "Activos financieros", data = COG_T_A$`Activos financieros`) %>%
      hc_add_series(name = "Bienes duraderos", data = COG_T_A$`Bienes duraderos`) %>%
      hc_add_series(name = "Transferencias corrientes", data = COG_T_A$ `Transferencias corrientes`) %>%
      hc_add_series(name = "Transferencias de capital", data = COG_T_A$`Transferencias de capital`) %>%
      hc_add_series(name = "Amortización", data = COG_T_A$`Amortización`) %>%
      hc_add_series(name = "Cuentas especiales", data = COG_T_A$`Cuentas especiales`) %>% 
      hc_chart(zoomType = "xy")
    
    COG_T_A 
    
  })
  
  output$COG_TOTAL_M_G <- renderHighchart({
    
    COG_T_A <- tabla.10.2
    
    COG_T_A  <- highchart() %>% 
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
      hc_xAxis(categories = COG_T_A$Fecha) %>% 
      hc_add_series(name = "Remuneraciones", data = COG_T_A$`Remuneraciones`) %>% 
      hc_add_series(name = "Servicios", data = COG_T_A$`Servicios`) %>% 
      hc_add_series(name = "Materiales y suministros", data = COG_T_A$`Materiales y suministros`) %>% 
      hc_add_series(name = "Intereses y comisiones", data = COG_T_A$`Intereses y comisiones`) %>%
      hc_add_series(name = "Activos financieros", data = COG_T_A$`Activos financieros`) %>%
      hc_add_series(name = "Bienes duraderos", data = COG_T_A$`Bienes duraderos`) %>%
      hc_add_series(name = "Transferencias corrientes", data = COG_T_A$`Transferencias corrientes`) %>%
      hc_add_series(name = "Transferencias de capital", data = COG_T_A$`Transferencias de capital`) %>%
      hc_add_series(name = "Amortización", data = COG_T_A$`Amortización`) %>%
      hc_add_series(name = "Cuentas especiales", data = COG_T_A$`Cuentas especiales`) %>% 
      hc_chart(zoomType = "xy")
    
    COG_T_A 
    
  })
  
  output$COG_TOTAL_M_G_VAR <- renderHighchart({
    
    COG_T_A <- tabla.10.2  
    
    COG_T_A <- mutate(COG_T_A,
                      var.Remuneraciones = round((Remuneraciones/lag(Remuneraciones,n = 12)-1)*100,1),
                      var.Servicios = round((`Servicios`/lag(`Servicios`,n = 12)-1)*100,1),
                      var.Materiales.suministros = round((`Materiales y suministros`/lag(`Materiales y suministros`,n = 12)-1)*100,1),
                      var.Bienes.duraderos = round((`Bienes duraderos`/lag(`Bienes duraderos`,n = 12)-1)*100,1),
                      var.Transferencias.corrientes = round((`Transferencias corrientes`/lag(`Transferencias corrientes`,n = 12)-1)*100,1),
                      var.Transferencias.capital = round((`Transferencias de capital`/lag(`Transferencias de capital`,n = 12)-1)*100,1),
                      var.Intereses.comisiones = round((`Intereses y comisiones`/lag(`Intereses y comisiones`,n = 12)-1)*100,1),
                      var.Amortización = round((`Amortización`/lag(`Amortización`,n = 12)-1)*100,1),
                      var.Activos.financieros = round((`Activos financieros`/lag(`Activos financieros`,n = 12)-1)*100,1)
                      
    )
    
    COG_T_A <- mutate(COG_T_A, 
                      acum_12.Remuneraciones = roll_sum(COG_T_A$Remuneraciones, 12, align = "right", fill = NA),
                      var.acum.Remuneraciones  = round((acum_12.Remuneraciones/lag(acum_12.Remuneraciones,n = 12)-1)*100,1),
                      
                      acum_12.Servicios = roll_sum(COG_T_A$Servicios, 12, align = "right", fill = NA),
                      var.acum.Servicios  = round((acum_12.Servicios/lag(acum_12.Servicios,n = 12)-1)*100,1),
                      
                      acum_12.Materiales.suministros = roll_sum(COG_T_A$`Materiales y suministros`, 12, align = "right", fill = NA),
                      var.acum.Materiales.suministros  = round((acum_12.Materiales.suministros/lag(acum_12.Materiales.suministros,n = 12)-1)*100,1), 
                      
                      acum_12.Bienes.duraderos = roll_sum(COG_T_A$`Bienes duraderos`, 12, align = "right", fill = NA),
                      var.acum.Bienes.duraderos  = round((acum_12.Bienes.duraderos/lag(acum_12.Bienes.duraderos,n = 12)-1)*100,1),
                      
                      acum_12.Transferencias.corrientes = roll_sum(COG_T_A$`Transferencias corrientes`, 12, align = "right", fill = NA),
                      var.acum.Transferencias.corrientes  = round((acum_12.Transferencias.corrientes/lag(acum_12.Transferencias.corrientes,n = 12)-1)*100,1),
                      
                      acum_12.Transferencias.capital = roll_sum(COG_T_A$`Transferencias de capital`, 12, align = "right", fill = NA),
                      var.acum.Transferencias.capital = round((acum_12.Transferencias.capital/lag(acum_12.Transferencias.capital,n = 12)-1)*100,1),
                      
                      acum_12.Intereses.comisiones = roll_sum(COG_T_A$`Intereses y comisiones`, 12, align = "right", fill = NA),
                      var.acum.Intereses.comisiones = round((acum_12.Intereses.comisiones/lag(acum_12.Intereses.comisiones,n = 12)-1)*100,1),
                      
                      acum_12.Amortización = roll_sum(COG_T_A$`Amortización`, 12, align = "right", fill = NA),
                      var.acum.Amortización = round((acum_12.Amortización/lag(acum_12.Amortización,n = 12)-1)*100,1),
                      
                      acum_12.Activos.financieros = roll_sum(COG_T_A$`Activos financieros`, 12, align = "right", fill = NA),
                      var.acum.Activos.financieros = round((acum_12.Activos.financieros/lag(acum_12.Activos.financieros,n = 12)-1)*100,1),
                      
    )           
    
    COG_T_A  <- highchart() %>% 
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
      hc_xAxis(categories = COG_T_A$Fecha) %>% 
      hc_add_series(name = "var.Remuneraciones", data = COG_T_A$`var.Remuneraciones`) %>% 
      hc_add_series(name = "var.acum.Remuneraciones", data = COG_T_A$`var.acum.Remuneraciones`) %>% 
      
      hc_add_series(name = "var.Servicios", data = COG_T_A$`var.Servicios`) %>% 
      hc_add_series(name = "var.acum.Servicios", data = COG_T_A$`var.acum.Servicios`) %>% 
      
      hc_add_series(name = "var.Materiales.suministros", data = COG_T_A$`var.Materiales.suministros`) %>% 
      hc_add_series(name = "var.acum.Materiales.suministros", data = COG_T_A$`var.acum.Materiales.suministros`) %>% 
      
      hc_add_series(name = "var.Bienes.duraderos", data = COG_T_A$`var.Bienes.duraderos`) %>% 
      hc_add_series(name = "var.acum.Bienes.duraderos", data = COG_T_A$`var.acum.Bienes.duraderos`) %>% 
      
      hc_add_series(name = "var.Transferencias.corrientes", data = COG_T_A$`var.Transferencias.corrientes`) %>% 
      hc_add_series(name = "var.acum.Transferencias.corrientes", data = COG_T_A$`var.acum.Transferencias.corrientes`) %>%
      
      hc_add_series(name = "var.Transferencias.capital", data = COG_T_A$`var.Transferencias.capital`) %>% 
      hc_add_series(name = "var.acum.Transferencias.capital", data = COG_T_A$`var.acum.Transferencias.capital`) %>%
      
      hc_add_series(name = "var.Intereses.comisiones", data = COG_T_A$`var.Intereses.comisiones`) %>% 
      hc_add_series(name = "var.acum.Intereses.comisiones", data = COG_T_A$`var.acum.Intereses.comisiones`) %>%
      
      hc_add_series(name = "var.Amortización", data = COG_T_A$`var.Amortización`) %>% 
      hc_add_series(name = "var.acum.Amortización", data = COG_T_A$`var.acum.Amortización`) %>%
      
      hc_add_series(name = "var.Activos.financieros", data = COG_T_A$`var.Activos.financieros`) %>% 
      hc_add_series(name = "var.acum.Activos.financieros", data = COG_T_A$`var.acum.Activos.financieros`) %>%
      
      hc_chart(zoomType = "xy")
    
    COG_T_A 
    
    
  })
  
  
  ############################
  #      General ECO.2       #
  ############################
  
  # Gasto anual
  
  output$ECO2_TOTAL_AG_CLA <- renderHighchart({
    
    ECO_T_A <- tabla.11.1 
    
    ECO_T_A  <- highchart() %>% 
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
      hc_xAxis(categories = ECO_T_A$Año) %>% 
      hc_add_series(name = "Gastos de consumo", data = ECO_T_A$`Gastos de consumo`) %>% 
      hc_add_series(name = "Intereses", data = ECO_T_A$`Intereses`) %>%
      hc_add_series(name = "Transferencias corrientes", data = ECO_T_A$`Transferencias corrientes`) %>% 
      hc_add_series(name = "Formación de capital", data = ECO_T_A$`Formación de capital`) %>%
      hc_add_series(name = "Adquisición de activos", data = ECO_T_A$`Adquisición de activos`) %>%
      hc_add_series(name = "Transferencias de capital", data = ECO_T_A$`Transferencias de capital`) %>%
      hc_add_series(name = "Sumas sin asignación", data = ECO_T_A$`Sumas sin asignación`) %>%
      hc_add_series(name = "Adquisición de valores", data = ECO_T_A$`Adquisición de valores`) %>%
  #    hc_add_series(name = "Amortización", data = ECO_T_A$"Amortización ") %>%
      hc_add_series(name = "Otros activos financieros", data = ECO_T_A$`Otros activos financieros`) %>%
      hc_chart(zoomType = "xy")
    
    ECO_T_A 
    
  })
  
  # Gasto Mensual
  
  output$ECO2_TOTAL_MG_CLA <- renderHighchart({
    
    ECO_T_M <- tabla.11.2
    
    ECO_T_M  <- highchart() %>% 
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
      hc_xAxis(categories = ECO_T_M$Año) %>% 
      hc_add_series(name = "Gastos de consumo", data = ECO_T_M$`Gastos de consumo`) %>% 
      hc_add_series(name = "Intereses", data = ECO_T_M$`Intereses`) %>%
      hc_add_series(name = "Transferencias corrientes", data = ECO_T_M$`Transferencias corrientes`) %>% 
      hc_add_series(name = "Formación de capital", data = ECO_T_M$`Formación de capital`) %>%
      hc_add_series(name = "Adquisición de activos", data = ECO_T_M$`Adquisición de activos`) %>%
      hc_add_series(name = "Transferencias de capital", data = ECO_T_M$`Transferencias de capital`) %>%
      hc_add_series(name = "Sumas sin asignación", data = ECO_T_M$`Sumas sin asignación`) %>%
      hc_add_series(name = "Adquisición de valores", data = ECO_T_M$`Adquisición de valores`) %>%
  #    hc_add_series(name = "Amortización", data = ECO_T_M$"Amortización ") %>%
      hc_add_series(name = "Otros activos financieros", data = ECO_T_M$`Otros activos financieros`) %>%
      hc_chart(zoomType = "xy")
    
    ECO_T_M 
    
  })
  
  # Varianción Gasto
  
  output$ECO2_TOTAL_MG_VAR <- renderHighchart({
    
    ECO_T_M <- tabla.11.2
    
    ECO_T_M <- mutate(ECO_T_M,
                      var.GastosConsumo = round((`Gastos de consumo`/lag(`Gastos de consumo`,n = 12)-1)*100,1),
                      var.TransferenciasCorrientes = round((`Transferencias corrientes`/lag(`Transferencias corrientes`,n = 12)-1)*100,1),
                      var.Intereses = round((`Intereses`/lag(`Intereses`,n = 12)-1)*100,1),
                      var.Formacion.Capital = round((`Formación de capital`/lag(`Formación de capital`,n = 12)-1)*100,1),
                      var.Transferencias.Capital = round((`Transferencias de capital`/lag(`Transferencias de capital`,n = 12)-1)*100,1),
                      var.Adquisición.Activos = round((`Adquisición de activos`/lag(`Adquisición de activos`,n = 12)-1)*100,1),
         #             var.Amortización = round((`Amortización`/lag(`Amortización`,n = 12)-1)*100,1),
                      var.Adquisición.Valores = round((`Adquisición de valores`/lag(`Adquisición de valores`,n = 12)-1)*100,1),
                      var.Otros.AF = round((`Otros activos financieros`/lag(`Otros activos financieros`,n = 12)-1)*100,1)
    )
    
    ECO_T_M <- mutate(ECO_T_M, 
                      acum_12.GastosConsumo = roll_sum(ECO_T_M$`Gastos de consumo`, 12, align = "right", fill = NA),
                      var.acum.GastosConsumo  = round((acum_12.GastosConsumo/lag(acum_12.GastosConsumo,n = 12)-1)*100,1),
                      
                      acum_12.TransferenciasCorrientes = roll_sum(ECO_T_M$`Transferencias corrientes`, 12, align = "right", fill = NA),
                      var.acum.TransferenciasCorrientes  = round((acum_12.TransferenciasCorrientes/lag(acum_12.TransferenciasCorrientes,n = 12)-1)*100,1),
                      
                      acum_12.Intereses = roll_sum(ECO_T_M$`Intereses`, 12, align = "right", fill = NA),
                      var.acum.Intereses  = round((acum_12.Intereses/lag(acum_12.Intereses,n = 12)-1)*100,1),
                      
                      acum_12.Formacion.Capital = roll_sum(ECO_T_M$`Formación de capital`, 12, align = "right", fill = NA),
                      var.acum.Formacion.Capital  = round((acum_12.Formacion.Capital/lag(acum_12.Formacion.Capital,n = 12)-1)*100,1),
                      
                      var.Transferencias.Capital = roll_sum(ECO_T_M$`Transferencias de capital`, 12, align = "right", fill = NA),
                      var.acum.Transferencias.Capital  = round((var.Transferencias.Capital/lag(var.Transferencias.Capital,n = 12)-1)*100,1),
                      
                      var.Adquisición.Activos = roll_sum(ECO_T_M$`Adquisición de valores`, 12, align = "right", fill = NA),
                      var.acum.Adquisición.Activos  = round((var.Adquisición.Activos/lag(var.Adquisición.Activos,n = 12)-1)*100,1),
                      
             #        var.Amortización = roll_sum(ECO_T_M$`Amortización `, 12, align = "right", fill = NA),
             #        var.acum.Amortización  = round((var.Amortización/lag(var.Amortización,n = 12)-1)*100,1),
                      
                      var.Adquisición.Valores = roll_sum(ECO_T_M$`Adquisición de valores`, 12, align = "right", fill = NA),
                      var.acum.Adquisición.Valores  = round((var.Adquisición.Valores/lag(var.Adquisición.Valores,n = 12)-1)*100,1),
                      
                      var.Otros.AF = roll_sum(ECO_T_M$`Otros activos financieros`, 12, align = "right", fill = NA),
                      var.acum.Otros.AF  = round((var.Otros.AF/lag(var.Otros.AF,n = 12)-1)*100,1)
    )
    
    
    ECO_T_M  <- highchart() %>% 
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
      
      hc_xAxis(categories = ECO_T_M$Fecha) %>% 
      
      hc_add_series(name = "var.GastosConsumo",      data = ECO_T_M$`var.GastosConsumo`) %>% 
      hc_add_series(name = "var.acum.GastosConsumo", data = ECO_T_M$`var.acum.GastosConsumo`) %>% 
      
      hc_add_series(name = "var.TransferenciasCorrientes", data = ECO_T_M$`var.TransferenciasCorrientes`) %>% 
      hc_add_series(name = "var.acum.TransferenciasCorrientes", data = ECO_T_M$`var.acum.TransferenciasCorrientes`) %>% 
      
      hc_add_series(name = "var.Intereses", data = ECO_T_M$`var.Intereses`) %>% 
      hc_add_series(name = "var.acum.Intereses", data = ECO_T_M$`var.acum.Intereses`) %>% 
      
      hc_add_series(name = "var.Formacion.Capital", data = ECO_T_M$`var.Formacion.Capital`) %>% 
      hc_add_series(name = "var.acum.Formacion.Capital", data = ECO_T_M$`var.acum.Formacion.Capital`) %>%
      
      hc_add_series(name = "var.Transferencias.Capital", data = ECO_T_M$`var.Transferencias.Capital`) %>% 
      hc_add_series(name = "var.acum.Transferencias.Capital", data = ECO_T_M$`var.acum.Transferencias.Capital`) %>%
      
  #    hc_add_series(name = "var.Amortización", data = ECO_T_M$`var.Amortización`) %>% 
  #    hc_add_series(name = "var.acum.Amortización", data = ECO_T_M$`var.acum.Amortización`) %>%
      
      hc_add_series(name = "var.Adquisición.Valores", data = ECO_T_M$`var.Adquisición.Valores`) %>% 
      hc_add_series(name = "var.acum.Adquisición.Valores", data = ECO_T_M$`var.acum.Adquisición.Valores`) %>%
      
      hc_add_series(name = "var.Otros.AF", data = ECO_T_M$`var.Otros.AF`) %>% 
      hc_add_series(name = "var.acum.Otros.AF", data = ECO_T_M$`var.acum.Otros.AF`) %>%
      
      hc_chart(zoomType = "xy")
    
    ECO_T_M 
    
    
  })
  
  ################################
  
  #
  ################################
  
  
  output$COG_TOTAL_A <- renderHighchart({
    
    COG_T_A <- tabla.10.1.F %>%
      filter(`CLA ECO 1` == input$eco1.5) 
    
    COG_T_A  <- highchart() %>% 
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
      hc_xAxis(categories = COG_T_A$Año) %>% 
      hc_add_series(name = "Remuneraciones", data = COG_T_A$`Remuneraciones`) %>% 
      hc_add_series(name = "Servicios", data = COG_T_A$`Servicios`) %>% 
      hc_add_series(name = "Materiales y suministros", data = COG_T_A$`Materiales y suministros`) %>% 
      hc_add_series(name = "Intereses y comisiones", data = COG_T_A$`Intereses y comisiones`) %>%
      hc_add_series(name = "Activos financieros", data = COG_T_A$`Activos financieros`) %>%
      hc_add_series(name = "Bienes duraderos", data = COG_T_A$`Bienes duraderos`) %>%
      hc_add_series(name = "Transferencias corrientes", data = COG_T_A$`Transferencias corrientes`) %>%
      hc_add_series(name = "Transferencias de capital", data = COG_T_A$`Transferencias de capital`) %>%
#     hc_add_series(name = "Amortización", data = COG_T_A$`Amortización`) %>%
      hc_add_series(name = "Cuentas especiales", data = COG_T_A$`Cuentas especiales`) %>% 
      hc_chart(zoomType = "xy")
    
    COG_T_A 
    
  })
  
  ######################
  #      Mensual COG   #
  ######################
  
  output$COG_TOTAL_M <- renderHighchart({
    
    COG_T_M <- tabla.10.2.F %>%
      filter(`CE1` == input$eco1.5) 
    
    COG_T_M  <- highchart() %>% 
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
      hc_xAxis(categories = COG_T_M$Fecha) %>% 
      hc_add_series(name = "Remuneraciones", data = COG_T_M$`Remuneraciones`) %>% 
      hc_add_series(name = "Servicios", data = COG_T_M$`Servicios`) %>% 
      hc_add_series(name = "Materiales y suministros", data = COG_T_M$`Materiales y suministros`) %>% 
      hc_add_series(name = "Intereses y comisiones", data = COG_T_M$`Intereses y comisiones`) %>%
      hc_add_series(name = "Activos financieros", data = COG_T_M$`Activos financieros`) %>%
      hc_add_series(name = "Bienes duraderos", data = COG_T_M$`Bienes duraderos`) %>%
      hc_add_series(name = "Transferencias corrientes", data = COG_T_M$`Transferencias corrientes`) %>%
      hc_add_series(name = "Transferencias de capital", data = COG_T_M$`Transferencias de capital`) %>%
 #    hc_add_series(name = "Amortización", data = COG_T_M$`Amortización`) %>%
      hc_add_series(name = "Cuentas especiales", data = COG_T_M$`Cuentas especiales`) %>% 
      hc_chart(zoomType = "xy")
    
    COG_T_M 
    
  })
  
  ######################################
  #   Variacion y variacion acumulada  #
  ######################################
  
  
  output$COG_TOTAL_VAR <- renderHighchart({
    
    COG_T_M <- tabla.10.2.F %>%
      filter(`CE1` == input$eco1.5) 
    
    COG_T_M <- mutate(COG_T_M,
                      var.Remuneraciones = round((Remuneraciones/lag(Remuneraciones,n = 12)-1)*100,1),
                      var.Servicios = round((`Servicios`/lag(`Servicios`,n = 12)-1)*100,1),
                      var.Materiales.suministros = round((`Materiales y suministros`/lag(`Materiales y suministros`,n = 12)-1)*100,1),
                      var.Bienes.duraderos = round((`Bienes duraderos`/lag(`Bienes duraderos`,n = 12)-1)*100,1),
                      var.Transferencias.corrientes = round((`Transferencias corrientes`/lag(`Transferencias corrientes`,n = 12)-1)*100,1),
                      var.Transferencias.capital = round((`Transferencias de capital`/lag(`Transferencias de capital`,n = 12)-1)*100,1),
                      var.Intereses.comisiones = round((`Intereses y comisiones`/lag(`Intereses y comisiones`,n = 12)-1)*100,1),
                 #   var.Amortización = round((`Amortización`/lag(`Amortización`,n = 12)-1)*100,1),
                      var.Activos.financieros = round((`Activos financieros`/lag(`Activos financieros`,n = 12)-1)*100,1)
                      
    )
    
    COG_T_M <- mutate(COG_T_M, 
                      acum_12.Remuneraciones = roll_sum(COG_T_M$Remuneraciones, 12, align = "right", fill = NA),
                      var.acum.Remuneraciones  = round((acum_12.Remuneraciones/lag(acum_12.Remuneraciones,n = 12)-1)*100,1),
                      
                      acum_12.Servicios = roll_sum(COG_T_M$Servicios, 12, align = "right", fill = NA),
                      var.acum.Servicios  = round((acum_12.Servicios/lag(acum_12.Servicios,n = 12)-1)*100,1),
                      
                      acum_12.Materiales.suministros = roll_sum(COG_T_M$`Materiales y suministros`, 12, align = "right", fill = NA),
                      var.acum.Materiales.suministros  = round((acum_12.Materiales.suministros/lag(acum_12.Materiales.suministros,n = 12)-1)*100,1), 
                      
                      acum_12.Bienes.duraderos = roll_sum(COG_T_M$`Bienes duraderos`, 12, align = "right", fill = NA),
                      var.acum.Bienes.duraderos  = round((acum_12.Bienes.duraderos/lag(acum_12.Bienes.duraderos,n = 12)-1)*100,1),
                      
                      acum_12.Transferencias.corrientes = roll_sum(COG_T_M$`Transferencias corrientes`, 12, align = "right", fill = NA),
                      var.acum.Transferencias.corrientes  = round((acum_12.Transferencias.corrientes/lag(acum_12.Transferencias.corrientes,n = 12)-1)*100,1),
                      
                      acum_12.Transferencias.capital = roll_sum(COG_T_M$`Transferencias de capital`, 12, align = "right", fill = NA),
                      var.acum.Transferencias.capital = round((acum_12.Transferencias.capital/lag(acum_12.Transferencias.capital,n = 12)-1)*100,1),
                      
                      acum_12.Intereses.comisiones = roll_sum(COG_T_M$`Intereses y comisiones`, 12, align = "right", fill = NA),
                      var.acum.Intereses.comisiones = round((acum_12.Intereses.comisiones/lag(acum_12.Intereses.comisiones,n = 12)-1)*100,1),
                      
                  #   acum_12.Amortización = roll_sum(COG_T_M$`Amortización`, 12, align = "right", fill = NA),
                  #   var.acum.Amortización = round((acum_12.Amortización/lag(acum_12.Amortización,n = 12)-1)*100,1),
                      
                      acum_12.Activos.financieros = roll_sum(COG_T_M$`Activos financieros`, 12, align = "right", fill = NA),
                      var.acum.Activos.financieros = round((acum_12.Activos.financieros/lag(acum_12.Activos.financieros,n = 12)-1)*100,1),
                      
    )           
    
    COG_T_M  <- highchart() %>% 
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
      hc_xAxis(categories = COG_T_M$Fecha) %>% 
      hc_add_series(name = "var.Remuneraciones", data = COG_T_M$`var.Remuneraciones`) %>% 
      hc_add_series(name = "var.acum.Remuneraciones", data = COG_T_M$`var.acum.Remuneraciones`) %>% 
      
      hc_add_series(name = "var.Servicios", data = COG_T_M$`var.Servicios`) %>% 
      hc_add_series(name = "var.acum.Servicios", data = COG_T_M$`var.acum.Servicios`) %>% 
      
      hc_add_series(name = "var.Materiales.suministros", data = COG_T_M$`var.Materiales.suministros`) %>% 
      hc_add_series(name = "var.acum.Materiales.suministros", data = COG_T_M$`var.acum.Materiales.suministros`) %>% 
      
      hc_add_series(name = "var.Bienes.duraderos", data = COG_T_M$`var.Bienes.duraderos`) %>% 
      hc_add_series(name = "var.acum.Bienes.duraderos", data = COG_T_M$`var.acum.Bienes.duraderos`) %>% 
      
      hc_add_series(name = "var.Transferencias.corrientes", data = COG_T_M$`var.Transferencias.corrientes`) %>% 
      hc_add_series(name = "var.acum.Transferencias.corrientes", data = COG_T_M$`var.acum.Transferencias.corrientes`) %>%
      
      hc_add_series(name = "var.Transferencias.capital", data = COG_T_M$`var.Transferencias.capital`) %>% 
      hc_add_series(name = "var.acum.Transferencias.capital", data = COG_T_M$`var.acum.Transferencias.capital`) %>%
      
      hc_add_series(name = "var.Intereses.comisiones", data = COG_T_M$`var.Intereses.comisiones`) %>% 
      hc_add_series(name = "var.acum.Intereses.comisiones", data = COG_T_M$`var.acum.Intereses.comisiones`) %>%
      
#    hc_add_series(name = "var.Amortización", data = COG_T_M$`var.Amortización`) %>% 
#    hc_add_series(name = "var.acum.Amortización", data = COG_T_M$`var.acum.Amortización`) %>%
      
      hc_add_series(name = "var.Activos.financieros", data = COG_T_M$`var.Activos.financieros`) %>% 
      hc_add_series(name = "var.acum.Activos.financieros", data = COG_T_M$`var.acum.Activos.financieros`) %>%
      
      hc_chart(zoomType = "xy")
    
    COG_T_M 
    
  })
  
  
  ########################
  #      TOTAL ECO.2     #
  ########################
  
  output$ECO_TOTAL_A <- renderHighchart({
    
    ECO_T_A <- tabla.11.1.F.A %>%
      filter(`CLA ECO 1` == input$eco1.6) 
    
    ECO_T_A  <- highchart() %>% 
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
      hc_xAxis(categories = ECO_T_A$Año) %>% 
      hc_add_series(name = "Gastos de consumo", data = ECO_T_A$`Gastos de consumo`) %>% 
      hc_add_series(name = "Intereses", data = ECO_T_A$`Intereses`) %>%
      hc_add_series(name = "Transferencias corrientes", data = ECO_T_A$`Transferencias corrientes`) %>% 
      hc_add_series(name = "Formacion de capital", data = ECO_T_A$`Formacion de capital`) %>%
      hc_add_series(name = "Adquisición de activos", data = ECO_T_A$`Adquisición de activos`) %>%
      hc_add_series(name = "Transferencias de capital", data = ECO_T_A$`Transferencias de capital`) %>%
      hc_add_series(name = "Sumas sin asignación", data = ECO_T_A$`Sumas sin asignación`) %>%
      hc_add_series(name = "Adquisición de valores", data = ECO_T_A$`Adquisición de valores`) %>%
  #    hc_add_series(name = "Amortización", data = ECO_T_A$`Amortización`) %>%
      hc_add_series(name = "Otros activos financieros", data = ECO_T_A$`Otros activos financieros`) %>%
      hc_chart(zoomType = "xy")
    
    ECO_T_A 
    
  })
  
  ######################
  #    Mensual ECO.2   #
  ######################
  
  output$ECO_TOTAL_M <- renderHighchart({
    
    ECO_T_M <- tabla.11.1.F.M %>%
      filter(CE1 == input$eco1.6) 
    
    ECO_T_M  <- highchart() %>% 
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
      hc_xAxis(categories = ECO_T_M$Fecha) %>% 
      hc_add_series(name = "Gastos de consumo", data = ECO_T_M$`Gastos de consumo`) %>% 
      hc_add_series(name = "Intereses", data = ECO_T_M$`Intereses`) %>%
      hc_add_series(name = "Transferencias corrientes", data = ECO_T_M$`Transferencias corrientes`) %>% 
      hc_add_series(name = "Formacion de capital", data = ECO_T_M$`Formacion de capital`) %>%
      hc_add_series(name = "Adquisición de activos", data = ECO_T_M$`Adquisición de activos`) %>%
      hc_add_series(name = "Transferencias de capital", data = ECO_T_M$`Transferencias de capital`) %>%
      hc_add_series(name = "Sumas sin asignación", data = ECO_T_M$`Sumas sin asignación`) %>%
      hc_add_series(name = "Adquisición de valores", data = ECO_T_M$`Adquisición de valores`) %>%
  #    hc_add_series(name = "Amortización", data = ECO_T_M$`Amortización`) %>%
      hc_add_series(name = "Otros activos financieros", data = ECO_T_M$`Otros activos financieros`) %>%
      hc_chart(zoomType = "xy")
    
    ECO_T_M 
    
  })
  
  
  ######################
  #    Mensual ECO.2   #
  ######################
  
  output$ECO_TOTAL_VAR <- renderHighchart({
    
    ECO_T_M <- tabla.11.1.F.M %>%
      filter(CE1 == input$eco1.6) 
    
    ECO_T_M <- mutate(ECO_T_M,
                      var.GastosConsumo = round((`Gastos de consumo`/lag(`Gastos de consumo`,n = 12)-1)*100,1),
                      var.TransferenciasCorrientes = round((`Transferencias corrientes`/lag(`Transferencias corrientes`,n = 12)-1)*100,1),
                      var.Intereses = round((`Intereses`/lag(`Intereses`,n = 12)-1)*100,1),
                      var.Formacion.Capital = round((`Formación de capital`/lag(`Formación de capital`,n = 12)-1)*100,1),
                      var.Transferencias.Capital = round((`Transferencias de capital`/lag(`Transferencias de capital`,n = 12)-1)*100,1),
                      var.Adquisición.Activos = round((`Adquisición de activos`/lag(`Adquisición de activos`,n = 12)-1)*100,1),
               #       var.Amortizacion = round((`Amortización`/lag(`Amortización`,n = 12)-1)*100,1),
                      var.Adquisición.Valores = round((`Adquisición de valores`/lag(`Adquisición de valores`,n = 12)-1)*100,1),
                      var.Otros.AF = round((`Otros activos financieros`/lag(`Otros activos financieros`,n = 12)-1)*100,1)
    )
    
    ECO_T_M <- mutate(ECO_T_M, 
                      acum_12.GastosConsumo = roll_sum(ECO_T_M$`Gastos de consumo`, 12, align = "right", fill = NA),
                      var.acum.GastosConsumo  = round((acum_12.GastosConsumo/lag(acum_12.GastosConsumo,n = 12)-1)*100,1),
                      
                      acum_12.TransferenciasCorrientes = roll_sum(ECO_T_M$`Transferencias corrientes`, 12, align = "right", fill = NA),
                      var.acum.TransferenciasCorrientes  = round((acum_12.TransferenciasCorrientes/lag(acum_12.TransferenciasCorrientes,n = 12)-1)*100,1),
                      
                      acum_12.Intereses = roll_sum(ECO_T_M$`Intereses`, 12, align = "right", fill = NA),
                      var.acum.Intereses  = round((acum_12.Intereses/lag(acum_12.Intereses,n = 12)-1)*100,1),
                      
                      acum_12.Formacion.Capital = roll_sum(ECO_T_M$`Formación de capital`, 12, align = "right", fill = NA),
                      var.acum.Formacion.Capital  = round((acum_12.Formacion.Capital/lag(acum_12.Formacion.Capital,n = 12)-1)*100,1),
                      
                      var.Transferencias.Capital = roll_sum(ECO_T_M$`Transferencias de capital`, 12, align = "right", fill = NA),
                      var.acum.Transferencias.Capital  = round((var.Transferencias.Capital/lag(var.Transferencias.Capital,n = 12)-1)*100,1),
                      
                      var.Adquisición.Activos = roll_sum(ECO_T_M$`Adquisición de valores`, 12, align = "right", fill = NA),
                      var.acum.Adquisición.Activos  = round((var.Adquisición.Activos/lag(var.Adquisición.Activos,n = 12)-1)*100,1),
                      
               #       var.Amortizacion = roll_sum(ECO_T_M$`Amortización`, 12, align = "right", fill = NA),
               #       var.acum.Amortizacion  = round((var.Amortizacion/lag(var.Amortizacion,n = 12)-1)*100,1),
                      
                      var.Adquisición.Valores = roll_sum(ECO_T_M$`Adquisición de valores`, 12, align = "right", fill = NA),
                      var.acum.Adquisición.Valores  = round((var.Adquisición.Valores/lag(var.Adquisición.Valores,n = 12)-1)*100,1),
                      
                      var.Otros.AF = roll_sum(ECO_T_M$`Otros activos financieros`, 12, align = "right", fill = NA),
                      var.acum.Otros.AF  = round((var.Otros.AF/lag(var.Otros.AF,n = 12)-1)*100,1)
    )
    
    
    ECO_T_M  <- highchart() %>% 
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
      
      hc_xAxis(categories = ECO_T_M$Fecha) %>% 
      
      hc_add_series(name = "var.GastosConsumo",      data = ECO_T_M$`var.GastosConsumo`) %>% 
      hc_add_series(name = "var.acum.GastosConsumo", data = ECO_T_M$`var.acum.GastosConsumo`) %>% 
      
      hc_add_series(name = "var.TransferenciasCorrientes", data = ECO_T_M$`var.TransferenciasCorrientes`) %>% 
      hc_add_series(name = "var.acum.TransferenciasCorrientes", data = ECO_T_M$`var.acum.TransferenciasCorrientes`) %>% 
      
      hc_add_series(name = "var.Intereses", data = ECO_T_M$`var.Intereses`) %>% 
      hc_add_series(name = "var.acum.Intereses", data = ECO_T_M$`var.acum.Intereses`) %>% 
      
      hc_add_series(name = "var.Formacion.Capital", data = ECO_T_M$`var.Formacion.Capital`) %>% 
      hc_add_series(name = "var.acum.Formacion.Capital", data = ECO_T_M$`var.acum.Formacion.Capital`) %>%
      
      hc_add_series(name = "var.Transferencias.Capital", data = ECO_T_M$`var.Transferencias.Capital`) %>% 
      hc_add_series(name = "var.acum.Transferencias.Capital", data = ECO_T_M$`var.acum.Transferencias.Capital`) %>%
      
  #   hc_add_series(name = "var.Amortización", data = ECO_T_M$`var.Amortización`) %>% 
  #   hc_add_series(name = "var.acum.Amortización", data = ECO_T_M$`var.acum.Amortización`) %>%
      
      hc_add_series(name = "var.Adquisición.Valores", data = ECO_T_M$`var.Adquisición.Valores`) %>% 
      hc_add_series(name = "var.acum.Adquisición.Valores", data = ECO_T_M$`var.acum.Adquisición.Valores`) %>%
      
      hc_add_series(name = "var.Otros.AF", data = ECO_T_M$`var.Otros.AF`) %>% 
      hc_add_series(name = "var.acum.Otros.AF", data = ECO_T_M$`var.acum.Otros.AF`) %>%
      
      hc_chart(zoomType = "xy")
    
    ECO_T_M 
    
  })
  
  
  #######################
  #      Titulo         #
  #######################
  
  ##########################
  #       COG - Anual      #
  ##########################
  
  output$clasi_COG_ANUAL_TAM = renderHighchart ({
    
    COG_AT <- tabla.12.1.COG.A %>%
      filter(`Título` == input$titulo.8) 
    
    COG_AT  <- highchart() %>% 
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
      hc_xAxis(categories = COG_AT$Año) %>% 
      hc_add_series(name = "Remuneraciones", data = COG_AT$`Remuneraciones`) %>% 
      hc_add_series(name = "Servicios", data = COG_AT$`Servicios`) %>% 
      hc_add_series(name = "Materiales y suministros", data = COG_AT$`Materiales y suministros`) %>% 
      hc_add_series(name = "Intereses y comisiones", data = COG_AT$`Intereses y comisiones`) %>%
      hc_add_series(name = "Activos financieros", data = COG_AT$`Activos financieros`) %>%
      hc_add_series(name = "Bienes duraderos", data = COG_AT$`Bienes duraderos`) %>%
      hc_add_series(name = "Transferencias corrientes", data = COG_AT$`Transferencias corrientes`) %>%
      hc_add_series(name = "Transferencias de capital", data = COG_AT$`Transferencias de capital`) %>%
  #   hc_add_series(name = "Amortización", data = COG_AT$`Amortización`) %>%
      hc_add_series(name = "Cuentas especiales", data = COG_AT$`Cuentas especiales`) %>% 
      hc_chart(zoomType = "xy")
    COG_AT 
    
  })
  
  
  output$clasi_COG_MENSUAL_T = renderHighchart ({
    
    COG_AM <- tabla.12.1.COG.M %>%
      filter(`Título` == input$titulo.8) 
    
 #   COG_AM <- COG_AM %>%
 #     dplyr::mutate(
 #       'Bienes.duraderos' = round(`Bienes duraderos`)
 #     )
    
    
    COG_AM  <- highchart() %>% 
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
      hc_xAxis(categories = COG_AM$Fecha) %>% 
      hc_add_series(name = "Remuneraciones",            data = COG_AM$`Remuneraciones`) %>% 
      hc_add_series(name = "Servicios",                 data = COG_AM$`Servicios`) %>% 
      hc_add_series(name = "Materiales y suministros",  data = COG_AM$`Materiales.y.suministros`) %>% 
      hc_add_series(name = "Intereses y comisiones",    data = COG_AM$`Intereses.y.comisiones`) %>%
      hc_add_series(name = "Activos financieros",       data = COG_AM$`Activos.financieros`) %>%
      hc_add_series(name = "Bienes duraderos",          data = COG_AM$`Bienes.duraderos`) %>%
      hc_add_series(name = "Transferencias corrientes", data = COG_AM$`Transferencias.corrientes`) %>%
      hc_add_series(name = "Transferencias de capital", data = COG_AM$`Transferencias.de.capital`) %>%
#      hc_add_series(name = "Amortización",              data = COG_AM$`Amortización`) %>%
      hc_add_series(name = "Cuentas especiales",        data = COG_AM$`Cuentas.especiales`) %>% 
      hc_chart(zoomType = "xy")
    
    COG_AM 
    
  })
  
  output$clasi_COG_VAR_MENSUAL_T = renderHighchart ({
    
    COG_AM <- tabla.12.1.COG.M %>%
      filter(`Título` == input$titulo.8) 
    
    COG_AM <- mutate(COG_AM,
                      var.Remuneraciones = round((Remuneraciones/lag(Remuneraciones,n = 12)-1)*100,1),
                      var.Servicios = round((`Servicios`/lag(`Servicios`,n = 12)-1)*100,1),
                      var.Materiales.suministros = round((`Materiales y suministros`/lag(`Materiales y suministros`,n = 12)-1)*100,1),
                      var.Bienes.duraderos = round((`Bienes duraderos`/lag(`Bienes duraderos`,n = 12)-1)*100,1),
                      var.Transferencias.corrientes = round((`Transferencias corrientes`/lag(`Transferencias corrientes`,n = 12)-1)*100,1),
                      var.Transferencias.capital = round((`Transferencias de capital`/lag(`Transferencias de capital`,n = 12)-1)*100,1),
                      var.Intereses.comisiones = round((`Intereses y comisiones`/lag(`Intereses y comisiones`,n = 12)-1)*100,1),
        #              var.Amortización = round((`Amortización`/lag(`Amortización`,n = 12)-1)*100,1),
                      var.Activos.financieros = round((`Activos financieros`/lag(`Activos financieros`,n = 12)-1)*100,1)
    )
    
    COG_AM <- mutate(COG_AM, 
                      acum_12.Remuneraciones = roll_sum(COG_AM$Remuneraciones, 12, align = "right", fill = NA),
                      var.acum.Remuneraciones  = round((acum_12.Remuneraciones/lag(acum_12.Remuneraciones,n = 12)-1)*100,1),
                      
                      acum_12.Servicios = roll_sum(COG_AM$Servicios, 12, align = "right", fill = NA),
                      var.acum.Servicios  = round((acum_12.Servicios/lag(acum_12.Servicios,n = 12)-1)*100,1),
                      
                      acum_12.Materiales.suministros = roll_sum(COG_AM$`Materiales y suministros`, 12, align = "right", fill = NA),
                      var.acum.Materiales.suministros  = round((acum_12.Materiales.suministros/lag(acum_12.Materiales.suministros,n = 12)-1)*100,1), 
                      
                      acum_12.Bienes.duraderos = roll_sum(COG_AM$`Bienes duraderos`, 12, align = "right", fill = NA),
                      var.acum.Bienes.duraderos  = round((acum_12.Bienes.duraderos/lag(acum_12.Bienes.duraderos,n = 12)-1)*100,1),
                      
                      acum_12.Transferencias.corrientes = roll_sum(COG_AM$`Transferencias corrientes`, 12, align = "right", fill = NA),
                      var.acum.Transferencias.corrientes  = round((acum_12.Transferencias.corrientes/lag(acum_12.Transferencias.corrientes,n = 12)-1)*100,1),
                      
                      acum_12.Transferencias.capital = roll_sum(COG_AM$`Transferencias de capital`, 12, align = "right", fill = NA),
                      var.acum.Transferencias.capital = round((acum_12.Transferencias.capital/lag(acum_12.Transferencias.capital,n = 12)-1)*100,1),
                      
                      acum_12.Intereses.comisiones = roll_sum(COG_AM$`Intereses y comisiones`, 12, align = "right", fill = NA),
                      var.acum.Intereses.comisiones = round((acum_12.Intereses.comisiones/lag(acum_12.Intereses.comisiones,n = 12)-1)*100,1),
                      
   #                   acum_12.Amortización = roll_sum(COG_AM$`Amortización`, 12, align = "right", fill = NA),
   #                   var.acum.Amortización = round((acum_12.Amortización/lag(acum_12.Amortización,n = 12)-1)*100,1),
                      
                      acum_12.Activos.financieros = roll_sum(COG_AM$`Activos financieros`, 12, align = "right", fill = NA),
                      var.acum.Activos.financieros = round((acum_12.Activos.financieros/lag(acum_12.Activos.financieros,n = 12)-1)*100,1)
    )
    
    
    COG_AM  <- highchart() %>% 
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
      hc_xAxis(categories = COG_AM$Fecha) %>% 
      hc_add_series(name = "var.Remuneraciones", data = COG_AM$`var.Remuneraciones`) %>% 
      hc_add_series(name = "var.acum.Remuneraciones", data = COG_AM$`var.acum.Remuneraciones`) %>% 
      
      hc_add_series(name = "var.Servicios", data = COG_AM$`var.Servicios`) %>% 
      hc_add_series(name = "var.acum.Servicios", data = COG_AM$`var.acum.Servicios`) %>% 
      
      hc_add_series(name = "var.Materiales.suministros", data = COG_AM$`var.Materiales.suministros`) %>% 
      hc_add_series(name = "var.acum.Materiales.suministros", data = COG_AM$`var.acum.Materiales.suministros`) %>% 
      
      hc_add_series(name = "var.Bienes.duraderos", data = COG_AM$`var.Bienes.duraderos`) %>% 
      hc_add_series(name = "var.acum.Bienes.duraderos", data = COG_AM$`var.acum.Bienes.duraderos`) %>% 
      
      hc_add_series(name = "var.Transferencias.corrientes", data = COG_AM$`var.Transferencias.corrientes`) %>% 
      hc_add_series(name = "var.acum.Transferencias.corrientes", data = COG_AM$`var.acum.Transferencias.corrientes`) %>%
      
      hc_add_series(name = "var.Transferencias.capital", data = COG_AM$`var.Transferencias.capital`) %>% 
      hc_add_series(name = "var.acum.Transferencias.capital", data = COG_AM$`var.acum.Transferencias.capital`) %>%
      
      hc_add_series(name = "var.Intereses.comisiones", data = COG_AM$`var.Intereses.comisiones`) %>% 
      hc_add_series(name = "var.acum.Intereses.comisiones", data = COG_AM$`var.acum.Intereses.comisiones`) %>%
      
 #     hc_add_series(name = "var.Amortización", data = COG_AM$`var.Amortización`) %>% 
 #     hc_add_series(name = "var.acum.Amortización", data = COG_AM$`var.acum.Amortización`) %>%
      
      hc_add_series(name = "var.Activos.financieros", data = COG_AM$`var.Activos.financieros`) %>% 
      hc_add_series(name = "var.acum.Activos.financieros", data = COG_AM$`var.acum.Activos.financieros`) %>%
      
      hc_chart(zoomType = "xy")
    
    COG_AM 
    
  
  })
  
  
  
  output$clasi_COG_total_T = renderHighchart ({
    
    COG_AT <- tabla.12.1.F.A %>%
      filter(`Título` == input$titulo.8) %>% 
      filter(`CLA ECO 1` == input$eco1.7)
      
    
    
    COG_AT  <- highchart() %>% 
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
      hc_xAxis(categories = COG_AT$Año) %>% 
      hc_add_series(name = "Remuneraciones", data = COG_AT$`Remuneraciones`) %>% 
      hc_add_series(name = "Servicios", data = COG_AT$`Servicios`) %>% 
      hc_add_series(name = "Materiales y suministros", data = COG_AT$`Materiales y suministros`) %>% 
      hc_add_series(name = "Intereses y comisiones", data = COG_AT$`Intereses y comisiones`) %>%
      hc_add_series(name = "Activos financieros", data = COG_AT$`Activos financieros`) %>%
      hc_add_series(name = "Bienes duraderos", data = COG_AT$`Bienes duraderos`) %>%
      hc_add_series(name = "Transferencias corrientes", data = COG_AT$`Transferencias corrientes`) %>%
      hc_add_series(name = "Transferencias de capital", data = COG_AT$`Transferencias de capital`) %>%
#      hc_add_series(name = "Amortización", data = COG_AT$`Amortización`) %>%
      hc_add_series(name = "Cuentas especiales", data = COG_AT$`Cuentas especiales`) %>% 
      hc_chart(zoomType = "xy")
    COG_AT 
    
  })
  
  
  
  output$clasi_COG_total_M = renderHighchart ({
    
    COG_AT <- tabla.12.1.F.M %>%
      filter(`Título` == input$titulo.8) %>% 
      filter(CE1 == input$eco1.7)
    
    
    
    COG_AT  <- highchart() %>% 
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
      hc_xAxis(categories = COG_AT$Año) %>% 
      hc_add_series(name = "Remuneraciones", data = COG_AT$`Remuneraciones`) %>% 
      hc_add_series(name = "Servicios", data = COG_AT$`Servicios`) %>% 
      hc_add_series(name = "Materiales y suministros", data = COG_AT$`Materiales y suministros`) %>% 
      hc_add_series(name = "Intereses y comisiones", data = COG_AT$`Intereses y comisiones`) %>%
      hc_add_series(name = "Activos financieros", data = COG_AT$`Activos financieros`) %>%
      hc_add_series(name = "Bienes duraderos", data = COG_AT$`Bienes duraderos`) %>%
      hc_add_series(name = "Transferencias corrientes", data = COG_AT$`Transferencias corrientes`) %>%
      hc_add_series(name = "Transferencias de capital", data = COG_AT$`Transferencias de capital`) %>%
#      hc_add_series(name = "Amortización", data = COG_AT$`Amortización`) %>%
      hc_add_series(name = "Cuentas especiales", data = COG_AT$`Cuentas especiales`) %>% 
      hc_chart(zoomType = "xy")
    COG_AT 
    
  })
  
  
#  output$tabla_COG_TOTAL_M <- renderReactable ({
#    
#    COG_AM <- tabla.12.1.F.M %>%
#      filter(Titulo == input$titulo.8) %>%
#      filter(cla_eco.1 == input$eco1.5)
#    
#    
#    reactable(COG_AM) 
#  })
  
  ##########################
  #      COG - Mensual     #
  ##########################
  
  output$clasi_COG_titulo_var = renderHighchart ({
    
    COG_AM <- tabla.12.1.F.M %>%
      filter(`Título` == input$titulo.8) %>%
      filter(CE1 == input$eco1.7)
    
    COG_AM <- mutate(COG_AM,
                     var.Remuneraciones = round((Remuneraciones/lag(Remuneraciones,n = 12)-1)*100,1),
                     var.Servicios = round((`Servicios`/lag(`Servicios`,n = 12)-1)*100,1),
                     var.Materiales.suministros = round((`Materiales y suministros`/lag(`Materiales y suministros`,n = 12)-1)*100,1),
                     var.Bienes.duraderos = round((`Bienes duraderos`/lag(`Bienes duraderos`,n = 12)-1)*100,1),
                     var.Transferencias.corrientes = round((`Transferencias corrientes`/lag(`Transferencias corrientes`,n = 12)-1)*100,1),
                     var.Transferencias.capital = round((`Transferencias de capital`/lag(`Transferencias de capital`,n = 12)-1)*100,1),
                     var.Intereses.comisiones = round((`Intereses y comisiones`/lag(`Intereses y comisiones`,n = 12)-1)*100,1),
           #          var.Amortización = round((`Amortización`/lag(`Amortización`,n = 12)-1)*100,1),
                     var.Activos.financieros = round((`Activos financieros`/lag(`Activos financieros`,n = 12)-1)*100,1)  
    )
    
    COG_AM <- mutate(COG_AM,
                     acum_12.Remuneraciones = roll_sum(Remuneraciones, 12, align = "right", fill = NA),
                     var.acum.Remuneraciones  = round((acum_12.Remuneraciones/lag(acum_12.Remuneraciones,n = 12)-1)*100,1),
                     
                     acum_12.Servicios = roll_sum(COG_AM$Servicios, 12, align = "right", fill = NA),
                     var.acum.Servicios  = round((acum_12.Servicios/lag(acum_12.Servicios,n = 12)-1)*100,1),
                     
                     acum_12.Materiales.suministros = roll_sum(COG_AM$`Materiales y suministros`, 12, align = "right", fill = NA),
                     var.acum.Materiales.suministros  = round((acum_12.Materiales.suministros/lag(acum_12.Materiales.suministros,n = 12)-1)*100,1),
                     
                     acum_12.Bienes.duraderos = roll_sum(COG_AM$`Bienes duraderos`, 12, align = "right", fill = NA),
                     var.acum.Bienes.duraderos  = round((acum_12.Bienes.duraderos/lag(acum_12.Bienes.duraderos,n = 12)-1)*100,1),
                     
                     acum_12.Transferencias.corrientes = roll_sum(COG_AM$`Transferencias corrientes`, 12, align = "right", fill = NA),
                     var.acum.Transferencias.corrientes  = round((acum_12.Transferencias.corrientes/lag(acum_12.Transferencias.corrientes,n = 12)-1)*100,1),
                     
                     acum_12.Transferencias.capital = roll_sum(COG_AM$`Transferencias de capital`, 12, align = "right", fill = NA),
                     var.acum.Transferencias.capital = round((acum_12.Transferencias.capital/lag(acum_12.Transferencias.capital,n = 12)-1)*100,1),
                     
                     acum_12.Intereses.comisiones = roll_sum(COG_AM$`Intereses y comisiones`, 12, align = "right", fill = NA),
                     var.acum.Intereses.comisiones = round((acum_12.Intereses.comisiones/lag(acum_12.Intereses.comisiones,n = 12)-1)*100,1),
                     
   #                  acum_12.Amortización = roll_sum(COG_AM$`Amortización`, 12, align = "right", fill = NA),
   #                  var.acum.Amortización = round((acum_12.Amortización/lag(acum_12.Amortización,n = 12)-1)*100,1),
                     
                     acum_12.Activos.financieros = roll_sum(COG_AM$`Activos financieros`, 12, align = "right", fill = NA),
                     var.acum.Activos.financieros = round((acum_12.Activos.financieros/lag(acum_12.Activos.financieros,n = 12)-1)*100,1)
                     
    )
    
    
    
    COG_AM  <- highchart() %>% 
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
      hc_xAxis(categories = COG_AM$Año) %>% 
      
      hc_add_series(name = "var.Remuneraciones", data = COG_AM$`var.Remuneraciones`) %>% 
      hc_add_series(name = "var.acum.Remuneraciones", data = COG_AM$`var.acum.Remuneraciones`) %>%
      
      hc_add_series(name = "var.Servicios", data = COG_AM$`var.Servicios`) %>% 
      hc_add_series(name = "var.acum.Servicios", data = COG_AM$`var.acum.Servicios`) %>%
      
      hc_add_series(name = "var.Materiales.suministros", data = COG_AM$`var.Materiales.suministros`) %>%
      hc_add_series(name = "var.acum.Materiales.suministros", data = COG_AM$`var.acum.Materiales.suministros`) %>% 
      
      hc_add_series(name = "var.Bienes.duraderos", data = COG_AM$`var.Bienes.duraderos`) %>% 
      hc_add_series(name = "var.acum.Bienes.duraderos", data = COG_AM$`var.acum.Bienes.duraderos`) %>%
      
      hc_add_series(name = "var.Transferencias.corrientes", data = COG_AM$`var.Transferencias.corrientes`) %>% 
      hc_add_series(name = "var.acum.Transferencias.corrientes", data = COG_AM$`var.acum.Transferencias.corrientes`) %>%
      
      hc_add_series(name = "var.Transferencias.capital", data = COG_AM$`var.Transferencias.capital`) %>% 
      hc_add_series(name = "var.acum.Transferencias.capital", data = COG_AM$`var.acum.Transferencias.capital`) %>%
      
      hc_add_series(name = "var.Intereses.comisiones", data = COG_AM$`var.Intereses.comisiones`) %>% 
      hc_add_series(name = "var.acum.Intereses.comisiones", data = COG_AM$`var.acum.Intereses.comisiones`) %>%
      
 #     hc_add_series(name = "var.Amortización", data = COG_AM$`var.Amortización`) %>% 
 #     hc_add_series(name = "var.acum.Amortización", data = COG_AM$`var.acum.Amortización`) %>%
      
      hc_add_series(name = "var.Activos.financieros", data = COG_AM$`var.Activos.financieros`) %>% 
      hc_add_series(name = "var.acum.Activos.financieros", data = COG_AM$`var.acum.Activos.financieros`) %>%
      
      
      hc_chart(zoomType = "xy")
    
    COG_AM 
    
  })
  
  ############################
  #      Titulo ECO.2 - A    #
  ############################
  
  output$clasi_ECO2_ANUAL_TAM = renderHighchart ({
  
    ECO_AT <- tabla.12.1.ECO2.A  %>%
      filter(`Título` == input$titulo.9) 
    
    ECO_AT  <- highchart() %>% 
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
      hc_xAxis(categories = ECO_AT$Año) %>% 
      hc_add_series(name = "Gastos de consumo", data = ECO_AT$`Gastos de consumo`) %>% 
      hc_add_series(name = "Intereses", data = ECO_AT$`Intereses`) %>%
      hc_add_series(name = "Transferencias corrientes", data = ECO_AT$`Transferencias corrientes`) %>% 
      hc_add_series(name = "Formacion de capital", data = ECO_AT$`Formación de capital`) %>%
      hc_add_series(name = "Adquisición de activos", data = ECO_AT$`Adquisición de activos`) %>%
      hc_add_series(name = "Transferencias de capital", data = ECO_AT$`Transferencias de capital`) %>%
      hc_add_series(name = "Sumas sin asignación", data = ECO_AT$`Sumas sin asignación`) %>%
      hc_add_series(name = "Adquisición de valores", data = ECO_AT$`Adquisición de valores`) %>%
 #     hc_add_series(name = "Amortización", data = ECO_AT$`Amortización`) %>%
      hc_add_series(name = "Otros activos financieros", data = ECO_AT$`Otros activos financieros`) %>%
      hc_chart(zoomType = "xy")
    
    ECO_AT 
    
  })
  
  output$clasi_ECO2_MENSUAL_T = renderHighchart ({
    
    ECO_AM <- tabla.12.1.ECO2.M  %>%
      filter(`Título` == input$titulo.9) 
    
    ECO_AM <- ECO_AM %>%
      dplyr::mutate(
        'Adquisición de activos' = round(`Adquisición de activos`),
        'Formación de capital' = round(`Formación de capital`)
        
      )
    
    ECO_AM  <- highchart() %>% 
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
      hc_xAxis(categories = ECO_AM$Año) %>% 
      hc_add_series(name = "Gastos de consumo", data = ECO_AM$`Gastos de consumo`) %>% 
      hc_add_series(name = "Intereses", data = ECO_AM$`Intereses`) %>%
      hc_add_series(name = "Transferencias corrientes", data = ECO_AM$`Transferencias corrientes`) %>% 
      hc_add_series(name = "Formacion de capital", data = ECO_AM$`Formación de capital`) %>%
      hc_add_series(name = "Adquisición de activos", data = ECO_AM$`Adquisición de activos`) %>%
      hc_add_series(name = "Transferencias de capital", data = ECO_AM$`Transferencias de capital`) %>%
      hc_add_series(name = "Sumas sin asignación", data = ECO_AM$`Sumas sin asignación`) %>%
      hc_add_series(name = "Adquisición de valores", data = ECO_AM$`Adquisición de valores`) %>%
 #     hc_add_series(name = "Amortización", data = ECO_AM$`Amortización`) %>%
      hc_add_series(name = "Otros activos financieros", data = ECO_AM$`Otros activos financieros`) %>%
      hc_chart(zoomType = "xy")
    
    ECO_AM 
    
  })
  
  
  output$clasi_ECO2_VAR_MENSUAL_T = renderHighchart ({
    
    ECO_AM <- tabla.12.1.ECO2.M  %>%
      filter(`Título` == input$titulo.9) 
    
    ECO_AM <- mutate(ECO_AM,
                     var.GastosConsumo = round((`Gastos de consumo`/lag(`Gastos de consumo`,n = 12)-1)*100,1),
                     var.TransferenciasCorrientes = round((`Transferencias corrientes`/lag(`Transferencias corrientes`,n = 12)-1)*100,1),
                     var.Intereses = round((`Intereses`/lag(`Intereses`,n = 12)-1)*100,1),
                     var.Formacion.Capital = round((`Formación de capital`/lag(`Formación de capital`,n = 12)-1)*100,1),
                     var.Transferencias.Capital = round((`Transferencias de capital`/lag(`Transferencias de capital`,n = 12)-1)*100,1),
                     var.Adquisición.Activos = round((`Adquisición de activos`/lag(`Adquisición de activos`,n = 12)-1)*100,1),
          #           var.Amortización = round((`Amortización`/lag(`Amortización`,n = 12)-1)*100,1),
                     var.Adquisición.Valores = round((`Adquisición de valores`/lag(`Adquisición de valores`,n = 12)-1)*100,1),
                     var.Otros.AF = round((`Otros activos financieros`/lag(`Otros activos financieros`,n = 12)-1)*100,1)
    )
    
    
    ECO_AM <- mutate(ECO_AM, 
                     acum_12.GastosConsumo = roll_sum(ECO_AM$`Gastos de consumo`, 12, align = "right", fill = NA),
                     var.acum.GastosConsumo  = round((acum_12.GastosConsumo/lag(acum_12.GastosConsumo,n = 12)-1)*100,1),
                     
                     acum_12.TransferenciasCorrientes = roll_sum(ECO_AM$`Transferencias corrientes`, 12, align = "right", fill = NA),
                     var.acum.TransferenciasCorrientes  = round((acum_12.TransferenciasCorrientes/lag(acum_12.TransferenciasCorrientes,n = 12)-1)*100,1),
                     
                     acum_12.Intereses = roll_sum(ECO_AM$`Intereses`, 12, align = "right", fill = NA),
                     var.acum.Intereses  = round((acum_12.Intereses/lag(acum_12.Intereses,n = 12)-1)*100,1),
                     
                     acum_12.Formacion.Capital = roll_sum(ECO_AM$`Formación de capital`, 12, align = "right", fill = NA),
                     var.acum.Formacion.Capital  = round((acum_12.Formacion.Capital/lag(acum_12.Formacion.Capital,n = 12)-1)*100,1),
                     
                     var.Transferencias.Capital = roll_sum(ECO_AM$`Transferencias de capital`, 12, align = "right", fill = NA),
                     var.acum.Transferencias.Capital  = round((var.Transferencias.Capital/lag(var.Transferencias.Capital,n = 12)-1)*100,1),
                     
                     var.Adquisición.Activos = roll_sum(ECO_AM$`Adquisición de valores`, 12, align = "right", fill = NA),
                     var.acum.Adquisición.Activos  = round((var.Adquisición.Activos/lag(var.Adquisición.Activos,n = 12)-1)*100,1),
                     
   #                 var.Amortización = roll_sum(ECO_AM$`Amortización`, 12, align = "right", fill = NA),
   #                 var.acum.Amortización  = round((var.Amortización/lag(var.Amortización,n = 12)-1)*100,1),
                     
                     var.Adquisición.Valores = roll_sum(ECO_AM$`Adquisición de valores`, 12, align = "right", fill = NA),
                     var.acum.Adquisición.Valores  = round((var.Adquisición.Valores/lag(var.Adquisición.Valores,n = 12)-1)*100,1),
                     
                     var.Otros.AF = roll_sum(ECO_AM$`Otros activos financieros`, 12, align = "right", fill = NA),
                     var.acum.Otros.AF  = round((var.Otros.AF/lag(var.Otros.AF,n = 12)-1)*100,1)
    )
    
    ECO_AM  <- highchart() %>% 
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
      
      hc_xAxis(categories = ECO_AM$Fecha) %>% 
      
      hc_add_series(name = "var.GastosConsumo",      data = ECO_AM$`var.GastosConsumo`) %>% 
      hc_add_series(name = "var.acum.GastosConsumo", data = ECO_AM$`var.acum.GastosConsumo`) %>% 
      
      hc_add_series(name = "var.TransferenciasCorrientes", data = ECO_AM$`var.TransferenciasCorrientes`) %>% 
      hc_add_series(name = "var.acum.TransferenciasCorrientes", data = ECO_AM$`var.acum.TransferenciasCorrientes`) %>% 
      
      hc_add_series(name = "var.Intereses", data = ECO_AM$`var.Intereses`) %>% 
      hc_add_series(name = "var.acum.Intereses", data = ECO_AM$`var.acum.Intereses`) %>% 
      
      hc_add_series(name = "var.Formacion.Capital", data = ECO_AM$`var.Formacion.Capital`) %>% 
      hc_add_series(name = "var.acum.Formacion.Capital", data = ECO_AM$`var.acum.Formacion.Capital`) %>%
      
      hc_add_series(name = "var.Transferencias.Capital", data = ECO_AM$`var.Transferencias.Capital`) %>% 
      hc_add_series(name = "var.acum.Transferencias.Capital", data = ECO_AM$`var.acum.Transferencias.Capital`) %>%
      
  #    hc_add_series(name = "var.Amortización", data = ECO_AM$`var.Amortización`) %>% 
  #    hc_add_series(name = "var.acum.Amortización", data = ECO_AM$`var.acum.Amortización`) %>%
      
      hc_add_series(name = "var.Adquisición.Valores", data = ECO_AM$`var.Adquisición.Valores`) %>% 
      hc_add_series(name = "var.acum.Adquisición.Valores", data = ECO_AM$`var.acum.Adquisición.Valores`) %>%
      
      hc_add_series(name = "var.Otros.AF", data = ECO_AM$`var.Otros.AF`) %>% 
      hc_add_series(name = "var.acum.Otros.AF", data = ECO_AM$`var.acum.Otros.AF`) %>%
      
      hc_chart(zoomType = "xy")
    
    ECO_AM 
    
  
  })
  
  
  
  output$clasi_ECO_total_T = renderHighchart ({
    
    ECO_AT <- tabla.13.1.F.A  %>%
      filter(`Título` == input$titulo.9) %>%
      filter(`CLA ECO 1` == input$eco1.8)
    
    ECO_AT  <- highchart() %>% 
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
      hc_xAxis(categories = ECO_AT$Año) %>% 
      hc_add_series(name = "Gastos de consumo", data = ECO_AT$`Gastos de consumo`) %>% 
      hc_add_series(name = "Intereses", data = ECO_AT$`Intereses`) %>%
      hc_add_series(name = "Transferencias corrientes", data = ECO_AT$`Transferencias corrientes`) %>% 
      hc_add_series(name = "Formacion de capital", data = ECO_AT$`Formacion de capital`) %>%
      hc_add_series(name = "Adquisición de activos", data = ECO_AT$`Adquisición de activos`) %>%
      hc_add_series(name = "Transferencias de capital", data = ECO_AT$`Transferencias de capital`) %>%
      hc_add_series(name = "Sumas sin asignación", data = ECO_AT$`Sumas sin asignación`) %>%
      hc_add_series(name = "Adquisición de valores", data = ECO_AT$`Adquisición de valores`) %>%
#      hc_add_series(name = "Amortización", data = ECO_AT$`Amortización`) %>%
      hc_add_series(name = "Otros activos financieros", data = ECO_AT$`Otros activos financieros`) %>%
      hc_chart(zoomType = "xy")
    
    ECO_AT 
    
  })
  
  
  output$clasi_ECO_total_M = renderHighchart ({
    
    ECO_AM <- tabla.13.1.F.M  %>%
      filter(`Título` == input$titulo.9) %>%
      filter(CE1 == input$eco1.8)
    
    ECO_AM <- ECO_AM %>%
      dplyr::mutate(
        'Adquisición de activos' = round(`Adquisición de activos`),
        'Formación de capital' = round(`Formación de capital`)
        
      )
    
    ECO_AM  <- highchart() %>% 
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
      hc_xAxis(categories = ECO_AM$Año) %>% 
      hc_add_series(name = "Gastos de consumo", data = ECO_AM$`Gastos de consumo`) %>% 
      hc_add_series(name = "Intereses", data = ECO_AM$`Intereses`) %>%
      hc_add_series(name = "Transferencias corrientes", data = ECO_AM$`Transferencias corrientes`) %>% 
      hc_add_series(name = "Formacion de capital", data = ECO_AM$`Formación de capital`) %>%
      hc_add_series(name = "Adquisición de activos", data = ECO_AM$`Adquisición de activos`) %>%
      hc_add_series(name = "Transferencias de capital", data = ECO_AM$`Transferencias de capital`) %>%
      hc_add_series(name = "Sumas sin asignación", data = ECO_AM$`Sumas sin asignación`) %>%
      hc_add_series(name = "Adquisición de valores", data = ECO_AM$`Adquisición de valores`) %>%
 #     hc_add_series(name = "Amortización", data = ECO_AM$`Amortización`) %>%
      hc_add_series(name = "Otros activos financieros", data = ECO_AM$`Otros activos financieros`) %>%
      hc_chart(zoomType = "xy")
    
    ECO_AM 
    
  })
  
  output$clasi_ECO2_titulo_var = renderHighchart ({
    
    ECO_AM <- tabla.13.1.F.M  %>%
      filter(`Título` == input$titulo.9) %>%
      filter(CE1 == input$eco1.8)
    
    
    ECO_AM <- mutate(ECO_AM,
                     var.GastosConsumo = round((`Gastos de consumo`/lag(`Gastos de consumo`,n = 12)-1)*100,1),
                     var.TransferenciasCorrientes = round((`Transferencias corrientes`/lag(`Transferencias corrientes`,n = 12)-1)*100,1),
                     var.Intereses = round((`Intereses`/lag(`Intereses`,n = 12)-1)*100,1),
                     var.Formacion.Capital = round((`Formación de capital`/lag(`Formación de capital`,n = 12)-1)*100,1),
                     var.Transferencias.Capital = round((`Transferencias de capital`/lag(`Transferencias de capital`,n = 12)-1)*100,1),
                     var.Adquisición.Activos = round((`Adquisición de activos`/lag(`Adquisición de activos`,n = 12)-1)*100,1),
            #         var.Amortización = round((`Amortización`/lag(`Amortización`,n = 12)-1)*100,1),
                     var.Adquisición.Valores = round((`Adquisición de valores`/lag(`Adquisición de valores`,n = 12)-1)*100,1),
                     var.Otros.AF = round((`Otros activos financieros`/lag(`Otros activos financieros`,n = 12)-1)*100,1)
    )
    
    
    ECO_AM <- mutate(ECO_AM, 
                     acum_12.GastosConsumo = roll_sum(ECO_AM$`Gastos de consumo`, 12, align = "right", fill = NA),
                     var.acum.GastosConsumo  = round((acum_12.GastosConsumo/lag(acum_12.GastosConsumo,n = 12)-1)*100,1),
                     
                     acum_12.TransferenciasCorrientes = roll_sum(ECO_AM$`Transferencias corrientes`, 12, align = "right", fill = NA),
                     var.acum.TransferenciasCorrientes  = round((acum_12.TransferenciasCorrientes/lag(acum_12.TransferenciasCorrientes,n = 12)-1)*100,1),
                     
                     acum_12.Intereses = roll_sum(ECO_AM$`Intereses`, 12, align = "right", fill = NA),
                     var.acum.Intereses  = round((acum_12.Intereses/lag(acum_12.Intereses,n = 12)-1)*100,1),
                     
                     acum_12.Formacion.Capital = roll_sum(ECO_AM$`Formación de capital`, 12, align = "right", fill = NA),
                     var.acum.Formacion.Capital  = round((acum_12.Formacion.Capital/lag(acum_12.Formacion.Capital,n = 12)-1)*100,1),
                     
                     var.Transferencias.Capital = roll_sum(ECO_AM$`Transferencias de capital`, 12, align = "right", fill = NA),
                     var.acum.Transferencias.Capital  = round((var.Transferencias.Capital/lag(var.Transferencias.Capital,n = 12)-1)*100,1),
                     
                     var.Adquisición.Activos = roll_sum(ECO_AM$`Adquisición de valores`, 12, align = "right", fill = NA),
                     var.acum.Adquisición.Activos  = round((var.Adquisición.Activos/lag(var.Adquisición.Activos,n = 12)-1)*100,1),
                     
     #                var.Amortización = roll_sum(ECO_AM$`Amortización`, 12, align = "right", fill = NA),
     #                var.acum.Amortización  = round((var.Amortización/lag(var.Amortización,n = 12)-1)*100,1),
     #                
                     var.Adquisición.Valores = roll_sum(ECO_AM$`Adquisición de valores`, 12, align = "right", fill = NA),
                     var.acum.Adquisición.Valores  = round((var.Adquisición.Valores/lag(var.Adquisición.Valores,n = 12)-1)*100,1),
                     
                     var.Otros.AF = roll_sum(ECO_AM$`Otros activos financieros`, 12, align = "right", fill = NA),
                     var.acum.Otros.AF  = round((var.Otros.AF/lag(var.Otros.AF,n = 12)-1)*100,1)
    )
    
    
    
    
    
    
    ECO_AM  <- highchart() %>% 
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
      
      hc_xAxis(categories = ECO_AM$Fecha) %>% 
      
      hc_add_series(name = "var.GastosConsumo",      data = ECO_AM$`var.GastosConsumo`) %>% 
      hc_add_series(name = "var.acum.GastosConsumo", data = ECO_AM$`var.acum.GastosConsumo`) %>% 
      
      hc_add_series(name = "var.TransferenciasCorrientes", data = ECO_AM$`var.TransferenciasCorrientes`) %>% 
      hc_add_series(name = "var.acum.TransferenciasCorrientes", data = ECO_AM$`var.acum.TransferenciasCorrientes`) %>% 
      
      hc_add_series(name = "var.Intereses", data = ECO_AM$`var.Intereses`) %>% 
      hc_add_series(name = "var.acum.Intereses", data = ECO_AM$`var.acum.Intereses`) %>% 
      
      hc_add_series(name = "var.Formacion.Capital", data = ECO_AM$`var.Formacion.Capital`) %>% 
      hc_add_series(name = "var.acum.Formacion.Capital", data = ECO_AM$`var.acum.Formacion.Capital`) %>%
      
      hc_add_series(name = "var.Transferencias.Capital", data = ECO_AM$`var.Transferencias.Capital`) %>% 
      hc_add_series(name = "var.acum.Transferencias.Capital", data = ECO_AM$`var.acum.Transferencias.Capital`) %>%
      
 #     hc_add_series(name = "var.Amortización", data = ECO_AM$`var.Amortización`) %>% 
 #     hc_add_series(name = "var.acum.Amortización", data = ECO_AM$`var.acum.Amortización`) %>%
      
      hc_add_series(name = "var.Adquisición.Valores", data = ECO_AM$`var.Adquisición.Valores`) %>% 
      hc_add_series(name = "var.acum.Adquisición.Valores", data = ECO_AM$`var.acum.Adquisición.Valores`) %>%
      
      hc_add_series(name = "var.Otros.AF", data = ECO_AM$`var.Otros.AF`) %>% 
      hc_add_series(name = "var.acum.Otros.AF", data = ECO_AM$`var.acum.Otros.AF`) %>%
      
      hc_chart(zoomType = "xy")
    
    ECO_AM 
    
    
  })
  
  ############################
  #      Titulo ECO.2 - M    #
  ############################
  
  ###########################################################
  #                        Indicadores                      #
  ###########################################################
  
  ################
  #  Acumulado   #
  ################
  
  
  output$index7 <- renderValueBox({
    
    ind1 <- round(indicador_1/Millones,1)
    
    ind1 <-  ind1 %>% 
      mutate( `Presupuestado a la fecha`= number(ind1$`Presupuestado a la fecha`, accuracy = .1, big.mark = ".", decimal.mark = ",")
              
      )
    
    # ind1 <- 
    
    valueBox(value= ind1, subtitle=paste0("Monto presupuestado para el ", Año_actual, " (en millones)"),
             icon = icon("money"), color = "purple", width = 3)
  })
  
  output$index2.2 <- renderValueBox({
    
    ind2 <- round(indicador_2/Millones,1)
    
    ind2 <-  ind2 %>% 
      mutate( `Ejecutado a la fecha`= number(ind2$`Ejecutado a la fecha`, accuracy = .1, big.mark = ".", decimal.mark = ",")
              
      )
    
    valueBox(value= ind2, subtitle=paste0("Monto total ejecutado a la fecha ", Sys.Date(), " (en millones)"),
             icon = icon("money"), color = "purple", width = 3)
    
  })
  
  output$index3.2 <- renderValueBox({
    
    ind3 <- round(indicador_3/Millones,1)
    
    ind3 <-  ind3 %>% 
      mutate( `Ejecutado a la fecha anterior`= number(ind3$`Ejecutado a la fecha anterior`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    valueBox(value= ind3, subtitle=paste0("Monto total ejecutado para el ", Año_pasado_1, " (en millones)"),
             icon = icon("money"), color = "purple", width = 3)
    
  })
  
  
  output$index4 <- renderValueBox({
    
    ind4 <- indicador_4
    
    valueBox(
      paste0(round(ind4,1),"%"), paste0("Porcentaje de ejecucion al ", Sys.Date()), icon = icon("money"),
      color = "purple", width = 3
    )
  })
  
  output$index5 <- renderValueBox({
    
    ind5 <- indicador_5
    
    valueBox(
      paste0(round(ind5,1),"%"), paste0("Porcentaje de variacion en la ejecucion a la fecha ", Sys.Date()), icon = icon("money"),
      color = "red", width = 3
    )
  })
  
  output$index6 <- renderValueBox({
    
    ind6 <- indicador_6
    
    valueBox(
      paste0(ind6), paste0(""), icon = icon(""),
      color = "blue", width = 3, 
    )
  })
  
  
  ####################################
  #   Gasto: nominal - %_variación   #
  ####################################
  
  #############
  #  Nominal  #
  #############
  
  output$tabla_ind1 <- renderReactable ({
    
    Egresos_GC_GC_mensual <- tabla_5
    
    Egresos_GC_GC_mensual <- as.data.frame(Egresos_GC_GC_mensual)
    
    c <- spread(Egresos_GC_GC_mensual, Año, round(Ejecutado,1))
    
    
    c <- c %>% 
      arrange(mes.cod)
    
    c <- c %>% 
      select(mes,`2017`,`2018`,`2019`,`2020`)
    
    c <- c %>%
      mutate(
        `2017` = number(c$`2017`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `2018` = number(c$`2018`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `2019` = number(c$`2019`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `2020` = number(c$`2020`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    
    reactable(c) 
  })
  
  #################
  #  % Variacion  #
  #################
  
  output$tabla_ind2 <- renderReactable ({
    
    Egresos_GC_GC_mensual <- tabla_5
    
    Egresos_GC_GC_mensual <- as.data.frame(Egresos_GC_GC_mensual)
    
    Egresos_GC_GC_mensual <- Egresos_GC_GC_mensual %>% 
      dplyr::mutate( var.Ejecutado = round( (Ejecutado/lag(Ejecutado,n = 12)-1)*100,1)) %>%
      dplyr::select(Año, mes.cod, mes, var.Ejecutado)
    
    
    d <- spread(Egresos_GC_GC_mensual, Año, var.Ejecutado)
    
    d  <- d %>% 
      arrange(mes.cod)
    
    d  <- d %>% 
      select(mes,`2017`,`2018`,`2019`,`2020`)
    
    reactable(d)
  })
  
  ######################
  #  Nominal Acumulado #
  ######################
  
  output$tabla_ind3 <- renderReactable ({
    
    Egresos_GC_GC_mensual <- tabla_5
    
    Egresos_GC_GC_mensual <- as.data.frame(Egresos_GC_GC_mensual)
    
    Egresos_GC_GC_mensual <- Egresos_GC_GC_mensual %>%
      mutate (
        acum_12 = roll_sum(Egresos_GC_GC_mensual$Ejecutado, 12, align = "right", fill = NA) #,
        #  var.acum_12  = round((acum_12/lag(acum_12,n = 12)-1)*100,1)
      ) %>%
      dplyr::arrange(Año,mes.cod) %>%
      dplyr::select(Año,mes,acum_12) 
    
    c <- Egresos_GC_GC_mensual %>%  
      pivot_wider(
        names_from = "Año", 
        values_from = "acum_12") 
    
    c <- c %>% 
      select(mes,`2017`,`2018`,`2019`,`2020`)
    
    c <- c %>%
      mutate(
        `2017` = number(c$`2017`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `2018` = number(c$`2018`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `2019` = number(c$`2019`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `2020` = number(c$`2020`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    
    reactable(c) 
  })
  
  ########################
  #  Variacion Acumulada #
  ########################
  
  output$tabla_ind4 <- renderReactable ({
    
    Egresos_GC_GC_mensual <- tabla_5
    
    Egresos_GC_GC_mensual <- as.data.frame(Egresos_GC_GC_mensual)
    
    Egresos_GC_GC_mensual <- Egresos_GC_GC_mensual %>%
      mutate (
        acum_12 = roll_sum(Egresos_GC_GC_mensual$Ejecutado, 12, align = "right", fill = NA) ,
        var.acum_12  = round((acum_12/lag(acum_12,n = 12)-1)*100,1)
      ) %>%
      dplyr::arrange(Año,mes.cod) %>%
      dplyr::select(Año,mes,var.acum_12) 
    
    c <- Egresos_GC_GC_mensual %>%  
      pivot_wider(
        names_from = "Año", 
        values_from = "var.acum_12") 
    
    c <- c %>% 
      select(mes,`2017`,`2018`,`2019`,`2020`)
    
    c <- c %>%
      mutate(
        `2017` = number(c$`2017`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `2018` = number(c$`2018`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `2019` = number(c$`2019`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `2020` = number(c$`2020`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    
    reactable(c) 
  })
  
  
  ############################################
  #              Pronóstico                  #
  ############################################
  
  ######################################
  #    Proyeccion del gasto en el GC   #
  ######################################
  
  output$forecast1 <- renderHighchart({
    
    
    Egresos_GC_GC_mensual_anual <- tabla_6
    
    Egresos_GC_GC_mensual_anual <- Egresos_GC_GC_mensual_anual[-c(dim(Egresos_GC_GC_mensual_anual)[1]),]
    
    serie1 <- ts(Egresos_GC_GC_mensual_anual$Ejecutado, frequency=12, start=c(2007,1))
    
    modelo <- Arima(serie1, order=c(1,1,1), seasonal=c(0,1,1))
    
    x1 <- forecast(modelo, h = input$horizonte.1, level = 95)
    
    X1<-hchart(serie1,name = "Gastos") %>% 
      hc_add_series(name = "Estimacion",fitted(modelo), type = "line", color="green") %>% 
      hc_add_series(name = "Pronostico", x1$mean, color="black") %>%
      
      hc_title(text = "",
               margin = 20, align = "center",
               style = list(color = "#black", useHTML = TRUE, fontWeight = "bold")) %>%
      hc_yAxis(title = list(text = "Colones"),
               labels = list(format = "{value}"))%>% 
      hc_xAxis(title = list(text = "Años") )  %>% 
      
      hc_tooltip(crosshairs = T,valueDecimals = 1, shared = TRUE, borderWidth = 5) %>%
      hc_chart(
        zoomType = "xy"
      )
    X1
    
  })
  
  output$tabla.forcast.1 <- function() ({
    
    Egresos_GC_GC_mensual_anual <- tabla_6
    
    Egresos_GC_GC_mensual_anual <- Egresos_GC_GC_mensual_anual[-c(dim(Egresos_GC_GC_mensual_anual)[1]),]
    
    serie1 <- ts(Egresos_GC_GC_mensual_anual$Ejecutado, frequency=12, start=c(2007,1))
    
    modelo <- Arima(serie1, order=c(1,1,1), seasonal=c(0,1,1))
    
    
    
    
    x1 <- forecast(modelo, h = input$horizonte.1, level = 95)
    
    pronosticos <- x1 %>%
      as.data.frame() %>%
      dplyr::rename(
        Proyeccion =`Point Forecast`,
        `Limite inferior` =`Lo 95`,
        `Limite superior` =`Hi 95`
      ) %>% mutate(
        Proyeccion = round(Proyeccion/Millones,1),
        `Limite inferior` = round(`Limite inferior`/Millones,1),
        `Limite superior` = round(`Limite superior`/Millones,1)
      )
    
    pronosticos <- pronosticos %>%
      mutate(
        `Proyeccion` = number(pronosticos$`Proyeccion`,           accuracy = .1, big.mark = ".", decimal.mark = ","),
        `Limite inferior` = number(pronosticos$`Limite inferior`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `Limite superior` = number(pronosticos$`Limite superior`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    pronosticos %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(c("Proyección mensual del gasto corriente."= 4)) %>%
      footnote(general = paste0("SIGAF - Ministerio de Hacienda."),
               general_title = "Fuente: ", 
               footnote_as_chunk = T)
    
  })
  
  #######################################
  #  Resumen de los pronósticos del GC  #
  #######################################
  
  output$Ejecutado1_1 = renderReactable({
    
    Egresos_GC_GC_mensual_anual <- tabla_6
    
    serie1 <- ts(Egresos_GC_GC_mensual_anual$Ejecutado, frequency=12, start=c(2007,1))
    
    modelo <- Arima(serie1, order=c(1,1,1), seasonal=c(0,1,1))
    
    x1 <- forecast(modelo, h = input$horizonte.1, level = 95)
    
    pronosticos <- x1 %>%
      as.data.frame() %>%
      dplyr::rename(
        Proyeccion =`Point Forecast`,
        `Limite inferior` =`Lo 95`,
        `Limite superior` =`Hi 95`
      ) %>% mutate(
        Proyeccion = round(Proyeccion/Millones,1),
        `Limite inferior` = round(`Limite inferior`/Millones,1),
        `Limite superior` = round(`Limite superior`/Millones,1)
      )
    
    Gasto.1 <- tabla_7
    
    Gasto.1 <- mutate(Gasto.1,
                      Ejecutado = round(Ejecutado,1)        
    )
    
    Gasto.1 <- as.data.frame(Gasto.1)
    
    Gastos.2 <- pronosticos %>% 
      select(Proyeccion) %>% 
      dplyr::rename(
        Ejecutado = Proyeccion
      )
    
    Gastos_mensual <- rbind.data.frame(Gasto.1,Gastos.2)
    
    #      Gastos_mensual <- Gastos_mensual %>%
    #                            mutate(
    #                              `Ejecutado` = number(Gastos_mensual$`Ejecutado`, accuracy = .1, big.mark = ".", decimal.mark = ",")
    #                            )
    #      
    #      reactable(Gastos_mensual)      
    
    
    # -------> NUEVO      
    
    a <- dim(Gastos_mensual)[1]
    
    mes.cod <- seq(1:a)
    
    n_meses <- as.data.frame(mes.cod)
    
    Gastos_mensual_ac <- data.frame(n_meses, Gastos_mensual)
    
    Gastos_mensual_ac  <- mutate(Gastos_mensual_ac,
                                 
                                 Mes = case_when(
                                   mes.cod == 1 ~ "Enero",
                                   mes.cod == 2 ~ "Febrero",
                                   mes.cod == 3 ~ "Marzo",
                                   mes.cod == 4 ~ "Abril",
                                   mes.cod == 5 ~ "Mayo",
                                   mes.cod == 6 ~ "Junio",
                                   mes.cod == 7 ~ "Julio",
                                   mes.cod == 8 ~ "Agosto",
                                   mes.cod == 9 ~ "Septiembre",
                                   mes.cod == 10 ~ "Octubre",
                                   mes.cod == 11 ~ "Noviembre",
                                   mes.cod == 12 ~ "Diciembre"
                                 ),
                                 Ejecutado = number(Gastos_mensual_ac$`Ejecutado`, accuracy = .1, big.mark = ".", decimal.mark = ",")
    ) %>%
      select(Mes, Ejecutado)
    
    
    datos <- data.frame(        Mes =  Gastos_mensual_ac$Mes,
                                Ejecucion = Gastos_mensual_ac$Ejecutado)
    
    reactable(datos)
    
    
    #reactable(Gastos_mensual_ac)
    
  })
  
  
  
  output$Ejecutado1_2 = renderValueBox({
    
    Egresos_GC_GC_mensual_anual <- tabla_6
    
    
    serie1 <- ts(Egresos_GC_GC_mensual_anual$Ejecutado, frequency=12, start=c(2007,1))
    
    modelo <- Arima(serie1, order=c(1,1,1), seasonal=c(0,1,1))
    
    x1 <- forecast(modelo, h = input$horizonte.1, level = 95)
    
    pronosticos <- x1 %>%
      as.data.frame() %>%
      dplyr::rename(
        Proyeccion =`Point Forecast`,
        `Limite inferior` =`Lo 95`,
        `Limite superior` =`Hi 95`
      ) %>% mutate(
        Proyeccion = round(Proyeccion/Millones,1),
        `Limite inferior` = round(`Limite inferior`/Millones,1),
        `Limite superior` = round(`Limite superior`/Millones,1)
      )
    
    Gasto.1 <- tabla_7
    
    Gasto.1 <- mutate(Gasto.1,
                      Ejecutado = round(Ejecutado,1)        
    )
    
    Gasto.1 <- as.data.frame(Gasto.1)
    
    Gastos.2 <- pronosticos %>% 
      select(Proyeccion) %>% 
      dplyr::rename(
        Ejecutado = Proyeccion
      )
    
    Gastos_mensual <- rbind.data.frame(Gasto.1,Gastos.2)
    
    Gastos_pronosticado <- Gastos_mensual %>% 
      dplyr::summarise ('Ejeccion anual' = sum(`Ejecutado`, na.rm = TRUE))
    
    Gastos_pronosticado <- as.data.frame(round(Gastos_pronosticado,1))
    
    Gastos_pronosticado <- Gastos_pronosticado %>%
      mutate(
        `Ejeccion anual` = number(Gastos_pronosticado$`Ejeccion anual`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    Gastos_pronosticado
    
    valueBox(
      paste0(Gastos_pronosticado,1), paste0("Gasto total proyectado para el ", Año_actual, " (millones)"), icon = icon("money"),
      color = "blue", width = 4
    )
  })
  
  ########################################
  #   Proyeccion del gasto por Titulo    #
  ########################################
  
  output$forecast2 <- renderHighchart({
    
    Egresos_GC_GC_mensual <- tabla_8 %>%
      dplyr::filter(Titulo == input$titulo.3)
    
    
    
    
    serie1 <- ts(Egresos_GC_GC_mensual$Ejecutado, frequency=12, start=c(2007,1))
    
    modelo <- Arima(serie1, order=c(1,1,1), seasonal=c(0,1,1))
    
    x1 <- forecast(modelo, h = input$horizonte.2, level = 95)
    
    X2<-hchart(x1)%>% 
      hc_add_series(name = "Estimacion",fitted(modelo), color="green")%>%  
      hc_title(text = "",
               margin = 20, align = "center",
               style = list(color = "#black", useHTML = TRUE, fontWeight = "bold")) %>%
      hc_yAxis(title = list(text = "Colones"),
               labels = list(format = "{value}"))%>% 
      hc_tooltip(crosshairs = T,valueDecimals = 1, shared = TRUE, borderWidth = 5) %>%
      hc_chart(
        zoomType = "xy"
      )
    X2
  })
  
  output$tabla.forcast.2 <- function() ({
    
    Egresos_GC_GC_mensual <- tabla_8 %>%
      dplyr::filter(Titulo == input$titulo.3)
    
    serie1 <- ts(Egresos_GC_GC_mensual$Ejecutado, frequency=12, start=c(2007,1))
    
    modelo <- Arima(serie1, order=c(1,1,1), seasonal=c(0,1,1))
    
    x1 <- forecast(modelo, h = input$horizonte.2, level = 95)
    
    pronosticos2 <- x1 %>%
      as.data.frame() %>%
      dplyr::rename(
        Proyeccion =`Point Forecast`,
        `Limite inferior` =`Lo 95`,
        `Limite superior` =`Hi 95`
      ) %>% mutate(
        Proyeccion = round(Proyeccion/Millones,1),
        `Limite inferior` = round(`Limite inferior`/Millones,1),
        `Limite superior` = round(`Limite superior`/Millones,1)
      )
    
    pronosticos2 <- pronosticos2 %>%
      mutate(
        `Proyeccion` = number(pronosticos2$`Proyeccion`,           accuracy = .1, big.mark = ".", decimal.mark = ","),
        `Limite inferior` = number(pronosticos2$`Limite inferior`, accuracy = .1, big.mark = ".", decimal.mark = ","),
        `Limite superior` = number(pronosticos2$`Limite superior`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    pronosticos2 %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(c("Proyeccion del gasto corriento (en millones)"= 4)) %>%
      footnote(general = paste0("SIGAF - Ministerio de Hacienda"),
               general_title = "Fuente: ", 
               footnote_as_chunk = T)
    
  })
  
  ########################################
  #   Resumen del gasto por Titulo       #
  ########################################
  
  ###############################
  #  Resumen del Gasto estimado #
  ###############################
  
  output$Ejecutado2_1 = renderReactable({
    
    Egresos_GC_GC_mensual_anual <-  tabla_8 %>%
      filter(Año >= Años_analisis) %>%
      dplyr::filter(Titulo == input$titulo.4)  %>%
      arrange(Año,mes.cod)
    
    serie1 <- ts(Egresos_GC_GC_mensual_anual$Ejecutado, frequency=12, start=c(2007,1))
    
    modelo <- Arima(serie1, order=c(1,1,1), seasonal=c(0,1,1))
    
    x1 <- forecast(modelo, h = input$horizonte.3, level = 95)
    
    pronosticos <- x1 %>%
      as.data.frame() %>%
      dplyr::rename(
        Proyeccion =`Point Forecast`,
        `Limite inferior` =`Lo 95`,
        `Limite superior` =`Hi 95`
      ) %>% mutate(
        Proyeccion = round(Proyeccion/Millones,1),
        `Limite inferior` = round(`Limite inferior`/Millones,1),
        `Limite superior` = round(`Limite superior`/Millones,1)
      )
    
    pronosticos
    
    Gasto.1 <- tabla_9  %>%
      dplyr::filter(Titulo == input$titulo.4)        
    
    Gasto.1 <- mutate(Gasto.1,
                      Ejecutado = round(Ejecutado,1) ) %>%
      select(Ejecutado)
    
    Gasto.1 <- as.data.frame(Gasto.1)
    
    Gastos.2 <- pronosticos %>% 
      select(Proyeccion) %>% 
      dplyr::rename(
        Ejecutado = Proyeccion
      )
    
    Gastos_mensual <- rbind.data.frame(Gasto.1,Gastos.2)
    
    #     Gastos_mensual <- Gastos_mensual %>%
    #                                    mutate(
    #                                      Ejecutado = number(Gastos_mensual$`Ejecutado`, accuracy = .1, big.mark = ".", decimal.mark = ",")
    #                                    )
    #     
    #     reactable(Gastos_mensual)
    
    a <- dim(Gastos_mensual)[1]
    
    mes.cod <- seq(1:a)
    
    n_meses <- as.data.frame(mes.cod)
    
    Gastos_mensual_ac <- data.frame(n_meses, Gastos_mensual)
    
    Gastos_mensual_ac  <- mutate(Gastos_mensual_ac,
                                 
                                 Mes = case_when(
                                   mes.cod == 1 ~ "Enero",
                                   mes.cod == 2 ~ "Febrero",
                                   mes.cod == 3 ~ "Marzo",
                                   mes.cod == 4 ~ "Abril",
                                   mes.cod == 5 ~ "Mayo",
                                   mes.cod == 6 ~ "Junio",
                                   mes.cod == 7 ~ "Julio",
                                   mes.cod == 8 ~ "Agosto",
                                   mes.cod == 9 ~ "Septiembre",
                                   mes.cod == 10 ~ "Octubre",
                                   mes.cod == 11 ~ "Noviembre",
                                   mes.cod == 12 ~ "Diciembre"
                                 ),
                                 Ejecutado = number(Gastos_mensual_ac$`Ejecutado`, accuracy = .1, big.mark = ".", decimal.mark = ",")
    ) %>%
      select(Mes, Ejecutado)
    
    
    datos <- data.frame(        Mes =  Gastos_mensual_ac$Mes,
                                Ejecucion = Gastos_mensual_ac$Ejecutado)
    
    reactable(datos)
    
    
  })
  
  ########################
  # Gasto total estimado #
  ########################
  
  output$Ejecutado2_2 = renderValueBox({
    
    Egresos_GC_GC_mensual_anual <-  tabla_8 %>%
      filter(Año >= Años_analisis) %>%
      dplyr::filter(Titulo == input$titulo.4)  %>%
      arrange(Año,mes.cod)
    
    
    serie1 <- ts(Egresos_GC_GC_mensual_anual$Ejecutado, frequency=12, start=c(2007,1))
    
    modelo <- Arima(serie1, order=c(1,1,1), seasonal=c(0,1,1))
    
    x1 <- forecast(modelo, h = input$horizonte.3, level = 95)
    
    pronosticos <- x1 %>%
      as.data.frame() %>%
      dplyr::rename(
        Proyeccion =`Point Forecast`,
        `Limite inferior` =`Lo 95`,
        `Limite superior` =`Hi 95`
      ) %>% mutate(
        Proyeccion = round(Proyeccion/Millones,1),
        `Limite inferior` = round(`Limite inferior`/Millones,1),
        `Limite superior` = round(`Limite superior`/Millones,1)
      )
    
    pronosticos
    
    Gasto.1 <- tabla_9  %>%
      dplyr::filter(Titulo == input$titulo.4)        
    
    Gasto.1 <- mutate(Gasto.1,
                      Ejecutado = round(Ejecutado,1) ) %>%
      select(Ejecutado)
    
    Gasto.1 <- as.data.frame(Gasto.1)
    
    Gastos.2 <- pronosticos %>% 
      select(Proyeccion) %>% 
      dplyr::rename(
        Ejecutado = Proyeccion
      )
    
    Gastos_mensual <- rbind.data.frame(Gasto.1,Gastos.2)
    
    Gastos_mensual
    
    Gastos_pronosticado <- Gastos_mensual %>% 
      dplyr::summarise (`Ejeccion anual` = sum(`Ejecutado`, na.rm = TRUE))
    
    
    Gastos_pronosticado <- as.data.frame(Gastos_pronosticado)
    
    Gastos_pronosticado <- Gastos_pronosticado %>%
      mutate (
        `Ejeccion anual` = number(Gastos_pronosticado$`Ejeccion anual`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    valueBox(
      paste0(Gastos_pronosticado), paste0("Gasto total para el ", Año_actual, " (millones)"), icon = icon("money"),
      color = "blue", width = 4
    )
  })
  
  ######################################################################
  ######################################################################
  #                             Alertas                                #
  ######################################################################
  ######################################################################
  
  output$gastoactual = renderReactable({
    
    # a.1 Gasto inicial 2020
    
    ind1 <- round(indicador_1/Millones,1)
    
    
    # a.2 Ejecución a la fecha
    
    ind2 <- indicador_2
    
    ind2 <- round(ind2/Millones,1)
    
    # a.3 % de ejecución
    
    ind4 <- indicador_4
    
    ind4 <- round(ind4,1)
    
    # a.4 Ejecución 2019
    
    ind3 <- indicador_3
    
    ind3 <- round(ind3/Millones,1)
    
    # a.5 Var % de la ejecución actual
    
    ind5 <- indicador_5
    
    
    ###########
    # Unión 1 #
    ###########
    
    actual <- cbind.data.frame(ind1,ind2,ind4,ind3,ind5)
    
    reactable(actual)
    
  })
  
  # -----------------------------------------------------------------------------------------------------  
  
  output$gasto_actual = renderReactable({
    
    # a.1 Gasto inicial 2020
    
    ind1 <- round(indicador_1/Millones,1)
    
    
    # a.2 Ejecución a la fecha
    
    ind2 <- indicador_2
    
    ind2 <- round(ind2/Millones,1)
    
    # a.3 % de ejecución
    
    ind4 <- indicador_4
    
    ind4 <- round(ind4,1)
    
    # a.4 Ejecución 2019
    
    ind3 <- indicador_3
    
    ind3 <- round(ind3/Millones,1)
    
    # a.5 Var % de la ejecución actual
    
    ind5 <- round(indicador_5,1)
    
    
    ###########
    # Unión 1 #
    ###########
    
    actual <- cbind.data.frame(ind1,ind2,ind4,ind3,ind5)
    
    
    
    actual <- actual %>%
      mutate(
        `Presupuestado a la fecha`      = number(actual$`Presupuestado a la fecha`,      accuracy = .1, big.mark = ".", decimal.mark = ","),
        `Ejecutado a la fecha`          = number(actual$`Ejecutado a la fecha`,          accuracy = .1, big.mark = ".", decimal.mark = ","),
        `Ejecutado a la fecha anterior` = number(actual$`Ejecutado a la fecha anterior`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    reactable(actual)
    
  })
  
  # -----------------------------------------------------------------------------------------------------    
  
  output$gastopronosticado = renderReactable({
    
    Egresos_GC_GC_mensual_anual <- tabla_6
    
    
    serie1 <- ts(Egresos_GC_GC_mensual_anual$Ejecutado, frequency=12, start=c(2007,1))
    
    modelo <- Arima(serie1, order=c(1,1,1), seasonal=c(0,1,1))
    
    x1 <- forecast(modelo, h = input$horizonte.1, level = 95)
    
    pronosticos <- x1 %>%
      as.data.frame() %>%
      dplyr::rename(
        Proyeccion =`Point Forecast`,
        `Limite inferior` =`Lo 95`,
        `Limite superior` =`Hi 95`
      ) %>% mutate(
        Proyeccion = round(Proyeccion/Millones,1),
        `Limite inferior` = round(`Limite inferior`/Millones,1),
        `Limite superior` = round(`Limite superior`/Millones,1)
      )
    
    Gasto.1 <- tabla_7
    
    Gasto.1 <- as.data.frame(Gasto.1)
    
    Gastos.2 <- pronosticos %>% 
      select(Proyeccion) %>% 
      dplyr::rename(
        Ejecutado = Proyeccion
      )
    
    Gastos_mensual <- rbind.data.frame(Gasto.1,Gastos.2)
    
    Gastos_pronosticado <- Gastos_mensual %>% 
      dplyr::summarise ('Gasto anual pronosticado' = sum(`Ejecutado`, na.rm = TRUE))
    
    Gastos_pronosticado <- round(Gastos_pronosticado,1)
    
    #
    
    # b.2 % de ejecución 2020
    
    ind1 <-  indicador_1
    
    ind1 <- ind1/Millones
    
    ind7 <- round(Gastos_pronosticado/ind1*100,1)
    
    ind7 <- ind7 %>%
      dplyr::rename(
        "Ejecucion estimada (%)"  = "Gasto anual pronosticado"
      )
    #
    ind3 <- indicador_3
    
    ind3 <- round(ind3/Millones,1)
    
    # b.3 Var % de la ejecución actual2020
    
    ind8 <- round((Gastos_pronosticado/ind3-1)*100,1)
    
    ind8 <- ind8 %>%
      dplyr::rename(
        "Variacion anula"  = "Gasto anual pronosticado"
      )
    
    pronosticado <- cbind.data.frame(Gastos_pronosticado,ind7, ind3,ind8)
    
    
    pronosticado <- pronosticado %>%
      mutate (
        `Gasto anual pronosticado`      = number(pronosticado$`Gasto anual pronosticado`,      accuracy = .1, big.mark = ".", decimal.mark = ","),
        `Ejecutado a la fecha anterior` = number(pronosticado$`Ejecutado a la fecha anterior`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    reactable(pronosticado)
    
  })
  
  output$FR.Actual <- renderValueBox({
    
    ind6 <- indicador_6 
    
    valueBox(
      paste0(ind6), paste0(""),
      color = "blue", width = 3
    )
  })
  
  output$FR.Pronostico <- renderValueBox({
    
    Egresos_GC_GC_mensual_anual <- tabla_6
    
    
    serie1 <- ts(Egresos_GC_GC_mensual_anual$Ejecutado, frequency=12, start=c(2007,1))
    
    modelo <- Arima(serie1, order=c(1,1,1), seasonal=c(0,1,1))
    
    x1 <- forecast(modelo, h = input$horizonte.1, level = 95)
    
    pronosticos <- x1 %>%
      as.data.frame() %>%
      dplyr::rename(
        Proyeccion =`Point Forecast`,
        `Limite inferior` =`Lo 95`,
        `Limite superior` =`Hi 95`
      ) %>% mutate(
        Proyeccion = round(Proyeccion/Millones,1),
        `Limite inferior` = round(`Limite inferior`/Millones,1),
        `Limite superior` = round(`Limite superior`/Millones,1)
      )
    
    Gasto.1 <- tabla_7
    
    Gasto.1 <- as.data.frame(Gasto.1)
    
    Gastos.2 <- pronosticos %>% 
      select(Proyeccion) %>% 
      dplyr::rename(
        Ejecutado = Proyeccion
      )
    
    Gastos_mensual <- rbind.data.frame(Gasto.1,Gastos.2)
    
    Gastos_pronosticado <- Gastos_mensual %>% 
      dplyr::summarise ('Gasto anual pronosticado' = sum(`Ejecutado`, na.rm = TRUE))
    
    Gastos_pronosticado <- round(Gastos_pronosticado,1)
    
    #
    
    # b.2 % de ejecución 2020
    
    ind1 <-  indicador_1
    
    ind1 <- ind1/Millones
    
    ind7 <- round(Gastos_pronosticado/ind1*100,1)
    
    ind7 <- ind7 %>%
      dplyr::rename(
        "Porcentaje de ejecucion estimada"  = "Gasto anual pronosticado"
      )
    #
    ind3 <- indicador_3
    
    ind3 <- round(ind3/Millones,1)
    
    # b.3 Var % de la ejecución actual2020
    
    ind8 <- round((Gastos_pronosticado/ind3-1)*100,1)
    
    ind8 <- ind8 %>%
      dplyr::rename(
        "% de Variacion del gasto anula"  = "Gasto anual pronosticado"
      )
    
    
    ind9 <- mutate(ind8,
                   valoracion = case_when(
                     `% de Variacion del gasto anula` <= RF_GC ~ "SI",
                     `% de Variacion del gasto anula` >  RF_GC  ~"NO"
                   )
    )
    
    ind9 <- ind9  %>%
      select(valoracion)
    
    valueBox(
      paste0(ind9), paste0(""),
      color = "red", width = 3
    )
  })
  
  ####################################
  #   Evolución de la Regla Fiscal   #
  ####################################
  
  ##############################
  #  Bitacora: valores nuevos  #
  ##############################
  
  ###########################################
  #    Informacion del gastos a la fecha    # 
  ###########################################
  
  output$bitacora_agregada = renderReactable({
    
    Bitacora <-  Bitacora %>% 
      select( `Fecha`,`Presupuestado`,`Ejecutado`,`%_Ejecucion`,
              `Variacion`,`Gasto_estimado`,`%_Ejecucion_estimado`,`%_Variacion_anual`
      ) %>% 
      mutate (
        `Gasto_estimado` = as.numeric(Bitacora$`Gasto_estimado`),
        `Fecha` = as.Date(substr(Bitacora$Fecha,1,10)) 
      )
    
    
    Bitacora <- as.data.frame(Bitacora)
    
    Bitacora <- Bitacora %>%
                    dplyr::arrange(Bitacora$`Fecha`)
    
    Bitacora <- Bitacora %>%
                                    dplyr::rename(
                                       "Ejecución (%)" = "%_Ejecucion",
                                       "Variación actual (%)" = "Variacion",
                                       "Ejecucion estimada (%)"   = "%_Ejecucion_estimado",
                                       "Gasto estimado" = "Gasto_estimado",
                                       "Variación estimada (%)" = "%_Variacion_anual"
                                    )
    
 #  Bitacora <- Bitacora %>%
 #                          dplyr::rename(
 #                            `Ejecucion (%)` = ´%_Ejecucion´,
 #                            'Variación % actual (%)' = ´Variacion´,
 #                            'Gasto estimado' = ´Gasto_estimado´,
 #                            'Ejecucion estimada (%)' = ´%_Ejecucion_estimado´
 #                            
 #                          )
    
    Bitacora <-  Bitacora %>% 
      mutate(
        `Presupuestado` = number(Bitacora$`Presupuestado`,   accuracy = .1, big.mark = ".", decimal.mark = ","),
        `Ejecutado` = number(Bitacora$`Ejecutado`,           accuracy = .1, big.mark = ".", decimal.mark = ","), 
        `Gasto estimado` = number(Bitacora$`Gasto estimado`, accuracy = .1, big.mark = ".", decimal.mark = ",")
      )
    
    reactable(Bitacora)
    
    
  })
  
  ##############################
  #         Evolucion          #
  ##############################
  output$bitaevo <- renderHighchart({
    
    
    Evo_bitacora <-Bitacora %>%
      dplyr::select(Fecha, `Variacion`) %>%
      dplyr::mutate( 'Regla Fiscal' = 4.67,
                     Fecha = as.Date(Fecha))
    
    
    Evo_bitacora_chart  <- highchart() %>% 
      hc_title(text = "",
               margin = 20, align = "center",
               style = list(color = "#134", useHTML = TRUE)) %>% 
      hc_legend(align = "left", verticalAlign = "top",
                layout = "vertical", x = 0, y = 100) %>%
      #  hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
      #             shared = TRUE, borderWidth = 5) %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_xAxis(categories = Evo_bitacora$Fecha) %>% 
      hc_add_series(name = "Regla Fiscal", data = Evo_bitacora$`Regla Fiscal`) %>%
      hc_add_series(name = "Variacion", data = Evo_bitacora$`Variacion`) %>% 
      hc_chart(zoomType = "xy")
    
    Evo_bitacora_chart 
  })
  
  output$gastomax <- renderSund2b({
    
    
    gasto_act <- indicador_2$`Ejecutado a la fecha` 
    gasto_2019 <- indicador_3$`Ejecutado a la fecha anterior`
    
    mulrf <- as.numeric(paste0("1.0","467"))
    gasto_max_act <- gasto_2019*mulrf
    
    por_ejec_2020 <- (gasto_max_act - gasto_act) / Millones
    gasto_act <- gasto_act / Millones
    
    datos <- data.frame (
      level1 = c("Ejecutado", "Por ejecutar"),
      size =  c(gasto_act,por_ejec_2020)
      
    )
    
    
    tree <- d3_nest(datos, value_cols = "size")
    
    sb3 <- sund2b(tree, width="100%")
    
    div(
      style="display: flex; align-items:center;",
      sb3
    )
    
    sb3
    
  })
  
}
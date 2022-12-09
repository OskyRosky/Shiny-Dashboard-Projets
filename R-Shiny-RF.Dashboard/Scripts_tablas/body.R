############################
#          body            # 
############################


body <- dashboardBody( 
  tabItems(  
    
    
    #################
    #  Presentacion #   
    #################
    
    tabItem(tabName = "inicio",
            h1("Análisis de la verificación de la Regla Fiscal en el Gobierno Central.", align = "center"),
            br(),
            h2(paste("Fecha de análisis:",  Sys.Date(),".")),
            fluidRow(
              box(
                title = "Referencia del dashboard acerca de la Regla Fiscal.", status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                imageOutput("picture", height = "auto")
              )
            )
            
    ), 
    
    ###########################################
    #      Evolucion de los presupuestos      #
    ###########################################
    
    ####################
    #        Anual     #
    ####################
    
   tabItem(tabName = "HC1",
           h1("Análisis del presupuesto inicial, ajustado y ejecutado en el Gobierno Central.", align = "center"),
           br(),
           h2("Análisis de los presupuestos en términos nominales (en millones)."),
           fluidRow(
             box(
               solidHeader = TRUE, 
               width = 12,
               highchartOutput("HCGC_A0",height = "500px")
             )
           ),
           br(), 
           
           h2("Análisis de los presupuestos en términos nominales, según clasificador económico (en millones)."),
           selectInput("eco1.1", "Seleccione el rubro:",  
                       list(`Tipo` = list("Gastos corrientes",
                                          "Gastos de capital", 
                                          "Transacciones financieras",
                                          "Sumas sin asignación"
                       )
                       )
           ),
           fluidRow(
             box(
               solidHeader = TRUE, 
               width = 12,
               highchartOutput("HCGC_A1",height = "500px")
             )
           ),
             br(), 
             h2("Análisis de los presupuestos, según clasificador económico, en términos de porcentaje de variación."),
             fluidRow(
               box(
                 solidHeader = TRUE, 
                 width = 12,
                 highchartOutput("HCGC_A2",height = "500px")
               )
             )
     ) ,
  
  ####################
  #      Mensual     #
  ####################
  
  tabItem(tabName = "HC2",
          h1("Análisis mensual del presupuesto ejecutado en el Gobierno Central.", align = "center"),
          br(),
          h2("Análisis del gasto mensual en términos nominales (en millones)."),
          br(),
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              highchartOutput("HCGC_B0",height = "500px")
            )
          ),
          
          selectInput("eco1.2", "Seleccione el rubro:",  
                      list(`Tipo` = list("Gastos corrientes",
                                         "Gastos de capital", 
                                         "Transacciones financieras",
                                         "Sumas sin asignación"
                      )
                      )
          ),
          h2("Análisis del gasto mensual en términos nominales, según clasificador económico (en millones)."),
          br(),
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              highchartOutput("HCGC_B1",height = "500px")
            )
          ),
          br(),
          h2("Análisis del porcentaje de varianción mensual de los gastos, según clasificador económico."),
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              highchartOutput("HCGC_B2",height = "500px")
            )
          )
          
  ) ,
  
  ####################
  #  Anual - Titulo  #
  ####################
  
  tabItem(tabName = "HC3",
          h1("Análisis, por título y clasificador económico, del presupuesto inicial, ajustado y ejecutado en el Gobierno Central.", align = "center"),
          br(),
          selectInput("titulo.1", "Seleccione el título:",  
                      list(`Títulos Gobierno Central` = list("Asamblea Legislativa",
                                                             "Contraloría General de la República", 
                                                             "Defensoría de los Habitantes de la República",
                                                             "Ministerio de Agricultura y Ganadería", 
                                                             "Ministerio de Ciencia y Tecnología",
                                                             "Ministerio de Comercio Exterior",
                                                             "Ministerio de Cultura y Juventud",
                                                             "Ministerio de Economía, Industria y Comercio",
                                                             "Ministerio de Educación Pública",
                                                             "Ministerio de Gobernación y Policía",
                                                             "Ministerio de Hacienda",
                                                             "Ministerio de Justicia y Paz",
                                                             "Ministerio de la Presidencia",
                                                             "Ministerio de Obras Públicas y Transportes",
                                                             "Ministerio de Planificación Nacional y Política Económica",
                                                             "Ministerio de Relaciones Exteriores y Culto",
                                                             "Ministerio de Salud",
                                                             "Ministerio de Seguridad Pública",
                                                             "Ministerio de Trabajo y Seguridad Social",
                                                             "Ministerio de Vivienda y Asentamientos Humanos",
                                                             "Ministerio del Ambiente y Energía",
                                                             "Partidas Específicas", 
                                                             "Poder Judicial",
                                                             "Presidencia de la República",
                                                             "Regímenes de Pensiones",
                                                             "Servicio de la Deuda Pública",
                                                             "Tribunal Supremo de Elecciones"
                                                              )
                      )
          ),
          selectInput("eco1.3", "Seleccione el rubro:",  
                      list(`Tipo` = list("Gastos corrientes",
                                         "Gastos de capital", 
                                         "Transacciones financieras",
                                         "Sumas sin asignación"
                      )
                      )
          ),
          br(), 
          h2("Análisis, por título y clasificador, de los presupuestos en términos nominales (en millones)."),
          
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              highchartOutput("HCGC_C1",height = "500px")
            )
          ),
          br(),
          h2("Análisis, por título y clasificador, de los presupuestos en términos de porcentaje de variación."),
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              highchartOutput("HCGC_C2",height = "500px")
            )
          )
  ) ,
 
 ####################
 # Mensual - Titulo #
 ####################
 
 tabItem(tabName = "HC4",
         h1("Análisis mensual, por título, del presupuesto ejecutado en el Gobierno Central.", align = "center"),
         br(), br(),
         selectInput("titulo.2", "Seleccione el titulo:",  
                     list(`Títulos Gobierno Central` = list("Asamblea Legislativa",
                                                            "Contraloría General de la República", 
                                                            "Defensoría de los Habitantes de la República",
                                                            "Ministerio de Agricultura y Ganadería", 
                                                            "Ministerio de Ciencia y Tecnología",
                                                            "Ministerio de Comercio Exterior",
                                                            "Ministerio de Cultura y Juventud",
                                                            "Ministerio de Economía, Industria y Comercio",
                                                            "Ministerio de Educación Pública",
                                                            "Ministerio de Gobernación y Policía",
                                                            "Ministerio de Hacienda",
                                                            "Ministerio de Justicia y Paz",
                                                            "Ministerio de la Presidencia",
                                                            "Ministerio de Obras Públicas y Transportes",
                                                            "Ministerio de Planificación Nacional y Política Económica",
                                                            "Ministerio de Relaciones Exteriores y Culto",
                                                            "Ministerio de Salud",
                                                            "Ministerio de Seguridad Pública",
                                                            "Ministerio de Trabajo y Seguridad Social",
                                                            "Ministerio de Vivienda y Asentamientos Humanos",
                                                            "Ministerio del Ambiente y Energía",
                                                            "Partidas Específicas", 
                                                            "Poder Judicial",
                                                            "Presidencia de la República",
                                                            "Regímenes de Pensiones",
                                                            "Servicio de la Deuda Pública",
                                                            "Tribunal Supremo de Elecciones"
                     )
                     )
         ),
         selectInput("eco1.4", "Seleccione el rubro:",  
                     list(`Tipo` = list("Gastos corrientes",
                                        "Gastos de capital", 
                                        "Transacciones financieras",
                                        "Sumas sin asignación"
                     )
                     )
         ),
          br(),
          h2("Análisis del gasto mensual, por título, en términos nominales (en millones).") ,
         highchartOutput("HCGC_D1",height = "500px"),
          br() ,
         h2("Análisis, por título, del porcentaje de varianción mensual de los gastos."),
         fluidRow(
           box(
             solidHeader = TRUE, 
             width = 12,
             highchartOutput("HCGC_D2",height = "500px")
           )
         )
 ) ,
 
 ######################################################################################
 #                              Analisis por clasificador                             #
 ######################################################################################
 
 
 ###################################
 #           COG General           # 
 ###################################
 
 tabItem(tabName = "detalle01",
         h1("Análisis del gasto, según clasificadores del Objeto Gasto en el Gobierno Central.", align = "center"),
         br(),
         h2("Análisis anual del gasto según el Clasificador de Objeto Gasto (en millones)."),
         br(),
         fluidRow(
           box(
             solidHeader = TRUE, 
             width = 12,
             br(), br(),
             highchartOutput("COG_TOTAL_A_G",height = "500px")
             
           )
         ),
         h2("Análisis mensual del gasto según el Clasificador de Objeto Gasto (en millones)."),
         br(),
         fluidRow(
           box(
             solidHeader = TRUE, 
             width = 12,
             br(), br(),
             highchartOutput("COG_TOTAL_M_G",height = "500px")
             
           )
         ),
         h2("Análisis mensual de la variación segun el Clasificador de Objeto Gasto (en millones)."),
         br(),
         fluidRow(
           box(
             solidHeader = TRUE, 
             width = 12,
             br(), br(),
             highchartOutput("COG_TOTAL_M_G_VAR",height = "500px")
             
           )
         )  
         
 ),
   
   ###################################
   #         ECO.2 General           # 
   ###################################
   
   tabItem(tabName = "detalle02",
           h1("Análisis del gasto, según clasificador ECO.2 en el Gobierno Central.", align = "center"),
           br(),
           h2("Análisis anual del gasto según el Clasificador Económico 2 (en millones)."),
           br(),
           fluidRow(
             box(
               solidHeader = TRUE, 
               width = 12,
               br(), br(),
               highchartOutput("ECO2_TOTAL_AG_CLA",height = "500px")
               
             )
           ),
           br(),
           h2("Análisis mensual del gasto según el Clasificador Económico 2 (en millones)."),
           fluidRow(
             box(
               solidHeader = TRUE, 
               width = 12,
               br(), br(),
               highchartOutput("ECO2_TOTAL_MG_CLA",height = "500px")
               
             )
           ),
           br(),
           h2("Análisis de la variacion mensual del gasto según el Clasificador Económico 2 (en millones)."),
           fluidRow(
             box(
               solidHeader = TRUE, 
               width = 12,
               br(), br(),
               highchartOutput("ECO2_TOTAL_MG_VAR",height = "500px")
               
             )
           )
           
   ),
  
  
  #############################
  #      TOTAL COG  - Filtro  #
  #############################
  
  tabItem(tabName = "detalle1",
          h1("Análisis del gasto anual, según clasificadores en el Gobierno Central.", align = "center"),
          br(),
          
          h2("Análisis anual del gasto según el Clasificador de Objeto Gasto y el económico (en millones)."),
          
          selectInput("eco1.5", "Seleccione el rubro:",  
                      list(`Tipo` = list("Gastos corrientes",
                                         "Gastos de capital", 
                                         "Transacciones financieras",
                                         "Sumas sin asignación"
                      )
                      )
          ),
          
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              br(), br(),
              highchartOutput("COG_TOTAL_A",height = "500px")
              
            )
          ),
          br(),
          h2("Análisis mensual del gasto según el Clasificador de Objeto Gasto y el económico (en millones)."),
           br(),
           h2("Análisis de la variación y variación acumulada en el Clasificador de Objeto Gasto y el económico (en millones)."),
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              br(), br(),
              highchartOutput("COG_TOTAL_VAR",height = "500px")
              
            )
          )
          
     ),
   
  ########################
  #      TOTAL ECO.2     #
  ########################
  
  tabItem(tabName = "detalle2",
          h1("Análisis del gasto según el clasificador Económico 2 en el Gobierno Central.", align = "center"),
          br(),
          h2("Análisis del gasto anual,  según el clasificador Económico 2 (en millones)."),
          selectInput("eco1.6", "Seleccione el rubro:",  
                      list(`Tipo` = list("Gastos corrientes",
                                         "Gastos de capital", 
                                         "Transacciones financieras",
                                         "Sumas sin asignación"
                      )
                      )
          ),
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              
              br(), br(),
              highchartOutput("ECO_TOTAL_A",height = "500px")
              
            )
          ),
          br(),
          h2("Análisis del gasto mensual en el clasificador Económico 2 (en millones)."),
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              br(), br(),
              highchartOutput("ECO_TOTAL_M",height = "500px"),
              reactableOutput("tabla_ECO_TOTAL_M")
              
            )
          ),
          
          h2("Análisis de la variacion y variación acumulada en en el clasificador Económico 2 (en millones)."),
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              br(), br(),
              highchartOutput("ECO_TOTAL_VAR",height = "500px")
              
            )
          )
          
  ),
  
  #######################
  #      Titulo COG     #
  #######################
  
  tabItem(tabName = "detalle3",
          h1("Análisis del gasto, por titulo, según el Clasificador de Objeto Gasto en el Gobierno Central.", align = "center"),
          
          selectInput("titulo.8", "Seleccione el Título de analisis:",  
                      list(`Títulos Gobierno Central` = list("Asamblea Legislativa",
                                                             "Contraloría General de la República", 
                                                             "Defensoría de los Habitantes de la República",
                                                             "Ministerio de Agricultura y Ganadería", 
                                                             "Ministerio de Ciencia y Tecnología",
                                                             "Ministerio de Comercio Exterior",
                                                             "Ministerio de Cultura y Juventud",
                                                             "Ministerio de Economía, Industria y Comercio",
                                                             "Ministerio de Educación Pública",
                                                             "Ministerio de Gobernación y Policía",
                                                             "Ministerio de Hacienda",
                                                             "Ministerio de Justicia y Paz",
                                                             "Ministerio de la Presidencia",
                                                             "Ministerio de Obras Públicas y Transportes",
                                                             "Ministerio de Planificación Nacional y Política Económica",
                                                             "Ministerio de Relaciones Exteriores y Culto",
                                                             "Ministerio de Salud",
                                                             "Ministerio de Seguridad Pública",
                                                             "Ministerio de Trabajo y Seguridad Social",
                                                             "Ministerio de Vivienda y Asentamientos Humanos",
                                                             "Ministerio del Ambiente y Energía",
                                                             "Partidas Específicas", 
                                                             "Poder Judicial",
                                                             "Presidencia de la República",
                                                             "Regímenes de Pensiones",
                                                             "Servicio de la Deuda Pública",
                                                             "Tribunal Supremo de Elecciones"
                      )
                      )
          ),
          h2("Análisis del gasto anual, por título, según el clasificador de objeto gasto (COG) (en millones)."),
          fluidRow(
            box( 
              solidHeader = TRUE, 
              width = 12,
              br(), 
              
              highchartOutput("clasi_COG_ANUAL_TAM",height = "500px")
              
              
            )       
          ),
          h2("Análisis del gasto mnensual, por título, según el clasificador de objeto gasto (COG) (en millones)."),
          fluidRow(
            box( 
              solidHeader = TRUE, 
              width = 12,
              br(), 
              
              highchartOutput("clasi_COG_MENSUAL_T",height = "500px")
              
              
            )       
          ),
          
          h2("Análisis de la variación del gasto mnensual, por título, según el clasificador de objeto gasto (COG) (en millones)."),
          fluidRow(
            box( 
              solidHeader = TRUE, 
              width = 12,
              br(), 
              
              highchartOutput("clasi_COG_VAR_MENSUAL_T",height = "500px")
              
              
            )       
          ),
      
          selectInput("eco1.7", "Seleccione el rubro:",  
                      list(`Tipo` = list("Gastos corrientes",
                                         "Gastos de capital", 
                                         "Transacciones financieras",
                                         "Sumas sin asignación"
                      )
                      )
          ),
          br(),
          h2("Análisis del gasto anual, por título y clasificador económico, según el clasificador de objeto gasto (COG) (en millones)."),
          fluidRow(
            box( 
              solidHeader = TRUE, 
              width = 12,
              br(), 
              
              highchartOutput("clasi_COG_total_T",height = "500px")
              
              
            )       
          ),
          h2("Análisis del gasto mensual, por título y clasificador económico, según el clasificador de objeto gasto (COG) (en millones)."),
          br(),
          fluidRow(
            box( 
              solidHeader = TRUE, 
              width = 12,
              br(), 
              
              highchartOutput("clasi_COG_total_M",height = "500px")
              
              
            )       
          ),
          
          h2("Análisis de la variación y variación acumulada, por título, en el Clasificador de Objeto Gasto y el económico (en millones)."),
          br(),
          fluidRow(
            box( 
              solidHeader = TRUE, 
              width = 12,
              br(), 
              
              highchartOutput("clasi_COG_titulo_var",height = "500px")
              
              
            )       
          )#,
          #   br(),
          #   fluidRow(
          #     box(
          #       solidHeader = TRUE, 
          #       width = 12,
          #       br(), br(),
          #       reactableOutput("tabla_COG_TOTAL_M")
          #     )
          #   )
          
  ),
  
  
   #########################
   #      Titulo ECO.2     #
   #########################
   
   tabItem(tabName = "detalle4",
           h1("Análisis del gasto, por título, según el clasificador Económico 2 en el Gobierno Central.", align = "center"),
           br(),
           selectInput("titulo.9", "Seleccione el Titulo de analisis:",  
                       list(`Títulos Gobierno Central` = list("Asamblea Legislativa",
                                                              "Contraloría General de la República", 
                                                              "Defensoría de los Habitantes de la República",
                                                              "Ministerio de Agricultura y Ganadería", 
                                                              "Ministerio de Ciencia y Tecnología",
                                                              "Ministerio de Comercio Exterior",
                                                              "Ministerio de Cultura y Juventud",
                                                              "Ministerio de Economía, Industria y Comercio",
                                                              "Ministerio de Educación Pública",
                                                              "Ministerio de Gobernación y Policía",
                                                              "Ministerio de Hacienda",
                                                              "Ministerio de Justicia y Paz",
                                                              "Ministerio de la Presidencia",
                                                              "Ministerio de Obras Públicas y Transportes",
                                                              "Ministerio de Planificación Nacional y Política Económica",
                                                              "Ministerio de Relaciones Exteriores y Culto",
                                                              "Ministerio de Salud",
                                                              "Ministerio de Seguridad Pública",
                                                              "Ministerio de Trabajo y Seguridad Social",
                                                              "Ministerio de Vivienda y Asentamientos Humanos",
                                                              "Ministerio del Ambiente y Energía",
                                                              "Partidas Específicas", 
                                                              "Poder Judicial",
                                                              "Presidencia de la República",
                                                              "Regímenes de Pensiones",
                                                              "Servicio de la Deuda Pública",
                                                              "Tribunal Supremo de Elecciones"
                       )
                       )
           ),
           
           
           h2("Análisis del gasto anual, por título, según el clasificador económico 2 (ECO2) (en millones)."),
           fluidRow(
             box( 
               solidHeader = TRUE, 
               width = 12,
               br(), 
               
               highchartOutput("clasi_ECO2_ANUAL_TAM",height = "500px")
               
               
             )       
           ),
           h2("Análisis del gasto mnensual, por título, según el clasificador económico 2 (ECO2) (en millones)."),
           fluidRow(
             box( 
               solidHeader = TRUE, 
               width = 12,
               br(), 
               
               highchartOutput("clasi_ECO2_MENSUAL_T",height = "500px")
               
               
             )       
           ),
           
           h2("Análisis de la variación del gasto mnensual, por título, según el clasificador económico 2 (ECO2) (en millones)."),
           fluidRow(
             box( 
               solidHeader = TRUE, 
               width = 12,
               br(), 
               
               highchartOutput("clasi_ECO2_VAR_MENSUAL_T",height = "500px")
               
               
             )       
           ),            

           
           selectInput("eco1.8", "Seleccione el rubro:",  
                       list(`Tipo` = list("Gastos corrientes",
                                          "Gastos de capital", 
                                          "Transacciones financieras",
                                          "Sumas sin asignación"
                       )
                       )
           ),
           h2("Análisis del gasto anual, por título, según el clasificador Económico 2 (en millones)."),
           
           fluidRow(
             box( 
               solidHeader = TRUE, 
               width = 12,
               br(), 
               
               highchartOutput("clasi_ECO_total_T",height = "500px")
               
             )       
           ),
           
           h2("Análisis del gasto mensual, por título, según el clasificador Económico 2 (en millones)."),  
           fluidRow(
             box( 
               solidHeader = TRUE, 
               width = 12,
               br(), 
               
               highchartOutput("clasi_ECO_total_M",height = "500px")
               
             )       
           ),
           br(),
           h2("Análisis de la variación y variación acumulada, por título, en el Clasificador ECO.2 (en millones)."),
           br(),
           fluidRow(
             box( 
               solidHeader = TRUE, 
               width = 12,
               br(), 
               
               highchartOutput("clasi_ECO2_titulo_var",height = "500px")
               
               
             )       
           )
   ),
   
  ##########################################################################
  #                            Indicadores del gasto                       #
  ##########################################################################
  
  tabItem(tabName = "index1",
          # title = "Monto presupuestado para el ano en curso."
          h1("Indicadores del gastos corrientes en el Gobierno Central.", align = "center"),  
          br(),
          fluidRow(
            box(
              title = paste("Monto presupuestado para el año en curso."), status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              valueBoxOutput("index7")
            )
            
          ),
          br(),
          fluidRow(
            box(
              title = paste("Presupuesto ejecutado realizado a la fecha:",Sys.Date(),", y  presupuesto ejecutado total para el año anterior."), status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              valueBoxOutput("index2.2"),
              valueBoxOutput("index3.2")
              
            )
          ),
          br(),
          fluidRow(
            box(
              title = paste("Porcentajes de ejecución realizado a la fecha: ",Sys.Date()), status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              valueBoxOutput("index4")
            )
          ),
          br(),
          fluidRow(
            box(
              title = paste("Porcentaje de variación en la ejecucion actual para la fecha: ",Sys.Date()),
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              valueBoxOutput("index5")
            )
          ),
          br(),
          
          fluidRow(
            box(
              title = paste("Se cumple el criterio de la Regla Fiscal en el Gobierno Central para la fecha: ", Sys.Date()),
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              valueBoxOutput("index6")
            )
          )#,
          #   fluidRow(
          #     #  h2(paste0("Se cumple el criterio de la Regla Fiscal en el Gobierno Central para la fecha: ",Sys.Date())),
          #     box(
          #       title = paste("Test: ", Sys.Date()),
          #       status = "primary", 
          #       solidHeader = TRUE, 
          #       width = 12,
          #       valueBoxOutput("index7")
          #     )
          #   ),
          
  ),
  
  tabItem(tabName = "index2",
          h1("Análisis del gastos en el Gobierno Central en los últimos 4 años (en millones).", align = "center"),
          br(),
          fluidRow(      
            box(
              h2("Ejecución del gasto en el Gobierno Central (nominal).", align = "center"),
              br(), br(),
              reactableOutput("tabla_ind1")
            ),
            
            box(
              h2("Ejecución del gasto en el Gobierno Central (variación).", align = "center"),
              br(), br(),
              reactableOutput("tabla_ind2")
            )
          ) ,
          fluidRow( 
            box(
              h2("Ejecución acumulada en el Gobierno Central (nominal).", align = "center"),
              br(), br(),
              reactableOutput("tabla_ind3")
            ), 
            box(
              h2("Ejecución acumulada del gasto en el Gobierno Central (variación).", align = "center"),
              br(), br(),
              reactableOutput("tabla_ind4")
              
            )
          )
  ),
  
  #################################
  #         Proyecciones          #
  #################################
  
  #######################
  #  Gobierno Central   #
  #######################
  
  tabItem(tabName = "pronos1",
          h1(paste0("Proyección del gasto corriente en el Gobierno Central para el ",Año_actual, "."), align = "center"),
          br(),
          sliderInput("horizonte.1", "Meses a pronosticar:",
                      min = 1, max = 12, value = Faltante_mes),
          br(),
          h3("Visualización mensuales del gasto corriente pronosticado en el Gobierno Central (en millones)."),
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              highchartOutput("forecast1")
            )
          ),
          br(),
          h3("Pronósticos mensuales del gasto corriente en el Gobierno Central (en millones)."),
          br(),
          tableOutput("tabla.forcast.1")
          
  ),
  
    tabItem(tabName = "pronos2",
            h1(paste0("Resumen mensual y proyección del gasto en el Gobierno Central para el año ", Año_actual,"."),align = "center"),
            br(),
            fluidRow(
              box(
                title = paste0("Resumen y proyección mensual de los gastos del Gobierno Central (millones) para el presente año."),
                solidHeader = TRUE, 
                status = "primary",
                reactableOutput("Ejecutado1_1") 
                
                
              )
            ),
            fluidRow( 
              box(
                title = paste0("Proyección del gasto corriente del Gobierno Central para el presente año (millones) ", Año_actual,"."), status = "warning",
                solidHeader = TRUE, 
                width = 8,
                valueBoxOutput("Ejecutado1_2")
              )
            )  
            
    ),
    
   ################
   #   Titulo     #
   ################
   
  tabItem(tabName = "pronos3",
          h1(paste0("Proyección por título del gasto corriente del Gobierno Central para el ",Año_actual, "."), align = "center"),
          br(),
          selectInput("titulo.3", "Seleccione el titulo:",  
                      list(`Títulos Gobierno Central` = list("Asamblea Legislativa",
                                                             "Contraloría General de la República", 
                                                             "Defensoría de los Habitantes de la República",
                                                             "Ministerio de Agricultura y Ganadería", 
                                                             "Ministerio de Ciencia y Tecnología",
                                                             "Ministerio de Comercio Exterior",
                                                             "Ministerio de Cultura y Juventud",
                                                             "Ministerio de Economía, Industria y Comercio",
                                                             "Ministerio de Educación Pública",
                                                             "Ministerio de Gobernación y Policía",
                                                             "Ministerio de Hacienda",
                                                             "Ministerio de Justicia y Paz",
                                                             "Ministerio de la Presidencia",
                                                             "Ministerio de Obras Públicas y Transportes",
                                                             "Ministerio de Planificación Nacional y Política Económica",
                                                             "Ministerio de Relaciones Exteriores y Culto",
                                                             "Ministerio de Salud",
                                                             "Ministerio de Seguridad Pública",
                                                             "Ministerio de Trabajo y Seguridad Social",
                                                             "Ministerio de Vivienda y Asentamientos Humanos",
                                                             "Ministerio del Ambiente y Energía",
                                                             "Partidas Específicas", 
                                                             "Poder Judicial",
                                                             "Presidencia de la República",
                                                             "Regímenes de Pensiones",
                                                             "Servicio de la Deuda Pública",
                                                             "Tribunal Supremo de Elecciones"
                      )
                      )
          ),
          br(),
          sliderInput("horizonte.2", "Meses a pronosticar:",
                      min = 1, max = 12, value = Faltante_mes),
          h3("Visualización mensuales, por título, del gasto corriente pronosticado (en millones)."),
          fluidRow(
            box(
              solidHeader = TRUE, 
              width = 12,
              highchartOutput("forecast2")
            )
          ),
          br(),
          h3("Pronosticos mensuales por título del gasto corriente en el Gobierno Centra (millones)."),
          br(),
          tableOutput("tabla.forcast.2")
          
  ),
  
    tabItem(tabName = "pronos4",
            h1(paste0("Resumen mensual y proyección del gasto por título en el Gobierno Central para el ano ", Año_actual,"."), align = "center"),
            br(),
            selectInput("titulo.4", "Seleccione el Título de análisis:",  
                        list(`Títulos Gobierno Central` = list("Asamblea Legislativa",
                                                               "Contraloría General de la República", 
                                                               "Defensoría de los Habitantes de la República",
                                                               "Ministerio de Agricultura y Ganadería", 
                                                               "Ministerio de Ciencia y Tecnología",
                                                               "Ministerio de Comercio Exterior",
                                                               "Ministerio de Cultura y Juventud",
                                                               "Ministerio de Economía, Industria y Comercio",
                                                               "Ministerio de Educación Pública",
                                                               "Ministerio de Gobernación y Policía",
                                                               "Ministerio de Hacienda",
                                                               "Ministerio de Justicia y Paz",
                                                               "Ministerio de la Presidencia",
                                                               "Ministerio de Obras Públicas y Transportes",
                                                               "Ministerio de Planificación Nacional y Política Económica",
                                                               "Ministerio de Relaciones Exteriores y Culto",
                                                               "Ministerio de Salud",
                                                               "Ministerio de Seguridad Pública",
                                                               "Ministerio de Trabajo y Seguridad Social",
                                                               "Ministerio de Vivienda y Asentamientos Humanos",
                                                               "Ministerio del Ambiente y Energía",
                                                               "Partidas Específicas", 
                                                               "Poder Judicial",
                                                               "Presidencia de la República",
                                                               "Regímenes de Pensiones",
                                                               "Servicio de la Deuda Pública",
                                                               "Tribunal Supremo de Elecciones"
                        )
                        )
            ),
            sliderInput("horizonte.3", "Meses a pronosticar:",
                        min = 1, max = 12, value = Faltante_mes),
            fluidRow(
              box(
                title = paste0("Resumen y proyeccioó mensual de los gastos corrientes en el ", Año_actual, ", según el título seleccionado (en millones)."), status = "primary",
                reactableOutput("Ejecutado2_1") 
                
              ),
              
              box(
                title = paste0("Proyección del gasto corriente, según el titulo seleccionado, para el ", Año_actual, "."), status = "warning",
                valueBoxOutput("Ejecutado2_2")
              )
            )   
            
            
    ),        
    
   ############################
   #  Indicadores de riesgo   #
   ############################
   
  tabItem(tabName = "alertas",
          h1(paste0("Indicadores sobre la verificación la Regla Fiscal en el Gobierno Central ", Año_actual, "."), align = "center"),
          br(),
          fluidRow(
            box(
              h4(paste0("Gasto corriente en el Gobierno Central a la fecha ", Sys.Date(), " (en millones)."), align = "center"),
              br(),
              status = "primary",
              
              reactableOutput("gasto_actual") 
              
            ),
            box(
              h4(paste0("Proyección del gasto corriente total en el Gobierno Central para el año ", Año_actual, " (en millones)."), align = "center"), 
              br(), 
              status = "primary",
              
              reactableOutput("gastopronosticado")
              
            )
            
          ),
          
          fluidRow(
            box(
              title = paste0("Se cumple el criterio de la Regla Fiscal a la fecha ", Sys.Date(),":") , status = "primary",
              valueBoxOutput("FR.Actual")
              
            ),
            box(
              title = paste0("Se cumplira el criterio de la Regla Fiscal para el año ", Año_actual, ":"), status = "primary",
              valueBoxOutput("FR.Pronostico")
            )
          ) 
  ),
  
  ############################
  #  Bitacora + evolución    #
  ############################
    
    tabItem(tabName = "creci1",
            h1(paste0("Evolución de los indicadores referentes a la Regla Fiscal en el  Gobierno Central."), align = "center"),
            br(), br(),
            h4("Bitacora con los indicadores principales indicadores del gasto corriente en el Gobierno Central (en millones)."),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                reactableOutput("bitacora_agregada")
              )
            )
            
    ),
    
    tabItem(tabName = "creci2",
            h1(paste0("Evolución del porcentaje de varianción del gasto corriente en el Gobierno Central."), align = "center"),
            br(),
            h3("Análisis de la ejecución actual y faltante en el gasto corriente del Gobierno Central (en millones)."),
            fluidRow(
              box(
                status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                sund2bOutput("gastomax")
              )
            ),
            br(),
            h3("Análisis de la evolución en la aplicación de la Regla Fiscal para los gastos corrientes en el Gobierno Central."),
            fluidRow(
              box(
                status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("bitaevo",height = "500px")
                
              )
            )
    )
  )
)

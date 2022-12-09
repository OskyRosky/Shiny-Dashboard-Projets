############################
#          body            # 
############################


body <- dashboardBody( 
  tabItems(  
    
    
    #################
    #  Presentacion #   
    #################
    
    tabItem(tabName = "inicio",
            h1("Análisis de los ingresos nacionales", align = "center"),
            br(),
            h2(paste0("Recaudación acumulada"),Ano_actual, "."),
            br(),
            fluidRow(
              box(
                title = paste0("Recaudación acumulada al ",Sys.Date()), status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                valueBoxOutput("index1")
              )
              
            ) 
     ),
    

    ##############################
    #  Indicadores del ingreso   #
    ##############################
    
    tabItem(tabName = "alertas",
            h1(paste0("Indicadores XYX ", Ano_actual, "."), align = "center"),
            br(),
            h2(paste0("Título"),Ano_actual, "."),
          
            h4("Tabla general de indicadores"),
            reactableOutput("tabla.indicadores")
            
            
    ),
            
    ###########################################
    #      Evolución de las transacciones     #
    ###########################################
    
    ####################
    #        Anual     #
    ####################
    
    tabItem(tabName = "HC_A_1",
            h1("Transacciones acumuladas a la CCSS - General", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_A_1",height = "500px")
              )
            )
            
            
    ),
    
    tabItem(tabName = "HC_A_2",
            h1("Transacciones acumuladas a la CCSS - Título", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            selectInput("clasi.E.A.titulo", "Seleccione el Título:",  
                        list(`Título` = list("Asamblea Legislativa",
                                           "Contraloría General de la República", 
                                           "Defensoría de los Habitantes de la República",
                                           "Ministerio de Agricultura y Ganadería",
                                           "Ministerio de Ciencia y Tecnología",
                                           "Ministerio de Ciencia, Tecnología y Telecomunicaciones",
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
                                           "Poder Judicial",
                                           "Presidencia de la República",
                                           "Regímenes de Pensiones",
                                           "Tribunal Supremo de Elecciones"
                        )
                        )
            ),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_A_2",height = "500px")
              )
            )
   
    ),
  
    tabItem(tabName = "HC_A_3",
            h1("Transacciones acumuladas a la CCSS - Partida", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            selectInput("clasi.E.A.partida", "Seleccione la partida:",  
                        list(`Partida` = list("Remuneraciones",
                                             "Transferencias corrientes",
                                             "Transferencias de capital"

                        )
                        )
            ),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_A_3",height = "500px")
              )
            )
            
            
            
    ),
    
    tabItem(tabName = "HC_A_4",
            h1("Transacciones acumuladas a la CCSS - Estatal o Patronal", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            selectInput("clasi.E.A.contribucion", "Seleccione la contribución:",  
                        list(`Contribución` = list("Contribución Estatal",
                                              "Contribución Patronal"
                                              
                        )
                        )
            ),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_A_4",height = "500px")
              )
            )
            
            
    ),
    
    tabItem(tabName = "HC_A_5",
            h1("Transacciones acumuladas a la CCSS - Régimen", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            
            selectInput("clasi.E.A.regimen", "Seleccione la contribución:",  
                        list(`Contribución` = list("IVM",
                                                   "RNC",
                                                   "SEM",
                                                   "FCL",
                                                   "Otros"
                                                   
                        )
                        )
            ),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_A_5",height = "500px")
              )
            )
            
            
            
    ),
    
    tabItem(tabName = "HC_A_6",
            h1("Transacciones acumuladas a la CCSS - Detalle", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            selectInput("clasi.E.A.detalle", "Seleccione el detalle:",  
                        list(`Contribución` = list("Amortizar deuda del estado con la CCSS",
                                                   "Aporte patronal al fondo de capitalización laboral",
                                                   "Asegurados por cuenta del Estado, leyes especiales",
                                                   "Aseguramiento de recolectores de café",
                                                   "Atención de personas indigentes",
                                                   "Centros penales Ley N°17",
                                                   "Contribución estatal al seguro de pensiones",
                                                   "Contribución estatal al seguro de pensiones trabajadores sector privado y público descentralizado",
                                                   "Contribución estatal al seguro de salud",
                                                   "Contribución estatal seguro de salud trabajadores del sector privado y sector público descentralizado",
                                                   "Contribución patronal al seguro de pensiones",
                                                   "Contribución patronal al seguro de salud",
                                                   "Contribución patronal según Ley de protección al trabajador",
                                                   "Convenios especiales seguro de pensiones",
                                                   "Convenios especiales seguro de salud",
                                                   "Cuota complementaria seguro de salud asegurados voluntarios",
                                                   "Cuota complementaria seguro de salud para trabajadoras domésticas",
                                                   "Destino específico impuesto antitabaco Ley 9028",
                                                   "Fecundación in vitro",
                                                   "Mitigar el efecto de la rebaja en la base mínima contributiva por COVID-19",
                                                   "Pago complementario seguro de salud trabajadores independientes",
                                                   "Pago complementario servicios prestados a asegurados voluntarios seguro de pensiones",
                                                   "Pago complementario trabajadores independientes",
                                                   "Pago de cuotas regimen de pensiones de comunicaciones",
                                                   "Pago estipendio comisión técnica valoración médica pensiones extraordinarias otros regímenes",
                                                   "Pago estipendio comisión técnica valoración médica pensiones extraordinarias Régimen del Magisterio",
                                                   "Para cubrir el déficit presupuestario de las pensiones del RNC",
                                                   "Pensiones adultos mayores en condiciones de pobreza",
                                                   "Pensiones RNC",
                                                   "Pruebas de ADN Ley N°8101",
                                                   "Seguro de pensiones para trabajadoras domésticas",
                                                   "Subsidio otorgado a responsables de pacientes terminales"
                                                   
                        )
                        )
            ),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_A_6",height = "500px")
              )
            )
            
            
    ),
    
  
    ####################
    #       Mensual    #
    ####################
    
    
    tabItem(tabName = "HC_M_1",
            h1("Transacciones mensuales a la CCSS - General", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_M_1",height = "500px")
              )
            )
            
            
    ),
    
    tabItem(tabName = "HC_M_2",
            h1("Transacciones mensuales a la CCSS - Título", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            selectInput("clasi.E.M.titulo", "Seleccione el Título:",  
                        list(`Título` = list("Asamblea Legislativa",
                                             "Contraloría General de la República", 
                                             "Defensoría de los Habitantes de la República",
                                             "Ministerio de Agricultura y Ganadería",
                                             "Ministerio de Ciencia y Tecnología",
                                             "Ministerio de Ciencia, Tecnología y Telecomunicaciones",
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
                                             "Poder Judicial",
                                             "Presidencia de la República",
                                             "Regímenes de Pensiones",
                                             "Tribunal Supremo de Elecciones"
                        )
                        )
            ),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_M_2",height = "500px")
              )
            )
            
            
    ),
    
    tabItem(tabName = "HC_M_3",
            h1("Transacciones mensuales a la CCSS - Partida", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            selectInput("clasi.E.M.partida", "Seleccione la partida:",  
                        list(`Partida` = list("Remuneraciones",
                                              "Transferencias corrientes",
                                              "Transferencias de capital"
                                              
                        )
                        )
            ),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_M_3",height = "500px")
              )
            )
            
            
            
    ),
    
    tabItem(tabName = "HC_M_4",
            h1("Transacciones mensuales a la CCSS - Estatal o Patronal", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            selectInput("clasi.E.M.contribucion", "Seleccione la contribución:",  
                        list(`Contribución` = list("Contribución Estatal",
                                                   "Contribución Patronal"
                                                   
                        )
                        )
            ),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_M_4",height = "500px")
              )
            )
            
            
            
            
    ),
    
    tabItem(tabName = "HC_M_5",
            h1("Transacciones mensuales a la CCSS - Régimen", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            selectInput("clasi.E.M.regimen", "Seleccione la contribución:",  
                        list(`Contribución` = list("IVM",
                                                   "RNC",
                                                   "SEM",
                                                   "FCL",
                                                   "Otros"
                                                   
                        )
                        )
            ),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_M_5",height = "500px")
              )
            )
            
    ),
    

    tabItem(tabName = "HC_M_6",
            h1("Transacciones mensuales a la CCSS - Detalle", align = "center"),
            br(),
            h2("ESCRIBIR", align = "center"),
            selectInput("clasi.E.M.detalle", "Seleccione el detalle:",  
                        list(`Contribución` = list("Amortizar deuda del estado con la CCSS",
                                                   "Aporte patronal al fondo de capitalización laboral",
                                                   "Asegurados por cuenta del Estado, leyes especiales",
                                                   "Aseguramiento de recolectores de café",
                                                   "Atención de personas indigentes",
                                                   "Centros penales Ley N°17",
                                                   "Contribución estatal al seguro de pensiones",
                                                   "Contribución estatal al seguro de pensiones trabajadores sector privado y público descentralizado",
                                                   "Contribución estatal al seguro de salud",
                                                   "Contribución estatal seguro de salud trabajadores del sector privado y sector público descentralizado",
                                                   "Contribución patronal al seguro de pensiones",
                                                   "Contribución patronal al seguro de salud",
                                                   "Contribución patronal según Ley de protección al trabajador",
                                                   "Convenios especiales seguro de pensiones",
                                                   "Convenios especiales seguro de salud",
                                                   "Cuota complementaria seguro de salud asegurados voluntarios",
                                                   "Cuota complementaria seguro de salud para trabajadoras domésticas",
                                                   "Destino específico impuesto antitabaco Ley 9028",
                                                   "Fecundación in vitro",
                                                   "Mitigar el efecto de la rebaja en la base mínima contributiva por COVID-19",
                                                   "Pago complementario seguro de salud trabajadores independientes",
                                                   "Pago complementario servicios prestados a asegurados voluntarios seguro de pensiones",
                                                   "Pago complementario trabajadores independientes",
                                                   "Pago de cuotas regimen de pensiones de comunicaciones",
                                                   "Pago estipendio comisión técnica valoración médica pensiones extraordinarias otros regímenes",
                                                   "Pago estipendio comisión técnica valoración médica pensiones extraordinarias Régimen del Magisterio",
                                                   "Para cubrir el déficit presupuestario de las pensiones del RNC",
                                                   "Pensiones adultos mayores en condiciones de pobreza",
                                                   "Pensiones RNC",
                                                   "Pruebas de ADN Ley N°8101",
                                                   "Seguro de pensiones para trabajadoras domésticas",
                                                   "Subsidio otorgado a responsables de pacientes terminales"
                                                   
                        )
                        )
            ),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("E_M_6",height = "500px")
              )
            )
            
            
            
    ),


   
    
   
  ############################
  #      Otros análisis      #
  ############################
  
  tabItem(tabName = "SI1",
          h1("Poner acá un título", align = "center")
          
          
          
          
  ),
  
  
    tabItem(tabName = "SI2",
            h1(paste0("Poner nombre o algo"), align = "center")
                       )
                     ),
                     
                     
 ) 



 


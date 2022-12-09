############################
#          sidebar         # 
############################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Presentacion",tabName = "inicio", icon = icon("chalkboard")),
    menuItem("Indicadores", icon = icon("map-pin"),  
             startExpanded = FALSE,
             menuSubItem("Indicadores",   tabName = "alertas", icon = icon("exclamation-triangle")
             )
    ),
    
    menuItem("Evolución", icon = icon("chart-line"),
             startExpanded = FALSE,
             menuSubItem("A - General",    tabName = "HC_A_1", icon = icon("chevron-circle-right")),
             menuSubItem("A - Título",     tabName = "HC_A_2", icon = icon("chevron-circle-right")),
             menuSubItem("A - Partida",    tabName = "HC_A_3", icon = icon("chevron-circle-right")),
             menuSubItem("A - E o P",      tabName = "HC_A_4", icon = icon("chevron-circle-right")),
             menuSubItem("A - Régimen",    tabName = "HC_A_5", icon = icon("chevron-circle-right")),
             menuSubItem("A - Detalle",    tabName = "HC_A_6", icon = icon("chevron-circle-right")),
             
             menuSubItem("M - General",    tabName = "HC_M_1", icon = icon("chevron-circle-right")),
             menuSubItem("M - Título",     tabName = "HC_M_2", icon = icon("chevron-circle-right")),
             menuSubItem("M - Partida",    tabName = "HC_M_3", icon = icon("chevron-circle-right")),
             menuSubItem("M - E o P",      tabName = "HC_M_4", icon = icon("chevron-circle-right")),
             menuSubItem("M - Régimen",    tabName = "HC_M_5", icon = icon("chevron-circle-right")),
             menuSubItem("M - Detalle",    tabName = "HC_M_6", icon = icon("chevron-circle-right"))
             

    ),


  menuItem("Otros análisis", icon = icon("chart-line"),
           startExpanded = FALSE,
           menuSubItem("General", tabName = "SI1", icon = icon("comment-dollar")),
           menuSubItem("Ingresos tributarios",  tabName = "SI2", icon = icon("accusoft")),
           menuSubItem("Impuestos",  tabName = "SI3", icon = icon("chevron-circle-right"))
           
  )
  
  ) 
)
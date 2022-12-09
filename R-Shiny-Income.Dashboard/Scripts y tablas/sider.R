############################
#          sidebar         # 
############################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Presentacion",tabName = "inicio", icon = icon("chalkboard")),
    menuItem("Definicion",tabName = "defi", icon = icon("chalkboard")),
    menuItem("Indicadores ingreso", icon = icon("map-pin"),  
             startExpanded = FALSE,
             menuSubItem("Indicadores",   tabName = "alertas", icon = icon("exclamation-triangle")
             )
    ),
    
    menuItem("Evolucion ingresos", icon = icon("chart-line"),
             startExpanded = FALSE,
             menuSubItem("Anuales",          tabName = "HC1", icon = icon("chevron-circle-right")),
             menuSubItem("Mensuales",        tabName = "HC2", icon = icon("chevron-circle-right")),
             menuSubItem("Impuestos",        tabName = "HC5", icon = icon("chevron-circle-right")),
             menuSubItem("Avanzado ",        tabName = "HC4", icon = icon("chevron-circle-right"))
    ),
 #   menuItem("Ingresos clasificador", icon = icon("diagnoses"),
 #            startExpanded = FALSE,
 #            menuSubItem("Clase",                   tabName = "IC1", icon = icon("line")),
 #            menuSubItem("Subclase",                tabName = "IC2", icon = icon("line")),
 #            menuSubItem("Grupo",                   tabName = "IC3", icon = icon("line")),
 #            menuSubItem("Subgrupo",                tabName = "IC4", icon = icon("line")),
 #            menuSubItem("Partida",                 tabName = "IC5", icon = icon("line")),
 #            menuSubItem("Subpartida",              tabName = "IC6", icon = icon("line")),
 #            menuSubItem("Renglón",                 tabName = "IC7", icon = icon("line")),
 #            menuSubItem("Subrenglón",              tabName = "IC8", icon = icon("line")),
 #            menuSubItem("Fuente de Financiación",  tabName = "IC9", icon = icon("line"))
 #            
 #   ),

    menuItem("Pronósticos", icon = icon("fas fa-chart-bar"),
             startExpanded = FALSE,
             menuSubItem("Ingresos totales", tabName = "pronos1", icon = icon("accusoft")),
          #   menuSubItem("Clasificador",          tabName = "pronos2", icon = icon("accusoft")),
             menuSubItem("Impuestos",          tabName = "pronos3", icon = icon("accusoft")),
             menuSubItem("Avanzado",          tabName = "pronos4", icon = icon("coins"))
             
             
    ),
  menuItem("Estacionalidad", icon = icon("chart-line"),
           startExpanded = FALSE,
           menuSubItem("General", tabName = "SI1", icon = icon("comment-dollar")),
           menuSubItem("Ingresos tributarios",  tabName = "SI2", icon = icon("accusoft")),
           menuSubItem("Impuestos",  tabName = "SI3", icon = icon("chevron-circle-right"))
           
  ),
 
 menuItem("Descargar",tabName = "download", icon = icon("chalkboard"))
  
  ) 
)
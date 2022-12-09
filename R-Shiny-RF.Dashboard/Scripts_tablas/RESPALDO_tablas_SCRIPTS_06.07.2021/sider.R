############################
#          sidebar         # 
############################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Presentación",tabName = "inicio", icon = icon("chalkboard")),
    menuItem("Evolución presupuesto", icon = icon("chart-line"),
             startExpanded = FALSE,
             menuSubItem("Anuales", tabName = "HC1", icon = icon("chevron-circle-right")),
             menuSubItem("Mensuales", tabName = "HC2", icon = icon("chevron-circle-right")),
             menuSubItem("Título - anual", tabName = "HC3", icon = icon("chevron-circle-right")),
             menuSubItem("Título - mensual", tabName = "HC4", icon = icon("chevron-circle-right"))
    ),
    menuItem("Análisis clasificador", icon = icon("diagnoses"),
             startExpanded = FALSE,
             menuSubItem("COG   | total",  tabName = "detalle01", icon = icon("line")),
             menuSubItem("ECO.2 | total",  tabName = "detalle02", icon = icon("line")),
             menuSubItem("COG   | total | F",  tabName = "detalle1", icon = icon("line")),
             menuSubItem("ECO.2 | total | F",  tabName = "detalle2", icon = icon("line")),
             menuSubItem("COG   | título | F", tabName = "detalle3", icon = icon("line")),
             menuSubItem("ECO.2 | título | F", tabName = "detalle4", icon = icon("line"))
    ),
    menuItem("Indicadores gastos", icon = icon("comment-dollar"),
             startExpanded = FALSE,
             menuSubItem("Gasto del GC", tabName = "index1", icon = icon("coins")),
             menuSubItem("Nominal | variación", tabName = "index2", icon = icon("coins"))
             
    ),  
    
    menuItem("Proyecciones", icon = icon("bar-chart"),
             startExpanded = FALSE,
             menuSubItem("Gobierno Central", tabName = "pronos1", icon = icon("accusoft")),
             menuSubItem("Resumen", tabName = "pronos2", icon = icon("coins")) #,
             #        menuSubItem("Titulo", tabName = "pronos3", icon = icon("accusoft")),
             #       menuSubItem("Titulo resumen", tabName = "pronos4", icon = icon("coins"))
             
    ),
    menuItem("Indicadores riesgo", icon = icon("map-pin"),  
             startExpanded = FALSE,
             menuSubItem("Alertas", tabName = "alertas", icon = icon("exclamation-triangle")
             )
    ),
    
    menuItem("Evolución", icon = icon("chart-line"),
             startExpanded = FALSE,
             menuSubItem("Bitacora", tabName = "creci1", icon = icon("comment-dollar")),
             menuSubItem("Cumplimiento", tabName = "creci2", icon = icon("accusoft"))
             
    )
    
  ) 
)
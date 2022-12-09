############################
#   Contenido del ui       # 
############################


ui <- dashboardPagePlus(skin = "blue",
                        header = dashboardHeaderPlus(
                          enable_rightsidebar = FALSE,
                          rightSidebarIcon = ""
                        ),
                        sidebar,
                        body # ,
                        #   setBackgroundColor(
                        #     color = c("#F7FBFF", "#2171B5"),
                        #     gradient = c("linear", "radial"),
                        #     direction = c("bottom", "top", "right", "left"),
                        #     shinydashboard = TRUE
                        #    )
                        
) 
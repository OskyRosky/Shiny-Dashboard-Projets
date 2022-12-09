######################################################################
#                                                                    #
#                                                                    #
#   Análisis del cumplimiento Regla Fiscal en el Gobierno Central    #
#                                                                    #   
#                                                                    #
######################################################################

options(encoding="utf-8")
options(scipen=999)

################
#  Directorio  #
################

setwd("C:/Users/oscar/Desktop/Regla fiscal/RF --- REVOLUTION/RF_WEB/Scripts_tablas")

#################
#   Librerias   # 
#################

suppressWarnings(source("Librerias.R"))

#################
#   Parametros  # 
#################

suppressWarnings(source("Parametros.R"))

############################
#   Importación de datos   # 
############################

suppressWarnings(source("importacion.R"))

###################################
#     Creacion del dashboard      # 
###################################

############################
#          header          # 
############################

suppressWarnings(source("header.R"))

############################
#          sidebar         # 
############################

suppressWarnings(source("sider.R"))

############################
#          body            # 
############################

suppressWarnings(source("body.R"))

##########################################################
#                Contenido del ui                        # 
##########################################################

suppressWarnings(source("ui.R"))

##########################################################
#                Contenido del server                    # 
##########################################################


suppressWarnings(source("server.R"))


###################################
#     Cargar la App de Shiny      # 
###################################

# shinyApp(ui, server)

#    runApp(
#      host = "127.0.0.1",
#      port = 7701
#    )



 require(shiny)

 
 x <- system("ipconfig", intern=TRUE)
 z <- x[grep("IPv4", x)]
 ip <- gsub(".*? ([[:digit:]])", "\\1", z)
 

 
# print(paste0("the Shiny Web application runs on: http://", ip, ":7701/"))
 
 runApp( launch.browser=TRUE, port = 7701, host = ip)







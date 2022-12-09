####################################
#  Apagar el Dashboard AI_SAF.R    #
####################################

##########################################################
#    Uso de la librerías y funciones del taskscheduleR   #
##########################################################

suppressWarnings(library(taskscheduleR))
suppressWarnings(library(dplyr))

###############################
##     Parar los Dashboard    #  
###############################


taskscheduler_stop("AI_SAF.R")

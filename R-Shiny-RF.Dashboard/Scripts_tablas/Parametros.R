####################################
#  Parámetros: datos + dashboard   #
####################################

#############
#  Millones #
#############

Millones <- 1000000

################################
#  Delimitación años análisis  #
################################

Años_analisis <- 2007

####################
#  Años referencia #
####################

Año_actual    <- as.numeric(substr(Sys.Date(),1,4))
Año_pasado_1  <- as.numeric(substr(Sys.Date(),1,4))-1  
Año_pasado_2  <- as.numeric(substr(Sys.Date(),1,4))-2

####################
#  Mes  referencia #
####################

mes_actual <- as.numeric(substr(Sys.Date(),6,7))


####################
#  Mes referencia  #
####################

Mes_actual <- substr(Sys.Date(),6,7)

#######################
#  Faltante de meses  #
#######################

Faltante_mes <- 12 - as.numeric(substr(Sys.Date(),6,7))

######################
#  Gastos corrientes #
######################

Gastos.corrientes <- 1

######################
#   Regla Fiscal     #
######################

RF_GC <- 4.67




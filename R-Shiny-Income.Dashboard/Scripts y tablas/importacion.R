########################################
#                                      #
#                                      #
#         Importación de tablas        #
#                                      #
#                                      #
########################################

##### Tabla de Conceptos ###### 

conceptos <- read_excel("conceptos.xlsx")

##### Tabla de Impuestos ###### 

Ingresos <- read_excel("ingresos_anual.xlsx")

Ingresos_0 <- read_excel("ingresos_anual_s0.xlsx")

Ingresos_mensual <- read_excel("ingresos_mensual.xlsx")

##### Tabla de Impuestos ######

Impuestos <- read_excel("Impuestos.xlsx", 
                        col_types = c("numeric", "numeric", "text", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric")
                        )

###### Indicadores ######

indicador.1 <- read_excel("indicador.1.xlsx")
indicador.2 <- read_excel("indicador.2.xlsx")
indicador.3 <- read_excel("indicador.3.xlsx")
indicador.4 <- read_excel("indicador.4.xlsx")
indicador.5 <- read_excel("indicador.5.xlsx")
indicador.6 <- read_excel("indicador.6.xlsx")

######    Tablas: evolucioón del presupuesto anual   ######  

tabla_1 <- read_excel("tabla_1.xlsx")

######   Tablas: evolucioón del ingreso mensual             ######  

tabla_2 <- read_excel("tabla_2.xlsx")
tabla_2_var <- read_excel("tabla_2_var.xlsx")


######   Tablas de evolución por mes   ####


tabla.evo.mensual.1_acum <- read_excel("tabla.evo.mensual.1_acum.xlsx")
tabla.evo.mensual.2_acum <- read_excel("tabla.evo.mensual.2_acum.xlsx")
tabla.evo.mensual.3_acum <- read_excel("tabla.evo.mensual.3_acum.xlsx")

######   Tablas:  Clasificador del ingreso   ####


# Clase
tabla_clasi_c_1_1 <- read_excel("tabla_clasi_c_1_1.xlsx")


# Subclase
tabla_clasi_c_2_1 <- read_excel("tabla_clasi_c_2_1.xlsx")

# Grupo
tabla_clasi_c_3_1 <- read_excel("tabla_clasi_c_3_1.xlsx")

# Subgrupo
tabla_clasi_c_4_1 <- read_excel("tabla_clasi_c_4_1.xlsx")

# Partida
tabla_clasi_c_5_1 <- read_excel("tabla_clasi_c_5_1.xlsx")

# Subpartida
tabla_clasi_c_6_1 <- read_excel("tabla_clasi_c_6_1.xlsx")

# Renglón
tabla_clasi_c_7_1 <- read_excel("tabla_clasi_c_7_1.xlsx")

# Subrenglón 
tabla_clasi_c_8_1 <- read_excel("tabla_clasi_c_8_1.xlsx")

# Fuente de Financiación
tabla_clasi_c_9_1 <- read_excel("tabla_clasi_c_9_1.xlsx")

# Fuente de Financiación

Estacionalidad <- read_excel("Estacionalidad.xlsx")
IT_Estacionalidad <- read_excel("IT_Estacionalidad.xlsx")


####################################
#####   Variable Dependiente   #####
####################################

library(haven)
library(dplyr)
datos <- read_sav("base_modelos.sav")
glimpse(datos)

# Función de cortes - 5 dias
cortes_5_inf <- function(vector){
      cortes <- c(-1,0,5,10,15,30,60,90,2000)
      etiquetas <- c("Sin vencido", "1-5 dias", "6-10 dias",
                     "11-15 dias", "16-30 dias", "31-60 dias",
                     "61-90 dias", "Mas de 90 días")
      resul <- cut(vector, breaks=cortes, labels = etiquetas)
      return(as.integer(resul))
}

cortes_5_sup <- function(vector){
      cortes <- c(-1,0,5,10,15,30,60,90,120, 2000)
      etiquetas <- c("Sin vencido", "1-5 dias", "6-10 dias",
                     "11-15 dias", "16-30 dias", "31-60 dias",
                     "61-90 dias", "De 91 a 120 días", "Mas de 120 días")
      resul <- cut(vector, breaks=cortes, labels = etiquetas)
      return(as.integer(resul))
}

# Función de cortes - 10 dias
cortes_10 <- function(vector){
      cortes <- c(-1,0,10,20,30,60,90,120,2000)
      etiquetas <- c("Sin vencido", "1-10 dias", "11-20 dias",
                     "21-30 dias", "31-60 dias", "61-90 dias",
                     "91-120 dias", "Mas de 120 días")
      resul <- cut(vector, breaks=cortes, labels = etiquetas)
      return(as.integer(resul))
}

# Función de cortes - 15 dias
cortes_15_inf <- function(vector){
      cortes <- c(-1,0,15,30,45,60,90,120,150,2000)
      etiquetas <- c("Sin vencido", "1-15 dias", "16-30 dias",
                     "31-45 dias", "46-60 dias", "61-90 dias", "91-120 dias",
                     "121-150 dias", "Mas de 150 días")
      resul <- cut(vector, breaks=cortes, labels = etiquetas)
      return(as.integer(resul))
}

cortes_15_sup <- function(vector){
      cortes <- c(-1,0,15,30,45,60,90,120,150,180,2000)
      etiquetas <- c("Sin vencido", "1-15 dias", "16-30 dias",
                     "31-45 dias", "46-60 dias", "61-90 dias", "91-120 dias",
                     "121-150 dias", "151-180 dias", "Mas de 180 días")
      resul <- cut(vector, breaks=cortes, labels = etiquetas)
      return(as.integer(resul))
}



table(cortes_5_inf(data[["DIAS_VEN_N11"]]))
barplot(table(cortes_5_inf(data[["DIAS_VEN_N11"]])))

datos[["SALDO_N1"]] < 42
datos[["DIAS_VEN_N1"]]

datos[["DIAS_VEN_N1"]][datos[["SALDO_N1"]]<42] <- 0

saldos <- paste("SALDO_N",c(1:12), sep="")
dias <- paste("DIAS_VEN_N",c(1:12), sep="")

# Enceramos los días en base a los saldos
for(i in 1:12){
      datos[[dias[i]]][datos[[saldos[i]]]<42] <- 0
}


table(cortes_5_inf(datos[["DIAS_VEN_N1"]]), cortes_5_sup(datos[["DIAS_VEN_N2"]]))

# Matriz de transición
for(i in 1:11){
      cat("Tabla ", dias[i], " vs ", dias[i+1])
      print(table(cortes_5_inf(datos[[dias[i]]]), cortes_5_sup(datos[[dias[i+1]]])))
}


# Roll Rate
avanza <- 1*(cortes_5_inf(datos[["DIAS_VEN_N1"]]) < cortes_5_sup(datos[["DIAS_VEN_N2"]]))
table(cortes_5_inf(datos[["DIAS_VEN_N1"]]), avanza)


avanza <- 1*(cortes_5_inf(datos[["DIAS_VEN_N11"]]) < cortes_5_sup(datos[["DIAS_VEN_N12"]]))
table(cortes_5_inf(datos[["DIAS_VEN_N11"]]), avanza)

# 0% - 40% ZONA VERDE
# 40% - 60% ZONA GRIS
# 60% - 100% ZONA ROJA


# Variable dependiente

datos$MAX_N1_N12 <- apply(datos[,grep("DIAS_VEN", colnames(datos))], 1, max)

datos$GB_15 <- ifelse(datos$MAX_N1_N12 <=15, 0, ifelse(datos$MAX_N1_N12>30, 1, 2))
table(datos$GB_15)

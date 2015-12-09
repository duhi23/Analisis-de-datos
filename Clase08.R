####################################
#####   Variable Dependiente   #####
####################################

library(haven)
library(dplyr)
datos <- read_sav("base_modelos.sav")
glimpse(datos)

# Función de cortes - 5 dias
cortes_5 <- function(vector){
      cortes <- c(-1,0,5,10,15,30,60,90,2000)
      etiquetas <- c("Sin vencido", "1-5 dias", "6-10 dias",
                     "11-15 dias", "16-30 dias", "31-60 dias",
                     "61-90 dias", "Mas de 90 días")
      resul <- cut(vector, breaks=cortes, labels = etiquetas)
      return(resul)
}

help(cut)

table(cortes_5(datos[["DIAS_VEN_N1"]]))
barplot(table(cortes_5(datos[["DIAS_VEN_N1"]])))

datos[["SALDO_N1"]] < 42
datos[["DIAS_VEN_N1"]]

datos[["DIAS_VEN_N1"]][datos[["SALDO_N1"]]<42] <- 0

saldos <- paste("SALDO_N",c(1:12), sep="")
dias <- paste("DIAS_VEN_N",c(1:12), sep="")

# Enceramos los días en base a los saldos
for(i in 1:12){
      datos[[dias[i]]][datos[[saldos[i]]]<42] <- 0
}


table(cortes_5(datos[["DIAS_VEN_N1"]]), cortes_5(datos[["DIAS_VEN_N2"]]))

# Matriz de transición
for(i in 1:11){
      cat("Tabla ", dias[i], " vs ", dias[i+1])
      print(table(cortes_5(datos[[dias[i]]]), cortes_5(datos[[dias[i+1]]])))
}
########################################
#######     Examen Bimestral     #######
########################################


library(dplyr)
library(haven)

data <- read_sav("base_examen.sav")
glimpse(data)

saldos <- paste("SALDO_N",c(1:12), sep="")
dias <- paste("DIAS_VEN_N",c(1:12), sep="")

# Enceramos los días en base a los saldos
for(i in 1:12){
      data[[dias[i]]][data[[saldos[i]]]<42] <- 0
}

# Resultados
for(i in 1:11){
      cat("Tabla ", dias[i], " vs ", dias[i+1])
      print(table(cortes_5_inf(data[[dias[i]]]), cortes_5_sup(data[[dias[i+1]]])))
}

data %>% select(-grep("rdt", colnames(data)))




########################################
#######     Examen Bimestral     #######
########################################


library(dplyr)
library(haven)

data <- read_sav("base_examen.sav")
glimpse(data)


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


saldos <- paste("SALDO_N",c(1:12), sep="")
dias <- paste("DIAS_VEN_N",c(1:12), sep="")

# Enceramos los días en base a los saldos
for(i in 1:12){
      data[[dias[i]]][data[[saldos[i]]]<42] <- 0
}

# Resultados
for(i in 1:11){
      cat("Tabla ", dias[i], " vs ", dias[i+1])
      print(table(cortes_15_inf(data[[dias[i]]]), cortes_15_sup(data[[dias[i+1]]])))
}


# Roll Rate - Mes N11 vs Mes N12
avanza <- 1*(cortes_15_inf(data[["DIAS_VEN_N11"]]) < cortes_15_sup(data[["DIAS_VEN_N12"]]))
table(cortes_15_inf(data[["DIAS_VEN_N11"]]), avanza)


# Damos formato a la tabla roll rate
RR(table(cortes_15_inf(data[["DIAS_VEN_N11"]]), avanza))

# BUENO:                            # Completar
# MALO:                             # Completar
# INDETERMINADO:                    # Completar

# Variable dependiente

# Maximo vencido de 12 meses
data$MAX_N1_N12 <- apply(data[,grep("DIAS_VEN", colnames(data))], 1, max)

# Variable GB

data$GB  <- ifelse(___________) # Completar








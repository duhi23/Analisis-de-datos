####################################
#####   Variable Dependiente   #####
####################################

# Tabla Roll Rate
RR <- function(tabla){
      suma <- tabla[,1]+tabla[,2]
      row_n <- round(100*ifelse(tabla[,1]==0,0,tabla[,1]/suma),digits=1)
      column_n <- round(100*ifelse(tabla[,1]==0,0,tabla[,1]/sum(tabla[,1])),digits=1)
      row_s <- round(100*ifelse(tabla[,1]==0,0,tabla[,2]/suma),digits=1)
      column_s <- round(100*ifelse(tabla[,2]==0,0,tabla[,2]/sum(tabla[,2])),digits=1)
      total <- round(100*suma/sum(suma),digits=1)
      mat<-cbind(tabla[,1],row_n,column_n,tabla[,2],row_s,column_s,suma, total)
      colnames(mat)<-c("No Avanza","%Fila","%Columna","Avanza","%Fila","%Columna", "N", "%Total")
      TOTAL <- c(sum(tabla[,1]),round(sum(tabla[,1])/sum(suma),digits=1),100,
                 sum(tabla[,2]),round(sum(tabla[,2])/sum(suma),digits=1),100,sum(suma), 100)
      matriz<-rbind(mat,TOTAL)
      return(matriz)
}

RR(table(cortes_5_inf(datos[["DIAS_VEN_N1"]]), avanza))


# Visualización KS

var <- datos[["SCORE"]]
GB <- as.integer(datos[["GB_15"]])

bivar <- data.frame(GB,var)
# Score mayor a 0
bivar <- subset(bivar, bivar$var>0)
# Prueba sobre B/M
bivar <- subset(bivar, bivar$GB<=1)
dim(bivar)

# Funcion acumulada / Buenos
Fn_B <- ecdf(bivar[bivar[,1]==0,][,2])
# Funcion acumulada / Malos
Fn_M <- ecdf(bivar[bivar[,1]==1,][,2])

# Curva de buenos
plot(Fn_B,do.points = FALSE,verticals=T,col='green',main='KS Test')
# Curva de malos
lines(Fn_M,lty=3,do.points = FALSE, verticals=T,col='red')
# Valor KS
ks.test(bivar[bivar[,1]==0,][,2],bivar[bivar[,1]==1,][,2], alternative = c("two.sided"))

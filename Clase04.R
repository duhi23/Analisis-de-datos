#################################################
##### Clase 04 - Validación de la identidad #####
#################################################

verificador <- function(cedula){
      if(nchar(cedula)==10){
            index <- c(2,1,2,1,2,1,2,1,2)
            val <- numeric(10)
            for(i in 1:10){
                  val[i] <- as.numeric(substring(cedula,i,i))
            }
            produ <- index*val[1:9]
            produ[which(produ >=10)] <- produ[produ>=10]-9
            if((sum(produ) + val[10])%%10 == 0){
                  msg <- paste("La cédula ", cedula, "SI es correcta")
            } else {
                  msg <- paste("La cédula ", cedula, " NO es correcta")
            }
            return(msg)
      } else {
            print("La identificación no es correcta")
      }
      
}

verificador("060401439")

load("DataCruce.RData")
head(data)

library(dplyr)
data <- tbl_df(data)
data <- data %>% mutate(id=ifelse(nchar(identificacion)==9, 
                          paste("0", identificacion, sep=""), 
                          as.character(identificacion)))


registro <- function(cedula){
      reg <- data %>% filter(id==cedula) 
      return(reg)      
}
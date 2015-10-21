##########################################
######   Encapsulamiento de objetos ######
##########################################

# Direccion en memoria - ambientes basicos
globalenv()
baseenv()
emptyenv()
environment()

# Creamos un nuevo ambiente
RC <- new.env(parent = emptyenv())
# Cargamos la información dentro del ambiente RC
load("DataCruce.RData", envir=RC)
environment(RC)
ls(RC)
parent.env(RC)

# Creamos la variable id dentro de data
library(dplyr)
RC$data <- tbl_df(RC$data)
RC$data <- RC$data %>% mutate(id=ifelse(nchar(identificacion)==9, 
                                        paste("0", identificacion, sep=""), 
                                        as.character(identificacion)))

# Función verificadora de identidad
RC$verificador <- function(cedula){
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

# Funcion que calcula la edad
RC$edad <- function(cedula){
      fna <- as.Date(as.character(RC$data %>% filter(id==cedula) 
                                  %>% select(fecha_nacimiento)))
      fac <- as.Date(Sys.time())
      cal <- trunc(as.numeric(difftime(fac, fna, units="days"))/365)
      if(cal < 18){
            edad <- 18
      } else if(cal> 70){
            edad <- 70
      } else {
            edad <- cal
      }
      return(edad)      
}

RC$edad("0604014399")
RC$verificador("0604014399")

RC$verificador <- verificador
RC$edad <- edad
rm(list=c("edad", "verificador"))

class(RC$data)
class(RC$edad)
class(RC$verificador)
str(RC)

RC$.codigo <- "MAT891"
ls(RC)
ls(RC, all.names = TRUE)
RC$.codigo
str(RC)

get("data", envir=RC)










###### Script Clase 03 ######

install.packages('haven', dependencies = TRUE)
install.packages('devtools', dependencies = TRUE)
devtools::install_github("hadley/haven")
library(haven)
library(dplyr)
library(sqldf)
list.files()

# Cargamos la base
data <- read_sav("SPSS_Chimborazo_Poblacion.sav")
dim(data)
glimpse(data)

# Coercion forzada para realizar los consultas
data$P01 <- as.numeric(data$P01)
data$I02 <- as.numeric(data$I02)

# Consultas utilizando sentencias SQL
sqldf("SELECT I02, count(P01)
      FROM data
      WHERE P01==1
      GROUP BY I02")

sqldf("SELECT I02, count(P01)
      FROM data
      WHERE P01==1
      GROUP BY I02")

table(data$I02, data$P01)

sqldf("SELECT P05
      FROM data")

# Variables del tipo labelled
clases <- unlist(lapply(data,class))=="labelled"
clases[clases==TRUE]
clases[clases==FALSE]


table(data$I02)

# FUncion que se aplica sobre columnas numericas
est <- function(vector){
      vari <- list()
      vari$min <- min(vector)
      vari$p5 <- quantile(vector, probs=c(0.05))
      vari$mediana <- median(vector)
      vari$p95 <- quantile(vector, probs=c(0.95))
      vari$max <- max(vector)
      return(unlist(vari))
}

columnas <- which(unlist(lapply(data, class))=="numeric")
class(columnas)

subdata <- data[,columnas]

lapply(subdata, est)


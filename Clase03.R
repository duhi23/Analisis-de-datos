###### Script Clase 03 ######

install.packages('haven', dependencies = TRUE)
install.packages('devtools', dependencies = TRUE)
devtools::install_github("hadley/haven")
library(haven)
library(dplyr)
library(sqldf)
list.files()

data <- read_sav("SPSS_Chimborazo_Poblacion.sav")
dim(data)
glimpse(data)

sqldf("SELECT count(P01)
      FROM data
      GROUP BY P01")




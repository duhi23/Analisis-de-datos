#################################
#####     Script Examen     #####
#################################

dir.create("./examen")
setwd("./examen")
list.files()

library(readxl)
library(sqldf)
library(ggvis)
library(dplyr)
data <- read_excel("examen_analisis.xls", sheet = 1)


PROM <- sqldf("SELECT CARRERA, AVG(PROMEDIO) AS CPROM
      FROM data
      WHERE PROMEDIO >0
      GROUP BY CARRERA")

PROM <- PROM %>% mutate(ABRCARRERA=abbreviate(CARRERA, 6L))

PROM %>% ggvis(~ABRCARRERA, ~CPROM) %>% layer_lines()


for (i in seq(1:55)) {
      knitr::knit('five_test.Rnw', output = paste('prueba_', i, '.tex', sep = ''))
      system(paste('pdflatex ', 'prueba_', i, ".tex", sep=''))
}
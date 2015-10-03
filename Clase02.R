#### Consultas SQL - Visulaización ####

# Datos
library(readxl)
poblacion <- read_excel("poblacion.xlsx", sheet = 1)
salud <- read_excel("base_salud.xls", sheet = 1)
dim(poblacion)
dim(salud)

# Consultas con sentencias SQL
library(sqldf)

sqldf("SELECT PROVINCIA, CANTON
      FROM poblacion")

sqldf("SELECT PROVINCIA, CANTON
      FROM poblacion
      WHERE PROVINCIA LIKE 'A%' ")

info_cantones <- sqldf("SELECT PROVINCIA, COUNT(CANTON) AS NCANTONES, SUM(POBLACION) AS HABITANTES
      FROM poblacion
      GROUP BY PROVINCIA
      ORDER BY HABITANTES DESC")

class(info_cantones)

info_salud <- sqldf("SELECT PROVINCIA, COUNT(CENTRO_SALUD) AS CENTRO
                    FROM salud
                    GROUP BY PROVINCIA")

## INNER JOINS

# Inner Join con SQL
sqldf("SELECT *
      FROM info_cantones AS C
      INNER JOIN info_salud AS S ON C.PROVINCIA=S.PROVINCIA")

# Inner Join con dplyr
library(dplyr)
datos <- inner_join(info_cantones,info_salud,by="PROVINCIA")

## Graficos

library(ggvis)

datos %>% ggvis(~PROVINCIA, ~NCANTONES) %>% layer_points()
datos %>% ggvis(~PROVINCIA, ~NCANTONES) %>% layer_bars()

datos %>% ggvis(~PROVINCIA, ~NCANTONES, size=~CENTRO) %>% layer_points()
datos %>% ggvis(~PROVINCIA, ~NCANTONES, size=~HABITANTES) %>% layer_points()
datos %>% ggvis(~PROVINCIA, ~NCANTONES, size:=50) %>% layer_points()

datos %>% ggvis(~PROVINCIA, ~HABITANTES, fill=~NCANTONES) %>% layer_points()
datos %>% ggvis(~PROVINCIA, ~HABITANTES, fill:="green") %>% layer_points()

datos %>% 
      ggvis(~PROVINCIA, ~HABITANTES, fill:="red",size=~CENTRO) %>% 
      layer_points()

datos %>% 
      ggvis(~PROVINCIA, ~HABITANTES, fill:="red",size:=40) %>% 
      layer_points()

# Extraemos los primeros 3 caracteres de las Provincias
datos <- datos %>% mutate(CODIGO=substring(PROVINCIA,1,3))

datos %>% 
      ggvis(~CODIGO, ~HABITANTES, fill:="red",size:=40) %>% 
      layer_points()

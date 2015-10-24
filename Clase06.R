#################################
##### Funciones de consulta #####
#################################

ls(RC)
library(readxl)
library(dplyr)
RC$vivienda <- read_excel("base_vivienda2.xlsx", sheet=1)
RC$educacion <- read_excel("base_educacion.xlsx", sheet=1)
colnames(RC$educacion)[13:15] <- c("P_BAC_TBA_EDU1", "P_BAC_TBA_EDU2", "P_BAC_TBA_EDU3")

RC$info <- function(cedula){
      reg <- RC$data %>% filter(id==cedula) %>% select(provincia, canton, parroquia)
      estviv <- RC$vivienda %>% filter(PROVINCIA==as.character(reg[1,1]), 
                             CANTON==as.character(reg[1,2]), 
                             PARROQUIA==as.character(reg[1,3]))
      estedu <- RC$educacion %>% filter(PROVINCIA==as.character(reg[1,1]), 
                                       CANTON==as.character(reg[1,2]), 
                                       PARROQUIA==as.character(reg[1,3]))
      return(data.frame(estviv, estedu))
}

RC$info("0604014399")

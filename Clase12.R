##########################################
#######        Segmentación        #######
##########################################

library(haven)
library(dplyr)
datos <- read_sav("segmentacion.sav")
glimpse(datos)
dim(datos)

summary(datos$IngresoEstimadoV2)
summary(datos$SCORE_V2)
datos <- datos %>% mutate(LN_INGRESOS=15*log(IngresoEstimadoV2),
                          SQRT_SCORE=sqrt(SCORE_V2))
subdata <- datos %>% select(EDAD, LN_INGRESOS, SQRT_SCORE)

# K medias
seg <- kmeans(subdata,2,iter.max=20)
str(seg)
subdata$CLUSTER <- seg$cluster
head(subdata)

round((seg$size/sum(seg$size))*100,digits=2)

plot(subdata[c("EDAD","LN_INGRESOS")], col=subdata$CLUSTER)
var(subdata$EDAD)
var(subdata$LN_INGRESOS)

plot(subdata[c("EDAD","SCORE_V2")], col=subdata$CLUSTER)
var(subdata$EDAD)
var(subdata$SQRT_SCORE)

persp(subdata$NIVEL_RIESGO, subdata$INGRESOESTIMADO, subdata$CLUSTER)



# Nubes dinámicas
kmeans(datos,9,iter.max=60,algorithm="MacQueen")

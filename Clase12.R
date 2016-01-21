##########################################
#######        Segmentación        #######
##########################################

library(haven)
library(dplyr)
datos <- read_sav("trabajo_modelos.sav")
glimpse(datos)

subdata <- datos %>% select(DIGITO, NIVEL_RIESGO, INGRESOESTIMADO, TARJETAS_CREDITO)


# K medias
CLUSTER <- kmeans(subdata,3,iter.max=20)
str(CLUSTER)
subdata$CLUSTER <- CLUSTER$cluster

round((CLUSTER$size/sum(CLUSTER$size))*100,digits=2)

plot(subdata[c("NIVEL_RIESGO","INGRESOESTIMADO")], col=subdata$CLUSTER)

persp(subdata$NIVEL_RIESGO, subdata$INGRESOESTIMADO, subdata$CLUSTER)



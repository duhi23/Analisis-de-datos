######################################
#####     Tabla Performance      #####
######################################

# KS Modelo

data_eval <- cbind(data_train, prob)

var <- data_eval[["prob"]]
GB <- as.integer(data_eval[["GB_60"]])

bivar <- data.frame(GB,var)

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

# Construccion deciles
index <- seq(1:nrow(data_eval))
pto_mean <- as.vector(round(quantile(index, probs=seq(0.1,0.9,by=0.1)),0))
cortes <- c(0,pto_mean,18000)

decil <- as.integer(cut(index, breaks=cortes))

library(dplyr)
data_eval <- tbl_df(data_eval)
data_eval <- data_eval %>% arrange(desc(prob))
data_eval <- data_eval %>% mutate(DECIL = decil)

datafin <- data_eval %>% select(GB_60, prob, DECIL)

by(datafin$GB_60, datafin$DECIL, table)









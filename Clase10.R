################################
#####     KS Function      #####
################################

datos$GB_60 <- as.integer(datos$GB_60)
GB <- as.integer(datos[["GB_60"]])

which(unlist(lapply(datos,class))=="labelled")

test_ks <- function (x){
      no_missg <- length(x[!is.na(x)])/length(x)
      if(is.numeric(x) && no_missg > 0.80){
            vars <- data.frame(GB,x)
            var_m <- vars[vars[,1]==1,2]
            var_b <- vars[vars[,1]==0,2]
            ks <- ks.test(var_b,var_m, alternative = c("two.sided"))
            ks <- as.numeric(ks$statistic)        
      }else{
            ks <- as.numeric(-999)
      }
      return(ks)
}


## vars: base con variables a calcular KS 
vars <- datos

KS <- c(0)
for (i in seq(1:ncol(vars)))
{
      KS[i] <- test_ks(vars[,i])
}


## Orden mejores variables segun KS
nom <- as.character(colnames(datos))
KS <- data.frame(nom,KS)
KS <- KS[order(KS[,2],decreasing = TRUE),]

## Base final
View(KS)
write.table(KS, file="TEST KS.csv", dec=".")


# Regresión Logistica

table(datos$GB_60)
data_train <- subset(datos, datos$GB_60 <=1)

modelo <- glm(GB_60 ~ d24m_2a6_SICOM + d24_1a6_sfr, 
              family=binomial(link='logit'), data=data_train)

summary(modelo)

# Predicciones

newdata <- data_train %>% select(d24m_2a6_SICOM, d24_1a6_sfr)
prob <- predict(modelo, newdata, type="response")

# Punto de corte 0.5
BM <-ifelse(prob<=0.5,0,1)
MC <- table(data_train$GB_60, BM)

# Error de prediccion 0.5
(MC[1,2]+MC[2,1])/sum(MC)

# Punto de corte 0.4
BM1 <-ifelse(prob<=0.4,0,1)
MC1 <- table(data_train$GB_60, BM1)

# Error de prediccion
(MC1[1,2]+MC1[2,1])/sum(MC1)



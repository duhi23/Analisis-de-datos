################################
#####     KS Function      #####
################################


test_ks <- function (x){
      no_missg <- length(x[!is.na(x)])/length(x)
      if(is.numeric(x) && no_missg > 0.50){
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
KS <- data.frame(nom,KS)
KS <- KS[order(KS[,2],decreasing = TRUE),]

## Base final
View(KS)
write.table(KS, file="TEST KS.csv", dec=".")
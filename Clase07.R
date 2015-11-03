#################################
##### Estimación Bootstrap  #####
#################################

# Calculamos la edad en RC
library(dplyr)
RC$edad <- function(cedula){
      library(dplyr)
      fna <- as.Date(as.character(RC$data %>% filter(id==cedula) 
                                  %>% select(fecha_nacimiento)))
      fac <- as.Date(Sys.time())
      cal <- trunc(as.numeric(difftime(fac, fna, units="days"))/365)
      if(cal < 0){
            edad <- 0
      } else if(cal> 100){
            edad <- 100
      } else {
            edad <- cal
      }
      return(edad)      
}

RC$vecedad <- as.numeric(sapply(RC$data$id[6000:6100], RC$edad))

bootmedia <- function(vector, iter, rpl=TRUE){
      est <- numeric(iter)
      for(i in 1:iter){
            est[i] <- mean(sample(vector, replace=rpl, size=length(vector)))
      }
      return(mean(est))
}


# IC Coeficiente de Correlación

x <- sort(sample(1:500, size=300))
y <- 2*sort(sample(1:1500, size=300))-rnorm(300, sd=300)
plot(x,y)
cor(x,y)


x1 <- sort(sample(x, size=length(x), replace=TRUE))
y1 <- sort(sample(y, size=length(y), replace=TRUE))
plot(x1,y1)
cor(x1,y1)

#################################
##### Estimación Bootstrap  #####
#################################

library(haven)
datos <- read_sav("bootstrap.sav")
dim(datos)

mean(datos$EDAD)

B <- 5000
n <- nrow(datos)

boot <- matrix(sample(datos$EDAD, 
                      size=B*n, replace=TRUE),B,n)

est <- apply(boot, 1, mean)
hist(est)

library(ggplot2)
ggplot(data.frame(media=est), aes(x=media)) +
      geom_histogram(binwidth=0.03, aes(y=..density..)) +
      geom_density(color="red")

quantile(est, probs=c(0.025,0.975))







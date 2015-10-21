###################################################
##### Clase 04 - Imputación de datos perdidos #####
###################################################

install.packages('mice', dependencies = TRUE)

# Problema de los datos perdidos

y <- c(1, 2, 4)
mean(y)

y <- c(1, 2, NA)
mean(y)

mean(y, na.rm = TRUE)

lm(Ozone ~ Wind, data = airquality)

fit <- lm(Ozone ~ Wind, data = airquality, na.action = na.omit)
coef(fit)

# Por default retira los valores NA
options(na.action = na.omit)

delete <- na.action(fit)
naprint(delete)

# Imputacion por media




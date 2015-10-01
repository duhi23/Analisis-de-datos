## Script - Clase 01

install.packages('dplyr', dependencies = TRUE)
install.packages(c('data.table','ggplot2'), dependencies = TRUE)

# Creación de objetos
## Vectores
x <- c(1,2,3,4,5)
y <- c(6.1,-7.05,8,9,10)
length(x)
typeof(x)

e <- c(5L,6L,7L)
typeof(e)

m <- c("Ecuador", "Colombia", "Venezuela")
typeof(m)

q <- c(TRUE, FALSE, TRUE, FALSE)
typeof(q)

xy <- c(x,y)
xy

# Coercion
xe <- c(x,e)
typeof(xe)

qm <- c(q,m)
typeof(qm)

mtcars
class(mtcars)

library(dplyr)
library(data.table)
mtcars <- tbl_df(mtcars)
mtcars <- data.table(mtcars)
class(mtcars)

# Operador de asignacion

x %>% mean()

xx <- c(3,5,7,NA,9)
xx
xx %>% sum(na.rm=TRUE)
sum(xx, na.rm=TRUE)

ls("package:dplyr")
str(mtcars)

mtcars %>%  select(mpg, cyl) # select(mtcars,mpg,cyl)
mtcars %>%  select(1,2)

mtcars %>% mutate(var_df=mpg+cyl, var2=mpg-cyl) %>% 
      select(mpg, cyl,var_df, var2)

str(mtcars2)

mtcars %>% filter(cyl==6) %>% select(-gear, -carb)
mtcars %>% filter(cyl==6) %>% select(ends_with("t"))
mtcars %>% filter(cyl==6) %>% select(contains("a"))
mtcars
mtcars %>% arrange(desc(disp))

mtcars %>% group_by(am) %>% 
      summarise(media=mean(cyl), desviacion=sd(cyl))


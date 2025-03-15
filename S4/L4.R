rm(list = ls())
setwd("C:/Users/et396/Dropbox/Docencia/Educate/Econometria/S4/Data")
# source: https://www.princeton.edu/~otorres/Panel101R.pdf

paquetes_set <- c("rio", "dplyr", "tidyverse", "oglmx","stargazer",
                  "texreg", "sjPlot", "ggplot2", "MASS","erer","plm")
#install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

base <- import ("BD4.dta")       # base datos
base %>%  dim()
base %>%  names()
base %>%  summary()
base %>%  str()

# Pregunta 1
#=========================================================================
base[,c('lb','b','cburg',"sburg", "ur")] %>%  summary()

# Pregunta 2
#=========================================================================
## Pregunta 2a
# Estimando un pooled data
m1<- lm(lb ~ lag(cburg,1) + lag(sburg,1) + ur + factor(pfa)+factor(year), data = base )
summary(m1)

## Pregunta 2b
#m2 <-plm(lb ~ lag(cburg,1) + lag(sburg,1) + ur, data = data,
#            index = c("pfa", "year"),effect="individual" ,model = "within")
m2 <-plm(lb ~ lag(cburg,1) + lag(sburg,1) + ur + factor(year), data = base,
         index = c("pfa", "year") ,model = "within")
summary(m2)

stargazer(m1, m2, title = "Results", type = "text")

#Test de OLS vs FE
#If the p-value is < 0.05 then the fixed effects model is a better choice
pFtest(m2, m1) 

#Pregunta 3
#=========================================================================
m3 <- plm(lb ~ lag(cburg,1) + lag(sburg,1) + ur, data=base, 
              index=c("pfa", "year"), model="random")

stargazer(m1, m2, m3, title = "Results", type = "text")

# Test de hausman
# if this number is < 0.05 then use fixed effects
phtest(m2, m3)

#install.packages("feisr")
library(feisr)
mudlak_mod <- feis(lb ~ lag(cburg,1) + lag(sburg,1) + ur | year , 
                    id='pfa', data=base)
summary(mudlak_mod)
mudlak_test <- feistest(mudlak_mod, robust = FALSE, type = "all")
summary(mudlak_test)


# Pregunta 4
#=========================================================================
# modelo final, efectos fijos
m4 <-plm(lb ~ lag(lb,1) + lag(cburg,1) + lag(sburg,1) + ur + factor(year), data = base,
         index = c("pfa", "year") ,model = "within")
stargazer(m2, m4, title = "Results", type = "text", order=c("lb", "cburg"))

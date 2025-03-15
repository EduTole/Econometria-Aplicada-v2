rm(list = ls())
setwd("C:/Users/et396/Dropbox/Docencia/Educate/Econometria/S5/Data")
# source: https://www.princeton.edu/~otorres/Panel101R.pdf

paquetes_set <- c("rio", "dplyr", "tidyverse", "oglmx","stargazer",
                  "texreg", "sjPlot", "ggplot2", "MASS","erer","AER")
#install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)


base <- import ("BD5.dta")       # base datos
base %>%  dim()
base %>%  names()
base %>%  summary()
base %>%  str()

# Pregunta 1
# ========================================================
m1 <- lm(logwage ~ educ + female , data= base)
summary(m1)

# Pregunta 3
# ========================================================
m2 <- lm(educ ~ female + test11r + test11m + mumso +dadso  , data= base)
summary(m2)
base$rerror <- m2$residuals

m3 <- ivreg(logwage ~ educ + female | female +  test11r + test11m + mumso +dadso , data = base)
summary(m3)


# Pregunta 4
# ========================================================
base$rerror_a <- m3$residuals
m4 <- lm(rerror_a ~ female + test11r + test11m + mumso +dadso  , data= base)
summary(m4)

# generar errores sin la variable dadso
m3b <- ivreg(logwage ~ educ + female | female +  test11r + test11m + mumso , data = base)
base$rerror_b <- m3b$residuals

m5 <- lm(rerror_b ~ female + test11r + test11m + mumso  , data= base)
summary(m5)


# Pregunta 5
# ========================================================
m6 <- lm(logwage ~ educ + female + rerror, data= base)
summary(m6)

# modelo de vi
summary(m3b)

#install.packages("ivreg", dependencies = TRUE)
#install.packages("remotes")
#library(remotes)
#remotes::install_github("https://github.com/zeileis/ivreg/")
m7 <- ivreg(logwage ~ educ + female | female + test11r + test11m + mumso , data = base)
summary(m7, vcov = sandwich, diagnostics = TRUE)


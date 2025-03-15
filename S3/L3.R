rm(list = ls())
setwd("C:/Users/et396/Dropbox/Docencia/Educate/Econometria/S3/Data")

paquetes_set <- c("rio", "dplyr", "tidyverse", "oglmx","stargazer",
                  "texreg", "sjPlot", "ggplot2", "MASS","erer")
#install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)


data <- import ("BD2_Multiproducto_2021.dta")       # base datos
data %>%  dim()
data %>%  names()
data %>%  summary()
data %>%  str()
Hmisc::describe(data$rvida)

# Limpiamos informacion de NA
# data <- data %>% 
#   drop_na()


# Question 1
#-------------------------------------------------------
m1_0 <- oprobit.reg(rvida~1,data=data,savemodelframe = TRUE)
summary(m1_0)
m1_1 <- oprobit.reg(rvida~rsexo + rpareja+ redadsq + redadsq + reduca+rmu+
                      rly+ rmiembros ,data=data)
summary(m1_1)
margins.oglmx(m1_1)

# Forma 2
#-------------------------------------------------------
m2_0 <- oglmx(rvida~1, data=data, link="probit", 
              constantMEAN = F, delta=0,threshparam=NULL)
summary(m2_0)

# Extraer coeficientes y modelo en latex
m2_oglmx     <- extract(m2_0,include.aic=TRUE)
t2_oglmx     = texreg(m2_oglmx)
t2_oglmx
write(t2_oglmx, "Tabla_oglmx_1.tex")

# Ordered probit with variables
m2_1 <- oglmx(rvida~rsexo+ rpareja+ redad + redadsq + reduca+rmu+
                rly + rmiembros , data=data, link="probit", 
            constantMEAN = F, delta=0,threshparam=NULL)
summary(m2_1)

# Extraer coeficientes y modelo en latex
m3_oglmx <- extract(m2_1,include.aic=TRUE)
t2_oglmx  = texreg(m3_oglmx)
t2_oglmx
write(t2_oglmx, "Tabla_oglmx_2.tex")

# Marginal effects
Hmisc::describe(data$rvida)
margins.oglmx(m2_1)
mfe <- margins.oglmx(m2_1, atmeans = TRUE, outcomes = "All" )
mfe

# Pregunta 3
#---------------------------------------------------------------------
summary(m2_1)
edad_opt <- -((m2_1$coefficients['redad'])/(2*m2_1$coefficients['redadsq']))
edad_opt
data$y_hat <- m2_1$coefficients['redad']*data$redad+ m2_1$coefficients['redadsq']*data$redadsq  
plot(data$redad, data$y_hat)

# Pregunta 4-5
#----------------------------------------------------------------------
# Probabilidades ; efectos marginales
mfe <- margins.oglmx(m2_1, atmeans = TRUE, outcomes = "All" )
mfe


rm(list = ls())
# rutas de librerias
#----------------------------------------------------------------
setwd("C:/Users/et396/Dropbox/Docencia/Educate/Econometria/S2/Data") # cambiar su ruta aqui


paquetes_set <- c("rio", "dplyr", "tidyverse", "oglmx", "mfx",
                  "stargazer", "texreg", "sjPlot", "ggplot2","readstata13")
#install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

# Import dataset
#----------------------------------------------------------------
base <- read.dta13("ENE_PANEL_Manufacturing.dta")
base %>%  names()       # names of variables
base %>%  str()         # structure data


Hmisc::describe(base$rexporta)

firms <- base %>%
  group_by(rid) %>%
  mutate(
    lnrproductivity_1 = dplyr::lag(lnrproductivity, n=1),
    redad_1 = dplyr::lag(redad, n=1),
    redadsq_1 = dplyr::lag(redadsq,n=1),
    rmype_1 = dplyr::lag(rmype, n=1)
  )

data_fit <- firms


# Question 1
# ==========================================================
# Modelo LOGIT- BINOMIAL
# Modelo de probabilidad lineal
m1_ols <- lm(rexporta ~ lnrproductivity_1 , data = data_fit)
m1_probit <- glm(rexporta ~ lnrproductivity_1, data = data_fit, family = binomial(link = "probit") )

stargazer(m1_ols, m1_probit, title="Results", type="text")

# Question 2
# ==========================================================
m1 <- glm(rexporta ~ lnrproductivity_1 , data = data_fit, family = binomial(link = "probit") )
m2 <- glm(rexporta ~ lnrproductivity_1 + redad_1 + redadsq_1 + rmype_1, data = data_fit, family = binomial(link = "probit") )
m3 <- glm(rexporta ~ lnrproductivity_1 + redad_1 + redadsq_1 + rmype_1 + factor(region), data = data_fit, family = binomial(link = "probit") )

stargazer(m1, m2, m3, title="Results", type="text")

# Question 3
# ==========================================================
m3 <- glm(rexporta ~ lnrproductivity_1 + redad_1 + redadsq_1 + rmype_1 + factor(region), data = data_fit, family = binomial(link = "probit") )

stargazer(m3, title="Results", type="text", keep = c("lnrproductivity_1", "redad_1",'redadsq_1','rmype_1'))


tabla1 <-  texreg(list(m3), label = "tab:1",
               #custom.coef.names = c(),
               custom.model.names = c("MPL model"),
               caption = "Probit",
               float.pos = "h", 
               return.string = TRUE, 
               bold = 0.05, 
               stars = c(0.01, 0.05, 0.1),
               custom.note = "Coefficients with $p < 0.05$ in \\textbf{bold}.",
               digits = 3, 
               leading.zero = FALSE, 
               omit.coef = "Inter")
tabla1
# export to latex
#write(tabla1, "tabla_1.tex")

# efectos marginales 
t1 <- probitmfx( rexporta ~ lnrproductivity_1 + redad_1 + redadsq_1 + rmype_1 + factor(region),
                 data_fit)

# Generar table en latex
# Preparar la tabla
modelss = list(t1$fit )
coefs   = list(c(0, t1$mfxest[, 1]) )
ses     = list(c(0, t1$mfxest[, 2]) )
pvals   = list(c(0, t1$mfxest[, 4]) )

te1 = texreg(modelss,
             override.coef = coefs,
             override.se = ses,
             override.pval = pvals,
             omit.coef = "(Intercept)",
             caption.above = TRUE,
             caption = "Modelo Probit. Marginal
             Effects",
             digits = 2, 
             #dcolumn = TRUE,
             #custom.note = "\%stars.",
             stars = c(0.01, 0.05, 0.1),
             custom.model.names = c("Probit (mean)"),
             return.string = TRUE)
te1

# export to latex
write(te1, "model_nolineal_margins.tex")
write(te1, "T_5_stata.tex")

# Question 4
# ==============================================================

m3 <- glm(rexporta ~ lnrproductivity_1 + redad_1 + redadsq_1 + rmype_1 + factor(region), data = data_fit, family = binomial(link = "probit") )
m4 <- glm(rexporta ~ lnrproductivity_1 + redad_1 + redadsq_1 + rmype_1 + factor(region), data = data_fit, family = binomial(link = "logit") )

stargazer(m3, m4, title="Results", type="text", keep = c("lnrproductivity_1", "redad_1",'redadsq_1','rmype_1'))


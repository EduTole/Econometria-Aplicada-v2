#-----------------------------
# Autor: Edinson Tolentino
# Proyecto : Nivel de hogar
#-----------------------------
rm(list=ls())

#-------------------------------------------------------------
# Carpetas de informacion
#-------------------------------------------------------------
ruta   <- "C:/Users/et396/Dropbox"
base   <- "/BASES/ENAHO"
codigo <- "//Docencia/Educate/Econometria/S3/Data"
out    <- "/Docencia/Educate/Econometria/S3/Data"

# Importar librerias
# Paquetes para instalar y poder usarlos
paquetes_set <- c("readstata13", "dplyr", "tidyverse", 
                  "survey", "stargazer", "caret", "foreign")
#install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

# comments    :   ctrl  + shift   + C
# no-comments  : 	Ctrl  + Shift   + C
# shortcuts   : https://support.posit.co/hc/en-us/articles/200711853-Keyboard-Shortcuts-in-the-RStudio-IDE
#-----------------------------------------------------------------------------
# factor de funciones
#-----------------------------------------------------------------------------
source(paste0(ruta,codigo,"/","Funciones_MTPE.R"))

#-----------------------------------------------------------------------------
# Base de datos de niveles
#-----------------------------------------------------------------------------
data <- read.dta13(paste0(ruta,"/",base,"/","2021","/","enaho01b-2021-2.dta"))

# Measure y depended variable  
base_hogar <- funcion_rhogar(data)                # Nivel de calidad del hogar

# filtro de variables
base_hogar <- base_hogar %>% 
  mutate(rcod_persona=paste0(conglome,vivienda, hogar,codperso),
         rcod_hogar=paste0(conglome,vivienda, hogar)) %>% 
  dplyr::select(rcod_hogar,rcod_persona, rhogar, rvida)

base_hogar %>%  str()     # structure de datasets
base_hogar %>%  names()   # names variables
base_hogar %>%  head()    # First observations
table(base_hogar$rhogar)  # table command

Hmisc::describe(base_hogar$rhogar)
Hmisc::describe(base_hogar$rvida)
Hmisc::describe(data$p37)

rm(data)                  # limpiar base usada
#-----------------------------------------------------------------------------
# Base de enpleo
#-----------------------------------------------------------------------------
data <- read.dta13(paste0(ruta,"/",base,"/","2021","/","enaho01a-2021-500.dta"))

# Filtro de MTPE
base_empleo <- funcion_rfiltro(data)
base_empleo <- base_empleo %>% 
  filter(rfiltro==1)

# Filtro de ingresos, departamento, edad, mujer y nivel educativo
base_empleo <- funcion_r500(base_empleo)
base_empleo <- funcion_rDpto(base_empleo)
base_empleo <- funcion_redad(base_empleo)
base_empleo <- funcion_rmujer(base_empleo)
base_empleo <- funcion_rneduca(base_empleo)
base_empleo <- funcion_rpareja(base_empleo)

# Solo jefe de hogar
base_empleo <- funcion_rjefe(base_empleo)

base_empleo %>%  dim()
base_empleo <- base_empleo %>% 
  filter(rjefe=="jefe")
base_empleo %>%  dim()

# filtro de solo PEA
base_empleo <- base_empleo %>% 
  filter(ocu500<=2)
base_empleo %>%  dim()
#base_empleo %>%  tail()

# filtro de variables 
base_empleo <- base_empleo %>% 
  mutate(rcod_persona=paste0(conglome,vivienda, hogar,codperso),
         rcod_hogar=paste0(conglome,vivienda, hogar),
         lnr6=ifelse(r6>0,log(r6),NA)) %>% 
  dplyr::select(rcod_persona,rcod_hogar,r6,lnr6,rDpto, redad, rmujer, rneduca,
                rmu, rocupado, rinfo, rcivil, rcivil_rpareja, rcivil_rsoltero, 
                rinfo)

base_empleo %>%  names()
base_empleo %>%  head()
base_empleo %>%  dim()
base_empleo %>%  summary()

rm(data)
#-----------------------------------------------------------------------------
# Base de poverty
#-----------------------------------------------------------------------------
data <- read.dta13(paste0(ruta,"/",base,"/","2021","/","sumaria-2021.dta"))

# Mesuare poverty 
base_poverty <- funcion_rpoverty(data)        # Mediicion de la Pobreza
base_poverty <- funcion_rDpto(base_poverty)   # Departamento

# Filtro de las variables creadas y nueva base a crear
base_poverty <- base_poverty %>% 
  mutate(rcod_hogar=paste0(conglome,vivienda, hogar),
         lnrgasto=log(rgasto)) %>% 
  dplyr::select(rcod_hogar,rpoverty, rpoverty_f, rpoverty_c, 
                rgasto,lnrgasto, rpeople, factor07)

base_poverty %>%  dim()
base_poverty %>% head()
Hmisc::describe(base_poverty$rpoverty_f)
Hmisc::describe(base_poverty$rpoverty_c)

rm(data)
#------------------------------------------------
# Base de datos para educacion
#------------------------------------------------
data <- read.dta13(paste0(ruta,"/",base,"/","2021","/","enaho01a-2021-300.dta"))

# Nivel de educacion
#------------------------------------------
base_educa <- funcion_reduca(data)
base_educa <- base_educa %>% 
  mutate(rcod_persona =paste0(conglome,vivienda, hogar,codperso)) %>% 
  dplyr::select(rcod_persona, reduca)

base_educa %>% names()
base_educa %>% summary()

#-----------------------------------------------------------------------------
# Union de bases de datos
#-----------------------------------------------------------------------------
base_unida <- merge(base_empleo, base_educa  , by=c("rcod_persona"))
base_unida <- merge(base_unida, base_hogar  , by=c("rcod_persona","rcod_hogar"))
base_unida <- merge(base_unida , base_poverty , by=c("rcod_hogar"))

base_unida %>%  dim()
base_unida %>%  names()
base_unida %>%  head()
base_unida %>%  str()
base_unida %>%  summary()

Hmisc::describe(base_unida$rneduca)
table(base_unida$rmu)  
prop.table(table(base_unida$rmu))

# Limpieza de informacion
# No missing values
base_final <- base_unida %>% 
  drop_na( rmu, rneduca, rvida, rhogar ) # limpiamos de missing values los valores
base_final %>%  dim()
base_final %>%  summary() # se tiene un toral de 19,809 observaciones 
base_final %>%  str()

# agregar la edad al cuadrado
base_final <- base_final %>% 
  mutate(redadsq = redad*redad )

Hmisc::describe(base_final$rhogar)
Hmisc::describe(base_final$rvida)

#-----------------------------------------------------------------------------
# Exportar la informacion final 
#-----------------------------------------------------------------------------
#library("writexl") # exportar informacion hacia CSV
write.csv(base_final, file = paste0(ruta,"/",out,"/","BD4_Multiproducto_2021.csv"))
#require(foreign) # exportar informacion hacia STATA
write.dta(base_final, file = paste0(ruta,"/",out,"/","BD4_Multiproducto_2021.dta"))
# Library dataset RDS
saveRDS(base_final, file = paste0(ruta,"/",out,"/","BD4_Multiproducto_2021.RDS"))

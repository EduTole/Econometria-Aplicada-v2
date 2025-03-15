
cls
clear all
glo Data		"C:\Users\et396\Dropbox\Docencia\Educate\Econometria\S1\Data"
glo Out			"C:\Users\et396\Dropbox\Docencia\Educate\Econometria\S1\Data"

*Carga de Data
u "${Data}/Mincer_2021.dta",clear
d

*Modelo Multivariado
*-----------------------------------
*Analisis exploratorio
glo Xs "r6 reduca rmujer redad redadsq rpareja"
glo Zs "reduca rmujer redad redadsq rpareja"
sum $Xs

*Pregunta 1a)
reg lnr6 reduca
reg lnr6 $Zs

*Pregunta 1b) Estimacion de errores del modelo		
*--------------------------------------------------------------
reg lnr6 $Zs
*Prediciones de los errores del modelo
predict uhat,resid

*Bosquejo de la densidad kernel de las estimaciones
*del error y tets para nomalidad de la distribucion de errors
kdensity uhat
sktest uhat, noadj 
graph export "${Out}/t1.png", replace

*Grafico de caja
graph box uhat
graph export "${Out}/t2.png", replace

*Pregunta 1c) prueba de errores de heterocedasticidad	
*--------------------------------------------------------------
*Test de Koenker para heterocedasticidad
hettest $Zs, iid

*Test manual de heterocedasticidad
gen uhatsq=uhat^2
label var uhatsq "$\mu^{2}$"

reg uhatsq $Zs
scalar r2 = e(r2)
scalar sample = e(N)
scalar lm_het = r2*sample
display lm_het

*Pregunta 2
*-------------------------------------------------------------
*Pregunta 2a) prediccion de los errores
eststo clear	
reg lnr6 reduca ,robust
reg lnr6 reduca rmujer ,r
reg lnr6 $Zs,r
	
*Pregunta 2b) prediccion de los errores
*Test de Wald: pareja y mujer
reg lnr6 reduca redad redadsq rpareja rmujer , r
*Extraer matrices
matrix b=e(b) 
matrix list b
matrix vb=e(V) 
matrix list vb

matrix bi=b[1,4..5] 
matrix vi=vb[4..5,4..5]
matrix w_test=bi*inv(vi)*bi'
matrix list w_test

test rmujer rpareja

*Pregunta 3
*--------------------------------------------------------------

reg lnr6 $Zs

scalar edad_optima = _b[redad]/(-2*_b[redadsq])
display edad_optima


scalar beta3 = _b[redad]/(-2*(_b[redadsq]^2))
scalar beta2 = 1/(-2*_b[redad])

scalar beta2_sq = beta2^2
scalar beta3_sq = beta3^2

*Extraer matrices
matrix b=e(b) 
matrix list b
matrix vb=e(V) 
matrix list vb

matrix vage=vb[3..4,3..4]
matrix list vage

* Metodo delta
scalar var_beta2 = vage[1,1]
scalar var_beta3 = vage[2,2]
scalar cov_beta2_beta3 = vage[2,1]

scalar delta = (beta2_sq*var_beta2) + (beta3_sq*var_beta3) + (beta2*beta3*cov_beta2_beta3)
display delta

nlcom - _b[redad]/(2*_b[redadsq]) - 50		
gen lnr6_predicted=_b[redad]*rexper +_b[redadsq]*redadsq
scatter lnr6_predicted redad		
graph export "${Imagen}/t3.png", replace	

reg lnr6 $Zs, r
test rmujer 
test rmujer rpareja

*Pregunta 4
*--------------------------------------------------------------
*Rgresion por separado

reg lnr6 $Zs
scalar ssrc=e(rss)
scalar dfc=e(df_r)
disp ssrc
disp dfc

reg lnr6 reduca  redad redadsq rpareja if rmujer==1 
scalar ssrum=e(rss)
scalar dfum=e(df_r)
disp ssrum
disp dfum

reg lnr6 reduca  redad redadsq rpareja if rmujer==0 
scalar ssruf=e(rss)
scalar dfuf=e(df_r)
disp ssruf
disp dfuf

***Test de Chow
scalar ssru=ssrum+ssruf
scalar dfu=dfum+dfuf
scalar g=dfc-dfu
scalar numer=((ssrc-ssru)/g)
scalar denom=(ssru/dfu)
scalar Chow_test=numer/denom
display Chow_test
display Ftail(g,dfu,Chow_test)

reg lnr6 $Zs
reg lnr6 reduca  redad redadsq rpareja if rmujer==1 
reg lnr6 reduca  redad redadsq rpareja if rmujer==0 


*OLS INTERACCIONES	
g i1=rmujer*reduca
g i2=rmujer*redad
g i3=rmujer*redadsq
g i4=rmujer*rpareja

label var i1 "educa x mujer"
reg lnr6 rmujer reduca redad redadsq rpareja i1 i2 i3 i4
reg lnr6 rmujer reduca redad redadsq rpareja
reg lnr6 reduca redad redadsq rpareja if rmujer==1
reg lnr6 reduca redad redadsq rpareja if rmujer==0
reg lnr6 rmujer reduca redad redadsq rpareja i1 i2 i3 i4

test i1 
test i1 i2
test i1 i2 i3 i4
	
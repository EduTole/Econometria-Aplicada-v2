cls
clear all
gl main "C:\Users\et396\Dropbox\Docencia\Cientifica\11. Verano\Econometrics\S5\Data"
gl Data 	"${main}/Data"
gl Tabla 	"${main}/Tablas"
gl Figure 	"${main}/Imagen"


	use "$Data\BD5.dta",clear
	
	sum 
	
	* Pregunta 1
	*Modelo MCO
	reg logwage educ female
	
	*Estimacion de regresion estructural
	reg educ female test11r test11m mumso dadso
	test test11r test11m mumso dadso
	predict educhat_x
	*Ahora regresion de segunda etapa
	reg logwage educhat_x female	
	
	* Pregunta 3
	ivreg logwage (educ = test11r test11m mumso dadso) female
	
	*Forma reducida
	reg educ female test11r test11m mumso dadso
	test test11r test11m mumso dadso	
	
	* Pregunta 4
	ivreg logwage (educ = test11r test11m mumso ) female
	predict uhat_iv2,resid	
	
	* Pregunta 5
	reg educ female test11r test11m mumso
	predict ehat,resid	
	reg logwage educ female ehat
	
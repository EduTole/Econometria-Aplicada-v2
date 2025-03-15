	cls
	clear all
	
	gl main 		"C:\Users\et396\Dropbox"
	gl base 		"${main}/BASES\ENE-INEI\BASES_NORMALIZADAS"
	gl Out			"${main}\BASES\ENE-INEI\BASES_PANEL"

	
	* Import datasets
	use "${Out}/ENE_PANEL_Manufacturing.dta",clear
	tab ryear
	xtset rid ryear
	d
	sum 
	
	* Graph
	tw (kdensity lnrproductivity if rexporta==1) (kdensity lnrproductivity if rexporta==0), legend(label(1 "Exporta") label(2 "No-Exporta"))
	graph box lnrproductivity , over(rexporta)
	
	* main dependend variables
	tab rexporta
	
	* variables explicativas
	glo Xs 		"L1.lnrproductivity L1.lnredad L1.rmype"
	glo Xws 	"L1.lnrproductivity L1.redad L1.rmype L1.redadsq"
	glo Zs 		"L1.lnrproductivity L1.lnredad L1.rmype i.ryear i.region"
	glo Zws 	"L1.lnrproductivity L1.redad L1.redadsq L1.rmype i.ryear i.region"
	
	* Pregunta 1
	*========================================================
	* Model OLS - MPL
	reg rexporta L1.lnrproductivity , r
	estimate store m_ols
			
	* Modelo Probit
	probit 	rexporta L1.lnrproductivity , r
	estimate store m_probit

	estimates table m_ols m_probit, b(%7.4f) stats(N aic) star
	estimates table m_ols m_probit, b(%7.4f) se(%7.4f) stats(N aic)
	
	
	* Pregunta 2
	*========================================================
	*No lineal - redad
	probit 	rexporta $Zws  , r

	scalar edad_opt = -(_b[L1.redad] / (2*_b[L1.redadsq]))
	display edad_opt
	
	* Pregunta 3
	*========================================================
	probit 	rexporta $Zws  , r
	margins, dydx(*) 
	
	
	* Pregunta 4
	*========================================================
	* Logit vs Probit
	probit 	rexporta $Zws  , r
	estimate store mnl_probit
	logit 	rexporta $Zws  , r
	estimate store mnl_logit
	estimates table mnl_probit mnl_logit, b(%7.4f) stats(N aic) star

	
	cls
	clear all
	
	gl main 		"C:\Users\et396\Dropbox"
	gl base 		"${main}/BASES\ENE-INEI\BASES_NORMALIZADAS"
	gl Out			"${main}\BASES\ENE-INEI\BASES_PANEL"
	gl Tablas		"${main}\Docencia\Educate\Econometria\S2\Tablas"
	
	* Import datasets
	use "${Out}/ENE_PANEL_Manufacturing.dta",clear
	d
	sum 
	
	glo Zws 	"lnrproductivity redad redadsq rmype ryear region"
	
	eststo clear
	estpost tabstat rexporta $Zws , s(n mean p50 min max sd) col(stat) 

	esttab using "${Tablas}\T_1_stata.tex", ///
	c("count(label(Firms)) mean(label(Promedio) fmt(%10.2fc)) p50(label(Mediana) fmt(%10.2fc) ) min(label(Min.) fmt(%10.2fc)) max(label(Max.) fmt(%10.2fc)) sd(label(Std) fmt(%10.0fc))") ///
	label nomtitles nodepvars noobs nonumbers booktabs prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T1} Estadisticas descriptivas }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Fuente: ENE - 2014-2017. ///
		\item[] Elaboracion: Autor  ///
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
		
	* Graph
	tw (kdensity lnrproductivity if rexporta==1) (kdensity lnrproductivity if rexporta==0), legend(label(1 "Exporta") label(2 "No-Exporta"))
	graph box lnrproductivity , over(rexporta)
	
	* main dependend variables
	xtset rid ryear
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
	
	
	eststo clear
	eststo: reg rexporta L1.lnrproductivity , r
	eststo: probit 	rexporta L1.lnrproductivity , r

	esttab using "${Tablas}\T_3_stata.tex",  label booktabs b(3) se(2) nonumber star(* 0.10 ** 0.05 *** 0.01) varlabels(`e(labels)') mtitles("MCO" "Probit") prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T3} Modelo No Lineal }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Errores estandar en parentesis. ///
		\item[] Fuente: ENE 2014-2017. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
	
	* Pregunta 2
	*========================================================
	*No lineal - redad
	probit 	rexporta $Zws  , r

	eststo clear
	eststo: probit 	rexporta L1.lnrproductivity , r
	eststo: probit 	rexporta $Xws  , r
	eststo: probit 	rexporta $Zws  , r

	esttab using "${Tablas}\T_4_stata.tex",  label booktabs b(3) se(2) nonumber star(* 0.10 ** 0.05 *** 0.01) drop( *region *ryear _cons ) varlabels(`e(labels)') mtitles("Exporta" "Exporta" "Exporta") prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T4} Modelo No Lineal }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Errores estandar en parentesis. ///
		\item[] Fuente: ENE 2014-2017. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace
	
	* relacion no lineal de la edad
	scalar edad_opt = -(_b[L1.redad] / (2*_b[L1.redadsq]))
	display edad_opt
	
	* Pregunta 3
	*========================================================
	probit 	rexporta $Zws  , r
	margins, dydx(*) 
	
	eststo clear
	quietly probit rexporta $Zws  
	margins , dydx(*) post
	
	esttab using "${Tablas}/T_5_stata.tex",  label booktabs b(3) se(2) nonumber  star(* 0.10 ** 0.05 *** 0.01) drop( *region *ryear) mtitles("Efectos Marginales" ) prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T3} Efectos Marginales probit }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Errores estandar en parentesis. ///
		\item[] Fuente: INEI -2021. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace 
		6
	* Pregunta 4
	*========================================================
	* Logit vs Probit
	probit 	rexporta $Zws  , r
	estimate store mnl_probit
	logit 	rexporta $Zws  , r
	estimate store mnl_logit
	estimates table mnl_probit mnl_logit, b(%7.4f) stats(N aic) star


	eststo clear
	eststo: probit 	rexporta $Zws  , r
	eststo: logit 	rexporta $Zws  , r

	esttab using "${Tablas}\T_6_stata.tex",  label booktabs b(3) se(2) nonumber star(* 0.10 ** 0.05 *** 0.01) drop( *region *ryear _cons ) varlabels(`e(labels)') mtitles("Probit" "Logit") prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T6} Modelo No Lineal }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Errores estandar en parentesis. ///
		\item[] Fuente: ENE 2014-2017. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace	
		
		
	eststo clear
	*reg rpublica $Xs 
	
	foreach o in reg probit logit{
	quietly `o' rexporta $Zws  , r
	estadd local Fixed1 "$\surd$",replace
	quietly margins, dydx(*) post
	eststo, title(Prodcuto `o')
	*estimates restore `o'
 }		
	*eststo drop probit
	
	
	esttab using "${Tablas}/T_7_stata.tex",  label booktabs b(3) se(4) nonumber  star(* 0.10 ** 0.05 *** 0.01) mtitles("MPL" "Probit" "Logit" ) drop( *region *ryear  ) stats(N r2_p ll k Fixed1, layout(@) fmt(a3 a3 a3 a2 a2 ) labels("Observaciones"  "Pseudo. R$^2$" "Log-L" "Grados de Libertad (k)" "Controls")  )  prehead("\begin{table}[H] \scriptsize \centering \begin{threeparttable} \protect \caption{\label{tab:T3} Efectos Marginales Modelos No Lineales }  \begin{tabular}{lrrrrrrrr}" \hline \hline) ///
		posthead(\hline) prefoot() postfoot(\hline \end{tabular} ///
		\begin{tablenotes} ///
		\begin{footnotesize} ///
		\item[] Errores estandar en parentesis. ///
		\item[] Fuente: ENE -2014-2017. ///
		\item[] Elaboracion: Autor  ///
		\item[] ***, **, * denote statistical significance at the 1\%, 5\% and 10\% levels respectively for zero.	///	
		\end{footnotesize} ///
		"\end{tablenotes} \end{threeparttable} \end{table}" ) replace		
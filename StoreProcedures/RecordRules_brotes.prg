FUNCTION RecValidationRuleFor_brotes

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El total pacientes Hospitalarios + Ambulatorios debe ser igual al total por grupos de edad'
			bIsValid = VAL(BROTES.gru_1)+VAL(BROTES.gru_2)+VAL(BROTES.GRUPO_3) + VAL(BROTES.GRUPO_4) + VAL(BROTES.GRUPO_5) + VAL(BROTES.GRUPO_6) + 		VAL(BROTES.GRUPO_7) +	VAL(BROTES.GRUPO_8) + VAL(BROTES.GRUPO_9) + VAL(BROTES.GRUPO_10) + VAL(BROTES.GRUPO_11) + ;
						VAL(BROTES.GRUPO_12) + VAL(BROTES.GRUPO_13) + VAL(BROTES.GRUPO_14) + VAL(BROTES.GRUPO_15) + VAL(BROTES.GRUPO_16) + VAL(BROTES.GRUPO_17) +	VAL(BROTES.GRUPO_18) = VAL(BROTES.pte_hos) + VAL(BROTES.pte_amb)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es 350 (ETA), el total por grupo de edad debe ser mayor que uno (>1)'
			bIsValid = BROTES.COD_EVE != '350' OR VAL(BROTES.gru_1)+VAL(BROTES.gru_2)+VAL(BROTES.GRUPO_3) + VAL(BROTES.GRUPO_4) + VAL(BROTES.GRUPO_5) + VAL(BROTES.GRUPO_6) + 		VAL(BROTES.GRUPO_7) +	VAL(BROTES.GRUPO_8) + VAL(BROTES.GRUPO_9) + VAL(BROTES.GRUPO_10) + VAL(BROTES.GRUPO_11) + ;
						VAL(BROTES.GRUPO_12) + VAL(BROTES.GRUPO_13) + VAL(BROTES.GRUPO_14) + VAL(BROTES.GRUPO_15) + VAL(BROTES.GRUPO_16) + VAL(BROTES.GRUPO_17) +	VAL(BROTES.GRUPO_18)> 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El Total de Casos por Clasificaciones (Probables, Confirmados por Laboratorio, por Clínica y por Nexo) debe ser igual al total por grupos de edad'
			bIsValid = VAL(BROTES.cas_sos)+VAL(BROTES.cas_pro)+VAL(BROTES.cas_conl)+VAL(BROTES.cas_conc)+VAL(BROTES.cas_conn) = VAL(BROTES.gru_1)+VAL(BROTES.gru_2)+VAL(BROTES.GRUPO_3) + VAL(BROTES.GRUPO_4) + VAL(BROTES.GRUPO_5) + VAL(BROTES.GRUPO_6) + 		VAL(BROTES.GRUPO_7) +	VAL(BROTES.GRUPO_8) + VAL(BROTES.GRUPO_9) + VAL(BROTES.GRUPO_10) + VAL(BROTES.GRUPO_11) + ;
						VAL(BROTES.GRUPO_12) + VAL(BROTES.GRUPO_13) + VAL(BROTES.GRUPO_14) + VAL(BROTES.GRUPO_15) + VAL(BROTES.GRUPO_16) + VAL(BROTES.GRUPO_17) +	VAL(BROTES.GRUPO_18)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El Total de Casos por Sexo, debe ser igual al total por grupos de edad'
			bIsValid = VAL(BROTES.hombres)+VAL(BROTES.mujeres) = VAL(BROTES.gru_1)+VAL(BROTES.gru_2)+VAL(BROTES.GRUPO_3) + VAL(BROTES.GRUPO_4) + VAL(BROTES.GRUPO_5) + VAL(BROTES.GRUPO_6) + 		VAL(BROTES.GRUPO_7) +	VAL(BROTES.GRUPO_8) + VAL(BROTES.GRUPO_9) + VAL(BROTES.GRUPO_10) + VAL(BROTES.GRUPO_11) + ;
						VAL(BROTES.GRUPO_12) + VAL(BROTES.GRUPO_13) + VAL(BROTES.GRUPO_14) + VAL(BROTES.GRUPO_15) + VAL(BROTES.GRUPO_16) + VAL(BROTES.GRUPO_17) +	VAL(BROTES.GRUPO_18)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El Total de Casos por Condición Final, debe ser igual al total por grupos de edad'
			bIsValid = VAL(BROTES.vivos)+VAL(BROTES.muertos) =VAL(BROTES.gru_1)+VAL(BROTES.gru_2)+VAL(BROTES.GRUPO_3) + VAL(BROTES.GRUPO_4) + VAL(BROTES.GRUPO_5) + VAL(BROTES.GRUPO_6) + 		VAL(BROTES.GRUPO_7) +	VAL(BROTES.GRUPO_8) + VAL(BROTES.GRUPO_9) + VAL(BROTES.GRUPO_10) + VAL(BROTES.GRUPO_11) + ;
						VAL(BROTES.GRUPO_12) + VAL(BROTES.GRUPO_13) + VAL(BROTES.GRUPO_14) + VAL(BROTES.GRUPO_15) + VAL(BROTES.GRUPO_16) + VAL(BROTES.GRUPO_17) +	VAL(BROTES.GRUPO_18)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El evento ETA Colectivo - 350 solo permite el ingreso de casos  por NEXO EPIDEMIOLOGICO  si se ha ingresado por lo menos 1 caso CONFIRMADO POR LABORATORIO'
			bIsValid = BROTES.COD_EVE!='350'  OR ((VAL(BROTES.CAS_CONN)<1 OR VAL(BROTES.CAS_CONL)>=1) )
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento ETA Colectivo - 350 al ingresar casos CONFIRMADOS POR CLINICA no se permite el ingreso de casos por OTRO TIPO DE CASO'
			bIsValid = BROTES.COD_EVE!='350' OR VAL(BROTES.CAS_CONC)<1 OR (VAL(BROTES.CAS_PRO)=0 AND VAL(BROTES.CAS_CONL)=0 AND VAL(BROTES.CAS_CONN)=0)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La semana epidemiológica NO debe ser superior a la determinada por la fecha actual de grabación'
			bIsValid = isValidEpidemiologicalWeek(BROTES.SEMANA,BROTES.AÑO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El municipio no es coherente con el código de UPGD'
			bIsValid = LEFT(BROTES.COD_PRE,2)!='11' OR LEFT(BROTES.COD_MUN,2)='11'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El municipio de notificación no es coherente con el código de UPGD'
			bIsValid = LEFT(BROTES.COD_PRE,2)='11' OR (LEFT(BROTES.COD_PRE,5)=BROTES.COD_MUN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Debe haber por lo menos uno de los grupos de edad con más de 0 casos presentados'
			bIsValid = VAL(GRU_1) + VAL(GRU_2) + VAL(GRUPO_3) + VAL(GRUPO_4) + VAL(GRUPO_5) + VAL(GRUPO_6) + VAL(GRUPO_7) + VAL(GRUPO_8) + VAL(GRUPO_9) + VAL(GRUPO_10) + VAL(GRUPO_11) + VAL(GRUPO_12) + VAL(GRUPO_13) + VAL(GRUPO_14) + VAL(GRUPO_15) + VAL(GRUPO_16) + VAL(GRUPO_17) + VAL(GRUPO_18) > 0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


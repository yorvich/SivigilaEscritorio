FUNCTION RecValidationRuleFor_upgd

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La Longitud del Nit no debe ser inferior a 5 Caracteres'
			bIsValid = EMPTY(UPGD.nit_upgd) OR LEN(ALLTRIM(UPGD.nit_upgd)) >= 5
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El correo electrónico de la UPGD no debe estar en blanco, debe tener al menos un carácter @'
			bIsValid = NOT EMPTY(UPGD.cor_ele) AND AT('@', UPGD.cor_ele) != 0
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de constitución no puede ser anterior a 1900'
			bIsValid = EMPTY(UPGD.fec_con) OR YEAR(UPGD.fec_con) >= 1900
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la UPGD está cerrada, no puede estar activa'
			bIsValid = UPGD.ESTADOUPGD!='2' OR UPGD.ACT_SIV=2
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ha diligenciado "Notifica IAD" en 1, debe diligenciar todas las variables pertinentes de la segunda sección de la ficha'
			bIsValid = UPGD.NOTIF_IAD!='1' OR (!EMPTY(UPGD.HOSP_UNIVE) AND !EMPTY(UPGD.REG_EXCEPC) AND !EMPTY(UPGD.TOT_CAMAS ) AND !EMPTY(UPGD.COMITE_INF) AND !EMPTY(UPGD.BI_PROFESI) AND !EMPTY(UPGD.IAAS_ULTIM) AND !EMPTY(UPGD.INF_TENDEN) AND !EMPTY(UPGD.SOCIAL_TEN) AND !EMPTY(UPGD.LAB_MICROB) AND !EMPTY(UPGD.MICR_CDI) AND !EMPTY(UPGD.WHONET ) AND !EMPTY(UPGD.INFORM_PAT) AND !EMPTY(UPGD.LAB_CON_PE) AND !EMPTY(UPGD.LAB_REM_CE) AND !EMPTY(UPGD.LAB_REPORT))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "La UPGD dispone de servicio de laboratorio de microbiología", debe diligenciar todas las variables de laboratorio'
			bIsValid = UPGD.LAB_MICROB!='1' OR (!EMPTY(UPGD.LAB_PROPIO) AND !EMPTY(UPGD.IDENT_GYE ) AND !EMPTY(UPGD.PRUE_SUCEP) AND !EMPTY(UPGD.LAB_AUTOMA) AND !EMPTY(UPGD.VITEK) AND !EMPTY(UPGD.MICROSCAN ) AND !EMPTY(UPGD.PHOENIX) AND !EMPTY(UPGD.LAB_CCI) AND !EMPTY(UPGD.LAB_CCE))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ha diligenciado "Notifica IAD" en 1, el nivel de complejidad de la institución debe ser superior a 1'
			bIsValid = UPGD.NOTIF_IAD!='1' OR INLIST(UPGD.NIV,2,3)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ha diligenciado "Notifica ISO" en 1, debe diligenciar todas las variables de la sección "DATOS DE LA VIGILANCIA DE LAS INFECCIONES ASOCIADAS A PROCEDIMIENTOS MÉDICO QUIRÚRGICOS"'
			bIsValid = UPGD.NOTIF_ISO!='1' OR (!EMPTY(UPGD.SER_CESARE) AND !EMPTY(UPGD.SER_PAR_VA) AND !EMPTY(UPGD.SER_COLECI) AND !EMPTY(UPGD.SER_HERNIO) AND !EMPTY(UPGD.SER_REVASC))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ha diligenciado "Notifica CAB" en 1, debe diligenciar todas las variables de la sección "DATOS DE LA VIGILANCIA DE CAB"'
			bIsValid = UPGD.NOTIF_CAB!='1' OR (!EMPTY(UPGD.QUIEN_VCAB) )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


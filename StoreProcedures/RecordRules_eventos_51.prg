FUNCTION RecValidationRuleFor_eventos_51

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() AND !IsInmediateNotification()  THEN
		IF bIsValid THEN
			sRuleViolated = 'La variable "Recolección de evidencia médico legal" es de obligatorio diligenciamiento para las opciones 6, 7, 10, 14 de violencia sexual'
			bIsValid = !(',' + ALLTRIM(EVENTOS_51.NAT_VIOSEX) + ',' $  ',6,7,10,14,') OR (!EMPTY(EVENTOS_51.EVI_MLEGAL))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La variables "Recolección de evidencia médico legal" es de obligatorio diligenciamiento para las opciones 6, 7, 10, 14 de violencia sexual'
			bIsValid = !(',' + ALLTRIM(EVENTOS_51.NAT_VIOSEX) + ',' $  ',6,7,10,14,') OR (!EMPTY(EVENTOS_51.EVI_MLEGAL))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Almenos una de las variables de modalidad de la violencia debe estar diligenciada'
			bIsValid = !EMPTY(EVENTOS_51.NATURALEZA) OR !EMPTY(EVENTOS_51.NAT_VIOSEX)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Debe existir por lo menos una variable diligenciada que dé cuenta de la relación con el agresor, ya sea familiar o no familiar'
			bIsValid = !EMPTY(EVENTOS_51.R_FAM_VIC + EVENTOS_51.R_NOFILIAR)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Las variables de atención en salud son de obligatorio diligenciamiento para las opciones 6, 7, 10, 14, 15 de violencia sexual'
			bIsValid = !(',' + ALLTRIM(EVENTOS_51.NAT_VIOSEX) + ',' $  ',6,7,10,14,15,') OR (!EMPTY(EVENTOS_51.SP_ITS) AND !EMPTY(EVENTOS_51.PROF_HEP_B) AND !EMPTY(EVENTOS_51.PROF_OTRAS))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'SI el "Mecanismo utilizado para la agresión" es Quemadura, debe indicar el sitio de la lesión'
			bIsValid = !INLIST(EVENTOS_51.ARMAS,'12','13','14') OR OCCURS('1',EVENTOS_51.QUE_CARA+EVENTOS_51.QUE_CUELLO+EVENTOS_51.QUE_MANO+EVENTOS_51.QUE_PIE+EVENTOS_51.QUE_PLIEGU+EVENTOS_51.QUE_GENITA+EVENTOS_51.QUE_TRONCO+EVENTOS_51.QUE_MIESUP+EVENTOS_51.QUE_MIEINF)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en los datos del agresor en el parentesco con la victima se ingreso la categoria 25=Ninguno, la variable "Agresor no familiar" debe estar diligenciada obligatoriamente'
			bIsValid = EVENTOS_51.R_FAM_VIC != '25' OR !EMPTY(EVENTOS_51.R_NOFILIAR)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


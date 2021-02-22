FUNCTION RecValidationRuleFor_eventos_15

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Las vacunas de Polio oral  y Antirotavirica, solo se pueden administrar vía oral y el sitio debe ser oral'
			bIsValid = !INLIST(EVENTOS_15.VACUNA4,'3','15') OR (EVENTOS_15.VIA4='1' AND EVENTOS_15.SITIO4='9')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Las vacunas de Polio oral  y Antirotavirica, solo se pueden administrar vía oral y el sitio debe ser oral'
			bIsValid = !INLIST(EVENTOS_15.VACUNA3,'3','15') OR (EVENTOS_15.VIA3='1' AND EVENTOS_15.SITIO3='9')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Las vacunas de Polio oral  y Antirotavirica, solo se pueden administrar vía oral y el sitio debe ser oral'
			bIsValid = !INLIST(EVENTOS_15.VACUNA2,'3','15') OR (EVENTOS_15.VIA2='1' AND EVENTOS_15.SITIO2='9')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'En los Datos Clínicos, debe haber por lo menos un 1=Sí'
			bIsValid = AT('1',EVENTOS_15.BECEGEITIS+EVENTOS_15.ABSCESO+EVENTOS_15.LINFADENIT+EVENTOS_15.FIEBRE+EVENTOS_15.CON_FEB+EVENTOS_15.CON_SINF+EVENTOS_15.HIPOTONÍA+EVENTOS_15.PARESTESIA+EVENTOS_15.PARÁLISIS+EVENTOS_15.ENCEFALOPA+EVENTOS_15.MENINGITIS+EVENTOS_15.URTICARIA+EVENTOS_15.ECZEMA+ EVENTOS_15.CHO_ANA+EVENTOS_15.GUI_BAR+EVENTOS_15.CELULITIS+EVENTOS_15.LLA_PER+EVENTOS_15.FATIGA+EVENTOS_15.DOLOR_CABE+EVENTOS_15.MIALGIA+EVENTOS_15.ARTRALGIA+EVENTOS_15.NAUSEAS) != 0
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Las vacunas de Polio oral  y Antirotavirica, solo se pueden administrar vía oral y el sitio debe ser oral'
			bIsValid = !INLIST(EVENTOS_15.VACUNA,'3','15') OR (EVENTOS_15.VIA='1' AND EVENTOS_15.SITIO='9')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


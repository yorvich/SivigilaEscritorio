FUNCTION RecValidationRuleFor_eventos_60

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() AND !IsInmediateNotification()  THEN
		IF bIsValid THEN
			sRuleViolated = 'Al menos un tipo de lesión debe ser 1'
			bIsValid = OCCURS('1',EVENTOS_60.LACERACION+EVENTOS_60.CONTUSION+EVENTOS_60.QUEMADURA+EVENTOS_60.AMPUTACION+EVENTOS_60.FRACTURAS+EVENTOS_60.DAÑ_OCU+EVENTOS_60.DAÑ_AUD+EVENTOS_60.VIA_AER+EVENTOS_60.ABDOMEN+EVENTOS_60.OTRO)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si quemadura es 1, debe seleccionar el sitio de la lesión'
			bIsValid = EVENTOS_60.QUEMADURA!='1' OR OCCURS('1',EVENTOS_60.QUE_CARA+EVENTOS_60.QUE_CUELLO+EVENTOS_60.QUE_MANO+EVENTOS_60.QUE_PIE+EVENTOS_60.QUE_PLIEGU+EVENTOS_60.QUE_GENITA+EVENTOS_60.QUE_TRONCO+EVENTOS_60.QUE_MIESUP+EVENTOS_60.QUE_MIEINF)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si amputación es 1, debe seleccionar el sitio de la lesión'
			bIsValid = EVENTOS_60.AMPUTACION!='1' OR OCCURS('1',EVENTOS_60.AMP_DEDMAN+EVENTOS_60.AMP_MANO+EVENTOS_60.AMP_ANTEBR+EVENTOS_60.AMP_BRAZO+EVENTOS_60.AMP_MUSLO+EVENTOS_60.AMP_PIERNA+EVENTOS_60.AMP_PIE+EVENTOS_60.AMP_DEDPIE)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si fractura es 1, debe seleccionar el sitio de la lesión'
			bIsValid = EVENTOS_60.FRACTURAS!='1' OR OCCURS('1',EVENTOS_60.FRA_CRANEO+EVENTOS_60.FRA_HUEMAN+EVENTOS_60.FRA_MIESUP+EVENTOS_60.FRA_REJA+EVENTOS_60.FRA_COLUMN+EVENTOS_60.FRA_CADERA+EVENTOS_60.FRA_MIEINF+EVENTOS_60.FRA_HUEPIE)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Debe diligenciar una de las siguientes:"Tipo de artefacto pirotécnico", "Actividad en que se presentó el evento - pólvora pirotécnica" o "Actividad en que se presentó el evento - artefactos explosivos, minas antipersonal y municiones sin explosionar"'
			bIsValid = !EMPTY(EVENTOS_60.ARTEFACTO + EVENTOS_60.ACT_POLVOR + EVENTOS_60.ACT_MINAS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Lesiones encontradas Otro" es 1, debe diligenciar cuál'
			bIsValid = EVENTOS_60.OTRO!='1' OR !EMPTY(EVENTOS_60.CUAL)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de artefacto pirotécnico" es 10, debe diligenciar cuál'
			bIsValid = EVENTOS_60.ARTEFACTO!='10' OR !EMPTY(EVENTOS_60.OTR_ART_PI)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Actividad en que se presentó el evento - pólvora pirotécnica" es 7, debe diligenciar cuál'
			bIsValid = EVENTOS_60.ACT_POLVOR!='7' OR !EMPTY(EVENTOS_60.OTR_AC_POL)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Actividad en que se presentó el evento - artefactos explosivos, minas antipersonal y municiones sin explosionar" es 5, debe diligenciar cuál'
			bIsValid = EVENTOS_60.ACT_MINAS!='5' OR !EMPTY(EVENTOS_60.OTR_AC_MIN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de artefacto que produjo la lesión" es 1, debe diligenciar "Hubo consumo de alcohol u otras sustancias psicoactivas previamente a la lesión por pólvora Lesionado"'
			bIsValid = EVENTOS_60.ARTEF_LESI!='1' OR !EMPTY(EVENTOS_60.CON_ALC_LE)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de artefacto que produjo la lesión" es 1, debe diligenciar "Actividad en que se presentó el evento - pólvora pirotécnica"'
			bIsValid = EVENTOS_60.ARTEF_LESI!='1' OR !EMPTY(EVENTOS_60.ACT_POLVOR)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


FUNCTION RecValidationRuleFor_eventos_07

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si embarazo = 1, es obligatorio digitar el número de semanas de embarazo al diagnóstico (De 0 a 45)'
			bIsValid = EVENTOS_07.EMBARAZO!='1' OR (!EMPTY(EVENTOS_07.SEM_EMB) AND VAL(EVENTOS_07.SEM_EMB)>=0 AND VAL(EVENTOS_07.SEM_EMB)<=45)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Ante todo caso de transmisión materno infantil es obligatorio diligenciar el nombre de la madre y su documento de identificación.'
			bIsValid = EVENTOS_07.MEC_PRO_T !='4' OR (!EMPTY(EVENTOS_07.NOM_MADRE) AND !EMPTY(EVENTOS_07.TIP_IDE_MA) AND !EMPTY(EVENTOS_07.NUM_IDE_MA))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Cuando el estado clínico es 2 = Sida o 3 = Muerto, al menos una de las variables de enfermedades asociadas debe ser 1 = Sí.'
			bIsValid = EVENTOS_07.EST_CLI = '1' OR (EVENTOS_07.CAN_ESO='1' OR EVENTOS_07.CAN_VA='1' OR EVENTOS_07.TUB_PUL='1' OR EVENTOS_07.CAN_CER='1' OR EVENTOS_07.TUB_EXP='1' OR EVENTOS_07.COCCIDIODO='1' OR EVENTOS_07.CITOMEGALO='1' OR EVENTOS_07.REN_CIT='1' OR EVENTOS_07.ENCEFALOPA='1' OR EVENTOS_07.OTRAS_MICR='1' OR EVENTOS_07.HIS_EXT='1' OR EVENTOS_07.ISO_CRO='1' OR EVENTOS_07.ERP_ZOS='1' OR EVENTOS_07.HIS_DIS='1' OR EVENTOS_07.LIN_BUR='1' OR EVENTOS_07.NEU_PNE='1' OR EVENTOS_07.NEU_REC='1' OR EVENTOS_07.LIN_INM='1' OR EVENTOS_07.CRI_CRO='1' OR EVENTOS_07.CRI_EXT='1' OR EVENTOS_07.SAR_KAP='1' OR EVENTOS_07.SIN_EMA='1' OR EVENTOS_07.LEU_MUL='1' OR EVENTOS_07.SEP_REC='1' OR EVENTOS_07.TOX_CER='1' OR EVENTOS_07.HEP_B='1' OR EVENTOS_07.HEP_C='1' OR EVENTOS_07.MENINGITIS='1' )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


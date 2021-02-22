FUNCTION RecValidationRuleFor_eventos_32

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La suma de pérdidas de vacuna antirrábica animal por diversas causas debe ser igual al total de pérdidas.'
			bIsValid = VAL(PER_VENC)+	VAL(PER_EXP_CA)+	VAL(PER_CONG)+	VAL(PER_RUPT)+	VAL(PER_HURTO)+	VAL(PER_POL)+	VAL(PER_ERR)+	VAL(PER_REAC)=val(Vac_per)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La suma de pérdidas de vacuna antirrábica humana por diversas causas debe ser igual al total de pérdidas.'
			bIsValid = VAL(PER_H_VEN)+	VAL(PER_H_EXP)+	VAL(PER_H_CON)+	VAL(PER_H_RUP)+	VAL(PER_H_HUR)+	VAL(PER_H_POL)+	VAL(PER_H_ERR)+	VAL(PER_H_REA)=VAL(Vah_per)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La suma de pérdidas de suero antirrábico humano por diversas causas debe ser igual al total de pérdidas.'
			bIsValid = VAL(PER_S_VEN)+	VAL(PER_S_EXP)+	VAL(PER_S_CON)+	VAL(PER_S_RUP)+	VAL(PER_S_HUR)+	VAL(PER_S_POL)+	VAL(PER_S_ERR)+	VAL(PER_S_REA)=VAL(Fsa_per)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La suma de resultado final observaciones de perros debe ser igual a la suma de perros observados.'
			bIsValid = VAL(PER_OB_DOC)+VAL(PER_OB_CLI)+VAL(PER_OB_CEN)=VAL(PER_CON_RA)+VAL(PER_SIN_RA)+VAL(PER_MUERTO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La suma de resultado final observaciones de gatos debe ser igual a la suma de gatos observados.'
			bIsValid = VAL(GAT_CON_RA)+VAL(GAT_SIN_RA)+VAL(GAT_MUERTO)=VAL(GAT_OB_DOC)+VAL(GAT_OB_CLI)+VAL(GAT_OB_CEN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La suma de dosis pos exposición en tratamientos concluidos, suspendidos y en curso debe ser igual al total de dosis pos exposición.'
			bIsValid = VAL(VAC_CONCLU)+	VAL(VAC_SUSP)+	VAL(VAC_CURSO)=VAL(VAC_AP_POS)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


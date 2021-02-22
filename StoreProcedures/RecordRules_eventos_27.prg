FUNCTION RecValidationRuleFor_eventos_27

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el caso es probable o "Confirmado por clínica" y es agudo, exige el diligenciamiento de micrométodo, gota gruesa, microhematocrito y Strout'
			bIsValid = fieldValue('TIP_CAS', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_27.AÑO + EVENTOS_27.SEMANA+EVENTOS_27.COD_EVE+EVENTOS_27.TIP_IDE+EVENTOS_27.NUM_IDE+EVENTOS_27.COD_PRE+EVENTOS_27.COD_SUB + "'", "AJUSTE DESC" ) = '5' OR (EVENTOS_27.CLAS_CASO!='1' OR (!EMPTY(EVENTOS_27.MICROMETOD) AND !EMPTY(EVENTOS_27.GOTA_GRUE) AND !EMPTY(EVENTOS_27.MICRO_HEM) AND !EMPTY(EVENTOS_27.STROUT)))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El tipo de caso "Confirmado por nexo" solo aplica para casos agudos'
			bIsValid = fieldValue('TIP_CAS', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_27.AÑO + EVENTOS_27.SEMANA+EVENTOS_27.COD_EVE+EVENTOS_27.TIP_IDE+EVENTOS_27.NUM_IDE+EVENTOS_27.COD_PRE+EVENTOS_27.COD_SUB + "'", "AJUSTE DESC" )!='5' OR EVENTOS_27.CLAS_CASO='1'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


FUNCTION RecValidationRuleFor_eventos_85

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La fecha de última menstruación debe ser inferior a la fecha de terminación del embarazo.'
			bIsValid = EMPTY(EVENTOS_85.FEC_TER_EM) OR EVENTOS_85.FEC_ULT_ME < EVENTOS_85.FEC_TER_EM
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de terminación del embarazo debe ser posterior a la fecha de primera ecografía'
			bIsValid = EMPTY(EVENTOS_85.FEC_TER_EM) OR EVENTOS_85.FEC_1_ECO < EVENTOS_85.FEC_TER_EM
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de última menstruación debe ser inferior a la fecha de primera ecografía'
			bIsValid = EMPTY(EVENTOS_85.FEC_1_ECO) OR EVENTOS_85.FEC_ULT_ME < EVENTOS_85.FEC_1_ECO
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el paciente tiene complicaciones neurológicas, las preguntas Fecha de inicio de sintomas, Tipo de complicación neurológica y Desplazamiento en los últimos 30 días, son de obligatorio diligenciamiento'
			bIsValid = EVENTOS_85.COMPL_NEUR!='1' OR (!EMPTY(EVENTOS_85.FEC_INI_SI) AND !EMPTY(EVENTOS_85.TIPO_COMPL) AND  !EMPTY(EVENTOS_85.DESPLAZAMI))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


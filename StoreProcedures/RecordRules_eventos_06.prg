FUNCTION RecValidationRuleFor_eventos_06

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La fecha de la última dosis de vacuna de sarampión no puede ser superior a la fecha de inicio de  erupción'
			bIsValid = EVENTOS_06.ult_sar <= EVENTOS_06.fini_eru
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de la última dosis de vacuna de Rubeola no puede ser superior a la fecha de inicio de  erupción'
			bIsValid = EVENTOS_06.ult_rub <= EVENTOS_06.fini_eru
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso es sospechoso para sindrome inflamatorio multisistemico, al menos debe tener un síntoma igual a 1'
			bIsValid = EVENTOS_06.SOSPE_MISC!='1' OR OCCURS('1',EVENTOS_06.VOMITO + EVENTOS_06.DIARREA + EVENTOS_06.EDEMA + EVENTOS_06.ALT_CONCIE)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso es sospechoso para sindrome inflamatorio multisistemico, la variabe "Vómito" es obligatoria'
			bIsValid = EVENTOS_06.SOSPE_MISC!='1' OR !EMPTY(EVENTOS_06.VOMITO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso es sospechoso para sindrome inflamatorio multisistemico, la variabe "Edema de mucosas" es obligatoria'
			bIsValid = EVENTOS_06.SOSPE_MISC!='1' OR !EMPTY(EVENTOS_06.EDEMA)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso es sospechoso para sindrome inflamatorio multisistemico, la variabe "Alteración del estado de conciencia" es obligatoria'
			bIsValid = EVENTOS_06.SOSPE_MISC!='1' OR !EMPTY(EVENTOS_06.ALT_CONCIE)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso es sospechoso para sindrome inflamatorio multisistemico, la variabe "Diarrea" es obligatoria'
			bIsValid = EVENTOS_06.SOSPE_MISC!='1' OR !EMPTY(EVENTOS_06.DIARREA)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


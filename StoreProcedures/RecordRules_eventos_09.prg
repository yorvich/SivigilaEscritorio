FUNCTION RecValidationRuleFor_eventos_09

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El número de muertos no puede ser menor al número de abortos'
			bIsValid = VAL(EVENTOS_09.muertos) >= VAL(EVENTOS_09.abortos)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad gestacional puede ser 20 o 21 semanas solo si el peso es superior o igual a 500 gr o, el peso puede ser inferior a 500 gr solo si la edad gestacional es superior o gual a 22 semanas'
			bIsValid = !INLIST(EVENTOS_09.MOM_OCU,'1','2') OR (VAL(EVENTOS_09.EDA_GES) >= 22 OR VAL(EVENTOS_09.PESO) >= 500)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el momento en que ocurrio la muerte corresponde a 3, 5, 6, 7, es obligatorio diligenciar "Edad neonatal en días"'
			bIsValid = !INLIST(EVENTOS_09.MOM_OCU,'3','5','6','7') OR !EMPTY(EVENTOS_09.EDAD_NEO)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


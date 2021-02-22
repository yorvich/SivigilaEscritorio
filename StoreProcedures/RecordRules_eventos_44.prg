FUNCTION RecValidationRuleFor_eventos_44

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si Asintomático es 2,  por lo menos una de las manifestaciones bioclínicas  desde aborto hasta alteraciones oculares debe estar en 1=Sí'
			bIsValid = ASINTOMATI!='2' OR (ABORTO = '1' OR 	MORTINATO = '1' OR 	PREMATURID = '1' OR 	ALTERACION = '1' OR 	LESIONCUTA = '1' OR 	RINISEROSA = '1' OR 	HEPATOESPL = '1' OR 	HIDROPESIA = '1' OR 	LESIONOSEA = '1' OR 	ALTERENAL = '1' OR 	ALTEHEMATO = '1' OR 	ALTEFUNHEP = '1' OR 	SORDERA = '1' OR 	ALTEOCULAR = '1' )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


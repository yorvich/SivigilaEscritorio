FUNCTION RecValidationRuleFor_Eventos_94

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La semana epidemiol�gica NO debe ser superior a la determinada por la fecha actual de grabaci�n'
			bIsValid = isValidEpidemiologicalWeek(EVENTOS_94.SEMANA,EVENTOS_94.A�O)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El municipio no es coherente con el c�digo de UPGD'
			bIsValid = LEFT(EVENTOS_94.COD_PRE,2)='11' OR (LEFT(EVENTOS_94.COD_PRE,5)=EVENTOS_94.COD_MUN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El municipio no es coherente con el c�digo de UPGD'
			bIsValid = LEFT(EVENTOS_94.COD_PRE,2)!='11' OR LEFT(EVENTOS_94.COD_MUN,2)='11'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


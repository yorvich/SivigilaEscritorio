FUNCTION RecValidationRuleFor_eventos_50

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si selecciona en la variable "Clasificación del caso según antecedente de tratamiento" la opción 2= previamente tratado, no aplica la opción 3 de la  variable "Clasificación del caso según tipo de medicamentos recibidos"'
			bIsValid = CLAS_ANT!='2' OR CLAS_MED!='3'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


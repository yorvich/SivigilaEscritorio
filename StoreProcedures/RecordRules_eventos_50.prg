FUNCTION RecValidationRuleFor_eventos_50

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si selecciona en la variable "Clasificaci�n del caso seg�n antecedente de tratamiento" la opci�n 2= previamente tratado, no aplica la opci�n 3 de la  variable "Clasificaci�n del caso seg�n tipo de medicamentos recibidos"'
			bIsValid = CLAS_ANT!='2' OR CLAS_MED!='3'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


FUNCTION RecValidationRuleFor_eventos_87

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Cuando el tipo de UCI es neonatal, es obligatorio diligenciar peso al nacer'
			bIsValid = EVENTOS_87.TIPO_UCI!='3' OR EVENTOS_87.VALOR_PESO!=0
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Cuando el tipo de UCI es neonatal, tipo de IAD solo puede ser 1=NAV ó 3= ITS AC'
			bIsValid = EVENTOS_87.TIPO_UCI != '3' OR (EVENTOS_87.TIPO_IAD = '1' OR EVENTOS_87.TIPO_IAD = '3')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de IAD" es NAV  debe diligenciarse ventilador mecánico'
			bIsValid = EVENTOS_87.TIPO_IAD!='1' OR (EVENTOS_87.VENTILADOR='1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de IAD" es ITS-AC debe diligenciarse catéter central'
			bIsValid = EVENTOS_87.TIPO_IAD!='3' OR (EVENTOS_87.CAT_CENTRA='1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de IAD" es ISTU-AC debe diligenciarse catéter urinario'
			bIsValid = EVENTOS_87.TIPO_IAD!='2' OR (EVENTOS_87.CAT_URINAR='1')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


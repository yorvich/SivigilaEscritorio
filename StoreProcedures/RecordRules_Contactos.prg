FUNCTION RecValidationRuleFor_Contactos

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Los datos registrados en tipo de documento de identidad, edad y unidad de medida no son consistentes'
			bIsValid = CONTACTOS.AJUSTE!='0' OR isValidIdentificationDoc(CONTACTOS.TIP_IDE, CONTACTOS.UNI_MED, CONTACTOS.EDAD)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de nacimiento, edad y unidad de medida deben ser consistentes.'
			bIsValid = IIF(CONTACTOS.AJUSTE = '0',isValidAge(CONTACTOS.FECHA_NTO, CONTACTOS.EDAD, CONTACTOS.UNI_MED),.T.)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha posible exposición debe ser igual o menor a la fecha de inicio de síntomas'
			bIsValid = EMPTY(CONTACTOS.FEC_INI_SI) OR (CONTACTOS.FEC_POS_EX <= CONTACTOS.FEC_INI_SI)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Los datos de identificación del contacto no pueden ser iguales a los del caso positivo'
			bIsValid = CONTACTOS.TIPIDE_POS != CONTACTOS.TIP_IDE OR CONTACTOS.NUMIDE_POS != CONTACTOS.NUM_IDE
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


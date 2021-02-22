FUNCTION RecValidationRuleFor_laboratorios

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el evento es 710 y la prueba es "Pruebas genotipicas", el valor debe ser uno de los siguientes genotipos: 1a, 1B, 1C, 1D, 1E, 1F, 1g,  2A, 2B, 2c'
			bIsValid = !(LABORATORIOS.COD_EVE='710' AND LABORATORIOS.Prueba='B5' AND INLIST(LABORATORIOS.RESULTADO,'1','6')) OR ',' + UPPER(ALLTRIM(LABORATORIOS.VALOR)) + ',' $ ',1A,1B,1C,1D,1E,1F,1G,2A,2B,2C,'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es 730 y la prueba es "Pruebas genotipicas", el valor debe ser uno de los siguientes genotipos: A, B1, B2, B3, C1, C2, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, E, F, G1, G2, G3, H1, H2'
			bIsValid = !(LABORATORIOS.COD_EVE='730' AND LABORATORIOS.Prueba='B5' AND INLIST(LABORATORIOS.RESULTADO,'1','6')) OR ',' + UPPER(ALLTRIM(LABORATORIOS.VALOR)) + ',' $ ',A,B1,B2,B3,C1,C2,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,E,F,G1,G2,G3,H1,H2,'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La prueba no es consistente con la muestra  para el Evento Chikungunya (Cod. 217)'
			bIsValid = LABORATORIOS.COD_EVE!='217' OR ((LABORATORIOS.PRUEBA!='30' OR LABORATORIOS.MUESTRA=='4 '))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El agente no es consistente con la prueba para el Evento hepatitis B, C y coinfección Hepatitis B y Delta (Cod. 340)'
			bIsValid = LABORATORIOS.COD_EVE!='340' OR (LABORATORIOS.AGENTE!='45' OR INLIST(LABORATORIOS.PRUEBA,'50','A4'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de la Toma del Examen es obligatoria excepto para el evento hepatitis B, C y coinfección Hepatitis B y Delta (Cod. 340)'
			bIsValid = LABORATORIOS.COD_EVE='340' OR !EMPTY(LABORATORIOS.FEC_EXA)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La prueba no es consistente con la muestra  para el Evento Chikungunya (Cod. 217)'
			bIsValid = LABORATORIOS.COD_EVE!='217' OR (!(LABORATORIOS.PRUEBA='2 ' OR LABORATORIOS.PRUEBA='3 ') OR LABORATORIOS.MUESTRA=='13')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el Agente es: Virus respiratorios 101 a 178 ETAs (código 99 de la tabla), el campo Valor sólo recibe los valores 2, 3 o 4'
			bIsValid = (LABORATORIOS.agente != '99' OR COD_EVE='357') OR BETWEEN(VAL(LABORATORIOS.VALOR),2,4)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de Recepción en el Laboratorio debe ser posterior o igual a la Fecha de toma del exámen'
			bIsValid = EMPTY(LABORATORIOS.FEC_REC) OR (LABORATORIOS.FEC_REC  >= LABORATORIOS.FEC_EXA)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El agente no es consistente con la prueba para el Evento hepatitis B, C y coinfección Hepatitis B y Delta (Cod. 340)'
			bIsValid = LABORATORIOS.COD_EVE!='340' OR (AGENTE!='12' OR INLIST(LABORATORIOS.PRUEBA,'26','30','50','51','93','A4'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de Expedición del resultado debe ser posterior o igual a la Fecha de Recepción en el Laboratorio'
			bIsValid = EMPTY(LABORATORIOS.FEC_EXP) OR (LABORATORIOS.FEC_EXP  >= LABORATORIOS.FEC_REC)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El agente no es consistente con la prueba para el Evento hepatitis B, C y coinfección Hepatitis B y Delta (Cod. 340)'
			bIsValid = LABORATORIOS.COD_EVE!='340' OR (AGENTE!='46' OR INLIST(LABORATORIOS.PRUEBA,'95','30','A4','B5','D0','H6'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento Tosferina (Cod. 800), si la muestra es sangre, el resultado debe ser estrictamente un número'
			bIsValid = LABORATORIOS.COD_EVE!='800' OR (LABORATORIOS.MUESTRA!='1' OR ISNUMERIC(LABORATORIOS.VALOR))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Endometritis puerperal (Cod. 351)'
			bIsValid = LABORATORIOS.COD_EVE!='351' OR (LABORATORIOS.MUESTRA!='1' OR LABORATORIOS.PRUEBA='92')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Endometritis puerperal (Cod. 351)'
			bIsValid = LABORATORIOS.COD_EVE!='351' OR (LABORATORIOS.MUESTRA!='4' OR LABORATORIOS.PRUEBA='55')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Endometritis puerperal (Cod. 351)'
			bIsValid = LABORATORIOS.COD_EVE!='351' OR (LABORATORIOS.MUESTRA!='32' OR LABORATORIOS.PRUEBA='55')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico (Cod. 352)'
			bIsValid = LABORATORIOS.COD_EVE!='352' OR (LABORATORIOS.MUESTRA!='1' OR LABORATORIOS.PRUEBA='92')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico (Cod. 352)'
			bIsValid = LABORATORIOS.COD_EVE!='352' OR (LABORATORIOS.MUESTRA!='4' OR LABORATORIOS.PRUEBA='55')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico (Cod. 352)'
			bIsValid = LABORATORIOS.COD_EVE!='352' OR (LABORATORIOS.MUESTRA!='32' OR LABORATORIOS.PRUEBA='55')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


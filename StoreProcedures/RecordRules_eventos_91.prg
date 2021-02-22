FUNCTION RecValidationRuleFor_eventos_91

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 2'
			bIsValid = VAL(EVENTOS_91.G2_ENFERMO)=VAL(EVENTOS_91.G2_VIVOS)+VAL(EVENTOS_91.G2_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 5'
			bIsValid = VAL(EVENTOS_91.G5_ENFERMO)=VAL(EVENTOS_91.G5_HOMBRES)+VAL(EVENTOS_91.G5_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 3'
			bIsValid = VAL(EVENTOS_91.G3_ENFERMO)=VAL(EVENTOS_91.G3_VIVOS)+VAL(EVENTOS_91.G3_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser mayor que uno (>1)'
			bIsValid = VAL(EVENTOS_91.G1_ENFERMO) + VAL(EVENTOS_91.G2_ENFERMO) + VAL(EVENTOS_91.G3_ENFERMO) + VAL(EVENTOS_91.G4_ENFERMO) + VAL(EVENTOS_91.G5_ENFERMO) + VAL(EVENTOS_91.G6_ENFERMO) + VAL(EVENTOS_91.G7_ENFERMO) > 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Modo de trasmisión - Otro diferente de agua y alimentol" es 1=sí,  los modos de transmisión Agua y Alimentos deben ser 2=No'
			bIsValid = EVENTOS_91.OTRO != '1' OR (EVENTOS_91.ALIMENTOS = '2' AND EVENTOS_91.AGUA = '2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 2'
			bIsValid = VAL(EVENTOS_91.G2_ENFERMO)=VAL(EVENTOS_91.G2_HOMBRES)+VAL(EVENTOS_91.G2_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 1'
			bIsValid = VAL(EVENTOS_91.G1_ENFERMO)=VAL(EVENTOS_91.G1_HOMBRES)+VAL(EVENTOS_91.G1_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 1'
			bIsValid = VAL(EVENTOS_91.G1_ENFERMO)=VAL(EVENTOS_91.G1_VIVOS)+VAL(EVENTOS_91.G1_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'SI "Modo de transmisión alimentos" es 1=sí, debe diligenciar Alimento 1'
			bIsValid = EVENTOS_91.ALIMENTOS != '1' OR !EMPTY(ALIMENTO1)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La semana epidemiológica NO debe ser superior a la determinada por la fecha actual de grabación'
			bIsValid = isValidEpidemiologicalWeek(EVENTOS_91.SEMANA,EVENTOS_91.AÑO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 7'
			bIsValid = VAL(EVENTOS_91.G7_ENFERMO)=VAL(EVENTOS_91.G7_VIVOS)+VAL(EVENTOS_91.G7_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 4'
			bIsValid = VAL(EVENTOS_91.G4_ENFERMO)=VAL(EVENTOS_91.G4_HOMBRES)+VAL(EVENTOS_91.G4_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de vivos y muertos debe ser mayor que uno (>1)'
			bIsValid = VAL(EVENTOS_91.G1_VIVOS) + VAL(EVENTOS_91.G2_VIVOS) + VAL(EVENTOS_91.G3_VIVOS) + VAL(EVENTOS_91.G4_VIVOS) + VAL(EVENTOS_91.G5_VIVOS) + VAL(EVENTOS_91.G6_VIVOS) + VAL(EVENTOS_91.G7_VIVOS) + VAL(EVENTOS_91.G1_MUERTOS) + VAL(EVENTOS_91.G2_MUERTOS) + VAL(EVENTOS_91.G3_MUERTOS) + VAL(EVENTOS_91.G4_MUERTOS) + VAL(EVENTOS_91.G5_MUERTOS) + VAL(EVENTOS_91.G6_MUERTOS) + VAL(EVENTOS_91.G7_MUERTOS) > 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 3'
			bIsValid = VAL(EVENTOS_91.G3_ENFERMO)=VAL(EVENTOS_91.G3_HOMBRES)+VAL(EVENTOS_91.G3_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 7'
			bIsValid = VAL(EVENTOS_91.G7_ENFERMO)=VAL(EVENTOS_91.G7_HOMBRES)+VAL(EVENTOS_91.G7_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de hombres y mujeres debe ser mayor que uno (>1)'
			bIsValid = VAL(EVENTOS_91.G1_HOMBRES) + VAL(EVENTOS_91.G2_HOMBRES) + VAL(EVENTOS_91.G3_HOMBRES) + VAL(EVENTOS_91.G4_HOMBRES) + VAL(EVENTOS_91.G5_HOMBRES) + VAL(EVENTOS_91.G6_HOMBRES) + VAL(EVENTOS_91.G7_HOMBRES) + VAL(EVENTOS_91.G1_MUJERES) + VAL(EVENTOS_91.G2_MUJERES) + VAL(EVENTOS_91.G3_MUJERES) + VAL(EVENTOS_91.G4_MUJERES) + VAL(EVENTOS_91.G5_MUJERES) + VAL(EVENTOS_91.G6_MUJERES) + VAL(EVENTOS_91.G7_MUJERES) > 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 6'
			bIsValid = VAL(EVENTOS_91.G6_ENFERMO)=VAL(EVENTOS_91.G6_VIVOS)+VAL(EVENTOS_91.G6_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 4'
			bIsValid = VAL(EVENTOS_91.G4_ENFERMO)=VAL(EVENTOS_91.G4_VIVOS)+VAL(EVENTOS_91.G4_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de hombres y mujeres debe ser igual al total de vivos y muertos'
			bIsValid = VAL(EVENTOS_91.G1_HOMBRES) + VAL(EVENTOS_91.G2_HOMBRES) + VAL(EVENTOS_91.G3_HOMBRES) + VAL(EVENTOS_91.G4_HOMBRES) + VAL(EVENTOS_91.G5_HOMBRES) + VAL(EVENTOS_91.G6_HOMBRES) + VAL(EVENTOS_91.G7_HOMBRES) + VAL(EVENTOS_91.G1_MUJERES) + VAL(EVENTOS_91.G2_MUJERES) + VAL(EVENTOS_91.G3_MUJERES) + VAL(EVENTOS_91.G4_MUJERES) + VAL(EVENTOS_91.G5_MUJERES) + VAL(EVENTOS_91.G6_MUJERES) + VAL(EVENTOS_91.G7_MUJERES) = VAL(EVENTOS_91.G1_VIVOS) + VAL(EVENTOS_91.G2_VIVOS) + VAL(EVENTOS_91.G3_VIVOS) + VAL(EVENTOS_91.G4_VIVOS) + VAL(EVENTOS_91.G5_VIVOS) + VAL(EVENTOS_91.G6_VIVOS) + VAL(EVENTOS_91.G7_VIVOS) + VAL(EVENTOS_91.G1_MUERTOS) + VAL(EVENTOS_91.G2_MUERTOS) + VAL(EVENTOS_91.G3_MUERTOS) + VAL(EVENTOS_91.G4_MUERTOS) + VAL(EVENTOS_91.G5_MUERTOS) + VAL(EVENTOS_91.G6_MUERTOS) + VAL(EVENTOS_91.G7_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 5'
			bIsValid = VAL(EVENTOS_91.G5_ENFERMO)=VAL(EVENTOS_91.G5_VIVOS)+VAL(EVENTOS_91.G5_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 6'
			bIsValid = VAL(EVENTOS_91.G6_ENFERMO)=VAL(EVENTOS_91.G6_HOMBRES)+VAL(EVENTOS_91.G6_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el brote está cerrado con identificacion del agente, debe haber algún agente identificado'
			bIsValid = EVENTOS_91.ESTADO_BRO!='2' OR !EMPTY(EVENTOS_91.AGENTE1 + EVENTOS_91.AGENTE2 + EVENTOS_91.AGENTE3 + EVENTOS_91.AGENTE4 + EVENTOS_91.AGENTE5)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


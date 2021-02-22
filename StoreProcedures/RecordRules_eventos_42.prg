FUNCTION RecValidationRuleFor_eventos_42

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La clasificaci�n final de dengue sin signos de alarma solo podr� tenerse cuando se hayan marcado del grupo de hallazgos semiol�gicos fiebre y dos o m�s adicionales'
			bIsValid = EVENTOS_42.CLASFINAL != '1' OR (EVENTOS_42.FIEBRE='1' AND (OCCURS('1', EVENTOS_42.CEFALEA + EVENTOS_42.DOLRRETROO + EVENTOS_42.MALGIAS + EVENTOS_42.ARTRALGIA + EVENTOS_42.ERUPCIONR) >= 2))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La conducta  0 (No aplica), solo es v�lida para el evento Mortalidad por dengue (Cod. 580)'
			bIsValid = EVENTOS_42.CONDUCTA != '0' OR EVENTOS_42.COD_EVE='580'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La clasificaci�n final de dengue con signos de alarma solo podr� tenerse cuando se hayan marcado fiebre y dos o m�s de los sintomas para dengue sin signos de alarma y por lo menos uno de los de dengue con signos de alarma'
			bIsValid = EVENTOS_42.CLASFINAL != '2' OR (EVENTOS_42.FIEBRE='1' AND (OCCURS('1', EVENTOS_42.CEFALEA + EVENTOS_42.DOLRRETROO + EVENTOS_42.MALGIAS + EVENTOS_42.ARTRALGIA + EVENTOS_42.ERUPCIONR) >= 2) AND OCCURS('1', EVENTOS_42.DOLOR_ABDO + EVENTOS_42.VOMITO + EVENTOS_42.DIARREA + EVENTOS_42.SOMNOLENCI + EVENTOS_42.HIPOTENSIO + EVENTOS_42.HEPATOMEG  + EVENTOS_42.HEM_MUCOSA + EVENTOS_42.HIPOTERMIA + EVENTOS_42.AUM_HEMATO +  EVENTOS_42.CAIDA_PLAQ + EVENTOS_42.ACUM_LIQUI) >= 1)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La clasificaci�n final de dengue grave solo podr� tenerse cuando se hayan marcado fiebre  y por lo menos uno de los s�ntomas clasificados para dengue grave'
			bIsValid = EVENTOS_42.CLASFINAL != '3' OR (EVENTOS_42.FIEBRE='1' AND (OCCURS('1', EVENTOS_42.EXTRAVASAC + EVENTOS_42.HEMORR_HEM + EVENTOS_42.DA�O_ORGAN + EVENTOS_42.CHOQUE) >= 1))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento muerte por dengue Cod. 580, debe seleccionarse en hallazgos semiol�gicos fiebre y al menos uno de los s�ntomas de dengue grave'
			bIsValid = EVENTOS_42.COD_EVE!='580' OR (EVENTOS_42.FIEBRE='1' AND OCCURS('1',EVENTOS_42.EXTRAVASAC + EVENTOS_42.HEMORR_HEM + EVENTOS_42.CHOQUE + EVENTOS_42.DA�O_ORGAN) >= 1)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La clasificaci�n final  0 (No aplica), solo es v�lida para el evento Mortalidad por dengue (Cod. 580)'
			bIsValid = EVENTOS_42.CLASFINAL != '0' OR EVENTOS_42.COD_EVE='580'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Cuando el evento es 210 y tiene al menos un s�ntoma de la secci�n con signos de alarma entonces la clasificaci�n final no puede ser sin signos de alarma'
			bIsValid = EVENTOS_42.COD_EVE!='210' OR (OCCURS('1',EVENTOS_42.DOLOR_ABDO + EVENTOS_42.VOMITO + EVENTOS_42.DIARREA + EVENTOS_42.SOMNOLENCI + EVENTOS_42.HIPOTENSIO + EVENTOS_42.HEPATOMEG  + EVENTOS_42.HEM_MUCOSA + EVENTOS_42.HIPOTERMIA + EVENTOS_42.AUM_HEMATO +  EVENTOS_42.CAIDA_PLAQ + EVENTOS_42.ACUM_LIQUI) < 1 OR (EVENTOS_42.CLASFINAL='2' OR EVENTOS_42.CLASFINAL='3'))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


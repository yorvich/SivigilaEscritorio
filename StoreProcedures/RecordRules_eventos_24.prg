FUNCTION RecValidationRuleFor_eventos_24

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el evento es Rabia en Perros y Gatos - Cod. 650, la clasificación incial del caso es obligatoria'
			bIsValid = EVENTOS_24.COD_EVE!='650' OR (EVENTOS_24.CLA_INI_CA='1' OR EVENTOS_24.CLA_INI_CA='2' )
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es Rabia en Perros y Gatos - Cod. 650, cuando el caso se clasifique como confirmado por laboratorio, las variables de  laboratorio deben ser diligenciadas en su totalidad'
			bIsValid = (EVENTOS_24.COD_EVE!='650' OR EVENTOS_24.CLA_INI_CA!='2') OR (!EMPTY(EVENTOS_24.F_TOM_MUE) AND !EMPTY(EVENTOS_24.F_TOM_REM) AND !EMPTY(EVENTOS_24.PR_DIA_CON) AND !EMPTY(EVENTOS_24.RESULTADO) AND (EVENTOS_24.RESULTADO!='1' OR (!EMPTY(EVENTOS_24.IDE_VV))))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es Rabia en Perros y Gatos - Cod. 650, cuando el caso se ajuste a confirmado por laboratorio, las variables de  laboratorio deben ser diligenciadas en su totalidad'
			bIsValid = (EVENTOS_24.COD_EVE!='650' OR EVENTOS_24.AJUSTE!='2') OR (!EMPTY(EVENTOS_24.F_TOM_MUE) AND !EMPTY(EVENTOS_24.F_TOM_REM) AND !EMPTY(EVENTOS_24.PR_DIA_CON) AND !EMPTY(EVENTOS_24.RESULTADO) AND (EVENTOS_24.RESULTADO!='1' OR (!EMPTY(EVENTOS_24.IDE_VV))))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


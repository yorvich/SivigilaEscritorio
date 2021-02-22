FUNCTION RecValidationRuleFor_eventos_37

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si clasificaci�n del caso es 3 = Confirmado por laboratorio,  se deben relacionar las fechas de las Pruebas: Cuadro Hem�tico,  Frotis de Sangre Perif�rica y Estudio de M�dula �sea'
			bIsValid = fieldValue('TIP_CAS', 'PACIENTE', "A�O+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_37.A�O + EVENTOS_37.SEMANA+EVENTOS_37.COD_EVE+EVENTOS_37.TIP_IDE+EVENTOS_37.NUM_IDE+EVENTOS_37.COD_PRE+EVENTOS_37.COD_SUB + "'", "AJUSTE DESC" ) != '3' OR (!EMPTY(EVENTOS_37.CH_FEC_TOM) AND !EMPTY(EVENTOS_37.CH_FEC_REC) AND !EMPTY(EVENTOS_37.CH_FEC_RES) AND !EMPTY(EVENTOS_37.FS_FEC_TOM) AND !EMPTY(EVENTOS_37.FS_FEC_REC) AND !EMPTY(EVENTOS_37.FS_FEC_RES) AND !EMPTY(EVENTOS_37.EM_FEC_TOM) AND !EMPTY(EVENTOS_37.EM_FEC_REC) AND !EMPTY(EVENTOS_37.EM_FEC_RES))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


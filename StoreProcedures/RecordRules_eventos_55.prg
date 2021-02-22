FUNCTION RecValidationRuleFor_eventos_55

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si la respuesta a Recaída fue 1=SI, es obligatorio diligencia la Fecha de diagnóstico'
			bIsValid = EVENTOS_55.RECAIDA!='1' OR !EMPTY(EVENTOS_55.FEC_DX)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en el "Método diagnostico otros tipos de cáncer" marcó la opción 8= Otro, debe indicar cuál otro'
			bIsValid = EVENTOS_55.DX_OTRO_CA!='8' OR !EMPTY(EVENTOS_55.OTRO_CUAL)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en el "Método diagnostico otros tipos de cáncer" marcó una de las opciones 1 a 5, la Fecha de toma es obligatoria'
			bIsValid = !(INLIST(EVENTOS_55.DX_OTRO_CA,'1','2','3','4','5')) OR !EMPTY(EVENTOS_55.FEC_TOM)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en el "Método diagnostico otros tipos de cáncer" marcó una de las opciones 1 a 5, la Fecha de resultado es obligatoria'
			bIsValid = !(INLIST(EVENTOS_55.DX_OTRO_CA,'1','2','3','4','5')) OR !EMPTY(EVENTOS_55.FEC_RES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de toma de Aspirado de médula ósea es obligatoria para casos con clasificación inicial confirmado por laboratorio'
			bIsValid = fieldValue('TIP_CAS', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_55.AÑO + EVENTOS_55.SEMANA+EVENTOS_55.COD_EVE+EVENTOS_55.TIP_IDE+EVENTOS_55.NUM_IDE+EVENTOS_55.COD_PRE+EVENTOS_55.COD_SUB + "'", "AJUSTE DESC" )!='3'  OR !INLIST(EVENTOS_55.TIPO_CA,'1','2','3') OR !EMPTY(EVENTOS_55.FEC_AS_MO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'EL Valor registrado % de blastos de Estudio de médula ósea es obligatorio para casos con clasificación inicial confirmado por laboratorio'
			bIsValid = fieldValue('TIP_CAS', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_55.AÑO + EVENTOS_55.SEMANA+EVENTOS_55.COD_EVE+EVENTOS_55.TIP_IDE+EVENTOS_55.NUM_IDE+EVENTOS_55.COD_PRE+EVENTOS_55.COD_SUB + "'", "AJUSTE DESC" )!='3' OR !INLIST(EVENTOS_55.TIPO_CA,'1','2','3') OR !EMPTY(EVENTOS_55.MO_BLASTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condición final es muerto en datos básicos, es obligatorio diligenciar la Causa directa de muerte'
			bIsValid = fieldValue('CON_FIN', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_55.AÑO + EVENTOS_55.SEMANA+EVENTOS_55.COD_EVE+EVENTOS_55.TIP_IDE+EVENTOS_55.NUM_IDE+EVENTOS_55.COD_PRE+EVENTOS_55.COD_SUB + "'", "AJUSTE DESC" )!='2' OR !EMPTY(EVENTOS_55.CAUSA_DIRE)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de resultado de Aspirado de médula ósea es obligatoria para casos con clasificación inicial confirmado por laboratorio'
			bIsValid = fieldValue('TIP_CAS', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_55.AÑO + EVENTOS_55.SEMANA+EVENTOS_55.COD_EVE+EVENTOS_55.TIP_IDE+EVENTOS_55.NUM_IDE+EVENTOS_55.COD_PRE+EVENTOS_55.COD_SUB + "'", "AJUSTE DESC" )!='3'  OR !INLIST(EVENTOS_55.TIPO_CA,'1','2','3') OR !EMPTY(EVENTOS_55.FEC_RES_MO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condición final es muerto en datos básicos, es obligatorio diligenciar "Causa de muerte determinada por"'
			bIsValid = fieldValue('CON_FIN', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_55.AÑO + EVENTOS_55.SEMANA+EVENTOS_55.COD_EVE+EVENTOS_55.TIP_IDE+EVENTOS_55.NUM_IDE+EVENTOS_55.COD_PRE+EVENTOS_55.COD_SUB + "'", "AJUSTE DESC" )!='2' OR !EMPTY(EVENTOS_55.CAUS_MUE_D)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condición final es muerto en datos básicos, es obligatorio diligenciar "Causa antecedentes"'
			bIsValid = fieldValue('CON_FIN', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_55.AÑO + EVENTOS_55.SEMANA+EVENTOS_55.COD_EVE+EVENTOS_55.TIP_IDE+EVENTOS_55.NUM_IDE+EVENTOS_55.COD_PRE+EVENTOS_55.COD_SUB + "'", "AJUSTE DESC" )!='2' OR !EMPTY(EVENTOS_55.CAUSA_ANTE)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condición final es muerto en datos básicos, es obligatorio diligenciar Sitio de defunción'
			bIsValid = fieldValue('CON_FIN', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_55.AÑO + EVENTOS_55.SEMANA+EVENTOS_55.COD_EVE+EVENTOS_55.TIP_IDE+EVENTOS_55.NUM_IDE+EVENTOS_55.COD_PRE+EVENTOS_55.COD_SUB + "'", "AJUSTE DESC" )!='2' OR !EMPTY(EVENTOS_55.SIT_DEF)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la respuesta a "Remitido a otra Institución" fue la opción 1=SI, es obligatorio diligenciar "Fecha de remisión","Departamento y Municipio al que se remite" y "Razón social de la UPGD a la que se remite"'
			bIsValid = EVENTOS_55.REM_INST!='1' OR (!EMPTY(EVENTOS_55.FEC_REM) AND !EMPTY(EVENTOS_55.COD_MUN_RE) AND !EMPTY(EVENTOS_55.UPGD_REM))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de inicio de tratamiento debe ser mayor o igual que la fecha de consulta.'
			bIsValid = EMPTY(FEC_INI_TR) OR FEC_INI_TR>=fieldValue('FEC_CON', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_55.AÑO + EVENTOS_55.SEMANA+EVENTOS_55.COD_EVE+EVENTOS_55.TIP_IDE+EVENTOS_55.NUM_IDE+EVENTOS_55.COD_PRE+EVENTOS_55.COD_SUB +"'", "AJUSTE DESC" )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


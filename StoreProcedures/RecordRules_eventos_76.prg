FUNCTION RecValidationRuleFor_eventos_76

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si Profilaxis antibiótica quirúrgica es 1, debe marcarse "Tiempo entre finalización de profilaxis e incisión quirúrgica o parto"'
			bIsValid = EVENTOS_76.PRF_ANTIB!='1' OR !EMPTY(EVENTOS_76.TIEM_ENTRE)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el procedimiento médico es 1 = Cesárea 2 = Herniorrafia o 5 = Colecistectomía, solo aplica la clasificación de la infección superficial primaria ó profunda primaria u órgano espacio'
			bIsValid = !INLIST(EVENTOS_76.PROCEDIMIE,'1','2','5') OR (EVENTOS_76.SUP_SECUND!='1' AND EVENTOS_76.PROF_SECUN!='1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si procedimiento médico es 1= césarea, solo aplica la clasificación de la infección superficial primaria, profunda primaria u órgano espacio'
			bIsValid = EVENTOS_76.PROCEDIMIE!='1' OR (EVENTOS_76.SUP_SECUND!='1' AND EVENTOS_76.PROF_SECUN!='1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Solo una de las variables del bloque de tipo de infección de sitio quirurgico debe estar marcado como 1'
			bIsValid = OCCURS('1', EVENTOS_76.SUP_PRIMAR + EVENTOS_76.PROF_PRIMA + EVENTOS_76.ORG_ESPACI + EVENTOS_76.SUP_SECUND + EVENTOS_76.PROF_SECUN) = 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si  el procedimiento médico quirúrgico realizado es 2 = Herniorrafia, Ógano/espacio afectado debe ser 3'
			bIsValid = !(EVENTOS_76.PROCEDIMIE = '2' AND EVENTOS_76.ORG_ESPACI = '1' ) OR VAL(EVENTOS_76.ORG_ESP_AF)=3
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si  el procedimiento médico quirúrgico realizado es  3 = Parto vaginal, Ógano/espacio afectado debe ser 1'
			bIsValid = !(EVENTOS_76.PROCEDIMIE = '3' AND EVENTOS_76.ORG_ESPACI = '1') OR VAL(EVENTOS_76.ORG_ESP_AF)=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si  el procedimiento médico quirúrgico realizado es   4 = Revascularización miocárdica con incisión torácica y del sitio donante, Ógano/espacio afectado debe ser 3 o 6 o 7 o 8 o 9 o 10 u 11'
			bIsValid = !(EVENTOS_76.PROCEDIMIE = '4'  AND EVENTOS_76.ORG_ESPACI = '1') OR (BETWEEN(VAL(EVENTOS_76.ORG_ESP_AF),6,11) OR VAL(EVENTOS_76.ORG_ESP_AF)=3)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si  el procedimiento médico quirúrgico realizado es    5 = Colecistectomía, Ógano/espacio afectado debe ser 2 o 3'
			bIsValid = !(EVENTOS_76.PROCEDIMIE = '5'  AND EVENTOS_76.ORG_ESPACI = '1') OR BETWEEN(VAL(EVENTOS_76.ORG_ESP_AF),2,3)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Detección de la infección" es 4, debe diligenciar "Nombre de la Institución donde se realizo el procedimiento quirúrgico"'
			bIsValid = EVENTOS_76.DETEC_INFE!='4' OR !EMPTY(EVENTOS_76.INST_PROCE)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si  el procedimiento médico quirúrgico realizado es 1=Cesárea o 3=Parto vaginal, debe diligenciar "Tiempo de duración del trabajo de parto" y "Tiempo de ruptura de membranas".'
			bIsValid = !((EVENTOS_76.PROCEDIMIE='1' OR EVENTOS_76.PROCEDIMIE='3') AND EVENTOS_76.DETEC_INFE!='4') OR (!EMPTY(EVENTOS_76.TIEMPO_PAR) AND !EMPTY(EVENTOS_76.TIEMPO_RUP))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si  el procedimiento médico quirúrgico realizado es 1=Cesárea, Ógano/espacio afectado debe ser 1 o 5'
			bIsValid = !(EVENTOS_76.PROCEDIMIE = '1' AND EVENTOS_76.ORG_ESPACI = '1') OR BETWEEN(VAL(EVENTOS_76.ORG_ESP_AF),1,5)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


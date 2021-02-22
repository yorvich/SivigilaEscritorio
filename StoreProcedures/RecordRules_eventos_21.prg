FUNCTION RecValidationRuleFor_eventos_21

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el caso se notifica como IRAG inusitado, por lo menos una de las variables del bloque "por qué se notifica el caso como IRAG inusitado (excluyendo Tos y Fiebre)", debería estar marcada en la opción 1=sí'
			bIsValid = EVENTOS_21.COD_EVE!='348' OR OCCURS('1', EVENTOS_21.TRAB_SALUD + EVENTOS_21.DETER_CLIN + EVENTOS_21.ASOC_BROTE + EVENTOS_21.viajó + EVENTOS_21.con_con + EVENTOS_21.CON_EST) >= 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Debe diligenciar: ¿Usó antivirales en la última semana? y la sección Si hubo complicaciones, ¿Cualés se presentaron?'
			bIsValid = (EVENTOS_21.COD_EVE!='348') OR (!EMPTY(EVENTOS_21.USO_ANTIV) AND !EMPTY(EVENTOS_21.DER_PLE) AND !EMPTY(EVENTOS_21.DER_PER) AND !EMPTY(EVENTOS_21.MIOCARDITI)AND !EMPTY(EVENTOS_21.SEPTICEMIA)AND !EMPTY(EVENTOS_21.FALLA_RESP)AND !EMPTY(EVENTOS_21.OTROS))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si viajó es 1, debe especificar el lugar, bien sea nacional o internacional'
			bIsValid = EVENTOS_21.VIAJÓ != '1' OR (!EMPTY(EVENTOS_21.MUNICIPIO) OR !EMPTY(EVENTOS_21.INTERNAL) OR !EMPTY(EVENTOS_21.CODPAIS_PR))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Datos clínicos Tuberculosis"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.TUBERCULOS,'1','2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Odinofagia"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.ODINOFAGIA,'1','2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Adinamia"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.TUBERCULOS,'1','2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso se notifica como IRAG inusitado, por lo menos una de las variables del bloque "por qué se notifica el caso como IRAG inusitado (excluyendo Tos y Fiebre)", debería estar marcada en la opción 1=sí'
			bIsValid = EVENTOS_21.COD_EVE!='348' OR OCCURS('1', EVENTOS_21.TRAB_SALUD + EVENTOS_21.DETER_CLIN + EVENTOS_21.ASOC_BROTE + EVENTOS_21.viajó + EVENTOS_21.con_con + EVENTOS_21.CON_EST) >= 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Dificultad Respiratoria"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.DIF_RES,'1','2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Rinorrea"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.RINORREA,'1','2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Conjuntivitis"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.CONJUNTIVI,'1','2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Cefalea"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.CEFALEA,'1','2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Diarrea"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.DIARREA,'1','2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Hipertensión"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.HIPERTENSI,'1','2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Pérdida del gusto o del olfato"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.PERD_GUSTO,'1','2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se trata del evento 346, debe marcar una opción en la variable "Síntomas - Otros"'
			bIsValid = EVENTOS_21.COD_EVE!='346' OR INLIST(EVENTOS_21.OTROS_SINT,'1','2')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


FUNCTION RecValidationRuleFor_Eventos_89

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si marcó 3 en "clasificación" del bloque tuberculosis farmacorresistente, debe diligenciar en 2=Resistente, al menos dos de los medicamentos Estreptomicina, Isoniazida, Etambutol y Pirazinamica'
			bIsValid = EVENTOS_89.TIPO_RESIS!='3' OR OCCURS('2',EVENTOS_89.ESTREPTOMI + EVENTOS_89.ISONIAZIDA + EVENTOS_89.ETAMBUTOL + EVENTOS_89.PIRAZINAMI)>=2
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si marcó 8 en "clasificación" del bloque tuberculosis farmacorresistente y si en Quinolonas marco resistente, entonces en inyectables debe marcar  sensible y viceversa'
			bIsValid = EVENTOS_89.TIPO_RESIS!='8' OR (EVENTOS_89.QUINOLAS != '2' OR EVENTOS_89.INYECTABLE = '1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condición es tuberculosis farmacorresistente y la clasificación de caso según antecedente es “Previamente tratado” , "Clasificación del caso según ingreso" solo admite 6 o 7'
			bIsValid = !(EVENTOS_89.COND_TUBER = '2' and EVENTOS_89.CLAS_ANT = '2') OR INLIST(EVENTOS_89.CLASCASO,'6','7')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si marcó 4 en "clasificación" del bloque tuberculosis farmacorresistente, debe haber  marcado 2=Resistente en Quinolonas e inyectables'
			bIsValid = EVENTOS_89.TIPO_RESIS!='4' OR OCCURS('2',EVENTOS_89.QUINOLAS + EVENTOS_89.INYECTABLE)=2
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si marcó 1 en "clasificación" del bloque tuberculosis farmacorresistente, debe haber una y sola una variable marcada como 2=Resistente entre Estreptomicina, Isoniazida, Etambutol, Pirazinamica'
			bIsValid = EVENTOS_89.TIPO_RESIS!='1' OR OCCURS('2',EVENTOS_89.ESTREPTOMI + EVENTOS_89.ISONIAZIDA + EVENTOS_89.ETAMBUTOL + EVENTOS_89.PIRAZINAMI)=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si marcó 2 en "clasificación" del bloque tuberculosis farmacorresistente,  debe haber  marcado 2=Resistente en Isoniazida y Rifampicina'
			bIsValid = EVENTOS_89.TIPO_RESIS!='2' OR OCCURS('2',EVENTOS_89.ISONIAZIDA + EVENTOS_89.RIFAMPI)=2
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condición es sensible y la clasificación de caso según antecedente es“Previamente tratado”, "Clasificación del caso según ingreso" solo admite 2,3,4 o 5'
			bIsValid = !(EVENTOS_89.COND_TUBER = '1' AND EVENTOS_89.CLAS_ANT = '2') OR INLIST(EVENTOS_89.CLASCASO,'2','3','4','5')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si marcó 8 en "clasificación" del bloque tuberculosis farmacorresistente y si en Quinolonas marco sensible, entonces en inyectables debe marcar  resistente  y viceversa'
			bIsValid = EVENTOS_89.TIPO_RESIS!='8' OR (EVENTOS_89.QUINOLAS != '1' OR EVENTOS_89.INYECTABLE = '2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si marcó 7 en "clasificación" del bloque tuberculosis farmacorresistente, entonces debe diligenciar en 2=Resistente el medicamento Rifampicina'
			bIsValid = EVENTOS_89.TIPO_RESIS!='7' OR EVENTOS_89.RIFAMPI='2'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Se debe tener al menos un 1=Sí en una de las variables Cuadro clínico, Nexo epidemiológico o Radiológico'
			bIsValid = OCCURS('1',EVENTOS_89.CUADRO_CLI+EVENTOS_89.NEX_EPI+EVENTOS_89.RADIOLOGIC)>0
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condición es tuberculosis farmacorresistente, la fecha de confirmación es obligatoria'
			bIsValid = EVENTOS_89.COND_TUBER!='2' OR !EMPTY(EVENTOS_89.FEC_CONF)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condición es tuberculosis farmacorresistente, es obligatorio diligenciar la variable clasificación del bloque tuberculosis farmacorresistente'
			bIsValid = EVENTOS_89.COND_TUBER!='2' OR !EMPTY(EVENTOS_89.TIPO_RESIS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condición es tuberculosis farmacorresistente, el resultado del cultivo o el resultado de la prueba molecular deben ser 1'
			bIsValid = EVENTOS_89.COND_TUBER != '2' OR (EVENTOS_89.RESCULTIVO='1' OR EVENTOS_89.RES_PR_MOL='1')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


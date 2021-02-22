FUNCTION RecValidationRuleFor_eventos_40

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El paciente debe tener fiebre y 2 o m�s datos cl�nicos en 1=S�'
			bIsValid = (EVENTOS_40.FIEBRE='1' AND (OCCURS('1',EVENTOS_40.ESCALOFRIO + EVENTOS_40.CEFALEA + EVENTOS_40.DOLOR_RETR + EVENTOS_40.VOMITO + EVENTOS_40.DIARREA + EVENTOS_40.DABDOMINAL + EVENTOS_40.ICTERICIA + EVENTOS_40.MIALGIAS + EVENTOS_40.ARTRALGIAS + EVENTOS_40.HEPATOMEGA + EVENTOS_40.ESPLENOMEG + EVENTOS_40.ADENOPATIA + EVENTOS_40.DISNEA + EVENTOS_40.DESHIDRATA + EVENTOS_40.SIG_MENING + EVENTOS_40.EXANTEMA + EVENTOS_40.EDEMA + EVENTOS_40.CONVULSION + EVENTOS_40.HEMORRAGIA + EVENTOS_40.DOLOR_PANT + EVENTOS_40.SUDOR_NOCT + EVENTOS_40.EPIDIDIMIT + EVENTOS_40.ORQUITIS + EVENTOS_40.OLIGURIA) >= 2))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el paciente tiene m�s de 30 d�as de fiebre, solo puede ingresar si tiene orquitis, epididimitis, esplenomegalia o hepatomegalia (alguno de ellos) y, adem�s, debe tener "Consumo de leche y sus derivados sin pasteurizar" o "Manipulaci�n de secreciones�'
			bIsValid = DIAS_FIEBR<=30 OR (EVENTOS_40.EPIDIDIMIT = '1' OR EVENTOS_40.ORQUITIS = '1' OR EVENTOS_40.HEPATOMEGA = '1' OR EVENTOS_40.ESPLENOMEG = '1') AND (EVENTOS_40.CONS_LECHE = '1' OR EVENTOS_40.MANIP_SECR = '1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en la variable animales en casa se seleccion� S�, por lo menos uno de los animales perros, gatos, etc. debe ser  1=S�'
			bIsValid = (EVENTOS_40.ANIM_EN_CA!='1' OR (OCCURS('1',EVENTOS_40.PERROS + EVENTOS_40.GATOS + EVENTOS_40.BOVINOS + EVENTOS_40.EQUINOS + EVENTOS_40.CERDOS + EVENTOS_40.SILVESTRES + EVENTOS_40.OTR_ANIMAL) >= 1))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


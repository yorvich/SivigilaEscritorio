FUNCTION RecValidationRuleFor_Paciente

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() AND !IsInmediateNotification()  THEN
		IF bIsValid THEN
			sRuleViolated = 'Mortalidad Perinatal y Neonatal Tard�a (560), La fecha de defunci�n no debe ser mayor a la Fecha de Notificaci�n'
			bIsValid = PACIENTE.COD_EVE!='560' OR (PACIENTE.FEC_DEF<=PACIENTE.FEC_NOT)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada  de la madre debe estar en el rango de 10 a 54 a�os o ser 60 a�os'
			bIsValid = PACIENTE.COD_EVE!='560'  OR (IsValidAgeForEvent('560',PACIENTE.EDAD,PACIENTE.UNI_MED) OR (PACIENTE.EDAD='60' AND PACIENTE.UNI_MED='1'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento S�filis gestacional (Cod. 750), es obligatorio diligenciar Semanas de gestaci�n'
			bIsValid = (PACIENTE.COD_EVE!='750')  OR !EMPTY(PACIENTE.SEM_GES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La semana epidemiol�gica NO debe ser superior a la determinada por la fecha actual de grabaci�n'
			bIsValid = isValidEpidemiologicalWeek(PACIENTE.SEMANA,PACIENTE.A�O)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la paciente pertenece al grupo poblacional gestante, solo se admiten edades entre 10 y 64 a�os'
			bIsValid = PACIENTE.GP_GESTAN != '1' OR (PACIENTE.UNI_MED='1' AND BETWEEN(VAL(PACIENTE.EDAD),10,64))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad l�mite establecida para el evento seg�n su definici�n en el documento de Codificaci�n y reglas de validaci�n de contenido para el ingreso de  los eventos de VSP de inter�s nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='549'  OR IsValidAgeForEvent('549',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Los datos registrados en tipo de documento de identidad, edad y unidad de medida no son consistentes'
			bIsValid = PACIENTE.AJUSTE!='0' OR isValidIdentificationDoc(PACIENTE.TIP_IDE, PACIENTE.UNI_MED, PACIENTE.EDAD)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ha diligenciado la causa b�sica de muerte, la condici�n final del paciente debe ser Muerto'
			bIsValid = EMPTY(PACIENTE.CBMTE) OR PACIENTE.CON_FIN='2'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Es obligatorio diligenciar la variable Hospitalizado'
			bIsValid = (PACIENTE.COD_EVE='110' OR PACIENTE.COD_EVE='000' OR PACIENTE.COD_EVE='855' OR PACIENTE.COD_EVE='339' OR PACIENTE.COD_EVE='344' OR PACIENTE.COD_EVE='207' OR PACIENTE.COD_EVE='745' OR PACIENTE.COD_EVE='896' OR PACIENTE.COD_EVE='467') OR  !EMPTY(PACIENTE.PAC_HOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento Exposici�n a fl�or, la edad solo admite valores 6, 12, 15 y 18'
			bIsValid = PACIENTE.COD_EVE!='228' OR INLIST(PACIENTE.EDAD,'6','12','15','18')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ingresa un registro nuevo, La semana de ocurrencia NO puede ser superior a la semana de notificaci�n'
			bIsValid = VAL(PACIENTE.ajuste) != 0  OR isLessThanOrEqual(VAL(PACIENTE.semana),VAL(PACIENTE.a�o),VAL(getSimpleEpidemiologicalYear(PACIENTE.FEC_NOT)),VAL(getSimpleEpidemiologicalYear(PACIENTE.FEC_NOT)))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si no es notificaci�n negativa, la Fecha de Nacimiento NO puede ser mayor a la fecha de Notificaci�n'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000') OR EMPTY(PACIENTE.FECHA_NTO) OR EMPTY(PACIENTE.FEC_NOT) OR (PACIENTE.FECHA_NTO <= PACIENTE.FEC_NOT)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si no es notificaci�n negativa, La Fecha de Nacimiento NO puede ser mayor a la fecha de Consulta'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000') OR EMPTY(PACIENTE.FECHA_NTO) OR EMPTY(PACIENTE.FEC_CON) OR (PACIENTE.FECHA_NTO <= PACIENTE.FEC_CON)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si no es notificaci�n negativa, La Fecha de Nacimiento NO puede ser mayor a la fecha de Inicio de Sintomas'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000') OR EMPTY(PACIENTE.FECHA_NTO) OR EMPTY(PACIENTE.INI_SIN) OR (PACIENTE.FECHA_NTO <= PACIENTE.INI_SIN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El a�o de la fecha de consulta  debe ser posterior al a�o 1999'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000','342') OR EMPTY(PACIENTE.FEC_CON) OR (YEAR(PACIENTE.FEC_CON) > 1999)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento NO corresponde a: 340 HEPATITIS B, 550 MORTALIDAD MATERNA, 560 MORTALIDAD PERINATAL, 850 VIH/SIDA/MORTALIDAD POR SIDA, 342 ENFERMEDADES HUERFANAS - RARAS, entonces el a�o de inicio de s�ntomas debe ser posterior al a�o 2000'
			bIsValid = INLIST(PACIENTE.COD_EVE, '000', '550 ','551 ', '560 ', '850 ', '340 ', '342 ') OR EMPTY(PACIENTE.INI_SIN) OR YEAR(PACIENTE.INI_SIN) > 2000
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si no es notificaci�n negativa y se est� haciendo un ajuste, la fecha del Ajuste no debe ser menor a la de Notificaci�n'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000') OR VAL(PACIENTE.AJUSTE) = 0 OR EMPTY(PACIENTE.FEC_AJU) OR EMPTY(FEC_NOT) OR ((PACIENTE.FEC_AJU >=PACIENTE.FEC_NOT))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada del paciente NO se encuentra dentro del rango permitido para la notificaci�n del evento'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000') OR belongsToAgeGroup(PACIENTE.UNI_MED, PACIENTE.EDAD, PACIENTE.COD_EVE)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la UPGD corresponde a fuerzas militares, los campos Fuerza, Unidad y Grado deben estar diligenciados'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000') OR NOT(upgdIsMilitaryForces()) OR (!EMPTY(PACIENTE.FM_FUERZA) AND (VAL(PACIENTE.FM_UNIDAD) != 0) AND !EMPTY(PACIENTE.FM_GRADO))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de Inicio de Sintomas no debe ser mayor a la de Fecha de Consulta'
			bIsValid = PACIENTE.COD_EVE = '000' OR ALLTRIM(PACIENTE.COD_EVE) $ '357,351,352' OR EMPTY(PACIENTE.FEC_CON) OR (PACIENTE.INI_SIN <= PACIENTE.FEC_CON)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de consulta no debe ser mayor a la de Notificaci�n'
			bIsValid = PACIENTE.COD_EVE = '000' OR (PACIENTE.FEC_CON <= PACIENTE.FEC_NOT)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de Hospitalizaci�n no debe ser inferior a la de Consulta'
			bIsValid = PACIENTE.COD_EVE = '000' OR ALLTRIM(PACIENTE.COD_EVE) $ '357,351,352' OR EMPTY(PACIENTE.FEC_HOS) OR EMPTY(PACIENTE.FEC_CON) OR (PACIENTE.FEC_HOS >= PACIENTE.FEC_CON)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento Defectos cong�nitos (Cod. 215) ,  no aplican los grupos poblacionales Carcelarios ni Gestantes'
			bIsValid = (PACIENTE.COD_EVE!='215') OR (PACIENTE.GP_GESTAN !='1' AND PACIENTE.GP_CARCELA!='1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada es inferior a la edad l�mite establecida para el evento seg�n su definici�n en el documento de Codificaci�n y reglas de validaci�n de contenido para el ingreso de  los eventos de VSP de inter�s nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='340'  OR IsValidAgeForEvent2('340',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad l�mite establecida para el evento seg�n su definici�n en el documento de Codificaci�n y reglas de validaci�n de contenido para el ingreso de  los eventos de VSP de inter�s nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='550'  OR IsValidAgeForEvent('550',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se marca SEXO masculino NO se puede tener Grupo poblacional GESTANTES'
			bIsValid = SEXO!='M' OR GP_GESTAN !='1'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El �nico evento que admite edad cero es defectos cong�nitos (Cod. 215)'
			bIsValid = PACIENTE.COD_EVE='000' OR (PACIENTE.EDAD!='0' OR PACIENTE.COD_EVE='215')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad l�mite establecida para el evento seg�n su definici�n en el documento de Codificaci�n y reglas de validaci�n de contenido para el ingreso de  los eventos de VSP de inter�s nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='459'  OR IsValidAgeForEvent('459',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para los eventos Difteria (Cod. 230) y Desnutrici�n aguda en menores de cinco a�os (Cod. 113) es obligatoria la fecha de nacimiento'
			bIsValid = !(PACIENTE.COD_EVE='230' OR PACIENTE.COD_EVE='113') OR !EMPTY(PACIENTE.FECHA_NTO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad l�mite establecida para el evento seg�n su definici�n en el documento de Codificaci�n y reglas de validaci�n de contenido para el ingreso de  los eventos de VSP de inter�s nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='115'  OR IsValidAgeForEvent('115',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento C�ncer en menores de 18 a�os (Cod. 115), solo se  puede tener clasificaci�n confirmado por cl�nica cuando el caso se reporte como mortalidad'
			bIsValid = PACIENTE.COD_EVE!='115' OR (PACIENTE.CON_FIN='2' OR PACIENTE.TIP_CAS!='4')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada es inferior a la edad l�mite establecida para el evento seg�n su definici�n en el documento de Codificaci�n y reglas de validaci�n de contenido para el ingreso de  los eventos de VSP de inter�s nacional en el aplicativo Sivigila'
			bIsValid = !INLIST(PACIENTE.COD_EVE,'855','339','344','207','745','896','467','351','110')  OR IsValidAgeForEvent2(PACIENTE.COD_EVE,PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad l�mite establecida para el evento seg�n su definici�n en el documento de Codificaci�n y reglas de validaci�n de contenido para el ingreso de  los eventos de VSP de inter�s nacional en el aplicativo Sivigila'
			bIsValid = !INLIST(PACIENTE.COD_EVE,'855','339','344','207','745','896','467','351')  OR IsValidAgeForEvent(PACIENTE.COD_EVE,PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es Defectos cong�nitos (Cod. 215), cuando la edad es diferente de 0, entonces condici�n final debe ser diferente de 0'
			bIsValid = PACIENTE.COD_EVE!='215' OR (PACIENTE.EDAD='0' OR PACIENTE.CON_FIN!='0')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condici�n final del paciente es Muerto, debe suministrar la fecha de defunci�n'
			bIsValid = PACIENTE.CON_FIN!='2' OR !EMPTY(PACIENTE.FEC_DEF)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de hospitalizaci�n no puede ser inferior a la fecha de inicio de s�ntomas excepto cuando se trata de uno de los eventos con Cod. 357,351,352'
			bIsValid = !(!(ALLTRIM(PACIENTE.COD_EVE) $ '357,351,352') AND !EMPTY(PACIENTE.FEC_HOS)) OR  (PACIENTE.FEC_HOS >= PACIENTE.INI_SIN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de Inicio de Sintomas no debe ser mayor a la de Fecha de Consulta'
			bIsValid = !(INLIST(PACIENTE.COD_EVE,'351','352') AND PACIENTE.PAC_HOS='2') OR (PACIENTE.INI_SIN <= PACIENTE.FEC_CON)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad l�mite establecida para el evento seg�n su definici�n en el documento de Codificaci�n y reglas de validaci�n de contenido para el ingreso de  los eventos de VSP de inter�s nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='551'  OR IsValidAgeForEvent('551',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fuente de la vigilancia es obligatoria'
			bIsValid = (!(INLIST(PACIENTE.AJUSTE,'0','7') OR EMPTY(PACIENTE.AJUSTE)) OR !EMPTY(PACIENTE.FUENTE))  OR COD_EVE='000'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es Malaria (Cod. 465), cuando la condici�n final es muerto, el tipo de caso debe ser  confirmado por nexo o por laboratorio'
			bIsValid = PACIENTE.COD_EVE!="465" OR ( PACIENTE.CON_FIN!='2' OR (PACIENTE.TIP_CAS='3' OR PACIENTE.TIP_CAS='5'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para cualquier opci�n distinta a "No afiliado" o "Indeterminado/Pendiente" en "Tipo de R�gimen en Salud", es obligatorio diligenciar el  c�digo de administradora'
			bIsValid = PACIENTE.COD_EVE='000' OR (INLIST(PACIENTE.TIP_SS,'N','I')  OR !EMPTY(PACIENTE.COD_ASE))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El sexo debe ser M o F. Solamente cuando el evento es Defectos cong�nitos (Cod. 215) o S�filis Congenita (Cod. 740) se admite sexo = I (Indeterminado)'
			bIsValid = PACIENTE.COD_EVE='000' OR (PACIENTE.COD_EVE='215' OR PACIENTE.COD_EVE='740' OR (PACIENTE.sexo='M' OR  PACIENTE.sexo='F'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El paciente debe pertenecer a alg�n grupo poblacional'
			bIsValid = INLIST(PACIENTE.COD_EVE,'000','855','339','344','207','745','896','467') OR (PACIENTE.GP_DISCAPA='1' OR PACIENTE.GP_DESPLAZ='1' OR PACIENTE.GP_MIGRANT='1' OR PACIENTE.GP_CARCELA='1' OR PACIENTE.GP_GESTAN ='1' OR PACIENTE.GP_INDIGEN='1' OR PACIENTE.GP_POBICFB='1' OR PACIENTE.GP_MAD_COM='1' OR PACIENTE.GP_DESMOVI='1' OR PACIENTE.GP_PSIQUIA='1' OR PACIENTE.GP_VIC_VIO='1' OR PACIENTE.GP_OTROS  ='1' )
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento Morbilidad materna extrema (Cod. 549), es obligatorio diligenciar Semanas de gestaci�n si se trata de una gestante'
			bIsValid = !(PACIENTE.COD_EVE='549' and PACIENTE.GP_GESTAN='1')  OR !EMPTY(PACIENTE.SEM_GES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El a�o de la fecha de consulta  debe ser posterior al a�o 1999'
			bIsValid = !(COD_EVE='342' AND EMPTY(FECHA_NTO)) OR YEAR(FEC_CON)>1999
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Nacionalidad es obligatoria'
			bIsValid = (!(INLIST(PACIENTE.AJUSTE,'0','7') OR EMPTY(PACIENTE.AJUSTE)) OR !EMPTY(PACIENTE.NACIONALID))  OR COD_EVE='000'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El tipo de documento PE solo aplica para pacientes de nacionalidad extranjera'
			bIsValid = PACIENTE.TIP_IDE!='PE' OR PACIENTE.NACIONALID!='170'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si area de ocurrencia es 3 = Rural Disperso, debe diligenciar como m�nimo el nombre de la vereda'
			bIsValid = PACIENTE.AREA!='3' OR !EMPTY(PACIENTE.VEREDA)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El tipo de documento CC solo aplica para pacientes de nacionalidad colombiana'
			bIsValid = PACIENTE.TIP_IDE!='CC' OR PACIENTE.NACIONALID='170'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es IRA por virus nuevo (Cod. 346), la clasificaci�n o ajuste "Confirmado por nexo" solo aplica si el paciente es hospitalizado o fallecido.'
			bIsValid = !(PACIENTE.COD_EVE='346' AND (PACIENTE.TIP_CAS='5' OR PACIENTE.AJUSTE='5')) OR (PACIENTE.CON_FIN='2' OR PACIENTE.PAC_HOS='1')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


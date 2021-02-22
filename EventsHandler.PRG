#INCLUDE SIVIGILA.H

FUNCTION getEpidemiologicalWeek as Byte

	LPARAMETERS dBaseDate as Date, sEpidemiologicalWeek as String, sEpidemiologicalYear as String
	
	*Busca en la tabla de CALENDARIO epidemiológico del Sistema, la semana y año epidemiológicos (sEpidemiologicalWeek , sEpidemiologicalYear);
	correspondientes a la fecha dBaseDate.
	
	*Retorna un de los siguientes valores:
	
	*Valor retornado	encontrada	cambiada	
	*	0					0			0		dBaseDate NO se encuentra en la tabla CALENDARIO
	
	*	1					0			1		dBaseDate es vacío; en este caso se hará (sEpidemiologicalWeek, sEpidemiologicalYear)=('0','')
	
	*	10					1			0		dBaseDate SÍ se encuentra en la tabla CALENDARIO y (sEpidemiologicalWeek, sEpidemiologicalYear) es ;
												idéntico a los valores CALENDARIO.DESDE, CALENDARIO.HASTA que se encuentran en la tabla CALENDARIO
												
	*	11					1			1		dBaseDate SÍ se encuentra en la tabla CALENDARIO y (sEpidemiologicalWeek, sEpidemiologicalYear) no es ;
												idéntico a los valores CALENDARIO.DESDE, CALENDARIO.HASTA que se encuentran en la tabla CALENDARIO. en ;
												este caso se hará (sEpidemiologicalWeek, sEpidemiologicalYear)=(CALENDARIO.DESDE, CALENDARIO.HASTA)


	LOCAL bReturnedValue as Boolean 

	IF !EMPTY(dBaseDate) THEN
		SELECT CALENDARIO.SEMANA, CALENDARIO.VIGENCIA FROM CALENDARIO WHERE ;
				BETWEEN(dBaseDate,CALENDARIO.DESDE, CALENDARIO.HASTA) INTO ARRAY aEpidemiologicalWeeks
		IF _TALLY > 0 THEN
			bReturnedValue = 10
			IF sEpidemiologicalWeek != ALLTRIM(STR(aEpidemiologicalWeeks(1,1))) THEN
				sEpidemiologicalWeek = ALLTRIM(STR(aEpidemiologicalWeeks(1,1)))
				bReturnedValue = 11
			ENDIF
			IF sEpidemiologicalYear != aEpidemiologicalWeeks(1,2) THEN
				sEpidemiologicalYear = aEpidemiologicalWeeks(1,2)
				bReturnedValue = 11
			ENDIF
		ELSE
			bReturnedValue = 0
		ENDIF
	ELSE
		sEpidemiologicalWeek = '0'
		sEpidemiologicalYear = ''
		bReturnedValue = 1
	ENDIF
	RETURN bReturnedValue
ENDFUNC


FUNCTION getMinEpidemiologicalDate as Date

	SELECT MIN(CALENDARIO.DESDE) FROM CALENDARIO INTO ARRAY aMinDate
	RETURN aMinDate(1,1)

ENDFUNC


FUNCTION getMinEpidemiologicalYear as Integer

	SELECT MIN(CALENDARIO.VIGENCIA) FROM CALENDARIO INTO ARRAY aMinYear
	RETURN VAL(aMinYear(1,1))

ENDFUNC


FUNCTION checkEpidemiologicalWeek AS Boolean

	LPARAMETERS eventCode as String, seed as String

	*Determina para el evento eventCode si debe tenerse en cuenta la fecha indicada por seed para establecer la semana epidemiológica según la;
	caracterización del evento en la tabla EVENTOS

	
	SELECT UPPER(EVENTOS.VAL_SEM) FROM EVENTOS WHERE EVENTOS.COD_EVE = '&eventCode' INTO ARRAY aEpidemiologicalWeekSeed
	RETURN (UPPER(seed) $ ALLTRIM(aEpidemiologicalWeekSeed(1,1)))
ENDFUNC

FUNCTION checkEpidemiologicalWeek_V01(eventCode as String, seed as String) as Boolean

	*Determina para el evento eventCode si debe tenerse en cuenta la fecha indicada por seed para establecer la semana epidemiológica según la;
	caracterización del evento en la tabla EVENTOS

	DIMENSION aWeekExpr(1)
	DIMENSION aEpidemiologicalWeekSeed(1)
SET STEP ON 	
	SELECT UPPER(EVENTOS.VAL_SEM) FROM EVENTOS WHERE EVENTOS.COD_EVE = '&eventCode' INTO ARRAY aWeekExpr
	nCandidatesWeeks = ALINES(aEpidemiologicalWeekSeed, aWeekExpr[1], 1+2+4+8,',')
	IF nCandidatesWeeks = 1 THEN
		RETURN (',' + UPPER(seed) + ',' $ ',' + ALLTRIM(aWeekExpr[1]) + ',') 
	ELSE
		bContinue = .T.
		bReturnedVal = .F.
		DO WHILE  nCandidateWeek <= nCandidatesWeeks AND bContinue
			sWeekToEvaluate = 'm.' + aEpidemiologicalWeekSeed[nCandidateWeek]
			IF !EMPTY(EVALUATE(sWeekToEvaluate)) THEN 
				bReturnedVal = (UPPER(seed) $ ALLTRIM(aEpidemiologicalWeekSeed[nCandidateWeek]))
				IF bReturnedVal THEN 
					bContinue = .F.
				ELSE
					nCandidateWeek = nCandidateWeek + 1
				ENDIF 
			ENDIF 
		ENDDO 
		RETURN bReturnedVal 
	ENDIF
ENDFUNC


FUNCTION existEpidemiologicalWeek AS Boolean

	LPARAMETERS yearSearched as String, weekSearched as byte

	*Retornar TRUE si {yearSearched , weekSearched } se encuentran en la tabla del CALENDARIO epidemiológico del Sistema Sivigila

	SELECT CALENDARIO.SEMANA, CALENDARIO.VIGENCIA FROM CALENDARIO WHERE CALENDARIO.VIGENCIA='&yearSearched' AND CALENDARIO.SEMANA=(weekSearched) INTO ARRAY aEpidemiologicalWeeks
	RETURN	(_TALLY>0)
	
ENDFUNC


FUNCTION getEpidemiologicalWeekDeterminants as Byte

	LPARAMETERS sEventCode as String, sWeekDeterminantFieldName as String, sTableName as String
	
	*Busca en la tabla de EVENTOS del Sistema, los nombres del campo sWeekDeterminantFieldName y de la tabla sTableName;
	a la que pertenece, a partir de los cuales se determina la semana y año epidemiológicos de ocurrencia del evento sEventCode.
	
	*En caso de que el evento sea '000' (Notificación negativa) o no exista en la tabla de EVENTOS, retorna false.
	
	LOCAL bReturnedValue as Boolean 
	
	bReturnedValue = .F.
	sWeekDeterminantFieldName = ''
	sTableName = ''
	IF !EMPTY(sEventCode) THEN
		SELECT DeterminantesSemanaPorEvento.TableName, DeterminantesSemanaPorEvento.FieldName FROM ;
			DeterminantesSemanaPorEvento WHERE DeterminantesSemanaPorEvento.EventId='&sEventCode' ;
			INTO ARRAY aEpidemiologicalDeterminants
		IF _TALLY > 0 THEN
			sWeekDeterminantFieldName = aEpidemiologicalDeterminants(1,2)
			sTableName = aEpidemiologicalDeterminants(1,1)
			bReturnedValue = .T.
		ENDIF
	ENDIF
	RETURN bReturnedValue
ENDFUNC

*----------------------------------------------------------------------------------------------------------------------------
FUNCTION AdjustmentIsAllowed(dNotificationDate as Date, dDeterminantDate as Date, sNotificationMsg as String, sEventCode as String)
	*Entró en desuso el 13/3/2020.;
	Consultar en su defecto NotificationIsAllowed
	
	* Valida si el ajuste ingresado está por fuera de tiempo;
		Base de datos cerrada a partir del 31 de Marzo del año en curso aproximadamente para eventos;
		del año inmediatamente anterior.
		
	bReturnedValue = .T.

	SET PROCEDURE TO Utilities ADDITIVE 

	IF sEventCode $ sAdjustmentDBClosureValidationEvents 
		* código de evento con ajuste validado -hasta 31 Marzo para eventos del año anterior-
		
		* La fecha de notificación es la que divide los intervalos
		IF dNotificationDate =< CTOD(DB_CLOSURE_DAY + '/' + DB_CLOSURE_MONTH + '/' + ALLTRIM(STR(YEAR(dNotificationDate))))
			* LAS FECHAS DE OCURRENCIA DEL EVENTO SE ADMITEN DESDE EL 1 DE ENERO DEL AÑO ANTERIOR HASTA EL 31 DE MARZO DEL PRESENTE AÑO
			dMinDate = CTOD('01/01/'+ALLTRIM(STR(YEAR(dNotificationDate) - 1)))
		ELSE
			* LAS FECHAS DE OCURRENCIA DEL EVENTO SE ADMITEN DESDE EL 1 DE ENERO DEL AÑO EN CURSO HASTA LA FECHA ACTUAL
			dMinDate =  CTOD('01/01/'+ALLTRIM(STR(YEAR(dNotificationDate))))
		ENDIF

		IF dDeterminantDate < dMinDate
			sNotificationMsg = 'Ha expirado la fecha de notificación de ajustes para Años Epidemiológicos anteriores'+CHR(13)+ CHR(10)+;
							'Fecha límite: ' + DB_CLOSURE_DAY + ' de ' +  MonthName(VAL(DB_CLOSURE_MONTH)) + ' del año siguiente a la ocurrencia del evento. Desea corregir el dato ?'
			bReturnedValue = .F.
		ENDIF
	ENDIF
	
	RETURN bReturnedValue

ENDFUNC

*----------------------------------------------------------------------------------------------------------------------------
FUNCTION NotificationIsAllowed(dNotificationDate as Date, dDeterminantDate as Date, sNotificationMsg as String, sEventCode as String, ;
								bDoNotGetYesOrNoAnswer AS Boolean)
				

	* Valida si un registro de notificación ingresado correspondiente al evento sEventCode está por fuera de tiempo. ;
	La Base de datos de un año cualquiera es cerrada a partir del 29 de Marzo del año en curso para eventos	del año inmediatamente anterior.

	bReturnedValue = .T.

	SET PROCEDURE TO Utilities ADDITIVE 
	
	IF !(sEventCode $ S_EXCLUDED_DB_CLOSURE_EVENTS)
		* El evento NO está exceptuado del cierre de base de datos

		* La fecha de notificación es la que divide los intervalos
		IF dNotificationDate =< CTOD(DB_CLOSURE_DAY + '/' + DB_CLOSURE_MONTH + '/' + ALLTRIM(STR(YEAR(dNotificationDate)))) 
			*Las fechas de ocurrencia del evento se admiten desde cinco días antes del 1 de enero del año anterior hasta aproximadamente ;
			el 27 de marzo del presente año
			dMinDate =  CTOD('01/01/'+ALLTRIM(STR(YEAR(dNotificationDate) - 1)))
		ELSE
			* LAS FECHAS DE OCURRENCIA DEL EVENTO SE ADMITEN DESDE EL 1 DE ENERO DEL AÑO EN CURSO HASTA LA FECHA ACTUAL
			dMinDate =  CTOD('01/01/'+ALLTRIM(STR(YEAR(dNotificationDate)))) - 5
		ENDIF

		IF dDeterminantDate < dMinDate
			sNotificationMsg = 'Ha expirado la fecha de notificación para eventos de Años Epidemiológicos anteriores'+CHR(13)+ CHR(10)+;
							'Fecha límite: ' + DB_CLOSURE_DAY + ' de ' +  MonthName(VAL(DB_CLOSURE_MONTH)) + ' del año siguiente a la ocurrencia del evento.'
			IF !bDoNotGetYesOrNoAnswer THEN
				sNotificationMsg = sNotificationMsg + ' Desea corregir el dato ?'
			ENDIF
			bReturnedValue = .F.
		ENDIF
	ENDIF
		
	RETURN bReturnedValue

ENDFUNC

*----------------------------------------------------------------------------------------------------------------------------
PROCEDURE CustomizeMe

	LPARAMETERS oContainer as Object, sEventCode as String, bRestoreDefaultValues, sFieldName AS String, sFieldValue as String 
	

	&&#DEFINE sConfigurationTableName "ConfiguracionDeObjetos"
	
	DIMENSION aControlsState(1)
*SET STEP ON 
	IF bRestoreDefaultValues THEN
		*Restaura los valores por defecto de las propiedades de los controles según se encuentran definidas en;
		la tabla sConfigurationTableName
		sSelectSqlCmd = "SELECT * FROM " + sConfigurationTableName + " WHERE UPPER(CONTENEDOR) = " + ;
						"UPPER('" + oContainer.name + "')" + ;
						" AND Default AND Acti_ctrl INTO CURSOR rsDefaultConfiguration "
		&sSelectSqlCmd
		
		DO getBoundedControlsIndex WITH oContainer , aControlsState, "rsDefaultConfiguration" IN SivigilaUtilities
		SELECT rsDefaultConfiguration
		SCAN
			nControlRow=ASCAN(aControlsState,rsDefaultConfiguration.NOMB_CTRL,-1,-1,2,15) 
			IF nControlRow > 0 THEN
				nControlIndex=aControlsState(nControlRow,1)
				setPropertyValue = "oContainer.Controls(nControlIndex)." + ALLTRIM(rsDefaultConfiguration.NOMB_PROP) + "="
				DO CASE
					CASE rsDefaultConfiguration.TIPO_VALR = 'C'
						setPropertyValue = setPropertyValue + "'" + rsDefaultConfiguration.VALR_PROP + "'"
					CASE rsDefaultConfiguration.TIPO_VALR = 'D'
						setPropertyValue = setPropertyValue + "CTOD(" + rsDefaultConfiguration.VALR_PROP + ")"
					OTHERWISE
						setPropertyValue = setPropertyValue + rsDefaultConfiguration.VALR_PROP
				ENDCASE
				
				TRY
					&setPropertyValue
				CATCH TO oException				
				ENDTRY
			ENDIF
		ENDSCAN
	ENDIF

	
	*/ Se seleccionan los controles que se va a configurar en función de sEventCode
	sSelectSqlCmd = "SELECT * FROM " + sConfigurationTableName + " WHERE UPPER(CONTENEDOR) == UPPER('" + oContainer.name + "')" + ;
					" AND COD_EVE == '" + ALLTRIM(sEventCode) + "' AND acti_ctrl "
	IF VARTYPE(sFieldName)!='L' THEN
		sSelectSqlCmd = sSelectSqlCmd +	" AND NOMB_CAMPO='" + sFieldName + "'"
		IF VARTYPE(sFieldValue)!='L' THEN
			sSelectSqlCmd = sSelectSqlCmd +	" AND VALR_CAMPO='" + sFieldValue + "'"
		ENDIF
	ENDIF
	sSelectSqlCmd = sSelectSqlCmd +	" INTO CURSOR rsConfiguration "
	&sSelectSqlCmd
	
	IF _TALLY != 0 THEN
	
		*Establece los valores de las propiedades de los controles según se encuentran definidas en;
		la tabla sConfigurationTableName
		DIMENSION aControlsState(1)
		DO getBoundedControlsIndex WITH oContainer , aControlsState, "rsConfiguration" IN SivigilaUtilities
		SELECT rsConfiguration
		SCAN 
			nControlRow=ASCAN(aControlsState,rsConfiguration.NOMB_CTRL,-1,-1,2,15) 
			IF nControlRow > 0 THEN
				nControlIndex=aControlsState(nControlRow,1)
				setPropertyValue = "oContainer.Controls(nControlIndex)." + ALLTRIM(rsConfiguration.NOMB_PROP) + "="
				DO CASE
					CASE rsConfiguration.TIPO_VALR = 'C'
						setPropertyValue = setPropertyValue + "'" + rsConfiguration.VALR_PROP + "'"
					CASE rsConfiguration.TIPO_VALR = 'D'
						setPropertyValue = setPropertyValue + "CTOD(" + rsConfiguration.VALR_PROP + ")"
					OTHERWISE
						setPropertyValue = setPropertyValue + rsConfiguration.VALR_PROP
				ENDCASE
				
				TRY
					&setPropertyValue
				CATCH TO oException				
				ENDTRY
			ENDIF
		ENDSCAN
	ENDIF				
ENDPROC


FUNCTION GetWarehouseTableName

	LPARAMETERS sEventCode as String
	
	DIMENSION aEventTableName(1)
	
	SELECT 'EVENTOS_' + PADL(ALLTRIM(FORMULARIO),2,'0')	FROM EVENTOS WHERE COD_EVE='&sEventCode' INTO ARRAY aEventTableName
	RETURN aEventTableName(1)
ENDFUNC


FUNCTION getEventName

	LPARAMETERS sEventCode as String
	
	DIMENSION aEventName(1)
	
	SELECT ALLTRIM(NOM_EVE) AS EventName FROM EVENTOS WHERE COD_EVE='&sEventCode' INTO ARRAY aEventName
	RETURN IIF(_TALLY > 0,aEventName(1),'')
ENDFUNC


FUNCTION isIndividual

	LPARAMETERS sEventCode as String
	
	DIMENSION aEventTableName(1)
	
	SELECT TIP_NOT FROM EVENTOS WHERE COD_EVE='&sEventCode' INTO ARRAY aEventKind
	IF _TALLY>0 THEN
		RETURN  aEventKind(1)= 1
	ELSE
		RETURN .F.
	ENDIF
	
ENDFUNC

FUNCTION isCollective

	LPARAMETERS sEventCode as String
	
	DIMENSION aEventTableName(1)
	
	SELECT TIP_NOT FROM EVENTOS WHERE COD_EVE='&sEventCode' INTO ARRAY aEventKind
	IF _TALLY>0 THEN
		RETURN  aEventKind(1)= 3
	ELSE
		RETURN .F.
	ENDIF
	
ENDFUNC


FUNCTION GetEventStatus

	LPARAMETERS EventCode, EventTableCode
	* Retorna 1 si el evento EventCode es colectivo según su definición en Tables.EVENTOS.Fields.Tip_not ;
	en caso contrario, retorna 0.
	*Si EventCode se encuentra en la tabla EVENTOS, retorna en EventTableCode el número de la tabla o formulario ;
	en donde se guarda o captura los datos complementarios del evento

	ReturnedValue = -1
	
	EventTableCode = 0
	IF EMPTY(EventCode)
		ReturnedValue = 0
	ELSE
	
		SqlClause = "SELECT tip_not, formulario, dc_tabla FROM Eventos WHERE cod_eve = '" + EventCode + "'" +;
					" into array a_Result "
		&SqlClause
		IF _TALLY > 0
			ReturnedValue = IIF(a_Result(1) != 3, 0, 1)
			IF !EMPTY(ALLTRIM(a_Result(2))) THEN
				EventTableCode = INT(VAL(a_Result(2)))
			ELSE
				IF !EMPTY(ALLTRIM(a_Result(3))) THEN
					EventTableCode = INT(VAL(SUBSTR(ALLTRIM(a_Result(3)),LEN(ALLTRIM(a_Result(3)))-1,2)))
				ENDIF
			ENDIF
		ENDIF
	ENDIF
	
	RETURN ReturnedValue
 
ENDFUNC

PROCEDURE GetEventStatusTst(sEventCode as String)
	
	LOCAL nEventTableCode as Number 
*SET STEP ON 	
	?GetEventStatus(sEventCode,@nEventTableCode) 
	?nEventTableCode
	
ENDPROC 


FUNCTION GetEventAllowedClassification

	LPARAMETERS EventCode
	* Retorna una cadena de caracteres separada por comas con las clasificaciones permitidas para el evento EventCode
	* según se encuentren definidas en tables.eventos.Fields.cla_per

	LOCAL nOldWorkArea as Number
	
	nOldWorkArea = SELECT()
	
	sReturnedValue = ""
	bTableIsUsed = .F.
	
	IF USED('EVENTOS')
		bTableisUsed = .T.
	ENDIF
	
	IF !EMPTY(EventCode)
	
		SqlClause = "SELECT cla_per FROM Eventos WHERE cod_eve = '" + EventCode + "'" +;
					" into array aClaPer "
		&SqlClause
		IF _TALLY > 0
			FOR iCounter = 1 TO LEN(aClaPer)
				IF !EMPTY(SUBSTR(aClaPer,iCounter,1)) THEN 	
					sReturnedValue = sReturnedValue + "," + SUBSTR(aClaPer,iCounter,1)
				ENDIF
			NEXT
		ENDIF
	ENDIF	
			
	IF !bTableisUsed	
		SELECT EVENTOS
		USE
	ENDIF
	
	SELECT (nOldWorkArea)
	
	RETURN sReturnedValue
 
ENDFUNC


FUNCTION IMCClasification
	
	LPARAMETERS nWeek as Byte, nIMC as Number 
	
	*Retorna la Clasificación IMC/semana gestación basado en el cálculo con peso, talla y semanas de gestación según se define en ;
	Tables.IMCG 
	
	*Los valores retornados son 1 = Enflaquecida 2 = Normal 3 = Sobrepeso 4 = Obesidad

	sReturnedValue="NA"
	sSQLCmd = "SELECT * FROM IMCG WHERE SEMG=" + ALLTRIM(STR(nWeek)) + " INTO ARRAY aLimits"
	&sSQLCmd
	IF _TALLY > 0 THEN
		IF nIMC <= aLimits(2) THEN
			RETURN '1'
		ENDIF
		IF nIMC <= aLimits(3) THEN
			RETURN '2'
		ENDIF
		IF nIMC <= aLimits(4) THEN
			RETURN '3'
		ELSE
			RETURN '4'
		ENDIF
	ENDIF
	RETURN sReturnedValue
ENDFUNC


FUNCTION isInmediateCustomizable

	LPARAMETERS sEventCode as String
	
	*Retorna True si el evento sEventCode tiene una configuración especial para Notificación inmediata según se encuentra definido en ;
	tables.Eventos.Fields.Admite_NI

	DIMENSION aEventTableName(1)
	
	SELECT ADMITE_NI FROM EVENTOS WHERE COD_EVE='&sEventCode' INTO ARRAY aEventKind
	IF _TALLY>0 THEN
		RETURN  aEventKind(1)
	ELSE
		RETURN .F.
	ENDIF
	
ENDFUNC


FUNCTION isAvoidable as Boolean 

	LPARAMETERS sEventCode, sTableName as String, sFieldName as String 
	
	*Retorna True si para el evento sEventCode, el campo sFieldName es evitable en la tabla sTableName. ;
	Un campo es evitable en una tabla si su definición en Tables.ConfiguracionNotificacionInmediata así lo indica ;
	explícitamente o si no se encuentra en esa tabla. Conceptualmente un campo es evitable en una tabla si puede ;
	obviarse su regla de valores admisibles.sFieldName

	LOCAL sFilter  as String, sLcEventCode as String

	DIMENSION aFieldKind(1)

	sFilter = "COD_EVE='"  
	IF VARTYPE(sEventCode)='N' THEN
		 sLcEventCode = ALLTRIM(STR(sEventCode))
	ELSE
		 sLcEventCode = sEventCode
	ENDIF
	sFilter = sFilter + sLcEventCode + "' AND UPPER(CONTENEDOR)=UPPER('" + sTableName + "') AND UPPER(NOMB_CAMPO)=UPPER('" + sFieldName + "')"
	sFilter = sFilter + ' AND Activo'
	
	SELECT OBLIGATORI FROM ConfiguracionNotificacionInmediata WHERE &sFilter INTO ARRAY aFieldKind
	IF _TALLY>0 THEN
		RETURN !aFieldKind(1)
	ELSE
		RETURN .T.
	ENDIF

ENDFUNC

FUNCTION IsInmediateNotification

	LOCAL lRetVal
	
	STORE .F. TO lRetVal
	
	*** Test for presence of the disabling variable
	IF VARTYPE( gbIsInmediateNotification ) = "L" 
		lRetVal= gbIsInmediateNotification 
	ENDIF
	RETURN lRetVal
ENDFUNC


FUNCTION IsInmediateNotification_V02

	RETURN (gsInmediateToOrdinary = '00')
ENDFUNC


FUNCTION isValidAdjustment AS Boolean

	LPARAMETERS eventCode as String, sAdjustment as String

	*Determina si para el evento eventCode, el ajuste sAdjustment es aplicable según la	caracterización del evento Tables.EVENTOS


	SELECT EVENTOS.AJUSTES_NA FROM EVENTOS WHERE EVENTOS.COD_EVE = '&eventCode' AND (UPPER('&sAdjustment') $ EVENTOS.AJUSTES_NA) ;
		INTO ARRAY aEpidemiologicalWeekSeed
	RETURN (_TALLY=0)
ENDFUNC


FUNCTION IsValidAgeForEvent
	
	LPARAMETERS cEventCode as string, cAge as String, cUnitOfMeasure as  string
	
	* Evalúa cAge y cUnitOfMeasure, contra los valores EDAD_MAX y UNIDAD_MED establecidos en la tabla de eventos para el evento
	*	cEventCode. Retorna .T. cuando se cumpla que, cUnitOfMeasure  es menor o igual que UNIDAD_MED y 
	*	cAge es menor o igual que EDAD_MAX. Retorna .F. en caso contrario.

	LOCAL vResults AS String, lcMaxAge as String, lcUnitOfmeasure as String, bReturnedValue as Boolean
	
	LOCAL nAgeInMinutes as Double, nReferenceAgeInMinutes as Double

	DECLARE aResults[2]
	
	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	vResults = fieldValue('edad_max, unidad_med','EVENTOS', "COD_EVE = '" + cEventCode + "'")

	bReturnedValue = .T.

	IF ALINES(aResults, vResults, 1, ',') > 1
		*Reduce a minutos cAge
		DO CASE
			CASE cUnitOfMeasure = '5'
				nAgeInMinutes = VAL(cAge)
				
			CASE cUnitOfMeasure = '4'
				nAgeInMinutes = VAL(cAge) * 60
				
			CASE cUnitOfMeasure = '3'
				nAgeInMinutes = (VAL(cAge) * 24) * 60

			CASE cUnitOfMeasure = '2'
				nAgeInMinutes = ((VAL(cAge) * 30) *24) * 60

			CASE cUnitOfMeasure = '1'
				nAgeInMinutes = (((VAL(cAge) * 12) * 30) *24) * 60

			OTHERWISE

		ENDCASE

		*Reduce a minutos el valor de edad recuperado de la tabla Eventos
		DO CASE
			CASE aResults(2) = '5'
				nReferenceAgeInMinutes = VAL(aResults(1))
				
			CASE aResults(2) = '4'
				nReferenceAgeInMinutes = VAL(aResults(1)) * 60
				
			CASE aResults(2) = '3'
				nReferenceAgeInMinutes = (VAL(aResults(1)) * 24) * 60

			CASE aResults(2) = '2'
				nReferenceAgeInMinutes = ((VAL(aResults(1)) * 30) *24) * 60

			CASE aResults(2) = '1'
				nReferenceAgeInMinutes = (((VAL(aResults(1)) * 12) * 30) *24) * 60

			OTHERWISE
				nReferenceAgeInMinutes = 8.9E307
		ENDCASE

		bReturnedValue = ((nAgeInMinutes > 0) AND (nAgeInMinutes <= nReferenceAgeInMinutes ) )
	ENDIF
	
	RETURN bReturnedValue	
ENDFUNC



FUNCTION IsValidAgeForEvent2
	
	LPARAMETERS cEventCode as string, cAge as String, cUnitOfMeasure as  string
	
	* Evalúa cAge y cUnitOfMeasure, contra los valores EDAD_MIN y UNIMED_MIN establecidos en la tabla de eventos para el evento
	* cEventCode. Retorna .T. si la edad dada por el par (cAge,cUnitOfMeasure)  es "mayor o igual" que el par (EDAD_MIN,UNIMED_MIN) ;
	* en el sentido usual de comparación de edades; en caso contrario, retorna .F.

	LOCAL vResults AS String, lcMaxAge as String, lcUnitOfmeasure as String, bReturnedValue as Boolean
	
	LOCAL nAgeInMinutes as Double, nReferenceAgeInMinutes as Double

	DECLARE aResults[2]

	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	vResults = fieldValue('EDAD_MIN, UNIMED_MIN','EVENTOS', "COD_EVE = '" + cEventCode + "'")

	bReturnedValue = .T.

	IF ALINES(aResults, vResults, 1, ',') > 1
		*Reduce a minutos cAge
		DO CASE
			CASE cUnitOfMeasure = '5'
				nAgeInMinutes = VAL(cAge)
				
			CASE cUnitOfMeasure = '4'
				nAgeInMinutes = VAL(cAge) * 60
				
			CASE cUnitOfMeasure = '3'
				nAgeInMinutes = (VAL(cAge) * 24) * 60

			CASE cUnitOfMeasure = '2'
				nAgeInMinutes = ((VAL(cAge) * 30) *24) * 60

			CASE cUnitOfMeasure = '1'
				nAgeInMinutes = (((VAL(cAge) * 12) * 30) *24) * 60

			OTHERWISE

		ENDCASE

		*Reduce a minutos el valor de edad recuperado de la tabla Eventos
		DO CASE
			CASE aResults(2) = '5'
				nReferenceAgeInMinutes = VAL(aResults(1))
				
			CASE aResults(2) = '4'
				nReferenceAgeInMinutes = VAL(aResults(1)) * 60
				
			CASE aResults(2) = '3'
				nReferenceAgeInMinutes = (VAL(aResults(1)) * 24) * 60

			CASE aResults(2) = '2'
				nReferenceAgeInMinutes = ((VAL(aResults(1)) * 30) *24) * 60

			CASE aResults(2) = '1'
				nReferenceAgeInMinutes = (((VAL(aResults(1)) * 12) * 30) *24) * 60

			OTHERWISE
				nReferenceAgeInMinutes = 0
		ENDCASE

		bReturnedValue = ((nAgeInMinutes > 0) AND (nAgeInMinutes >= nReferenceAgeInMinutes ) )
	ENDIF
	
	RETURN bReturnedValue	
ENDFUNC

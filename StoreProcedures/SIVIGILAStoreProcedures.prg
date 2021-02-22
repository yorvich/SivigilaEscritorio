PROCEDURE spEvento_359_CountNewCases(sYear as String, sCodUPGD as String, nMes as String)

	*Establece el número de casos nuevos corrspondientes al evento 359 por PESO_NAC y por TIPO_IAD. Los resultados quedan almacenados ;
	en recordsets con nombres rsEventos_87NewCases_PESO_NAC y rsEventos_87NewCases_TIPO_IAD, respectivamente
	
	LOCAL sSQLCmd as String, sFilter as String 

	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	sFilter = "AÑO ='" +  sYear + "' AND COD_PRE+COD_SUB='" + sCodUPGD + "' AND MONTH(FEC_DIAG) = " + ALLTRIM(STR(nMes))
	DO fastProcessAdjustments WITH 'EVENTOS_87',getKeyFields('EVENTOS_87'), .T., sFilter IN SIVIGILAUtilities			

	sSQLCmd = "SELECT TIPO_UCI, TIPO_IAD, PESO_NAC, COUNT(*) AS N FROM rsAdjustedCases WHERE CASO_EXTRA!='1' AND AJUSTE!='6' AND AJUSTE!='D' AND TIPO_UCI = '3' AND " +;
				"AÑO ='" +  sYear + "' AND COD_PRE+COD_SUB='" + sCodUPGD + "' AND MONTH(FEC_DIAG) = " + ALLTRIM(STR(nMes)) + ;
				" GROUP BY TIPO_UCI, TIPO_IAD, PESO_NAC INTO CURSOR rsEventos_87NewCases_PESO_NAC"  
	&sSQLCmd

	sSQLCmd = "SELECT TIPO_UCI, TIPO_IAD, COUNT(*) AS N FROM rsAdjustedCases WHERE CASO_EXTRA!='1' AND AJUSTE!='6' AND AJUSTE!='D' AND AÑO ='" +  sYear + "' " +;
				"AND COD_PRE+COD_SUB='" + sCodUPGD + "' AND MONTH(FEC_DIAG) = "	+ ALLTRIM(STR(nMes)) + ;
				" GROUP BY TIPO_UCI, TIPO_IAD INTO CURSOR rsEventos_87NewCases_TIPO_IAD"  
	&sSQLCmd
ENDPROC

PROCEDURE spEvento_362_CountNewCases_V01(sYear as String, sCodUPGD as String, nMes as String)
	*Comentado por:	Wilson Aguilar
	*Fecha:	25/01/2020
	*Este sp entró en desuso con motivo de la implementación del requerimiento con ID=2944
	*Establece el número de casos nuevos correspondientes al evento 362 con base en los casos individuales de los eventos 352 y 351.;
	Los resultados quedan almacenados en recordsets con nombres rsEventos_76NewCases_PROCEDIMIE y rsEventos_75NewCases_ATEN_PARTO, respectivamente
	
	LOCAL sSQLCmd as String, sFilter as String 

	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	sFilter = "AÑO ='" +  sYear + "' AND COD_PRE+COD_SUB='" + sCodUPGD + "' AND MONTH(FEC_PROC) = " + ALLTRIM(STR(nMes))
	DO fastProcessAdjustments WITH 'EVENTOS_76',getKeyFields('EVENTOS_76'), .T., sFilter, 'rsEventos76AdjustedCases' IN SIVIGILAUtilities			

	sSQLCmd = "SELECT PROCEDIMIE, COUNT(*) AS N FROM rsEventos76AdjustedCases WHERE AJUSTE!='6' AND AJUSTE!='D' AND ATENC_UPGD != '2' "  +;
				" AND AÑO ='" +  sYear + "' AND COD_PRE+COD_SUB='" + sCodUPGD + "' AND MONTH(FEC_PROC) = " + ALLTRIM(STR(nMes)) + ;
				" GROUP BY PROCEDIMIE INTO CURSOR rsEventos_76NewCases_PROCEDIMIE"  
	&sSQLCmd

	sFilter = "AÑO ='" +  sYear + "' AND COD_PRE+COD_SUB='" + sCodUPGD + "' AND MONTH(FEC_PARTO) = " + ALLTRIM(STR(nMes))
	DO fastProcessAdjustments WITH 'EVENTOS_75',getKeyFields('EVENTOS_75'), .T., sFilter, 'rsEventos75AdjustedCases' IN SIVIGILAUtilities			
	sSQLCmd = "SELECT ATEN_PARTO, COUNT(*) AS N FROM rsEventos75AdjustedCases WHERE AJUSTE!='6' AND AJUSTE!='D' AND ATENC_UPGD != '2' "  +;
				" AND AÑO ='" +  sYear + "' " +;
				"AND COD_PRE+COD_SUB='" + sCodUPGD + "' AND MONTH(FEC_PARTO) = "	+ ALLTRIM(STR(nMes)) + ;
				" GROUP BY ATEN_PARTO INTO CURSOR rsEventos_75NewCases_ATEN_PARTO"  
	&sSQLCmd
ENDPROC

PROCEDURE spEvento_362_CountNewCases(sYear as String, sCodUPGD as String, nMes as String)
	*Establece el número de casos nuevos correspondientes al evento 362 con base en los casos individuales del evento 352.;
	Los resultados quedan almacenados en un recordset con nombre rsEventos_76NewCases_PROCEDIMIE
	
	LOCAL sSQLCmd as String, sFilter as String 

	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	sFilter = "AÑO ='" +  sYear + "' AND COD_PRE+COD_SUB='" + sCodUPGD + "' AND MONTH(FEC_PROC) = " + ALLTRIM(STR(nMes))
	DO fastProcessAdjustments WITH 'EVENTOS_76',getKeyFields('EVENTOS_76'), .T., sFilter, 'rsEventos76AdjustedCases' IN SIVIGILAUtilities			

	sSQLCmd = "SELECT PROCEDIMIE, COUNT(*) AS N FROM rsEventos76AdjustedCases WHERE AJUSTE!='6' AND AJUSTE!='D' "  +;
				" AND AÑO ='" +  sYear + "' AND COD_PRE+COD_SUB='" + sCodUPGD + "' AND MONTH(FEC_PROC) = " + ALLTRIM(STR(nMes)) + ;
				" GROUP BY PROCEDIMIE INTO CURSOR rsEventos_76NewCases_PROCEDIMIE"  
	&sSQLCmd

ENDPROC

FUNCTION DoNotApplyRules

	LOCAL lRetVal
	
	STORE .F. TO lRetVal
	
	*** Test for presence of the disabling variable
	IF VARTYPE( glDisableRules ) = "L" 
		lRetVal= glDisableRules
	ENDIF
	RETURN lRetVal
ENDFUNC


FUNCTION IsInmediateNotification 

	LOCAL lRetVal
	
	STORE .F. TO lRetVal
	
	*** Test for presence of the flag variable
	IF VARTYPE( gbIsInmediateNotification ) = "L" 
		lRetVal= gbIsInmediateNotification 
	ENDIF
	RETURN lRetVal
ENDFUNC


FUNCTION isAvoidable as Boolean 

	LPARAMETERS sEventCode as String, sTableName as String, sFieldName as String 
	
	*Retorna True si para el evento sEventCode, el campo sFieldName es evitable en la tabla sTableName. ;
	Un campo es evitable en una tabla si su definición en Tables.ConfiguracionNotificacionInmediata así lo indica ;
	explícitamente o si no se encuentra en esa tabla. Conceptualmente un campo es evitable en una tabla si puede ;
	obviarse su regla de valores admisibles.sFieldName

	LOCAL sFilter  as String

	DIMENSION aFieldKind(1)

	sFilter = "COD_EVE='" + sEventCode + "' AND UPPER(CONTENEDOR)=UPPER('" + sTableName + "') AND UPPER(NOMB_CAMPO)=UPPER('" + sFieldName + "')"
	SELECT OBLIGATORI FROM ConfiguracionNotificacionInmediata WHERE &sFilter INTO ARRAY aFieldKind
	IF _TALLY>0 THEN
		RETURN !aFieldKind(1)
	ELSE
		RETURN .T.
	ENDIF

ENDFUNC


FUNCTION IsNumeric

	LPARAMETERS sExpression as String

	*Retorna True si sExpression corresponde a un número. El único separador de cifras decimales admitido es el '.'; por tanto, por ejemplo, ;
	'12.4' es un número pero '12,4', no lo es.
	
	
	LOCAL sExpressionWithoutLeftZeros as String
	*expresión obtenida al eliminar ceros no significativos a la izquierda de sExpression 
	
	LOCAL sAux as String
	*expresión obtenida al agregar a la derecha de sExpressionWithoutLeftZeros el número necesario de ceros hasta completar 10 decimales. ;
	La expresión obtenida sAux será de longitud <= a 21 caracteres
	 
	LOCAL  nPointPosition as Number, sLength as Number 
	
	
	IF VARTYPE(sExpression )='N' THEN
		RETURN .T.
	ELSE
		*Elimina de sExpression todos los ceros a la izquierda
		sExpressionWithoutLeftZeros=LTRIM(sExpression,1,'0')
		
		*Si sExpression contiene decimales, agrega un cero a la izquierda
		nPointPosition = AT('.',sExpressionWithoutLeftZeros)
		IF nPointPosition > 0 THEN
			IF LEFT(sExpressionWithoutLeftZeros,1) = '.' THEN
				sExpressionWithoutLeftZeros = '0' + sExpressionWithoutLeftZeros
			ENDIF
		ENDIF
		
		sLength=LEN(ALLTRIM(sExpressionWithoutLeftZeros))
		
		*Agrega el número necesario de ceros a la derecha de sExpressionWithoutLeftZeros hasta completar un número de decimales igual 10
		sAux=ALLTRIM(STR(VAL(sExpressionWithoutLeftZeros),21,10))
		
		*Retorna True si la expresión que se obtiene al eliminar a la derecha los ceros no significativos es idéntica a la expresión ;
		obtenida al eliminar ceros no significativos a la izquierda
		RETURN	STUFF(sAux,sLength+1,LEN(sAux)-sLength,'')=ALLTRIM(sExpressionWithoutLeftZeros)
	ENDIF
	
ENDFUNC


FUNCTION getSimpleEpidemiologicalWeek 

	LPARAMETERS dDateBase as Date
	
	sReturnedValue = ''

	IF VARTYPE(dDateBase) = 'D' THEN
		lcsSelectSqlCmd = "SELECT TRANSFORM(semana, '99') FROM CALENDARIO WHERE " +;
			" dDateBase BETWEEN desde AND hasta into ARRAY aResult"
		&lcsSelectSqlCmd
		IF _TALLY != 0
			sReturnedValue = aResult
		ENDIF
	ENDIF
		
	RETURN sReturnedValue
ENDFUNC

FUNCTION getSimpleEpidemiologicalYear

	LPARAMETERS dDateBase as Date
	
	sReturnedValue = ''

	IF VARTYPE(dDateBase) = 'D' THEN
		lcsSelectSqlCmd = "SELECT TRANSFORM(Vigencia, '9999') FROM CALENDARIO WHERE " +;
			" dDateBase BETWEEN desde AND hasta into ARRAY aResult"
		&lcsSelectSqlCmd
		IF _TALLY != 0
			sReturnedValue = aResult
		ENDIF
	ENDIF
		
	RETURN sReturnedValue
ENDFUNC

FUNCTION upgdIsMilitaryForces
  
  	bReturnedValue=.F.
  	
  	IF getBasicDataValue('FM', 'GENERAL', '', '')  = 1
  		bReturnedValue = .T.
  	ENDIF
  	
  	RETURN bReturnedValue
ENDFUNC



FUNCTION belongsToAgeGroup

	LPARAMETERS sAgeMeasureUnit AS String, sPatientAge AS String, strEventCode AS String
	LOCAL sAgeGroup, bReturnedValue
	
	bReturnedValue=.T.
	
	IF VAL(sAgeMeasureUnit)=1
      DO CASE
      CASE VAL(sPatientAge)=0
         sAgeGroup='1'
      CASE BETWEEN(VAL(sPatientAge),1,4)
         sAgeGroup = '2'
      CASE BETWEEN(VAL(sPatientAge),5,14)
         sAgeGroup = '3'
      CASE BETWEEN(VAL(sPatientAge),15,44)
         sAgeGroup = '4'
      CASE BETWEEN(VAL(sPatientAge),45,60)
         sAgeGroup = '5'
      CASE VAL(sPatientAge)>60
         sAgeGroup = '6'
      ENDCASE
   ELSE
      sAgeGroup = '1'
   ENDIF

	sValidationGroupString = getBasicDataValue('GRU_PER', 'EVENTOS', 'COD_EVE', strEventCode) 
	IF !ISNULL(sValidationGroupString) THEN
		IF LEN(CHRTRAN(sValidationGroupString, sAgeGroup,''))<>LEN(sValidationGroupString)
			bReturnedValue = .F.
		ENDIF
	ENDIF
	
	RETURN bReturnedValue

ENDFUNC

FUNCTION getBasicDataValue

	LPARAMETERS sfieldToExtract AS String, sTableForSearch as String, sKeyExpression as String, sStringForSEarch as String
	LOCAL vReturnedValue  AS VARIANT
	vReturnedValue = NULL
	
	DO CASE
		CASE VARTYPE(sStringForSEarch) = 'C'
			sStringForSEarchTransformed = "'" + (sStringForSEarch) + "' "
		CASE VARTYPE(sStringForSEarch) = 'D'
			sStringForSEarchTransformed = "CTOD('" + DTOC(sStringForSEarch) + "') "
		CASE VARTYPE(sStringForSEarch) = 'N'
			sStringForSEarchTransformed = TRANS(sStringForSEarch)
		CASE VARTYPE(sStringForSEarch) = 'L'
			sStringForSEarchTransformed = IIF(sStringForSEarch, ' .T. ', ' .F. ')
			
		OTHERWISE
			sStringForSEarchTransformed = '.F.'		
	ENDCASE
	
	lcsSelectSqlCmd = 'SELECT ' + (sfieldToExtract) + ' FROM ' + (sTableForSearch) 
	
	IF !EMPTY(sKeyExpression) AND !EMPTY(sStringForSEarch)
		lcsSelectSqlCmd  = lcsSelectSqlCmd + ' WHERE ' + (sKeyExpression) + ' = ' + sStringForSEarchTransformed
	ENDIF	
	lcsSelectSqlCmd  = lcsSelectSqlCmd  + ' INTO ARRAY aResult'
	&lcsSelectSqlCmd
	IF _TALLY != 0
		vReturnedValue = aResult
	ENDIF
	
	RETURN vReturnedValue
ENDFUNC

FUNCTION compareTimePeriod
	LPARAMETERS sTime1 as String, sTime2 as String
	
	*Returns: ;
		-1 if sTime1 "is less than " sTime2 ;
		0  if sTime1 "is equal to" sTime2 ;
		1  if sTime1 "is gfrater than" sTime2 ;
		2  otherwise
		
	*sTime1 and sTime are time specifications in the format mm:hh:dd

	LOCAL ARRAY aLocalVersion(1,3)
	LOCAL ARRAY aForeignVersion(1,3)
	LOCAL iNMajorLocalVersion as Integer, iNMinorLocalVersion as integer, iNBuildLocalVersion as integer
	LOCAL iNMajorForeignVersion as Integer, iNMinorForeignVersion as integer, iNBuildForeignVersion as integer

	iReturnedValueForLess = -1
	iReturnedValueForEqual = 0
	iReturnedValueForBig = 1
	iReturnedValueForOtherwise = -2
	iReturnedValue = iReturnedValueForOtherwise
	
	* Validar que las expresiones que se pasan están bien formadas, esto es, de la forma ##:##:##
	IF AT(":", sTime1, 2) > 0 AND AT(":", sTime2, 2) > 0
		* Las cadenas no traen los caracteres ':'

		ALINES(aLocalVersion, sTime2 , 1, ':')
		ALINES(aForeignVersion, sTime1 , 1, ':')	
		
		iNMajorForeignVersion = INT(VAL(aForeignVersion(3)))
		iNMinorForeignVersion = INT(VAL(aForeignVersion(2)))
		iNBuildForeignVersion = INT(VAL(aForeignVersion(1)))
		
		iNMajorLocalVersion = INT(VAL(aLocalVersion(3)))
		iNMinorLocalVersion = INT(VAL(aLocalVersion(2)))
		iNBuildLocalVersion = INT(VAL(aLocalVersion(1)))
		
		*/ Validación para NMajor
		IF VARTYPE(iNMajorLocalVersion) = 'N' AND VARTYPE(iNMajorForeignVersion) = 'N'
			IF iNMajorForeignVersion >= iNMajorLocalVersion
				iReturnedValue = IIF(iNMajorForeignVersion > iNMajorLocalVersion, iReturnedValueForBig, iReturnedValueForEqual)
			ELSE
				iReturnedValue = iReturnedValueForLess
			ENDIF
		ELSE
			iReturnedValue = iReturnedValueForOtherwise
		ENDIF
			
		*/ Validación para NMinor
		IF iReturnedValue = iReturnedValueForEqual
			IF VARTYPE(iNMinorLocalVersion) = 'N' AND VARTYPE(iNMinorForeignVersion) = 'N'
				IF iNMinorForeignVersion >= iNMinorLocalVersion
					iReturnedValue = IIF(iNMinorForeignVersion > iNMinorLocalVersion, iReturnedValueForBig, iReturnedValueForEqual)
				ELSE
					iReturnedValue = iReturnedValueForLess
				ENDIF
			ELSE
				iReturnedValue = iReturnedValueForOtherwise
			ENDIF
		ENDIF
		
		*/ Validación para nBuild
		IF iReturnedValue = iReturnedValueForEqual
			IF VARTYPE(iNBuildLocalVersion) = 'N' AND VARTYPE(iNBuildForeignVersion) = 'N'
				IF iNBuildForeignVersion >= iNBuildLocalVersion
					iReturnedValue = IIF(iNBuildForeignVersion > iNBuildLocalVersion, iReturnedValueForBig, iReturnedValueForEqual)
				ELSE
					iReturnedValue = iReturnedValueForLess
				ENDIF
			ELSE
				iReturnedValue = iReturnedValueForOtherwise
			ENDIF
		ENDIF
	ENDIF
	RETURN iReturnedValue 
ENDFUNC


FUNCTION fieldValue

	LPARAMETERS sTargetFieldName AS String, sTargetTableName as String, sFilter as String, sOrderBy as String
	
	*Esta función es copia de la que se encuentra en el módulo SIVIGILAUtilities.
	
	*Retorna el valor almacenado en el(los) campo(s) sTargetFieldName del registro de la tabla sTargetTableName que cumpla la condición sFilter. ;
	Si hay más de un registro en sTargetTableName  que cumpla la condición, retorna el valor del primero encontrado.
	
	*sTargetFieldName puede ser una lista de campos separada por comas; en este caso, se retornará una cadena separada por comas con los valores ;
	de cada campo convertido a un string; si sTargetFieldName es un solo campo, retornará su valor sin conversión alguna
	
	*El parámetro opcional sOrderBy permite especificar un criterio de ordenación para los registros que cumplan la condición sFilter ;
	de tal forma que, si existen varios registros que la cumplan, el valor retornado sea el primero de acuerdo con ese orden.
	
	
	LOCAL vReturnedValue  AS VARIANT
	LOCAL nSelectedWorkArea as Integer, bSourceTableIsOpen as Boolean
		
	vReturnedValue = NULL
	
	nSelectedWorkArea=SELECT()
	bSourceTableIsOpen= USED(sTargetTableName) 

	IF FILE('&sTargetTableName..DBF') THEN
		lcsSelectSqlCmd = 'SELECT ' + sTargetFieldName + ' FROM ' + sTargetTableName
		IF !EMPTY(sFilter)
			lcsSelectSqlCmd  = lcsSelectSqlCmd + ' WHERE ' + sFilter
		ENDIF
		IF !EMPTY(sOrderBy )
			lcsSelectSqlCmd  = lcsSelectSqlCmd + ' ORDER BY ' + sOrderBy 
		ENDIF

		lcsSelectSqlCmd  = lcsSelectSqlCmd  + ' INTO ARRAY aResult'
		&lcsSelectSqlCmd
		IF _TALLY != 0
			IF ALEN(aResult,2) = 1 THEN
				vReturnedValue = aResult
			ELSE
				sValuesList = ''
				FOR nValue=1 TO ALEN(aResult,2)
					DO CASE 
						CASE VARTYPE(aResult(1,nValue))='C'
							sValuesList  = sValuesList  + aResult(1,nValue)
						CASE VARTYPE(aResult(1,nValue))='D'
							sValuesList  = sValuesList  + DTOC(aResult(1,nValue))
						CASE VARTYPE(aResult(1,nValue))='N'
							sValuesList  = sValuesList  + ALLTRIM(STR(aResult(1,nValue)))
						CASE VARTYPE(aResult(1,nValue))='L'
							sValuesList  = sValuesList  + TRANSFORM(aResult(1,nValue),'Y')
					ENDCASE
					sValuesList  = sValuesList  + ","
				NEXT nValue
				sValuesList  = LEFT(sValuesList ,LEN(sValuesList )-1)
				vReturnedValue = sValuesList 
			ENDIF
		ENDIF
		
		IF !bSourceTableIsOpen THEN
			SELECT (sTargetTableName)
			USE
		ENDIF
	ENDIF
	SELECT (nSelectedWorkArea)
	
	RETURN vReturnedValue
ENDFUNC


FUNCTION IsValidAgeForEvent
	
	LPARAMETERS cEventCode as string, cAge as String, cUnitOfMeasure as  string
	
	*Esta función es copia de la que se encuentra en el módulo EventsHandler excepto por una modificación en la llamada a ;
	feldValue, en donde se ha eliminado la instrucción previa SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	
	* Evalúa cAge y cUnitOfMeasure, contra los valores EDAD_MAX y UNIDAD_MED establecidos en la tabla de eventos para el evento
	*	cEventCode. Retorna .T. cuando se cumpla que, cUnitOfMeasure  es menor o igual que UNIDAD_MED y 
	*	cAge es menor o igual que EDAD_MAX. Retorna .F. en caso contrario.

	LOCAL vResults AS String, lcMaxAge as String, lcUnitOfmeasure as String, bReturnedValue as Boolean
	
	LOCAL nAgeInMinutes as Double, nReferenceAgeInMinutes as Double

	DECLARE aResults[2]
	
	*SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
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
	
	*Esta función es copia de la que se encuentra en el módulo EventsHandler excepto por una modificación en la llamada a ;
	feldValue, en donde se ha eliminado la instrucción previa SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	
	* Evalúa cAge y cUnitOfMeasure, contra los valores EDAD_MIN y UNIMED_MIN establecidos en la tabla de eventos para el evento
	* cEventCode. Retorna .T. si la edad dada por el par (cAge,cUnitOfMeasure)  es "mayor o igual" que el par (EDAD_MIN,UNIMED_MIN) ;
	* en el sentido usual de comparación de edades; en caso contrario, retorna .F.

	LOCAL vResults AS String, lcMaxAge as String, lcUnitOfmeasure as String, bReturnedValue as Boolean
	
	LOCAL nAgeInMinutes as Double, nReferenceAgeInMinutes as Double

	DECLARE aResults[2]

	*SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
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

FUNCTION ageIsLessThan

	LPARAMETERS sAge1 as String, sAge2 as String

	*Retorna TRUE si sAge1 < sAge2. ;
	sAge(i) i=1,2 es una lista separada por comas en donde el primer item es el número de la edad ;
	y el segundo la unidad de medida (1-Años, 2-Meses, 3-Días, 4-Horas, 5-Minutos)
	
	DIMENSION aAge1(1)
	DIMENSION aAge2(1)
	
	=ALINES(aAge1,sAge1,15,',')  
	=ALINES(aAge2,sAge2,15,',')
	
	bReturnedValue=.T.
	IF aAge1(2)=aAge2(2)
		RETURN (VAL(aAge1(1))<VAL(aAge2(1)))
	ELSE
		RETURN reduceToMinutes(@aAge1)<reduceToMinutes(@aAge2)
	ENDIF
	
ENDFUNC


FUNCTION isValidIdentificationDoc

	LPARAMETERS sIdentificationKind as String, sUnitOfMeasure as string, sAge as String, sErrMsg as sgtring	
	
	*Establece la consistencia entre el tipo de identifcación sIdentificationKind y la edad determinada por el par (sUnitOfMeasure, sAge );
	Devuelve .T. si hay consistencia; en caso contrario decuelve .F. ;
	En el parámetro opcional sErrMsg, se retorna un aviso indicativo de la inconsistencia encontrada
	
	IF sUnitOfMeasure ='1' AND (VAL(sAge)<18 OR VAL(sAge)>132)
		*sUnitOfMeasure es 1 = Años
		IF sIdentificationKind ='CC' OR sIdentificationKind ='CE' OR sIdentificationKind ='AS'
			IF VARTYPE(sErrMsg)='C' THEN
				sErrMsg = "Edad inválida para el tipo de documento"
			ENDIF
	    	RETURN .F.
		ENDIF
	ENDIF
	
	IF sUnitOfMeasure !='1'
		*sUnitOfMeasure es una de las unidades 0 = No aplica 2 = Meses 3 = Días 4 = Horas 5 = Minutos
		IF sIdentificationKind ='CC' OR sIdentificationKind ='CE' OR sIdentificationKind ='AS'
			IF VARTYPE(sErrMsg)='C' THEN
				sErrMsg = "Edad inválida para el tipo de documento"
			ENDIF
	    	RETURN .F.
		ENDIF
	ENDIF
	
	IF sUnitOfMeasure ='1' AND VAL(sAge)<7
		*sUnitOfMeasure es 1 = Años
		IF sIdentificationKind!='MS' AND sIdentificationKind!='RC' AND sIdentificationKind!='PA'  AND sIdentificationKind!='PE'
			IF VARTYPE(sErrMsg)='C' THEN
				sErrMsg = "Edad inválida para el tipo de documento"
			ENDIF
	    	RETURN .F.
		ENDIF
	ENDIF

	IF INLIST(ALLTRIM(sIdentificationKind),'CC','TI','CE') AND sUnitOfMeasure  !='1' THEN
		IF VARTYPE(sErrMsg)='C' THEN
			sErrMsg = 'Si selecciona  CC, TI o CE  en documento de identidad, unidad de medida solo puede ser 1'
		ENDIF
		RETURN .F.
	ENDIF

	IF sUnitOfMeasure ='1' AND VAL(sAge)>17 
		IF sIdentificationKind ='MS' OR sIdentificationKind ='RC' OR sIdentificationKind ='TI'
			IF VARTYPE(sErrMsg)='C' THEN
				sErrMsg = "Edad inválida para el tipo de documento"
			ENDIF
	    	RETURN .F.
		ENDIF
	ENDIF
 
	IF ALLTRIM(sIdentificationKind) = 'CN' THEN
		sErrMsg = 'Si selecciona  CN en documento de identidad, la edad debe ser menor o igual a tres meses'
		IF EMPTY(sAge) OR EMPTY(sUnitOfMeasure) then
			RETURN .F.
		ELSE
			*RETURN (ageIsLessThan('&sAge,&sUnitOfMeasure','4,2'))
			RETURN (ageIsLessThan('&sAge,&sUnitOfMeasure','91,3'))
		ENDIF
	ENDIF
	
	RETURN .T.
ENDFUNC


FUNCTION defaultValueFor_eventos_55_DIF_FC_CH
	
	*Establece el valor del campo dif_fc_ch como la diferencia entre la fecha de consulta de la tabla de paciente y la fecha de resultado de cuadro hemático
	
	LOCAL sDiff  AS Number
	LOCAL dCurrentDBFieldValue  AS Date 

	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		dCurrentDBFieldValue = fieldValue('FEC_CON','PACIENTE',"AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + ;
											EVENTOS_55.AÑO + EVENTOS_55.SEMANA+EVENTOS_55.COD_EVE+EVENTOS_55.TIP_IDE+EVENTOS_55.NUM_IDE+ ;
											EVENTOS_55.COD_PRE+EVENTOS_55.COD_SUB + "'","FEC_AJU DESC")
		IF !ISNULL(dCurrentDBFieldValue) AND !EMPTY(EVENTOS_55.FEC_RES_CE) THEN 
			sDiff = ALLTRIM(STR(EVENTOS_55.FEC_RES_CE - dCurrentDBFieldValue))
			IF EVENTOS_55.DIF_FC_CH!=sDiff
				REPLACE EVENTOS_55.DIF_FC_CH WITH sDiff 
			ENDIF
		ENDIF
		RETURN .T.
	ENDIF
ENDFUNC


FUNCTION defaultValueFor_eventos_55_DIF_CH_AMO
	
	*Establece el valor del campo DIF_CH_AMO como Diferencia en días entre la fecha de resultado de cuadro hemático ;
	 y la fecha de resultado del Aspirado de médula ósea
	
	LOCAL sDiff  AS Number

	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		IF !EMPTY(EVENTOS_55.FEC_RES_MO) AND !EMPTY(EVENTOS_55.FEC_RES_CE) THEN 
			sDiff = ALLTRIM(STR(EVENTOS_55.FEC_RES_MO-EVENTOS_55.FEC_RES_CE))
			IF EVENTOS_55.DIF_CH_AMO != sDiff
				REPLACE EVENTOS_55.DIF_CH_AMO WITH sDiff 
			ENDIF
		ENDIF
		RETURN .T.
	ENDIF
ENDFUNC


FUNCTION defaultValueFor_eventos_55_DIF_AMO_IT
	
	*Establece el valor del campo DIF_AMO_IT como la Diferencia en días entre la fecha de resultado de ;
	Aspirado de médula ósea y la fecha de inicio de tratamiento
	
	LOCAL sDiff  AS Number

	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		IF !EMPTY(EVENTOS_55.FEC_RES_MO) AND !EMPTY(EVENTOS_55.FEC_INI_TR) THEN 
			sDiff = ALLTRIM(STR(EVENTOS_55.FEC_INI_TR - EVENTOS_55.FEC_RES_MO))
			IF EVENTOS_55.DIF_AMO_IT != sDiff
				REPLACE EVENTOS_55.DIF_AMO_IT WITH sDiff 
			ENDIF
		ENDIF
		RETURN .T.
	ENDIF
ENDFUNC


FUNCTION defaultValueFor_paciente_INGR_X_NI

	*Establece el valor del campo INGR_X_NI igual al valor de la variable gbIsInmediateNotification
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		IF VARTYPE(gbIsInmediateNotification)='L' THEN
			IF PACIENTE.INGR_X_NI != gbIsInmediateNotification
				REPLACE PACIENTE.INGR_X_NI WITH gbIsInmediateNotification
			ENDIF
		ENDIF
		RETURN .T.
	ENDIF
ENDFUNC


FUNCTION isLessThanOrEqual
	
	LPARAMETERS	a AS Number, b AS Number, c AS Number, d AS Number

	*Esta función es copia de la que se encuentra en el módulo Utilities.
	
	*Retorna .T. si el par (a,b) es menor o igual que el par (c,d). (a,b) "<" (c,d) si d>b o si d=b y c>a; por otra parte;
	(a,b) = (c,d) en el sentido matemático usual
	
	bReturnedValue = .F.
	IF d>b THEN
		bReturnedValue=.T.
	ELSE
		IF d=b THEN
			bReturnedValue = (c>=a)
		ENDIF
	ENDIF
	RETURN bReturnedValue
	
ENDFUNC


FUNCTION isValidEpidemiologicalWeek

	LPARAMETERS sEpidemiologicalWeek as String, sEpidemiologicalYear as String
	
	*Retorna .T. si la semana epidemiológica calculada a partir de la fecha actual es "mayor o igual" que el par (sEpidemiologicalWeek, sEpidemiologicalYear);
	en el sentido que se define a continuación: un par (s1,a1) es mayor que otro (s2,a2) si a1 > a2 o (a1 = a2 y  s1 >= s2)
	
	sYear = getSimpleEpidemiologicalYear(DATE())
	sWeek = getSimpleEpidemiologicalWeek(DATE())
	
	RETURN VAL(sYear)>VAL(sEpidemiologicalYear) OR (VAL(sYear)=VAL(sEpidemiologicalYear) AND VAL(sWeek)>=VAL(sEpidemiologicalWeek))
ENDFUNC

	
FUNCTION daysOfMonth(nYear as Number, nMonth as Number )

	*Esta función es copia de la que se encuentra en el módulo Utilities.

	x = DATE(nYear, nMonth, 1)
	y = GOMONTH(x,1)
	RETURN  y - x 
ENDFUNC
FUNCTION FieldRuleFor_brotes_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRU_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRU_1)>=0 AND ISNUMERIC(GRU_1)) OR  (EMPTY(GRU_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRU_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRU_2)>=0 AND ISNUMERIC(GRU_2)) OR  (EMPTY(GRU_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_3)>=0 AND ISNUMERIC(GRUPO_3)) OR  (EMPTY(GRUPO_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_4
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_4)>=0 AND ISNUMERIC(GRUPO_4)) OR  (EMPTY(GRUPO_4)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_5
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_5)>=0 AND ISNUMERIC(GRUPO_5)) OR  (EMPTY(GRUPO_5)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_6
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_6)>=0 AND ISNUMERIC(GRUPO_6)) OR  (EMPTY(GRUPO_6)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_7
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_7)>=0 AND ISNUMERIC(GRUPO_7)) OR  (EMPTY(GRUPO_7)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_8
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_8)>=0 AND ISNUMERIC(GRUPO_8)) OR  (EMPTY(GRUPO_8)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_9
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_9)>=0 AND ISNUMERIC(GRUPO_9)) OR  (EMPTY(GRUPO_9)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_10
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_10)>=0 AND ISNUMERIC(GRUPO_10)) OR  (EMPTY(GRUPO_10)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_11
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_11)>=0 AND ISNUMERIC(GRUPO_11)) OR  (EMPTY(GRUPO_11)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_12
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_12)>=0 AND ISNUMERIC(GRUPO_12)) OR  (EMPTY(GRUPO_12)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_13
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_13)>=0 AND ISNUMERIC(GRUPO_13)) OR  (EMPTY(GRUPO_13)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_14
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_14)>=0 AND ISNUMERIC(GRUPO_14)) OR  (EMPTY(GRUPO_14)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_15
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_15)>=0 AND ISNUMERIC(GRUPO_15)) OR  (EMPTY(GRUPO_15)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_16
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_16)>=0 AND ISNUMERIC(GRUPO_16)) OR  (EMPTY(GRUPO_16)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_17
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_17)>=0 AND ISNUMERIC(GRUPO_17)) OR  (EMPTY(GRUPO_17)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_GRUPO_18
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GRUPO_18)>=0 AND ISNUMERIC(GRUPO_18)) OR  (EMPTY(GRUPO_18)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_CAS_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_PRO)>=0 AND ISNUMERIC(CAS_PRO)) OR  (EMPTY(CAS_PRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_CAS_CONL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_CONL)>=0 AND ISNUMERIC(CAS_CONL)) OR  (EMPTY(CAS_CONL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_CAS_CONC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_CONC)>=0 AND ISNUMERIC(CAS_CONC)) OR  (EMPTY(CAS_CONC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_CAS_CONN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_CONN)>=0 AND ISNUMERIC(CAS_CONN)) OR  (EMPTY(CAS_CONN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_HOMBRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(HOMBRES)>=0 AND ISNUMERIC(HOMBRES)) AND  (.NOT. EMPTY(HOMBRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_MUJERES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(MUJERES)>=0 AND ISNUMERIC(MUJERES)) AND  (.NOT. EMPTY(MUJERES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(VIVOS)>=0 AND ISNUMERIC(VIVOS)) AND  (.NOT. EMPTY(VIVOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_MUERTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(MUERTOS)>=0 AND ISNUMERIC(MUERTOS)) AND  (.NOT. EMPTY(MUERTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR  (EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_FEC_AJU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(FEC_AJU))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_PTE_HOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PTE_HOS)>=0 AND ISNUMERIC(PTE_HOS)) AND  (.NOT. EMPTY(PTE_HOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_brotes_PTE_AMB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PTE_AMB)>=0 AND ISNUMERIC(PTE_AMB)) AND  (.NOT. EMPTY(PTE_AMB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_PRI_NOM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PRI_NOM))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_PRI_APE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PRI_APE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_EDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD)>=1  AND  VAL(EDAD) <=130 AND ISNUMERIC(EDAD)) AND ( .NOT. EMPTY(EDAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_UNI_MED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UNI_MED,'1','2','3','4','5')) AND ( .NOT. EMPTY(UNI_MED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_SEXO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEXO,'M','F')) AND ( .NOT. EMPTY(SEXO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_DIR_RES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DIR_RES)) OR INLIST(COD_EVE,'357','351','352')
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_OCUPACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(OCUPACION)) OR INLIST(COD_EVE,'357','351','352')
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_TIP_SS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_SS,'C','S','P','E','N','I')) AND ( .NOT. EMPTY(TIP_SS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_COD_MUN_R
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_MUN_R))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) AND ( .NOT. EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_TELEFONO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(TELEFONO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_DESPLAZAMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESPLAZAMI,'1','2')) AND ( .NOT. EMPTY(DESPLAZAMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_ID_PADRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ID_PADRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_TIPO_CONTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_CONTA,'1','2','3','4','5')) AND ( .NOT. EMPTY(TIPO_CONTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_ROL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ROL,'1','2')) AND ( .NOT. EMPTY(ROL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_FEC_AJU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_AJU))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'3','5','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_FECINI_GRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FECINI_GRA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_EST_INGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_INGR,0,2,3)) OR (EST_INGR=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_ID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ID) OR NOTIF_IAAS!='1')
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_TOMA_MUEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOMA_MUEST,'1','2')) AND ( .NOT. EMPTY(TOMA_MUEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_RESULTADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULTADO,'1','2')) OR ( EMPTY(RESULTADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_TIPIDE_POS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPIDE_POS,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIPIDE_POS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_NUMIDE_POS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUMIDE_POS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_COD_EVEPOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVEPOS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Contactos_SINTOMATIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SINTOMATIC,'1','2')) AND ( .NOT. EMPTY(SINTOMATIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_CARNE_VACU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARNE_VACU,'1','2')) AND ( .NOT. EMPTY(CARNE_VACU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_VAC_FA1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_FA1,'1','2','3')) OR ( EMPTY(VAC_FA1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_MALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MALGIAS,'1','2')) AND ( .NOT. EMPTY(MALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_ARTRALGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRALGIA,'1','2')) AND ( .NOT. EMPTY(ARTRALGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) AND ( .NOT. EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_ICTERICIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ICTERICIA,'1','2')) AND ( .NOT. EMPTY(ICTERICIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_HEMOPTISIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMOPTISIS,'1','2')) AND ( .NOT. EMPTY(HEMOPTISIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_SFAGET
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SFAGET,'1','2')) AND ( .NOT. EMPTY(SFAGET)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_HIPIREMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPIREMIA,'1','2')) AND ( .NOT. EMPTY(HIPIREMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_HEMATEMESI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMATEMESI,'1','2')) AND ( .NOT. EMPTY(HEMATEMESI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_OLIGURIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OLIGURIA,'1','2')) AND ( .NOT. EMPTY(OLIGURIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_PETEQUIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PETEQUIAS,'1','2')) AND ( .NOT. EMPTY(PETEQUIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_METRORRAGI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(METRORRAGI,'1','2')) AND ( .NOT. EMPTY(METRORRAGI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_CHOQUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CHOQUE,'1','2')) AND ( .NOT. EMPTY(CHOQUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_BRADICARDI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BRADICARDI,'1','2')) AND ( .NOT. EMPTY(BRADICARDI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_MELENAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MELENAS,'1','2')) AND ( .NOT. EMPTY(MELENAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_EQUIMOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EQUIMOSIS,'1','2')) AND ( .NOT. EMPTY(EQUIMOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_EPISTAXIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPISTAXIS,'1','2')) AND ( .NOT. EMPTY(EPISTAXIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_HEMATURIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMATURIA,'1','2')) AND ( .NOT. EMPTY(HEMATURIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_FALLA_RENA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_RENA,'1','2')) AND ( .NOT. EMPTY(FALLA_RENA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_FALLA_HEPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_HEPA,'1','2')) AND ( .NOT. EMPTY(FALLA_HEPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_HEPATOMEGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATOMEGA,'1','2')) AND ( .NOT. EMPTY(HEPATOMEGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_CAS_FA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAS_FA,'1','2')) AND ( .NOT. EMPTY(CAS_FA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_CODMUNINFE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CODMUNINFE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_01_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_COD_SUST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUST))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_NOM_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_PRO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_TIP_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_EXP,'1','2','3','4','6','8','9','10')) AND ( .NOT. EMPTY(TIP_EXP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_PFS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PFS,'1','2')) AND ( .NOT. EMPTY(PFS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_ADE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADE,'1','2')) AND ( .NOT. EMPTY(ADE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_APLI_AGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APLI_AGR,'1','2')) AND ( .NOT. EMPTY(APLI_AGR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_APL_DOM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_DOM,'1','2')) AND ( .NOT. EMPTY(APL_DOM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_APL_SP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_SP,'1','2')) AND ( .NOT. EMPTY(APL_SP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_USO_INDUST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(USO_INDUST,'1','2')) AND ( .NOT. EMPTY(USO_INDUST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_TRA_HUM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRA_HUM,'1','2')) AND ( .NOT. EMPTY(TRA_HUM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_TRA_VET
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRA_VET,'1','2')) AND ( .NOT. EMPTY(TRA_VET)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_ACT_SOCIAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_SOCIAL,'1','2')) AND ( .NOT. EMPTY(ACT_SOCIAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_DESCONOCID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESCONOCID,'1','2')) AND ( .NOT. EMPTY(DESCONOCID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_OTROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS,'1','2')) AND ( .NOT. EMPTY(OTROS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_FEC_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_EXP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_HOR_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(HOR_EXP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_VIA_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIA_EXP,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(VIA_EXP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_ESCOLARIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESCOLARIDA,'1','2','3','4')) AND ( .NOT. EMPTY(ESCOLARIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_EMBARAZADA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EMBARAZADA,'1','2')) OR ( EMPTY(EMBARAZADA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_AFI_ARP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AFI_ARP,'1','2')) AND ( .NOT. EMPTY(AFI_ARP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_EST_CIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_CIV,'1','2','3','4','5')) AND ( .NOT. EMPTY(EST_CIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_PARTE_BROT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARTE_BROT,'1','2')) AND ( .NOT. EMPTY(PARTE_BROT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_INV_EPI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INV_EPI,'1','2')) OR ( EMPTY(INV_EPI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_02_SIT_ALE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_ALE,'1','2')) AND ( .NOT. EMPTY(SIT_ALE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_CASO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CASO,'1','2','3')) AND ( .NOT. EMPTY(CASO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_CLA_CLINIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLA_CLINIC,'1','2')) AND ( .NOT. EMPTY(CLA_CLINIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_BACILOSCOP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BACILOSCOP,'1','2')) AND ( .NOT. EMPTY(BACILOSCOP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_RES_BACILO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(RES_BACILO)>=0.0  AND  VAL(RES_BACILO) <=6.0 AND ISNUMERIC(RES_BACILO)) OR ( EMPTY(RES_BACILO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_BIOPSIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BIOPSIA,'1','2')) AND ( .NOT. EMPTY(BIOPSIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_RES_BIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_BIO,'1','2','3','4','5','6')) OR ( EMPTY(RES_BIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_MAX_GRA_DI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAX_GRA_DI,'0','1','2')) AND ( .NOT. EMPTY(MAX_GRA_DI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_PRE_REA_LE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRE_REA_LE,'1','2','3')) AND ( .NOT. EMPTY(PRE_REA_LE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_NUM_LESION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(NUM_LESION)>=1  AND  VAL(NUM_LESION) <=99 AND ISNUMERIC(NUM_LESION)) AND ( .NOT. EMPTY(NUM_LESION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_03_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_FLA_MSD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FLA_MSD,'1','2')) OR ( EMPTY(FLA_MSD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_PAR_MSI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAR_MSI,'1','2')) OR ( EMPTY(PAR_MSI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_PARA_MSI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARA_MSI,'1','2')) OR ( EMPTY(PARA_MSI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_FLA_MSI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FLA_MSI,'1','2')) OR ( EMPTY(FLA_MSI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_PAR_MID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAR_MID,'1','2')) OR ( EMPTY(PAR_MID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_PARA_MID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARA_MID,'1','2')) OR ( EMPTY(PARA_MID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_FLA_MID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FLA_MID,'1','2')) OR ( EMPTY(FLA_MID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_PAR_MII
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAR_MII,'1','2')) OR ( EMPTY(PAR_MII)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_PARA_MII
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARA_MII,'1','2')) OR ( EMPTY(PARA_MII)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_FLA_MII
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FLA_MII,'1','2')) OR ( EMPTY(FLA_MII)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_LOC_MSD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LOC_MSD,'1','2')) OR ( EMPTY(LOC_MSD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_SEN_MSD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEN_MSD,'1','2','3')) OR ( EMPTY(SEN_MSD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_ROT_MSD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ROT_MSD,'1','2','3')) OR ( EMPTY(ROT_MSD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_LOC_MSI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LOC_MSI,'1','2')) OR ( EMPTY(LOC_MSI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_SEN_MSI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEN_MSI,'1','2','3')) OR ( EMPTY(SEN_MSI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_ROT_MSI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ROT_MSI,'1','2','3')) OR ( EMPTY(ROT_MSI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_LOC_MID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LOC_MID,'1','2')) OR ( EMPTY(LOC_MID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_SEN_MID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEN_MID,'1','2','3')) OR ( EMPTY(SEN_MID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_ROT_MID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ROT_MID,'1','2','3')) OR ( EMPTY(ROT_MID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_LOC_MII
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LOC_MII,'1','2')) OR ( EMPTY(LOC_MII)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_DOS_REC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DOS_REC)>=0  AND  VAL(DOS_REC) <=9 AND ISNUMERIC(DOS_REC)) AND ( .NOT. EMPTY(DOS_REC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_DOS_VIP_RE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DOS_VIP_RE)>=0  AND  VAL(DOS_VIP_RE) <=9 AND ISNUMERIC(DOS_VIP_RE)) AND ( .NOT. EMPTY(DOS_VIP_RE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_TIE_CAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIE_CAR,'1','2','3')) AND ( .NOT. EMPTY(TIE_CAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2','3')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_RESPIRATOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESPIRATOR,'1','2','3')) AND ( .NOT. EMPTY(RESPIRATOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_DIGESTIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIGESTIVOS,'1','2','3')) AND ( .NOT. EMPTY(DIGESTIVOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_DOL_MUS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOL_MUS,'1','2','3')) AND ( .NOT. EMPTY(DOL_MUS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_SIG_MEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIG_MEN,'1','2','3')) AND ( .NOT. EMPTY(SIG_MEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_FIE_INI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIE_INI,'1','2','3')) AND ( .NOT. EMPTY(FIE_INI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_PROGRESION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROGRESION,'1','2','3')) AND ( .NOT. EMPTY(PROGRESION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_INI_PAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(INI_PAR))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_PAR_MSD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAR_MSD,'1','2')) OR ( EMPTY(PAR_MSD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_PARA_MSD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARA_MSD,'1','2')) OR ( EMPTY(PARA_MSD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_SEN_MII
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEN_MII,'1','2','3')) OR ( EMPTY(SEN_MII)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_ROT_MII
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ROT_MII,'1','2','3')) OR ( EMPTY(ROT_MII)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_CMUS_RES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CMUS_RES,'1','2','3')) AND ( .NOT. EMPTY(CMUS_RES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_CSIG_MEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CSIG_MEN,'1','2','3')) AND ( .NOT. EMPTY(CSIG_MEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_COM_BAB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_BAB,'1','2','3')) AND ( .NOT. EMPTY(COM_BAB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_COM_BRU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_BRU,'1','2','3')) AND ( .NOT. EMPTY(COM_BRU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_CPAR_CRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CPAR_CRA,'1','2','3')) AND ( .NOT. EMPTY(CPAR_CRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_TOM_LIQ_CE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOM_LIQ_CE,'1','2','3')) OR ( EMPTY(TOM_LIQ_CE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_TELE_MIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TELE_MIO,'1','2','3')) OR ( EMPTY(TELE_MIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_VEL_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VEL_CON,'1','2','3')) OR ( EMPTY(VEL_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_MAT_FEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAT_FEC,'1','2','3')) OR ( EMPTY(MAT_FEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_RES_MF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_MF,'1','2','3','4','5','6','7','8','9','10','11','12','13')) OR ( EMPTY(RES_MF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_CAS_DETX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAS_DETX,'1','2','3','4','5','6','7','8')) OR ( EMPTY(CAS_DETX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_PAR_RES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAR_RES,'1','2','3')) OR ( EMPTY(PAR_RES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_ATROFIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATROFIA,'1','2','3')) OR ( EMPTY(ATROFIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_CLA_FIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLA_FIN,'1','2','3','4','5')) OR ( EMPTY(CLA_FIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_CRI_CLA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRI_CLA,'1','2','3','4','5','6')) OR ( EMPTY(CRI_CLA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_04_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_TIP_AGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_AGR,'1','2','3','6','7','8')) AND ( .NOT. EMPTY(TIP_AGR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_AREA_MORDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AREA_MORDE,'1','2')) OR ( EMPTY(AREA_MORDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_AGR_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGR_PRO,'1','2')) AND ( .NOT. EMPTY(AGR_PRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_TIP_LES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_LES,'1','2')) AND ( .NOT. EMPTY(TIP_LES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_PROFUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROFUN,'1','2')) AND ( .NOT. EMPTY(PROFUN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_CCC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CCC,'1','2')) AND ( .NOT. EMPTY(CCC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_MAN_DED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAN_DED,'1','2')) AND ( .NOT. EMPTY(MAN_DED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_TRONCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRONCO,'1','2')) AND ( .NOT. EMPTY(TRONCO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_MIE_SUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIE_SUP,'1','2')) AND ( .NOT. EMPTY(MIE_SUP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_MIE_INF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIE_INF,'1','2')) AND ( .NOT. EMPTY(MIE_INF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_PIES_DEDOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIES_DEDOS,'1','2')) AND ( .NOT. EMPTY(PIES_DEDOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_GENIT_EXT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GENIT_EXT,'1','2')) AND ( .NOT. EMPTY(GENIT_EXT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_FEC_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_EXP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_ESP_ANI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_ANI,'1','2','3','4','5','7','8','9','10','12','13','14')) AND ( .NOT. EMPTY(ESP_ANI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_ANT_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_VAC,'1','2','3')) OR ( EMPTY(ANT_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_CAR_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAR_VAC,'1','2')) OR ( EMPTY(CAR_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_EST_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_MA,'1','2','3')) OR ( EMPTY(EST_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_ESTADO_ANI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADO_ANI,'1','2','3')) OR ( EMPTY(ESTADO_ANI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_UBICACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBICACION,'1','2')) OR ( EMPTY(UBICACION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_TIP_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_EXP,'0','1','2')) AND ( .NOT. EMPTY(TIP_EXP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_SUE_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUE_ANT,'1','2','3')) AND ( .NOT. EMPTY(SUE_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_VAC_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_ANT,'1','2','3')) AND ( .NOT. EMPTY(VAC_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_LE_AGU_JAB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LE_AGU_JAB,'1','2')) AND ( .NOT. EMPTY(LE_AGU_JAB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_SUT_HER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUT_HER,'1','2')) AND ( .NOT. EMPTY(SUT_HER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_APL_SA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_SA,'1','2')) AND ( .NOT. EMPTY(APL_SA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_APL_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_VAC,'1','2')) AND ( .NOT. EMPTY(APL_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_CAS_DETX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAS_DETX,'1','2','3','4','5','6','7','8')) AND ( .NOT. EMPTY(CAS_DETX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_VAC_SAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_SAR,'1','2','3')) AND ( .NOT. EMPTY(VAC_SAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_FTE_SAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FTE_SAR,'1','2','3')) OR ( EMPTY(FTE_SAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_TIP_VAC_SA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_VAC_SA,'1','2','3')) OR ( EMPTY(TIP_VAC_SA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_VAC_RUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_RUB,'1','2','3')) AND ( .NOT. EMPTY(VAC_RUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_FTE_RUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FTE_RUB,'1','2','3')) OR ( EMPTY(FTE_RUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_TIP_VAC_RU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_VAC_RU,'1','2','3')) OR ( EMPTY(TIP_VAC_RU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_DX_INI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DX_INI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_TIP_ERU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_ERU,'1','2','3','4')) AND ( .NOT. EMPTY(TIP_ERU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_FINI_ERU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FINI_ERU))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_DUR_DIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DUR_DIA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_TOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOS,'1','2','3')) AND ( .NOT. EMPTY(TOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_CORIZA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CORIZA,'1','2','3')) AND ( .NOT. EMPTY(CORIZA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_CONJUNTIVI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONJUNTIVI,'1','2','3')) AND ( .NOT. EMPTY(CONJUNTIVI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_ADENOPATIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADENOPATIA,'1','2','3')) AND ( .NOT. EMPTY(ADENOPATIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_ARTRALGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRALGIA,'1','2','3')) AND ( .NOT. EMPTY(ARTRALGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_SAR_RUB_7
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SAR_RUB_7,'1','2','3','4','5')) AND ( .NOT. EMPTY(SAR_RUB_7)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_SAR_RUB_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SAR_RUB_A,'1','2','3','4','5')) AND ( .NOT. EMPTY(SAR_RUB_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_VIAJO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIAJO,'1','2','3')) AND ( .NOT. EMPTY(VIAJO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_CON_MUJ_E
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_MUJ_E,'1','2','3')) AND ( .NOT. EMPTY(CON_MUJ_E)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_VAC_BLOQ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_BLOQ,'1','2','3')) OR ( EMPTY(VAC_BLOQ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_MON_RAP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MON_RAP,'1','2','3')) OR ( EMPTY(MON_RAP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_SEG_CONT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEG_CONT,'1','2','3')) OR ( EMPTY(SEG_CONT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_FTE_INFECC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FTE_INFECC,'1','2','3','4')) OR ( EMPTY(FTE_INFECC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_CRI_DES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRI_DES,'1','2','3','4','5','6','7')) OR ( EMPTY(CRI_DES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_FTE_CONTAG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FTE_CONTAG,'1','2','3','88','99')) AND ( .NOT. EMPTY(FTE_CONTAG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_SOSPE_MISC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SOSPE_MISC,'1','2')) AND ( .NOT. EMPTY(SOSPE_MISC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_RT_PCR_POS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RT_PCR_POS,'1','2')) OR ( EMPTY(RT_PCR_POS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_ACIGMIGG_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACIGMIGG_P,'1','2')) OR ( EMPTY(ACIGMIGG_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_NEXO_POS_C
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NEXO_POS_C,'1','2')) OR ( EMPTY(NEXO_POS_C)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) OR ( EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_EDEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EDEMA,'1','2')) OR ( EMPTY(EDEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_ALT_CONCIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALT_CONCIE,'1','2')) OR ( EMPTY(ALT_CONCIE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_FIBRIOGENO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((FIBRIOGENO>=0  AND FIBRIOGENO <=1000 AND ISNUMERIC(FIBRIOGENO)) OR (FIBRIOGENO=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_PROT_C_REA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PROT_C_REA>=0  AND PROT_C_REA <=1000 AND ISNUMERIC(PROT_C_REA)) OR (PROT_C_REA=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_FERRITINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((FERRITINA>=0  AND FERRITINA <=5000 AND ISNUMERIC(FERRITINA)) OR (FERRITINA=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_DÍMERO_D
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((DÍMERO_D>=0  AND DÍMERO_D <=20000 AND ISNUMERIC(DÍMERO_D)) OR (DÍMERO_D=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_LINFOPENIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((LINFOPENIA>=0  AND LINFOPENIA <=4999 AND ISNUMERIC(LINFOPENIA)) OR (LINFOPENIA=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_06_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) OR ( EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_MEC_PRO_T
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MEC_PRO_T,'1','2','3','4','5','6','7','9','10','11','12','13','14')) AND ( .NOT. EMPTY(MEC_PRO_T)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_TIP_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_MA,'RC','TI','CC','CE','PA','MS','AS','PE')) OR ( EMPTY(TIP_IDE_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_IDE_GENERO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IDE_GENERO,'M','F','T')) AND ( .NOT. EMPTY(IDE_GENERO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_DONO_SANGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DONO_SANGR,'1','2')) AND ( .NOT. EMPTY(DONO_SANGR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_TIP_PRU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_PRU,'1','2','3','4')) OR ( EMPTY(TIP_PRU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_FEC_RES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_RES))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_EST_CLI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_CLI,'1','2','3')) AND ( .NOT. EMPTY(EST_CLI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_CAN_ESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAN_ESO,'1','2')) AND ( .NOT. EMPTY(CAN_ESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_CAN_VA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAN_VA,'1','2')) AND ( .NOT. EMPTY(CAN_VA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_TUB_PUL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TUB_PUL,'1','2')) AND ( .NOT. EMPTY(TUB_PUL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_CAN_CER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAN_CER,'1','2')) AND ( .NOT. EMPTY(CAN_CER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_TUB_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TUB_EXP,'1','2')) AND ( .NOT. EMPTY(TUB_EXP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_COCCIDIODO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COCCIDIODO,'1','2')) AND ( .NOT. EMPTY(COCCIDIODO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_CITOMEGALO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CITOMEGALO,'1','2')) AND ( .NOT. EMPTY(CITOMEGALO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_REN_CIT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REN_CIT,'1','2')) AND ( .NOT. EMPTY(REN_CIT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_ENCEFALOPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENCEFALOPA,'1','2')) AND ( .NOT. EMPTY(ENCEFALOPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_OTRAS_MICR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRAS_MICR,'1','2')) AND ( .NOT. EMPTY(OTRAS_MICR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_HIS_EXT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIS_EXT,'1','2')) AND ( .NOT. EMPTY(HIS_EXT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_ISO_CRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ISO_CRO,'1','2')) AND ( .NOT. EMPTY(ISO_CRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_ERP_ZOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ERP_ZOS,'1','2')) AND ( .NOT. EMPTY(ERP_ZOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_HIS_DIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIS_DIS,'1','2')) AND ( .NOT. EMPTY(HIS_DIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_LIN_BUR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LIN_BUR,'1','2')) AND ( .NOT. EMPTY(LIN_BUR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_NEU_PNE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NEU_PNE,'1','2')) AND ( .NOT. EMPTY(NEU_PNE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_NEU_REC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NEU_REC,'1','2')) AND ( .NOT. EMPTY(NEU_REC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_LIN_INM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LIN_INM,'1','2')) AND ( .NOT. EMPTY(LIN_INM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_CRI_CRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRI_CRO,'1','2')) AND ( .NOT. EMPTY(CRI_CRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_CRI_EXT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRI_EXT,'1','2')) AND ( .NOT. EMPTY(CRI_EXT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_SAR_KAP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SAR_KAP,'1','2')) AND ( .NOT. EMPTY(SAR_KAP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_SIN_EMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIN_EMA,'1','2')) AND ( .NOT. EMPTY(SIN_EMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_LEU_MUL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LEU_MUL,'1','2')) AND ( .NOT. EMPTY(LEU_MUL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_SEP_REC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEP_REC,'1','2')) AND ( .NOT. EMPTY(SEP_REC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_TOX_CER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOX_CER,'1','2')) AND ( .NOT. EMPTY(TOX_CER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_HEP_B
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEP_B,'1','2')) AND ( .NOT. EMPTY(HEP_B)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_HEP_C
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEP_C,'1','2')) AND ( .NOT. EMPTY(HEP_C)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_MENINGITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MENINGITIS,'1','2')) AND ( .NOT. EMPTY(MENINGITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_07_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_FEC_ACC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_ACC))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_ACT_REA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_REA,'1','2','3','5','6','8','7')) AND ( .NOT. EMPTY(ACT_REA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_TIP_ATEN_I
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_ATEN_I,'1','2','3','4','5','6','9','7')) AND ( .NOT. EMPTY(TIP_ATEN_I)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_PRAC_NOMED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRAC_NOMED,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(PRAC_NOMED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_LOC_MOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LOC_MOR,'1','2','3','4','5','6','7','9','10','11','12')) AND ( .NOT. EMPTY(LOC_MOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_HUE_COL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HUE_COL,'1','2')) AND ( .NOT. EMPTY(HUE_COL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_SER_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SER_IDE,'1','2')) AND ( .NOT. EMPTY(SER_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_SER_CAP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SER_CAP,'1','2')) AND ( .NOT. EMPTY(SER_CAP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_AGE_AGRG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGE_AGRG,'1','2','3','4','7','8','9','6')) AND ( .NOT. EMPTY(AGE_AGRG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_AGE_AGRN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGE_AGRN,'1','2','3','4','5','6','7','8','9','10','11','12','13','14','16','17','15')) AND ( .NOT. EMPTY(AGE_AGRN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_EDEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EDEMA,'1','2')) AND ( .NOT. EMPTY(EDEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_DOLOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR,'1','2')) AND ( .NOT. EMPTY(DOLOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_ERITEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ERITEMA,'1','2')) AND ( .NOT. EMPTY(ERITEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_FLICTENAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FLICTENAS,'1','2')) AND ( .NOT. EMPTY(FLICTENAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_PARESTESIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARESTESIA,'1','2')) AND ( .NOT. EMPTY(PARESTESIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_EQUIMOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EQUIMOSIS,'1','2')) AND ( .NOT. EMPTY(EQUIMOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_HEMATOMAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMATOMAS,'1','2')) AND ( .NOT. EMPTY(HEMATOMAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_OTRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRO,'1','2')) AND ( .NOT. EMPTY(OTRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_NAUSEAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NAUSEAS,'1','2')) AND ( .NOT. EMPTY(NAUSEAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) AND ( .NOT. EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_SIALORREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIALORREA,'1','2')) AND ( .NOT. EMPTY(SIALORREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) AND ( .NOT. EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_BRADICARDI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BRADICARDI,'1','2')) AND ( .NOT. EMPTY(BRADICARDI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_HIPOTENSIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPOTENSIO,'1','2')) AND ( .NOT. EMPTY(HIPOTENSIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_DOL_ABD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOL_ABD,'1','2')) AND ( .NOT. EMPTY(DOL_ABD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_FAS_NEU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FAS_NEU,'1','2')) AND ( .NOT. EMPTY(FAS_NEU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_ALT_VIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALT_VIS,'1','2')) AND ( .NOT. EMPTY(ALT_VIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_ALT_SEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALT_SEN,'1','2')) AND ( .NOT. EMPTY(ALT_SEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_DEB_MUS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEB_MUS,'1','2')) AND ( .NOT. EMPTY(DEB_MUS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_OLIGURIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OLIGURIA,'1','2')) AND ( .NOT. EMPTY(OLIGURIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_CIANOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CIANOSIS,'1','2')) AND ( .NOT. EMPTY(CIANOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_EPISTAXIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPISTAXIS,'1','2')) AND ( .NOT. EMPTY(EPISTAXIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_GINGIVORRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GINGIVORRA,'1','2')) AND ( .NOT. EMPTY(GINGIVORRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_HEMATEMESI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMATEMESI,'1','2')) AND ( .NOT. EMPTY(HEMATEMESI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_HEMATURIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMATURIA,'1','2')) AND ( .NOT. EMPTY(HEMATURIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_HEMATOQUEX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMATOQUEX,'1','2')) AND ( .NOT. EMPTY(HEMATOQUEX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_VERTIGO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VERTIGO,'1','2')) AND ( .NOT. EMPTY(VERTIGO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_PTO_PALPEB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PTO_PALPEB,'1','2')) AND ( .NOT. EMPTY(PTO_PALPEB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_DIF_HABLAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIF_HABLAR,'1','2')) AND ( .NOT. EMPTY(DIF_HABLAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_DISFAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISFAGIA,'1','2')) AND ( .NOT. EMPTY(DISFAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_OTRO_MS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRO_MS,'1','2')) AND ( .NOT. EMPTY(OTRO_MS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_CELULITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CELULITIS,'1','2')) AND ( .NOT. EMPTY(CELULITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_ABSCESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ABSCESO,'1','2')) AND ( .NOT. EMPTY(ABSCESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_NECROSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NECROSIS,'1','2')) AND ( .NOT. EMPTY(NECROSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_MIONECROSI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIONECROSI,'1','2')) AND ( .NOT. EMPTY(MIONECROSI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_FASCEITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FASCEITIS,'1','2')) AND ( .NOT. EMPTY(FASCEITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_ALTER_CIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALTER_CIR,'1','2')) AND ( .NOT. EMPTY(ALTER_CIR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_OTRO_CL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRO_CL,'1','2')) AND ( .NOT. EMPTY(OTRO_CL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_ANE_AGU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANE_AGU,'1','2')) AND ( .NOT. EMPTY(ANE_AGU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_SHO_HIP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SHO_HIP,'1','2')) AND ( .NOT. EMPTY(SHO_HIP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_SHO_SEP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SHO_SEP,'1','2')) AND ( .NOT. EMPTY(SHO_SEP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_IRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IRA,'1','2')) AND ( .NOT. EMPTY(IRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_CID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CID,'1','2')) AND ( .NOT. EMPTY(CID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_HSA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HSA,'1','2')) AND ( .NOT. EMPTY(HSA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_EDEMA_CEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EDEMA_CEL,'1','2')) AND ( .NOT. EMPTY(EDEMA_CEL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_FALLA_VENT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_VENT,'1','2')) AND ( .NOT. EMPTY(FALLA_VENT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_COMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMA,'1','2')) AND ( .NOT. EMPTY(COMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_OTRA_COM_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRA_COM_S,'1','2')) AND ( .NOT. EMPTY(OTRA_COM_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_SEV_ACC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEV_ACC,'1','2','3','4')) AND ( .NOT. EMPTY(SEV_ACC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_EMP_SUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EMP_SUE,'1','2')) AND ( .NOT. EMPTY(EMP_SUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_TIP_SA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_SA,'1','3')) OR ( EMPTY(TIP_SA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_REACC_APL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REACC_APL,'1','2','3')) OR ( EMPTY(REACC_APL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_REMITIDO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REMITIDO,'1','2')) AND ( .NOT. EMPTY(REMITIDO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_TTO_QX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TTO_QX,'1','2')) AND ( .NOT. EMPTY(TTO_QX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_TIP_TTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_TTO,'1','2','3','4','5','6')) OR ( EMPTY(TIP_TTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_SIND_COMPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIND_COMPA,'1','2')) AND ( .NOT. EMPTY(SIND_COMPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_08_FABRI_SUER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FABRI_SUER,'1','2','3','4')) OR ( EMPTY(FABRI_SUER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_TIP_PAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_PAR,'1','2','3','4','5')) OR ( EMPTY(TIP_PAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_SIT_DEF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_DEF,'6','8','9','10','11','12','13','7')) AND ( .NOT. EMPTY(SIT_DEF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_GESTACIONE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GESTACIONE)>=1  AND  VAL(GESTACIONE) <= 25 AND ISNUMERIC(GESTACIONE)) AND ( .NOT. EMPTY(GESTACIONE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_PARTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PARTOS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_CESAREAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CESAREAS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_ABORTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ABORTOS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_MUERTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(MUERTOS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(VIVOS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_NUM_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(NUM_CON)>=0  AND  VAL(NUM_CON) <=25 AND ISNUMERIC(NUM_CON)) AND ( .NOT. EMPTY(NUM_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_SI_CTRL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SI_CTRL)>= 1  AND  VAL(SI_CTRL) <= 45 AND ISNUMERIC(SI_CTRL)) OR ( EMPTY(SI_CTRL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_PAR_ATE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAR_ATE,'1','2','3','4','5','6','7','8')) OR ( EMPTY(PAR_ATE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_SITIO_PART
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SITIO_PART,'1','2','3')) OR ( EMPTY(SITIO_PART)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_NIV_ATE_PA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NIV_ATE_PA,'1','2','3')) OR ( EMPTY(NIV_ATE_PA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_MOM_OCU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MOM_OCU,'1','2','3','5','6','7')) AND ( .NOT. EMPTY(MOM_OCU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_EDA_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDA_GES)>= 20  AND  VAL(EDA_GES) <= 45 AND ISNUMERIC(EDA_GES)) AND ( .NOT. EMPTY(EDA_GES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_EDAD_NEO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD_NEO)>=0  AND  VAL(EDAD_NEO) <=28 AND ISNUMERIC(EDAD_NEO)) OR ( EMPTY(EDAD_NEO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PESO)>= 300  AND  VAL(PESO) <= 6000 AND ISNUMERIC(PESO)) AND ( .NOT. EMPTY(PESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_TALLA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TALLA)>=18  AND  VAL(TALLA) <=53 AND ISNUMERIC(TALLA)) AND ( .NOT. EMPTY(TALLA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_SEXO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEXO,'M','F','I')) AND ( .NOT. EMPTY(SEXO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_CAU_MTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAU_MTE,'1','2','3')) AND ( .NOT. EMPTY(CAU_MTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_09_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_CLA_INI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLA_INI,'1','2')) AND ( .NOT. EMPTY(CLA_INI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_MUN_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(MUN_NAC))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_FUE_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FUE_NOT,'1','2','3','4','5','8','9')) AND ( .NOT. EMPTY(FUE_NOT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_NOM_MAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_MAD))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_EDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD)>=6 AND ISNUMERIC(EDAD)) AND ( .NOT. EMPTY(EDAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_NUM_EMB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(NUM_EMB)>= 1  AND  VAL(NUM_EMB) <=25 AND ISNUMERIC(NUM_EMB)) AND ( .NOT. EMPTY(NUM_EMB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_VIAJES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIAJES,'1','2','9')) AND ( .NOT. EMPTY(VIAJES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_SE_VIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SE_VIA)>= 1  AND  VAL(SE_VIA) <= 45 AND ISNUMERIC(SE_VIA)) OR ( EMPTY(SE_VIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_APGAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(APGAR)>= 1  AND  VAL(APGAR) <= 10 AND ISNUMERIC(APGAR)) AND ( .NOT. EMPTY(APGAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_BPN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BPN,'1','2','9')) AND ( .NOT. EMPTY(BPN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PESO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_PEQ_EDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEQ_EDA,'1','2','9')) AND ( .NOT. EMPTY(PEQ_EDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_SEMANAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SEMANAS)>= 20  AND  VAL(SEMANAS) <= 45 AND ISNUMERIC(SEMANAS)) OR ( EMPTY(SEMANAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_CATARATAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CATARATAS,'1','2','9')) AND ( .NOT. EMPTY(CATARATAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_GLAUCOMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GLAUCOMA,'1','2','9')) AND ( .NOT. EMPTY(GLAUCOMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_RETINOPATI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RETINOPATI,'1','2','9')) AND ( .NOT. EMPTY(RETINOPATI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_OTR_OJO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTR_OJO,'1','2','9')) AND ( .NOT. EMPTY(OTR_OJO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_PER_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PER_CON,'1','2','9')) AND ( .NOT. EMPTY(PER_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_EST_ART
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_ART,'1','2','9')) AND ( .NOT. EMPTY(EST_ART)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_OTR_COR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTR_COR,'1','2','9')) AND ( .NOT. EMPTY(OTR_COR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_SORDERA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SORDERA,'1','2','9')) AND ( .NOT. EMPTY(SORDERA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_OTR_OID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTR_OID,'1','2','9')) AND ( .NOT. EMPTY(OTR_OID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_MICROCEFAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MICROCEFAL,'1','2','9')) AND ( .NOT. EMPTY(MICROCEFAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_RET_DES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RET_DES,'1','2','9')) AND ( .NOT. EMPTY(RET_DES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_PURPURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PURPURA,'1','2','9')) AND ( .NOT. EMPTY(PURPURA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_HEPATOMEGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATOMEGA,'1','2','9')) AND ( .NOT. EMPTY(HEPATOMEGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_ICT_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ICT_NAC,'1','2','9')) AND ( .NOT. EMPTY(ICT_NAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_ESPLENOMEG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESPLENOMEG,'1','2','9')) AND ( .NOT. EMPTY(ESPLENOMEG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_OST_RAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OST_RAD,'1','2','9')) AND ( .NOT. EMPTY(OST_RAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_MENINGOENC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MENINGOENC,'1','2','9')) AND ( .NOT. EMPTY(MENINGOENC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_OTRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRO,'1','2','9')) AND ( .NOT. EMPTY(OTRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_DX_FIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DX_FIN,'1','2','9')) AND ( .NOT. EMPTY(DX_FIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_10_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_CONDENDIAG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONDENDIAG,'1','2','3','4')) AND ( .NOT. EMPTY(CONDENDIAG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_CON_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_PRE,'1','2')) AND ( .NOT. EMPTY(CON_PRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_EDA_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDA_GES)>= 2  AND  VAL(EDA_GES) <= 42 AND ISNUMERIC(EDA_GES)) OR ( EMPTY(EDA_GES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_PR_NO_TREP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PR_NO_TREP,'1','2')) AND ( .NOT. EMPTY(PR_NO_TREP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_EG_PRI_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EG_PRI_S)>= 2  AND  VAL(EG_PRI_S) <= 42 AND ISNUMERIC(EG_PRI_S)) OR ( EMPTY(EG_PRI_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_RESSERVDRL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESSERVDRL,'3','4','5','6','7','8','9','10','11','12','13')) OR ( EMPTY(RESSERVDRL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_PRUETREPON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRUETREPON,'1','2')) AND ( .NOT. EMPTY(PRUETREPON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_EG_PR_TREP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EG_PR_TREP)>= 2  AND  VAL(EG_PR_TREP) <= 42 AND ISNUMERIC(EG_PR_TREP)) OR ( EMPTY(EG_PR_TREP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_QUE_PR_TRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUE_PR_TRE,'4','5')) OR ( EMPTY(QUE_PR_TRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_RESULTREPO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULTREPO,'1','2')) OR ( EMPTY(RESULTREPO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_PEN_BENZAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEN_BENZAT,'0','1','2','3')) AND ( .NOT. EMPTY(PEN_BENZAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_TTO_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TTO_CON,'1','2')) AND ( .NOT. EMPTY(TTO_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_DIA_EMBACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIA_EMBACT,'1','2')) AND ( .NOT. EMPTY(DIA_EMBACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_11_TIEM_RESID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIEM_RESID,'1','2')) OR ( EMPTY(TIEM_RESID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_LUG_PAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LUG_PAR,'1','2')) AND ( .NOT. EMPTY(LUG_PAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_ATE_PAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATE_PAR,'1','2','3','4','5','6','7','8','9')) AND ( .NOT. EMPTY(ATE_PAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_APL_MUÑ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_MUÑ,'1','2')) AND ( .NOT. EMPTY(APL_MUÑ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_NOM_MAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_MAD))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_EDA_MAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(EDA_MAD))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_LLA_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LLA_NAC,'1','2')) AND ( .NOT. EMPTY(LLA_NAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_MAM_NOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAM_NOR,'1','2')) AND ( .NOT. EMPTY(MAM_NOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_DEJ_MAM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEJ_MAM,'1','2')) AND ( .NOT. EMPTY(DEJ_MAM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_HIPERTERMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPERTERMI,'1','2')) AND ( .NOT. EMPTY(HIPERTERMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_FON_ABO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FON_ABO,'1','2')) AND ( .NOT. EMPTY(FON_ABO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_RIG_NUC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RIG_NUC,'1','2')) AND ( .NOT. EMPTY(RIG_NUC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_TRISMUS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRISMUS,'1','2')) AND ( .NOT. EMPTY(TRISMUS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_CONVULSION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONVULSION,'1','2')) AND ( .NOT. EMPTY(CONVULSION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_ESPASMOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESPASMOS,'1','2')) AND ( .NOT. EMPTY(ESPASMOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_CONTRACCIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONTRACCIO,'1','2')) AND ( .NOT. EMPTY(CONTRACCIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_OPISTOTONO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OPISTOTONO,'1','2')) AND ( .NOT. EMPTY(OPISTOTONO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_LLA_EXC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LLA_EXC,'1','2')) AND ( .NOT. EMPTY(LLA_EXC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_SEP_UMB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEP_UMB,'1','2')) AND ( .NOT. EMPTY(SEP_UMB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_NUM_EMB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_EMB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_CON_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_PRE,'1','2')) AND ( .NOT. EMPTY(CON_PRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_ATE_MED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATE_MED,'1','2')) OR ( EMPTY(ATE_MED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_ATE_ENF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATE_ENF,'1','2')) OR ( EMPTY(ATE_ENF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_ATE_AUX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATE_AUX,'1','2')) OR ( EMPTY(ATE_AUX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_ATE_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATE_PRO,'1','2')) OR ( EMPTY(ATE_PRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_ATE_OTR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATE_OTR,'1','2')) OR ( EMPTY(ATE_OTR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_VIV_MIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIV_MIS,'1','2')) AND ( .NOT. EMPTY(VIV_MIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_VAC_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_ANT,'1','2')) AND ( .NOT. EMPTY(VAC_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_12_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_TIP_TUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_TUB,'1','2')) AND ( .NOT. EMPTY(TIP_TUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_LOCTBREXTR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LOCTBREXTR,'1','2','3','4','5','6','7','8','9','10','11','12')) OR ( EMPTY(LOCTBREXTR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_CLASCASO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLASCASO,'1','2','3','4')) AND ( .NOT. EMPTY(CLASCASO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_CIC_VCG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CIC_VCG,'1','2')) AND ( .NOT. EMPTY(CIC_VCG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_VCNBCGCN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VCNBCGCN,'1','2')) AND ( .NOT. EMPTY(VCNBCGCN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_EMBARAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EMBARAZO,'1','2','3')) OR ( EMPTY(EMBARAZO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_CONSPREVIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONSPREVIH,'1','2')) AND ( .NOT. EMPTY(CONSPREVIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_PRUEBDIAGN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRUEBDIAGN,'1','2')) AND ( .NOT. EMPTY(PRUEBDIAGN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_ASO_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASO_VIH,'1','2','3')) AND ( .NOT. EMPTY(ASO_VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_PESO_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_ACT>=2.0  AND PESO_ACT <=250.0 AND ISNUMERIC(PESO_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_TALLA_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA_ACT>=0.20  AND TALLA_ACT <=2.5 AND ISNUMERIC(TALLA_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_INI_TRAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INI_TRAT,'1','2')) AND ( .NOT. EMPTY(INI_TRAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_BACILOSCOP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BACILOSCOP,'1','2')) AND ( .NOT. EMPTY(BACILOSCOP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_RES_BK
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_BK,'1','2','3','4')) OR ( EMPTY(RES_BK)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_REA_CUL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REA_CUL,'1','2')) AND ( .NOT. EMPTY(REA_CUL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_RESCULTIVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESCULTIVO,'1','2')) OR ( EMPTY(RESCULTIVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_RES_CULT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_CULT,'1','2','3','4','5','6','7')) OR ( EMPTY(RES_CULT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_HISTOPATOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HISTOPATOL,'1','2')) AND ( .NOT. EMPTY(HISTOPATOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_RESHISTOPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESHISTOPA,'1','2')) OR ( EMPTY(RESHISTOPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_CLI_PTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLI_PTA,'1','2')) AND ( .NOT. EMPTY(CLI_PTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_NEX_EPI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NEX_EPI,'1','2')) AND ( .NOT. EMPTY(NEX_EPI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_REDIOLOGIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REDIOLOGIC,'1','2')) AND ( .NOT. EMPTY(REDIOLOGIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_ADA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADA,'1','2')) AND ( .NOT. EMPTY(ADA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_TUBERCULIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TUBERCULIN,'1','2')) AND ( .NOT. EMPTY(TUBERCULIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_MET_HALL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MET_HALL,'1','2','3')) AND ( .NOT. EMPTY(MET_HALL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_POSFTECNTC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(POSFTECNTC,'1','2','3')) AND ( .NOT. EMPTY(POSFTECNTC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_INVESCAMPO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INVESCAMPO,'1','2')) AND ( .NOT. EMPTY(INVESCAMPO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_13_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_CON_CC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_CC,'1','2','3')) AND ( .NOT. EMPTY(CON_CC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_CAR_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAR_VAC,'1','2')) AND ( .NOT. EMPTY(CAR_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_DOS_APLI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOS_APLI,'0','1','2','3','4','5')) AND ( .NOT. EMPTY(DOS_APLI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_TIP_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_VAC,'1','2','3','4')) OR ( EMPTY(TIP_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2','3')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_AMIGDALITI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AMIGDALITI,'1','2','3')) AND ( .NOT. EMPTY(AMIGDALITI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_FARINGITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FARINGITIS,'1','2','3')) AND ( .NOT. EMPTY(FARINGITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_LARINGITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LARINGITIS,'1','2','3')) AND ( .NOT. EMPTY(LARINGITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_PRE_MEM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRE_MEM,'1','2','3')) AND ( .NOT. EMPTY(PRE_MEM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_COMPLICACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMPLICACI,'1','2','3')) AND ( .NOT. EMPTY(COMPLICACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_TIP_COM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_COM,'1','2','3','4','5')) OR ( EMPTY(TIP_COM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_14_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_VACUNA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VACUNA,'1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','17','18','19','20','21','22','23','24','16')) AND ( .NOT. EMPTY(VACUNA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_DOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOSIS,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(DOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_VIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIA,'1','2','3','4')) AND ( .NOT. EMPTY(VIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_SITIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SITIO,'1','2','3','4','5','6','7','8','9')) AND ( .NOT. EMPTY(SITIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_FEC_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_VAC))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_FABRICANT1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FABRICANT1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_LOTE1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(LOTE1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_VACUNA2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VACUNA2,'1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','17','18','19','20','21','22','23','24','16')) OR ( EMPTY(VACUNA2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_DOSIS2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOSIS2,'1','2','3','4','5','6')) OR ( EMPTY(DOSIS2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_VIA2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIA2,'1','2','3','4')) OR ( EMPTY(VIA2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_SITIO2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SITIO2,'1','2','3','4','5','6','7','8','9')) OR ( EMPTY(SITIO2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_VACUNA3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VACUNA3,'1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','17','18','19','20','21','22','23','24','16')) OR ( EMPTY(VACUNA3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_DOSIS3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOSIS3,'1','2','3','4','5','6')) OR ( EMPTY(DOSIS3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_VIA3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIA3,'1','2','3','4')) OR ( EMPTY(VIA3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_SITIO3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SITIO3,'1','2','3','4','5','6','7','8','9')) OR ( EMPTY(SITIO3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_VACUNA4
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VACUNA4,'1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','17','18','19','20','21','22','23','24','16')) OR ( EMPTY(VACUNA4)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_DOSIS4
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOSIS4,'1','2','3','4','5','6')) OR ( EMPTY(DOSIS4)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_VIA4
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIA4,'1','2','3','4')) OR ( EMPTY(VIA4)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_SITIO4
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SITIO4,'1','2','3','4','5','6','7','8','9')) OR ( EMPTY(SITIO4)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_BECEGEITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BECEGEITIS,'1','2')) OR ( EMPTY(BECEGEITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_ABSCESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ABSCESO,'1','2')) AND ( .NOT. EMPTY(ABSCESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_LINFADENIT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LINFADENIT,'1','2')) AND ( .NOT. EMPTY(LINFADENIT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_CON_FEB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_FEB,'1','2')) AND ( .NOT. EMPTY(CON_FEB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_CON_SINF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_SINF,'1','2')) AND ( .NOT. EMPTY(CON_SINF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_HIPOTONÍA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPOTONÍA,'1','2')) AND ( .NOT. EMPTY(HIPOTONÍA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_PARESTESIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARESTESIA,'1','2')) AND ( .NOT. EMPTY(PARESTESIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_PARÁLISIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARÁLISIS,'1','2')) AND ( .NOT. EMPTY(PARÁLISIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_ENCEFALOPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENCEFALOPA,'1','2')) AND ( .NOT. EMPTY(ENCEFALOPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_MENINGITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MENINGITIS,'1','2')) AND ( .NOT. EMPTY(MENINGITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_URTICARIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(URTICARIA,'1','2')) AND ( .NOT. EMPTY(URTICARIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_ECZEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ECZEMA,'1','2')) AND ( .NOT. EMPTY(ECZEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_CHO_ANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CHO_ANA,'1','2')) AND ( .NOT. EMPTY(CHO_ANA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_GUI_BAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GUI_BAR,'1','2')) AND ( .NOT. EMPTY(GUI_BAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_CELULITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CELULITIS,'1','2')) AND ( .NOT. EMPTY(CELULITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_LLA_PER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LLA_PER,'1','2')) AND ( .NOT. EMPTY(LLA_PER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_TIE_TRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(TIE_TRA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_UN_TIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UN_TIE,'2','3','4','5')) AND ( .NOT. EMPTY(UN_TIE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_ANT_PAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_PAT,'1','2')) AND ( .NOT. EMPTY(ANT_PAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_ANT_ALE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_ALE,'1','2')) AND ( .NOT. EMPTY(ANT_ALE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_AASV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AASV,'1','2')) AND ( .NOT. EMPTY(AASV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_EST_FIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_FIN,'2','3')) OR ( EMPTY(EST_FIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_CLA_FINAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLA_FINAL,'2','3','4','5','6','7','8')) AND ( .NOT. EMPTY(CLA_FINAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_FATIGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FATIGA,'1','2')) OR ( EMPTY(FATIGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_DOLOR_CABE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR_CABE,'1','2')) OR ( EMPTY(DOLOR_CABE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_MIALGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIALGIA,'1','2')) OR ( EMPTY(MIALGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_ARTRALGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRALGIA,'1','2')) OR ( EMPTY(ARTRALGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_15_NAUSEAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NAUSEAS,'1','2')) OR ( EMPTY(NAUSEAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_CLAS_FINAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_FINAL,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(CLAS_FINAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_HIJ_MAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIJ_MAD,'1','2')) AND ( .NOT. EMPTY(HIJ_MAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_COM_SEX_IN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_SEX_IN,'1','2')) AND ( .NOT. EMPTY(COM_SEX_IN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_MULTITRANS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MULTITRANS,'1','2')) AND ( .NOT. EMPTY(MULTITRANS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_HEMODIALIZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMODIALIZ,'1','2')) AND ( .NOT. EMPTY(HEMODIALIZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_TRA_SAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRA_SAL,'1','2')) AND ( .NOT. EMPTY(TRA_SAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_DRO_PAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DRO_PAR,'1','2')) AND ( .NOT. EMPTY(DRO_PAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_CON_POR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_POR,'1','2')) AND ( .NOT. EMPTY(CON_POR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_CON_SEX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_SEX,'1','2')) AND ( .NOT. EMPTY(CON_SEX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_MET_TRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MET_TRA,'1','2','3','4')) AND ( .NOT. EMPTY(MET_TRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_DON_SAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DON_SAN,'1','2')) AND ( .NOT. EMPTY(DON_SAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_SEM_GES_IN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SEM_GES_IN)>=0  AND  VAL(SEM_GES_IN) <=45 AND ISNUMERIC(SEM_GES_IN)) OR ( EMPTY(SEM_GES_IN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_VAC_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_ANT,'1','2')) AND ( .NOT. EMPTY(VAC_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_FUENTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FUENTE,'1','2','3')) AND ( .NOT. EMPTY(FUENTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_PRES_SYS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRES_SYS,'1','2')) AND ( .NOT. EMPTY(PRES_SYS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_CUALES_COM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUALES_COM,'1','2','3','4','5')) AND ( .NOT. EMPTY(CUALES_COM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_TIP_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_MA,'RC','TI','CC','CE','PA','MS','AS','PE')) OR ( EMPTY(TIP_IDE_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_VAC_REC_NA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_REC_NA,'1','2','3','4','5')) OR ( EMPTY(VAC_REC_NA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_GAM_REC_NA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GAM_REC_NA,'1','2','3','4','5')) OR ( EMPTY(GAM_REC_NA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_HOMOSEXUAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HOMOSEXUAL,'1','2')) AND ( .NOT. EMPTY(HOMOSEXUAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_BISEXUAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BISEXUAL,'1','2')) AND ( .NOT. EMPTY(BISEXUAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_ACC_LABORA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACC_LABORA,'1','2')) AND ( .NOT. EMPTY(ACC_LABORA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_TRANS_ORGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRANS_ORGA,'1','2')) AND ( .NOT. EMPTY(TRANS_ORGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_PROCEDIMIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROCEDIMIE,'1','2')) AND ( .NOT. EMPTY(PROCEDIMIE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_ACUPUNTURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACUPUNTURA,'1','2')) AND ( .NOT. EMPTY(ACUPUNTURA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_COINF_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COINF_VIH,'1','2')) AND ( .NOT. EMPTY(COINF_VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_16_MOMDIAG_GE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MOMDIAG_GE,'1','2','3','4')) OR ( EMPTY(MOMDIAG_GE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_CARA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARA,'1','2')) OR ( EMPTY(CARA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_TRONCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRONCO,'1','2')) OR ( EMPTY(TRONCO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_MIE_SUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIE_SUP,'1','2')) OR ( EMPTY(MIE_SUP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_MIE_INF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIE_INF,'1','2')) OR ( EMPTY(MIE_INF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_MUC_AFE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUC_AFE,'1','2','3','4','5','6','7')) OR ( EMPTY(MUC_AFE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_RINORREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RINORREA,'1','2')) OR ( EMPTY(RINORREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_EPISTAXIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPISTAXIS,'1','2')) OR ( EMPTY(EPISTAXIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_ONASAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ONASAL,'1','2')) OR ( EMPTY(ONASAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_DISFONÍA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISFONÍA,'1','2')) OR ( EMPTY(DISFONÍA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_DISFAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISFAGIA,'1','2')) OR ( EMPTY(DISFAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_HIPEREMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPEREMIA,'1','2')) OR ( EMPTY(HIPEREMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_ULCERACIÓN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ULCERACIÓN,'1','2')) OR ( EMPTY(ULCERACIÓN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_PERFORACIÓ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PERFORACIÓ,'1','2')) OR ( EMPTY(PERFORACIÓ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_DESTRUCCIÓ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESTRUCCIÓ,'1','2')) OR ( EMPTY(DESTRUCCIÓ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) OR ( EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_HEPATOMEGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATOMEGA,'1','2')) OR ( EMPTY(HEPATOMEGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_ESPLENOMEG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESPLENOMEG,'1','2')) OR ( EMPTY(ESPLENOMEG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_ANEMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANEMIA,'1','2')) OR ( EMPTY(ANEMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_LEUCOPENIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LEUCOPENIA,'1','2')) OR ( EMPTY(LEUCOPENIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_TROMBOCITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TROMBOCITO,'1','2')) OR ( EMPTY(TROMBOCITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_COINFE_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COINFE_VIH,'1','2','3')) AND ( .NOT. EMPTY(COINFE_VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_REC_TTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REC_TTO,'1','2')) AND ( .NOT. EMPTY(REC_TTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_PES_PTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PES_PTE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_MED_FOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MED_FOR,'1','2','3','4','5','6','7','8')) AND ( .NOT. EMPTY(MED_FOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_DIA_TTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DIA_TTO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_TOT_AMP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TOT_AMP)>=0  AND  VAL(TOT_AMP) <=180 AND ISNUMERIC(TOT_AMP)) AND ( .NOT. EMPTY(TOT_AMP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_17_TTO_LOCAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TTO_LOCAL,'1','2')) OR ( EMPTY(TTO_LOCAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_VIAJO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIAJO,'1','2','3')) AND ( .NOT. EMPTY(VIAJO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_PAD_MAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAD_MAL,'1','2','3')) AND ( .NOT. EMPTY(PAD_MAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_AUTOMEDICA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AUTOMEDICA,'1','2','3')) AND ( .NOT. EMPTY(AUTOMEDICA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ANT_TRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_TRA,'1','2','3')) AND ( .NOT. EMPTY(ANT_TRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_CEREBR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_CEREBR,'1','2')) AND ( .NOT. EMPTY(COM_CEREBR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_RENAL,'1','2')) AND ( .NOT. EMPTY(COM_RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_HEPATI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_HEPATI,'1','2')) AND ( .NOT. EMPTY(COM_HEPATI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_PULMON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_PULMON,'1','2')) AND ( .NOT. EMPTY(COM_PULMON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_HEMATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_HEMATO,'1','2')) AND ( .NOT. EMPTY(COM_HEMATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_OTRAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_OTRAS,'1','2')) AND ( .NOT. EMPTY(COM_OTRAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ESCALOFRÍO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESCALOFRÍO,'1','2')) AND ( .NOT. EMPTY(ESCALOFRÍO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_SUDORACIÓN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUDORACIÓN,'1','2')) AND ( .NOT. EMPTY(SUDORACIÓN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_MIALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIALGIAS,'1','2')) AND ( .NOT. EMPTY(MIALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HIPEREMESI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPEREMESI,'1','2')) AND ( .NOT. EMPTY(HIPEREMESI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_NAUSEAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NAUSEAS,'1','2')) AND ( .NOT. EMPTY(NAUSEAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ASTENIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASTENIA,'1','2')) AND ( .NOT. EMPTY(ASTENIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ADINAMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADINAMIA,'1','2')) AND ( .NOT. EMPTY(ADINAMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HEMOGLOB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMOGLOB,'1','2')) AND ( .NOT. EMPTY(HEMOGLOB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_PLAQUETA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PLAQUETA,'1','2')) AND ( .NOT. EMPTY(PLAQUETA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HEMORRAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMORRAGIA,'1','2')) AND ( .NOT. EMPTY(HEMORRAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CID,'1','2')) AND ( .NOT. EMPTY(CID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HEPATOMEGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATOMEGA,'1','2')) AND ( .NOT. EMPTY(HEPATOMEGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ESPLENOMEG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESPLENOMEG,'1','2')) AND ( .NOT. EMPTY(ESPLENOMEG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_EDE_PUL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EDE_PUL,'1','2')) AND ( .NOT. EMPTY(EDE_PUL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HIPOTENSIÓ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPOTENSIÓ,'1','2')) AND ( .NOT. EMPTY(HIPOTENSIÓ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RENAL,'1','2')) AND ( .NOT. EMPTY(RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_RESPIRATOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESPIRATOR,'1','2')) AND ( .NOT. EMPTY(RESPIRATOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HEPÁTICA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPÁTICA,'1','2')) AND ( .NOT. EMPTY(HEPÁTICA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CONFUSIÓN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONFUSIÓN,'1','2')) AND ( .NOT. EMPTY(CONFUSIÓN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_SOMNOLENCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SOMNOLENCI,'1','2')) AND ( .NOT. EMPTY(SOMNOLENCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CONVULSIÓN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONVULSIÓN,'1','2')) AND ( .NOT. EMPTY(CONVULSIÓN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMA,'1','2')) AND ( .NOT. EMPTY(COMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CHOQUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CHOQUE,'1','2')) AND ( .NOT. EMPTY(CHOQUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ESP_PLA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_PLA,'1','2','3','4')) AND ( .NOT. EMPTY(ESP_PLA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_EMBARAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EMBARAZO,'1','2','3')) AND ( .NOT. EMPTY(EMBARAZO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CLOROQUINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLOROQUINA,'1','2','3')) AND ( .NOT. EMPTY(CLOROQUINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_AMODIAQUIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AMODIAQUIN,'1','2','3')) AND ( .NOT. EMPTY(AMODIAQUIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_SULFAPIRIM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SULFAPIRIM,'1','2','3')) AND ( .NOT. EMPTY(SULFAPIRIM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_PRIMAQUINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRIMAQUINA,'1','2','3')) AND ( .NOT. EMPTY(PRIMAQUINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_QUININA_O
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUININA_O,'1','2','3')) AND ( .NOT. EMPTY(QUININA_O)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_QUININA_I
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUININA_I,'1','2','3')) AND ( .NOT. EMPTY(QUININA_I)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_MEFLOQUINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MEFLOQUINA,'1','2','3')) AND ( .NOT. EMPTY(MEFLOQUINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ARTESUNATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTESUNATO,'1','2','3')) AND ( .NOT. EMPTY(ARTESUNATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COARTEM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COARTEM,'1','2','3')) AND ( .NOT. EMPTY(COARTEM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CLINDAMICI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLINDAMICI,'1','2','3')) AND ( .NOT. EMPTY(CLINDAMICI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_OTRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRO,'1','2','3')) AND ( .NOT. EMPTY(OTRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_ANT_HIB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_HIB,'1','2','3')) AND ( .NOT. EMPTY(ANT_HIB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_CARNE_HIB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARNE_HIB,'1','2')) OR ( EMPTY(CARNE_HIB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_MENINGOCOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MENINGOCOC,'1','2','3')) AND ( .NOT. EMPTY(MENINGOCOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_CARNE_MEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARNE_MEN,'1','2')) OR ( EMPTY(CARNE_MEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_NEUMOCOCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NEUMOCOCO,'1','2','3')) AND ( .NOT. EMPTY(NEUMOCOCO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_CARNE_NEU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARNE_NEU,'1','2')) OR ( EMPTY(CARNE_NEU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_RIG_NUCA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RIG_NUCA,'1','2')) AND ( .NOT. EMPTY(RIG_NUCA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_IRRI_MEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IRRI_MEN,'1','2')) AND ( .NOT. EMPTY(IRRI_MEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_RASH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RASH,'1','2')) AND ( .NOT. EMPTY(RASH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_AFONTANELA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AFONTANELA,'1','2')) AND ( .NOT. EMPTY(AFONTANELA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_ALT_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALT_CON,'1','2')) AND ( .NOT. EMPTY(ALT_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_USO_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(USO_ANT,'1','2')) AND ( .NOT. EMPTY(USO_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_19_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_IDENTIFICA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IDENTIFICA,'1','2','3','4')) AND ( .NOT. EMPTY(IDENTIFICA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_ANTIPERTUS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANTIPERTUS,'0','1','2','3','4','5')) AND ( .NOT. EMPTY(ANTIPERTUS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_TIP_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_VAC,'1','2','3')) OR ( EMPTY(TIP_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_ETA_ENF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ETA_ENF,'1','2','3')) AND ( .NOT. EMPTY(ETA_ENF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_IRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IRA,'1','2')) AND ( .NOT. EMPTY(IRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_TOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOS,'1','2')) AND ( .NOT. EMPTY(TOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_TOS_PAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOS_PAR,'1','2')) OR ( EMPTY(TOS_PAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_ESTRIDOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTRIDOR,'1','2')) AND ( .NOT. EMPTY(ESTRIDOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_APNEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APNEA,'1','2')) AND ( .NOT. EMPTY(APNEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_CIANOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CIANOSIS,'1','2')) AND ( .NOT. EMPTY(CIANOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) AND ( .NOT. EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_COMPLICACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMPLICACI,'1','2')) AND ( .NOT. EMPTY(COMPLICACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_TIP_COM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_COM,'1','2','3','4','5')) OR ( EMPTY(TIP_COM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_TTO_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TTO_ANT,'1','2')) AND ( .NOT. EMPTY(TTO_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_INV_CAM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INV_CAM,'1','2')) OR ( EMPTY(INV_CAM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_ANTEC_MATE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANTEC_MATE,'1','2')) AND ( .NOT. EMPTY(ANTEC_MATE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_TIP_IDE_PM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_PM,'RC','TI','CC','CE','PA','MS','AS','PE')) AND ( .NOT. EMPTY(TIP_IDE_PM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_NUM_IDE_PM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_PM))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_20_SEM_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SEM_GES)>=1  AND  VAL(SEM_GES) <=45 AND ISNUMERIC(SEM_GES)) OR ( EMPTY(SEM_GES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_VAC_EI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_EI,'1','2','3')) OR ( EMPTY(VAC_EI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_ASOC_BROTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASOC_BROTE,'1','2')) OR ( EMPTY(ASOC_BROTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_VIAJÓ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIAJÓ,'1','2')) OR ( EMPTY(VIAJÓ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_CON_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_CON,'1','2')) OR ( EMPTY(CON_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_CON_EST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_EST,'1','2')) OR ( EMPTY(CON_EST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_ASMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASMA,'1','2')) AND ( .NOT. EMPTY(ASMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_EPOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPOC,'1','2')) AND ( .NOT. EMPTY(EPOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_DIABETES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIABETES,'1','2')) AND ( .NOT. EMPTY(DIABETES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIH,'1','2')) AND ( .NOT. EMPTY(VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_ENF_CARD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENF_CARD,'1','2')) AND ( .NOT. EMPTY(ENF_CARD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_CANCER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CANCER,'1','2')) AND ( .NOT. EMPTY(CANCER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_MALNUTRI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MALNUTRI,'1','2')) AND ( .NOT. EMPTY(MALNUTRI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_OBESIDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OBESIDAD,'1','2')) AND ( .NOT. EMPTY(OBESIDAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_INS_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INS_RENAL,'1','2')) AND ( .NOT. EMPTY(INS_RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_OTR_MEDINM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTR_MEDINM,'1','2')) AND ( .NOT. EMPTY(OTR_MEDINM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_FUMADOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FUMADOR,'1','2')) AND ( .NOT. EMPTY(FUMADOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_OTROS_DC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS_DC,'1','2')) AND ( .NOT. EMPTY(OTROS_DC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_TOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOS,'1','2')) AND ( .NOT. EMPTY(TOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_RINORREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RINORREA,'1','2')) OR ( EMPTY(RINORREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_CONJUNTIVI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONJUNTIVI,'1','2')) OR ( EMPTY(CONJUNTIVI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) OR ( EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_DIF_RES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIF_RES,'1','2')) OR ( EMPTY(DIF_RES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) OR ( EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_HALLAZ_RAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HALLAZ_RAD,'1','2','3','4')) OR ( EMPTY(HALLAZ_RAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_USO_ANTIB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(USO_ANTIB,'1','2')) OR ( EMPTY(USO_ANTIB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_USO_ANTIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(USO_ANTIV,'1','2')) OR ( EMPTY(USO_ANTIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_SERV_HOSP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SERV_HOSP,'1','3')) OR ( EMPTY(SERV_HOSP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_DER_PLE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DER_PLE,'1','2')) AND ( .NOT. EMPTY(DER_PLE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_DER_PER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DER_PER,'1','2')) AND ( .NOT. EMPTY(DER_PER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_MIOCARDITI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIOCARDITI,'1','2')) AND ( .NOT. EMPTY(MIOCARDITI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_SEPTICEMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEPTICEMIA,'1','2')) AND ( .NOT. EMPTY(SEPTICEMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_FALLA_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_RESP,'1','2')) AND ( .NOT. EMPTY(FALLA_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_OTROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS,'1','2')) AND ( .NOT. EMPTY(OTROS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_TRAB_SALUD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAB_SALUD,'1','2')) OR ( EMPTY(TRAB_SALUD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_DETER_CLIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DETER_CLIN,'1','2')) OR ( EMPTY(DETER_CLIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_TROMBOCITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TROMBOCITO,'1','2')) OR ( EMPTY(TROMBOCITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_PLAQUETAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PLAQUETAS>=0  AND PLAQUETAS <=800000 AND ISNUMERIC(PLAQUETAS)) OR (PLAQUETAS=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_TUBERCULOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TUBERCULOS,'1','2')) OR ( EMPTY(TUBERCULOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_ODINOFAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ODINOFAGIA,'1','2')) OR ( EMPTY(ODINOFAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_ADINAMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADINAMIA,'1','2')) OR ( EMPTY(ADINAMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_HIPERTENSI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPERTENSI,'1','2')) OR ( EMPTY(HIPERTENSI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_PERD_GUSTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PERD_GUSTO,'1','2')) OR ( EMPTY(PERD_GUSTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_21_OTROS_SINT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS_SINT,'1','2')) OR ( EMPTY(OTROS_SINT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_NAUSEAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NAUSEAS,'1','2')) AND ( .NOT. EMPTY(NAUSEAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) AND ( .NOT. EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) AND ( .NOT. EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_CALAMBRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CALAMBRES,'1','2')) AND ( .NOT. EMPTY(CALAMBRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_DESHIDRATA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESHIDRATA,'1','2')) AND ( .NOT. EMPTY(DESHIDRATA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_CIANOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CIANOSIS,'1','2')) AND ( .NOT. EMPTY(CIANOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_MIALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIALGIAS,'1','2')) AND ( .NOT. EMPTY(MIALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_ARTRALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRALGIAS,'1','2')) AND ( .NOT. EMPTY(ARTRALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_MAREO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAREO,'1','2')) AND ( .NOT. EMPTY(MAREO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_LMACULOPAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LMACULOPAL,'1','2')) AND ( .NOT. EMPTY(LMACULOPAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_ESCALOFRIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESCALOFRIO,'1','2')) AND ( .NOT. EMPTY(ESCALOFRIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_PARESTESIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARESTESIA,'1','2')) AND ( .NOT. EMPTY(PARESTESIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_SIALORREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIALORREA,'1','2')) AND ( .NOT. EMPTY(SIALORREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_MIOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIOSIS,'1','2')) AND ( .NOT. EMPTY(MIOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_OTROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS,'1','2')) AND ( .NOT. EMPTY(OTROS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_HORA_INI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(HORA_INI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_ALI_IN_NA1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ALI_IN_NA1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_ALI_IN_HM1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ALI_IN_HM1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_ALI_IN_LC1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ALI_IN_LC1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_AI_ANT_NA1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AI_ANT_NA1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_AI_ANT_HM1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AI_ANT_HM1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_AI_ANT_LC1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AI_ANT_LC1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_AI_AN2_NA1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AI_AN2_NA1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_AI_AN2_HM1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AI_AN2_HM1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_AI_AN2_LC1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AI_AN2_LC1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_NOM_LUG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_LUG))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_DIR_LUG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DIR_LUG))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_ASO_BRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASO_BRO,'1','2')) AND ( .NOT. EMPTY(ASO_BRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_CAPTADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAPTADO,'1','2')) AND ( .NOT. EMPTY(CAPTADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_RELACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RELACION,'1','2')) AND ( .NOT. EMPTY(RELACION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_MTRA_BIOLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MTRA_BIOLO,'1','2')) AND ( .NOT. EMPTY(MTRA_BIOLO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_MTRA_HECES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MTRA_HECES,'1','2')) OR ( EMPTY(MTRA_HECES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_MTRA_VOMIT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MTRA_VOMIT,'1','2')) OR ( EMPTY(MTRA_VOMIT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_MTRA_SANGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MTRA_SANGR,'1','2')) OR ( EMPTY(MTRA_SANGR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_MTRA_OTRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MTRA_OTRA,'1','2')) OR ( EMPTY(MTRA_OTRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_22_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_COD_MUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_MUN)) OR INLIST(COD_EVE,'855','339','344','207','745','896','467')
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_NUM_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_CON))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_FEC_INV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_INV))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_DSS_NINGUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DSS_NINGUN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_NAUSEAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NAUSEAS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_VÓMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(VÓMITO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DIARREA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FIEBRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_DOLOR_ABDO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DOLOR_ABDO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CEFALEA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_DESHIDRATA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DESHIDRATA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_CIANOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CIANOSIS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_MIALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(MIALGIAS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ARTRALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ARTRALGIAS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_MAREO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(MAREO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ESTREÑIMIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ESTREÑIMIE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ESCALOFRÍO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ESCALOFRÍO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_PARESTESIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PARESTESIA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ICTERICIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ICTERICIA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ACOLIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ACOLIA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_COLURIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COLURIA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_LESIONESMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(LESIONESMA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ANOREXIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ANOREXIA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_MALESTAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(MALESTAR))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_BRADICARDI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(BRADICARDI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_SIALORREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SIALORREA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_MIOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(MIOSIS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_OTROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(OTROS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_PER_IN_COR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PER_IN_COR))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_PER_IN_LAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PER_IN_LAR))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_MUE_BIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUE_BIO,'2','3','4','5','6','7','8')) AND ( .NOT. EMPTY(MUE_BIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_MUE_ALI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUE_ALI,'1','2')) AND ( .NOT. EMPTY(MUE_ALI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_MUE_SUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUE_SUP,'1','2')) AND ( .NOT. EMPTY(MUE_SUP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_EST_MAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_MAN,'2','3','4','5','6','7')) AND ( .NOT. EMPTY(EST_MAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_LUG_CON_IM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(LUG_CON_IM))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_LUG_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LUG_CON,'1','2','3','4','5','6','7','8','9')) AND ( .NOT. EMPTY(LUG_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_CAD_FRIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAD_FRIO,'1','2')) AND ( .NOT. EMPTY(CAD_FRIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_CONSERVACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONSERVACI,'1','2')) AND ( .NOT. EMPTY(CONSERVACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ALMACENADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALMACENADO,'1','2')) AND ( .NOT. EMPTY(ALMACENADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_COCION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COCION,'1','2')) AND ( .NOT. EMPTY(COCION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_HIG_PER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIG_PER,'1','2')) AND ( .NOT. EMPTY(HIG_PER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_CON_CRU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_CRU,'1','2')) AND ( .NOT. EMPTY(CON_CRU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_LIM_UTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LIM_UTE,'1','2')) AND ( .NOT. EMPTY(LIM_UTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_AMBIENTAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AMBIENTAL,'1','2')) AND ( .NOT. EMPTY(AMBIENTAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_FTE_ALI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FTE_ALI,'1','2')) AND ( .NOT. EMPTY(FTE_ALI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_UTE_TOX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UTE_TOX,'1','2')) AND ( .NOT. EMPTY(UTE_TOX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ADI_TOX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADI_TOX,'1','2')) AND ( .NOT. EMPTY(ADI_TOX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_AGUA_NO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGUA_NO,'1','2')) AND ( .NOT. EMPTY(AGUA_NO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_TOX_TEJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOX_TEJ,'1','2')) AND ( .NOT. EMPTY(TOX_TEJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ING_EXE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ING_EXE,'1','2')) AND ( .NOT. EMPTY(ING_EXE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_MAN_INF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAN_INF,'1','2')) AND ( .NOT. EMPTY(MAN_INF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ACIDIFICAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACIDIFICAC,'1','2')) AND ( .NOT. EMPTY(ACIDIFICAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_ENFRIADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENFRIADO,'1','2')) AND ( .NOT. EMPTY(ENFRIADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_FAC_DET_EX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FAC_DET_EX,'1','2')) AND ( .NOT. EMPTY(FAC_DET_EX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_FAC_DET_AB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FAC_DET_AB,'1','2')) AND ( .NOT. EMPTY(FAC_DET_AB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_NINGUNA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NINGUNA,'1','2')) AND ( .NOT. EMPTY(NINGUNA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_CLAUSURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAUSURA,'1','2')) AND ( .NOT. EMPTY(CLAUSURA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_SUSPENSION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUSPENSION,'1','2')) AND ( .NOT. EMPTY(SUSPENSION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_CONGELACIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONGELACIO,'1','2')) AND ( .NOT. EMPTY(CONGELACIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_DECOMISO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DECOMISO,'1','2')) AND ( .NOT. EMPTY(DECOMISO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_AISLAMENTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AISLAMENTO,'1','2')) AND ( .NOT. EMPTY(AISLAMENTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_VACUNACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VACUNACION,'1','2')) AND ( .NOT. EMPTY(VACUNACION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_CTRL_INS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CTRL_INS,'1','2')) AND ( .NOT. EMPTY(CTRL_INS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_MEDIDA_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MEDIDA_PRE,'1','2')) AND ( .NOT. EMPTY(MEDIDA_PRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_CLA_ETA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLA_ETA,'1','2')) AND ( .NOT. EMPTY(CLA_ETA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_MUE_BIO2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUE_BIO2,'2','3','4','5','6','7','8')) AND ( .NOT. EMPTY(MUE_BIO2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_NUM_MU_EM1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_MU_EM1>=1 AND ISNUMERIC(NUM_MU_EM1)) OR (NUM_MU_EM1=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_EST_MAN_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_MAN_2,'2','3','4','5','6','7')) AND ( .NOT. EMPTY(EST_MAN_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_NUM_MU_EM2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_MU_EM2>=1 AND ISNUMERIC(NUM_MU_EM2)) OR (NUM_MU_EM2=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_NUM_MU_BI1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_MU_BI1>=1 AND ISNUMERIC(NUM_MU_BI1)) OR (NUM_MU_BI1=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_23_NUM_MU_BI2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_MU_BI2>=1 AND ISNUMERIC(NUM_MU_BI2)) OR (NUM_MU_BI2=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_CLA_INI_CA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLA_INI_CA,'1','2')) OR ( EMPTY(CLA_INI_CA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_ESP_ANI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_ANI,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(ESP_ANI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_UNI_MED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UNI_MED,'1','2')) OR ( EMPTY(UNI_MED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_ANT_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_VAC,'1','2','3')) OR ( EMPTY(ANT_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_AREA_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AREA_PRO,'1','2','3')) OR ( EMPTY(AREA_PRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_AGRESIVIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGRESIVIDA,'1','2')) AND ( .NOT. EMPTY(AGRESIVIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_PARALISIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARALISIS,'1','2')) AND ( .NOT. EMPTY(PARALISIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_SALIVACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SALIVACION,'1','2')) AND ( .NOT. EMPTY(SALIVACION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_APETITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APETITO,'1','2')) AND ( .NOT. EMPTY(APETITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_VORACIDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VORACIDAD,'1','2')) AND ( .NOT. EMPTY(VORACIDAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_DEGLUCION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEGLUCION,'1','2')) AND ( .NOT. EMPTY(DEGLUCION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_LADRIDO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LADRIDO,'1','2')) AND ( .NOT. EMPTY(LADRIDO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_MANDIBULA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MANDIBULA,'1','2')) AND ( .NOT. EMPTY(MANDIBULA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_ANISOCORIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANISOCORIA,'1','2')) AND ( .NOT. EMPTY(ANISOCORIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_OTRO_SS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRO_SS,'1','2')) AND ( .NOT. EMPTY(OTRO_SS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_MUERTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUERTE,'1','2','3','4')) AND ( .NOT. EMPTY(MUERTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_F_TOM_MUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(F_TOM_MUE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_PR_DIA_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PR_DIA_CON,'1','2')) OR ( EMPTY(PR_DIA_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_RESULTADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULTADO,'1','2','3','4')) OR ( EMPTY(RESULTADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_IDE_VV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IDE_VV,'1','2')) OR ( EMPTY(IDE_VV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_VAR_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAR_IDE,'1','3','4','5','8','0')) OR ( EMPTY(VAR_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COD_EVE,'650','652')) AND ( .NOT. EMPTY(COD_EVE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_24_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','2','7','D','6')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_EST_SOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EST_SOC)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','EST_SOC'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EST_SOC,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(EST_SOC)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_ESCOLARIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ESCOLARIDA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','ESCOLARIDA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ESCOLARIDA,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(ESCOLARIDA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_AMPUTACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMPUTACION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','AMPUTACION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMPUTACION,'0','3','4','5','6','7','8','9','10','11')) AND ( .NOT. EMPTY(AMPUTACION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_LACERACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LACERACION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','LACERACION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(LACERACION,'1','2')) AND ( .NOT. EMPTY(LACERACION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CONTUSION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CONTUSION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CONTUSION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CONTUSION,'1','2')) AND ( .NOT. EMPTY(CONTUSION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_HERIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(HERIDA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','HERIDA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(HERIDA,'1','2')) AND ( .NOT. EMPTY(HERIDA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_QUEMADURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUEMADURA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','QUEMADURA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUEMADURA,'1','2')) AND ( .NOT. EMPTY(QUEMADURA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_SIT_QUEMAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SIT_QUEMAD)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','SIT_QUEMAD'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SIT_QUEMAD,'1','2','3','4','5','6')) OR ( EMPTY(SIT_QUEMAD)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CLA_GRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CLA_GRA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CLA_GRA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CLA_GRA,'1','2','3','4','5')) OR ( EMPTY(CLA_GRA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_EXT_QUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EXT_QUE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','EXT_QUE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EXT_QUE,'1','2','3','4')) OR ( EMPTY(EXT_QUE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_DAÑ_OCU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(DAÑ_OCU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','DAÑ_OCU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(DAÑ_OCU,'0','3','4','5')) AND ( .NOT. EMPTY(DAÑ_OCU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_DAÑ_AUD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(DAÑ_AUD)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','DAÑ_AUD'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(DAÑ_AUD,'0','3','4','5')) AND ( .NOT. EMPTY(DAÑ_AUD)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_FRACTURAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRACTURAS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','FRACTURAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRACTURAS,'0','1','2','3','4','5','6','7','8','9')) AND ( .NOT. EMPTY(FRACTURAS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_SIN_DATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SIN_DATO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','SIN_DATO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SIN_DATO,'1','2')) AND ( .NOT. EMPTY(SIN_DATO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_VIA_AER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(VIA_AER)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','VIA_AER'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(VIA_AER,'1','2')) AND ( .NOT. EMPTY(VIA_AER)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_ABDOMEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ABDOMEN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','ABDOMEN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ABDOMEN,'1','2')) AND ( .NOT. EMPTY(ABDOMEN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_SA_SINDATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SA_SINDATO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','SA_SINDATO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SA_SINDATO,'1','2')) AND ( .NOT. EMPTY(SA_SINDATO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_SA_OTRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SA_OTRO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','SA_OTRO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SA_OTRO,'1','2')) AND ( .NOT. EMPTY(SA_OTRO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CIRCUNSTAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CIRCUNSTAN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CIRCUNSTAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CIRCUNSTAN,'1','2','3','4','5','6','7','8','9','10')) AND ( .NOT. EMPTY(CIRCUNSTAN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_INTENC_MAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(INTENC_MAN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','INTENC_MAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(INTENC_MAN,'1','2','3','4','5','6','7')) OR ( EMPTY(INTENC_MAN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_LUG_OCU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LUG_OCU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','LUG_OCU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(LUG_OCU,'1','2','3','4','5','7','6')) AND ( .NOT. EMPTY(LUG_OCU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CIR_SOCIAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CIR_SOCIAL)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CIR_SOCIAL'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CIR_SOCIAL,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(CIR_SOCIAL)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CON_ALC_LE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CON_ALC_LE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CON_ALC_LE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CON_ALC_LE,'1','2')) OR ( EMPTY(CON_ALC_LE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CON_ALC_AC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CON_ALC_AC)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CON_ALC_AC'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CON_ALC_AC,'1','2')) OR ( EMPTY(CON_ALC_AC)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_ARTEF_LESI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ARTEF_LESI)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','ARTEF_LESI'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ARTEF_LESI,'1','2','3','4','5')) AND ( .NOT. EMPTY(ARTEF_LESI)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_ARTEFACTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ARTEFACTO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','ARTEFACTO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ARTEFACTO,'1','2','3','4','5','6','7','8','10','11')) OR ( EMPTY(ARTEFACTO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AJUSTE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','AJUSTE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_CLAS_CASO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_CASO,'1','2')) AND ( .NOT. EMPTY(CLAS_CASO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_FIEBRE_AG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE_AG,'1','2')) OR ( EMPTY(FIEBRE_AG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_DISNEA_AG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISNEA_AG,'1','2')) OR ( EMPTY(DISNEA_AG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_EDE_FAC_AG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EDE_FAC_AG,'1','2')) OR ( EMPTY(EDE_FAC_AG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_ED_MI_AG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ED_MI_AG,'1','2')) OR ( EMPTY(ED_MI_AG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_DER_PERI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DER_PERI,'1','2')) OR ( EMPTY(DER_PERI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_HEP_AG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEP_AG,'1','2')) OR ( EMPTY(HEP_AG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_ADENO_AG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADENO_AG,'1','2')) OR ( EMPTY(ADENO_AG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_ROMAÑA_AG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ROMAÑA_AG,'1','2')) OR ( EMPTY(ROMAÑA_AG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_CHAGOMA_AG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CHAGOMA_AG,'1','2')) OR ( EMPTY(CHAGOMA_AG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_F_CARD_CR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(F_CARD_CR,'1','2')) AND ( .NOT. EMPTY(F_CARD_CR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_TORAC_CR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TORAC_CR,'1','2')) AND ( .NOT. EMPTY(TORAC_CR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_BRADI_CR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BRADI_CR,'1','2')) AND ( .NOT. EMPTY(BRADI_CR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_DISFAG_CR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISFAG_CR,'1','2')) AND ( .NOT. EMPTY(DISFAG_CR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_ARRITMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARRITMIA,'1','2')) AND ( .NOT. EMPTY(ARRITMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_GOTA_GRUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GOTA_GRUE,'1','2','3')) OR ( EMPTY(GOTA_GRUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_MICRO_HEM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MICRO_HEM,'1','2','3')) OR ( EMPTY(MICRO_HEM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_STROUT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(STROUT,'1','2','3')) OR ( EMPTY(STROUT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_ELISA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ELISA,'1','2','3')) OR ( EMPTY(ELISA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_INMUNOFLUO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INMUNOFLUO,'1','2','3')) OR ( EMPTY(INMUNOFLUO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_PROB_VIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROB_VIA,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(PROB_VIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_INMONOBLOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INMONOBLOT,'1','2','3')) OR ( EMPTY(INMONOBLOT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_REACTIVACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REACTIVACI,'1','2')) OR ( EMPTY(REACTIVACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_ELISA_NOCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ELISA_NOCO,'1','2','3')) OR ( EMPTY(ELISA_NOCO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_27_MICROMETOD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MICROMETOD,'1','2','3')) OR ( EMPTY(MICROMETOD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_MOM_DX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MOM_DX,'1','2')) AND ( .NOT. EMPTY(MOM_DX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_EDAD_GES_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD_GES_1)>= 0  AND  VAL(EDAD_GES_1) <= 45 AND ISNUMERIC(EDAD_GES_1)) AND ( .NOT. EMPTY(EDAD_GES_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_EDAD_GESDX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD_GESDX)>= 0  AND  VAL(EDAD_GESDX) <= 45 AND ISNUMERIC(EDAD_GESDX)) AND ( .NOT. EMPTY(EDAD_GESDX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_CD4
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CD4,'1','2')) AND ( .NOT. EMPTY(CD4)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_CVIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CVIR,'1','2')) AND ( .NOT. EMPTY(CVIR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_CVIR_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CVIR_2,'1','2')) OR ( EMPTY(CVIR_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_TTO_ARV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TTO_ARV,'1','2')) AND ( .NOT. EMPTY(TTO_ARV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_E_TTO_ARV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(E_TTO_ARV,'1','2','3','4')) OR ( EMPTY(E_TTO_ARV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_INI_TTO_IN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INI_TTO_IN,'1','2')) OR ( EMPTY(INI_TTO_IN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_E_TTO_INTR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(E_TTO_INTR,'1','2')) OR ( EMPTY(E_TTO_INTR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_TIPO_PAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_PAR,'1','2')) AND ( .NOT. EMPTY(TIPO_PAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_FEC_PARTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_PARTO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_CONDIC_RN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONDIC_RN,'1','2')) AND ( .NOT. EMPTY(CONDIC_RN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_MOTIVO_SAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MOTIVO_SAL,'1','2','3','4')) AND ( .NOT. EMPTY(MOTIVO_SAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_28_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_NOM_MEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_MEN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_F_NACTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(F_NACTO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_REG_CIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(REG_CIV))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_SEXO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEXO,'M','F')) AND ( .NOT. EMPTY(SEXO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_EDAD_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD_GES)>= 0  AND  VAL(EDAD_GES) <= 45 AND ISNUMERIC(EDAD_GES)) AND ( .NOT. EMPTY(EDAD_GES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_TIPO_PARTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_PARTO,'1','2')) AND ( .NOT. EMPTY(TIPO_PARTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PESO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_LONGITUD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(LONGITUD))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_CVIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CVIR,'1','2')) AND ( .NOT. EMPTY(CVIR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_CVIR2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CVIR2,'1','2')) AND ( .NOT. EMPTY(CVIR2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_TAR_PROF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TAR_PROF,'1','2')) AND ( .NOT. EMPTY(TAR_PROF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_ESQUEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESQUEMA,'1','2','3','4')) OR ( EMPTY(ESQUEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_REC_FOR_LA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REC_FOR_LA,'1','2')) AND ( .NOT. EMPTY(REC_FOR_LA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_REC_FORLA6
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REC_FORLA6,'1','2')) AND ( .NOT. EMPTY(REC_FORLA6)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_CLA_FIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLA_FIN,'1','2')) AND ( .NOT. EMPTY(CLA_FIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_29_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'CC','CE','PA','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_FEC_INV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_INV))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_ENT_REA_IN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENT_REA_IN,'1','2','3')) AND ( .NOT. EMPTY(ENT_REA_IN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_MUE_DETX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUE_DETX,'1','2','3')) AND ( .NOT. EMPTY(MUE_DETX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_SITIO_MUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SITIO_MUE,'1','2','3','4','5')) AND ( .NOT. EMPTY(SITIO_MUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_NOMBRE_ENT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOMBRE_ENT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_TIP_DOC_EN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_DOC_EN,'CC','CE','PA','AS')) AND ( .NOT. EMPTY(TIP_DOC_EN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_NUM_IDE_EN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_EN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_PARENTESCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARENTESCO,'1','2','3','4','5')) AND ( .NOT. EMPTY(PARENTESCO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_ESCOLA_MAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESCOLA_MAD,'1','2','3','4','5')) AND ( .NOT. EMPTY(ESCOLA_MAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_ESTRATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTRATO,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(ESTRATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_ESTADO_CIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADO_CIV,'1','2','3','4','5')) AND ( .NOT. EMPTY(ESTADO_CIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_OCUPACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(OCUPACION)) OR INLIST(COD_EVE,'357','351','352')
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_PESO_NACER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PESO_NACER)>=100  AND  VAL(PESO_NACER) <=6000 AND ISNUMERIC(PESO_NACER)) OR ( EMPTY(PESO_NACER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_ANT_DES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_DES,'1','2','3')) OR ( EMPTY(ANT_DES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_ESQ_VACUNA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESQ_VACUNA,'1','2','3')) OR ( EMPTY(ESQ_VACUNA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_HACINAMIEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HACINAMIEN,'1','2')) AND ( .NOT. EMPTY(HACINAMIEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_POCA_VENTI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(POCA_VENTI,'1','2')) AND ( .NOT. EMPTY(POCA_VENTI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_HUMEDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HUMEDAD,'1','2')) AND ( .NOT. EMPTY(HUMEDAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_ACUEDUCTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACUEDUCTO,'1','2')) AND ( .NOT. EMPTY(ACUEDUCTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_ALCANTARIL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALCANTARIL,'1','2')) AND ( .NOT. EMPTY(ALCANTARIL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_GAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GAS,'1','2')) AND ( .NOT. EMPTY(GAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_ELECTRICID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ELECTRICID,'1','2')) AND ( .NOT. EMPTY(ELECTRICID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_SE_FUMA_VI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SE_FUMA_VI,'1','2')) AND ( .NOT. EMPTY(SE_FUMA_VI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_COCINA_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COCINA_CON,'1','2','3','4')) AND ( .NOT. EMPTY(COCINA_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_FTES_CONTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FTES_CONTA,'1','2')) AND ( .NOT. EMPTY(FTES_CONTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_TRAFICO_VE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAFICO_VE,'1','2')) AND ( .NOT. EMPTY(TRAFICO_VE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_SIGNOS_ALA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIGNOS_ALA,'1','2','3')) OR ( EMPTY(SIGNOS_ALA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_AYUDA_MÉD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AYUDA_MÉD,'1','2','3')) OR ( EMPTY(AYUDA_MÉD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_FACIL_TRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FACIL_TRAN,'1','2','3')) AND ( .NOT. EMPTY(FACIL_TRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_DIF_ADMITI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIF_ADMITI,'1','2','3')) AND ( .NOT. EMPTY(DIF_ADMITI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_CUAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUAL,'1','2','3')) OR ( EMPTY(CUAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_ATE_OPORTU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATE_OPORTU,'1','2','3')) AND ( .NOT. EMPTY(ATE_OPORTU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_CAL_ATENCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAL_ATENCI,'1','2','3','4')) OR ( EMPTY(CAL_ATENCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_30_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_MES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(MES)>=1  AND  VAL(MES) <=12 AND ISNUMERIC(MES)) AND  (.NOT. EMPTY(MES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(AÑO)>=2011 AND ISNUMERIC(AÑO)) AND  (.NOT. EMPTY(AÑO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_COD_DEP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(COD_DEP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_COD_MUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(COD_MUN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_VAC_EXI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(VAC_EXI)>= 0 AND ISNUMERIC(VAC_EXI)) AND  (.NOT. EMPTY(VAC_EXI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_VAC_REC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(VAC_REC)>= 0 AND ISNUMERIC(VAC_REC)) AND  (.NOT. EMPTY(VAC_REC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_VAC_DIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(VAC_DIS)>=0 AND ISNUMERIC(VAC_DIS)) AND  (.NOT. EMPTY(VAC_DIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_VAC_PER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(VAC_PER)>=0 AND ISNUMERIC(VAC_PER)) AND  (.NOT. EMPTY(VAC_PER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_VAC_SAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(VAC_SAL)>=0 AND ISNUMERIC(VAC_SAL)) AND  (.NOT. EMPTY(VAC_SAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_VENC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_VENC)>=0 AND ISNUMERIC(PER_VENC)) AND  (.NOT. EMPTY(PER_VENC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_EXP_CA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_EXP_CA)>=0 AND ISNUMERIC(PER_EXP_CA)) AND  (.NOT. EMPTY(PER_EXP_CA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_CONG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_CONG)>=0 AND ISNUMERIC(PER_CONG)) AND  (.NOT. EMPTY(PER_CONG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_RUPT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_RUPT)>=0 AND ISNUMERIC(PER_RUPT)) AND  (.NOT. EMPTY(PER_RUPT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_HURTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_HURTO)>=0 AND ISNUMERIC(PER_HURTO)) AND  (.NOT. EMPTY(PER_HURTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_POL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_POL)>=0 AND ISNUMERIC(PER_POL)) AND  (.NOT. EMPTY(PER_POL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_ERR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_ERR)>=0 AND ISNUMERIC(PER_ERR)) AND  (.NOT. EMPTY(PER_ERR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_REAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_REAC)>=0 AND ISNUMERIC(PER_REAC)) AND  (.NOT. EMPTY(PER_REAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_CENSO_PC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CENSO_PC)>=1 AND ISNUMERIC(CENSO_PC)) AND  (.NOT. EMPTY(CENSO_PC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PERROS_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PERROS_VAC)>=0 AND ISNUMERIC(PERROS_VAC)) AND  (.NOT. EMPTY(PERROS_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_VACPRIPERR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(VACPRIPERR)>=0 AND ISNUMERIC(VACPRIPERR)) AND  (.NOT. EMPTY(VACPRIPERR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_COBMENPERR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(COBMENPERR)>=0 AND ISNUMERIC(COBMENPERR)) AND  (.NOT. EMPTY(COBMENPERR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_COB_ACU_VC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(COB_ACU_VC))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_CENSO_PF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CENSO_PF)>=1 AND ISNUMERIC(CENSO_PF)) AND  (.NOT. EMPTY(CENSO_PF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_GATOS_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(GATOS_VAC))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_VAC_PRIGAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(VAC_PRIGAT)>=0 AND ISNUMERIC(VAC_PRIGAT)) AND  (.NOT. EMPTY(VAC_PRIGAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_COB_MENGAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(COB_MENGAT)>=0 AND ISNUMERIC(COB_MENGAT)) AND  (.NOT. EMPTY(COB_MENGAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_COB_ACU_VF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(COB_ACU_VF))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PERROS_ELI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PERROS_ELI)>=0 AND ISNUMERIC(PERROS_ELI)) AND  (.NOT. EMPTY(PERROS_ELI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_GATOS_ELI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GATOS_ELI)>=0 AND ISNUMERIC(GATOS_ELI)) AND  (.NOT. EMPTY(GATOS_ELI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_HEMBP_EST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(HEMBP_EST)>=0 AND ISNUMERIC(HEMBP_EST)) AND  (.NOT. EMPTY(HEMBP_EST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_MACHOP_EST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(MACHOP_EST)>=0 AND ISNUMERIC(MACHOP_EST)) AND  (.NOT. EMPTY(MACHOP_EST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_HEMBG_EST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(HEMBG_EST)>=0 AND ISNUMERIC(HEMBG_EST)) AND  (.NOT. EMPTY(HEMBG_EST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_MACHOG_EST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(MACHOG_EST)>=0 AND ISNUMERIC(MACHOG_EST)) AND  (.NOT. EMPTY(MACHOG_EST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_OB_DOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_OB_DOC)>=0 AND ISNUMERIC(PER_OB_DOC)) AND  (.NOT. EMPTY(PER_OB_DOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_OB_CLI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_OB_CLI)>=0 AND ISNUMERIC(PER_OB_CLI)) AND  (.NOT. EMPTY(PER_OB_CLI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_OB_CEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_OB_CEN)>=0 AND ISNUMERIC(PER_OB_CEN)) AND  (.NOT. EMPTY(PER_OB_CEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_GAT_OB_DOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GAT_OB_DOC)>=0 AND ISNUMERIC(GAT_OB_DOC)) AND  (.NOT. EMPTY(GAT_OB_DOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_GAT_OB_CLI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GAT_OB_CLI)>=0 AND ISNUMERIC(GAT_OB_CLI)) AND  (.NOT. EMPTY(GAT_OB_CLI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_GAT_OB_CEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GAT_OB_CEN)>=0 AND ISNUMERIC(GAT_OB_CEN)) AND  (.NOT. EMPTY(GAT_OB_CEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_CON_RA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_CON_RA)>=0 AND ISNUMERIC(PER_CON_RA)) AND  (.NOT. EMPTY(PER_CON_RA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_SIN_RA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_SIN_RA)>=0 AND ISNUMERIC(PER_SIN_RA)) AND  (.NOT. EMPTY(PER_SIN_RA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_PER_MUERTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PER_MUERTO)>=0 AND ISNUMERIC(PER_MUERTO)) AND  (.NOT. EMPTY(PER_MUERTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_GAT_CON_RA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GAT_CON_RA)>=0 AND ISNUMERIC(GAT_CON_RA)) AND  (.NOT. EMPTY(GAT_CON_RA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_GAT_SIN_RA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GAT_SIN_RA)>=0 AND ISNUMERIC(GAT_SIN_RA)) AND  (.NOT. EMPTY(GAT_SIN_RA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_GAT_MUERTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GAT_MUERTO)>=0 AND ISNUMERIC(GAT_MUERTO)) AND  (.NOT. EMPTY(GAT_MUERTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_NOM_DIL_FI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(NOM_DIL_FI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_FEC_DILIGE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN  (.NOT. EMPTY(FEC_DILIGE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_32_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','7','D')) OR  (EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_VAC_CON_RO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_CON_RO,'1','2','3')) AND ( .NOT. EMPTY(VAC_CON_RO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_TIENE_CARN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIENE_CARN,'1','2')) AND ( .NOT. EMPTY(TIENE_CARN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_PESO_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PESO_NAC)>= 500  AND  VAL(PESO_NAC) <=6000 AND ISNUMERIC(PESO_NAC)) AND ( .NOT. EMPTY(PESO_NAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_REC_LECHE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REC_LECHE,'1','2','3')) AND ( .NOT. EMPTY(REC_LECHE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_ALIM_ACTUA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALIM_ACTUA,'1','2','3','4')) AND ( .NOT. EMPTY(ALIM_ACTUA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2','3')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2','3')) AND ( .NOT. EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_FEC_INI_DI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_INI_DI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_NO_DEPOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NO_DEPOS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_FEC_TER_DI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_TER_DI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_CARAC_HECE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARAC_HECE,'1','2','3','4')) AND ( .NOT. EMPTY(CARAC_HECE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_ESTADO_ING
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADO_ING,'1','2')) AND ( .NOT. EMPTY(ESTADO_ING)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_GRAD_DESHI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GRAD_DESHI,'1','2','3','4')) OR ( EMPTY(GRAD_DESHI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_PESO_INGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PESO_INGR)>=1000  AND  VAL(PESO_INGR) <=50000 AND ISNUMERIC(PESO_INGR)) AND ( .NOT. EMPTY(PESO_INGR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_TALLA_INGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TALLA_INGR)>=45  AND  VAL(TALLA_INGR) <=150 AND ISNUMERIC(TALLA_INGR)) AND ( .NOT. EMPTY(TALLA_INGR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_REC_ANTIBI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REC_ANTIBI,'1','2','3')) AND ( .NOT. EMPTY(REC_ANTIBI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_TRAT_HIDRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAT_HIDRA,'1','2')) AND ( .NOT. EMPTY(TRAT_HIDRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_COMP_HOSP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMP_HOSP,'1','2','3')) AND ( .NOT. EMPTY(COMP_HOSP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_REC_ANT_HO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REC_ANT_HO,'1','2','3')) AND ( .NOT. EMPTY(REC_ANT_HO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_DIAS_HOSP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DIAS_HOSP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_DIAS_H_URG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DIAS_H_URG))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_DIAS_H_PED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DIAS_H_PED))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_DIAS_H_UCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DIAS_H_UCI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_FEC_EGRESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_EGRESO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_MOTIVO_EGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MOTIVO_EGR,'1','2','3')) AND ( .NOT. EMPTY(MOTIVO_EGR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_SAL_CON_DI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SAL_CON_DI,'1','2','3')) AND ( .NOT. EMPTY(SAL_CON_DI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_DIAG_EGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DIAG_EGR))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_IDENT_ROTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IDENT_ROTA,'1','2')) OR ( EMPTY(IDENT_ROTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_IDENT_BACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IDENT_BACT,'1','2')) OR ( EMPTY(IDENT_BACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_IDENT_PARA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IDENT_PARA,'1','2')) OR ( EMPTY(IDENT_PARA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_GUARDERIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GUARDERIA,'1','2')) AND ( .NOT. EMPTY(GUARDERIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_MAS_PERSON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAS_PERSON,'1','2','3')) AND ( .NOT. EMPTY(MAS_PERSON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_33_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_FEC_INV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_INV))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ENTID_INVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENTID_INVE,'1','2','3')) AND ( .NOT. EMPTY(ENTID_INVE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_CAUS_MUER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUS_MUER,'1','2','3')) AND ( .NOT. EMPTY(CAUS_MUER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_SIT_MUER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_MUER,'1','2','3','4','5')) AND ( .NOT. EMPTY(SIT_MUER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_NOM_ENTREV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_ENTREV))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_TIP_IDE_EN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_EN,'CC','CE','PA','AS')) AND ( .NOT. EMPTY(TIP_IDE_EN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_NUM_IDE_EN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_EN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_PARENT_ENT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARENT_ENT,'1','2','3','4','5')) AND ( .NOT. EMPTY(PARENT_ENT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_PARENT_CUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARENT_CUI,'1','2','5')) AND ( .NOT. EMPTY(PARENT_CUI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ETNIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ETNIA,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(ETNIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ESC_CUID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESC_CUID,'1','2','3','4','5')) AND ( .NOT. EMPTY(ESC_CUID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ESTRATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTRATO,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(ESTRATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ESTAD_CUID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTAD_CUID,'1','2','3','4','5')) AND ( .NOT. EMPTY(ESTAD_CUID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_OCUPAC_CUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(OCUPAC_CUI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ESQ_VACU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESQ_VACU,'1','2','3')) AND ( .NOT. EMPTY(ESQ_VACU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_CARNE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARNE,'1','2')) AND ( .NOT. EMPTY(CARNE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_DESNUTRICI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESNUTRICI,'1','2')) AND ( .NOT. EMPTY(DESNUTRICI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_BAJO_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BAJO_PESO,'1','2')) AND ( .NOT. EMPTY(BAJO_PESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_HACINAMIEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HACINAMIEN,'1','2')) AND ( .NOT. EMPTY(HACINAMIEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_HAB_INADEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HAB_INADEC,'1','2')) AND ( .NOT. EMPTY(HAB_INADEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_PISO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PISO,'1','2')) AND ( .NOT. EMPTY(PISO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_INSECTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INSECTOS,'1','2')) AND ( .NOT. EMPTY(INSECTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_INA_MAN_AL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INA_MAN_AL,'1','2')) AND ( .NOT. EMPTY(INA_MAN_AL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_FUEN_AGUA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FUEN_AGUA,'1','2','3','4','5','6','7','8')) AND ( .NOT. EMPTY(FUEN_AGUA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_DISP_EXCRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISP_EXCRE,'1','2','3','4','5')) AND ( .NOT. EMPTY(DISP_EXCRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_RECON_SIG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECON_SIG,'1','2','3')) AND ( .NOT. EMPTY(RECON_SIG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_BUSC_AYUDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BUSC_AYUDA,'1','2','3')) AND ( .NOT. EMPTY(BUSC_AYUDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_FACIL_TRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FACIL_TRAN,'1','2','3')) AND ( .NOT. EMPTY(FACIL_TRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_RECON_DIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECON_DIA,'1','2')) AND ( .NOT. EMPTY(RECON_DIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_RECIBI_SRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECIBI_SRO,'1','2')) AND ( .NOT. EMPTY(RECIBI_SRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_TERAP_NO_M
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TERAP_NO_M,'1','2')) AND ( .NOT. EMPTY(TERAP_NO_M)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_DIFIC_ADM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIFIC_ADM,'1','2','3')) AND ( .NOT. EMPTY(DIFIC_ADM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_CUAL_DIFIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUAL_DIFIC,'1','2','3')) OR ( EMPTY(CUAL_DIFIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_ATN_OPORTU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATN_OPORTU,'1','2','3')) AND ( .NOT. EMPTY(ATN_OPORTU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_CALIDAD_AT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CALIDAD_AT,'1','2','3','4')) AND ( .NOT. EMPTY(CALIDAD_AT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_34_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_TIP_AGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_AGR,'1','2','3','6','7','8')) AND ( .NOT. EMPTY(TIP_AGR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_AREA_MORDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AREA_MORDE,'1','2')) OR ( EMPTY(AREA_MORDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_AGR_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGR_PRO,'1','2')) AND ( .NOT. EMPTY(AGR_PRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_TIP_LES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_LES,'1','2')) AND ( .NOT. EMPTY(TIP_LES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_PROFUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROFUN,'1','2')) AND ( .NOT. EMPTY(PROFUN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_CCC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CCC,'1','2')) AND ( .NOT. EMPTY(CCC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_MAN_DED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAN_DED,'1','2')) AND ( .NOT. EMPTY(MAN_DED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_TRONCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRONCO,'1','2')) AND ( .NOT. EMPTY(TRONCO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_MIE_SUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIE_SUP,'1','2')) AND ( .NOT. EMPTY(MIE_SUP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_MIE_INF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIE_INF,'1','2')) AND ( .NOT. EMPTY(MIE_INF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_PIES_DEDOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIES_DEDOS,'1','2')) OR ( EMPTY(PIES_DEDOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_GENIT_EXT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GENIT_EXT,'1','2')) OR ( EMPTY(GENIT_EXT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_FEC_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_EXP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_ESP_ANI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_ANI,'1','2','3','4','5','7','8','9','10','12','13','14')) AND ( .NOT. EMPTY(ESP_ANI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_ANT_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_VAC,'1','2','3')) OR ( EMPTY(ANT_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_CAR_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAR_VAC,'1','2')) OR ( EMPTY(CAR_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_EST_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_MA,'1','2','3')) OR ( EMPTY(EST_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_ESTADO_ANI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADO_ANI,'1','2','3')) OR ( EMPTY(ESTADO_ANI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_UBICACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBICACION,'1','2')) OR ( EMPTY(UBICACION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_SUE_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUE_ANT,'1','2','3')) AND ( .NOT. EMPTY(SUE_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_VAC_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_ANT,'1','2','3')) AND ( .NOT. EMPTY(VAC_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_APL_SA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_SA,'1','2')) AND ( .NOT. EMPTY(APL_SA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_APL_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_VAC,'1','2')) AND ( .NOT. EMPTY(APL_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_OTROS_MEDI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS_MEDI,'1','2')) AND ( .NOT. EMPTY(OTROS_MEDI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_HIPOREXIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPOREXIA,'1','2')) AND ( .NOT. EMPTY(HIPOREXIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) AND ( .NOT. EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_PARESIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARESIAS,'1','2')) AND ( .NOT. EMPTY(PARESIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_PARESTESIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARESTESIA,'1','2')) AND ( .NOT. EMPTY(PARESTESIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_DISFAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISFAGIA,'1','2')) AND ( .NOT. EMPTY(DISFAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_ODINOFAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ODINOFAGIA,'1','2')) AND ( .NOT. EMPTY(ODINOFAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_ARREFLEXIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARREFLEXIA,'1','2')) AND ( .NOT. EMPTY(ARREFLEXIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_PSICOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PSICOSIS,'1','2')) AND ( .NOT. EMPTY(PSICOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_FASCIES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FASCIES,'1','2')) AND ( .NOT. EMPTY(FASCIES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_SIALORREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIALORREA,'1','2')) AND ( .NOT. EMPTY(SIALORREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_AEROFOBIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AEROFOBIA,'1','2')) AND ( .NOT. EMPTY(AEROFOBIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_HIDROFOBIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIDROFOBIA,'1','2')) AND ( .NOT. EMPTY(HIDROFOBIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_TRANQ_EXCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRANQ_EXCI,'1','2')) AND ( .NOT. EMPTY(TRANQ_EXCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_DEPRESION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEPRESION,'1','2')) AND ( .NOT. EMPTY(DEPRESION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_HIPEREXITA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPEREXITA,'1','2')) AND ( .NOT. EMPTY(HIPEREXITA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_AGRESIVIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGRESIVIDA,'1','2')) AND ( .NOT. EMPTY(AGRESIVIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_ESPASMOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESPASMOS,'1','2')) AND ( .NOT. EMPTY(ESPASMOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_CONVULSION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONVULSION,'1','2')) AND ( .NOT. EMPTY(CONVULSION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_PARALISIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARALISIS,'1','2')) AND ( .NOT. EMPTY(PARALISIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_CRIS_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRIS_RESP,'1','2')) AND ( .NOT. EMPTY(CRIS_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_COMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMA,'1','2')) AND ( .NOT. EMPTY(COMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_PARO_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARO_RESP,'1','2')) AND ( .NOT. EMPTY(PARO_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_PR_DIAG_CO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PR_DIAG_CO,'1','2','3','4','5')) OR ( EMPTY(PR_DIAG_CO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_RESULTADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULTADO,'1','2','4')) OR ( EMPTY(RESULTADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_ID_VARIANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ID_VARIANT,'1','2','3')) OR ( EMPTY(ID_VARIANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_VAR_IDENTI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAR_IDENTI,'1','3','4','5','8','9','0')) OR ( EMPTY(VAR_IDENTI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_35_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_MIALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIALGIAS,'1','2')) AND ( .NOT. EMPTY(MIALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_HEPATOMEGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATOMEGA,'1','2')) AND ( .NOT. EMPTY(HEPATOMEGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_ICTERICIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ICTERICIA,'1','2')) AND ( .NOT. EMPTY(ICTERICIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_PERROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PERROS,'1','2')) AND ( .NOT. EMPTY(PERROS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_GATOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GATOS,'1','2')) AND ( .NOT. EMPTY(GATOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_BOVINOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BOVINOS,'1','2')) AND ( .NOT. EMPTY(BOVINOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_EQUINOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EQUINOS,'1','2')) AND ( .NOT. EMPTY(EQUINOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_PORCINOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PORCINOS,'1','2')) AND ( .NOT. EMPTY(PORCINOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_NINGUNO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NINGUNO,'1','2')) AND ( .NOT. EMPTY(NINGUNO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_OTROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS,'1','2')) AND ( .NOT. EMPTY(OTROS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_RATAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RATAS,'1','2')) AND ( .NOT. EMPTY(RATAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_ACUEDUCTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACUEDUCTO,'1','2')) AND ( .NOT. EMPTY(ACUEDUCTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_POZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(POZO,'1','2')) AND ( .NOT. EMPTY(POZO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_RIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RIO,'1','2')) AND ( .NOT. EMPTY(RIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_TANQUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TANQUE,'1','2')) AND ( .NOT. EMPTY(TANQUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_ALCAN_DES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALCAN_DES,'1','2')) AND ( .NOT. EMPTY(ALCAN_DES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_C_AGU_ESTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(C_AGU_ESTA,'1','2')) AND ( .NOT. EMPTY(C_AGU_ESTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_REPRESA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REPRESA,'1','2')) AND ( .NOT. EMPTY(REPRESA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_RÍO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RÍO,'1','2')) AND ( .NOT. EMPTY(RÍO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_ARROYO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARROYO,'1','2')) AND ( .NOT. EMPTY(ARROYO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_LAGOLAGUNA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LAGOLAGUNA,'1','2')) AND ( .NOT. EMPTY(LAGOLAGUNA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_SINANTECED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SINANTECED,'1','2')) AND ( .NOT. EMPTY(SINANTECED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_D_RES_SOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(D_RES_SOL,'1','2')) AND ( .NOT. EMPTY(D_RES_SOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_36_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_TIPO_LEUCE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_LEUCE,'1','2','3','4')) AND ( .NOT. EMPTY(TIPO_LEUCE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_INFECCION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFECCION,'1','2')) AND ( .NOT. EMPTY(INFECCION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_SLISIS_TUM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SLISIS_TUM,'1','2')) AND ( .NOT. EMPTY(SLISIS_TUM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_TROMBOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TROMBOSIS,'1','2')) AND ( .NOT. EMPTY(TROMBOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_HEMORRAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMORRAGIA,'1','2')) AND ( .NOT. EMPTY(HEMORRAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_CCONVULSIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CCONVULSIV,'1','2')) AND ( .NOT. EMPTY(CCONVULSIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_DESCONOCID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESCONOCID,'1','2')) OR ( EMPTY(DESCONOCID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_OTRAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRAS,'1','2')) AND ( .NOT. EMPTY(OTRAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_SIT_DEF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_DEF,'1','2','3','4')) OR ( EMPTY(SIT_DEF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_CAUS_MUE_D
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUS_MUE_D,'1','2','3')) OR ( EMPTY(CAUS_MUE_D)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_FEC_INI_TR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_INI_TR))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_TPOR_DIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TPOR_DIAS)>= 0  AND  VAL(TPOR_DIAS) <= 30 AND ISNUMERIC(TPOR_DIAS)) OR ( EMPTY(TPOR_DIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_TPOR_MESES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TPOR_MESES)>= 0  AND  VAL(TPOR_MESES) <= 12 AND ISNUMERIC(TPOR_MESES)) OR ( EMPTY(TPOR_MESES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_TPOD_DIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TPOD_DIAS)>= 0  AND  VAL(TPOD_DIAS) <= 30 AND ISNUMERIC(TPOD_DIAS)) OR ( EMPTY(TPOD_DIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_TPOD_MESES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TPOD_MESES)>= 0  AND  VAL(TPOD_MESES) <= 12 AND ISNUMERIC(TPOD_MESES)) OR ( EMPTY(TPOD_MESES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_TPOI_DIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TPOI_DIAS)>= 0  AND  VAL(TPOI_DIAS) <= 30 AND ISNUMERIC(TPOI_DIAS)) OR ( EMPTY(TPOI_DIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_TPOI_MESES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TPOI_MESES)>= 0  AND  VAL(TPOI_MESES) <= 12 AND ISNUMERIC(TPOI_MESES)) OR ( EMPTY(TPOI_MESES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_37_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_NOM_MADRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_MADRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_TIP_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_MA,'RC','TI','CC','CE','PA','MS','AS')) AND ( .NOT. EMPTY(TIP_IDE_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_NUM_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_MA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_EDAD_NIÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD_NIÑO)>0  AND  VAL(EDAD_NIÑO) <=48 AND ISNUMERIC(EDAD_NIÑO)) OR ( EMPTY(EDAD_NIÑO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_EDAD_GEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD_GEST)>=22  AND  VAL(EDAD_GEST) <=43 AND ISNUMERIC(EDAD_GEST)) AND ( .NOT. EMPTY(EDAD_GEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_PARTO_MULT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARTO_MULT,'1','2')) AND ( .NOT. EMPTY(PARTO_MULT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_BAJO_PN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BAJO_PN,'1','2')) AND ( .NOT. EMPTY(BAJO_PN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_PESO_NACER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PESO_NACER)>=500  AND  VAL(PESO_NACER) <=4999 AND ISNUMERIC(PESO_NACER)) AND ( .NOT. EMPTY(PESO_NACER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_TAMIZAJE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TAMIZAJE,'1','2')) AND ( .NOT. EMPTY(TAMIZAJE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_RESULTADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULTADO,'1','2')) OR ( EMPTY(RESULTADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_PARTO_ATEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARTO_ATEN,'1','2','3')) AND ( .NOT. EMPTY(PARTO_ATEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_COND_M_DX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COND_M_DX,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(COND_M_DX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_RES_TSH_NA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(RES_TSH_NA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_REC_TTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REC_TTO,'1','2')) AND ( .NOT. EMPTY(REC_TTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_VAL_MED_GR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAL_MED_GR,'1','2')) AND ( .NOT. EMPTY(VAL_MED_GR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_VAL_PEDIAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAL_PEDIAT,'1','2')) AND ( .NOT. EMPTY(VAL_PEDIAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_VAL_ENDOCR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAL_ENDOCR,'1','2')) AND ( .NOT. EMPTY(VAL_ENDOCR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_VAL_NEUROL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAL_NEUROL,'1','2')) AND ( .NOT. EMPTY(VAL_NEUROL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_VAL_GENETI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAL_GENETI,'1','2')) AND ( .NOT. EMPTY(VAL_GENETI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_VAL_NINGUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAL_NINGUN,'1','2')) AND ( .NOT. EMPTY(VAL_NINGUN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_ECO_TIROID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ECO_TIROID,'1','2')) AND ( .NOT. EMPTY(ECO_TIROID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_RX_RODILLA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RX_RODILLA,'1','2')) AND ( .NOT. EMPTY(RX_RODILLA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_TTO_MATERN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TTO_MATERN,'1','2')) AND ( .NOT. EMPTY(TTO_MATERN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_TTO_LACTAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TTO_LACTAN,'1','2')) AND ( .NOT. EMPTY(TTO_LACTAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_TSH_CORDON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TSH_CORDON,'1','2')) OR ( EMPTY(TSH_CORDON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_RES_CORDON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_CORDON,'1','2','3')) OR ( EMPTY(RES_CORDON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_TSH_TALON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TSH_TALON,'1','2')) OR ( EMPTY(TSH_TALON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_RES_TALON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_TALON,'1','2','3')) OR ( EMPTY(RES_TALON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_TSH_CONFIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TSH_CONFIR,'1','2')) OR ( EMPTY(TSH_CONFIR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_RES_CONFIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_CONFIR,'1','2','3')) OR ( EMPTY(RES_CONFIR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_T4_TOTAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(T4_TOTAL,'1','2')) OR ( EMPTY(T4_TOTAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_RES_T4_TOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_T4_TOT,'1','2','3')) OR ( EMPTY(RES_T4_TOT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_T4_LIBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(T4_LIBRE,'1','2')) OR ( EMPTY(T4_LIBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_RES_T4_LIB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_T4_LIB,'1','2','3')) OR ( EMPTY(RES_T4_LIB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_T3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(T3,'1','2')) OR ( EMPTY(T3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_RES_T3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_T3,'1','2','3')) OR ( EMPTY(RES_T3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_TBG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TBG,'1','2')) OR ( EMPTY(TBG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_RES_TBG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_TBG,'1','2','3')) OR ( EMPTY(RES_TBG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_ENE_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENE_1,'1','2')) AND ( .NOT. EMPTY(ENE_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_FEB_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FEB_1,'1','2')) AND ( .NOT. EMPTY(FEB_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_MAR_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAR_1,'1','2')) AND ( .NOT. EMPTY(MAR_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_ABR_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ABR_1,'1','2')) AND ( .NOT. EMPTY(ABR_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_MAY_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAY_1,'1','2')) AND ( .NOT. EMPTY(MAY_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_JUN_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JUN_1,'1','2')) AND ( .NOT. EMPTY(JUN_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_JUL_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JUL_1,'1','2')) AND ( .NOT. EMPTY(JUL_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_AGO_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGO_1,'1','2')) AND ( .NOT. EMPTY(AGO_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_SEP_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEP_1,'1','2')) AND ( .NOT. EMPTY(SEP_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_OCT_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OCT_1,'1','2')) AND ( .NOT. EMPTY(OCT_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_NOV_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NOV_1,'1','2')) AND ( .NOT. EMPTY(NOV_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_DIC_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIC_1,'1','2')) AND ( .NOT. EMPTY(DIC_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_ENE_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENE_2,'1','2')) AND ( .NOT. EMPTY(ENE_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_FEB_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FEB_2,'1','2')) AND ( .NOT. EMPTY(FEB_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_MAR_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAR_2,'1','2')) AND ( .NOT. EMPTY(MAR_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_ABR_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ABR_2,'1','2')) AND ( .NOT. EMPTY(ABR_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_MAY_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAY_2,'1','2')) AND ( .NOT. EMPTY(MAY_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_JUN_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JUN_2,'1','2')) AND ( .NOT. EMPTY(JUN_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_JUL_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JUL_2,'1','2')) AND ( .NOT. EMPTY(JUL_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_AGO_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGO_2,'1','2')) AND ( .NOT. EMPTY(AGO_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_SEP_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEP_2,'1','2')) AND ( .NOT. EMPTY(SEP_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_OCT_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OCT_2,'1','2')) AND ( .NOT. EMPTY(OCT_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_NOV_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NOV_2,'1','2')) AND ( .NOT. EMPTY(NOV_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_DIC_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIC_2,'1','2')) AND ( .NOT. EMPTY(DIC_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_ENE_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENE_3,'1','2')) AND ( .NOT. EMPTY(ENE_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_FEB_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FEB_3,'1','2')) AND ( .NOT. EMPTY(FEB_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_MAR_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAR_3,'1','2')) AND ( .NOT. EMPTY(MAR_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_ABR_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ABR_3,'1','2')) AND ( .NOT. EMPTY(ABR_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_MAY_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAY_3,'1','2')) AND ( .NOT. EMPTY(MAY_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_JUN_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JUN_3,'1','2')) AND ( .NOT. EMPTY(JUN_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_JUL_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JUL_3,'1','2')) AND ( .NOT. EMPTY(JUL_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_AGO_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGO_3,'1','2')) AND ( .NOT. EMPTY(AGO_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_SEP_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEP_3,'1','2')) AND ( .NOT. EMPTY(SEP_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_OCT_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OCT_3,'1','2')) AND ( .NOT. EMPTY(OCT_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_NOV_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NOV_3,'1','2')) AND ( .NOT. EMPTY(NOV_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_DIC_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIC_3,'1','2')) AND ( .NOT. EMPTY(DIC_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_PUNTAJE_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PUNTAJE_1,'1','2')) AND ( .NOT. EMPTY(PUNTAJE_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_PUNTAJE_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PUNTAJE_2,'1','2')) AND ( .NOT. EMPTY(PUNTAJE_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_PUNTAJE_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PUNTAJE_3,'1','2')) AND ( .NOT. EMPTY(PUNTAJE_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_38_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_AFE_NER_CR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AFE_NER_CR,'1','2','3')) AND ( .NOT. EMPTY(AFE_NER_CR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_TRISMUS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRISMUS,'1','2','3')) AND ( .NOT. EMPTY(TRISMUS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_OPISTÓTONO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OPISTÓTONO,'1','2','3')) AND ( .NOT. EMPTY(OPISTÓTONO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2','3')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_DOL_CUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOL_CUE,'1','2','3')) AND ( .NOT. EMPTY(DOL_CUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_DOL_GAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOL_GAR,'1','2','3')) AND ( .NOT. EMPTY(DOL_GAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_IMP_HABLAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IMP_HABLAR,'1','2','3')) AND ( .NOT. EMPTY(IMP_HABLAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_DISFAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISFAGIA,'1','2','3')) AND ( .NOT. EMPTY(DISFAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_CONVULSION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONVULSION,'1','2','3')) AND ( .NOT. EMPTY(CONVULSION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_CON_MUSCUL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_MUSCUL,'1','2','3')) AND ( .NOT. EMPTY(CON_MUSCUL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_RIG_MU_ABD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RIG_MU_ABD,'1','2','3')) AND ( .NOT. EMPTY(RIG_MU_ABD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_ESP_GENERA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_GENERA,'1','2','3')) AND ( .NOT. EMPTY(ESP_GENERA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_RIG_NUCA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RIG_NUCA,'1','2','3')) AND ( .NOT. EMPTY(RIG_NUCA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_39_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_VIAJO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIAJO,'1','2','3')) AND ( .NOT. EMPTY(VIAJO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_RATAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RATAS,'1','2')) OR ( EMPTY(RATAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_PERROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PERROS,'1','2')) OR ( EMPTY(PERROS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_CERDOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CERDOS,'1','2')) OR ( EMPTY(CERDOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_CONTAC_ACC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONTAC_ACC,'1','2','3')) AND ( .NOT. EMPTY(CONTAC_ACC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_RECIBIO_TR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECIBIO_TR,'1','2','3')) AND ( .NOT. EMPTY(RECIBIO_TR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_ESCALOFRIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESCALOFRIO,'1','2')) AND ( .NOT. EMPTY(ESCALOFRIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_DOLOR_RETR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR_RETR,'1','2')) AND ( .NOT. EMPTY(DOLOR_RETR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) AND ( .NOT. EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) AND ( .NOT. EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_DABDOMINAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DABDOMINAL,'1','2')) AND ( .NOT. EMPTY(DABDOMINAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_ICTERICIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ICTERICIA,'1','2')) AND ( .NOT. EMPTY(ICTERICIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_PETEQUIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PETEQUIAS,'1','2')) OR ( EMPTY(PETEQUIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_EQUIMOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EQUIMOSIS,'1','2')) OR ( EMPTY(EQUIMOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_HEMOPTISIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMOPTISIS,'1','2')) OR ( EMPTY(HEMOPTISIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_GINGIVORRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GINGIVORRA,'1','2')) OR ( EMPTY(GINGIVORRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_METRORRAGI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(METRORRAGI,'1','2')) OR ( EMPTY(METRORRAGI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_HEMATURIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMATURIA,'1','2')) OR ( EMPTY(HEMATURIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_MELENAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MELENAS,'1','2')) OR ( EMPTY(MELENAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_MIALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIALGIAS,'1','2')) AND ( .NOT. EMPTY(MIALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_ARTRALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRALGIAS,'1','2')) AND ( .NOT. EMPTY(ARTRALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_HEPATOMEGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATOMEGA,'1','2')) AND ( .NOT. EMPTY(HEPATOMEGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_ESPLENOMEG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESPLENOMEG,'1','2')) AND ( .NOT. EMPTY(ESPLENOMEG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_ADENOPATIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADENOPATIA,'1','2')) AND ( .NOT. EMPTY(ADENOPATIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_DISNEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISNEA,'1','2')) AND ( .NOT. EMPTY(DISNEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_DESHIDRATA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESHIDRATA,'1','2')) AND ( .NOT. EMPTY(DESHIDRATA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_SIG_MENING
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIG_MENING,'1','2')) AND ( .NOT. EMPTY(SIG_MENING)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_EXANTEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EXANTEMA,'1','2')) AND ( .NOT. EMPTY(EXANTEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_EDEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EDEMA,'1','2')) AND ( .NOT. EMPTY(EDEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_CONVULSION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONVULSION,'1','2')) AND ( .NOT. EMPTY(CONVULSION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_HEMORRAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMORRAGIA,'1','2')) AND ( .NOT. EMPTY(HEMORRAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_DOLOR_PANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR_PANT,'1','2')) AND ( .NOT. EMPTY(DOLOR_PANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_SUDOR_NOCT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUDOR_NOCT,'1','2')) AND ( .NOT. EMPTY(SUDOR_NOCT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_EPIDIDIMIT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPIDIDIMIT,'1','2')) AND ( .NOT. EMPTY(EPIDIDIMIT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_ORQUITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ORQUITIS,'1','2')) AND ( .NOT. EMPTY(ORQUITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_OLIGURIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OLIGURIA,'1','2')) AND ( .NOT. EMPTY(OLIGURIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_ANIM_EN_CA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANIM_EN_CA,'1','2')) AND ( .NOT. EMPTY(ANIM_EN_CA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_GATOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GATOS,'1','2')) OR ( EMPTY(GATOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_BOVINOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BOVINOS,'1','2')) OR ( EMPTY(BOVINOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_EQUINOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EQUINOS,'1','2')) OR ( EMPTY(EQUINOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_SILVESTRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SILVESTRES,'1','2')) OR ( EMPTY(SILVESTRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_OTR_ANIMAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTR_ANIMAL,'1','2')) OR ( EMPTY(OTR_ANIMAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_PICAD_GARR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PICAD_GARR,'1','2')) AND ( .NOT. EMPTY(PICAD_GARR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_PICAD_PIOJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PICAD_PIOJ,'1','2')) AND ( .NOT. EMPTY(PICAD_PIOJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_PICAD_PULG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PICAD_PULG,'1','2')) AND ( .NOT. EMPTY(PICAD_PULG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_CONS_LECHE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_LECHE,'1','2')) AND ( .NOT. EMPTY(CONS_LECHE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_MULT_PAREJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MULT_PAREJ,'1','2')) AND ( .NOT. EMPTY(MULT_PAREJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_USA_HEMODI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(USA_HEMODI,'1','2')) AND ( .NOT. EMPTY(USA_HEMODI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_USA_DROG_I
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(USA_DROG_I,'1','2')) AND ( .NOT. EMPTY(USA_DROG_I)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_CONT_HBSAG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONT_HBSAG,'1','2')) AND ( .NOT. EMPTY(CONT_HBSAG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_MANIP_SECR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MANIP_SECR,'1','2')) AND ( .NOT. EMPTY(MANIP_SECR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_DET_RAPIDO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DET_RAPIDO,'1','2')) AND ( .NOT. EMPTY(DET_RAPIDO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_CONJ_NO_SU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONJ_NO_SU,'1','2')) AND ( .NOT. EMPTY(CONJ_NO_SU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_40_MANIF_PURP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MANIF_PURP,'1','2')) OR ( EMPTY(MANIF_PURP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_NOM_MADRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_MADRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_TIP_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_MA,'RC','TI','CC','CE','PA','MS','AS')) AND ( .NOT. EMPTY(TIP_IDE_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_NUM_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_MA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_NO_EMBARAZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NO_EMBARAZ>=0 AND ISNUMERIC(NO_EMBARAZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_TOM_SER_SI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOM_SER_SI,'1','2','3')) AND ( .NOT. EMPTY(TOM_SER_SI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_RES_SEROLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_SEROLO,'1','2')) OR ( EMPTY(RES_SEROLO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_TOMA_IGM_T
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOMA_IGM_T,'1','2')) AND ( .NOT. EMPTY(TOMA_IGM_T)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_RES_IGM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_IGM,'1','2')) OR ( EMPTY(RES_IGM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_VAC_ANTIRU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_ANTIRU,'1','2','3')) AND ( .NOT. EMPTY(VAC_ANTIRU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_SIN_RUBEOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIN_RUBEOL,'1','2')) AND ( .NOT. EMPTY(SIN_RUBEOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_OE_BILOGIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OE_BILOGIC,'1','2')) AND ( .NOT. EMPTY(OE_BILOGIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_OE_MEDIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OE_MEDIC,'1','2')) AND ( .NOT. EMPTY(OE_MEDIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_OE_AMB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OE_AMB,'1','2')) AND ( .NOT. EMPTY(OE_AMB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_OE_OTRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OE_OTRA,'1','2')) AND ( .NOT. EMPTY(OE_OTRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_OE_NINGUNA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OE_NINGUNA,'1','2')) AND ( .NOT. EMPTY(OE_NINGUNA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_GEMELO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GEMELO,'1','2')) AND ( .NOT. EMPTY(GEMELO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_NATIVIVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NATIVIVO,'1','2')) AND ( .NOT. EMPTY(NATIVIVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_NATIMORTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NATIMORTO,'1','2')) AND ( .NOT. EMPTY(NATIMORTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_SEM_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((SEM_GES>=22  AND SEM_GES <=45 AND ISNUMERIC(SEM_GES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_EDAD_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((EDAD_GES>=22  AND EDAD_GES <=45 AND ISNUMERIC(EDAD_GES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_STORCH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(STORCH,'1','2')) OR ( EMPTY(STORCH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_ANENCEFALI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANENCEFALI,'1','2')) AND ( .NOT. EMPTY(ANENCEFALI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_ANO_IMPERF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANO_IMPERF,'1','2')) AND ( .NOT. EMPTY(ANO_IMPERF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_ANOM_OCU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANOM_OCU,'1','2')) AND ( .NOT. EMPTY(ANOM_OCU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_ANOTIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANOTIA,'1','2')) AND ( .NOT. EMPTY(ANOTIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_ATRESIA_ES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATRESIA_ES,'1','2')) AND ( .NOT. EMPTY(ATRESIA_ES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_ATRESIA_IN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATRESIA_IN,'1','2')) AND ( .NOT. EMPTY(ATRESIA_IN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_CARDIOPATI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARDIOPATI,'1','2')) AND ( .NOT. EMPTY(CARDIOPATI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_CATARATA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CATARATA,'1','2')) AND ( .NOT. EMPTY(CATARATA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_CEFALOCELE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALOCELE,'1','2')) AND ( .NOT. EMPTY(CEFALOCELE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_CRIPTORQUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRIPTORQUI,'1','2')) AND ( .NOT. EMPTY(CRIPTORQUI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_DEFE_PARED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEFE_PARED,'1','2')) AND ( .NOT. EMPTY(DEFE_PARED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_DISP_ESQUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISP_ESQUE,'1','2')) AND ( .NOT. EMPTY(DISP_ESQUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_ESPINA_BIF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESPINA_BIF,'1','2')) AND ( .NOT. EMPTY(ESPINA_BIF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_FISURA_ORA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FISURA_ORA,'1','2')) AND ( .NOT. EMPTY(FISURA_ORA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_GEMELOS_AC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GEMELOS_AC,'1','2')) AND ( .NOT. EMPTY(GEMELOS_AC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_GENITALES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GENITALES,'1','2')) AND ( .NOT. EMPTY(GENITALES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_HEMANGIOM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMANGIOM,'1','2')) AND ( .NOT. EMPTY(HEMANGIOM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_HEPATOESPL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATOESPL,'1','2')) AND ( .NOT. EMPTY(HEPATOESPL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_HIDROCEFAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIDROCEFAL,'1','2')) AND ( .NOT. EMPTY(HIDROCEFAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_HIPOACUSIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPOACUSIA,'1','2')) AND ( .NOT. EMPTY(HIPOACUSIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_HIPOSPADIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPOSPADIA,'1','2')) AND ( .NOT. EMPTY(HIPOSPADIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_MICROCEFAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MICROCEFAL,'1','2')) AND ( .NOT. EMPTY(MICROCEFAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_POLIDACTI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(POLIDACTI,'1','2')) AND ( .NOT. EMPTY(POLIDACTI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_POLIMALFOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(POLIMALFOR,'1','2')) AND ( .NOT. EMPTY(POLIMALFOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_PURPURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PURPURA,'1','2')) AND ( .NOT. EMPTY(PURPURA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_RED_MIEMBR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RED_MIEMBR,'1','2')) AND ( .NOT. EMPTY(RED_MIEMBR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_SINDACTIL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SINDACTIL,'1','2')) AND ( .NOT. EMPTY(SINDACTIL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_DOWN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOWN,'1','2')) AND ( .NOT. EMPTY(DOWN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_TALIPES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TALIPES,'1','2')) AND ( .NOT. EMPTY(TALIPES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_BAJO_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BAJO_PESO,'1','2')) AND ( .NOT. EMPTY(BAJO_PESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_ANOM_FUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANOM_FUN,'1','2')) AND ( .NOT. EMPTY(ANOM_FUN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_INFEC_CONG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFEC_CONG,'1','2')) AND ( .NOT. EMPTY(INFEC_CONG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_OTRAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRAS,'1','2')) AND ( .NOT. EMPTY(OTRAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_41_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_HIPOTENSIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPOTENSIO,'1','2')) OR ( EMPTY(HIPOTENSIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_HEPATOMEG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATOMEG,'1','2')) OR ( EMPTY(HEPATOMEG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_DESPLAZAMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESPLAZAMI,'1','2')) AND ( .NOT. EMPTY(DESPLAZAMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_FAMANTDNGU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FAMANTDNGU,'1','2','3')) AND ( .NOT. EMPTY(FAMANTDNGU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) OR ( EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_MALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MALGIAS,'1','2')) OR ( EMPTY(MALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) OR ( EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_DOLRRETROO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLRRETROO,'1','2')) OR ( EMPTY(DOLRRETROO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_ARTRALGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRALGIA,'1','2')) OR ( EMPTY(ARTRALGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_ERUPCIONR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ERUPCIONR,'1','2')) OR ( EMPTY(ERUPCIONR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_HEM_MUCOSA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEM_MUCOSA,'1','2')) OR ( EMPTY(HEM_MUCOSA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_HIPOTERMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPOTERMIA,'1','2')) OR ( EMPTY(HIPOTERMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_AUM_HEMATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AUM_HEMATO,'1','2')) OR ( EMPTY(AUM_HEMATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_CAIDA_PLAQ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAIDA_PLAQ,'1','2')) OR ( EMPTY(CAIDA_PLAQ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_ACUM_LIQUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACUM_LIQUI,'1','2')) OR ( EMPTY(ACUM_LIQUI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_EXTRAVASAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EXTRAVASAC,'1','2')) OR ( EMPTY(EXTRAVASAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_HEMORR_HEM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMORR_HEM,'1','2')) OR ( EMPTY(HEMORR_HEM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_DAÑO_ORGAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DAÑO_ORGAN,'1','2')) OR ( EMPTY(DAÑO_ORGAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_CHOQUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CHOQUE,'1','2')) OR ( EMPTY(CHOQUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) OR ( EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_SOMNOLENCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SOMNOLENCI,'1','2')) OR ( EMPTY(SOMNOLENCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_DOLOR_ABDO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR_ABDO,'1','2')) OR ( EMPTY(DOLOR_ABDO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) OR ( EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_MUESTTEJID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUESTTEJID,'1','2')) OR ( EMPTY(MUESTTEJID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_MUESHIGADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUESHIGADO,'1','2')) OR ( EMPTY(MUESHIGADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_MUESBAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUESBAZO,'1','2')) OR ( EMPTY(MUESBAZO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_MUESPULMON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUESPULMON,'1','2')) OR ( EMPTY(MUESPULMON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_MUESCEREBR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUESCEREBR,'1','2')) OR ( EMPTY(MUESCEREBR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_MUESMIOCAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUESMIOCAR,'1','2')) OR ( EMPTY(MUESMIOCAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_MUESMEDULA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUESMEDULA,'1','2')) OR ( EMPTY(MUESMEDULA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_MUESRIÑON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUESRIÑON,'1','2')) OR ( EMPTY(MUESRIÑON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_CLASFINAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLASFINAL,'0','1','2','3')) AND ( .NOT. EMPTY(CLASFINAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_CONDUCTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONDUCTA,'0','1','2','3','4','5')) AND ( .NOT. EMPTY(CONDUCTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_42_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_43_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_43_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_43_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_43_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_43_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_43_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_43_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','4','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_43_FEC_AJU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_AJU))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_EDA_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDA_GES)>= 2  AND  VAL(EDA_GES) <= 42 AND ISNUMERIC(EDA_GES)) OR ( EMPTY(EDA_GES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_TOMASEROLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOMASEROLO,'1','2')) AND ( .NOT. EMPTY(TOMASEROLO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_EG_PRI_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EG_PRI_S)>= 2  AND  VAL(EG_PRI_S) <= 42 AND ISNUMERIC(EG_PRI_S)) OR ( EMPTY(EG_PRI_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_RESSERVDRL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESSERVDRL,'3','4','5','6','7','8','9','10','11','12','13')) OR ( EMPTY(RESSERVDRL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_PRUETREPON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRUETREPON,'1','2')) AND ( .NOT. EMPTY(PRUETREPON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_EG_PR_TREP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EG_PR_TREP)>= 2  AND  VAL(EG_PR_TREP) <= 42 AND ISNUMERIC(EG_PR_TREP)) OR ( EMPTY(EG_PR_TREP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_QUE_PR_TRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUE_PR_TRE,'4','5')) OR ( EMPTY(QUE_PR_TRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_RESULTREPO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULTREPO,'1','2')) OR ( EMPTY(RESULTREPO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_PEN_BENZAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEN_BENZAT,'0','1','2','3')) AND ( .NOT. EMPTY(PEN_BENZAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_TTO_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TTO_CON,'1','2')) AND ( .NOT. EMPTY(TTO_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_CONDENDIAG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONDENDIAG,'1','2','3')) AND ( .NOT. EMPTY(CONDENDIAG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_CON_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_PRE,'1','2')) AND ( .NOT. EMPTY(CON_PRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_NOM_MADRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_MADRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_TIP_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_MA,'RC','TI','CC','CE','PA','MS','AS','PE')) AND ( .NOT. EMPTY(TIP_IDE_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_NUM_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_MA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_RES_GESTAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_GESTAC,'1','3')) AND ( .NOT. EMPTY(RES_GESTAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_NO_PRODUCT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(NO_PRODUCT)>= 1 AND ISNUMERIC(NO_PRODUCT)) AND ( .NOT. EMPTY(NO_PRODUCT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_EDA_GES_NA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDA_GES_NA)>= 22  AND  VAL(EDA_GES_NA) <= 42 AND ISNUMERIC(EDA_GES_NA)) AND ( .NOT. EMPTY(EDA_GES_NA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_RES_SER_MP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_SER_MP,'3','4','5','6','7','8','9','10','11','12','13')) AND ( .NOT. EMPTY(RES_SER_MP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_RESULTSERO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULTSERO,'3','4','5','6','7','8','9','10','11','12','13','14')) OR ( EMPTY(RESULTSERO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_DIA_EMBACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIA_EMBACT,'1','2')) AND ( .NOT. EMPTY(DIA_EMBACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_44_TIEM_RESID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIEM_RESID,'1','2')) OR ( EMPTY(TIEM_RESID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_VIG_ACTIVA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIG_ACTIVA,'1','2')) AND ( .NOT. EMPTY(VIG_ACTIVA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_SINTOMATIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SINTOMATIC,'1','2')) AND ( .NOT. EMPTY(SINTOMATIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_CLAS_CASO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_CASO,'1','2')) AND ( .NOT. EMPTY(CLAS_CASO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_NUEVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NUEVO,'1','2')) AND ( .NOT. EMPTY(NUEVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_RECRUDECE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECRUDECE,'1','2')) AND ( .NOT. EMPTY(RECRUDECE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_TRIMESTRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRIMESTRE,'1','2','3')) OR ( EMPTY(TRIMESTRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_TIPOEXAMEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(TIPOEXAMEN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_TRATAMIENT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRATAMIENT,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(TRATAMIENT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_GAMETOCITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GAMETOCITO,'1','2')) AND ( .NOT. EMPTY(GAMETOCITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_RESP_DIAG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(RESP_DIAG))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_RESULT_EXA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULT_EXA,'1','2')) AND ( .NOT. EMPTY(RESULT_EXA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_FEC_RESULT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_RESULT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_45_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_DESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESP,'1','2')) AND ( .NOT. EMPTY(DESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_CONT_SS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONT_SS,'1','2')) AND ( .NOT. EMPTY(CONT_SS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_ASOC_BROTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASOC_BROTE,'1','2')) AND ( .NOT. EMPTY(ASOC_BROTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_CAPTADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAPTADO,'1','2','3')) AND ( .NOT. EMPTY(CAPTADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_AGENTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGENTE,'17','10','80','81','82','83','84','77','78','79')) OR ( EMPTY(AGENTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_46_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_TIP_DOC_RN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_DOC_RN,'RC','MS','PE','CN')) AND ( .NOT. EMPTY(TIP_DOC_RN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_NO_IDE_RN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NO_IDE_RN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_FECHA_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FECHA_NAC))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_EDAD_RN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((EDAD_RN>=0 AND ISNUMERIC(EDAD_RN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_SEXO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEXO,'M','F')) AND ( .NOT. EMPTY(SEXO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_PESO_NACER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PESO_NACER)>=900  AND  VAL(PESO_NACER) <=2499 AND ISNUMERIC(PESO_NACER)) AND ( .NOT. EMPTY(PESO_NACER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_TALLA_NACE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA_NACE>=30.0  AND TALLA_NACE <=55.0 AND ISNUMERIC(TALLA_NACE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_SEM_GEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SEM_GEST)>=37  AND  VAL(SEM_GEST) <=45 AND ISNUMERIC(SEM_GEST)) AND ( .NOT. EMPTY(SEM_GEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_CLA_PES_NA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLA_PES_NA,'1','2')) AND ( .NOT. EMPTY(CLA_PES_NA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_SIT_ATE_PA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_ATE_PA,'1','2','3','4')) AND ( .NOT. EMPTY(SIT_ATE_PA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_MULT_EMBAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MULT_EMBAR,'1','2','3')) AND ( .NOT. EMPTY(MULT_EMBAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_NUM_EM_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_EM_PRE>= 0  AND NUM_EM_PRE <=20 AND ISNUMERIC(NUM_EM_PRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_NUM_HI_VIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_HI_VIV>= 1  AND NUM_HI_VIV <=20 AND ISNUMERIC(NUM_HI_VIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_NIV_EDU_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NIV_EDU_MA,'1','2','3','4')) OR ( EMPTY(NIV_EDU_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_47_EST_INGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_INGR,0,2,3)) OR (EST_INGR=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_PTE_REMTDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PTE_REMTDA,'1','2')) AND ( .NOT. EMPTY(PTE_REMTDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_TIEM_REMIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TIEM_REMIS)>=0 AND ISNUMERIC(TIEM_REMIS)) OR ( EMPTY(TIEM_REMIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_NUM_GESTAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_GESTAC>=1  AND NUM_GESTAC <=19 AND ISNUMERIC(NUM_GESTAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_NUM_PARVAG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_PARVAG>=0  AND NUM_PARVAG <=19 AND ISNUMERIC(NUM_PARVAG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_NUM_CESARE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_CESARE>=0  AND NUM_CESARE <=19 AND ISNUMERIC(NUM_CESARE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_NUM_ABORTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_ABORTO>=0  AND NUM_ABORTO <=19 AND ISNUMERIC(NUM_ABORTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_NUM_MOLAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_MOLAS>=0  AND NUM_MOLAS <=19 AND ISNUMERIC(NUM_MOLAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_NUM_ECTOPI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_ECTOPI>=0  AND NUM_ECTOPI <=19 AND ISNUMERIC(NUM_ECTOPI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_NUM_MUERTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_MUERTO>=0  AND NUM_MUERTO <=19 AND ISNUMERIC(NUM_MUERTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_NUM_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_VIVOS>=0  AND NUM_VIVOS <=19 AND ISNUMERIC(NUM_VIVOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_NO_CON_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NO_CON_PRE>=0  AND NO_CON_PRE <=50 AND ISNUMERIC(NO_CON_PRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_SEM_C_PREN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((SEM_C_PREN>=0  AND SEM_C_PREN <=40 AND ISNUMERIC(SEM_C_PREN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_TERM_GESTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TERM_GESTA,'1','2','3','4','5')) AND ( .NOT. EMPTY(TERM_GESTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_MOC_REL_TG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MOC_REL_TG,'1','2','3')) AND ( .NOT. EMPTY(MOC_REL_TG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_ECLAMPSIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ECLAMPSIA,'1','2')) AND ( .NOT. EMPTY(ECLAMPSIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_CHOQ_SEPTI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CHOQ_SEPTI,'1','2')) AND ( .NOT. EMPTY(CHOQ_SEPTI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_CHOQ_HIPOV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CHOQ_HIPOV,'1','2')) AND ( .NOT. EMPTY(CHOQ_HIPOV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_FALLA_CARD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_CARD,'1','2')) AND ( .NOT. EMPTY(FALLA_CARD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_FALLA_RENA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_RENA,'1','2')) AND ( .NOT. EMPTY(FALLA_RENA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_FALLA_HEPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_HEPA,'1','2')) AND ( .NOT. EMPTY(FALLA_HEPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_FALLA_CERE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_CERE,'1','2')) AND ( .NOT. EMPTY(FALLA_CERE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_FALLA_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_RESP,'1','2')) AND ( .NOT. EMPTY(FALLA_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_FALLA_COAG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_COAG,'1','2')) AND ( .NOT. EMPTY(FALLA_COAG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_CIR_ADICIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CIR_ADICIO,'1','2')) AND ( .NOT. EMPTY(CIR_ADICIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_TTL_CRITER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TTL_CRITER>=1  AND TTL_CRITER <=32 AND ISNUMERIC(TTL_CRITER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_DIAS_HOSPI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((DIAS_HOSPI>=0 AND ISNUMERIC(DIAS_HOSPI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_CIR_ADIC_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CIR_ADIC_1,'1','2','3','4')) OR ( EMPTY(CIR_ADIC_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_CIR_ADIC_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CIR_ADIC_2,'1','2','3','4')) OR ( EMPTY(CIR_ADIC_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_CAUS_PRINC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CAUS_PRINC))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_CAUS_AGRUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CAUS_AGRUP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_PRECLAMPSI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRECLAMPSI,'1','2')) AND ( .NOT. EMPTY(PRECLAMPSI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_RUPT_UTERI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RUPT_UTERI,'1','2')) AND ( .NOT. EMPTY(RUPT_UTERI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_48_EGRESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EGRESO,'1','2')) OR ( EMPTY(EGRESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_16
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_16,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_16)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_15
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_15,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_15)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_13
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_13,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_13)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_12
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_12,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_12)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_11
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_11,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_11)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_21
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_21,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_21)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_22
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_22,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_22)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_23
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_23,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_23)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_25
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_25,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_25)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_26
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_26,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_26)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_36
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_36,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_36)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_DEAN_46
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEAN_46,'0','1','2','3','4','5','9')) AND ( .NOT. EMPTY(DEAN_46)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_CLAS_LES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_LES,'0','1','2','3','4','5')) AND ( .NOT. EMPTY(CLAS_LES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_CONS_AGUA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_AGUA,'1','2','3','4','5')) AND ( .NOT. EMPTY(CONS_AGUA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_ING_CREM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ING_CREM,'1','2')) AND ( .NOT. EMPTY(ING_CREM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_T_FLOUR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(T_FLOUR,'1','2')) AND ( .NOT. EMPTY(T_FLOUR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_LACT_MATER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LACT_MATER,'1','2','3')) OR ( EMPTY(LACT_MATER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_49_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_INYECTABLE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INYECTABLE,'1','2','3')) OR ( EMPTY(INYECTABLE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_RES_NITRAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_NITRAT,'1','2')) OR ( EMPTY(RES_NITRAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_RES_PROPOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_PROPOR,'1','2')) OR ( EMPTY(RES_PROPOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_RES_BATECM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_BATECM,'1','2')) OR ( EMPTY(RES_BATECM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_RES_PRO_AG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_PRO_AG,'1','2')) OR ( EMPTY(RES_PRO_AG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_RES_PR_MOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_PR_MOL,'1','2')) OR ( EMPTY(RES_PR_MOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_TIPO_TB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_TB,'1','2')) AND ( .NOT. EMPTY(TIPO_TB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_LOCALIZA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LOCALIZA,'1','2','3','4','5','7','8','9','10','11','12')) OR ( EMPTY(LOCALIZA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_CLAS_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_ANT,'1','2')) AND ( .NOT. EMPTY(CLAS_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_CLAS_MED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_MED,'1','3','4')) AND ( .NOT. EMPTY(CLAS_MED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_CLAS_ING
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_ING,'10','11','12','9')) OR ( EMPTY(CLAS_ING)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_FEC_CONF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_CONF))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_BACILOSC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BACILOSC,'1','2')) AND ( .NOT. EMPTY(BACILOSC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_RES_BACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_BACI,'1','2','3','4')) OR ( EMPTY(RES_BACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_CULTIVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CULTIVO,'1','2')) AND ( .NOT. EMPTY(CULTIVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_RES_CULT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_CULT,'1','2','3','4','5','6','7')) OR ( EMPTY(RES_CULT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_PSF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PSF,'1','2')) OR ( EMPTY(PSF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_RES_PSF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_PSF,'1','2','3','4','5')) OR ( EMPTY(RES_PSF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_PSF_1_LINE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PSF_1_LINE,'1','2')) OR ( EMPTY(PSF_1_LINE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_ESTREPTOMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTREPTOMI,'1','2','3')) OR ( EMPTY(ESTREPTOMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_ISONIAZIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ISONIAZIDA,'1','2','3')) OR ( EMPTY(ISONIAZIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_RIFAMPI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RIFAMPI,'1','2','3')) OR ( EMPTY(RIFAMPI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_ETAMBUTOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ETAMBUTOL,'1','2','3')) OR ( EMPTY(ETAMBUTOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_PIRAZINAMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIRAZINAMI,'1','2','3')) OR ( EMPTY(PIRAZINAMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_PSF_2_LINE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PSF_2_LINE,'1','2')) OR ( EMPTY(PSF_2_LINE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_TIPO_RESIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_RESIS,'1','2','3','4','6','7','8')) AND ( .NOT. EMPTY(TIPO_RESIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_NITRATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NITRATO,'1','2')) AND ( .NOT. EMPTY(NITRATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_PROPOR_LJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROPOR_LJ,'1','2')) AND ( .NOT. EMPTY(PROPOR_LJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_MGIT_960
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MGIT_960,'1','2')) AND ( .NOT. EMPTY(MGIT_960)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_P_AGAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(P_AGAR,'1','2')) AND ( .NOT. EMPTY(P_AGAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_P_MOLEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(P_MOLEC,'1','2')) AND ( .NOT. EMPTY(P_MOLEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_NOM_PMOLEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NOM_PMOLEC,'1','2','3')) OR ( EMPTY(NOM_PMOLEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_FACT_RIESG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FACT_RIESG,'1','2')) AND ( .NOT. EMPTY(FACT_RIESG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_CONTAC_PAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONTAC_PAC,'1','2')) OR ( EMPTY(CONTAC_PAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_FARMACOD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FARMACOD,'1','2')) OR ( EMPTY(FARMACOD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_OTR_INMUNO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTR_INMUNO,'1','2')) OR ( EMPTY(OTR_INMUNO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_VIV_TBFAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIV_TBFAR,'1','2')) OR ( EMPTY(VIV_TBFAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_TRAT_IRRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAT_IRRE,'1','2')) OR ( EMPTY(TRAT_IRRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_MENOS3MED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MENOS3MED,'1','2')) OR ( EMPTY(MENOS3MED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_OTRO_FR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRO_FR,'1','2')) OR ( EMPTY(OTRO_FR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_COOM_CESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COOM_CESP,'1','2')) AND ( .NOT. EMPTY(COOM_CESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_DIABETES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIABETES,'1','2')) OR ( EMPTY(DIABETES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_SILICOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SILICOSIS,'1','2')) OR ( EMPTY(SILICOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_E_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(E_RENAL,'1','2')) OR ( EMPTY(E_RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_EPOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPOC,'1','2')) OR ( EMPTY(EPOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_E_HEPAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(E_HEPAT,'1','2')) OR ( EMPTY(E_HEPAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_CANCER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CANCER,'1','2')) OR ( EMPTY(CANCER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_ARTRITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRITIS,'1','2')) OR ( EMPTY(ARTRITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_DESNUTR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESNUTR,'1','2')) OR ( EMPTY(DESNUTR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIH,'1','2')) OR ( EMPTY(VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_OTR_COOM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTR_COOM,'1','2')) OR ( EMPTY(OTR_COOM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_CONSEJERIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONSEJERIA,'1','2')) OR ( EMPTY(CONSEJERIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_PR_VOL_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PR_VOL_VIH,'1','2')) OR ( EMPTY(PR_VOL_VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_COINF_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COINF_VIH,'1','2','3')) OR ( EMPTY(COINF_VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_TRIMETRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRIMETRO,'1','2','3')) OR ( EMPTY(TRIMETRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_TRAT_ARV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAT_ARV,'1','2','3')) OR ( EMPTY(TRAT_ARV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_PESO_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_ACT>=2  AND PESO_ACT <=250.0 AND ISNUMERIC(PESO_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_TALLA_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA_ACT>=0.20  AND TALLA_ACT <=2.50 AND ISNUMERIC(TALLA_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_QUINOLAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUINOLAS,'1','2','3')) OR ( EMPTY(QUINOLAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_50_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_EVI_MLEGAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EVI_MLEGAL)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','EVI_MLEGAL'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EVI_MLEGAL,'1','2')) OR ( EMPTY(EVI_MLEGAL)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ESTADOTRAN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ESTADOTRAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_NATURALEZA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(NATURALEZA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','NATURALEZA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(NATURALEZA,'1','2','3')) OR ( EMPTY(NATURALEZA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ACTIVIDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ACTIVIDAD)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ACTIVIDAD'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ACTIVIDAD,'13','24','26','28','29','30','31','32','33')) AND ( .NOT. EMPTY(ACTIVIDAD)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_CONSUM_SPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CONSUM_SPA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','CONSUM_SPA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CONSUM_SPA,'1','2')) AND ( .NOT. EMPTY(CONSUM_SPA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_MUJER_CABF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(MUJER_CABF)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','MUJER_CABF'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(MUJER_CABF,'1','2')) AND ( .NOT. EMPTY(MUJER_CABF)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ANTEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ANTEC)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ANTEC'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ANTEC,'1','2')) AND ( .NOT. EMPTY(ANTEC)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_SEXO_AGRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SEXO_AGRE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','SEXO_AGRE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SEXO_AGRE,'M','F','I')) OR ( EMPTY(SEXO_AGRE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_R_FAM_VIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(R_FAM_VIC)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','R_FAM_VIC'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(R_FAM_VIC,'9','10','22','23','24','25')) OR ( EMPTY(R_FAM_VIC)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_CONV_AGRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CONV_AGRE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','CONV_AGRE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CONV_AGRE,'1','2')) AND ( .NOT. EMPTY(CONV_AGRE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_R_NOFILIAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(R_NOFILIAR)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','R_NOFILIAR'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(R_NOFILIAR,'1','2','3','4','6','7','8','10','11','12','13')) OR ( EMPTY(R_NOFILIAR)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ARMAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ARMAS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ARMAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ARMAS,'1','2','3','4','11','12','13','14','15','16')) OR ( EMPTY(ARMAS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_CARA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_CARA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_CARA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_CARA,'1','2')) OR ( EMPTY(QUE_CARA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_CUELLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_CUELLO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_CUELLO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_CUELLO,'1','2')) OR ( EMPTY(QUE_CUELLO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_MANO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MANO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_MANO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MANO,'1','2')) OR ( EMPTY(QUE_MANO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_PIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_PIE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_PIE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_PIE,'1','2')) OR ( EMPTY(QUE_PIE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_PLIEGU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_PLIEGU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_PLIEGU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_PLIEGU,'1','2')) OR ( EMPTY(QUE_PLIEGU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_GENITA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_GENITA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_GENITA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_GENITA,'1','2')) OR ( EMPTY(QUE_GENITA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_TRONCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_TRONCO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_TRONCO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_TRONCO,'1','2')) OR ( EMPTY(QUE_TRONCO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_MIESUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MIESUP)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_MIESUP'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MIESUP,'1','2')) OR ( EMPTY(QUE_MIESUP)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_MIEINF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MIEINF)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_MIEINF'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MIEINF,'1','2')) OR ( EMPTY(QUE_MIEINF)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_CLA_GRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CLA_GRA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','CLA_GRA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CLA_GRA,'1','2','3')) OR ( EMPTY(CLA_GRA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_EXT_QUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EXT_QUE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','EXT_QUE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EXT_QUE,'1','2','3')) OR ( EMPTY(EXT_QUE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_SUST_VICT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SUST_VICT)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','SUST_VICT'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SUST_VICT,'1','2')) AND ( .NOT. EMPTY(SUST_VICT)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_FEC_HECHO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FEC_HECHO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','FEC_HECHO'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(FEC_HECHO))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ESCENARIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ESCENARIO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ESCENARIO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ESCENARIO,'1','2','3','4','7','8','9','10','11','12')) AND ( .NOT. EMPTY(ESCENARIO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ZONA_CONF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ZONA_CONF)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ZONA_CONF'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ZONA_CONF,'1','2')) AND ( .NOT. EMPTY(ZONA_CONF)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AC_MENTAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AC_MENTAL)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','AC_MENTAL'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AC_MENTAL,'1','2')) AND ( .NOT. EMPTY(AC_MENTAL)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_SP_ITS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SP_ITS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','SP_ITS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SP_ITS,'1','2')) OR ( EMPTY(SP_ITS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_PROF_HEP_B
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PROF_HEP_B)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','PROF_HEP_B'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(PROF_HEP_B,'1','2')) OR ( EMPTY(PROF_HEP_B)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_PROF_OTRAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PROF_OTRAS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','PROF_OTRAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(PROF_OTRAS,'1','2')) OR ( EMPTY(PROF_OTRAS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AC_ANTICON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AC_ANTICON)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','AC_ANTICON'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AC_ANTICON,'1','2')) OR ( EMPTY(AC_ANTICON)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AC_IVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AC_IVE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','AC_IVE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AC_IVE,'1','2')) OR ( EMPTY(AC_IVE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_INF_AUT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(INF_AUT)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','INF_AUT'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(INF_AUT,'1','2')) AND ( .NOT. EMPTY(INF_AUT)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_REMIT_PROT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(REMIT_PROT)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','REMIT_PROT'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(REMIT_PROT,'1','2')) AND ( .NOT. EMPTY(REMIT_PROT)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AJUSTE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','AJUSTE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_NAT_VIOSEX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(NAT_VIOSEX)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','NAT_VIOSEX'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(NAT_VIOSEX,'5','6','7','10','12','14','15')) OR ( EMPTY(NAT_VIOSEX)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ORIENT_SEX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ORIENT_SEX)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ORIENT_SEX'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ORIENT_SEX,'1','2','5','6')) AND ( .NOT. EMPTY(ORIENT_SEX)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_IDENT_GENE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(IDENT_GENE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','IDENT_GENE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(IDENT_GENE,'1','2','3')) AND ( .NOT. EMPTY(IDENT_GENE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AMBITO_LUG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMBITO_LUG)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','AMBITO_LUG'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMBITO_LUG,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(AMBITO_LUG)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_TIPO_GRUPO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(TIPO_GRUPO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','TIPO_GRUPO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(TIPO_GRUPO,'1','2')) OR ( EMPTY(TIPO_GRUPO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_P_NOMBRE_M
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(P_NOMBRE_M))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_P_APELL_M
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(P_APELL_M))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_TIDE_MAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIDE_MAD,'RC','TI','CC','CE','PA','MS','AS','PE')) AND ( .NOT. EMPTY(TIDE_MAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_NUMID_MAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUMID_MAD))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_N_EDUCAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(N_EDUCAT,'1','2','3','4','5')) OR ( EMPTY(N_EDUCAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_ESTRATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTRATO,'1','2','3','4','5','6')) OR ( EMPTY(ESTRATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_MENORES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((MENORES>=0  AND MENORES <= 20 AND ISNUMERIC(MENORES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_SITIO_DEF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SITIO_DEF,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(SITIO_DEF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_PESO_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PESO_NAC)>=900  AND  VAL(PESO_NAC) <=5000 AND ISNUMERIC(PESO_NAC)) OR ( EMPTY(PESO_NAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_TALLA_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TALLA_NAC)>= 30.0  AND  VAL(TALLA_NAC) <= 55.0 AND ISNUMERIC(TALLA_NAC)) OR ( EMPTY(TALLA_NAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_EDAD_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD_GES)>= 0  AND  VAL(EDAD_GES) <= 45 AND ISNUMERIC(EDAD_GES)) OR ( EMPTY(EDAD_GES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_CREC_DLLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CREC_DLLO,'1','2')) AND ( .NOT. EMPTY(CREC_DLLO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_PESO_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_ACT>=1  AND PESO_ACT <= 30 AND ISNUMERIC(PESO_ACT)) OR (PESO_ACT=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_TALLA_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA_ACT>=45.0  AND TALLA_ACT <= 120.0 AND ISNUMERIC(TALLA_ACT)) OR (TALLA_ACT=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_ESQ_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESQ_VAC,'1','2','3')) AND ( .NOT. EMPTY(ESQ_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_CARNE_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARNE_VAC,'1','2')) OR ( EMPTY(CARNE_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_CLAS_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_PESO,'5','6','7')) AND ( .NOT. EMPTY(CLAS_PESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_CLAS_TALLA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_TALLA,'1','4','3')) AND ( .NOT. EMPTY(CLAS_TALLA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_C_PES_TAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(C_PES_TAL))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_EDEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EDEMA,'1','2')) AND ( .NOT. EMPTY(EDEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_DELGADEZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DELGADEZ,'1','2')) AND ( .NOT. EMPTY(DELGADEZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_PIEL_RESE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIEL_RESE,'1','2')) AND ( .NOT. EMPTY(PIEL_RESE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_HIPERPIGM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPERPIGM,'1','2')) AND ( .NOT. EMPTY(HIPERPIGM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_LES_CABEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LES_CABEL,'1','2')) AND ( .NOT. EMPTY(LES_CABEL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_PALIDEZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PALIDEZ,'1','2')) AND ( .NOT. EMPTY(PALIDEZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_C_DIRECT_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(C_DIRECT_A))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_C_DETERM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(C_DETERM,'1','2','3')) AND ( .NOT. EMPTY(C_DETERM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_DES_CBMTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DES_CBMTE,'1','2')) OR ( EMPTY(DES_CBMTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_DES_CPAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DES_CPAT,'1','2')) OR ( EMPTY(DES_CPAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_DEMORA_1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEMORA_1,'1','2')) OR ( EMPTY(DEMORA_1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_DEMORA_2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEMORA_2,'1','2')) OR ( EMPTY(DEMORA_2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_DEMORA_3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEMORA_3,'1','2')) OR ( EMPTY(DEMORA_3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_DEMORA_4
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEMORA_4,'1','2')) OR ( EMPTY(DEMORA_4)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_52_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_TRIM_GEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRIM_GEST,'1','2','3')) AND ( .NOT. EMPTY(TRIM_GEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_CIR_BRAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((CIR_BRAZO>=8.0  AND CIR_BRAZO <=50.0 AND ISNUMERIC(CIR_BRAZO)) OR (CIR_BRAZO=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_TAMI_ALERT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(TAMI_ALERT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_N_GES_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_GES_ACT>=1  AND N_GES_ACT <=25 AND ISNUMERIC(N_GES_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_S_GES_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(S_GES_ACT,'1','2','3')) AND ( .NOT. EMPTY(S_GES_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_N_S_GES_AC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_S_GES_AC>=3  AND N_S_GES_AC <=45 AND ISNUMERIC(N_S_GES_AC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_PESO_PREG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_PREG>=30.0  AND PESO_PREG <=200.0 AND ISNUMERIC(PESO_PREG)) OR (PESO_PREG=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_PESO_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_ACT>=30.0  AND PESO_ACT <=200.0 AND ISNUMERIC(PESO_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_TALLA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA>=120.0  AND TALLA <=220.0 AND ISNUMERIC(TALLA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_CLAS_IMC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_IMC,'1','2','3','4')) AND ( .NOT. EMPTY(CLAS_IMC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_EDA_GES_IN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((EDA_GES_IN>=2  AND EDA_GES_IN <=45 AND ISNUMERIC(EDA_GES_IN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_N_CONT_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_CONT_PRE>=1  AND N_CONT_PRE <=25 AND ISNUMERIC(N_CONT_PRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_MULT_EMB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MULT_EMB,'1','2','3')) AND ( .NOT. EMPTY(MULT_EMB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_HEMOGLOBIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((HEMOGLOBIN>=4.0  AND HEMOGLOBIN <=20.0 AND ISNUMERIC(HEMOGLOBIN)) OR (HEMOGLOBIN=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_HEMO_ALERT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(HEMO_ALERT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_MICR_CALC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MICR_CALC,'1','2')) AND ( .NOT. EMPTY(MICR_CALC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_MICR_HIERR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MICR_HIERR,'1','2')) AND ( .NOT. EMPTY(MICR_HIERR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_MICR_ACID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MICR_ACID,'1','2')) AND ( .NOT. EMPTY(MICR_ACID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_53_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_TIP_TUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_TUB,'1','2')) AND ( .NOT. EMPTY(TIP_TUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_LOCTBREXTR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LOCTBREXTR,'1','2','3','4','5','7','8','9','10','11','12')) OR ( EMPTY(LOCTBREXTR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_CLASCASO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLASCASO,'1','2','3','4','5')) AND ( .NOT. EMPTY(CLASCASO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_CIC_VCG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CIC_VCG,'1','2')) AND ( .NOT. EMPTY(CIC_VCG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_VCNBCGCN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VCNBCGCN,'1','2')) AND ( .NOT. EMPTY(VCNBCGCN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_TRAB_SALUD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAB_SALUD,'1','2')) AND ( .NOT. EMPTY(TRAB_SALUD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_PREV_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PREV_VIH,'1','2')) AND ( .NOT. EMPTY(PREV_VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_CONSPREVIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONSPREVIH,'1','2')) OR ( EMPTY(CONSPREVIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_PRUEBDIAGN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRUEBDIAGN,'1','2')) OR ( EMPTY(PRUEBDIAGN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_TER_PREV_T
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TER_PREV_T,'1','2')) OR ( EMPTY(TER_PREV_T)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_TRAT_ANTIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAT_ANTIR,'1','2')) OR ( EMPTY(TRAT_ANTIR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_PESO_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_ACT>=2.0  AND PESO_ACT <=250.0 AND ISNUMERIC(PESO_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_TALLA_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA_ACT>=0.20  AND TALLA_ACT <=2.5 AND ISNUMERIC(TALLA_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_INI_TRAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INI_TRAT,'1','2')) AND ( .NOT. EMPTY(INI_TRAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_BACILOSCOP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BACILOSCOP,'1','2')) AND ( .NOT. EMPTY(BACILOSCOP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_RES_BK
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_BK,'1','2','3','4')) OR ( EMPTY(RES_BK)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_REA_CUL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REA_CUL,'1','2')) AND ( .NOT. EMPTY(REA_CUL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_RESCULTIVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESCULTIVO,'1','2')) OR ( EMPTY(RESCULTIVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_RES_CULT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_CULT,'1','2','3','4','5','6','7')) OR ( EMPTY(RES_CULT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_HISTOPATOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HISTOPATOL,'1','2')) AND ( .NOT. EMPTY(HISTOPATOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_RESHISTOPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESHISTOPA,'1','2')) OR ( EMPTY(RESHISTOPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_PRUEB_MOLE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRUEB_MOLE,'1','2')) AND ( .NOT. EMPTY(PRUEB_MOLE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_NOM_PMOLEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NOM_PMOLEC,'1','2','3')) OR ( EMPTY(NOM_PMOLEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_CLI_PTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLI_PTA,'1','2')) AND ( .NOT. EMPTY(CLI_PTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_NEX_EPI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NEX_EPI,'1','2')) AND ( .NOT. EMPTY(NEX_EPI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_REDIOLOGIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REDIOLOGIC,'1','2')) AND ( .NOT. EMPTY(REDIOLOGIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_ADA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADA,'1','2')) AND ( .NOT. EMPTY(ADA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_TUBERCULIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TUBERCULIN,'1','2')) AND ( .NOT. EMPTY(TUBERCULIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_RES_PRUVIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_PRUVIH,'1','2','3')) OR ( EMPTY(RES_PRUVIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_DIABETES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIABETES,'1','2')) AND ( .NOT. EMPTY(DIABETES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_SILICOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SILICOSIS,'1','2')) AND ( .NOT. EMPTY(SILICOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_54_ENFE_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENFE_RENAL,'1','2')) AND ( .NOT. EMPTY(ENFE_RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_TIPO_CA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_CA,'1','2','3','4','5','6','7','8','9','10','11','12','13','14')) AND ( .NOT. EMPTY(TIPO_CA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_CLA_RIESGO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLA_RIESGO,'1','2','3')) OR ( EMPTY(CLA_RIESGO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_RECAIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECAIDA,'1','2')) AND ( .NOT. EMPTY(RECAIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_DX_OTRO_CA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DX_OTRO_CA,'1','2','3','4','5','6','7','8')) OR ( EMPTY(DX_OTRO_CA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_CAUS_MUE_D
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUS_MUE_D,'1','2','3')) OR ( EMPTY(CAUS_MUE_D)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_SIT_DEF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_DEF,'1','2','3','4')) OR ( EMPTY(SIT_DEF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_REM_INST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REM_INST,'1','2')) AND ( .NOT. EMPTY(REM_INST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_55_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_EMB_MULTIP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EMB_MULTIP,'1','2')) AND ( .NOT. EMPTY(EMB_MULTIP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_NATIVIVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NATIVIVO,'1','2','3')) AND ( .NOT. EMPTY(NATIVIVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_EDAD_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD_GES)>= 10  AND  VAL(EDAD_GES) <=45 AND ISNUMERIC(EDAD_GES)) OR ( EMPTY(EDAD_GES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_PESO_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PESO_NAC)>=50  AND  VAL(PESO_NAC) <=6000 AND ISNUMERIC(PESO_NAC)) OR ( EMPTY(PESO_NAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_EDADGE_DIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDADGE_DIA)>= 1  AND  VAL(EDADGE_DIA) <= 45 AND ISNUMERIC(EDADGE_DIA)) AND ( .NOT. EMPTY(EDADGE_DIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_PATOL_CRON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PATOL_CRON,'1','2')) AND ( .NOT. EMPTY(PATOL_CRON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_NOM_MADRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_MADRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_TIP_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_MA,'RC','TI','CC','CE','PA','MS','AS','PE')) AND ( .NOT. EMPTY(TIP_IDE_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_NUM_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_MA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_EDAD_MADRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((EDAD_MADRE>=9 AND ISNUMERIC(EDAD_MADRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_NO_EMBARAZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NO_EMBARAZ>=1  AND NO_EMBARAZ <=30 AND ISNUMERIC(NO_EMBARAZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_NO_NAC_VIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NO_NAC_VIV>= 0  AND NO_NAC_VIV <= 20 AND ISNUMERIC(NO_NAC_VIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_NO_ABORTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NO_ABORTOS>= 0 AND ISNUMERIC(NO_ABORTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_NO_MORTINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NO_MORTINA>= 0 AND ISNUMERIC(NO_MORTINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_DIAGNOSTIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIAGNOSTIC,'1','2')) AND ( .NOT. EMPTY(DIAGNOSTIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_STORCH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(STORCH,'1','2')) AND ( .NOT. EMPTY(STORCH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_T4_TOTAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(T4_TOTAL,'1','2')) AND ( .NOT. EMPTY(T4_TOTAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_RES_T4_TOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_T4_TOT,'2','3')) OR ( EMPTY(RES_T4_TOT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_T4_LIBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(T4_LIBRE,'1','2')) AND ( .NOT. EMPTY(T4_LIBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_RES_T4_LIB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_T4_LIB,'2','3')) OR ( EMPTY(RES_T4_LIB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_PER_CEFALI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PER_CEFALI>=10.0  AND PER_CEFALI <= 60.0 AND ISNUMERIC(PER_CEFALI)) OR (PER_CEFALI=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_TSH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TSH,'1','2')) AND ( .NOT. EMPTY(TSH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_56_RES_TSH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_TSH,'1','2')) OR ( EMPTY(RES_TSH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_VAC_HEP_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_HEP_A,'1','2','3')) AND ( .NOT. EMPTY(VAC_HEP_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_NUM_DOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(NUM_DOSIS)>=1  AND  VAL(NUM_DOSIS) <=3 AND ISNUMERIC(NUM_DOSIS)) OR ( EMPTY(NUM_DOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_FUENTE_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FUENTE_VAC,'1','2','3')) AND ( .NOT. EMPTY(FUENTE_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_MALESTAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MALESTAR,'1','2')) AND ( .NOT. EMPTY(MALESTAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_ASTENIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASTENIA,'1','2')) AND ( .NOT. EMPTY(ASTENIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_ADINAMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADINAMIA,'1','2')) AND ( .NOT. EMPTY(ADINAMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) AND ( .NOT. EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) AND ( .NOT. EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_HEPATOMEGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATOMEGA,'1','2')) AND ( .NOT. EMPTY(HEPATOMEGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_MIALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIALGIAS,'1','2')) AND ( .NOT. EMPTY(MIALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_ICTERICIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ICTERICIA,'1','2')) AND ( .NOT. EMPTY(ICTERICIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_DABDOMINAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DABDOMINAL,'1','2')) AND ( .NOT. EMPTY(DABDOMINAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_PRURITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRURITO,'1','2')) AND ( .NOT. EMPTY(PRURITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_COLURIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COLURIA,'1','2')) AND ( .NOT. EMPTY(COLURIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_ERUPCION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ERUPCION,'1','2')) AND ( .NOT. EMPTY(ERUPCION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_ACOLIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACOLIA,'1','2')) AND ( .NOT. EMPTY(ACOLIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_COMPLICACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMPLICACI,'1','2')) AND ( .NOT. EMPTY(COMPLICACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_ICT_PROLON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ICT_PROLON,'1','2')) OR ( EMPTY(ICT_PROLON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_HEP_COLEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEP_COLEST,'1','2')) OR ( EMPTY(HEP_COLEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_HEP_RECIDI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEP_RECIDI,'1','2')) OR ( EMPTY(HEP_RECIDI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_FALLA_HEPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_HEPA,'1','2')) OR ( EMPTY(FALLA_HEPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_COM_NEUROL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_NEUROL,'1','2')) OR ( EMPTY(COM_NEUROL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_SEPSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEPSIS,'1','2')) OR ( EMPTY(SEPSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_VIAJO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIAJO,'1','2')) AND ( .NOT. EMPTY(VIAJO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_ANTEC_CONT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANTEC_CONT,'1','2')) AND ( .NOT. EMPTY(ANTEC_CONT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_LUGAR_ASOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LUGAR_ASOC,'1','2','3','4','5')) OR ( EMPTY(LUGAR_ASOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_ASOC_BROTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASOC_BROTE,'1','2')) OR ( EMPTY(ASOC_BROTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_ORIG_HIDRI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ORIG_HIDRI,'1','2')) OR ( EMPTY(ORIG_HIDRI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_FUEN_PROGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FUEN_PROGA,'1','2')) OR ( EMPTY(FUEN_PROGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_MON_RAP_VA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MON_RAP_VA,'1','2')) OR ( EMPTY(MON_RAP_VA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_VAC_BLOQUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_BLOQUE,'1','2')) OR ( EMPTY(VAC_BLOQUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_SEG_CONTAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEG_CONTAC,'1','2')) OR ( EMPTY(SEG_CONTAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_57_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_SIT_DEF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_DEF,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(SIT_DEF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_CONVIVENCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONVIVENCI,'1','2','3','4')) AND ( .NOT. EMPTY(CONVIVENCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_ESCOLARIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESCOLARIDA,'1','2','3','4','5')) AND ( .NOT. EMPTY(ESCOLARIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_REG_FEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REG_FEC,'1','2','3','4','5','6','7','8','9')) AND ( .NOT. EMPTY(REG_FEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_GESTACIONE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(GESTACIONE)>=1  AND  VAL(GESTACIONE) <= 20 AND ISNUMERIC(GESTACIONE)) AND ( .NOT. EMPTY(GESTACIONE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_PARTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PARTOS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_CESAREAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CESAREAS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_ABORTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ABORTOS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_MUERTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(MUERTOS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(VIVOS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_NINGUNO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NINGUNO,'1','2')) AND ( .NOT. EMPTY(NINGUNO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_HIP_CRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIP_CRO,'1','2')) AND ( .NOT. EMPTY(HIP_CRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_CARDIOPATÍ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARDIOPATÍ,'1','2')) AND ( .NOT. EMPTY(CARDIOPATÍ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_DIABETES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIABETES,'1','2')) AND ( .NOT. EMPTY(DIABETES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_MOLA_HIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MOLA_HIDA,'1','2')) AND ( .NOT. EMPTY(MOLA_HIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_PRETERMINO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRETERMINO,'1','2')) AND ( .NOT. EMPTY(PRETERMINO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_BAJO_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BAJO_PESO,'1','2')) AND ( .NOT. EMPTY(BAJO_PESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_MACROSOMIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MACROSOMIC,'1','2')) AND ( .NOT. EMPTY(MACROSOMIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_TRANSTORNO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRANSTORNO,'1','2')) AND ( .NOT. EMPTY(TRANSTORNO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_OBESIDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OBESIDAD,'1','2')) AND ( .NOT. EMPTY(OBESIDAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_D_CRONICA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(D_CRONICA,'1','2')) AND ( .NOT. EMPTY(D_CRONICA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_INTER_GENE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INTER_GENE,'1','2')) AND ( .NOT. EMPTY(INTER_GENE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_ITS_DIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ITS_DIS,'1','2')) AND ( .NOT. EMPTY(ITS_DIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_VIH_SIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIH_SIDA,'1','2')) AND ( .NOT. EMPTY(VIH_SIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_OTRAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRAS,'1','2')) AND ( .NOT. EMPTY(OTRAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_RH_NEGATIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RH_NEGATIV,'1','2')) AND ( .NOT. EMPTY(RH_NEGATIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_TABAQUISMO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TABAQUISMO,'1','2')) AND ( .NOT. EMPTY(TABAQUISMO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_ALCOHOLISM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALCOHOLISM,'1','2')) AND ( .NOT. EMPTY(ALCOHOLISM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_PSICOACTIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PSICOACTIV,'1','2')) AND ( .NOT. EMPTY(PSICOACTIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_DEF_CONDIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEF_CONDIC,'1','2')) AND ( .NOT. EMPTY(DEF_CONDIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_SIFILIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIFILIS,'1','2')) AND ( .NOT. EMPTY(SIFILIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_HEPATITISB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATITISB,'1','2')) AND ( .NOT. EMPTY(HEPATITISB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_OTROS_FR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS_FR,'1','2')) AND ( .NOT. EMPTY(OTROS_FR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_GINGIVITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GINGIVITIS,'1','2')) AND ( .NOT. EMPTY(GINGIVITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_PREECLAMPS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PREECLAMPS,'1','2')) AND ( .NOT. EMPTY(PREECLAMPS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_ECLAMPSIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ECLAMPSIA,'1','2')) AND ( .NOT. EMPTY(ECLAMPSIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_SIN_HELLP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIN_HELLP,'1','2')) AND ( .NOT. EMPTY(SIN_HELLP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_D_GESTACIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(D_GESTACIO,'1','2')) AND ( .NOT. EMPTY(D_GESTACIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_SEPSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEPSIS,'1','2')) AND ( .NOT. EMPTY(SEPSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_HEM_1ER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEM_1ER,'1','2')) AND ( .NOT. EMPTY(HEM_1ER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_HEM_2DO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEM_2DO,'1','2')) AND ( .NOT. EMPTY(HEM_2DO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_HEM_3ER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEM_3ER,'1','2')) AND ( .NOT. EMPTY(HEM_3ER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_DESP_CEFAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESP_CEFAL,'1','2')) AND ( .NOT. EMPTY(DESP_CEFAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_RETARDO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RETARDO,'1','2')) AND ( .NOT. EMPTY(RETARDO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_ENFER_AUTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENFER_AUTO,'1','2')) AND ( .NOT. EMPTY(ENFER_AUTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_MALARIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MALARIA,'1','2')) AND ( .NOT. EMPTY(MALARIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_EMBARAZO_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EMBARAZO_N,'1','2')) AND ( .NOT. EMPTY(EMBARAZO_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_VIOLECIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIOLECIA,'1','2')) AND ( .NOT. EMPTY(VIOLECIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_GES_PRO_VS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GES_PRO_VS,'1','2')) AND ( .NOT. EMPTY(GES_PRO_VS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_FETO_INCOM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FETO_INCOM,'1','2')) AND ( .NOT. EMPTY(FETO_INCOM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_SIN_DEPRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIN_DEPRES,'1','2')) AND ( .NOT. EMPTY(SIN_DEPRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_OTRAS_COMP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRAS_COMP,'1','2')) AND ( .NOT. EMPTY(OTRAS_COMP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_NUM_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(NUM_CON)>=0  AND  VAL(NUM_CON) <=25 AND ISNUMERIC(NUM_CON)) AND ( .NOT. EMPTY(NUM_CON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_SI_CTRL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SI_CTRL)>= 1  AND  VAL(SI_CTRL) <= 45 AND ISNUMERIC(SI_CTRL)) OR ( EMPTY(SI_CTRL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_CON_REA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_REA,'1','2','3','4','5')) OR ( EMPTY(CON_REA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_NIV_ATE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NIV_ATE,'1','2','3','4')) OR ( EMPTY(NIV_ATE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_REM_OPO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REM_OPO,'1','2','3')) AND ( .NOT. EMPTY(REM_OPO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_MOM_OCU_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MOM_OCU_P,'1','2','3','4')) AND ( .NOT. EMPTY(MOM_OCU_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_SEM_MUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SEM_MUE)>= 1  AND  VAL(SEM_MUE) <= 42 AND ISNUMERIC(SEM_MUE)) AND ( .NOT. EMPTY(SEM_MUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_TIP_PAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_PAR,'1','2','3','4','5')) OR ( EMPTY(TIP_PAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_PAR_ATE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAR_ATE,'1','2','3','4','5','6','7')) OR ( EMPTY(PAR_ATE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_NIV_ATE_PA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NIV_ATE_PA,'1','2','3','4')) OR ( EMPTY(NIV_ATE_PA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_CAU_BAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CAU_BAS))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_CAU_MTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAU_MTE,'1','2','3')) AND ( .NOT. EMPTY(CAU_MTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_DEMORAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEMORAS,'1','2')) AND ( .NOT. EMPTY(DEMORAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_58_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_FEC_INV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_INV))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_ENTID_INVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENTID_INVE,'1','2','3')) AND ( .NOT. EMPTY(ENTID_INVE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_CAUS_MUER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUS_MUER,'1','2','3')) AND ( .NOT. EMPTY(CAUS_MUER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_SIT_MUER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_MUER,'1','2','3','4','5')) AND ( .NOT. EMPTY(SIT_MUER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_NOM_ENTREV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_ENTREV))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_TIP_IDE_EN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_EN,'CC','CE','PA','AS')) AND ( .NOT. EMPTY(TIP_IDE_EN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_NUM_IDE_EN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_EN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_PARENT_ENT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARENT_ENT,'1','2','3','4','5')) AND ( .NOT. EMPTY(PARENT_ENT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_PARENT_CUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARENT_CUI,'1','2','5')) AND ( .NOT. EMPTY(PARENT_CUI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_ETNIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ETNIA,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(ETNIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_ESC_CUID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESC_CUID,'1','2','3','4','5')) AND ( .NOT. EMPTY(ESC_CUID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_ESTRATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTRATO,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(ESTRATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_ESTAD_CUID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTAD_CUID,'1','2','3','4','5')) AND ( .NOT. EMPTY(ESTAD_CUID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_OCUPAC_CUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(OCUPAC_CUI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_PESO_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_NAC>=100 AND ISNUMERIC(PESO_NAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_ESQ_VACU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESQ_VACU,'1','2','3')) AND ( .NOT. EMPTY(ESQ_VACU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_CARNE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARNE,'1','2')) AND ( .NOT. EMPTY(CARNE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_DESNUTRICI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESNUTRICI,'1','2')) AND ( .NOT. EMPTY(DESNUTRICI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_BAJO_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BAJO_PESO,'1','2')) AND ( .NOT. EMPTY(BAJO_PESO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_HACINAMIEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HACINAMIEN,'1','2')) AND ( .NOT. EMPTY(HACINAMIEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_HAB_INADEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HAB_INADEC,'1','2')) AND ( .NOT. EMPTY(HAB_INADEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_PISO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PISO,'1','2')) AND ( .NOT. EMPTY(PISO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_INSECTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INSECTOS,'1','2')) AND ( .NOT. EMPTY(INSECTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_INA_MAN_AL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INA_MAN_AL,'1','2')) AND ( .NOT. EMPTY(INA_MAN_AL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_FUEN_AGUA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FUEN_AGUA,'1','2','3','4','5','6','7','8')) AND ( .NOT. EMPTY(FUEN_AGUA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_DISP_EXCRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISP_EXCRE,'1','2','3','4','5')) AND ( .NOT. EMPTY(DISP_EXCRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_RECON_SIG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECON_SIG,'1','2','3')) AND ( .NOT. EMPTY(RECON_SIG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_BUSC_AYUDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BUSC_AYUDA,'1','2','3')) AND ( .NOT. EMPTY(BUSC_AYUDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_FACIL_TRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FACIL_TRAN,'1','2','3')) AND ( .NOT. EMPTY(FACIL_TRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_RECON_DIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECON_DIA,'1','2')) AND ( .NOT. EMPTY(RECON_DIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_RECIBI_SRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECIBI_SRO,'1','2')) AND ( .NOT. EMPTY(RECIBI_SRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_TERAP_NO_M
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TERAP_NO_M,'1','2')) AND ( .NOT. EMPTY(TERAP_NO_M)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_DIFIC_ADM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIFIC_ADM,'1','2','3')) AND ( .NOT. EMPTY(DIFIC_ADM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_CUAL_DIFIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUAL_DIFIC,'1','2','3')) OR ( EMPTY(CUAL_DIFIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_ATN_OPORTU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATN_OPORTU,'1','2','3')) AND ( .NOT. EMPTY(ATN_OPORTU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_CALIDAD_AT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CALIDAD_AT,'1','2','3','4')) AND ( .NOT. EMPTY(CALIDAD_AT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_59_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_LACERACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LACERACION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','LACERACION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(LACERACION,'1','2')) AND ( .NOT. EMPTY(LACERACION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_CONTUSION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CONTUSION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','CONTUSION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CONTUSION,'1','2')) AND ( .NOT. EMPTY(CONTUSION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUEMADURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUEMADURA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUEMADURA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUEMADURA,'1','2')) AND ( .NOT. EMPTY(QUEMADURA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_CARA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_CARA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_CARA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_CARA,'1','2')) OR ( EMPTY(QUE_CARA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_CUELLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_CUELLO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_CUELLO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_CUELLO,'1','2')) OR ( EMPTY(QUE_CUELLO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_MANO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MANO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_MANO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MANO,'1','2')) OR ( EMPTY(QUE_MANO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_PIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_PIE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_PIE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_PIE,'1','2')) OR ( EMPTY(QUE_PIE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_PLIEGU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_PLIEGU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_PLIEGU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_PLIEGU,'1','2')) OR ( EMPTY(QUE_PLIEGU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_GENITA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_GENITA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_GENITA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_GENITA,'1','2')) OR ( EMPTY(QUE_GENITA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_TRONCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_TRONCO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_TRONCO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_TRONCO,'1','2')) OR ( EMPTY(QUE_TRONCO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_MIESUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MIESUP)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_MIESUP'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MIESUP,'1','2')) OR ( EMPTY(QUE_MIESUP)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_MIEINF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MIEINF)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_MIEINF'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MIEINF,'1','2')) OR ( EMPTY(QUE_MIEINF)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_CLA_GRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CLA_GRA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','CLA_GRA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CLA_GRA,'1','2','3')) OR ( EMPTY(CLA_GRA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_EXT_QUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EXT_QUE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','EXT_QUE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EXT_QUE,'1','2','3')) OR ( EMPTY(EXT_QUE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMPUTACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMPUTACION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMPUTACION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMPUTACION,'1','2')) AND ( .NOT. EMPTY(AMPUTACION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_DEDMAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_DEDMAN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_DEDMAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_DEDMAN,'1','2')) OR ( EMPTY(AMP_DEDMAN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_MANO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_MANO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_MANO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_MANO,'1','2')) OR ( EMPTY(AMP_MANO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_ANTEBR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_ANTEBR)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_ANTEBR'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_ANTEBR,'1','2')) OR ( EMPTY(AMP_ANTEBR)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_BRAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_BRAZO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_BRAZO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_BRAZO,'1','2')) OR ( EMPTY(AMP_BRAZO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_MUSLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_MUSLO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_MUSLO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_MUSLO,'1','2')) OR ( EMPTY(AMP_MUSLO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_PIERNA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_PIERNA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_PIERNA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_PIERNA,'1','2')) OR ( EMPTY(AMP_PIERNA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_PIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_PIE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_PIE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_PIE,'1','2')) OR ( EMPTY(AMP_PIE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_DEDPIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_DEDPIE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_DEDPIE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_DEDPIE,'1','2')) OR ( EMPTY(AMP_DEDPIE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_DAÑ_OCU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(DAÑ_OCU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','DAÑ_OCU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(DAÑ_OCU,'1','2')) AND ( .NOT. EMPTY(DAÑ_OCU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_DAÑ_AUD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(DAÑ_AUD)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','DAÑ_AUD'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(DAÑ_AUD,'1','2')) AND ( .NOT. EMPTY(DAÑ_AUD)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRACTURAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRACTURAS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRACTURAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRACTURAS,'1','2')) AND ( .NOT. EMPTY(FRACTURAS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_CRANEO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_CRANEO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_CRANEO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_CRANEO,'1','2')) OR ( EMPTY(FRA_CRANEO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_HUEMAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_HUEMAN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_HUEMAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_HUEMAN,'1','2')) OR ( EMPTY(FRA_HUEMAN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_HUEPIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_HUEPIE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_HUEPIE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_HUEPIE,'1','2')) OR ( EMPTY(FRA_HUEPIE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_REJA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_REJA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_REJA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_REJA,'1','2')) OR ( EMPTY(FRA_REJA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_VIA_AER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(VIA_AER)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','VIA_AER'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(VIA_AER,'1','2')) AND ( .NOT. EMPTY(VIA_AER)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_ABDOMEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ABDOMEN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','ABDOMEN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ABDOMEN,'1','2')) AND ( .NOT. EMPTY(ABDOMEN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_OTRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(OTRO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','OTRO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(OTRO,'1','2')) AND ( .NOT. EMPTY(OTRO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_ARTEF_LESI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ARTEF_LESI)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','ARTEF_LESI'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ARTEF_LESI,'1','2','3')) AND ( .NOT. EMPTY(ARTEF_LESI)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_ARTEFACTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ARTEFACTO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','ARTEFACTO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ARTEFACTO,'1','2','3','4','5','6','7','8','9','10')) OR ( EMPTY(ARTEFACTO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_CON_ALC_LE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CON_ALC_LE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','CON_ALC_LE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CON_ALC_LE,'1','2')) OR ( EMPTY(CON_ALC_LE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_CON_ALC_AC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CON_ALC_AC)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','CON_ALC_AC'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CON_ALC_AC,'1','2')) OR ( EMPTY(CON_ALC_AC)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_LUG_OCU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LUG_OCU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','LUG_OCU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(LUG_OCU,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(LUG_OCU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_ACT_POLVOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ACT_POLVOR)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','ACT_POLVOR'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ACT_POLVOR,'1','2','3','4','5','6','7')) OR ( EMPTY(ACT_POLVOR)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_ACT_MINAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ACT_MINAS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','ACT_MINAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ACT_MINAS,'1','2','3','4','5')) OR ( EMPTY(ACT_MINAS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AJUSTE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AJUSTE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_MIESUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_MIESUP)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_MIESUP'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_MIESUP,'1','2')) OR ( EMPTY(FRA_MIESUP)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_COLUMN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_COLUMN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_COLUMN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_COLUMN,'1','2')) OR ( EMPTY(FRA_COLUMN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_CADERA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_CADERA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_CADERA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_CADERA,'1','2')) OR ( EMPTY(FRA_CADERA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_MIEINF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_MIEINF)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_MIEINF'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_MIEINF,'1','2')) OR ( EMPTY(FRA_MIEINF)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_T_EN_RESID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(T_EN_RESID,'1','2','3')) AND ( .NOT. EMPTY(T_EN_RESID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_TRIQ_PREVI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRIQ_PREVI,'1','2')) AND ( .NOT. EMPTY(TRIQ_PREVI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_OJO_CIRUGI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OJO_CIRUGI,'1','2','3')) OR ( EMPTY(OJO_CIRUGI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PAR_OJODER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAR_OJODER,'1','2','3')) OR ( EMPTY(PAR_OJODER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PAR_OJOIZQ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAR_OJOIZQ,'1','2','3')) OR ( EMPTY(PAR_OJOIZQ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_CIC_MUCOSA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CIC_MUCOSA,'1','2')) AND ( .NOT. EMPTY(CIC_MUCOSA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_CIC_MUC_OJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CIC_MUC_OJ,'1','2','3')) OR ( EMPTY(CIC_MUC_OJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_DEP_PARSUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEP_PARSUP,'1','2')) AND ( .NOT. EMPTY(DEP_PARSUP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_DEP_PS_OJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEP_PS_OJ,'1','2','3')) OR ( EMPTY(DEP_PS_OJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEST_PS_OD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEST_PS_OD,'0','1','2','3','4')) OR ( EMPTY(PEST_PS_OD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEST_PS_OI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEST_PS_OI,'0','1','2','3','4')) OR ( EMPTY(PEST_PS_OI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_DEP_PARINF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEP_PARINF,'1','2')) AND ( .NOT. EMPTY(DEP_PARINF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_DEP_PI_OJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEP_PI_OJ,'1','2','3')) OR ( EMPTY(DEP_PI_OJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEST_PI_OD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEST_PI_OD,'0','1','2','3','4')) OR ( EMPTY(PEST_PI_OD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEST_PI_OI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEST_PI_OI,'0','1','2','3','4')) OR ( EMPTY(PEST_PI_OI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEST_GO_PS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEST_GO_PS,'1','2')) AND ( .NOT. EMPTY(PEST_GO_PS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEGO_PS_OJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEGO_PS_OJ,'1','2','3')) OR ( EMPTY(PEGO_PS_OJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEGO_PS_OD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEGO_PS_OD,'1','2','3','4')) OR ( EMPTY(PEGO_PS_OD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEGO_PS_OI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEGO_PS_OI,'1','2','3','4')) OR ( EMPTY(PEGO_PS_OI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEST_GO_PI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEST_GO_PI,'1','2')) AND ( .NOT. EMPTY(PEST_GO_PI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEGO_PI_OJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEGO_PI_OJ,'1','2','3')) OR ( EMPTY(PEGO_PI_OJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEGO_PI_OD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEGO_PI_OD,'1','2','3','4')) OR ( EMPTY(PEGO_PI_OD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PEGO_PI_OI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PEGO_PI_OI,'1','2','3','4')) OR ( EMPTY(PEGO_PI_OI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PESTCORNEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PESTCORNEA,'1','2')) AND ( .NOT. EMPTY(PESTCORNEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PES_COR_OJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PES_COR_OJ,'1','2','3')) OR ( EMPTY(PES_COR_OJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PES_COR_OD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PES_COR_OD,'1','2','3','4')) OR ( EMPTY(PES_COR_OD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PES_COR_OI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PES_COR_OI,'1','2','3','4')) OR ( EMPTY(PES_COR_OI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_OPC_CORNEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OPC_CORNEA,'1','2','3','4')) AND ( .NOT. EMPTY(OPC_CORNEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_ENG_PARPAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENG_PARPAD,'1','2','3','4')) AND ( .NOT. EMPTY(ENG_PARPAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_PES_MALPOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PES_MALPOS,'1','2','3','4')) AND ( .NOT. EMPTY(PES_MALPOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_CUERPO_EXT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUERPO_EXT,'1','2','3','4')) AND ( .NOT. EMPTY(CUERPO_EXT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_FOTOFOBIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FOTOFOBIA,'1','2','3','4')) AND ( .NOT. EMPTY(FOTOFOBIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_AGUDEZA_OD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AGUDEZA_OD))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_AGUDEZA_OI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AGUDEZA_OI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_61_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_TIPO_AGENT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_AGENT,'1','2','3','4','5')) AND ( .NOT. EMPTY(TIPO_AGENT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_ANT_HIB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_HIB,'1','2','3')) AND ( .NOT. EMPTY(ANT_HIB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_MENINGOCOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MENINGOCOC,'1','2','3')) AND ( .NOT. EMPTY(MENINGOCOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_NEUMOCOCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NEUMOCOCO,'1','2','3')) AND ( .NOT. EMPTY(NEUMOCOCO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_USO_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(USO_ANT,'1','2')) AND ( .NOT. EMPTY(USO_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_62_CLAS_FINAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_FINAL,'1','2','3')) OR ( EMPTY(CLAS_FINAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_CONSX2_NEO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONSX2_NEO,'1','2')) AND ( .NOT. EMPTY(CONSX2_NEO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_RECAIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECAIDA,'1','2')) AND ( .NOT. EMPTY(RECAIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_CRIT_DX_PR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRIT_DX_PR,'1','2','3','4','5')) AND ( .NOT. EMPTY(CRIT_DX_PR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_CRIT_DX_DE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRIT_DX_DE,'1','2','3','4','5','7','8')) OR ( EMPTY(CRIT_DX_DE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_TIPO_CA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_CA,'1','2','3','4','5','6','7','8','9','10','11','12','13','14')) AND ( .NOT. EMPTY(TIPO_CA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_63_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_CLAS_NOTIF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_NOTIF,'1','2')) AND ( .NOT. EMPTY(CLAS_NOTIF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_TIP_AGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_AGR,'1','2','3','6','7','8')) AND ( .NOT. EMPTY(TIP_AGR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_AGR_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGR_PRO,'1','2')) AND ( .NOT. EMPTY(AGR_PRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_TIP_LES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_LES,'1','2')) OR ( EMPTY(TIP_LES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_PROFUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROFUN,'1','2')) OR ( EMPTY(PROFUN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_CCC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CCC,'1','2')) OR ( EMPTY(CCC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_MAN_DED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAN_DED,'1','2')) OR ( EMPTY(MAN_DED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_TRONCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRONCO,'1','2')) OR ( EMPTY(TRONCO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_MIE_SUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIE_SUP,'1','2')) OR ( EMPTY(MIE_SUP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_MIE_INF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIE_INF,'1','2')) OR ( EMPTY(MIE_INF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_PIES_DEDOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIES_DEDOS,'1','2')) OR ( EMPTY(PIES_DEDOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_GENIT_EXT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GENIT_EXT,'1','2')) OR ( EMPTY(GENIT_EXT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_FEC_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_EXP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_ESP_ANI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_ANI,'1','2','3','4','5','7','8','9','10','12','13','14')) AND ( .NOT. EMPTY(ESP_ANI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_ANT_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_VAC,'1','2','3')) OR ( EMPTY(ANT_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_CAR_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAR_VAC,'1','2')) OR ( EMPTY(CAR_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_EST_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_MA,'1','2','3')) OR ( EMPTY(EST_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_UBICACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBICACION,'1','2')) OR ( EMPTY(UBICACION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_TIP_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_EXP,'0','1','2')) AND ( .NOT. EMPTY(TIP_EXP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_SUE_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUE_ANT,'1','2','3')) AND ( .NOT. EMPTY(SUE_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_VAC_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_ANT,'1','2','3')) AND ( .NOT. EMPTY(VAC_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_LE_AGU_JAB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LE_AGU_JAB,'1','2')) OR ( EMPTY(LE_AGU_JAB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_SUT_HER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUT_HER,'1','2')) OR ( EMPTY(SUT_HER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_APL_SA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_SA,'1','2')) AND ( .NOT. EMPTY(APL_SA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_APL_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_VAC,'1','2')) AND ( .NOT. EMPTY(APL_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) OR ( EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_HIPOREXIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPOREXIA,'1','2')) OR ( EMPTY(HIPOREXIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) OR ( EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) OR ( EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_PARESIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARESIAS,'1','2')) OR ( EMPTY(PARESIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_PARESTESIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARESTESIA,'1','2')) OR ( EMPTY(PARESTESIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_DISFAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISFAGIA,'1','2')) OR ( EMPTY(DISFAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_ODINOFAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ODINOFAGIA,'1','2')) OR ( EMPTY(ODINOFAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_ARREFLEXIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARREFLEXIA,'1','2')) OR ( EMPTY(ARREFLEXIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_PSICOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PSICOSIS,'1','2')) OR ( EMPTY(PSICOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_FASCIES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FASCIES,'1','2')) OR ( EMPTY(FASCIES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_SIALORREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIALORREA,'1','2')) OR ( EMPTY(SIALORREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_AEROFOBIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AEROFOBIA,'1','2')) OR ( EMPTY(AEROFOBIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_HIDROFOBIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIDROFOBIA,'1','2')) OR ( EMPTY(HIDROFOBIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_TRANQ_EXCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRANQ_EXCI,'1','2')) OR ( EMPTY(TRANQ_EXCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_DEPRESION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEPRESION,'1','2')) OR ( EMPTY(DEPRESION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_HIPEREXITA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPEREXITA,'1','2')) OR ( EMPTY(HIPEREXITA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_AGRESIVIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGRESIVIDA,'1','2')) OR ( EMPTY(AGRESIVIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_ESPASMOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESPASMOS,'1','2')) OR ( EMPTY(ESPASMOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_CONVULSION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONVULSION,'1','2')) OR ( EMPTY(CONVULSION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_PARALISIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARALISIS,'1','2')) OR ( EMPTY(PARALISIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_CRIS_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRIS_RESP,'1','2')) OR ( EMPTY(CRIS_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_COMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMA,'1','2')) OR ( EMPTY(COMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_PARO_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARO_RESP,'1','2')) OR ( EMPTY(PARO_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_PR_DIAG_CO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PR_DIAG_CO,'1','2','3','4')) OR ( EMPTY(PR_DIAG_CO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_RESULTADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULTADO,'1','2','3','4')) OR ( EMPTY(RESULTADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_ID_VARIANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ID_VARIANT,'1','2','3')) OR ( EMPTY(ID_VARIANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_VAR_IDENTI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAR_IDENTI,'1','3','4','5','8','9','0')) OR ( EMPTY(VAR_IDENTI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_AREA_MORDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AREA_MORDE,'1','2')) OR ( EMPTY(AREA_MORDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_64_ESTADO_ANI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADO_ANI,'1','2','3')) OR ( EMPTY(ESTADO_ANI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_VIG_ACTIVA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIG_ACTIVA,'1','2')) AND ( .NOT. EMPTY(VIG_ACTIVA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_SINTOMATIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SINTOMATIC,'1','2')) AND ( .NOT. EMPTY(SINTOMATIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_CLAS_CASO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_CASO,'1','2')) AND ( .NOT. EMPTY(CLAS_CASO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_RECRUDECE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RECRUDECE,'1','2')) AND ( .NOT. EMPTY(RECRUDECE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_TRIMESTRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRIMESTRE,'1','2','3')) OR ( EMPTY(TRIMESTRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_TRATAMIENT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRATAMIENT,'1','2','3','5','6','7','8','9','10','11','12','13','14','15','16')) AND ( .NOT. EMPTY(TRATAMIENT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_TIPOEXAMEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(TIPOEXAMEN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_RECUENTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((RECUENTO>=16 AND ISNUMERIC(RECUENTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_GAMETOCITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GAMETOCITO,'1','2')) AND ( .NOT. EMPTY(GAMETOCITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_COMPLICACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMPLICACI,'1','2')) AND ( .NOT. EMPTY(COMPLICACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_COM_CEREBR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_CEREBR,'1','2')) OR ( EMPTY(COM_CEREBR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_COM_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_RENAL,'1','2')) OR ( EMPTY(COM_RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_COM_HEPATI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_HEPATI,'1','2')) OR ( EMPTY(COM_HEPATI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_COM_PULMON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_PULMON,'1','2')) OR ( EMPTY(COM_PULMON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_COM_HEMATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_HEMATO,'1','2')) OR ( EMPTY(COM_HEMATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_COM_OTRAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_OTRAS,'1','2')) OR ( EMPTY(COM_OTRAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_ESP_PLA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_PLA,'1','2','3','4')) AND ( .NOT. EMPTY(ESP_PLA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_RESP_DIAG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(RESP_DIAG))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_FEC_RESULT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_RESULT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_65_DESPLAZAMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESPLAZAMI,'1','2')) AND ( .NOT. EMPTY(DESPLAZAMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_GRUPO_SUST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GRUPO_SUST,'1','2','3','4','5','6','7','8')) AND ( .NOT. EMPTY(GRUPO_SUST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_COD_SUST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUST))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_NOM_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_PRO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_TIP_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_EXP,'1','2','4','6','8','9','10','11')) AND ( .NOT. EMPTY(TIP_EXP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_LUGAR_EXPO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LUGAR_EXPO,'1','2','3','4','5','6','7','8')) AND ( .NOT. EMPTY(LUGAR_EXPO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_FEC_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_EXP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_HOR_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(HOR_EXP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_VIA_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIA_EXP,'1','2','3','4','5','6','8')) AND ( .NOT. EMPTY(VIA_EXP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_ESCOLARIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESCOLARIDA,'1','2','3','4','5','6','7','8','9','10','11','12','13','14')) AND ( .NOT. EMPTY(ESCOLARIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_AFI_ARP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AFI_ARP,'1','2')) AND ( .NOT. EMPTY(AFI_ARP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_EST_CIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_CIV,'1','2','3','4','5')) AND ( .NOT. EMPTY(EST_CIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_PARTE_BROT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARTE_BROT,'1','2')) AND ( .NOT. EMPTY(PARTE_BROT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_SIT_ALE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_ALE,'1','2')) AND ( .NOT. EMPTY(SIT_ALE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_MUEST_TOXI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUEST_TOXI,'1','2')) AND ( .NOT. EMPTY(MUEST_TOXI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_TIPO_MUEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_MUEST,'1','2','4','13','15','17','18','23','25','26','27','28','29','30','32')) OR ( EMPTY(TIPO_MUEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_66_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_COD_ENFERM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_ENFERM))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_NIVEL_EDUC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NIVEL_EDUC,'1','2','3','4','5','6','7','8','9','10','11','12','13')) AND ( .NOT. EMPTY(NIVEL_EDUC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_TRAB_URBAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAB_URBAN,'1','2')) AND ( .NOT. EMPTY(TRAB_URBAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_TRAB_RURAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAB_RURAL,'1','2')) AND ( .NOT. EMPTY(TRAB_RURAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_JOV_VUL_RU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JOV_VUL_RU,'1','2')) AND ( .NOT. EMPTY(JOV_VUL_RU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_JOV_VUL_UR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JOV_VUL_UR,'1','2')) AND ( .NOT. EMPTY(JOV_VUL_UR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_SISNER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_SISNER,'1','2')) AND ( .NOT. EMPTY(DIS_SISNER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_OJOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_OJOS,'1','2')) AND ( .NOT. EMPTY(DIS_OJOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_OIDOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_OIDOS,'1','2')) AND ( .NOT. EMPTY(DIS_OIDOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_OTRSEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_OTRSEN,'1','2')) AND ( .NOT. EMPTY(DIS_OTRSEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_VOZHAB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_VOZHAB,'1','2')) AND ( .NOT. EMPTY(DIS_VOZHAB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_CARDIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_CARDIO,'1','2')) AND ( .NOT. EMPTY(DIS_CARDIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_DIGEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_DIGEST,'1','2')) AND ( .NOT. EMPTY(DIS_DIGEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_SISGEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_SISGEN,'1','2')) AND ( .NOT. EMPTY(DIS_SISGEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_MOVIMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_MOVIMI,'1','2')) AND ( .NOT. EMPTY(DIS_MOVIMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_PIEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_PIEL,'1','2')) AND ( .NOT. EMPTY(DIS_PIEL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_OTRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_OTRA,'1','2')) AND ( .NOT. EMPTY(DIS_OTRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_NO_DEFINID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NO_DEFINID,'1','2')) AND ( .NOT. EMPTY(NO_DEFINID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_FEC_DIAGNO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_DIAGNO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_RES_INMUNO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_INMUNO,'1','2','3')) AND ( .NOT. EMPTY(RES_INMUNO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_AUS_ANTIGE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AUS_ANTIGE,'1','2','3')) OR ( EMPTY(AUS_ANTIGE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_UBIC_DONAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBIC_DONAN,'1','2')) AND ( .NOT. EMPTY(UBIC_DONAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_DONAN_ASIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DONAN_ASIS,'1','2')) OR ( EMPTY(DONAN_ASIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_CAUSA_NOUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUSA_NOUB,'1','2','3','4')) OR ( EMPTY(CAUSA_NOUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_INFORME_ST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFORME_ST,'1','2','3')) OR ( EMPTY(INFORME_ST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_68_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_MARC_REACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MARC_REACT,'1','2','3')) AND ( .NOT. EMPTY(MARC_REACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_NEUTR_HBS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NEUTR_HBS,'1','2','3')) OR ( EMPTY(NEUTR_HBS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_ANTI_HBC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANTI_HBC,'1','2')) OR ( EMPTY(ANTI_HBC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_ANTI_HBSAG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ANTI_HBSAG)>= 0 AND ISNUMERIC(ANTI_HBSAG)) OR ( EMPTY(ANTI_HBSAG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_INTERPRETA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INTERPRETA,'1','2','3')) AND ( .NOT. EMPTY(INTERPRETA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_UBIC_DONAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBIC_DONAN,'1','2')) OR ( EMPTY(UBIC_DONAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_DONAN_ASIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DONAN_ASIS,'1','2')) OR ( EMPTY(DONAN_ASIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_CAUSA_NOUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUSA_NOUB,'1','2','3','4')) OR ( EMPTY(CAUSA_NOUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_INFORME_ST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFORME_ST,'1','2','3')) OR ( EMPTY(INFORME_ST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_69_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_RES_INMUNO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_INMUNO,'1','2','3')) OR ( EMPTY(RES_INMUNO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_PCR_NAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PCR_NAT,'1','2','3')) AND ( .NOT. EMPTY(PCR_NAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_UBIC_DONAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBIC_DONAN,'1','2')) OR ( EMPTY(UBIC_DONAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_DONAN_ASIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DONAN_ASIS,'1','2')) OR ( EMPTY(DONAN_ASIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_CAUSA_NOUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUSA_NOUB,'1','2','3','4')) OR ( EMPTY(CAUSA_NOUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_INFORME_ST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFORME_ST,'1','2','3')) OR ( EMPTY(INFORME_ST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_70_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_TIPO_PRUEB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_PRUEB,'1','2')) AND ( .NOT. EMPTY(TIPO_PRUEB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_IFI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IFI,'1','2')) OR ( EMPTY(IFI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_SEG_TAMIZA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEG_TAMIZA,'1','2')) OR ( EMPTY(SEG_TAMIZA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_RES_INMUNO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_INMUNO,'1','2','3')) OR ( EMPTY(RES_INMUNO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_UBIC_DONAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBIC_DONAN,'1','2')) OR ( EMPTY(UBIC_DONAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_DONAN_ASIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DONAN_ASIS,'1','2')) OR ( EMPTY(DONAN_ASIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_CAUSA_NOUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUSA_NOUB,'1','2','3','4')) OR ( EMPTY(CAUSA_NOUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_INFORME_ST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFORME_ST,'1','2','3')) OR ( EMPTY(INFORME_ST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_71_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_TIPO_PRUEB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_PRUEB,'1','2')) AND ( .NOT. EMPTY(TIPO_PRUEB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_VDRL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VDRL,'1','2')) OR ( EMPTY(VDRL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_RPR_M
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RPR_M,'1','2')) OR ( EMPTY(RPR_M)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_UBIC_DONAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBIC_DONAN,'1','2')) AND ( .NOT. EMPTY(UBIC_DONAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_DONAN_ASIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DONAN_ASIS,'1','2')) OR ( EMPTY(DONAN_ASIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_CAUSA_NOUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUSA_NOUB,'1','2','3','4')) OR ( EMPTY(CAUSA_NOUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_INFORME_ST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFORME_ST,'1','2','3')) OR ( EMPTY(INFORME_ST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_72_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_RES_INMUNO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_INMUNO,'1','2','3')) AND ( .NOT. EMPTY(RES_INMUNO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_UBIC_DONAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBIC_DONAN,'1','2')) OR ( EMPTY(UBIC_DONAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_DONAN_ASIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DONAN_ASIS,'1','2')) OR ( EMPTY(DONAN_ASIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_CAUSA_NOUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUSA_NOUB,'1','2','3','4')) OR ( EMPTY(CAUSA_NOUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_INFORME_ST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFORME_ST,'1','2','3')) OR ( EMPTY(INFORME_ST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_73_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_PROC_CONFI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROC_CONFI,'1','2','3')) AND ( .NOT. EMPTY(PROC_CONFI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_UBIC_DONAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBIC_DONAN,'1','2')) AND ( .NOT. EMPTY(UBIC_DONAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_DONAN_ASIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DONAN_ASIS,'1','2')) OR ( EMPTY(DONAN_ASIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_CAUSA_NOUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAUSA_NOUB,'1','2','3','4')) OR ( EMPTY(CAUSA_NOUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_INFORME_ST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFORME_ST,'1','2','3')) OR ( EMPTY(INFORME_ST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_74_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_ATEN_PARTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATEN_PARTO,'1','2')) AND ( .NOT. EMPTY(ATEN_PARTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_FEC_PARTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_PARTO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_PROCEDIMIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROCEDIMIE,'1','2','3','4','5')) AND ( .NOT. EMPTY(PROCEDIMIE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_CONTE_PROC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONTE_PROC,'1','2','4')) AND ( .NOT. EMPTY(CONTE_PROC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_PRF_ANTIB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRF_ANTIB,'1','2')) OR ( EMPTY(PRF_ANTIB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_TIEMPO_RUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TIEMPO_RUP)>=0  AND  VAL(TIEMPO_RUP) <= 2880 AND ISNUMERIC(TIEMPO_RUP)) OR ( EMPTY(TIEMPO_RUP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_REQ_INTERV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REQ_INTERV,'1','2')) AND ( .NOT. EMPTY(REQ_INTERV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_TIEM_ENTRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIEM_ENTRE,'1','2','3')) OR ( EMPTY(TIEM_ENTRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_ATENC_UPGD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ATENC_UPGD,'1','2')) AND ( .NOT. EMPTY(ATENC_UPGD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_75_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'3','5','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_FEC_PROC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_PROC))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_PROCEDIMIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROCEDIMIE,'1','2','3','4','5')) AND ( .NOT. EMPTY(PROCEDIMIE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_CLASIF_ASA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLASIF_ASA,'1','2','3','4','5')) OR ( EMPTY(CLASIF_ASA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_TIPOHERIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPOHERIDA,'1','2')) OR ( EMPTY(TIPOHERIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_CONTE_PROC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONTE_PROC,'1','2')) OR ( EMPTY(CONTE_PROC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_PRF_ANTIB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRF_ANTIB,'1','2')) OR ( EMPTY(PRF_ANTIB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_PROF_PRIMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROF_PRIMA,'1','2')) AND ( .NOT. EMPTY(PROF_PRIMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_PROF_SECUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROF_SECUN,'1','2')) AND ( .NOT. EMPTY(PROF_SECUN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_ORG_ESPACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ORG_ESPACI,'1','2')) AND ( .NOT. EMPTY(ORG_ESPACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_SUP_PRIMAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUP_PRIMAR,'1','2')) AND ( .NOT. EMPTY(SUP_PRIMAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_SUP_SECUND
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUP_SECUND,'1','2')) AND ( .NOT. EMPTY(SUP_SECUND)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_DUR_PROC_M
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((DUR_PROC_M>=1 AND ISNUMERIC(DUR_PROC_M)) OR (DUR_PROC_M=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_TIEM_ENTRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIEM_ENTRE,'1','2','3')) OR ( EMPTY(TIEM_ENTRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_CUPS_PROCE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CUPS_PROCE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_TIEMPO_RUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TIEMPO_RUP)>=0  AND  VAL(TIEMPO_RUP) <= 2880 AND ISNUMERIC(TIEMPO_RUP)) OR ( EMPTY(TIEMPO_RUP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_DIABET_MEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIABET_MEL,'1','2')) OR ( EMPTY(DIABET_MEL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO>=1.0  AND PESO <=350.0 AND ISNUMERIC(PESO)) OR (PESO=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_TALLA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA>= 0.30  AND TALLA <= 2.50 AND ISNUMERIC(TALLA)) OR (TALLA=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_76_DETEC_INFE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DETEC_INFE,'1','2','3','4')) AND ( .NOT. EMPTY(DETEC_INFE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_PROB_FAMIL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROB_FAMIL,'1','2')) AND ( .NOT. EMPTY(PROB_FAMIL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ESTADO_CIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADO_CIV,'1','2','3','4','5')) AND ( .NOT. EMPTY(ESTADO_CIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_MUERTE_FAM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUERTE_FAM,'1','2')) AND ( .NOT. EMPTY(MUERTE_FAM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_PROB_LEGAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROB_LEGAL,'1','2')) AND ( .NOT. EMPTY(PROB_LEGAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_PSICOLOGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PSICOLOGIA,'1','2')) OR ( EMPTY(PSICOLOGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_PSIQUIATRI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PSIQUIATRI,'1','2')) OR ( EMPTY(PSIQUIATRI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_INTEN_PREV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INTEN_PREV,'1','2')) AND ( .NOT. EMPTY(INTEN_PREV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_INTENTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INTENTOS,'1','2','3','4','99')) OR ( EMPTY(INTENTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_PROB_ECONO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROB_ECONO,'1','2')) AND ( .NOT. EMPTY(PROB_ECONO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_PROB_PAREJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROB_PAREJ,'1','2')) AND ( .NOT. EMPTY(PROB_PAREJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_PROB_CONSU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROB_CONSU,'1','2')) AND ( .NOT. EMPTY(PROB_CONSU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ANTEC_TRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANTEC_TRAN,'1','2')) AND ( .NOT. EMPTY(ANTEC_TRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_HIST_FAMIL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIST_FAMIL,'1','2')) AND ( .NOT. EMPTY(HIST_FAMIL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_IDEA_SUICI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IDEA_SUICI,'1','2')) AND ( .NOT. EMPTY(IDEA_SUICI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ENFER_GRAV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENFER_GRAV,'1','2')) AND ( .NOT. EMPTY(ENFER_GRAV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_TRAN_DEPRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAN_DEPRE,'1','2')) OR ( EMPTY(TRAN_DEPRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ESQUIZOFRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESQUIZOFRE,'1','2')) OR ( EMPTY(ESQUIZOFRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ABUSO_ALCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ABUSO_ALCO,'1','2')) AND ( .NOT. EMPTY(ABUSO_ALCO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_TRANS_PERS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRANS_PERS,'1','2')) OR ( EMPTY(TRANS_PERS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_AHORCAMIEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AHORCAMIEN,'1','2')) AND ( .NOT. EMPTY(AHORCAMIEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ARMA_CORTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARMA_CORTO,'1','2')) AND ( .NOT. EMPTY(ARMA_CORTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_INTOXICACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INTOXICACI,'1','2')) AND ( .NOT. EMPTY(INTOXICACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_TIPO_SUSTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_SUSTA,'1','2','3','4','5','6','7','8')) OR ( EMPTY(TIPO_SUSTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_LANZ_VACIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LANZ_VACIO,'1','2')) AND ( .NOT. EMPTY(LANZ_VACIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_LANZ_VEHIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LANZ_VEHIC,'1','2')) AND ( .NOT. EMPTY(LANZ_VEHIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ARMA_FUEGO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARMA_FUEGO,'1','2')) AND ( .NOT. EMPTY(ARMA_FUEGO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_TRAB_SOCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAB_SOCIA,'1','2')) OR ( EMPTY(TRAB_SOCIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ESCOLARID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESCOLARID,'1','2','3','5','7','8','9','10','11','12','13','14')) AND ( .NOT. EMPTY(ESCOLARID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_SUICI_FM_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUICI_FM_A,'1','2')) AND ( .NOT. EMPTY(SUICI_FM_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_PROB_LABOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROB_LABOR,'1','2')) OR ( EMPTY(PROB_LABOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_LANZ_AGUA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LANZ_AGUA,'1','2')) OR ( EMPTY(LANZ_AGUA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_MALTR_FPS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MALTR_FPS,'1','2')) AND ( .NOT. EMPTY(MALTR_FPS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ANTEC_V_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANTEC_V_A,'1','2')) AND ( .NOT. EMPTY(ANTEC_V_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_INMOLACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INMOLACION,'1','2')) AND ( .NOT. EMPTY(INMOLACION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ESCO_EDUC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESCO_EDUC,'1','2')) AND ( .NOT. EMPTY(ESCO_EDUC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_PLAN_SUICI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PLAN_SUICI,'1','2')) AND ( .NOT. EMPTY(PLAN_SUICI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_VIA_EXPOSI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIA_EXPOSI,'1','2','3','4','5','6','7')) OR ( EMPTY(VIA_EXPOSI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_LUGAR_INTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LUGAR_INTO,'1','2','3','4','5','6','7','8')) OR ( EMPTY(LUGAR_INTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_77_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'1','2','3')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_EDAD_INICI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((EDAD_INICI>=6  AND EDAD_INICI <=70 AND ISNUMERIC(EDAD_INICI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_DROGA_INIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DROGA_INIC,'1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20')) AND ( .NOT. EMPTY(DROGA_INIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_EDAD_INYEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((EDAD_INYEC>=6  AND EDAD_INYEC <= 70 AND ISNUMERIC(EDAD_INYEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_DROG_VIA_I
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DROG_VIA_I,'1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20')) AND ( .NOT. EMPTY(DROG_VIA_I)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_TRANQU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_TRANQU,'1','2')) AND ( .NOT. EMPTY(ACT_TRANQU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_ESTIMU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_ESTIMU,'1','2')) AND ( .NOT. EMPTY(ACT_ESTIMU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_ALCOHO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_ALCOHO,'1','2')) AND ( .NOT. EMPTY(ACT_ALCOHO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_COCAIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_COCAIN,'1','2')) AND ( .NOT. EMPTY(ACT_COCAIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_HEROIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_HEROIN,'1','2')) AND ( .NOT. EMPTY(ACT_HEROIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_METANF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_METANF,'1','2')) AND ( .NOT. EMPTY(ACT_METANF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_METADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_METADO,'1','2')) AND ( .NOT. EMPTY(ACT_METADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_ANALGE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_ANALGE,'1','2')) AND ( .NOT. EMPTY(ACT_ANALGE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_LSD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_LSD,'1','2')) AND ( .NOT. EMPTY(ACT_LSD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_KETAMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_KETAMI,'1','2')) AND ( .NOT. EMPTY(ACT_KETAMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_GHB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_GHB,'1','2')) AND ( .NOT. EMPTY(ACT_GHB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_ACT_SPEEDB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_SPEEDB,'1','2')) AND ( .NOT. EMPTY(ACT_SPEEDB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_FREC_INYEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FREC_INYEC,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(FREC_INYEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_COMP_MATER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMP_MATER,'1','2')) AND ( .NOT. EMPTY(COMP_MATER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_RELSIN_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RELSIN_PRE,'1','2')) AND ( .NOT. EMPTY(RELSIN_PRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_SOBRED_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SOBRED_AÑO,'1','2')) AND ( .NOT. EMPTY(SOBRED_AÑO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_SOBRED_MES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SOBRED_MES,'1','2')) AND ( .NOT. EMPTY(SOBRED_MES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_78_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_TIPO_CANCE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_CANCE,'1','2','3')) AND ( .NOT. EMPTY(TIPO_CANCE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_RES_BIOPS9
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_BIOPS9,'1','2')) OR ( EMPTY(RES_BIOPS9)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_GRAD_HISTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GRAD_HISTO,'1','2','3')) OR ( EMPTY(GRAD_HISTO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_BIOP_EXOCE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BIOP_EXOCE,'1','2')) OR ( EMPTY(BIOP_EXOCE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_BIOP_ENDOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BIOP_ENDOC,'1','2')) OR ( EMPTY(BIOP_ENDOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_RES_B_EXOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_B_EXOC,'1','2')) OR ( EMPTY(RES_B_EXOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_GRADO_HIST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GRADO_HIST,'1','2','3')) OR ( EMPTY(GRADO_HIST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_RES_B_ADEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_B_ADEN,'1','2')) OR ( EMPTY(RES_B_ADEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_RES_B_HIST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_B_HIST,'1','2','3')) OR ( EMPTY(RES_B_HIST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_SEG_TRAT_I
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEG_TRAT_I,'1','2')) AND ( .NOT. EMPTY(SEG_TRAT_I)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_RADIOTERAP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RADIOTERAP,'1','2')) OR ( EMPTY(RADIOTERAP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_QUIRURGICO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUIRURGICO,'1','2')) OR ( EMPTY(QUIRURGICO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_QUIMIOTERA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUIMIOTERA,'1','2')) OR ( EMPTY(QUIMIOTERA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_HORMONOTER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HORMONOTER,'1','2')) OR ( EMPTY(HORMONOTER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_CUID_PALIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUID_PALIA,'1','2')) OR ( EMPTY(CUID_PALIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_79_INMUNOTERA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INMUNOTERA,'1','2')) OR ( EMPTY(INMUNOTERA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_MES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(MES)>=1  AND  VAL(MES) <=12 AND ISNUMERIC(MES)) AND ( .NOT. EMPTY(MES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(AÑO)>=2011 AND ISNUMERIC(AÑO)) AND ( .NOT. EMPTY(AÑO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_SER_UCIV_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SER_UCIV_A)>=1  AND  VAL(SER_UCIV_A) <= 99 AND ISNUMERIC(SER_UCIV_A)) OR ( EMPTY(SER_UCIV_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_CAM_UCIV_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAM_UCIV_A)>=1  AND  VAL(CAM_UCIV_A) <= 99 AND ISNUMERIC(CAM_UCIV_A)) OR ( EMPTY(CAM_UCIV_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_DIAS_P_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_DIAS_P_A)>= 1  AND  VAL(N_DIAS_P_A) <= 9999 AND ISNUMERIC(N_DIAS_P_A)) OR ( EMPTY(N_DIAS_P_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_NV_S_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_NV_S_A)>=0  AND  VAL(N_C_NV_S_A) <=99 AND ISNUMERIC(N_C_NV_S_A)) OR ( EMPTY(N_C_NV_S_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_DS_D_S_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_DS_D_S_A)>=1  AND  VAL(N_DS_D_S_A) <=999 AND ISNUMERIC(N_DS_D_S_A)) OR ( EMPTY(N_DS_D_S_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_IN_S_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_IN_S_A)>=0.00  AND  VAL(TAS_IN_S_A) <=1000.00 AND ISNUMERIC(TAS_IN_S_A)) OR ( EMPTY(TAS_IN_S_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_CAS_PL_V_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAS_PL_V_A,'1','2')) OR ( EMPTY(CAS_PL_V_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_PL_V_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_PL_V_A)>=1  AND  VAL(N_C_PL_V_A) <=999 AND ISNUMERIC(N_C_PL_V_A)) OR ( EMPTY(N_C_PL_V_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_SER_UCIV_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SER_UCIV_N)>=1  AND  VAL(SER_UCIV_N) <=99 AND ISNUMERIC(SER_UCIV_N)) OR ( EMPTY(SER_UCIV_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_CAM_UCIV_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAM_UCIV_N)>=1  AND  VAL(CAM_UCIV_N) <=99 AND ISNUMERIC(CAM_UCIV_N)) OR ( EMPTY(CAM_UCIV_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_DIAS_P_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_DIAS_P_N)>=1  AND  VAL(N_DIAS_P_N) <=9999 AND ISNUMERIC(N_DIAS_P_N)) OR ( EMPTY(N_DIAS_P_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_N1_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_N1_S_N)>=0  AND  VAL(N_C_N1_S_N) <=99 AND ISNUMERIC(N_C_N1_S_N)) OR ( EMPTY(N_C_N1_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_C1_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C1_S_N)>=0  AND  VAL(N_D_C1_S_N) <=9999 AND ISNUMERIC(N_D_C1_S_N)) OR ( EMPTY(N_D_C1_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_P1_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P1_S_N)>=0  AND  VAL(N_D_P1_S_N) <=9999 AND ISNUMERIC(N_D_P1_S_N)) OR ( EMPTY(N_D_P1_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_I1_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_I1_S_N)>=0.00  AND  VAL(TAS_I1_S_N) <=1000.00 AND ISNUMERIC(TAS_I1_S_N)) OR ( EMPTY(TAS_I1_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_U_D1_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_U_D1_S_N)>=0  AND  VAL(P_U_D1_S_N) <=100.00 AND ISNUMERIC(P_U_D1_S_N)) OR ( EMPTY(P_U_D1_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_N2_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_N2_S_N)>=0  AND  VAL(N_C_N2_S_N) <=99 AND ISNUMERIC(N_C_N2_S_N)) OR ( EMPTY(N_C_N2_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_C2_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C2_S_N)>=0  AND  VAL(N_D_C2_S_N) <=9999 AND ISNUMERIC(N_D_C2_S_N)) OR ( EMPTY(N_D_C2_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_P2_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P2_S_N)>=0  AND  VAL(N_D_P2_S_N) <=9999 AND ISNUMERIC(N_D_P2_S_N)) OR ( EMPTY(N_D_P2_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_I2_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_I2_S_N)>=0.00  AND  VAL(TAS_I2_S_N) <=1000.00 AND ISNUMERIC(TAS_I2_S_N)) OR ( EMPTY(TAS_I2_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_U_D2_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_U_D2_S_N)>=0  AND  VAL(P_U_D2_S_N) <=100.00 AND ISNUMERIC(P_U_D2_S_N)) OR ( EMPTY(P_U_D2_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_N3_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_N3_S_N)>=0  AND  VAL(N_C_N3_S_N) <=99 AND ISNUMERIC(N_C_N3_S_N)) OR ( EMPTY(N_C_N3_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_C3_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C3_S_N)>=0  AND  VAL(N_D_C3_S_N) <=9999 AND ISNUMERIC(N_D_C3_S_N)) OR ( EMPTY(N_D_C3_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_P3_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P3_S_N)>=0  AND  VAL(N_D_P3_S_N) <=9999 AND ISNUMERIC(N_D_P3_S_N)) OR ( EMPTY(N_D_P3_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_I3_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_I3_S_N)>=0.00  AND  VAL(TAS_I3_S_N) <=1000.00 AND ISNUMERIC(TAS_I3_S_N)) OR ( EMPTY(TAS_I3_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_U_D3_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_U_D3_S_N)>=0  AND  VAL(P_U_D3_S_N) <=100.00 AND ISNUMERIC(P_U_D3_S_N)) OR ( EMPTY(P_U_D3_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_N4_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_N4_S_N)>=0  AND  VAL(N_C_N4_S_N) <=99 AND ISNUMERIC(N_C_N4_S_N)) OR ( EMPTY(N_C_N4_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_C4_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C4_S_N)>=0  AND  VAL(N_D_C4_S_N) <=9999 AND ISNUMERIC(N_D_C4_S_N)) OR ( EMPTY(N_D_C4_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_P4_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P4_S_N)>=0  AND  VAL(N_D_P4_S_N) <=9999 AND ISNUMERIC(N_D_P4_S_N)) OR ( EMPTY(N_D_P4_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_I4_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_I4_S_N)>=0.00  AND  VAL(TAS_I4_S_N) <=1000.00 AND ISNUMERIC(TAS_I4_S_N)) OR ( EMPTY(TAS_I4_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_USO_DI_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_USO_DI_S)>=1.00  AND  VAL(P_USO_DI_S) <=100.00 AND ISNUMERIC(P_USO_DI_S)) OR ( EMPTY(P_USO_DI_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_NV_U_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_NV_U_A)>=0  AND  VAL(N_C_NV_U_A) <=99 AND ISNUMERIC(N_C_NV_U_A)) OR ( EMPTY(N_C_NV_U_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_DS_D_U_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_DS_D_U_A)>=1  AND  VAL(N_DS_D_U_A) <=999 AND ISNUMERIC(N_DS_D_U_A)) OR ( EMPTY(N_DS_D_U_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_IN_U_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_IN_U_A)>=0.00  AND  VAL(TAS_IN_U_A) <=1000.00 AND ISNUMERIC(TAS_IN_U_A)) OR ( EMPTY(TAS_IN_U_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_PR_U_D_U_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PR_U_D_U_A)>=1.00  AND  VAL(PR_U_D_U_A) <=100.00 AND ISNUMERIC(PR_U_D_U_A)) OR ( EMPTY(PR_U_D_U_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_NV_V_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_NV_V_A)>=0  AND  VAL(N_C_NV_V_A) <=99 AND ISNUMERIC(N_C_NV_V_A)) OR ( EMPTY(N_C_NV_V_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_DS_D_V_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_DS_D_V_A)>=1  AND  VAL(N_DS_D_V_A) <=999 AND ISNUMERIC(N_DS_D_V_A)) OR ( EMPTY(N_DS_D_V_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_IN_V_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_IN_V_A)>=0.00  AND  VAL(TAS_IN_V_A) <=1000.00 AND ISNUMERIC(TAS_IN_V_A)) OR ( EMPTY(TAS_IN_V_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_PR_U_D_V_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PR_U_D_V_A)>=1.00  AND  VAL(PR_U_D_V_A) <=100.00 AND ISNUMERIC(PR_U_D_V_A)) OR ( EMPTY(PR_U_D_V_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_CAS_PL_S_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAS_PL_S_A,'1','2')) OR ( EMPTY(CAS_PL_S_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_PL_S_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_PL_S_A)>=1  AND  VAL(N_C_PL_S_A) <=999 AND ISNUMERIC(N_C_PL_S_A)) OR ( EMPTY(N_C_PL_S_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_U_D4_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_U_D4_S_N)>=0  AND  VAL(P_U_D4_S_N) <=100.00 AND ISNUMERIC(P_U_D4_S_N)) OR ( EMPTY(P_U_D4_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_N5_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_N5_S_N)>=0  AND  VAL(N_C_N5_S_N) <=99 AND ISNUMERIC(N_C_N5_S_N)) OR ( EMPTY(N_C_N5_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_C5_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C5_S_N)>=0  AND  VAL(N_D_C5_S_N) <=9999 AND ISNUMERIC(N_D_C5_S_N)) OR ( EMPTY(N_D_C5_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_P5_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P5_S_N)>=0  AND  VAL(N_D_P5_S_N) <=9999 AND ISNUMERIC(N_D_P5_S_N)) OR ( EMPTY(N_D_P5_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_I5_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_I5_S_N)>=0.00  AND  VAL(TAS_I5_S_N) <=1000.00 AND ISNUMERIC(TAS_I5_S_N)) OR ( EMPTY(TAS_I5_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_U_D5_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_U_D5_S_N)>=0  AND  VAL(P_U_D5_S_N) <=100.00 AND ISNUMERIC(P_U_D5_S_N)) OR ( EMPTY(P_U_D5_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_N1_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_N1_V_N)>=0  AND  VAL(N_C_N1_V_N) <=99 AND ISNUMERIC(N_C_N1_V_N)) OR ( EMPTY(N_C_N1_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_C1_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C1_V_N)>=0  AND  VAL(N_D_C1_V_N) <=9999 AND ISNUMERIC(N_D_C1_V_N)) OR ( EMPTY(N_D_C1_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_P1_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P1_V_N)>=0  AND  VAL(N_D_P1_V_N) <=9999 AND ISNUMERIC(N_D_P1_V_N)) OR ( EMPTY(N_D_P1_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_I1_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_I1_V_N)>=0.00  AND  VAL(TAS_I1_V_N) <=1000.00 AND ISNUMERIC(TAS_I1_V_N)) OR ( EMPTY(TAS_I1_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_U_D1_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_U_D1_V_N)>=0  AND  VAL(P_U_D1_V_N) <=100.00 AND ISNUMERIC(P_U_D1_V_N)) OR ( EMPTY(P_U_D1_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_N2_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_N2_V_N)>=0  AND  VAL(N_C_N2_V_N) <=99 AND ISNUMERIC(N_C_N2_V_N)) OR ( EMPTY(N_C_N2_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_C2_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C2_V_N)>=0  AND  VAL(N_D_C2_V_N) <=9999 AND ISNUMERIC(N_D_C2_V_N)) OR ( EMPTY(N_D_C2_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_P2_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P2_V_N)>=0  AND  VAL(N_D_P2_V_N) <=9999 AND ISNUMERIC(N_D_P2_V_N)) OR ( EMPTY(N_D_P2_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_I2_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_I2_V_N)>=0.00  AND  VAL(TAS_I2_V_N) <=1000.00 AND ISNUMERIC(TAS_I2_V_N)) OR ( EMPTY(TAS_I2_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_U_D2_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_U_D2_V_N)>=0  AND  VAL(P_U_D2_V_N) <=100.00 AND ISNUMERIC(P_U_D2_V_N)) OR ( EMPTY(P_U_D2_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_N3_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_N3_V_N)>=0  AND  VAL(N_C_N3_V_N) <=99 AND ISNUMERIC(N_C_N3_V_N)) OR ( EMPTY(N_C_N3_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_C3_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C3_V_N)>=0  AND  VAL(N_D_C3_V_N) <=9999 AND ISNUMERIC(N_D_C3_V_N)) OR ( EMPTY(N_D_C3_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_CAS_PL_U_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAS_PL_U_A,'1','2')) OR ( EMPTY(CAS_PL_U_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_PL_U_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_PL_U_A)>=1  AND  VAL(N_C_PL_U_A) <=999 AND ISNUMERIC(N_C_PL_U_A)) OR ( EMPTY(N_C_PL_U_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_P3_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P3_V_N)>=0  AND  VAL(N_D_P3_V_N) <=9999 AND ISNUMERIC(N_D_P3_V_N)) OR ( EMPTY(N_D_P3_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_I3_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_I3_V_N)>=0.00  AND  VAL(TAS_I3_V_N) <=1000.00 AND ISNUMERIC(TAS_I3_V_N)) OR ( EMPTY(TAS_I3_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_U_D3_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_U_D3_V_N)>=0  AND  VAL(P_U_D3_V_N) <=100.00 AND ISNUMERIC(P_U_D3_V_N)) OR ( EMPTY(P_U_D3_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_N4_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_N4_V_N)>=0  AND  VAL(N_C_N4_V_N) <=99 AND ISNUMERIC(N_C_N4_V_N)) OR ( EMPTY(N_C_N4_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_C4_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C4_V_N)>=0  AND  VAL(N_D_C4_V_N) <=9999 AND ISNUMERIC(N_D_C4_V_N)) OR ( EMPTY(N_D_C4_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_P4_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P4_V_N)>=0  AND  VAL(N_D_P4_V_N) <=9999 AND ISNUMERIC(N_D_P4_V_N)) OR ( EMPTY(N_D_P4_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_I4_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_I4_V_N)>=0.00  AND  VAL(TAS_I4_V_N) <=1000.00 AND ISNUMERIC(TAS_I4_V_N)) OR ( EMPTY(TAS_I4_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_U_D4_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_U_D4_V_N)>=0  AND  VAL(P_U_D4_V_N) <=100.00 AND ISNUMERIC(P_U_D4_V_N)) OR ( EMPTY(P_U_D4_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_N5_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_N5_V_N)>=0  AND  VAL(N_C_N5_V_N) <=99 AND ISNUMERIC(N_C_N5_V_N)) OR ( EMPTY(N_C_N5_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_C5_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C5_V_N)>=0  AND  VAL(N_D_C5_V_N) <=9999 AND ISNUMERIC(N_D_C5_V_N)) OR ( EMPTY(N_D_C5_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_D_P5_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P5_V_N)>=0  AND  VAL(N_D_P5_V_N) <=9999 AND ISNUMERIC(N_D_P5_V_N)) OR ( EMPTY(N_D_P5_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_I5_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_I5_V_N)>=0.00  AND  VAL(TAS_I5_V_N) <=1000.00 AND ISNUMERIC(TAS_I5_V_N)) OR ( EMPTY(TAS_I5_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_U_D5_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_U_D5_V_N)>=0  AND  VAL(P_U_D5_V_N) <=100.00 AND ISNUMERIC(P_U_D5_V_N)) OR ( EMPTY(P_U_D5_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_SER_UCIV_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SER_UCIV_P)>=1  AND  VAL(SER_UCIV_P) <= 99 AND ISNUMERIC(SER_UCIV_P)) OR ( EMPTY(SER_UCIV_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_CAM_UCIV_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAM_UCIV_P)>=1  AND  VAL(CAM_UCIV_P) <= 99 AND ISNUMERIC(CAM_UCIV_P)) OR ( EMPTY(CAM_UCIV_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_DIAS_P_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_DIAS_P_P)>= 1  AND  VAL(N_DIAS_P_P) <= 9999 AND ISNUMERIC(N_DIAS_P_P)) OR ( EMPTY(N_DIAS_P_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_NV_S_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_NV_S_P)>=0  AND  VAL(N_C_NV_S_P) <=99 AND ISNUMERIC(N_C_NV_S_P)) OR ( EMPTY(N_C_NV_S_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_DS_D_S_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_DS_D_S_P)>=1  AND  VAL(N_DS_D_S_P) <=999 AND ISNUMERIC(N_DS_D_S_P)) OR ( EMPTY(N_DS_D_S_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_IN_S_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_IN_S_P)>=0.00  AND  VAL(TAS_IN_S_P) <=1000.00 AND ISNUMERIC(TAS_IN_S_P)) OR ( EMPTY(TAS_IN_S_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_P_USO_DI_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(P_USO_DI_P)>=1.00  AND  VAL(P_USO_DI_P) <=100.00 AND ISNUMERIC(P_USO_DI_P)) OR ( EMPTY(P_USO_DI_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_NV_U_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_NV_U_P)>=0  AND  VAL(N_C_NV_U_P) <=99 AND ISNUMERIC(N_C_NV_U_P)) OR ( EMPTY(N_C_NV_U_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_DS_D_U_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_DS_D_U_P)>=1  AND  VAL(N_DS_D_U_P) <=999 AND ISNUMERIC(N_DS_D_U_P)) OR ( EMPTY(N_DS_D_U_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_IN_U_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_IN_U_P)>=0.00  AND  VAL(TAS_IN_U_P) <=1000.00 AND ISNUMERIC(TAS_IN_U_P)) OR ( EMPTY(TAS_IN_U_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_PR_U_D_U_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PR_U_D_U_P)>=1.00  AND  VAL(PR_U_D_U_P) <=100.00 AND ISNUMERIC(PR_U_D_U_P)) OR ( EMPTY(PR_U_D_U_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_NV_V_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_NV_V_P)>=0  AND  VAL(N_C_NV_V_P) <=99 AND ISNUMERIC(N_C_NV_V_P)) OR ( EMPTY(N_C_NV_V_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_DS_D_V_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_DS_D_V_P)>=1  AND  VAL(N_DS_D_V_P) <=999 AND ISNUMERIC(N_DS_D_V_P)) OR ( EMPTY(N_DS_D_V_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_TAS_IN_V_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TAS_IN_V_P)>=0.00  AND  VAL(TAS_IN_V_P) <=1000.00 AND ISNUMERIC(TAS_IN_V_P)) OR ( EMPTY(TAS_IN_V_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_PR_U_D_V_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PR_U_D_V_P)>=1.00  AND  VAL(PR_U_D_V_P) <=100.00 AND ISNUMERIC(PR_U_D_V_P)) OR ( EMPTY(PR_U_D_V_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_CAS_PL_S_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAS_PL_S_P,'1','2')) OR ( EMPTY(CAS_PL_S_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_PL_S_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_PL_S_P)>=1  AND  VAL(N_C_PL_S_P) <=999 AND ISNUMERIC(N_C_PL_S_P)) OR ( EMPTY(N_C_PL_S_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_CAS_PL_U_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAS_PL_U_P,'1','2')) OR ( EMPTY(CAS_PL_U_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_PL_U_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_PL_U_P)>=1  AND  VAL(N_C_PL_U_P) <=999 AND ISNUMERIC(N_C_PL_U_P)) OR ( EMPTY(N_C_PL_U_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_CAS_PL_V_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAS_PL_V_P,'1','2')) OR ( EMPTY(CAS_PL_V_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_N_C_PL_V_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_C_PL_V_P)>=1  AND  VAL(N_C_PL_V_P) <=999 AND ISNUMERIC(N_C_PL_V_P)) OR ( EMPTY(N_C_PL_V_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN1_N_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN1_N_A>=1 AND ISNUMERIC(AS_MN1_N_A)) OR (AS_MN1_N_A=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_NUM_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_CON))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN1_M_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN1_M_N>=0  AND AS_MN1_M_N <=999 AND ISNUMERIC(AS_MN1_M_N)) OR (AS_MN1_M_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN2_M_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN2_M_N>=0  AND AS_MN2_M_N <=999 AND ISNUMERIC(AS_MN2_M_N)) OR (AS_MN2_M_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN3_M_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN3_M_N>=0  AND AS_MN3_M_N <=999 AND ISNUMERIC(AS_MN3_M_N)) OR (AS_MN3_M_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN4_N_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN4_N_N>=0  AND AS_MN4_N_N <=999 AND ISNUMERIC(AS_MN4_N_N)) OR (AS_MN4_N_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN5_N_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN5_N_N>=0  AND AS_MN5_N_N <=999 AND ISNUMERIC(AS_MN5_N_N)) OR (AS_MN5_N_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN1_O_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN1_O_N>=0  AND AS_MN1_O_N <=999 AND ISNUMERIC(AS_MN1_O_N)) OR (AS_MN1_O_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN2_P_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN2_P_N>=0  AND AS_MN2_P_N <=999 AND ISNUMERIC(AS_MN2_P_N)) OR (AS_MN2_P_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN3_P_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN3_P_N>=0  AND AS_MN3_P_N <=999 AND ISNUMERIC(AS_MN3_P_N)) OR (AS_MN3_P_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN4_Q_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN4_Q_N>=0  AND AS_MN4_Q_N <=999 AND ISNUMERIC(AS_MN4_Q_N)) OR (AS_MN4_Q_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_80_AS_MN5_R_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((AS_MN5_R_N>=0  AND AS_MN5_R_N <=999 AND ISNUMERIC(AS_MN5_R_N)) OR (AS_MN5_R_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_MES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(MES)>=1  AND  VAL(MES) <=12 AND ISNUMERIC(MES)) AND ( .NOT. EMPTY(MES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(AÑO)>=2011 AND ISNUMERIC(AÑO)) AND ( .NOT. EMPTY(AÑO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_NUM_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_CON))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_T_CAM_UCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(T_CAM_UCIA)>=1 AND ISNUMERIC(T_CAM_UCIA)) OR ( EMPTY(T_CAM_UCIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_TIPOS_VIGI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPOS_VIGI,'1','2','3')) AND ( .NOT. EMPTY(TIPOS_VIGI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_T_DCO_UCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(T_DCO_UCIA)>=1 AND ISNUMERIC(T_DCO_UCIA)) OR ( EMPTY(T_DCO_UCIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_T_DCD_UCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(T_DCD_UCIA)>=1 AND ISNUMERIC(T_DCD_UCIA)) OR ( EMPTY(T_DCD_UCIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_PRO_O_UCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PRO_O_UCIA)>=0.00  AND  VAL(PRO_O_UCIA) <=1.00 AND ISNUMERIC(PRO_O_UCIA)) OR ( EMPTY(PRO_O_UCIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_CEF_UCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_CEF_UCIA)>=0.0 AND ISNUMERIC(G_CEF_UCIA)) OR ( EMPTY(G_CEF_UCIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_CEF_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_CEF_A)>= 0  AND  VAL(DDD_CEF_A) <=150 AND ISNUMERIC(DDD_CEF_A)) OR ( EMPTY(DDD_CEF_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_MER_UCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_MER_UCIA)>=0.0 AND ISNUMERIC(G_MER_UCIA)) OR ( EMPTY(G_MER_UCIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_MER_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_MER_A)>= 0  AND  VAL(DDD_MER_A) <=150 AND ISNUMERIC(DDD_MER_A)) OR ( EMPTY(DDD_MER_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_PIP_UCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_PIP_UCIA)>=0.0 AND ISNUMERIC(G_PIP_UCIA)) OR ( EMPTY(G_PIP_UCIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_PIP_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_PIP_A)>= 0  AND  VAL(DDD_PIP_A) <=150 AND ISNUMERIC(DDD_PIP_A)) OR ( EMPTY(DDD_PIP_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_VAN_UCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_VAN_UCIA)>=0.0 AND ISNUMERIC(G_VAN_UCIA)) OR ( EMPTY(G_VAN_UCIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_VAN_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_VAN_A)>= 0  AND  VAL(DDD_VAN_A) <=150 AND ISNUMERIC(DDD_VAN_A)) OR ( EMPTY(DDD_VAN_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_T_CAM_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(T_CAM_SHA)>=1 AND ISNUMERIC(T_CAM_SHA)) OR ( EMPTY(T_CAM_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_T_DCO_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(T_DCO_SHA)>=1 AND ISNUMERIC(T_DCO_SHA)) OR ( EMPTY(T_DCO_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_T_DCD_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(T_DCD_SHA)>=1 AND ISNUMERIC(T_DCD_SHA)) OR ( EMPTY(T_DCD_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_PRO_O_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(PRO_O_SHA)>=0.00  AND  VAL(PRO_O_SHA) <=1.00 AND ISNUMERIC(PRO_O_SHA)) OR ( EMPTY(PRO_O_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_CEF_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_CEF_SHA)>=0.0 AND ISNUMERIC(G_CEF_SHA)) OR ( EMPTY(G_CEF_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_CEF_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_CEF_S)>= 0  AND  VAL(DDD_CEF_S) <=150 AND ISNUMERIC(DDD_CEF_S)) OR ( EMPTY(DDD_CEF_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_CIPP_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_CIPP_SHA)>=0.0 AND ISNUMERIC(G_CIPP_SHA)) OR ( EMPTY(G_CIPP_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_CIPP_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_CIPP_S)>= 0  AND  VAL(DDD_CIPP_S) <=150 AND ISNUMERIC(DDD_CIPP_S)) OR ( EMPTY(DDD_CIPP_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_CIPE_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_CIPE_SHA)>=0.0 AND ISNUMERIC(G_CIPE_SHA)) OR ( EMPTY(G_CIPE_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_CIPE_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_CIPE_S)>= 0  AND  VAL(DDD_CIPE_S) <=150 AND ISNUMERIC(DDD_CIPE_S)) OR ( EMPTY(DDD_CIPE_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_MER_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_MER_SHA)>=0.0 AND ISNUMERIC(G_MER_SHA)) OR ( EMPTY(G_MER_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_MER_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_MER_S)>= 0  AND  VAL(DDD_MER_S) <=150 AND ISNUMERIC(DDD_MER_S)) OR ( EMPTY(DDD_MER_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_PIP_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_PIP_SHA)>=0.0 AND ISNUMERIC(G_PIP_SHA)) OR ( EMPTY(G_PIP_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_PIP_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_PIP_S)>= 0  AND  VAL(DDD_PIP_S) <=150 AND ISNUMERIC(DDD_PIP_S)) OR ( EMPTY(DDD_PIP_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_VAN_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_VAN_SHA)>=0.0 AND ISNUMERIC(G_VAN_SHA)) OR ( EMPTY(G_VAN_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_VAN_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_VAN_S)>= 0  AND  VAL(DDD_VAN_S) <=150 AND ISNUMERIC(DDD_VAN_S)) OR ( EMPTY(DDD_VAN_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_ERT_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_ERT_SHA)>=0.0 AND ISNUMERIC(G_ERT_SHA)) OR ( EMPTY(G_ERT_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_ERT_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_ERT_S)>= 0  AND  VAL(DDD_ERT_S) <=150 AND ISNUMERIC(DDD_ERT_S)) OR ( EMPTY(DDD_ERT_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_CEFE_SHA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_CEFE_SHA)>=0.0 AND ISNUMERIC(G_CEFE_SHA)) OR ( EMPTY(G_CEFE_SHA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_CEFE_S
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_CEFE_S)>= 0  AND  VAL(DDD_CEFE_S) <=150 AND ISNUMERIC(DDD_CEFE_S)) OR ( EMPTY(DDD_CEFE_S)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_ERT_UCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_ERT_UCIA)>=0.0 AND ISNUMERIC(G_ERT_UCIA)) OR ( EMPTY(G_ERT_UCIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_ERT_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_ERT_A)>= 0  AND  VAL(DDD_ERT_A) <=150 AND ISNUMERIC(DDD_ERT_A)) OR ( EMPTY(DDD_ERT_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_G_CEFE_UCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G_CEFE_UCI)>=0.0 AND ISNUMERIC(G_CEFE_UCI)) OR ( EMPTY(G_CEFE_UCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_81_DDD_CEFE_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DDD_CEFE_A)>= 0  AND  VAL(DDD_CEFE_A) <=150 AND ISNUMERIC(DDD_CEFE_A)) OR ( EMPTY(DDD_CEFE_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_SIT_DEFUNC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIT_DEFUNC,'1','2','3','4','5','6','9')) AND ( .NOT. EMPTY(SIT_DEFUNC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_NOM_MADRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_MADRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_APEL_MADRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(APEL_MADRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_TIP_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_MA,'RC','TI','CC','CE','PA','MS','AS','PE')) AND ( .NOT. EMPTY(TIP_IDE_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_NUM_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_MA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_EDAD_MADRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((EDAD_MADRE>= 10  AND EDAD_MADRE <=60 AND ISNUMERIC(EDAD_MADRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_NUM_HI_VIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_HI_VIV>= 0  AND NUM_HI_VIV <=20 AND ISNUMERIC(NUM_HI_VIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_NUM_HI_MUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((NUM_HI_MUE>= 1  AND NUM_HI_MUE <=20 AND ISNUMERIC(NUM_HI_MUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_EST_CONYUG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_CONYUG,'1','2','3','4','5','6','9')) AND ( .NOT. EMPTY(EST_CONYUG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_ULT_AÑO_ES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((ULT_AÑO_ES>= 0  AND ULT_AÑO_ES <=11 AND ISNUMERIC(ULT_AÑO_ES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_ASIS_MEDIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASIS_MEDIC,'1','2','3')) AND ( .NOT. EMPTY(ASIS_MEDIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_C_DIRECT_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(C_DIRECT_A))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_CLAS_CASO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_CASO,'1','2','3')) AND ( .NOT. EMPTY(CLAS_CASO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_82_EST_INGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_INGR,0,2,3)) OR (EST_INGR=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_PRI_NOM_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PRI_NOM_MA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_PRI_APE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PRI_APE_MA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_TIP_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE_MA,'RC','TI','CC','CE','PA','MS','AS','PE')) AND ( .NOT. EMPTY(TIP_IDE_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_NUM_IDE_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE_MA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_NIV_EDUCAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NIV_EDUCAT,'1','2','3','4','5')) OR ( EMPTY(NIV_EDUCAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_MENORES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((MENORES>=0  AND MENORES <= 20 AND ISNUMERIC(MENORES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_PESO_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_NAC>=900  AND PESO_NAC <=5000 AND ISNUMERIC(PESO_NAC)) OR (PESO_NAC=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_TALLA_NAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA_NAC>= 30.0  AND TALLA_NAC <= 55.0 AND ISNUMERIC(TALLA_NAC)) OR (TALLA_NAC=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_EDAD_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((EDAD_GES>=20  AND EDAD_GES <= 45 AND ISNUMERIC(EDAD_GES)) OR (EDAD_GES=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_T_LECHEM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((T_LECHEM>=0 AND ISNUMERIC(T_LECHEM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_E_COMPLEM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((E_COMPLEM>=0 AND ISNUMERIC(E_COMPLEM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_CREC_DLLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CREC_DLLO,'1','2')) AND ( .NOT. EMPTY(CREC_DLLO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_ESQ_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESQ_VAC,'1','2','3')) AND ( .NOT. EMPTY(ESQ_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_CARNE_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARNE_VAC,'1','2')) OR ( EMPTY(CARNE_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_PESO_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_ACT>=1.0  AND PESO_ACT <= 50.0 AND ISNUMERIC(PESO_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_TALLA_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA_ACT>=45.0  AND TALLA_ACT <= 150.0 AND ISNUMERIC(TALLA_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_PER_BRAQUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PER_BRAQUI>=6.0  AND PER_BRAQUI <= 30.0 AND ISNUMERIC(PER_BRAQUI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_ZSCORE_TE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((ZSCORE_TE>=-8  AND ZSCORE_TE <=6 AND ISNUMERIC(ZSCORE_TE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_CLAS_TALLA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_TALLA,'1','2','3')) OR ( EMPTY(CLAS_TALLA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_EDEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EDEMA,'1','2')) AND ( .NOT. EMPTY(EDEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_DELGADEZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DELGADEZ,'1','2')) AND ( .NOT. EMPTY(DELGADEZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_PIEL_RESE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIEL_RESE,'1','2')) AND ( .NOT. EMPTY(PIEL_RESE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_HIPERPIGM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPERPIGM,'1','2')) AND ( .NOT. EMPTY(HIPERPIGM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_LES_CABEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LES_CABEL,'1','2')) AND ( .NOT. EMPTY(LES_CABEL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_PALIDEZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PALIDEZ,'1','2')) AND ( .NOT. EMPTY(PALIDEZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_RUTA_ATENC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RUTA_ATENC,'1','2')) AND ( .NOT. EMPTY(RUTA_ATENC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_TIPO_MANEJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_MANEJ,'1','2')) OR ( EMPTY(TIPO_MANEJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_83_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SIND_BRONQ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SIND_BRONQ,'1','2')) OR ( EMPTY(SIND_BRONQ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_EV_CORONAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EV_CORONAR,'1','2')) OR ( EMPTY(EV_CORONAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SEAN_SSSN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEAN_SSSN,'1','2')) OR ( EMPTY(SEAN_SSSN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOS,'1','2')) OR ( EMPTY(TOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DISNEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DISNEA,'1','2')) OR ( EMPTY(DISNEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DIF_RESPIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIF_RESPIR,'1','2')) OR ( EMPTY(DIF_RESPIR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DOLOR_TORA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR_TORA,'1','2')) OR ( EMPTY(DOLOR_TORA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NAUSEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NAUSEA,'1','2')) OR ( EMPTY(NAUSEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) OR ( EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) OR ( EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DOLOR_ABDO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR_ABDO,'1','2')) OR ( EMPTY(DOLOR_ABDO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_OTRA_CLINI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRA_CLINI,'1','2')) OR ( EMPTY(OTRA_CLINI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_FR_USO_CIG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FR_USO_CIG,'1','2','3','4')) OR ( EMPTY(FR_USO_CIG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NICOTINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NICOTINA,'1','2')) OR ( EMPTY(NICOTINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SABOR_AROM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SABOR_AROM,'1','2')) OR ( EMPTY(SABOR_AROM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DER_CANABI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DER_CANABI,'1','2')) OR ( EMPTY(DER_CANABI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_OTRASSUSTA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRASSUSTA,'1','2')) OR ( EMPTY(OTRASSUSTA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_INTOXICACI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INTOXICACI,'1','2')) OR ( EMPTY(INTOXICACI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_QUEMADURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUEMADURA,'1','2')) OR ( EMPTY(QUEMADURA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ALERGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALERGIA,'1','2')) OR ( EMPTY(ALERGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CONS_CIGAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_CIGAR,'1','2')) OR ( EMPTY(CONS_CIGAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CONS_MARIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_MARIH,'1','2')) OR ( EMPTY(CONS_MARIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CONS_COCAI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_COCAI,'1','2')) OR ( EMPTY(CONS_COCAI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CONS_BAZUC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_BAZUC,'1','2')) OR ( EMPTY(CONS_BAZUC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CONS_HEROI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONS_HEROI,'1','2')) OR ( EMPTY(CONS_HEROI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ASMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASMA,'1','2')) OR ( EMPTY(ASMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_EPOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPOC,'1','2')) OR ( EMPTY(EPOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ALERG_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALERG_RESP,'1','2')) OR ( EMPTY(ALERG_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_FIBROS_QUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIBROS_QUI,'1','2')) OR ( EMPTY(FIBROS_QUI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ENF_CORONA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENF_CORONA,'1','2')) OR ( EMPTY(ENF_CORONA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NOMBRE_ELE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOMBRE_ELE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_OCASIO_POR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OCASIO_POR,'1','4')) AND ( .NOT. EMPTY(OCASIO_POR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ASFIXIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASFIXIA,'1','2')) OR ( EMPTY(ASFIXIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ESTRANGULA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTRANGULA,'1','2')) OR ( EMPTY(ESTRANGULA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_HERIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HERIDA,'1','2')) OR ( EMPTY(HERIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TRAUMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAUMA,'1','2')) OR ( EMPTY(TRAUMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CHOQ_ELECT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CHOQ_ELECT,'1','2')) OR ( EMPTY(CHOQ_ELECT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_FRACTURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FRACTURA,'1','2')) OR ( EMPTY(FRACTURA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_POLITRAUMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(POLITRAUMA,'1','2')) OR ( EMPTY(POLITRAUMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_AMPUTACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AMPUTACION,'1','2')) OR ( EMPTY(AMPUTACION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_QUEMADURAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUEMADURAS,'1','2')) OR ( EMPTY(QUEMADURAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_INTOXICAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INTOXICAC,'1','2')) OR ( EMPTY(INTOXICAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_INFECCION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFECCION,'1','2')) OR ( EMPTY(INFECCION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SEPSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEPSIS,'1','2')) OR ( EMPTY(SEPSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PERFORACIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PERFORACIO,'1','2')) OR ( EMPTY(PERFORACIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_HEMORRAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMORRAGIA,'1','2')) OR ( EMPTY(HEMORRAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NECROSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NECROSIS,'1','2')) OR ( EMPTY(NECROSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_EMBOLIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EMBOLIA,'1','2')) OR ( EMPTY(EMBOLIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DEPRE_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEPRE_RESP,'1','2')) OR ( EMPTY(DEPRE_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CRANEO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRANEO,'1','2')) OR ( EMPTY(CRANEO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CARA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CARA,'1','2')) OR ( EMPTY(CARA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_OJOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OJOS,'1','2')) OR ( EMPTY(OJOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_NARIZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NARIZ,'1','2')) OR ( EMPTY(NARIZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_OREJAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OREJAS,'1','2')) OR ( EMPTY(OREJAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_BOCA_DIENT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BOCA_DIENT,'1','2')) OR ( EMPTY(BOCA_DIENT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CUELLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUELLO,'1','2')) OR ( EMPTY(CUELLO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_BRAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BRAZO,'1','2')) OR ( EMPTY(BRAZO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ANTEBRAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANTEBRAZO,'1','2')) OR ( EMPTY(ANTEBRAZO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MANO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MANO,'1','2')) OR ( EMPTY(MANO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DEDOS_MANO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEDOS_MANO,'1','2')) OR ( EMPTY(DEDOS_MANO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TORX_ANTER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TORX_ANTER,'1','2')) OR ( EMPTY(TORX_ANTER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TORX_POST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TORX_POST,'1','2')) OR ( EMPTY(TORX_POST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MAMAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAMAS,'1','2')) OR ( EMPTY(MAMAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ABDOMEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ABDOMEN,'1','2')) OR ( EMPTY(ABDOMEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PELV_PERIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PELV_PERIN,'1','2')) OR ( EMPTY(PELV_PERIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_GENITALES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GENITALES,'1','2')) OR ( EMPTY(GENITALES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MUSLOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUSLOS,'1','2')) OR ( EMPTY(MUSLOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PIERNAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIERNAS,'1','2')) OR ( EMPTY(PIERNAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PIES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIES,'1','2')) OR ( EMPTY(PIES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_DEDOS_PIES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEDOS_PIES,'1','2')) OR ( EMPTY(DEDOS_PIES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ORG_INTERN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ORG_INTERN,'1','2')) OR ( EMPTY(ORG_INTERN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PIEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIEL,'1','2')) OR ( EMPTY(PIEL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MAQUINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAQUINA,'1','2')) OR ( EMPTY(MAQUINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MEDIOS_TRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MEDIOS_TRA,'1','2')) OR ( EMPTY(MEDIOS_TRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_JUGUETES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JUGUETES,'1','2')) OR ( EMPTY(JUGUETES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ELECTR_ILU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ELECTR_ILU,'1','2')) OR ( EMPTY(ELECTR_ILU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_VEST_ACCES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VEST_ACCES,'1','2')) OR ( EMPTY(VEST_ACCES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_UTIL_ESCOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UTIL_ESCOL,'1','2')) OR ( EMPTY(UTIL_ESCOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_UTEN_COMED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UTEN_COMED,'1','2')) OR ( EMPTY(UTEN_COMED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ACC_INFAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACC_INFAN,'1','2')) OR ( EMPTY(ACC_INFAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_EQU_DEPORT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EQU_DEPORT,'1','2')) OR ( EMPTY(EQU_DEPORT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ELECT_AUDI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ELECT_AUDI,'1','2')) OR ( EMPTY(ELECT_AUDI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_BELLEZA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BELLEZA,'1','2')) OR ( EMPTY(BELLEZA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_MEDICAMEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MEDICAMEN,'1','2')) OR ( EMPTY(MEDICAMEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_APAR_ESTET
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APAR_ESTET,'1','2')) OR ( EMPTY(APAR_ESTET)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_EQU_BIOMED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EQU_BIOMED,'1','2')) OR ( EMPTY(EQU_BIOMED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_HOGAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HOGAR,'1','2')) OR ( EMPTY(HOGAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_COLEGIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COLEGIO,'1','2')) OR ( EMPTY(COLEGIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CALLE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CALLE,'1','2')) OR ( EMPTY(CALLE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PARQUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PARQUE,'1','2')) OR ( EMPTY(PARQUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_INDUSTRIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INDUSTRIA,'1','2')) OR ( EMPTY(INDUSTRIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_CENTR_ESTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CENTR_ESTE,'1','2')) OR ( EMPTY(CENTR_ESTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_SPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SPA,'1','2')) OR ( EMPTY(SPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_IPS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IPS,'1','2')) OR ( EMPTY(IPS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PROC_QUIRU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROC_QUIRU,'1','2','3','4')) OR ( EMPTY(PROC_QUIRU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_TIPO_PROF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_PROF,'1','2','3','4','5','6')) OR ( EMPTY(TIPO_PROF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_HOSPITALIZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HOSPITALIZ,'1','2')) OR ( EMPTY(HOSPITALIZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_UCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UCI,'1','2')) OR ( EMPTY(UCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_PROD_QUIMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROD_QUIMI,'1','2')) OR ( EMPTY(PROD_QUIMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ESTABLECIM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTABLECIM,'1','2')) OR ( EMPTY(ESTABLECIM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_GLUTEOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GLUTEOS,'1','2')) OR ( EMPTY(GLUTEOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_84_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_COMPL_NEUR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMPL_NEUR,'1','2')) AND ( .NOT. EMPTY(COMPL_NEUR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_DESPLAZAMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESPLAZAMI,'1','2')) OR ( EMPTY(DESPLAZAMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_HUBO_1_ECO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HUBO_1_ECO,'1','2')) OR ( EMPTY(HUBO_1_ECO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_EDAD_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EDAD_GES)>= 0  AND  VAL(EDAD_GES) <= 42 AND ISNUMERIC(EDAD_GES)) OR ( EMPTY(EDAD_GES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_SEG_EAPB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEG_EAPB,'1','2','3')) OR ( EMPTY(SEG_EAPB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_GES_TER_EM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GES_TER_EM,'1','2')) OR ( EMPTY(GES_TER_EM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_CON_FIN_PR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CON_FIN_PR,'1','2','3')) OR ( EMPTY(CON_FIN_PR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_PER_CEFALI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PER_CEFALI>=0  AND PER_CEFALI <= 99 AND ISNUMERIC(PER_CEFALI)) OR (PER_CEFALI=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_AUTOPSIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AUTOPSIA,'1','2')) OR ( EMPTY(AUTOPSIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_DEF_CONGEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DEF_CONGEN,'1','2')) OR ( EMPTY(DEF_CONGEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_MUE_SUERO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUE_SUERO,'1','2')) OR ( EMPTY(MUE_SUERO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_MUE_CORDON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUE_CORDON,'1','2')) OR ( EMPTY(MUE_CORDON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_EXANTEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EXANTEMA,'1','2')) AND ( .NOT. EMPTY(EXANTEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_HIPEREMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPEREMIA,'1','2')) AND ( .NOT. EMPTY(HIPEREMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_ARTRALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRALGIAS,'1','2')) AND ( .NOT. EMPTY(ARTRALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_MIALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIALGIAS,'1','2')) AND ( .NOT. EMPTY(MIALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_85_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_PIEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIEL,'1','2')) OR ( EMPTY(PIEL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_HIGADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIGADO,'1','2')) OR ( EMPTY(HIGADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_BAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BAZO,'1','2')) OR ( EMPTY(BAZO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_PULMON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PULMON,'1','2')) OR ( EMPTY(PULMON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_CEREBRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEREBRO,'1','2')) OR ( EMPTY(CEREBRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_MIOCARDIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIOCARDIO,'1','2')) OR ( EMPTY(MIOCARDIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_MEDULA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MEDULA,'1','2')) OR ( EMPTY(MEDULA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_RIÑON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RIÑON,'1','2')) OR ( EMPTY(RIÑON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_ARTRALGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRALGIA,'1','2')) AND ( .NOT. EMPTY(ARTRALGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_RASH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RASH,'1','2')) AND ( .NOT. EMPTY(RASH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) AND ( .NOT. EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_86_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) AND ( .NOT. EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_TIPO_UCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_UCI,'1','2','3')) AND ( .NOT. EMPTY(TIPO_UCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_FEC_INGUCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_INGUCI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_REMITIDO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REMITIDO,'1','2')) AND ( .NOT. EMPTY(REMITIDO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_CASO_EXTRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CASO_EXTRA,'1','2')) OR ( EMPTY(CASO_EXTRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_TIPO_IAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_IAD,'1','2','3')) AND ( .NOT. EMPTY(TIPO_IAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_CRIT_NAV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRIT_NAV,'1','2','3')) OR ( EMPTY(CRIT_NAV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_CRIT_ITSAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRIT_ITSAC,'1','2','3')) OR ( EMPTY(CRIT_ITSAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_CRIT_ISTUA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRIT_ISTUA,'1','2','3','4')) OR ( EMPTY(CRIT_ISTUA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_FEC_DIAG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_DIAG))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_IAD_POLIM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IAD_POLIM,'1','2')) OR ( EMPTY(IAD_POLIM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_VENTILADOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VENTILADOR,'1','2')) AND ( .NOT. EMPTY(VENTILADOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_CAT_CENTRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAT_CENTRA,'1','2')) AND ( .NOT. EMPTY(CAT_CENTRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_CAT_URINAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAT_URINAR,'1','2')) AND ( .NOT. EMPTY(CAT_URINAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_CANCER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CANCER,'1','2')) AND ( .NOT. EMPTY(CANCER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_DESNUTRIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESNUTRIC,'1','2')) AND ( .NOT. EMPTY(DESNUTRIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_DIABETES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIABETES,'1','2')) AND ( .NOT. EMPTY(DIABETES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_ENF_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENF_RENAL,'1','2')) AND ( .NOT. EMPTY(ENF_RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_EPOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPOC,'1','2')) AND ( .NOT. EMPTY(EPOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_INMUNOSUPR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INMUNOSUPR,'1','2')) AND ( .NOT. EMPTY(INMUNOSUPR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_VIH_SIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIH_SIDA,'1','2')) AND ( .NOT. EMPTY(VIH_SIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_INFEC_PREV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFEC_PREV,'1','2')) AND ( .NOT. EMPTY(INFEC_PREV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_TRAUMATISM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAUMATISM,'1','2')) AND ( .NOT. EMPTY(TRAUMATISM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_OBESIDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OBESIDAD,'1','2')) AND ( .NOT. EMPTY(OBESIDAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_OTRO_FACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRO_FACT,'1','2')) AND ( .NOT. EMPTY(OTRO_FACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_VALOR_PESO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((VALOR_PESO>=350  AND VALOR_PESO <=4500 AND ISNUMERIC(VALOR_PESO)) OR (VALOR_PESO=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_INSERT_UCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INSERT_UCI,'1','2')) AND ( .NOT. EMPTY(INSERT_UCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_87_RETIRO_48H
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RETIRO_48H,'1','2')) AND ( .NOT. EMPTY(RETIRO_48H)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_MES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(MES)>=1  AND  VAL(MES) <=12 AND ISNUMERIC(MES)) AND ( .NOT. EMPTY(MES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(AÑO)>=2011 AND ISNUMERIC(AÑO)) AND ( .NOT. EMPTY(AÑO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_NUM_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_CON))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_SER_UCI_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SER_UCI_A)>=1  AND  VAL(SER_UCI_A) <= 99 AND ISNUMERIC(SER_UCI_A)) OR ( EMPTY(SER_UCI_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_CAM_UCI_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAM_UCI_A)>=1  AND  VAL(CAM_UCI_A) <= 999 AND ISNUMERIC(CAM_UCI_A)) OR ( EMPTY(CAM_UCI_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_DIA_PAC_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DIA_PAC_A)>= 1  AND  VAL(DIA_PAC_A) <= 9999 AND ISNUMERIC(DIA_PAC_A)) OR ( EMPTY(DIA_PAC_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_ITSAC_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_ITSAC_A>=0  AND N_ITSAC_A <=99 AND ISNUMERIC(N_ITSAC_A)) OR (N_ITSAC_A=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_D_ITSAC_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_ITSAC_A)>=0  AND  VAL(D_ITSAC_A) <=9999 AND ISNUMERIC(D_ITSAC_A)) OR ( EMPTY(D_ITSAC_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_ISTUA_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_ISTUA_A>=0  AND N_ISTUA_A <=99 AND ISNUMERIC(N_ISTUA_A)) OR (N_ISTUA_A=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_D_ISTUA_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_ISTUA_A)>=0  AND  VAL(D_ISTUA_A) <=9999 AND ISNUMERIC(D_ISTUA_A)) OR ( EMPTY(D_ISTUA_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_NAV_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_NAV_A>=0  AND N_NAV_A <=99 AND ISNUMERIC(N_NAV_A)) OR (N_NAV_A=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_D_NAV_A
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_NAV_A)>=0  AND  VAL(D_NAV_A) <=9999 AND ISNUMERIC(D_NAV_A)) OR ( EMPTY(D_NAV_A)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_SER_UCI_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SER_UCI_P)>=1  AND  VAL(SER_UCI_P) <= 99 AND ISNUMERIC(SER_UCI_P)) OR ( EMPTY(SER_UCI_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_CAM_UCI_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAM_UCI_P)>=1  AND  VAL(CAM_UCI_P) <= 999 AND ISNUMERIC(CAM_UCI_P)) OR ( EMPTY(CAM_UCI_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_DIA_PAC_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DIA_PAC_P)>= 1  AND  VAL(DIA_PAC_P) <= 9999 AND ISNUMERIC(DIA_PAC_P)) OR ( EMPTY(DIA_PAC_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_ITSAC_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_ITSAC_P>=0  AND N_ITSAC_P <=99 AND ISNUMERIC(N_ITSAC_P)) OR (N_ITSAC_P=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_D_ITSAC_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_ITSAC_P)>=0  AND  VAL(D_ITSAC_P) <=9999 AND ISNUMERIC(D_ITSAC_P)) OR ( EMPTY(D_ITSAC_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_ISTUA_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_ISTUA_P>=0  AND N_ISTUA_P <=99 AND ISNUMERIC(N_ISTUA_P)) OR (N_ISTUA_P=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_D_ISTUA_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_ISTUA_P)>=0  AND  VAL(D_ISTUA_P) <=9999 AND ISNUMERIC(D_ISTUA_P)) OR ( EMPTY(D_ISTUA_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_NAV_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_NAV_P>=0  AND N_NAV_P <=99 AND ISNUMERIC(N_NAV_P)) OR (N_NAV_P=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_D_NAV_P
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_NAV_P)>=0  AND  VAL(D_NAV_P) <=9999 AND ISNUMERIC(D_NAV_P)) OR ( EMPTY(D_NAV_P)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_SER_UCI_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(SER_UCI_N)>=1  AND  VAL(SER_UCI_N) <=99 AND ISNUMERIC(SER_UCI_N)) OR ( EMPTY(SER_UCI_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_CAM_UCI_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAM_UCI_N)>=1  AND  VAL(CAM_UCI_N) <=999 AND ISNUMERIC(CAM_UCI_N)) OR ( EMPTY(CAM_UCI_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_DIA_PAC_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(DIA_PAC_N)>=1  AND  VAL(DIA_PAC_N) <=9999 AND ISNUMERIC(DIA_PAC_N)) OR ( EMPTY(DIA_PAC_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_C_N1_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_C_N1_S_N>=0  AND N_C_N1_S_N <=99 AND ISNUMERIC(N_C_N1_S_N)) OR (N_C_N1_S_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_C1_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C1_S_N)>=0  AND  VAL(N_D_C1_S_N) <=9999 AND ISNUMERIC(N_D_C1_S_N)) OR ( EMPTY(N_D_C1_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_P1_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P1_S_N)>=0  AND  VAL(N_D_P1_S_N) <=9999 AND ISNUMERIC(N_D_P1_S_N)) OR ( EMPTY(N_D_P1_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_C_N2_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_C_N2_S_N>=0  AND N_C_N2_S_N <=99 AND ISNUMERIC(N_C_N2_S_N)) OR (N_C_N2_S_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_C2_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C2_S_N)>=0  AND  VAL(N_D_C2_S_N) <=9999 AND ISNUMERIC(N_D_C2_S_N)) OR ( EMPTY(N_D_C2_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_P2_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P2_S_N)>=0  AND  VAL(N_D_P2_S_N) <=9999 AND ISNUMERIC(N_D_P2_S_N)) OR ( EMPTY(N_D_P2_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_C_N3_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_C_N3_S_N>=0  AND N_C_N3_S_N <=99 AND ISNUMERIC(N_C_N3_S_N)) OR (N_C_N3_S_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_C3_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C3_S_N)>=0  AND  VAL(N_D_C3_S_N) <=9999 AND ISNUMERIC(N_D_C3_S_N)) OR ( EMPTY(N_D_C3_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_P3_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P3_S_N)>=0  AND  VAL(N_D_P3_S_N) <=9999 AND ISNUMERIC(N_D_P3_S_N)) OR ( EMPTY(N_D_P3_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_C_N4_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_C_N4_S_N>=0  AND N_C_N4_S_N <=99 AND ISNUMERIC(N_C_N4_S_N)) OR (N_C_N4_S_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_C4_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C4_S_N)>=0  AND  VAL(N_D_C4_S_N) <=9999 AND ISNUMERIC(N_D_C4_S_N)) OR ( EMPTY(N_D_C4_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_P4_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P4_S_N)>=0  AND  VAL(N_D_P4_S_N) <=9999 AND ISNUMERIC(N_D_P4_S_N)) OR ( EMPTY(N_D_P4_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_C_N5_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_C_N5_S_N>=0  AND N_C_N5_S_N <=99 AND ISNUMERIC(N_C_N5_S_N)) OR (N_C_N5_S_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_C5_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C5_S_N)>=0  AND  VAL(N_D_C5_S_N) <=9999 AND ISNUMERIC(N_D_C5_S_N)) OR ( EMPTY(N_D_C5_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_P5_S_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P5_S_N)>=0  AND  VAL(N_D_P5_S_N) <=9999 AND ISNUMERIC(N_D_P5_S_N)) OR ( EMPTY(N_D_P5_S_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_C_N1_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_C_N1_V_N>=0  AND N_C_N1_V_N <=99 AND ISNUMERIC(N_C_N1_V_N)) OR (N_C_N1_V_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_C1_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C1_V_N)>=0  AND  VAL(N_D_C1_V_N) <=9999 AND ISNUMERIC(N_D_C1_V_N)) OR ( EMPTY(N_D_C1_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_P1_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P1_V_N)>=0  AND  VAL(N_D_P1_V_N) <=9999 AND ISNUMERIC(N_D_P1_V_N)) OR ( EMPTY(N_D_P1_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_C_N2_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_C_N2_V_N>=0  AND N_C_N2_V_N <=99 AND ISNUMERIC(N_C_N2_V_N)) OR (N_C_N2_V_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_C2_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C2_V_N)>=0  AND  VAL(N_D_C2_V_N) <=9999 AND ISNUMERIC(N_D_C2_V_N)) OR ( EMPTY(N_D_C2_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_P2_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P2_V_N)>=0  AND  VAL(N_D_P2_V_N) <=9999 AND ISNUMERIC(N_D_P2_V_N)) OR ( EMPTY(N_D_P2_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_C_N3_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_C_N3_V_N>=0  AND N_C_N3_V_N <=99 AND ISNUMERIC(N_C_N3_V_N)) OR (N_C_N3_V_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_C3_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C3_V_N)>=0  AND  VAL(N_D_C3_V_N) <=9999 AND ISNUMERIC(N_D_C3_V_N)) OR ( EMPTY(N_D_C3_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_P3_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P3_V_N)>=0  AND  VAL(N_D_P3_V_N) <=9999 AND ISNUMERIC(N_D_P3_V_N)) OR ( EMPTY(N_D_P3_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_C_N4_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_C_N4_V_N>=0  AND N_C_N4_V_N <=99 AND ISNUMERIC(N_C_N4_V_N)) OR (N_C_N4_V_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_C4_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C4_V_N)>=0  AND  VAL(N_D_C4_V_N) <=9999 AND ISNUMERIC(N_D_C4_V_N)) OR ( EMPTY(N_D_C4_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_P4_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P4_V_N)>=0  AND  VAL(N_D_P4_V_N) <=9999 AND ISNUMERIC(N_D_P4_V_N)) OR ( EMPTY(N_D_P4_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_C_N5_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_C_N5_V_N>=0  AND N_C_N5_V_N <=99 AND ISNUMERIC(N_C_N5_V_N)) OR (N_C_N5_V_N=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_C5_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_C5_V_N)>=0  AND  VAL(N_D_C5_V_N) <=9999 AND ISNUMERIC(N_D_C5_V_N)) OR ( EMPTY(N_D_C5_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_N_D_P5_V_N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(N_D_P5_V_N)>=0  AND  VAL(N_D_P5_V_N) <=9999 AND ISNUMERIC(N_D_P5_V_N)) OR ( EMPTY(N_D_P5_V_N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_88_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_CUADRO_CLI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CUADRO_CLI,'1','2')) AND ( .NOT. EMPTY(CUADRO_CLI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_NEX_EPI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NEX_EPI,'1','2')) AND ( .NOT. EMPTY(NEX_EPI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RADIOLOGIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RADIOLOGIC,'1','2')) AND ( .NOT. EMPTY(RADIOLOGIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ADA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADA,'1','2')) AND ( .NOT. EMPTY(ADA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TUBERCULIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TUBERCULIN,'1','2')) AND ( .NOT. EMPTY(TUBERCULIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_PRUEB_MOLE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRUEB_MOLE,'1','2')) AND ( .NOT. EMPTY(PRUEB_MOLE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RES_PR_MOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_PR_MOL,'1','2')) OR ( EMPTY(RES_PR_MOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ESP_PMOLEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_PMOLEC,'1','2','3','4','5')) OR ( EMPTY(ESP_PMOLEC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_DIABETES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIABETES,'1','2')) AND ( .NOT. EMPTY(DIABETES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_SILICOSIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SILICOSIS,'1','2')) AND ( .NOT. EMPTY(SILICOSIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ENFE_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENFE_RENAL,'1','2')) AND ( .NOT. EMPTY(ENFE_RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_EPOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPOC,'1','2')) AND ( .NOT. EMPTY(EPOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ENFE_HEPAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENFE_HEPAT,'1','2')) AND ( .NOT. EMPTY(ENFE_HEPAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_COND_TUBER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COND_TUBER,'1','2')) AND ( .NOT. EMPTY(COND_TUBER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TIP_TUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_TUB,'1','2')) AND ( .NOT. EMPTY(TIP_TUB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_LOCTBREXTR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LOCTBREXTR,'1','2','3','4','5','7','8','9','10','11','12')) OR ( EMPTY(LOCTBREXTR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_CLAS_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLAS_ANT,'1','2')) AND ( .NOT. EMPTY(CLAS_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_CLASCASO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLASCASO,'2','3','4','5','6','7')) OR ( EMPTY(CLASCASO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TRAB_SALUD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAB_SALUD,'1','2')) AND ( .NOT. EMPTY(TRAB_SALUD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_PREV_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PREV_VIH,'1','2')) AND ( .NOT. EMPTY(PREV_VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_PESO_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((PESO_ACT>=2.0  AND PESO_ACT <=250.0 AND ISNUMERIC(PESO_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TALLA_ACT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TALLA_ACT>=0.20  AND TALLA_ACT <=2.5 AND ISNUMERIC(TALLA_ACT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_BACILOSCOP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BACILOSCOP,'1','2')) AND ( .NOT. EMPTY(BACILOSCOP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RES_BK
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RES_BK,'1','2')) OR ( EMPTY(RES_BK)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_CULTIVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CULTIVO,'1','2')) AND ( .NOT. EMPTY(CULTIVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RESCULTIVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESCULTIVO,'1','2','3')) OR ( EMPTY(RESCULTIVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_HISTOPATOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HISTOPATOL,'1','2')) AND ( .NOT. EMPTY(HISTOPATOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RESHISTOPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESHISTOPA,'1','2')) OR ( EMPTY(RESHISTOPA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_CANCER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CANCER,'1','2')) AND ( .NOT. EMPTY(CANCER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ARTRITIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTRITIS,'1','2')) AND ( .NOT. EMPTY(ARTRITIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_DESNUTRICI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESNUTRICI,'1','2')) AND ( .NOT. EMPTY(DESNUTRICI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_PSF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PSF,'1','2')) OR ( EMPTY(PSF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_TIPO_RESIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_RESIS,'1','2','3','4','6','7','8')) OR ( EMPTY(TIPO_RESIS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ESTREPTOMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTREPTOMI,'1','2','3')) OR ( EMPTY(ESTREPTOMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ISONIAZIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ISONIAZIDA,'1','2','3')) OR ( EMPTY(ISONIAZIDA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_RIFAMPI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RIFAMPI,'1','2','3')) OR ( EMPTY(RIFAMPI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_ETAMBUTOL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ETAMBUTOL,'1','2','3')) OR ( EMPTY(ETAMBUTOL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_PIRAZINAMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIRAZINAMI,'1','2','3')) OR ( EMPTY(PIRAZINAMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_QUINOLAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUINOLAS,'1','2','3')) OR ( EMPTY(QUINOLAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_INYECTABLE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INYECTABLE,'1','2','3')) OR ( EMPTY(INYECTABLE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_89_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_MES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(MES)>=1  AND  VAL(MES) <=12 AND ISNUMERIC(MES)) AND ( .NOT. EMPTY(MES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(AÑO)>=2011 AND ISNUMERIC(AÑO)) AND ( .NOT. EMPTY(AÑO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_NUM_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_CON))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_N_COLECIST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_COLECIST>=0  AND N_COLECIST <=99 AND ISNUMERIC(N_COLECIST)) OR (N_COLECIST=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_D_COLECIST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_COLECIST)>=0  AND  VAL(D_COLECIST) <=9999 AND ISNUMERIC(D_COLECIST)) OR ( EMPTY(D_COLECIST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_N_HERNIORR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_HERNIORR>=0  AND N_HERNIORR <=99 AND ISNUMERIC(N_HERNIORR)) OR (N_HERNIORR=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_D_HERNIORR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_HERNIORR)>=0  AND  VAL(D_HERNIORR) <=9999 AND ISNUMERIC(D_HERNIORR)) OR ( EMPTY(D_HERNIORR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_N_REVASCUL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_REVASCUL>=0  AND N_REVASCUL <=99 AND ISNUMERIC(N_REVASCUL)) OR (N_REVASCUL=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_D_REVASCUL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_REVASCUL)>=0  AND  VAL(D_REVASCUL) <=9999 AND ISNUMERIC(D_REVASCUL)) OR ( EMPTY(D_REVASCUL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_N_CESAREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_CESAREA>=0  AND N_CESAREA <=99 AND ISNUMERIC(N_CESAREA)) OR (N_CESAREA=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_D_CESAREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_CESAREA)>=0  AND  VAL(D_CESAREA) <=9999 AND ISNUMERIC(D_CESAREA)) OR ( EMPTY(D_CESAREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_N_POSTPART
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((N_POSTPART>=0  AND N_POSTPART <=99 AND ISNUMERIC(N_POSTPART)) OR (N_POSTPART=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_D_POSTPART
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(D_POSTPART)>=0  AND  VAL(D_POSTPART) <=9999 AND ISNUMERIC(D_POSTPART)) OR ( EMPTY(D_POSTPART)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_90_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_COD_MUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_MUN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_AGUA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGUA,'1','2')) AND ( .NOT. EMPTY(AGUA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ALIMENTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ALIMENTOS,'1','2')) AND ( .NOT. EMPTY(ALIMENTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_PERS_PERS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PERS_PERS,'1','2')) AND ( .NOT. EMPTY(PERS_PERS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CONT_AMBIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONT_AMBIE,'1','2')) AND ( .NOT. EMPTY(CONT_AMBIE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_OTRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRO,'1','2')) AND ( .NOT. EMPTY(OTRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_DESCONOCID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DESCONOCID,'1','2')) AND ( .NOT. EMPTY(DESCONOCID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ESTADO_BRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADO_BRO,'1','2','3')) AND ( .NOT. EMPTY(ESTADO_BRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ORAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ORAL,'1','2')) AND ( .NOT. EMPTY(ORAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ORAL_FECAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ORAL_FECAL,'1','2')) AND ( .NOT. EMPTY(ORAL_FECAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CRUZADA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CRUZADA,'1','2')) AND ( .NOT. EMPTY(CRUZADA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G1_ENFERMO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G1_ENFERMO)>=0 AND ISNUMERIC(G1_ENFERMO)) OR ( EMPTY(G1_ENFERMO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G1_EXPUEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G1_EXPUEST)>=0 AND ISNUMERIC(G1_EXPUEST)) OR ( EMPTY(G1_EXPUEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G1_HOMBRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G1_HOMBRES)>=0 AND ISNUMERIC(G1_HOMBRES)) OR ( EMPTY(G1_HOMBRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G1_MUJERES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G1_MUJERES)>=0 AND ISNUMERIC(G1_MUJERES)) OR ( EMPTY(G1_MUJERES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G1_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G1_VIVOS)>=0 AND ISNUMERIC(G1_VIVOS)) OR ( EMPTY(G1_VIVOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G1_MUERTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G1_MUERTOS)>=0 AND ISNUMERIC(G1_MUERTOS)) OR ( EMPTY(G1_MUERTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G2_ENFERMO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G2_ENFERMO)>=0 AND ISNUMERIC(G2_ENFERMO)) OR ( EMPTY(G2_ENFERMO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G2_EXPUEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G2_EXPUEST)>=0 AND ISNUMERIC(G2_EXPUEST)) OR ( EMPTY(G2_EXPUEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G2_HOMBRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G2_HOMBRES)>=0 AND ISNUMERIC(G2_HOMBRES)) OR ( EMPTY(G2_HOMBRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G2_MUJERES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G2_MUJERES)>=0 AND ISNUMERIC(G2_MUJERES)) OR ( EMPTY(G2_MUJERES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G2_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G2_VIVOS)>=0 AND ISNUMERIC(G2_VIVOS)) OR ( EMPTY(G2_VIVOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G2_MUERTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G2_MUERTOS)>=0 AND ISNUMERIC(G2_MUERTOS)) OR ( EMPTY(G2_MUERTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G3_ENFERMO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G3_ENFERMO)>=0 AND ISNUMERIC(G3_ENFERMO)) OR ( EMPTY(G3_ENFERMO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G3_EXPUEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G3_EXPUEST)>=0 AND ISNUMERIC(G3_EXPUEST)) OR ( EMPTY(G3_EXPUEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G3_HOMBRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G3_HOMBRES)>=0 AND ISNUMERIC(G3_HOMBRES)) OR ( EMPTY(G3_HOMBRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G3_MUJERES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G3_MUJERES)>=0 AND ISNUMERIC(G3_MUJERES)) OR ( EMPTY(G3_MUJERES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G3_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G3_VIVOS)>=0 AND ISNUMERIC(G3_VIVOS)) OR ( EMPTY(G3_VIVOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G3_MUERTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G3_MUERTOS)>=0 AND ISNUMERIC(G3_MUERTOS)) OR ( EMPTY(G3_MUERTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G4_ENFERMO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G4_ENFERMO)>=0 AND ISNUMERIC(G4_ENFERMO)) OR ( EMPTY(G4_ENFERMO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G4_EXPUEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G4_EXPUEST)>=0 AND ISNUMERIC(G4_EXPUEST)) OR ( EMPTY(G4_EXPUEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G4_HOMBRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G4_HOMBRES)>=0 AND ISNUMERIC(G4_HOMBRES)) OR ( EMPTY(G4_HOMBRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G4_MUJERES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G4_MUJERES)>=0 AND ISNUMERIC(G4_MUJERES)) OR ( EMPTY(G4_MUJERES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G4_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G4_VIVOS)>=0 AND ISNUMERIC(G4_VIVOS)) OR ( EMPTY(G4_VIVOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G4_MUERTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G4_MUERTOS)>=0 AND ISNUMERIC(G4_MUERTOS)) OR ( EMPTY(G4_MUERTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G5_ENFERMO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G5_ENFERMO)>=0 AND ISNUMERIC(G5_ENFERMO)) OR ( EMPTY(G5_ENFERMO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G5_EXPUEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G5_EXPUEST)>=0 AND ISNUMERIC(G5_EXPUEST)) OR ( EMPTY(G5_EXPUEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G5_HOMBRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G5_HOMBRES)>=0 AND ISNUMERIC(G5_HOMBRES)) OR ( EMPTY(G5_HOMBRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G5_MUJERES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G5_MUJERES)>=0 AND ISNUMERIC(G5_MUJERES)) OR ( EMPTY(G5_MUJERES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G5_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G5_VIVOS)>=0 AND ISNUMERIC(G5_VIVOS)) OR ( EMPTY(G5_VIVOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G5_MUERTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G5_MUERTOS)>=0 AND ISNUMERIC(G5_MUERTOS)) OR ( EMPTY(G5_MUERTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G6_ENFERMO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G6_ENFERMO)>=0 AND ISNUMERIC(G6_ENFERMO)) OR ( EMPTY(G6_ENFERMO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G6_EXPUEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G6_EXPUEST)>=0 AND ISNUMERIC(G6_EXPUEST)) OR ( EMPTY(G6_EXPUEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G6_HOMBRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G6_HOMBRES)>=0 AND ISNUMERIC(G6_HOMBRES)) OR ( EMPTY(G6_HOMBRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G6_MUJERES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G6_MUJERES)>=0 AND ISNUMERIC(G6_MUJERES)) OR ( EMPTY(G6_MUJERES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G6_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G6_VIVOS)>=0 AND ISNUMERIC(G6_VIVOS)) OR ( EMPTY(G6_VIVOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G6_MUERTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G6_MUERTOS)>=0 AND ISNUMERIC(G6_MUERTOS)) OR ( EMPTY(G6_MUERTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G7_ENFERMO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G7_ENFERMO)>=0 AND ISNUMERIC(G7_ENFERMO)) OR ( EMPTY(G7_ENFERMO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G7_EXPUEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G7_EXPUEST)>=0 AND ISNUMERIC(G7_EXPUEST)) OR ( EMPTY(G7_EXPUEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G7_HOMBRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G7_HOMBRES)>=0 AND ISNUMERIC(G7_HOMBRES)) OR ( EMPTY(G7_HOMBRES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G7_MUJERES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G7_MUJERES)>=0 AND ISNUMERIC(G7_MUJERES)) OR ( EMPTY(G7_MUJERES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G7_VIVOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G7_VIVOS)>=0 AND ISNUMERIC(G7_VIVOS)) OR ( EMPTY(G7_VIVOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_G7_MUERTOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(G7_MUERTOS)>=0 AND ISNUMERIC(G7_MUERTOS)) OR ( EMPTY(G7_MUERTOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ENF_ALIM1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ENF_ALIM1)>=0 AND ISNUMERIC(ENF_ALIM1)) OR ( EMPTY(ENF_ALIM1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_EXP_ALIM1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EXP_ALIM1)>=0 AND ISNUMERIC(EXP_ALIM1)) OR ( EMPTY(EXP_ALIM1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ENF_ALIM2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ENF_ALIM2)>=0 AND ISNUMERIC(ENF_ALIM2)) OR ( EMPTY(ENF_ALIM2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_EXP_ALIM2
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EXP_ALIM2)>=0 AND ISNUMERIC(EXP_ALIM2)) OR ( EMPTY(EXP_ALIM2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ENF_ALIM3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ENF_ALIM3)>=0 AND ISNUMERIC(ENF_ALIM3)) OR ( EMPTY(ENF_ALIM3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_EXP_ALIM3
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EXP_ALIM3)>=0 AND ISNUMERIC(EXP_ALIM3)) OR ( EMPTY(EXP_ALIM3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ENF_ALIM4
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ENF_ALIM4)>=0 AND ISNUMERIC(ENF_ALIM4)) OR ( EMPTY(ENF_ALIM4)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_EXP_ALIM4
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EXP_ALIM4)>=0 AND ISNUMERIC(EXP_ALIM4)) OR ( EMPTY(EXP_ALIM4)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ENF_ALIM5
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ENF_ALIM5)>=0 AND ISNUMERIC(ENF_ALIM5)) OR ( EMPTY(ENF_ALIM5)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_EXP_ALIM5
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EXP_ALIM5)>=0 AND ISNUMERIC(EXP_ALIM5)) OR ( EMPTY(EXP_ALIM5)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ENF_ALIM6
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ENF_ALIM6)>=0 AND ISNUMERIC(ENF_ALIM6)) OR ( EMPTY(ENF_ALIM6)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_EXP_ALIM6
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EXP_ALIM6)>=0 AND ISNUMERIC(EXP_ALIM6)) OR ( EMPTY(EXP_ALIM6)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ENF_ALIM7
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ENF_ALIM7)>=0 AND ISNUMERIC(ENF_ALIM7)) OR ( EMPTY(ENF_ALIM7)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_EXP_ALIM7
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EXP_ALIM7)>=0 AND ISNUMERIC(EXP_ALIM7)) OR ( EMPTY(EXP_ALIM7)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ENF_ALIM8
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ENF_ALIM8)>=0 AND ISNUMERIC(ENF_ALIM8)) OR ( EMPTY(ENF_ALIM8)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_EXP_ALIM8
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EXP_ALIM8)>=0 AND ISNUMERIC(EXP_ALIM8)) OR ( EMPTY(EXP_ALIM8)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ENF_ALIM9
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ENF_ALIM9)>=0 AND ISNUMERIC(ENF_ALIM9)) OR ( EMPTY(ENF_ALIM9)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_EXP_ALIM9
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EXP_ALIM9)>=0 AND ISNUMERIC(EXP_ALIM9)) OR ( EMPTY(EXP_ALIM9)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_ENF_ALIM10
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(ENF_ALIM10)>=0 AND ISNUMERIC(ENF_ALIM10)) OR ( EMPTY(ENF_ALIM10)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_EXP_ALIM10
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(EXP_ALIM10)>=0 AND ISNUMERIC(EXP_ALIM10)) OR ( EMPTY(EXP_ALIM10)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_NINGUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_NINGUN)>=0 AND ISNUMERIC(CAS_NINGUN)) OR ( EMPTY(CAS_NINGUN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_NAUSEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_NAUSEA)>=0 AND ISNUMERIC(CAS_NAUSEA)) OR ( EMPTY(CAS_NAUSEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_VOMITO)>=0 AND ISNUMERIC(CAS_VOMITO)) OR ( EMPTY(CAS_VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_DIARRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_DIARRE)>=0 AND ISNUMERIC(CAS_DIARRE)) OR ( EMPTY(CAS_DIARRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_Fiebre
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_Fiebre)>=0 AND ISNUMERIC(CAS_Fiebre)) OR ( EMPTY(CAS_Fiebre)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_DOLABD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_DOLABD)>=0 AND ISNUMERIC(CAS_DOLABD)) OR ( EMPTY(CAS_DOLABD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_CEFALE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_CEFALE)>=0 AND ISNUMERIC(CAS_CEFALE)) OR ( EMPTY(CAS_CEFALE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_DESHID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_DESHID)>=0 AND ISNUMERIC(CAS_DESHID)) OR ( EMPTY(CAS_DESHID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_CIANOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_CIANOS)>=0 AND ISNUMERIC(CAS_CIANOS)) OR ( EMPTY(CAS_CIANOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_MIALGI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_MIALGI)>=0 AND ISNUMERIC(CAS_MIALGI)) OR ( EMPTY(CAS_MIALGI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_ALTRAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_ALTRAL)>=0 AND ISNUMERIC(CAS_ALTRAL)) OR ( EMPTY(CAS_ALTRAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_Mareo
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_Mareo)>=0 AND ISNUMERIC(CAS_Mareo)) OR ( EMPTY(CAS_Mareo)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_ESTRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_ESTRE)>=0 AND ISNUMERIC(CAS_ESTRE)) OR ( EMPTY(CAS_ESTRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_ESCALO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_ESCALO)>=0 AND ISNUMERIC(CAS_ESCALO)) OR ( EMPTY(CAS_ESCALO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_PAREST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_PAREST)>=0 AND ISNUMERIC(CAS_PAREST)) OR ( EMPTY(CAS_PAREST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_ICTERI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_ICTERI)>=0 AND ISNUMERIC(CAS_ICTERI)) OR ( EMPTY(CAS_ICTERI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_Acolia
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_Acolia)>=0 AND ISNUMERIC(CAS_Acolia)) OR ( EMPTY(CAS_Acolia)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_COLURI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_COLURI)>=0 AND ISNUMERIC(CAS_COLURI)) OR ( EMPTY(CAS_COLURI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_LESMAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_LESMAC)>=0 AND ISNUMERIC(CAS_LESMAC)) OR ( EMPTY(CAS_LESMAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_ANOREX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_ANOREX)>=0 AND ISNUMERIC(CAS_ANOREX)) OR ( EMPTY(CAS_ANOREX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_BRADIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_BRADIC)>=0 AND ISNUMERIC(CAS_BRADIC)) OR ( EMPTY(CAS_BRADIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_SIALOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_SIALOR)>=0 AND ISNUMERIC(CAS_SIALOR)) OR ( EMPTY(CAS_SIALOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_Miosis
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_Miosis)>=0 AND ISNUMERIC(CAS_Miosis)) OR ( EMPTY(CAS_Miosis)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_CAS_OTROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(CAS_OTROS)>=0 AND ISNUMERIC(CAS_OTROS)) OR ( EMPTY(CAS_OTROS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_LUGAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LUGAR,'1','2','3','4','5','6','8','9','10','11')) AND ( .NOT. EMPTY(LUGAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_TOTAL_ENF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TOTAL_ENF>=2 AND ISNUMERIC(TOTAL_ENF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_TOTAL_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TOTAL_EXP>=2 AND ISNUMERIC(TOTAL_EXP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_TOTAL_HOM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TOTAL_HOM>=0 AND ISNUMERIC(TOTAL_HOM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_TOTAL_MUJ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TOTAL_MUJ>=0 AND ISNUMERIC(TOTAL_MUJ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_TOTAL_VIVO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TOTAL_VIVO>=0 AND ISNUMERIC(TOTAL_VIVO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_TOTAL_MUER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TOTAL_MUER>=0 AND ISNUMERIC(TOTAL_MUER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_91_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_FEC_INGRES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_INGRES))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_CODMUN1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CODMUN1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_CODDEP1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CODDEP1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_CODPAIS1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CODPAIS1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_FEC_DESP1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_DESP1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_VOMITO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VOMITO,'1','2')) AND ( .NOT. EMPTY(VOMITO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_SANGRADOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SANGRADOS,'1','2')) AND ( .NOT. EMPTY(SANGRADOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_EDEMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EDEMA,'1','2')) AND ( .NOT. EMPTY(EDEMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_DOLOR_ABDO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR_ABDO,'1','2')) AND ( .NOT. EMPTY(DOLOR_ABDO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_ASTE_ADINI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASTE_ADINI,'1','2')) AND ( .NOT. EMPTY(ASTE_ADINI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_ERUPCION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ERUPCION,'1','2')) AND ( .NOT. EMPTY(ERUPCION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_DOLOR_MUS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOLOR_MUS,'1','2')) AND ( .NOT. EMPTY(DOLOR_MUS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_ANOREXIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANOREXIA,'1','2')) AND ( .NOT. EMPTY(ANOREXIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) AND ( .NOT. EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_CONJUNTIVI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONJUNTIVI,'1','2')) AND ( .NOT. EMPTY(CONJUNTIVI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_OTROS_SIGN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS_SIGN,'1','2')) AND ( .NOT. EMPTY(OTROS_SIGN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_NOM_CONTA1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_CONTA1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_FEC_CONTA1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_CONTA1))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_FEC_ENTREV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_ENTREV))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_HORA_ENTRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(HORA_ENTRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_CARGO_ENTR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CARGO_ENTR))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_92_NOM_FAMILI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_FAMILI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_PRE_CAR_V1
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRE_CAR_V1,'1','2')) AND ( .NOT. EMPTY(PRE_CAR_V1)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_VAC_SP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_SP,'1','2','3')) AND ( .NOT. EMPTY(VAC_SP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_VAC_EI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_EI,'1','2','3')) AND ( .NOT. EMPTY(VAC_EI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_ASMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASMA,'1','2')) AND ( .NOT. EMPTY(ASMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_EPOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EPOC,'1','2')) AND ( .NOT. EMPTY(EPOC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_DIABETES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIABETES,'1','2')) AND ( .NOT. EMPTY(DIABETES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_VIH
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIH,'1','2')) AND ( .NOT. EMPTY(VIH)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_ENF_CARD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ENF_CARD,'1','2')) AND ( .NOT. EMPTY(ENF_CARD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_CANCER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CANCER,'1','2')) AND ( .NOT. EMPTY(CANCER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_MALNUTRI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MALNUTRI,'1','2')) AND ( .NOT. EMPTY(MALNUTRI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_OBESIDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OBESIDAD,'1','2')) AND ( .NOT. EMPTY(OBESIDAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_INS_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INS_RENAL,'1','2')) AND ( .NOT. EMPTY(INS_RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_OTR_MEDINM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTR_MEDINM,'1','2')) AND ( .NOT. EMPTY(OTR_MEDINM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_FUMADOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FUMADOR,'1','2')) AND ( .NOT. EMPTY(FUMADOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_OTROS_DC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS_DC,'1','2')) AND ( .NOT. EMPTY(OTROS_DC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_TOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOS,'1','2')) AND ( .NOT. EMPTY(TOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_DOL_GAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DOL_GAR,'1','2')) AND ( .NOT. EMPTY(DOL_GAR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_RINORREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RINORREA,'1','2')) AND ( .NOT. EMPTY(RINORREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_CONJUNTIVI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONJUNTIVI,'1','2')) AND ( .NOT. EMPTY(CONJUNTIVI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_DIF_RES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIF_RES,'1','2')) AND ( .NOT. EMPTY(DIF_RES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_DIARREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIARREA,'1','2')) AND ( .NOT. EMPTY(DIARREA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_RX_TORAX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RX_TORAX,'1','2','3')) AND ( .NOT. EMPTY(RX_TORAX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_HALLAZ_RAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HALLAZ_RAD,'1','2','3')) OR ( EMPTY(HALLAZ_RAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_USO_ANTIB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(USO_ANTIB,'1','2')) AND ( .NOT. EMPTY(USO_ANTIB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_USO_ANTIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(USO_ANTIV,'1','2')) AND ( .NOT. EMPTY(USO_ANTIV)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_SERV_HOSP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SERV_HOSP,'1','3')) OR ( EMPTY(SERV_HOSP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_DER_PLE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DER_PLE,'1','2')) AND ( .NOT. EMPTY(DER_PLE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_DER_PER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DER_PER,'1','2')) AND ( .NOT. EMPTY(DER_PER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_MIOCARDITI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIOCARDITI,'1','2')) AND ( .NOT. EMPTY(MIOCARDITI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_SEPTICEMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SEPTICEMIA,'1','2')) AND ( .NOT. EMPTY(SEPTICEMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_FALLA_RESP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FALLA_RESP,'1','2')) AND ( .NOT. EMPTY(FALLA_RESP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_OTROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTROS,'1','2')) AND ( .NOT. EMPTY(OTROS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_DX_INI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DX_INI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_93_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_CARGO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CARGO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_COD_MUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_MUN))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_DILIGENCIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DILIGENCIA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_FEC_AJU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_AJU))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_NUM_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_CON))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Eventos_94_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_CONTROL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CONTROL))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_PRI_NOM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PRI_NOM))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_PRI_APE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PRI_APE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_MUESTRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUESTRA,'0','1','2','3','4','6','7','8','10','11','12','13','14','15','16','17','18','19','20','21','22','31','32','33','34')) AND ( .NOT. EMPTY(MUESTRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_PRUEBA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PRUEBA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_AGENTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AGENTE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_RESULTADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULTADO,'1','2','3','4','5','6','7','9','10','11','12','13','14','15','16','17','18','19')) AND ( .NOT. EMPTY(RESULTADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_TIP_REG_SA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_REG_SA,'C','S','P','E','N')) OR ( EMPTY(TIP_REG_SA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'3','5','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_PRI_NOM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PRI_NOM)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','PRI_NOM'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(PRI_NOM)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_PRI_APE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PRI_APE)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','PRI_APE'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(PRI_APE)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE))) OR COD_EVE='000'
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE)) OR COD_EVE='000'
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_EDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EDAD)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','EDAD'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(EDAD)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_UNI_MED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(UNI_MED)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','UNI_MED'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(UNI_MED,'0','1','2','3','4','5')) AND ( .NOT. EMPTY(UNI_MED))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_SEXO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SEXO)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','SEXO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SEXO,'M','F','I')) AND ( .NOT. EMPTY(SEXO))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_COD_MUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(COD_MUN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','COD_MUN'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(COD_MUN)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_AREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AREA)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','AREA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AREA,'1','2','3')) AND ( .NOT. EMPTY(AREA))) OR INLIST(COD_EVE,'357','351','352') OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_DIR_RES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(DIR_RES)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','DIR_RES'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(DIR_RES)) OR INLIST(COD_EVE,'357','351','352') OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_OCUPACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(OCUPACION)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','OCUPACION'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(OCUPACION)) OR INLIST(COD_EVE,'357','351','352') OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_TIP_SS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(TIP_SS)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','TIP_SS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(TIP_SS,'C','S','P','E','N','I')) AND ( .NOT. EMPTY(TIP_SS))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_PER_ETN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PER_ETN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','PER_ETN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(PER_ETN,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(PER_ETN))) OR INLIST(COD_EVE,'357','351','352') OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_DISCAPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_DISCAPA)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_DISCAPA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_DISCAPA,'1','2')) OR ( EMPTY(GP_DISCAPA))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_DESPLAZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_DESPLAZ)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_DESPLAZ'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_DESPLAZ,'1','2')) OR ( EMPTY(GP_DESPLAZ))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_MIGRANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_MIGRANT)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_MIGRANT'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_MIGRANT,'1','2')) OR ( EMPTY(GP_MIGRANT))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_CARCELA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_CARCELA)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_CARCELA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_CARCELA,'1','2')) OR ( EMPTY(GP_CARCELA))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_GESTAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_GESTAN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_GESTAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_GESTAN,'1','2')) OR ( EMPTY(GP_GESTAN))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_INDIGEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_INDIGEN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_INDIGEN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_INDIGEN,'1','2')) OR ( EMPTY(GP_INDIGEN))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_POBICFB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_POBICFB)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_POBICFB'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_POBICFB,'1','2')) OR ( EMPTY(GP_POBICFB))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_MAD_COM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_MAD_COM)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_MAD_COM'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_MAD_COM,'1','2')) OR ( EMPTY(GP_MAD_COM))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_DESMOVI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_DESMOVI)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_DESMOVI'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_DESMOVI,'1','2')) OR ( EMPTY(GP_DESMOVI))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_PSIQUIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_PSIQUIA)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_PSIQUIA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_PSIQUIA,'1','2')) OR ( EMPTY(GP_PSIQUIA))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_VIC_VIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_VIC_VIO)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_VIC_VIO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_VIC_VIO,'1','2')) OR ( EMPTY(GP_VIC_VIO))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_OTROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_OTROS)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_OTROS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_OTROS,'1','2')) OR ( EMPTY(GP_OTROS))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_MUN_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(MUN_PRO)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','MUN_PRO'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(MUN_PRO)) OR INLIST(COD_EVE,'357','351','352') OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_TIP_CAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(TIP_CAS)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','TIP_CAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(TIP_CAS,'1','2','3','4','5')) AND ( .NOT. EMPTY(TIP_CAS))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_PAC_HOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PAC_HOS)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','PAC_HOS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(PAC_HOS,'1','2')) OR ( EMPTY(PAC_HOS))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_CON_FIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CON_FIN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','CON_FIN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CON_FIN,'0','1','2')) AND ( .NOT. EMPTY(CON_FIN))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AJUSTE)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','AJUSTE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_TELEFONO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(TELEFONO)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','TELEFONO'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(TELEFONO)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_NOM_DIL_FI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_DIL_FI)) OR COD_EVE='000'
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_TEL_DIL_FI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(TEL_DIL_FI)) OR COD_EVE='000'
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_FEC_AJU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_AJU)) OR COD_EVE='000'
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ESTADOTRAN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','ESTADOTRAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ESTADOTRAN,'3','5','2')) AND ( .NOT. EMPTY(ESTADOTRAN))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_FM_FUERZA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FM_FUERZA)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','FM_FUERZA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FM_FUERZA,'1','2','3','4','5','6','7','8')) OR ( EMPTY(FM_FUERZA))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_EST_INGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EST_INGR)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','EST_INGR'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EST_INGR,0,2,3)) OR (EST_INGR=0)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_FUENTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FUENTE)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','FUENTE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FUENTE,'1','2','3','4','5')) OR ( EMPTY(FUENTE))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_ESTRATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ESTRATO)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','ESTRATO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ESTRATO,'1','2','3','4','5','6')) OR ( EMPTY(ESTRATO))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_SEM_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SEM_GES)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','SEM_GES'))) THEN
			RETURN .T.
		ELSE
			RETURN (( VAL(SEM_GES)>=1  AND  VAL(SEM_GES) <=45 AND ISNUMERIC(SEM_GES)) OR ( EMPTY(SEM_GES))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_LAT_DIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LAT_DIR)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','LAT_DIR'))) THEN
			RETURN .T.
		ELSE
			RETURN ((LAT_DIR>=-90  AND LAT_DIR <= 90 AND ISNUMERIC(LAT_DIR)) OR (LAT_DIR=0)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_LONG_DIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LONG_DIR)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','LONG_DIR'))) THEN
			RETURN .T.
		ELSE
			RETURN ((LONG_DIR>=-180  AND LONG_DIR <= 180 AND ISNUMERIC(LONG_DIR)) OR (LONG_DIR=0)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_CONFGEODIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CONFGEODIR)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','CONFGEODIR'))) THEN
			RETURN .T.
		ELSE
			RETURN ((CONFGEODIR>=0  AND CONFGEODIR <= 100 AND ISNUMERIC(CONFGEODIR)) OR (CONFGEODIR=0)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_ID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ID) OR NOTIF_IAAS!='1')
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_ID_PERSONA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ID_PERSONA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_SINTOMATIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SINTOMATIC,'1','2')) AND ( .NOT. EMPTY(SINTOMATIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_HOSPITALIZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HOSPITALIZ,'1','2')) OR ( EMPTY(HOSPITALIZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_FEC_SEGUMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_SEGUMI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_HORA_SEGUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(HORA_SEGUI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_TOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOS,'1','2')) OR ( EMPTY(TOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) OR ( EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_DIF_RES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIF_RES,'1','2')) OR ( EMPTY(DIF_RES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_ODINOFAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ODINOFAGIA,'1','2')) OR ( EMPTY(ODINOFAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_ADINAMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADINAMIA,'1','2')) OR ( EMPTY(ADINAMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','6','7','D')) AND ( .NOT. EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_FEC_AJU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_AJU))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_EST_INGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_INGR,0,2,3)) OR (EST_INGR=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Seguimient_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'3','5','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_FECINI_GRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FECINI_GRA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_ID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ID) OR NOTIF_IAAS!='1')
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_ID_PERSONA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(ID_PERSONA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_SINTOMATIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SINTOMATIC,'1','2')) AND ( .NOT. EMPTY(SINTOMATIC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_HOSPITALIZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HOSPITALIZ,'1','2')) AND ( .NOT. EMPTY(HOSPITALIZ)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_FEC_SEGUMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_SEGUMI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_HORA_SEGUI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(HORA_SEGUI))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_TOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TOS,'1','2')) OR ( EMPTY(TOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) OR ( EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_DIF_RES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIF_RES,'1','2')) OR ( EMPTY(DIF_RES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_ODINOFAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ODINOFAGIA,'1','2')) OR ( EMPTY(ODINOFAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_ADINAMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADINAMIA,'1','2')) OR ( EMPTY(ADINAMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','6','7','D')) AND ( .NOT. EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_FEC_AJU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_AJU))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_EST_INGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_INGR,0,2,3)) OR (EST_INGR=0))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_SeguimientoContactos_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'3','5','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_FEC_CAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_CAR))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_RAZ_SOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(RAZ_SOC))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_DIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(DIR)) OR INLIST(COD_EVE,'357','351','352')
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_REP_LEG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(REP_LEG))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_COR_ELE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COR_ELE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_RES_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(RES_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_TEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(TEL))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_FEC_CON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_CON))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_NAT_JUR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NAT_JUR,1,2,3,4)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_NIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NIV,1,2,3)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_UNI_ANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UNI_ANA,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_COV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COV,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_TAL_HUM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TAL_HUM,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_TEC_DIS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TEC_DIS,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_COM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_FAX_MOD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FAX_MOD,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_TIE_COR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIE_COR,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_INT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INT,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_TEL_FAX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TEL_FAX,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_RAD_TEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RAD_TEL,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_ACT_SIV
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACT_SIV,1,2)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_NIT_UPGD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NIT_UPGD))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_ES_UNI_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ES_UNI_NOT,'1','2')) AND ( .NOT. EMPTY(ES_UNI_NOT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_ESTADOUPGD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOUPGD,'1','2')) AND ( .NOT. EMPTY(ESTADOUPGD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_FEC_INICAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_INICAR))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_HOSP_UNIVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HOSP_UNIVE,'1','2')) OR ( EMPTY(HOSP_UNIVE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_REG_EXCEPC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(REG_EXCEPC,'1','2')) OR ( EMPTY(REG_EXCEPC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_TOT_CAMAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(TOT_CAMAS)>=1  AND  VAL(TOT_CAMAS) <= 99999 AND ISNUMERIC(TOT_CAMAS)) OR ( EMPTY(TOT_CAMAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_COMITE_INF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMITE_INF,'1','2')) OR ( EMPTY(COMITE_INF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_BI_PROFESI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(BI_PROFESI,'1','2')) OR ( EMPTY(BI_PROFESI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_IAAS_ULTIM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( VAL(IAAS_ULTIM)>=0  AND  VAL(IAAS_ULTIM) <=100 AND ISNUMERIC(IAAS_ULTIM)) OR ( EMPTY(IAAS_ULTIM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_INF_TENDEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INF_TENDEN,'1','2')) OR ( EMPTY(INF_TENDEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_SOCIAL_TEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SOCIAL_TEN,'1','2')) OR ( EMPTY(SOCIAL_TEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_LAB_MICROB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LAB_MICROB,'1','2')) OR ( EMPTY(LAB_MICROB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_LAB_PROPIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LAB_PROPIO,'1','2')) OR ( EMPTY(LAB_PROPIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_IDENT_GYE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(IDENT_GYE,'1','2')) OR ( EMPTY(IDENT_GYE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_PRUE_SUCEP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRUE_SUCEP,'1','2')) OR ( EMPTY(PRUE_SUCEP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_LAB_AUTOMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LAB_AUTOMA,'1','2')) OR ( EMPTY(LAB_AUTOMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_VITEK
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VITEK,'1','2')) OR ( EMPTY(VITEK)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_MICROSCAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MICROSCAN,'1','2')) OR ( EMPTY(MICROSCAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_PHOENIX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PHOENIX,'1','2')) OR ( EMPTY(PHOENIX)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_LAB_CCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LAB_CCI,'1','2')) OR ( EMPTY(LAB_CCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_LAB_CCE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LAB_CCE,'1','2')) OR ( EMPTY(LAB_CCE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_MICR_CDI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MICR_CDI,'1','2')) OR ( EMPTY(MICR_CDI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_WHONET
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(WHONET,'1','2')) OR ( EMPTY(WHONET)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_INFORM_PAT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(INFORM_PAT,'1','2')) OR ( EMPTY(INFORM_PAT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_LAB_CON_PE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LAB_CON_PE,'1','2')) OR ( EMPTY(LAB_CON_PE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_LAB_REM_CE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LAB_REM_CE,'1','2')) OR ( EMPTY(LAB_REM_CE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_LAB_REPORT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LAB_REPORT,'1','2')) OR ( EMPTY(LAB_REPORT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_QUIEN_VCAB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUIEN_VCAB,'1','2','3')) OR ( EMPTY(QUIEN_VCAB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_SER_CESARE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SER_CESARE,'1','2')) OR ( EMPTY(SER_CESARE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_SER_PAR_VA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SER_PAR_VA,'1','2')) OR ( EMPTY(SER_PAR_VA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_SER_COLECI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SER_COLECI,'1','2')) OR ( EMPTY(SER_COLECI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_SER_HERNIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SER_HERNIO,'1','2')) OR ( EMPTY(SER_HERNIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_SER_REVASC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SER_REVASC,'1','2')) OR ( EMPTY(SER_REVASC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_NOTIF_IAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NOTIF_IAD,'1','2')) AND ( .NOT. EMPTY(NOTIF_IAD)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_NOTIF_ISO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NOTIF_ISO,'1','2')) AND ( .NOT. EMPTY(NOTIF_ISO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_NOTIF_CAB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NOTIF_CAB,'1','2')) AND ( .NOT. EMPTY(NOTIF_CAB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'3','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_ucis_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_ucis_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_ucis_TIPO_UCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIPO_UCI,'A','P','N')) AND ( .NOT. EMPTY(TIPO_UCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_ucis_TOT_UCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ((TOT_UCI>=1  AND TOT_UCI <= 999 AND ISNUMERIC(TOT_UCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_ucis_SUB_TIPUCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUB_TIPUCI,'2','3')) AND ( .NOT. EMPTY(SUB_TIPUCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_upgd_ucis_ACTIVA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ACTIVA,'1','2')) AND ( .NOT. EMPTY(ACTIVA)))
	ENDIF
ENDFUNC


FUNCTION RecValidationRuleFor_brotes

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El total pacientes Hospitalarios + Ambulatorios debe ser igual al total por grupos de edad'
			bIsValid = VAL(BROTES.gru_1)+VAL(BROTES.gru_2)+VAL(BROTES.GRUPO_3) + VAL(BROTES.GRUPO_4) + VAL(BROTES.GRUPO_5) + VAL(BROTES.GRUPO_6) + 		VAL(BROTES.GRUPO_7) +	VAL(BROTES.GRUPO_8) + VAL(BROTES.GRUPO_9) + VAL(BROTES.GRUPO_10) + VAL(BROTES.GRUPO_11) + ;
						VAL(BROTES.GRUPO_12) + VAL(BROTES.GRUPO_13) + VAL(BROTES.GRUPO_14) + VAL(BROTES.GRUPO_15) + VAL(BROTES.GRUPO_16) + VAL(BROTES.GRUPO_17) +	VAL(BROTES.GRUPO_18) = VAL(BROTES.pte_hos) + VAL(BROTES.pte_amb)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es 350 (ETA), el total por grupo de edad debe ser mayor que uno (>1)'
			bIsValid = BROTES.COD_EVE != '350' OR VAL(BROTES.gru_1)+VAL(BROTES.gru_2)+VAL(BROTES.GRUPO_3) + VAL(BROTES.GRUPO_4) + VAL(BROTES.GRUPO_5) + VAL(BROTES.GRUPO_6) + 		VAL(BROTES.GRUPO_7) +	VAL(BROTES.GRUPO_8) + VAL(BROTES.GRUPO_9) + VAL(BROTES.GRUPO_10) + VAL(BROTES.GRUPO_11) + ;
						VAL(BROTES.GRUPO_12) + VAL(BROTES.GRUPO_13) + VAL(BROTES.GRUPO_14) + VAL(BROTES.GRUPO_15) + VAL(BROTES.GRUPO_16) + VAL(BROTES.GRUPO_17) +	VAL(BROTES.GRUPO_18)> 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El Total de Casos por Clasificaciones (Probables, Confirmados por Laboratorio, por Clínica y por Nexo) debe ser igual al total por grupos de edad'
			bIsValid = VAL(BROTES.cas_sos)+VAL(BROTES.cas_pro)+VAL(BROTES.cas_conl)+VAL(BROTES.cas_conc)+VAL(BROTES.cas_conn) = VAL(BROTES.gru_1)+VAL(BROTES.gru_2)+VAL(BROTES.GRUPO_3) + VAL(BROTES.GRUPO_4) + VAL(BROTES.GRUPO_5) + VAL(BROTES.GRUPO_6) + 		VAL(BROTES.GRUPO_7) +	VAL(BROTES.GRUPO_8) + VAL(BROTES.GRUPO_9) + VAL(BROTES.GRUPO_10) + VAL(BROTES.GRUPO_11) + ;
						VAL(BROTES.GRUPO_12) + VAL(BROTES.GRUPO_13) + VAL(BROTES.GRUPO_14) + VAL(BROTES.GRUPO_15) + VAL(BROTES.GRUPO_16) + VAL(BROTES.GRUPO_17) +	VAL(BROTES.GRUPO_18)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El Total de Casos por Sexo, debe ser igual al total por grupos de edad'
			bIsValid = VAL(BROTES.hombres)+VAL(BROTES.mujeres) = VAL(BROTES.gru_1)+VAL(BROTES.gru_2)+VAL(BROTES.GRUPO_3) + VAL(BROTES.GRUPO_4) + VAL(BROTES.GRUPO_5) + VAL(BROTES.GRUPO_6) + 		VAL(BROTES.GRUPO_7) +	VAL(BROTES.GRUPO_8) + VAL(BROTES.GRUPO_9) + VAL(BROTES.GRUPO_10) + VAL(BROTES.GRUPO_11) + ;
						VAL(BROTES.GRUPO_12) + VAL(BROTES.GRUPO_13) + VAL(BROTES.GRUPO_14) + VAL(BROTES.GRUPO_15) + VAL(BROTES.GRUPO_16) + VAL(BROTES.GRUPO_17) +	VAL(BROTES.GRUPO_18)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El Total de Casos por Condición Final, debe ser igual al total por grupos de edad'
			bIsValid = VAL(BROTES.vivos)+VAL(BROTES.muertos) =VAL(BROTES.gru_1)+VAL(BROTES.gru_2)+VAL(BROTES.GRUPO_3) + VAL(BROTES.GRUPO_4) + VAL(BROTES.GRUPO_5) + VAL(BROTES.GRUPO_6) + 		VAL(BROTES.GRUPO_7) +	VAL(BROTES.GRUPO_8) + VAL(BROTES.GRUPO_9) + VAL(BROTES.GRUPO_10) + VAL(BROTES.GRUPO_11) + ;
						VAL(BROTES.GRUPO_12) + VAL(BROTES.GRUPO_13) + VAL(BROTES.GRUPO_14) + VAL(BROTES.GRUPO_15) + VAL(BROTES.GRUPO_16) + VAL(BROTES.GRUPO_17) +	VAL(BROTES.GRUPO_18)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El evento ETA Colectivo - 350 solo permite el ingreso de casos  por NEXO EPIDEMIOLOGICO  si se ha ingresado por lo menos 1 caso CONFIRMADO POR LABORATORIO'
			bIsValid = BROTES.COD_EVE!='350'  OR ((VAL(BROTES.CAS_CONN)<1 OR VAL(BROTES.CAS_CONL)>=1) )
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento ETA Colectivo - 350 al ingresar casos CONFIRMADOS POR CLINICA no se permite el ingreso de casos por OTRO TIPO DE CASO'
			bIsValid = BROTES.COD_EVE!='350' OR VAL(BROTES.CAS_CONC)<1 OR (VAL(BROTES.CAS_PRO)=0 AND VAL(BROTES.CAS_CONL)=0 AND VAL(BROTES.CAS_CONN)=0)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La semana epidemiológica NO debe ser superior a la determinada por la fecha actual de grabación'
			bIsValid = isValidEpidemiologicalWeek(BROTES.SEMANA,BROTES.AÑO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El municipio no es coherente con el código de UPGD'
			bIsValid = LEFT(BROTES.COD_PRE,2)!='11' OR LEFT(BROTES.COD_MUN,2)='11'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El municipio de notificación no es coherente con el código de UPGD'
			bIsValid = LEFT(BROTES.COD_PRE,2)='11' OR (LEFT(BROTES.COD_PRE,5)=BROTES.COD_MUN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Debe haber por lo menos uno de los grupos de edad con más de 0 casos presentados'
			bIsValid = VAL(GRU_1) + VAL(GRU_2) + VAL(GRUPO_3) + VAL(GRUPO_4) + VAL(GRUPO_5) + VAL(GRUPO_6) + VAL(GRUPO_7) + VAL(GRUPO_8) + VAL(GRUPO_9) + VAL(GRUPO_10) + VAL(GRUPO_11) + VAL(GRUPO_12) + VAL(GRUPO_13) + VAL(GRUPO_14) + VAL(GRUPO_15) + VAL(GRUPO_16) + VAL(GRUPO_17) + VAL(GRUPO_18) > 0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_Contactos

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Los datos registrados en tipo de documento de identidad, edad y unidad de medida no son consistentes'
			bIsValid = CONTACTOS.AJUSTE!='0' OR isValidIdentificationDoc(CONTACTOS.TIP_IDE, CONTACTOS.UNI_MED, CONTACTOS.EDAD)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de nacimiento, edad y unidad de medida deben ser consistentes.'
			bIsValid = IIF(CONTACTOS.AJUSTE = '0',isValidAge(CONTACTOS.FECHA_NTO, CONTACTOS.EDAD, CONTACTOS.UNI_MED),.T.)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha posible exposición debe ser igual o menor a la fecha de inicio de síntomas'
			bIsValid = EMPTY(CONTACTOS.FEC_INI_SI) OR (CONTACTOS.FEC_POS_EX <= CONTACTOS.FEC_INI_SI)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Los datos de identificación del contacto no pueden ser iguales a los del caso positivo'
			bIsValid = CONTACTOS.TIPIDE_POS != CONTACTOS.TIP_IDE OR CONTACTOS.NUMIDE_POS != CONTACTOS.NUM_IDE
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_01

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Se está notificando un evento de Fiebre Amarilla (310), por tanto el valor en el campo Fiebre debe ser SI (1)'
			bIsValid = EVENTOS_01.FIEBRE='1'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_02

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'la Fecha de exposición NO debe ser mayor que la fecha de notificación'
			bIsValid = EVENTOS_02.FEC_EXP <= getBasicDataValue('FEC_NOT', 'PACIENTE', 'AÑO + SEMANA + COD_EVE + TIP_IDE + NUM_IDE + COD_PRE + COD_SUB', EVENTOS_02.AÑO + EVENTOS_02.SEMANA+EVENTOS_02.COD_EVE+EVENTOS_02.TIP_IDE+EVENTOS_02.NUM_IDE+EVENTOS_02.COD_PRE+EVENTOS_02.COD_SUB )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_03

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si se selecciona Paucibasilar los valores permitidos para el número de lesiones son de 1 a 5'
			bIsValid = EVENTOS_03.CLA_CLINIC != '1' OR BETWEEN(VAL(EVENTOS_03.NUM_LESION),1,5)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = ' Si se selecciona Multibacilar, los valores permitidos para el número de lesiones son de 6 a 99'
			bIsValid = EVENTOS_03.CLA_CLINIC != '2' OR BETWEEN(VAL(EVENTOS_03.NUM_LESION),6,99)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_04

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Se debe marcar por lo menos  un si (1) en cualquiera de los miembros entre parálisis,  paresia y flacidez'
			bIsValid = AT('1',EVENTOS_04.Par_msd + EVENTOS_04.Para_msd + EVENTOS_04.Fla_msd + EVENTOS_04.Par_msi + EVENTOS_04.Para_msi + EVENTOS_04.Fla_msi + EVENTOS_04.Par_mid + EVENTOS_04.Para_mid + EVENTOS_04.Fla_mid + EVENTOS_04.Par_mii + EVENTOS_04.Para_mii + EVENTOS_04.Fla_mii) != 0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_05

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Fecha de la Última Dosis No puede ser superior a la fecha de agresión'
			bIsValid = EVENTOS_05.f_ult_dos  <= EVENTOS_05.fec_exp
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'En los datos de localización anatómica, no puede registrar a todos NO (2) debe haber por lo menos un SI (1)'
			bIsValid = AT('1', EVENTOS_05.ccc + EVENTOS_05.man_ded + EVENTOS_05.tronco + EVENTOS_05.mie_sup + EVENTOS_05.mie_inf + EVENTOS_05.PIES_DEDOS + EVENTOS_05.GENIT_EXT) != 0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_06

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La fecha de la última dosis de vacuna de sarampión no puede ser superior a la fecha de inicio de  erupción'
			bIsValid = EVENTOS_06.ult_sar <= EVENTOS_06.fini_eru
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de la última dosis de vacuna de Rubeola no puede ser superior a la fecha de inicio de  erupción'
			bIsValid = EVENTOS_06.ult_rub <= EVENTOS_06.fini_eru
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso es sospechoso para sindrome inflamatorio multisistemico, al menos debe tener un síntoma igual a 1'
			bIsValid = EVENTOS_06.SOSPE_MISC!='1' OR OCCURS('1',EVENTOS_06.VOMITO + EVENTOS_06.DIARREA + EVENTOS_06.EDEMA + EVENTOS_06.ALT_CONCIE)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso es sospechoso para sindrome inflamatorio multisistemico, la variabe "Vómito" es obligatoria'
			bIsValid = EVENTOS_06.SOSPE_MISC!='1' OR !EMPTY(EVENTOS_06.VOMITO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso es sospechoso para sindrome inflamatorio multisistemico, la variabe "Edema de mucosas" es obligatoria'
			bIsValid = EVENTOS_06.SOSPE_MISC!='1' OR !EMPTY(EVENTOS_06.EDEMA)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso es sospechoso para sindrome inflamatorio multisistemico, la variabe "Alteración del estado de conciencia" es obligatoria'
			bIsValid = EVENTOS_06.SOSPE_MISC!='1' OR !EMPTY(EVENTOS_06.ALT_CONCIE)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el caso es sospechoso para sindrome inflamatorio multisistemico, la variabe "Diarrea" es obligatoria'
			bIsValid = EVENTOS_06.SOSPE_MISC!='1' OR !EMPTY(EVENTOS_06.DIARREA)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_07

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si embarazo = 1, es obligatorio digitar el número de semanas de embarazo al diagnóstico (De 0 a 45)'
			bIsValid = EVENTOS_07.EMBARAZO!='1' OR (!EMPTY(EVENTOS_07.SEM_EMB) AND VAL(EVENTOS_07.SEM_EMB)>=0 AND VAL(EVENTOS_07.SEM_EMB)<=45)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Ante todo caso de transmisión materno infantil es obligatorio diligenciar el nombre de la madre y su documento de identificación.'
			bIsValid = EVENTOS_07.MEC_PRO_T !='4' OR (!EMPTY(EVENTOS_07.NOM_MADRE) AND !EMPTY(EVENTOS_07.TIP_IDE_MA) AND !EMPTY(EVENTOS_07.NUM_IDE_MA))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Cuando el estado clínico es 2 = Sida o 3 = Muerto, al menos una de las variables de enfermedades asociadas debe ser 1 = Sí.'
			bIsValid = EVENTOS_07.EST_CLI = '1' OR (EVENTOS_07.CAN_ESO='1' OR EVENTOS_07.CAN_VA='1' OR EVENTOS_07.TUB_PUL='1' OR EVENTOS_07.CAN_CER='1' OR EVENTOS_07.TUB_EXP='1' OR EVENTOS_07.COCCIDIODO='1' OR EVENTOS_07.CITOMEGALO='1' OR EVENTOS_07.REN_CIT='1' OR EVENTOS_07.ENCEFALOPA='1' OR EVENTOS_07.OTRAS_MICR='1' OR EVENTOS_07.HIS_EXT='1' OR EVENTOS_07.ISO_CRO='1' OR EVENTOS_07.ERP_ZOS='1' OR EVENTOS_07.HIS_DIS='1' OR EVENTOS_07.LIN_BUR='1' OR EVENTOS_07.NEU_PNE='1' OR EVENTOS_07.NEU_REC='1' OR EVENTOS_07.LIN_INM='1' OR EVENTOS_07.CRI_CRO='1' OR EVENTOS_07.CRI_EXT='1' OR EVENTOS_07.SAR_KAP='1' OR EVENTOS_07.SIN_EMA='1' OR EVENTOS_07.LEU_MUL='1' OR EVENTOS_07.SEP_REC='1' OR EVENTOS_07.TOX_CER='1' OR EVENTOS_07.HEP_B='1' OR EVENTOS_07.HEP_C='1' OR EVENTOS_07.MENINGITIS='1' )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_08

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si en Atención hospitalaria, Empleó Suero  = Si, En la variable tiempo transcurrido, El valor para horas debe estar entre 0 y 24'
			bIsValid = EVENTOS_08.EMP_SUE != '1' OR !EMPTY(EVENTOS_08.tie_adm) OR BETWEEN(VAL(SUBSTR(EVENTOS_08.tie_adm,1,2)),0,24)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en Atención hospitalaria, Empleó Suero  = Si,  En la variable tiempo transcurrido, El valor para minutos debe estar entre 0 y 59'
			bIsValid = EVENTOS_08.EMP_SUE != '1' OR !EMPTY(EVENTOS_08.tie_adm) OR BETWEEN(VAL(SUBSTR(EVENTOS_08.tie_adm,4,2)),0,59)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en Atención hospitalaria, Empleó Suero  = Si,  la variable tiempo transcurrido no puede ser 00:00'
			bIsValid = EVENTOS_08.EMP_SUE!='1' OR (VAL(STRTRAN(EVENTOS_08.tie_adm,':')) != 0 )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_09

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El número de muertos no puede ser menor al número de abortos'
			bIsValid = VAL(EVENTOS_09.muertos) >= VAL(EVENTOS_09.abortos)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad gestacional puede ser 20 o 21 semanas solo si el peso es superior o igual a 500 gr o, el peso puede ser inferior a 500 gr solo si la edad gestacional es superior o gual a 22 semanas'
			bIsValid = !INLIST(EVENTOS_09.MOM_OCU,'1','2') OR (VAL(EVENTOS_09.EDA_GES) >= 22 OR VAL(EVENTOS_09.PESO) >= 500)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el momento en que ocurrio la muerte corresponde a 3, 5, 6, 7, es obligatorio diligenciar "Edad neonatal en días"'
			bIsValid = !INLIST(EVENTOS_09.MOM_OCU,'3','5','6','7') OR !EMPTY(EVENTOS_09.EDAD_NEO)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_10

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si la variable bajo peso al nacer  es igual a 2, el peso debe ser superior o igual a 2500 gramos'
			bIsValid = EVENTOS_10.BPN!='2' OR VAL(EVENTOS_10.PESO)>=2500
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la variable bajo peso al nacer  es igual a 1, el peso debe ser inferior a 2500 gramos'
			bIsValid = EVENTOS_10.BPN!='1' OR VAL(EVENTOS_10.PESO)<2500
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_14

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una de las variables de datos clínicos debe registrarse en la opción 1=sí'
			bIsValid = OCCURS('1', EVENTOS_14.FIEBRE + EVENTOS_14.AMIGDALITI + EVENTOS_14.FARINGITIS + EVENTOS_14.LARINGITIS + EVENTOS_14.PRE_MEM) >= 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_15

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Las vacunas de Polio oral  y Antirotavirica, solo se pueden administrar vía oral y el sitio debe ser oral'
			bIsValid = !INLIST(EVENTOS_15.VACUNA4,'3','15') OR (EVENTOS_15.VIA4='1' AND EVENTOS_15.SITIO4='9')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Las vacunas de Polio oral  y Antirotavirica, solo se pueden administrar vía oral y el sitio debe ser oral'
			bIsValid = !INLIST(EVENTOS_15.VACUNA3,'3','15') OR (EVENTOS_15.VIA3='1' AND EVENTOS_15.SITIO3='9')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Las vacunas de Polio oral  y Antirotavirica, solo se pueden administrar vía oral y el sitio debe ser oral'
			bIsValid = !INLIST(EVENTOS_15.VACUNA2,'3','15') OR (EVENTOS_15.VIA2='1' AND EVENTOS_15.SITIO2='9')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'En los Datos Clínicos, debe haber por lo menos un 1=Sí'
			bIsValid = AT('1',EVENTOS_15.BECEGEITIS+EVENTOS_15.ABSCESO+EVENTOS_15.LINFADENIT+EVENTOS_15.FIEBRE+EVENTOS_15.CON_FEB+EVENTOS_15.CON_SINF+EVENTOS_15.HIPOTONÍA+EVENTOS_15.PARESTESIA+EVENTOS_15.PARÁLISIS+EVENTOS_15.ENCEFALOPA+EVENTOS_15.MENINGITIS+EVENTOS_15.URTICARIA+EVENTOS_15.ECZEMA+ EVENTOS_15.CHO_ANA+EVENTOS_15.GUI_BAR+EVENTOS_15.CELULITIS+EVENTOS_15.LLA_PER+EVENTOS_15.FATIGA+EVENTOS_15.DOLOR_CABE+EVENTOS_15.MIALGIA+EVENTOS_15.ARTRALGIA+EVENTOS_15.NAUSEAS) != 0
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Las vacunas de Polio oral  y Antirotavirica, solo se pueden administrar vía oral y el sitio debe ser oral'
			bIsValid = !INLIST(EVENTOS_15.VACUNA,'3','15') OR (EVENTOS_15.VIA='1' AND EVENTOS_15.SITIO='9')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_20

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La fecha de inicio de investigación no puede ser anterior a la fecha de inicio de síntomas'
			bIsValid = EMPTY(EVENTOS_20.fec_ii) OR (EVENTOS_20.fec_ii >= fieldValue('INI_SIN', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_20.AÑO + EVENTOS_20.SEMANA+EVENTOS_20.COD_EVE+EVENTOS_20.TIP_IDE+EVENTOS_20.NUM_IDE+EVENTOS_20.COD_PRE+EVENTOS_20.COD_SUB  + "'", "AJUSTE DESC"))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

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

FUNCTION RecValidationRuleFor_eventos_23

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El número de expuestos NO DEBE ser menor al número de enfermos'
			bIsValid = EVENTOS_23.EXPUESTOS >= EVENTOS_23.ENFERMOS
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El período de incubación más largo NO puede ser MENOR que el período de incubación más corto'
			bIsValid = BETWEEN(compareTimePeriod(EVENTOS_23.Per_in_cor,EVENTOS_23.Per_in_lar),-1,0)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El número de expuestos DEBE ser mayor que 1'
			bIsValid = EVENTOS_23.EXPUESTOS > 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

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

FUNCTION RecValidationRuleFor_eventos_25

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La fecha de informe no puede se menor a la fecha de inicio de síntomas'
			bIsValid = EVENTOS_25.fec_inf >= fieldValue('INI_SIN', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_25.AÑO + EVENTOS_25.SEMANA+EVENTOS_25.COD_EVE+EVENTOS_25.TIP_IDE+EVENTOS_25.NUM_IDE+EVENTOS_25.COD_PRE+EVENTOS_25.COD_SUB  + "'", "AJUSTE DESC")
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_27

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el caso es probable o "Confirmado por clínica" y es agudo, exige el diligenciamiento de micrométodo, gota gruesa, microhematocrito y Strout'
			bIsValid = fieldValue('TIP_CAS', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_27.AÑO + EVENTOS_27.SEMANA+EVENTOS_27.COD_EVE+EVENTOS_27.TIP_IDE+EVENTOS_27.NUM_IDE+EVENTOS_27.COD_PRE+EVENTOS_27.COD_SUB + "'", "AJUSTE DESC" ) = '5' OR (EVENTOS_27.CLAS_CASO!='1' OR (!EMPTY(EVENTOS_27.MICROMETOD) AND !EMPTY(EVENTOS_27.GOTA_GRUE) AND !EMPTY(EVENTOS_27.MICRO_HEM) AND !EMPTY(EVENTOS_27.STROUT)))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El tipo de caso "Confirmado por nexo" solo aplica para casos agudos'
			bIsValid = fieldValue('TIP_CAS', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_27.AÑO + EVENTOS_27.SEMANA+EVENTOS_27.COD_EVE+EVENTOS_27.TIP_IDE+EVENTOS_27.NUM_IDE+EVENTOS_27.COD_PRE+EVENTOS_27.COD_SUB + "'", "AJUSTE DESC" )!='5' OR EVENTOS_27.CLAS_CASO='1'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_32

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La suma de pérdidas de vacuna antirrábica animal por diversas causas debe ser igual al total de pérdidas.'
			bIsValid = VAL(PER_VENC)+	VAL(PER_EXP_CA)+	VAL(PER_CONG)+	VAL(PER_RUPT)+	VAL(PER_HURTO)+	VAL(PER_POL)+	VAL(PER_ERR)+	VAL(PER_REAC)=val(Vac_per)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La suma de pérdidas de vacuna antirrábica humana por diversas causas debe ser igual al total de pérdidas.'
			bIsValid = VAL(PER_H_VEN)+	VAL(PER_H_EXP)+	VAL(PER_H_CON)+	VAL(PER_H_RUP)+	VAL(PER_H_HUR)+	VAL(PER_H_POL)+	VAL(PER_H_ERR)+	VAL(PER_H_REA)=VAL(Vah_per)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La suma de pérdidas de suero antirrábico humano por diversas causas debe ser igual al total de pérdidas.'
			bIsValid = VAL(PER_S_VEN)+	VAL(PER_S_EXP)+	VAL(PER_S_CON)+	VAL(PER_S_RUP)+	VAL(PER_S_HUR)+	VAL(PER_S_POL)+	VAL(PER_S_ERR)+	VAL(PER_S_REA)=VAL(Fsa_per)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La suma de resultado final observaciones de perros debe ser igual a la suma de perros observados.'
			bIsValid = VAL(PER_OB_DOC)+VAL(PER_OB_CLI)+VAL(PER_OB_CEN)=VAL(PER_CON_RA)+VAL(PER_SIN_RA)+VAL(PER_MUERTO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La suma de resultado final observaciones de gatos debe ser igual a la suma de gatos observados.'
			bIsValid = VAL(GAT_CON_RA)+VAL(GAT_SIN_RA)+VAL(GAT_MUERTO)=VAL(GAT_OB_DOC)+VAL(GAT_OB_CLI)+VAL(GAT_OB_CEN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La suma de dosis pos exposición en tratamientos concluidos, suspendidos y en curso debe ser igual al total de dosis pos exposición.'
			bIsValid = VAL(VAC_CONCLU)+	VAL(VAC_SUSP)+	VAL(VAC_CURSO)=VAL(VAC_AP_POS)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_35

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el tipo de lesión es única, no puede haber más de una localización anatómica comprometida'
			bIsValid = EVENTOS_35.TIP_LES!='1' OR OCCURS('1',EVENTOS_35.CCC + EVENTOS_35.MAN_DED + EVENTOS_35.TRONCO + EVENTOS_35.MIE_SUP + EVENTOS_35.MIE_INF + EVENTOS_35.PIES_DEDOS + EVENTOS_35.GENIT_EXT) = 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_37

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si clasificación del caso es 3 = Confirmado por laboratorio,  se deben relacionar las fechas de las Pruebas: Cuadro Hemático,  Frotis de Sangre Periférica y Estudio de Médula Ósea'
			bIsValid = fieldValue('TIP_CAS', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_37.AÑO + EVENTOS_37.SEMANA+EVENTOS_37.COD_EVE+EVENTOS_37.TIP_IDE+EVENTOS_37.NUM_IDE+EVENTOS_37.COD_PRE+EVENTOS_37.COD_SUB + "'", "AJUSTE DESC" ) != '3' OR (!EMPTY(EVENTOS_37.CH_FEC_TOM) AND !EMPTY(EVENTOS_37.CH_FEC_REC) AND !EMPTY(EVENTOS_37.CH_FEC_RES) AND !EMPTY(EVENTOS_37.FS_FEC_TOM) AND !EMPTY(EVENTOS_37.FS_FEC_REC) AND !EMPTY(EVENTOS_37.FS_FEC_RES) AND !EMPTY(EVENTOS_37.EM_FEC_TOM) AND !EMPTY(EVENTOS_37.EM_FEC_REC) AND !EMPTY(EVENTOS_37.EM_FEC_RES))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_38

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si ha sido valorado por ninguno es igual a 1, las restantes valoraciones deben ser 2; Si ha sido valorado por ninguno es 2, almenos una de las restantes valoraciones debe ser 1'
			bIsValid = EVENTOS_38.VAL_NINGUN!='2' OR (EVENTOS_38.val_med_gr='1' OR EVENTOS_38.val_pediat='1' OR EVENTOS_38.val_endocr='1' OR EVENTOS_38.val_neurol='1' OR EVENTOS_38.val_geneti='1')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_39

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'En datos clínicos debe haber por lo menos uno'
			bIsValid = AT('1', EVENTOS_39.DOL_CUE + EVENTOS_39.DOL_GAR + EVENTOS_39.IMP_HABLAR + EVENTOS_39.DISFAGIA + EVENTOS_39.CONVULSION + EVENTOS_39.CON_MUSCUL + EVENTOS_39.RIG_MU_ABD + EVENTOS_39.ESP_GENERA + EVENTOS_39.RIG_NUCA + EVENTOS_39.AFE_NER_CR + EVENTOS_39.TRISMUS + EVENTOS_39.OPISTÓTONO +  EVENTOS_39.FIEBRE) != 0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_40

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El paciente debe tener fiebre y 2 o más datos clínicos en 1=Sí'
			bIsValid = (EVENTOS_40.FIEBRE='1' AND (OCCURS('1',EVENTOS_40.ESCALOFRIO + EVENTOS_40.CEFALEA + EVENTOS_40.DOLOR_RETR + EVENTOS_40.VOMITO + EVENTOS_40.DIARREA + EVENTOS_40.DABDOMINAL + EVENTOS_40.ICTERICIA + EVENTOS_40.MIALGIAS + EVENTOS_40.ARTRALGIAS + EVENTOS_40.HEPATOMEGA + EVENTOS_40.ESPLENOMEG + EVENTOS_40.ADENOPATIA + EVENTOS_40.DISNEA + EVENTOS_40.DESHIDRATA + EVENTOS_40.SIG_MENING + EVENTOS_40.EXANTEMA + EVENTOS_40.EDEMA + EVENTOS_40.CONVULSION + EVENTOS_40.HEMORRAGIA + EVENTOS_40.DOLOR_PANT + EVENTOS_40.SUDOR_NOCT + EVENTOS_40.EPIDIDIMIT + EVENTOS_40.ORQUITIS + EVENTOS_40.OLIGURIA) >= 2))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el paciente tiene más de 30 días de fiebre, solo puede ingresar si tiene orquitis, epididimitis, esplenomegalia o hepatomegalia (alguno de ellos) y, además, debe tener "Consumo de leche y sus derivados sin pasteurizar" o "Manipulación de secreciones…'
			bIsValid = DIAS_FIEBR<=30 OR (EVENTOS_40.EPIDIDIMIT = '1' OR EVENTOS_40.ORQUITIS = '1' OR EVENTOS_40.HEPATOMEGA = '1' OR EVENTOS_40.ESPLENOMEG = '1') AND (EVENTOS_40.CONS_LECHE = '1' OR EVENTOS_40.MANIP_SECR = '1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en la variable animales en casa se seleccionó Sí, por lo menos uno de los animales perros, gatos, etc. debe ser  1=Sí'
			bIsValid = (EVENTOS_40.ANIM_EN_CA!='1' OR (OCCURS('1',EVENTOS_40.PERROS + EVENTOS_40.GATOS + EVENTOS_40.BOVINOS + EVENTOS_40.EQUINOS + EVENTOS_40.CERDOS + EVENTOS_40.SILVESTRES + EVENTOS_40.OTR_ANIMAL) >= 1))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_41

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La diferencia entre las variables  Semanas de gestación y edad gestacional no puede ser superior a 4'
			bIsValid = ABS(EVENTOS_41.SEM_GES - EVENTOS_41.EDAD_GES) <= 4
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El Número de embarazos previos no puede ser inferior a la suma de Numero de pérdidas (abortos) y Numero de cesáreas'
			bIsValid = EVENTOS_41.NO_EMBARAZ >= (EVENTOS_41.NO_ABORTOS + EVENTOS_41.NO_CESAREA)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_42

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La clasificación final de dengue sin signos de alarma solo podrá tenerse cuando se hayan marcado del grupo de hallazgos semiológicos fiebre y dos o más adicionales'
			bIsValid = EVENTOS_42.CLASFINAL != '1' OR (EVENTOS_42.FIEBRE='1' AND (OCCURS('1', EVENTOS_42.CEFALEA + EVENTOS_42.DOLRRETROO + EVENTOS_42.MALGIAS + EVENTOS_42.ARTRALGIA + EVENTOS_42.ERUPCIONR) >= 2))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La conducta  0 (No aplica), solo es válida para el evento Mortalidad por dengue (Cod. 580)'
			bIsValid = EVENTOS_42.CONDUCTA != '0' OR EVENTOS_42.COD_EVE='580'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La clasificación final de dengue con signos de alarma solo podrá tenerse cuando se hayan marcado fiebre y dos o más de los sintomas para dengue sin signos de alarma y por lo menos uno de los de dengue con signos de alarma'
			bIsValid = EVENTOS_42.CLASFINAL != '2' OR (EVENTOS_42.FIEBRE='1' AND (OCCURS('1', EVENTOS_42.CEFALEA + EVENTOS_42.DOLRRETROO + EVENTOS_42.MALGIAS + EVENTOS_42.ARTRALGIA + EVENTOS_42.ERUPCIONR) >= 2) AND OCCURS('1', EVENTOS_42.DOLOR_ABDO + EVENTOS_42.VOMITO + EVENTOS_42.DIARREA + EVENTOS_42.SOMNOLENCI + EVENTOS_42.HIPOTENSIO + EVENTOS_42.HEPATOMEG  + EVENTOS_42.HEM_MUCOSA + EVENTOS_42.HIPOTERMIA + EVENTOS_42.AUM_HEMATO +  EVENTOS_42.CAIDA_PLAQ + EVENTOS_42.ACUM_LIQUI) >= 1)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La clasificación final de dengue grave solo podrá tenerse cuando se hayan marcado fiebre  y por lo menos uno de los síntomas clasificados para dengue grave'
			bIsValid = EVENTOS_42.CLASFINAL != '3' OR (EVENTOS_42.FIEBRE='1' AND (OCCURS('1', EVENTOS_42.EXTRAVASAC + EVENTOS_42.HEMORR_HEM + EVENTOS_42.DAÑO_ORGAN + EVENTOS_42.CHOQUE) >= 1))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento muerte por dengue Cod. 580, debe seleccionarse en hallazgos semiológicos fiebre y al menos uno de los síntomas de dengue grave'
			bIsValid = EVENTOS_42.COD_EVE!='580' OR (EVENTOS_42.FIEBRE='1' AND OCCURS('1',EVENTOS_42.EXTRAVASAC + EVENTOS_42.HEMORR_HEM + EVENTOS_42.CHOQUE + EVENTOS_42.DAÑO_ORGAN) >= 1)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La clasificación final  0 (No aplica), solo es válida para el evento Mortalidad por dengue (Cod. 580)'
			bIsValid = EVENTOS_42.CLASFINAL != '0' OR EVENTOS_42.COD_EVE='580'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Cuando el evento es 210 y tiene al menos un síntoma de la sección con signos de alarma entonces la clasificación final no puede ser sin signos de alarma'
			bIsValid = EVENTOS_42.COD_EVE!='210' OR (OCCURS('1',EVENTOS_42.DOLOR_ABDO + EVENTOS_42.VOMITO + EVENTOS_42.DIARREA + EVENTOS_42.SOMNOLENCI + EVENTOS_42.HIPOTENSIO + EVENTOS_42.HEPATOMEG  + EVENTOS_42.HEM_MUCOSA + EVENTOS_42.HIPOTERMIA + EVENTOS_42.AUM_HEMATO +  EVENTOS_42.CAIDA_PLAQ + EVENTOS_42.ACUM_LIQUI) < 1 OR (EVENTOS_42.CLASFINAL='2' OR EVENTOS_42.CLASFINAL='3'))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_43

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Al menos uno de los grupos de edad correspondientes a Todas las causas de Hospitalización, Todas las causas de Hosp. en UCI, Todas las causas de muerte y Total  consulta externa y urgencias, debe ser mayor que cero'
			bIsValid = HOSPITAL_1+HOSPITAL_2+HOSPITAL_3+HOSPITAL_4+HOSPITAL_5+HOSPITAL_6+HOSPITAL_7+ TOTUCI_1+TOTUCI_2+TOTUCI_3+TOTUCI_4+TOTUCI_5+TOTUCI_6+TOTUCI_7+ MTETOT_1+MTETOT_2+MTETOT_3+MTETOT_4+MTETOT_5+MTETOT_6+MTETOT_7+ TOTCEXT_1 + TOTCEXT_2  + TOTCEXT_3  +  TOTCEXT_4  + TOTCEXT_5  + TOTCEXT_6  + TOTCEXT_7 > 0
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'En todo grupo de edad los denominadores no pueden ser inferiores a los numeradores'
			bIsValid = EVENTOS_43.HOSPITAL_1 >= EVENTOS_43.GRUPOH_1 AND EVENTOS_43.HOSPITAL_2 >= EVENTOS_43.GRUPOH_2 AND EVENTOS_43.HOSPITAL_3 >= EVENTOS_43.GRUPOH_3 AND EVENTOS_43.HOSPITAL_4 >= EVENTOS_43.GRUPOH_4 AND EVENTOS_43.HOSPITAL_5 >= EVENTOS_43.GRUPOH_5 AND EVENTOS_43.HOSPITAL_6 >= EVENTOS_43.GRUPOH_6 AND EVENTOS_43.HOSPITAL_7 >= EVENTOS_43.GRUPOH_7 AND EVENTOS_43.TOTUCI_1 >= EVENTOS_43.IRAUCI_1 AND EVENTOS_43.TOTUCI_2 >= EVENTOS_43.IRAUCI_2 AND EVENTOS_43.TOTUCI_3 >= EVENTOS_43.IRAUCI_3 AND EVENTOS_43.TOTUCI_4 >= EVENTOS_43.IRAUCI_4 AND EVENTOS_43.TOTUCI_5 >= EVENTOS_43.IRAUCI_5 AND EVENTOS_43.TOTUCI_6 >= EVENTOS_43.IRAUCI_6 AND EVENTOS_43.TOTUCI_7 >= EVENTOS_43.IRAUCI_7 AND EVENTOS_43.MTETOT_1 >= EVENTOS_43.MTEIRA_1 AND EVENTOS_43.MTETOT_2 >= EVENTOS_43.MTEIRA_2 AND EVENTOS_43.MTETOT_3 >= EVENTOS_43.MTEIRA_3 AND EVENTOS_43.MTETOT_4 >= EVENTOS_43.MTEIRA_4 AND EVENTOS_43.MTETOT_5 >= EVENTOS_43.MTEIRA_5 AND EVENTOS_43.MTETOT_6 >= EVENTOS_43.MTEIRA_6 AND EVENTOS_43.MTETOT_7 >= EVENTOS_43.MTEIRA_7 AND EVENTOS_43.TOTCEXT_1 >= EVENTOS_43.IRACEXT_1 AND EVENTOS_43.TOTCEXT_2 >= EVENTOS_43.IRACEXT_2 AND EVENTOS_43.TOTCEXT_3 >= EVENTOS_43.IRACEXT_3 AND EVENTOS_43.TOTCEXT_4 >= EVENTOS_43.IRACEXT_4 AND EVENTOS_43.TOTCEXT_5 >= EVENTOS_43.IRACEXT_5 AND EVENTOS_43.TOTCEXT_6 >= EVENTOS_43.IRACEXT_6 AND EVENTOS_43.TOTCEXT_7 >= EVENTOS_43.IRACEXT_7
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El municipio no es coherente con el código de UPGD'
			bIsValid = LEFT(EVENTOS_43.COD_PRE,2)!='11' OR LEFT(EVENTOS_43.COD_MUN,2)='11'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El municipio no es coherente con el código de UPGD'
			bIsValid = LEFT(EVENTOS_43.COD_PRE,2)='11' OR (LEFT(EVENTOS_43.COD_PRE,5)=EVENTOS_43.COD_MUN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La semana epidemiológica NO debe ser superior a la determinada por la fecha actual de grabación'
			bIsValid = isValidEpidemiologicalWeek(EVENTOS_43.SEMANA,EVENTOS_43.AÑO)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_44

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si Asintomático es 2,  por lo menos una de las manifestaciones bioclínicas  desde aborto hasta alteraciones oculares debe estar en 1=Sí'
			bIsValid = ASINTOMATI!='2' OR (ABORTO = '1' OR 	MORTINATO = '1' OR 	PREMATURID = '1' OR 	ALTERACION = '1' OR 	LESIONCUTA = '1' OR 	RINISEROSA = '1' OR 	HEPATOESPL = '1' OR 	HIDROPESIA = '1' OR 	LESIONOSEA = '1' OR 	ALTERENAL = '1' OR 	ALTEHEMATO = '1' OR 	ALTEFUNHEP = '1' OR 	SORDERA = '1' OR 	ALTEOCULAR = '1' )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_Eventos_45

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el paciente pertenece al grupo poblacional Gestantes, es obligatorio diligenciar el trimestre de gestación'
			bIsValid = fieldValue('GP_GESTAN', 'PACIENTE', "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB='" + EVENTOS_45.AÑO + EVENTOS_45.SEMANA+EVENTOS_45.COD_EVE+EVENTOS_45.TIP_IDE+EVENTOS_45.NUM_IDE+EVENTOS_45.COD_PRE+EVENTOS_45.COD_SUB + "'", "AJUSTE DESC" )!='1' OR !EMPTY(EVENTOS_45.TRIMESTRE)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_Eventos_46

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si hubo desplazamiento en los últimos cinco días, al menos se debe diligenciar una fecha de llegada'
			bIsValid = EVENTOS_46.DESP!='1' OR !EMPTY(EVENTOS_46.FEC_LLEG1) OR !EMPTY(EVENTOS_46.FEC_LLEG2) OR !EMPTY(EVENTOS_46.FEC_LLEG3) OR !EMPTY(EVENTOS_46.FEC_LLEG4)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_47

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Los datos registrados en tipo de documento de identidad, edad y unidad de medida no son consistentes'
			bIsValid = isValidIdentificationDoc(EVENTOS_47.TIP_DOC_RN, '3', ALLTRIM(STR(EVENTOS_47.EDAD_RN)))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_48

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si la  paciente fue remitida, la Institución de referencia es de obligatorio diligenciamiento'
			bIsValid = EVENTOS_48.PTE_REMTDA!='1' OR !EMPTY(EVENTOS_48.INST_REFE1)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_49

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una de las variables índice de Dean por diente debe ser mayor de 0'
			bIsValid = (VAL(DEAN_16)+VAL(DEAN_15)+VAL(DEAN_13)+VAL(DEAN_12)+VAL(DEAN_11)+VAL(DEAN_21)+VAL(DEAN_22)+VAL(DEAN_23)+VAL(DEAN_25)+VAL(DEAN_26)+VAL(DEAN_36)+VAL(DEAN_46))>0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_50

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si selecciona en la variable "Clasificación del caso según antecedente de tratamiento" la opción 2= previamente tratado, no aplica la opción 3 de la  variable "Clasificación del caso según tipo de medicamentos recibidos"'
			bIsValid = CLAS_ANT!='2' OR CLAS_MED!='3'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_51

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() AND !IsInmediateNotification()  THEN
		IF bIsValid THEN
			sRuleViolated = 'La variable "Recolección de evidencia médico legal" es de obligatorio diligenciamiento para las opciones 6, 7, 10, 14 de violencia sexual'
			bIsValid = !(',' + ALLTRIM(EVENTOS_51.NAT_VIOSEX) + ',' $  ',6,7,10,14,') OR (!EMPTY(EVENTOS_51.EVI_MLEGAL))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La variables "Recolección de evidencia médico legal" es de obligatorio diligenciamiento para las opciones 6, 7, 10, 14 de violencia sexual'
			bIsValid = !(',' + ALLTRIM(EVENTOS_51.NAT_VIOSEX) + ',' $  ',6,7,10,14,') OR (!EMPTY(EVENTOS_51.EVI_MLEGAL))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Almenos una de las variables de modalidad de la violencia debe estar diligenciada'
			bIsValid = !EMPTY(EVENTOS_51.NATURALEZA) OR !EMPTY(EVENTOS_51.NAT_VIOSEX)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Debe existir por lo menos una variable diligenciada que dé cuenta de la relación con el agresor, ya sea familiar o no familiar'
			bIsValid = !EMPTY(EVENTOS_51.R_FAM_VIC + EVENTOS_51.R_NOFILIAR)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Las variables de atención en salud son de obligatorio diligenciamiento para las opciones 6, 7, 10, 14, 15 de violencia sexual'
			bIsValid = !(',' + ALLTRIM(EVENTOS_51.NAT_VIOSEX) + ',' $  ',6,7,10,14,15,') OR (!EMPTY(EVENTOS_51.SP_ITS) AND !EMPTY(EVENTOS_51.PROF_HEP_B) AND !EMPTY(EVENTOS_51.PROF_OTRAS))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'SI el "Mecanismo utilizado para la agresión" es Quemadura, debe indicar el sitio de la lesión'
			bIsValid = !INLIST(EVENTOS_51.ARMAS,'12','13','14') OR OCCURS('1',EVENTOS_51.QUE_CARA+EVENTOS_51.QUE_CUELLO+EVENTOS_51.QUE_MANO+EVENTOS_51.QUE_PIE+EVENTOS_51.QUE_PLIEGU+EVENTOS_51.QUE_GENITA+EVENTOS_51.QUE_TRONCO+EVENTOS_51.QUE_MIESUP+EVENTOS_51.QUE_MIEINF)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si en los datos del agresor en el parentesco con la victima se ingreso la categoria 25=Ninguno, la variable "Agresor no familiar" debe estar diligenciada obligatoriamente'
			bIsValid = EVENTOS_51.R_FAM_VIC != '25' OR !EMPTY(EVENTOS_51.R_NOFILIAR)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_52

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si "La desnutrición fue la causa básica de muerte" es 2=No, es obligatorio diligenciar "La desnutrición fue una causa asociada o estado patológico de muerte"'
			bIsValid = EVENTOS_52.DES_CBMTE != '2' OR !EMPTY(EVENTOS_52.DES_CPAT)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

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

FUNCTION RecValidationRuleFor_eventos_56

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Por lo menos se debe ingresarse un tipo de defecto, ya sea funcional o malformación congénita'
			bIsValid = !EMPTY(EVENTOS_56.DEFMET_COD) OR !EMPTY(EVENTOS_56.DEFSE1_COD) OR !EMPTY(EVENTOS_56.DEFSE2_COD) OR !EMPTY(EVENTOS_56.MALFO1_COD) OR !EMPTY(EVENTOS_56.MALFO2_COD) OR !EMPTY(EVENTOS_56.MALFO3_COD) OR !EMPTY(EVENTOS_56.MALFO4_COD) OR !EMPTY(EVENTOS_56.MALFO5_COD)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_58

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si hay antecedentes de riesgo, alguno de ellos  debe quedar como 1=Sí'
			bIsValid = EVENTOS_58.NINGUNO!='2' OR (EVENTOS_58.HIP_CRO='1' OR EVENTOS_58.CARDIOPATÍ='1' OR EVENTOS_58.DIABETES='1' OR EVENTOS_58.MOLA_HIDA='1' OR EVENTOS_58.PRETERMINO='1' OR EVENTOS_58.BAJO_PESO='1' OR EVENTOS_58.MACROSOMIC='1' OR 	EVENTOS_58.TRANSTORNO='1' OR EVENTOS_58.OBESIDAD='1' OR EVENTOS_58.D_CRONICA='1' OR EVENTOS_58.INTER_GENE='1' OR EVENTOS_58.ITS_DIS='1' OR EVENTOS_58.VIH_SIDA='1' OR EVENTOS_58.OTRAS='1' OR EVENTOS_58.RH_NEGATIV='1' OR EVENTOS_58.TABAQUISMO='1' OR EVENTOS_58.ALCOHOLISM='1' OR EVENTOS_58.PSICOACTIV='1' OR EVENTOS_58.DEF_CONDIC='1' OR EVENTOS_58.SIFILIS='1' OR EVENTOS_58.HEPATITISB='1' OR EVENTOS_58.OTROS_FR='1' OR EVENTOS_58.GINGIVITIS='1' )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_60

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() AND !IsInmediateNotification()  THEN
		IF bIsValid THEN
			sRuleViolated = 'Al menos un tipo de lesión debe ser 1'
			bIsValid = OCCURS('1',EVENTOS_60.LACERACION+EVENTOS_60.CONTUSION+EVENTOS_60.QUEMADURA+EVENTOS_60.AMPUTACION+EVENTOS_60.FRACTURAS+EVENTOS_60.DAÑ_OCU+EVENTOS_60.DAÑ_AUD+EVENTOS_60.VIA_AER+EVENTOS_60.ABDOMEN+EVENTOS_60.OTRO)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si quemadura es 1, debe seleccionar el sitio de la lesión'
			bIsValid = EVENTOS_60.QUEMADURA!='1' OR OCCURS('1',EVENTOS_60.QUE_CARA+EVENTOS_60.QUE_CUELLO+EVENTOS_60.QUE_MANO+EVENTOS_60.QUE_PIE+EVENTOS_60.QUE_PLIEGU+EVENTOS_60.QUE_GENITA+EVENTOS_60.QUE_TRONCO+EVENTOS_60.QUE_MIESUP+EVENTOS_60.QUE_MIEINF)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si amputación es 1, debe seleccionar el sitio de la lesión'
			bIsValid = EVENTOS_60.AMPUTACION!='1' OR OCCURS('1',EVENTOS_60.AMP_DEDMAN+EVENTOS_60.AMP_MANO+EVENTOS_60.AMP_ANTEBR+EVENTOS_60.AMP_BRAZO+EVENTOS_60.AMP_MUSLO+EVENTOS_60.AMP_PIERNA+EVENTOS_60.AMP_PIE+EVENTOS_60.AMP_DEDPIE)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si fractura es 1, debe seleccionar el sitio de la lesión'
			bIsValid = EVENTOS_60.FRACTURAS!='1' OR OCCURS('1',EVENTOS_60.FRA_CRANEO+EVENTOS_60.FRA_HUEMAN+EVENTOS_60.FRA_MIESUP+EVENTOS_60.FRA_REJA+EVENTOS_60.FRA_COLUMN+EVENTOS_60.FRA_CADERA+EVENTOS_60.FRA_MIEINF+EVENTOS_60.FRA_HUEPIE)>=1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Debe diligenciar una de las siguientes:"Tipo de artefacto pirotécnico", "Actividad en que se presentó el evento - pólvora pirotécnica" o "Actividad en que se presentó el evento - artefactos explosivos, minas antipersonal y municiones sin explosionar"'
			bIsValid = !EMPTY(EVENTOS_60.ARTEFACTO + EVENTOS_60.ACT_POLVOR + EVENTOS_60.ACT_MINAS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Lesiones encontradas Otro" es 1, debe diligenciar cuál'
			bIsValid = EVENTOS_60.OTRO!='1' OR !EMPTY(EVENTOS_60.CUAL)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de artefacto pirotécnico" es 10, debe diligenciar cuál'
			bIsValid = EVENTOS_60.ARTEFACTO!='10' OR !EMPTY(EVENTOS_60.OTR_ART_PI)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Actividad en que se presentó el evento - pólvora pirotécnica" es 7, debe diligenciar cuál'
			bIsValid = EVENTOS_60.ACT_POLVOR!='7' OR !EMPTY(EVENTOS_60.OTR_AC_POL)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Actividad en que se presentó el evento - artefactos explosivos, minas antipersonal y municiones sin explosionar" es 5, debe diligenciar cuál'
			bIsValid = EVENTOS_60.ACT_MINAS!='5' OR !EMPTY(EVENTOS_60.OTR_AC_MIN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de artefacto que produjo la lesión" es 1, debe diligenciar "Hubo consumo de alcohol u otras sustancias psicoactivas previamente a la lesión por pólvora Lesionado"'
			bIsValid = EVENTOS_60.ARTEF_LESI!='1' OR !EMPTY(EVENTOS_60.CON_ALC_LE)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de artefacto que produjo la lesión" es 1, debe diligenciar "Actividad en que se presentó el evento - pólvora pirotécnica"'
			bIsValid = EVENTOS_60.ARTEF_LESI!='1' OR !EMPTY(EVENTOS_60.ACT_POLVOR)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_63

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Las variables segunda neoplasia y recaida son excluyentes'
			bIsValid = EVENTOS_63.CONSX2_NEO != '1' OR EVENTOS_63.RECAIDA = '2'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Las variables segunda neoplasia y recaida son excluyentes'
			bIsValid = EVENTOS_63.RECAIDA != '1' OR  EVENTOS_63.CONSX2_NEO = '2'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_64

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el tipo de lesión es única, no puede haber más de una localización anatómica comprometida'
			bIsValid = EVENTOS_64.TIP_LES!='1' OR OCCURS('1',EVENTOS_64.CCC + EVENTOS_64.MAN_DED + EVENTOS_64.TRONCO + EVENTOS_64.MIE_SUP + EVENTOS_64.MIE_INF + EVENTOS_64.PIES_DEDOS + EVENTOS_64.GENIT_EXT) = 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la clasificación de la notificación es 2, entonces la clasificación del caso en datos básicos solo puede ser 2 = Probable o 3 = Confirmado por Laboratorio'
			bIsValid = (CLAS_NOTIF!='2' OR (PACIENTE.TIP_CAS='2' OR PACIENTE.TIP_CAS='3'))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_65

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Las opciones 6 = QUININA INTRAVENOSA y 7 = ARTESUNATO, de la variable tratamiento, solo son admisibles cuando se seleccione la opción 1 en la variable complicación'
			bIsValid = !(EVENTOS_65.TRATAMIENT $ ' 6 7 ') OR EVENTOS_65.COMPLICACI='1'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la malaria presenta complicaciones, al menos una de las complicaciones específicas debe ser igual a 1'
			bIsValid = EVENTOS_65.COMPLICACI!='1' OR OCCURS('1',EVENTOS_65.COM_CEREBR+EVENTOS_65.COM_RENAL+EVENTOS_65.COM_HEPATI+EVENTOS_65.COM_PULMON+EVENTOS_65.COM_HEMATO+EVENTOS_65.COM_OTRAS)>0
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_66

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'En menores de 5 años, las opciones 1. Ocupacional, 3. Intencional suicida y 10. Automedicación/autoprescripción, no aplican para "tipo de exposición"'
			bIsValid = !(PACIENTE.UNI_MED!='1' OR (PACIENTE.UNI_MED='1' AND VAL(PACIENTE.EDAD)<5)) OR INLIST(EVENTOS_66.TIP_EXP,'2','4','6','8','9')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_75

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si Profilaxis antibiótica quirúrgica es 1, debe marcarse "Tiempo entre finalización de profilaxis e incisión quirúrgica o parto"'
			bIsValid = EVENTOS_75.PRF_ANTIB!='1' OR !EMPTY(EVENTOS_75.TIEM_ENTRE)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

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

FUNCTION RecValidationRuleFor_eventos_77

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Se debe elegir por lo menos un mecanismo'
			bIsValid = OCCURS('1', EVENTOS_77.AHORCAMIEN + EVENTOS_77.ARMA_CORTO + EVENTOS_77.ARMA_FUEGO + EVENTOS_77.INMOLACION + EVENTOS_77.LANZ_AGUA + EVENTOS_77.LANZ_VACIO + EVENTOS_77.LANZ_VEHIC + EVENTOS_77.INTOXICACI) >= 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la variable intoxicación es 1, debe diligenciar cual tipo de sustancia ocasionó la intoxicación'
			bIsValid = EVENTOS_77.INTOXICACI != '1' OR !EMPTY(EVENTOS_77.TIPO_SUSTA)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la variable  trastorno psiquiátricos es 1, es obligatorio que se presente por lo menos 1 de los trastornos psiquiátricos detallados'
			bIsValid = EVENTOS_77.ANTEC_TRAN != '1' OR OCCURS('1', EVENTOS_77.TRAN_DEPRE + EVENTOS_77.TRAST_PERS + EVENTOS_77.TRANS_PERS + EVENTOS_77.ESQUIZOFRE)>=1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_79

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Cuando se marque la opcion 1 = LEIAG en la variable "Resultado biopsia exocervix",  no puede marcar la opcion "infiltrante" de la variable "Grado histopatológico"'
			bIsValid = EVENTOS_79.RES_B_EXOC!='1' OR EVENTOS_79.GRADO_HIST!='2'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una de las variables de tratamiento debe registrarse en la opción 1=sí'
			bIsValid = EVENTOS_79.SEG_TRAT_I!='1' OR OCCURS('1', EVENTOS_79.RADIOTERAP + EVENTOS_79.QUIRURGICO + EVENTOS_79.QUIMIOTERA + EVENTOS_79.HORMONOTER + EVENTOS_79.CUID_PALIA + EVENTOS_79.INMUNOTERA) >= 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_80

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La suma de las variables Día Paciente UCI_N  no puede ser mayor al producto del número de camas por el número de días del mes notificado'
			bIsValid = VAL(EVENTOS_80.N_D_P1_S_N) + VAL(EVENTOS_80.N_D_P2_S_N) + VAL(EVENTOS_80.N_D_P3_S_N) + VAL(EVENTOS_80.N_D_P4_S_N) + VAL(EVENTOS_80.N_D_P5_S_N) <= (VAL(EVENTOS_80.CAM_UCIV_N) * daysOfMonth(VAL(EVENTOS_80.AÑO), VAL(EVENTOS_80.MES)))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_82

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una de las variables de datos clínicos debe registrarse en la opción 1=sí'
			bIsValid = OCCURS('1', EVENTOS_82.EDEMA + EVENTOS_82.DELGADEZ + EVENTOS_82.PIEL_RESE + EVENTOS_82.HIPERPIGM + EVENTOS_82.LES_CABEL + EVENTOS_82.PALIDEZ + EVENTOS_82.DIARREA + EVENTOS_82.DESHIDRATA + EVENTOS_82.FIEBRE + EVENTOS_82.TOS + EVENTOS_82.DIF_RESPIR) >= 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_83

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El Zscore talla/edad debe estar entre -6de y 6de inclusive'
			bIsValid = EVENTOS_83.ZSCORE_TE >= -8 AND EVENTOS_83.ZSCORE_TE <=6
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'No cumple la definición de caso: el Zscore peso/talla debe estar entre -6DE y -2DE'
			bIsValid = EVENTOS_83.EDEMA!='2' OR (EVENTOS_83.ZSCORE_PT > -6 AND EVENTOS_83.ZSCORE_PT < -2)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_84

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si "SEAN/SSSN" es 2=NO, debe diigenciar: Politrauma,Quemadura,Perforacion,Hemorragia,Craneo,Cara,Ojos,Nariz,Orejas,Boca_dient,Cuello,Brazo,Antebrazo,Mano,Dedos_mano,Torx_anter,Torx_post,Abdomen,Muslos,Piernas,Pies,Dedos_pies,Org_intern,Piel'
			bIsValid = EVENTOS_84.SEAN_SSSN!='2' OR (!EMPTY(EVENTOS_84.politrauma) AND !EMPTY(EVENTOS_84.QUEMADURAS) AND !EMPTY(EVENTOS_84.PERFORACIO) AND !EMPTY(EVENTOS_84.HEMORRAGIA) AND !EMPTY(EVENTOS_84.CRANEO) AND !EMPTY(EVENTOS_84.CARA) AND !EMPTY(EVENTOS_84.OJOS) AND !EMPTY(EVENTOS_84.NARIZ) AND !EMPTY(EVENTOS_84.OREJAS) AND !EMPTY(EVENTOS_84.BOCA_DIENT) AND !EMPTY(EVENTOS_84.CUELLO) AND !EMPTY(EVENTOS_84.BRAZO) AND !EMPTY(EVENTOS_84.ANTEBRAZO) AND !EMPTY(EVENTOS_84.MANO) AND !EMPTY(EVENTOS_84.DEDOS_MANO) AND !EMPTY(EVENTOS_84.TORX_ANTER) AND !EMPTY(EVENTOS_84.TORX_POST) AND !EMPTY(EVENTOS_84.ABDOMEN) AND !EMPTY(EVENTOS_84.MUSLOS) AND !EMPTY(EVENTOS_84.PIERNAS) AND !EMPTY(EVENTOS_84.PIES) AND !EMPTY(EVENTOS_84.DEDOS_PIES) AND !EMPTY(EVENTOS_84.ORG_INTERN) AND !EMPTY(EVENTOS_84.PIEL))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una variable del bloque mecanismo o agente que ocasionó la lesión debe estar marcada en la opción 1=sí'
			bIsValid = OCCURS('1', EVENTOS_84.MAQUINA + EVENTOS_84.MEDIOS_TRA + EVENTOS_84.PROD_QUIMI + EVENTOS_84.ANIMALES  + EVENTOS_84.JUGUETES + EVENTOS_84.ELECTR_ILU + EVENTOS_84.VEST_ACCES + EVENTOS_84.UTIL_ESCOL + EVENTOS_84.UTEN_COMED + EVENTOS_84.ACC_INFAN + EVENTOS_84.EQU_DEPORT + EVENTOS_84.ELECT_AUDI + EVENTOS_84.BELLEZA + EVENTOS_84.MEDICAMEN  + EVENTOS_84.APAR_ESTET + EVENTOS_84.EQU_BIOMED + EVENTOS_84.SEAN_SSSN  )  >= 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una variable del bloque sitio anatómico debe estar marcada en la opción 1=sí'
			bIsValid = EVENTOS_84.SEAN_SSSN = '1' OR OCCURS('1',EVENTOS_84.CRANEO + EVENTOS_84.CARA + EVENTOS_84.OJOS + EVENTOS_84.NARIZ + EVENTOS_84.OREJAS + EVENTOS_84.BOCA_DIENT + EVENTOS_84.CUELLO + EVENTOS_84.BRAZO + EVENTOS_84.ANTEBRAZO + EVENTOS_84.MANO + EVENTOS_84.DEDOS_MANO + EVENTOS_84.TORX_ANTER + EVENTOS_84.TORX_POST + EVENTOS_84.MAMAS + EVENTOS_84.ABDOMEN + EVENTOS_84.PELV_PERIN + EVENTOS_84.GENITALES + EVENTOS_84.MUSLOS + EVENTOS_84.PIERNAS + EVENTOS_84.PIES + EVENTOS_84.DEDOS_PIES + EVENTOS_84.ORG_INTERN + EVENTOS_84.PIEL +  + EVENTOS_84.GLUTEOS )  >= 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Por lo menos una variable del bloque lugar de ocurrencia debe estar marcada en la opción 1=sí'
			bIsValid = EVENTOS_84.SEAN_SSSN = '1' OR OCCURS('1', EVENTOS_84.HOGAR + EVENTOS_84.COLEGIO +  EVENTOS_84.CALLE + EVENTOS_84.PARQUE +   EVENTOS_84.INDUSTRIA + EVENTOS_84.ZONA_CULT + EVENTOS_84.CENTR_ESTE + EVENTOS_84.SPA + EVENTOS_84.IPS + EVENTOS_84.ESTABLECIM )  >= 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "SEAN/SSSN" es diferente de 1, por lo menos una de las variables del bloque tipo de lesión debe estar marcada en la opción 1=sí'
			bIsValid = EVENTOS_84.SEAN_SSSN='1' OR OCCURS('1',EVENTOS_84.ASFIXIA + EVENTOS_84.ESTRANGULA + EVENTOS_84.HERIDA + EVENTOS_84.TRAUMA + EVENTOS_84.CHOQ_ELECT + EVENTOS_84.FRACTURA  + EVENTOS_84.POLITRAUMA + EVENTOS_84.AMPUTACION + EVENTOS_84.QUEMADURAS + EVENTOS_84.INTOXICAC + EVENTOS_84.INFECCION + EVENTOS_84.SEPSIS + EVENTOS_84.PERFORACIO + EVENTOS_84.HEMORRAGIA + EVENTOS_84.NECROSIS + EVENTOS_84.EMBOLIA + EVENTOS_84.DEPRE_RESP)  >= 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "SEAN/SSSN" es 1=Sí, por lo menos una de las variables del bloque "Manifestaciones clínicas" debe estar marcada en la opción 1=sí'
			bIsValid = EVENTOS_84.SEAN_SSSN !='1' OR OCCURS('1',EVENTOS_84.TOS + EVENTOS_84.DISNEA + EVENTOS_84.DIF_RESPIR + EVENTOS_84.DOLOR_TORA + EVENTOS_84.NAUSEA + EVENTOS_84.VOMITO + EVENTOS_84.DIARREA + EVENTOS_84.DOLOR_ABDO + EVENTOS_84.OTRA_CLINI) >= 1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_85

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La fecha de última menstruación debe ser inferior a la fecha de terminación del embarazo.'
			bIsValid = EMPTY(EVENTOS_85.FEC_TER_EM) OR EVENTOS_85.FEC_ULT_ME < EVENTOS_85.FEC_TER_EM
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de terminación del embarazo debe ser posterior a la fecha de primera ecografía'
			bIsValid = EMPTY(EVENTOS_85.FEC_TER_EM) OR EVENTOS_85.FEC_1_ECO < EVENTOS_85.FEC_TER_EM
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de última menstruación debe ser inferior a la fecha de primera ecografía'
			bIsValid = EMPTY(EVENTOS_85.FEC_1_ECO) OR EVENTOS_85.FEC_ULT_ME < EVENTOS_85.FEC_1_ECO
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el paciente tiene complicaciones neurológicas, las preguntas Fecha de inicio de sintomas, Tipo de complicación neurológica y Desplazamiento en los últimos 30 días, son de obligatorio diligenciamiento'
			bIsValid = EVENTOS_85.COMPL_NEUR!='1' OR (!EMPTY(EVENTOS_85.FEC_INI_SI) AND !EMPTY(EVENTOS_85.TIPO_COMPL) AND  !EMPTY(EVENTOS_85.DESPLAZAMI))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_87

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Cuando el tipo de UCI es neonatal, es obligatorio diligenciar peso al nacer'
			bIsValid = EVENTOS_87.TIPO_UCI!='3' OR EVENTOS_87.VALOR_PESO!=0
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Cuando el tipo de UCI es neonatal, tipo de IAD solo puede ser 1=NAV ó 3= ITS AC'
			bIsValid = EVENTOS_87.TIPO_UCI != '3' OR (EVENTOS_87.TIPO_IAD = '1' OR EVENTOS_87.TIPO_IAD = '3')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de IAD" es NAV  debe diligenciarse ventilador mecánico'
			bIsValid = EVENTOS_87.TIPO_IAD!='1' OR (EVENTOS_87.VENTILADOR='1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de IAD" es ITS-AC debe diligenciarse catéter central'
			bIsValid = EVENTOS_87.TIPO_IAD!='3' OR (EVENTOS_87.CAT_CENTRA='1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Tipo de IAD" es ISTU-AC debe diligenciarse catéter urinario'
			bIsValid = EVENTOS_87.TIPO_IAD!='2' OR (EVENTOS_87.CAT_URINAR='1')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_88

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La variable días paciente excede el valor permitido de acuerdo al número de camas'
			bIsValid = VAL(EVENTOS_88.N_D_P1_S_N) + VAL(EVENTOS_88.N_D_P2_S_N) + VAL(EVENTOS_88.N_D_P3_S_N) + VAL(EVENTOS_88.N_D_P4_S_N) + VAL(EVENTOS_88.N_D_P5_S_N) <= (VAL(EVENTOS_88.CAM_UCI_N) * daysOfMonth(VAL(EVENTOS_88.AÑO), VAL(EVENTOS_88.MES)))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La variable días paciente excede el valor permitido de acuerdo al número de camas'
			bIsValid = VAL(EVENTOS_88.N_D_P1_SIN) + VAL(EVENTOS_88.N_D_P2_SIN) + VAL(EVENTOS_88.N_D_P3_SIN) + VAL(EVENTOS_88.N_D_P4_SIN) + VAL(EVENTOS_88.N_D_P5_SIN) <= (VAL(EVENTOS_88.CAM_UCI_IN) * daysOfMonth(VAL(EVENTOS_88.AÑO), VAL(EVENTOS_88.MES)))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

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

FUNCTION RecValidationRuleFor_eventos_90

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Mes y año de vigilancia deben ser iguales o inferiores al mes y año de la fecha de notificación.'
			bIsValid = isLessThanOrEqual(VAL(EVENTOS_90.MES),VAL(EVENTOS_90.AÑO),MONTH(EVENTOS_90.FEC_NOT),YEAR(EVENTOS_90.FEC_NOT))
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_eventos_91

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 2'
			bIsValid = VAL(EVENTOS_91.G2_ENFERMO)=VAL(EVENTOS_91.G2_VIVOS)+VAL(EVENTOS_91.G2_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 5'
			bIsValid = VAL(EVENTOS_91.G5_ENFERMO)=VAL(EVENTOS_91.G5_HOMBRES)+VAL(EVENTOS_91.G5_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 3'
			bIsValid = VAL(EVENTOS_91.G3_ENFERMO)=VAL(EVENTOS_91.G3_VIVOS)+VAL(EVENTOS_91.G3_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser mayor que uno (>1)'
			bIsValid = VAL(EVENTOS_91.G1_ENFERMO) + VAL(EVENTOS_91.G2_ENFERMO) + VAL(EVENTOS_91.G3_ENFERMO) + VAL(EVENTOS_91.G4_ENFERMO) + VAL(EVENTOS_91.G5_ENFERMO) + VAL(EVENTOS_91.G6_ENFERMO) + VAL(EVENTOS_91.G7_ENFERMO) > 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "Modo de trasmisión - Otro diferente de agua y alimentol" es 1=sí,  los modos de transmisión Agua y Alimentos deben ser 2=No'
			bIsValid = EVENTOS_91.OTRO != '1' OR (EVENTOS_91.ALIMENTOS = '2' AND EVENTOS_91.AGUA = '2')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 2'
			bIsValid = VAL(EVENTOS_91.G2_ENFERMO)=VAL(EVENTOS_91.G2_HOMBRES)+VAL(EVENTOS_91.G2_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 1'
			bIsValid = VAL(EVENTOS_91.G1_ENFERMO)=VAL(EVENTOS_91.G1_HOMBRES)+VAL(EVENTOS_91.G1_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 1'
			bIsValid = VAL(EVENTOS_91.G1_ENFERMO)=VAL(EVENTOS_91.G1_VIVOS)+VAL(EVENTOS_91.G1_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'SI "Modo de transmisión alimentos" es 1=sí, debe diligenciar Alimento 1'
			bIsValid = EVENTOS_91.ALIMENTOS != '1' OR !EMPTY(ALIMENTO1)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La semana epidemiológica NO debe ser superior a la determinada por la fecha actual de grabación'
			bIsValid = isValidEpidemiologicalWeek(EVENTOS_91.SEMANA,EVENTOS_91.AÑO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 7'
			bIsValid = VAL(EVENTOS_91.G7_ENFERMO)=VAL(EVENTOS_91.G7_VIVOS)+VAL(EVENTOS_91.G7_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 4'
			bIsValid = VAL(EVENTOS_91.G4_ENFERMO)=VAL(EVENTOS_91.G4_HOMBRES)+VAL(EVENTOS_91.G4_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de vivos y muertos debe ser mayor que uno (>1)'
			bIsValid = VAL(EVENTOS_91.G1_VIVOS) + VAL(EVENTOS_91.G2_VIVOS) + VAL(EVENTOS_91.G3_VIVOS) + VAL(EVENTOS_91.G4_VIVOS) + VAL(EVENTOS_91.G5_VIVOS) + VAL(EVENTOS_91.G6_VIVOS) + VAL(EVENTOS_91.G7_VIVOS) + VAL(EVENTOS_91.G1_MUERTOS) + VAL(EVENTOS_91.G2_MUERTOS) + VAL(EVENTOS_91.G3_MUERTOS) + VAL(EVENTOS_91.G4_MUERTOS) + VAL(EVENTOS_91.G5_MUERTOS) + VAL(EVENTOS_91.G6_MUERTOS) + VAL(EVENTOS_91.G7_MUERTOS) > 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 3'
			bIsValid = VAL(EVENTOS_91.G3_ENFERMO)=VAL(EVENTOS_91.G3_HOMBRES)+VAL(EVENTOS_91.G3_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 7'
			bIsValid = VAL(EVENTOS_91.G7_ENFERMO)=VAL(EVENTOS_91.G7_HOMBRES)+VAL(EVENTOS_91.G7_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de hombres y mujeres debe ser mayor que uno (>1)'
			bIsValid = VAL(EVENTOS_91.G1_HOMBRES) + VAL(EVENTOS_91.G2_HOMBRES) + VAL(EVENTOS_91.G3_HOMBRES) + VAL(EVENTOS_91.G4_HOMBRES) + VAL(EVENTOS_91.G5_HOMBRES) + VAL(EVENTOS_91.G6_HOMBRES) + VAL(EVENTOS_91.G7_HOMBRES) + VAL(EVENTOS_91.G1_MUJERES) + VAL(EVENTOS_91.G2_MUJERES) + VAL(EVENTOS_91.G3_MUJERES) + VAL(EVENTOS_91.G4_MUJERES) + VAL(EVENTOS_91.G5_MUJERES) + VAL(EVENTOS_91.G6_MUJERES) + VAL(EVENTOS_91.G7_MUJERES) > 1
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 6'
			bIsValid = VAL(EVENTOS_91.G6_ENFERMO)=VAL(EVENTOS_91.G6_VIVOS)+VAL(EVENTOS_91.G6_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 4'
			bIsValid = VAL(EVENTOS_91.G4_ENFERMO)=VAL(EVENTOS_91.G4_VIVOS)+VAL(EVENTOS_91.G4_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de hombres y mujeres debe ser igual al total de vivos y muertos'
			bIsValid = VAL(EVENTOS_91.G1_HOMBRES) + VAL(EVENTOS_91.G2_HOMBRES) + VAL(EVENTOS_91.G3_HOMBRES) + VAL(EVENTOS_91.G4_HOMBRES) + VAL(EVENTOS_91.G5_HOMBRES) + VAL(EVENTOS_91.G6_HOMBRES) + VAL(EVENTOS_91.G7_HOMBRES) + VAL(EVENTOS_91.G1_MUJERES) + VAL(EVENTOS_91.G2_MUJERES) + VAL(EVENTOS_91.G3_MUJERES) + VAL(EVENTOS_91.G4_MUJERES) + VAL(EVENTOS_91.G5_MUJERES) + VAL(EVENTOS_91.G6_MUJERES) + VAL(EVENTOS_91.G7_MUJERES) = VAL(EVENTOS_91.G1_VIVOS) + VAL(EVENTOS_91.G2_VIVOS) + VAL(EVENTOS_91.G3_VIVOS) + VAL(EVENTOS_91.G4_VIVOS) + VAL(EVENTOS_91.G5_VIVOS) + VAL(EVENTOS_91.G6_VIVOS) + VAL(EVENTOS_91.G7_VIVOS) + VAL(EVENTOS_91.G1_MUERTOS) + VAL(EVENTOS_91.G2_MUERTOS) + VAL(EVENTOS_91.G3_MUERTOS) + VAL(EVENTOS_91.G4_MUERTOS) + VAL(EVENTOS_91.G5_MUERTOS) + VAL(EVENTOS_91.G6_MUERTOS) + VAL(EVENTOS_91.G7_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a vivos + muertos para el grupo 5'
			bIsValid = VAL(EVENTOS_91.G5_ENFERMO)=VAL(EVENTOS_91.G5_VIVOS)+VAL(EVENTOS_91.G5_MUERTOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El total de enfermos debe ser igual a hombres + mujeres para el grupo 6'
			bIsValid = VAL(EVENTOS_91.G6_ENFERMO)=VAL(EVENTOS_91.G6_HOMBRES)+VAL(EVENTOS_91.G6_MUJERES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el brote está cerrado con identificacion del agente, debe haber algún agente identificado'
			bIsValid = EVENTOS_91.ESTADO_BRO!='2' OR !EMPTY(EVENTOS_91.AGENTE1 + EVENTOS_91.AGENTE2 + EVENTOS_91.AGENTE3 + EVENTOS_91.AGENTE4 + EVENTOS_91.AGENTE5)
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_Eventos_94

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La semana epidemiológica NO debe ser superior a la determinada por la fecha actual de grabación'
			bIsValid = isValidEpidemiologicalWeek(EVENTOS_94.SEMANA,EVENTOS_94.AÑO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El municipio no es coherente con el código de UPGD'
			bIsValid = LEFT(EVENTOS_94.COD_PRE,2)='11' OR (LEFT(EVENTOS_94.COD_PRE,5)=EVENTOS_94.COD_MUN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El municipio no es coherente con el código de UPGD'
			bIsValid = LEFT(EVENTOS_94.COD_PRE,2)!='11' OR LEFT(EVENTOS_94.COD_MUN,2)='11'
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_laboratorios

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si el evento es 710 y la prueba es "Pruebas genotipicas", el valor debe ser uno de los siguientes genotipos: 1a, 1B, 1C, 1D, 1E, 1F, 1g,  2A, 2B, 2c'
			bIsValid = !(LABORATORIOS.COD_EVE='710' AND LABORATORIOS.Prueba='B5' AND INLIST(LABORATORIOS.RESULTADO,'1','6')) OR ',' + UPPER(ALLTRIM(LABORATORIOS.VALOR)) + ',' $ ',1A,1B,1C,1D,1E,1F,1G,2A,2B,2C,'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es 730 y la prueba es "Pruebas genotipicas", el valor debe ser uno de los siguientes genotipos: A, B1, B2, B3, C1, C2, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, E, F, G1, G2, G3, H1, H2'
			bIsValid = !(LABORATORIOS.COD_EVE='730' AND LABORATORIOS.Prueba='B5' AND INLIST(LABORATORIOS.RESULTADO,'1','6')) OR ',' + UPPER(ALLTRIM(LABORATORIOS.VALOR)) + ',' $ ',A,B1,B2,B3,C1,C2,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,E,F,G1,G2,G3,H1,H2,'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La prueba no es consistente con la muestra  para el Evento Chikungunya (Cod. 217)'
			bIsValid = LABORATORIOS.COD_EVE!='217' OR ((LABORATORIOS.PRUEBA!='30' OR LABORATORIOS.MUESTRA=='4 '))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El agente no es consistente con la prueba para el Evento hepatitis B, C y coinfección Hepatitis B y Delta (Cod. 340)'
			bIsValid = LABORATORIOS.COD_EVE!='340' OR (LABORATORIOS.AGENTE!='45' OR INLIST(LABORATORIOS.PRUEBA,'50','A4'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de la Toma del Examen es obligatoria excepto para el evento hepatitis B, C y coinfección Hepatitis B y Delta (Cod. 340)'
			bIsValid = LABORATORIOS.COD_EVE='340' OR !EMPTY(LABORATORIOS.FEC_EXA)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La prueba no es consistente con la muestra  para el Evento Chikungunya (Cod. 217)'
			bIsValid = LABORATORIOS.COD_EVE!='217' OR (!(LABORATORIOS.PRUEBA='2 ' OR LABORATORIOS.PRUEBA='3 ') OR LABORATORIOS.MUESTRA=='13')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el Agente es: Virus respiratorios 101 a 178 ETAs (código 99 de la tabla), el campo Valor sólo recibe los valores 2, 3 o 4'
			bIsValid = (LABORATORIOS.agente != '99' OR COD_EVE='357') OR BETWEEN(VAL(LABORATORIOS.VALOR),2,4)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de Recepción en el Laboratorio debe ser posterior o igual a la Fecha de toma del exámen'
			bIsValid = EMPTY(LABORATORIOS.FEC_REC) OR (LABORATORIOS.FEC_REC  >= LABORATORIOS.FEC_EXA)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El agente no es consistente con la prueba para el Evento hepatitis B, C y coinfección Hepatitis B y Delta (Cod. 340)'
			bIsValid = LABORATORIOS.COD_EVE!='340' OR (AGENTE!='12' OR INLIST(LABORATORIOS.PRUEBA,'26','30','50','51','93','A4'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de Expedición del resultado debe ser posterior o igual a la Fecha de Recepción en el Laboratorio'
			bIsValid = EMPTY(LABORATORIOS.FEC_EXP) OR (LABORATORIOS.FEC_EXP  >= LABORATORIOS.FEC_REC)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El agente no es consistente con la prueba para el Evento hepatitis B, C y coinfección Hepatitis B y Delta (Cod. 340)'
			bIsValid = LABORATORIOS.COD_EVE!='340' OR (AGENTE!='46' OR INLIST(LABORATORIOS.PRUEBA,'95','30','A4','B5','D0','H6'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento Tosferina (Cod. 800), si la muestra es sangre, el resultado debe ser estrictamente un número'
			bIsValid = LABORATORIOS.COD_EVE!='800' OR (LABORATORIOS.MUESTRA!='1' OR ISNUMERIC(LABORATORIOS.VALOR))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Endometritis puerperal (Cod. 351)'
			bIsValid = LABORATORIOS.COD_EVE!='351' OR (LABORATORIOS.MUESTRA!='1' OR LABORATORIOS.PRUEBA='92')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Endometritis puerperal (Cod. 351)'
			bIsValid = LABORATORIOS.COD_EVE!='351' OR (LABORATORIOS.MUESTRA!='4' OR LABORATORIOS.PRUEBA='55')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Endometritis puerperal (Cod. 351)'
			bIsValid = LABORATORIOS.COD_EVE!='351' OR (LABORATORIOS.MUESTRA!='32' OR LABORATORIOS.PRUEBA='55')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico (Cod. 352)'
			bIsValid = LABORATORIOS.COD_EVE!='352' OR (LABORATORIOS.MUESTRA!='1' OR LABORATORIOS.PRUEBA='92')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico (Cod. 352)'
			bIsValid = LABORATORIOS.COD_EVE!='352' OR (LABORATORIOS.MUESTRA!='4' OR LABORATORIOS.PRUEBA='55')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La muestra no es consistente con la prueba para el evento Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico (Cod. 352)'
			bIsValid = LABORATORIOS.COD_EVE!='352' OR (LABORATORIOS.MUESTRA!='32' OR LABORATORIOS.PRUEBA='55')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_Paciente

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() AND !IsInmediateNotification()  THEN
		IF bIsValid THEN
			sRuleViolated = 'Mortalidad Perinatal y Neonatal Tardía (560), La fecha de defunción no debe ser mayor a la Fecha de Notificación'
			bIsValid = PACIENTE.COD_EVE!='560' OR (PACIENTE.FEC_DEF<=PACIENTE.FEC_NOT)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada  de la madre debe estar en el rango de 10 a 54 años o ser 60 años'
			bIsValid = PACIENTE.COD_EVE!='560'  OR (IsValidAgeForEvent('560',PACIENTE.EDAD,PACIENTE.UNI_MED) OR (PACIENTE.EDAD='60' AND PACIENTE.UNI_MED='1'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento Sífilis gestacional (Cod. 750), es obligatorio diligenciar Semanas de gestación'
			bIsValid = (PACIENTE.COD_EVE!='750')  OR !EMPTY(PACIENTE.SEM_GES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La semana epidemiológica NO debe ser superior a la determinada por la fecha actual de grabación'
			bIsValid = isValidEpidemiologicalWeek(PACIENTE.SEMANA,PACIENTE.AÑO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la paciente pertenece al grupo poblacional gestante, solo se admiten edades entre 10 y 64 años'
			bIsValid = PACIENTE.GP_GESTAN != '1' OR (PACIENTE.UNI_MED='1' AND BETWEEN(VAL(PACIENTE.EDAD),10,64))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad límite establecida para el evento según su definición en el documento de Codificación y reglas de validación de contenido para el ingreso de  los eventos de VSP de interés nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='549'  OR IsValidAgeForEvent('549',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Los datos registrados en tipo de documento de identidad, edad y unidad de medida no son consistentes'
			bIsValid = PACIENTE.AJUSTE!='0' OR isValidIdentificationDoc(PACIENTE.TIP_IDE, PACIENTE.UNI_MED, PACIENTE.EDAD)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ha diligenciado la causa básica de muerte, la condición final del paciente debe ser Muerto'
			bIsValid = EMPTY(PACIENTE.CBMTE) OR PACIENTE.CON_FIN='2'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Es obligatorio diligenciar la variable Hospitalizado'
			bIsValid = (PACIENTE.COD_EVE='110' OR PACIENTE.COD_EVE='000' OR PACIENTE.COD_EVE='855' OR PACIENTE.COD_EVE='339' OR PACIENTE.COD_EVE='344' OR PACIENTE.COD_EVE='207' OR PACIENTE.COD_EVE='745' OR PACIENTE.COD_EVE='896' OR PACIENTE.COD_EVE='467') OR  !EMPTY(PACIENTE.PAC_HOS)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento Exposición a flúor, la edad solo admite valores 6, 12, 15 y 18'
			bIsValid = PACIENTE.COD_EVE!='228' OR INLIST(PACIENTE.EDAD,'6','12','15','18')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ingresa un registro nuevo, La semana de ocurrencia NO puede ser superior a la semana de notificación'
			bIsValid = VAL(PACIENTE.ajuste) != 0  OR isLessThanOrEqual(VAL(PACIENTE.semana),VAL(PACIENTE.año),VAL(getSimpleEpidemiologicalYear(PACIENTE.FEC_NOT)),VAL(getSimpleEpidemiologicalYear(PACIENTE.FEC_NOT)))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si no es notificación negativa, la Fecha de Nacimiento NO puede ser mayor a la fecha de Notificación'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000') OR EMPTY(PACIENTE.FECHA_NTO) OR EMPTY(PACIENTE.FEC_NOT) OR (PACIENTE.FECHA_NTO <= PACIENTE.FEC_NOT)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si no es notificación negativa, La Fecha de Nacimiento NO puede ser mayor a la fecha de Consulta'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000') OR EMPTY(PACIENTE.FECHA_NTO) OR EMPTY(PACIENTE.FEC_CON) OR (PACIENTE.FECHA_NTO <= PACIENTE.FEC_CON)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si no es notificación negativa, La Fecha de Nacimiento NO puede ser mayor a la fecha de Inicio de Sintomas'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000') OR EMPTY(PACIENTE.FECHA_NTO) OR EMPTY(PACIENTE.INI_SIN) OR (PACIENTE.FECHA_NTO <= PACIENTE.INI_SIN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El año de la fecha de consulta  debe ser posterior al año 1999'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000','342') OR EMPTY(PACIENTE.FEC_CON) OR (YEAR(PACIENTE.FEC_CON) > 1999)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento NO corresponde a: 340 HEPATITIS B, 550 MORTALIDAD MATERNA, 560 MORTALIDAD PERINATAL, 850 VIH/SIDA/MORTALIDAD POR SIDA, 342 ENFERMEDADES HUERFANAS - RARAS, entonces el año de inicio de síntomas debe ser posterior al año 2000'
			bIsValid = INLIST(PACIENTE.COD_EVE, '000', '550 ','551 ', '560 ', '850 ', '340 ', '342 ') OR EMPTY(PACIENTE.INI_SIN) OR YEAR(PACIENTE.INI_SIN) > 2000
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si no es notificación negativa y se está haciendo un ajuste, la fecha del Ajuste no debe ser menor a la de Notificación'
			bIsValid = INLIST(PACIENTE.COD_EVE , '000') OR VAL(PACIENTE.AJUSTE) = 0 OR EMPTY(PACIENTE.FEC_AJU) OR EMPTY(FEC_NOT) OR ((PACIENTE.FEC_AJU >=PACIENTE.FEC_NOT))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada del paciente NO se encuentra dentro del rango permitido para la notificación del evento'
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
			sRuleViolated = 'La fecha de consulta no debe ser mayor a la de Notificación'
			bIsValid = PACIENTE.COD_EVE = '000' OR (PACIENTE.FEC_CON <= PACIENTE.FEC_NOT)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de Hospitalización no debe ser inferior a la de Consulta'
			bIsValid = PACIENTE.COD_EVE = '000' OR ALLTRIM(PACIENTE.COD_EVE) $ '357,351,352' OR EMPTY(PACIENTE.FEC_HOS) OR EMPTY(PACIENTE.FEC_CON) OR (PACIENTE.FEC_HOS >= PACIENTE.FEC_CON)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento Defectos congénitos (Cod. 215) ,  no aplican los grupos poblacionales Carcelarios ni Gestantes'
			bIsValid = (PACIENTE.COD_EVE!='215') OR (PACIENTE.GP_GESTAN !='1' AND PACIENTE.GP_CARCELA!='1')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada es inferior a la edad límite establecida para el evento según su definición en el documento de Codificación y reglas de validación de contenido para el ingreso de  los eventos de VSP de interés nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='340'  OR IsValidAgeForEvent2('340',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad límite establecida para el evento según su definición en el documento de Codificación y reglas de validación de contenido para el ingreso de  los eventos de VSP de interés nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='550'  OR IsValidAgeForEvent('550',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se marca SEXO masculino NO se puede tener Grupo poblacional GESTANTES'
			bIsValid = SEXO!='M' OR GP_GESTAN !='1'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El único evento que admite edad cero es defectos congénitos (Cod. 215)'
			bIsValid = PACIENTE.COD_EVE='000' OR (PACIENTE.EDAD!='0' OR PACIENTE.COD_EVE='215')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad límite establecida para el evento según su definición en el documento de Codificación y reglas de validación de contenido para el ingreso de  los eventos de VSP de interés nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='459'  OR IsValidAgeForEvent('459',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para los eventos Difteria (Cod. 230) y Desnutrición aguda en menores de cinco años (Cod. 113) es obligatoria la fecha de nacimiento'
			bIsValid = !(PACIENTE.COD_EVE='230' OR PACIENTE.COD_EVE='113') OR !EMPTY(PACIENTE.FECHA_NTO)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad límite establecida para el evento según su definición en el documento de Codificación y reglas de validación de contenido para el ingreso de  los eventos de VSP de interés nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='115'  OR IsValidAgeForEvent('115',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento Cáncer en menores de 18 años (Cod. 115), solo se  puede tener clasificación confirmado por clínica cuando el caso se reporte como mortalidad'
			bIsValid = PACIENTE.COD_EVE!='115' OR (PACIENTE.CON_FIN='2' OR PACIENTE.TIP_CAS!='4')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada es inferior a la edad límite establecida para el evento según su definición en el documento de Codificación y reglas de validación de contenido para el ingreso de  los eventos de VSP de interés nacional en el aplicativo Sivigila'
			bIsValid = !INLIST(PACIENTE.COD_EVE,'855','339','344','207','745','896','467','351','110')  OR IsValidAgeForEvent2(PACIENTE.COD_EVE,PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad límite establecida para el evento según su definición en el documento de Codificación y reglas de validación de contenido para el ingreso de  los eventos de VSP de interés nacional en el aplicativo Sivigila'
			bIsValid = !INLIST(PACIENTE.COD_EVE,'855','339','344','207','745','896','467','351')  OR IsValidAgeForEvent(PACIENTE.COD_EVE,PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es Defectos congénitos (Cod. 215), cuando la edad es diferente de 0, entonces condición final debe ser diferente de 0'
			bIsValid = PACIENTE.COD_EVE!='215' OR (PACIENTE.EDAD='0' OR PACIENTE.CON_FIN!='0')
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la condición final del paciente es Muerto, debe suministrar la fecha de defunción'
			bIsValid = PACIENTE.CON_FIN!='2' OR !EMPTY(PACIENTE.FEC_DEF)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de hospitalización no puede ser inferior a la fecha de inicio de síntomas excepto cuando se trata de uno de los eventos con Cod. 357,351,352'
			bIsValid = !(!(ALLTRIM(PACIENTE.COD_EVE) $ '357,351,352') AND !EMPTY(PACIENTE.FEC_HOS)) OR  (PACIENTE.FEC_HOS >= PACIENTE.INI_SIN)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La Fecha de Inicio de Sintomas no debe ser mayor a la de Fecha de Consulta'
			bIsValid = !(INLIST(PACIENTE.COD_EVE,'351','352') AND PACIENTE.PAC_HOS='2') OR (PACIENTE.INI_SIN <= PACIENTE.FEC_CON)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La edad registrada supera la edad límite establecida para el evento según su definición en el documento de Codificación y reglas de validación de contenido para el ingreso de  los eventos de VSP de interés nacional en el aplicativo Sivigila'
			bIsValid = PACIENTE.COD_EVE!='551'  OR IsValidAgeForEvent('551',PACIENTE.EDAD,PACIENTE.UNI_MED)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fuente de la vigilancia es obligatoria'
			bIsValid = (!(INLIST(PACIENTE.AJUSTE,'0','7') OR EMPTY(PACIENTE.AJUSTE)) OR !EMPTY(PACIENTE.FUENTE))  OR COD_EVE='000'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es Malaria (Cod. 465), cuando la condición final es muerto, el tipo de caso debe ser  confirmado por nexo o por laboratorio'
			bIsValid = PACIENTE.COD_EVE!="465" OR ( PACIENTE.CON_FIN!='2' OR (PACIENTE.TIP_CAS='3' OR PACIENTE.TIP_CAS='5'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para cualquier opción distinta a "No afiliado" o "Indeterminado/Pendiente" en "Tipo de Régimen en Salud", es obligatorio diligenciar el  código de administradora'
			bIsValid = PACIENTE.COD_EVE='000' OR (INLIST(PACIENTE.TIP_SS,'N','I')  OR !EMPTY(PACIENTE.COD_ASE))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El sexo debe ser M o F. Solamente cuando el evento es Defectos congénitos (Cod. 215) o Sífilis Congenita (Cod. 740) se admite sexo = I (Indeterminado)'
			bIsValid = PACIENTE.COD_EVE='000' OR (PACIENTE.COD_EVE='215' OR PACIENTE.COD_EVE='740' OR (PACIENTE.sexo='M' OR  PACIENTE.sexo='F'))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El paciente debe pertenecer a algún grupo poblacional'
			bIsValid = INLIST(PACIENTE.COD_EVE,'000','855','339','344','207','745','896','467') OR (PACIENTE.GP_DISCAPA='1' OR PACIENTE.GP_DESPLAZ='1' OR PACIENTE.GP_MIGRANT='1' OR PACIENTE.GP_CARCELA='1' OR PACIENTE.GP_GESTAN ='1' OR PACIENTE.GP_INDIGEN='1' OR PACIENTE.GP_POBICFB='1' OR PACIENTE.GP_MAD_COM='1' OR PACIENTE.GP_DESMOVI='1' OR PACIENTE.GP_PSIQUIA='1' OR PACIENTE.GP_VIC_VIO='1' OR PACIENTE.GP_OTROS  ='1' )
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Para el evento Morbilidad materna extrema (Cod. 549), es obligatorio diligenciar Semanas de gestación si se trata de una gestante'
			bIsValid = !(PACIENTE.COD_EVE='549' and PACIENTE.GP_GESTAN='1')  OR !EMPTY(PACIENTE.SEM_GES)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El año de la fecha de consulta  debe ser posterior al año 1999'
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
			sRuleViolated = 'Si area de ocurrencia es 3 = Rural Disperso, debe diligenciar como mínimo el nombre de la vereda'
			bIsValid = PACIENTE.AREA!='3' OR !EMPTY(PACIENTE.VEREDA)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El tipo de documento CC solo aplica para pacientes de nacionalidad colombiana'
			bIsValid = PACIENTE.TIP_IDE!='CC' OR PACIENTE.NACIONALID='170'
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si el evento es IRA por virus nuevo (Cod. 346), la clasificación o ajuste "Confirmado por nexo" solo aplica si el paciente es hospitalizado o fallecido.'
			bIsValid = !(PACIENTE.COD_EVE='346' AND (PACIENTE.TIP_CAS='5' OR PACIENTE.AJUSTE='5')) OR (PACIENTE.CON_FIN='2' OR PACIENTE.PAC_HOS='1')
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_SeguimientoContactos

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'Si marcó 1 en "Sintomático" , debe diligenciar en 1=Sí, al menos uno de los síntomas'
			bIsValid = SeguimientoContactos.SINTOMATIC!='1' OR OCCURS('1',SeguimientoContactos.TOS+SeguimientoContactos.FIEBRE+SeguimientoContactos.DIF_RES+SeguimientoContactos.ODINOFAGIA+SeguimientoContactos.ADINAMIA)>=1
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC

FUNCTION RecValidationRuleFor_upgd

	bIsValid=.T.
	sRuleViolated=''

	IF !DoNotApplyRules() THEN
		IF bIsValid THEN
			sRuleViolated = 'La Longitud del Nit no debe ser inferior a 5 Caracteres'
			bIsValid = EMPTY(UPGD.nit_upgd) OR LEN(ALLTRIM(UPGD.nit_upgd)) >= 5
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'El correo electrónico de la UPGD no debe estar en blanco, debe tener al menos un carácter @'
			bIsValid = NOT EMPTY(UPGD.cor_ele) AND AT('@', UPGD.cor_ele) != 0
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'La fecha de constitución no puede ser anterior a 1900'
			bIsValid = EMPTY(UPGD.fec_con) OR YEAR(UPGD.fec_con) >= 1900
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si la UPGD está cerrada, no puede estar activa'
			bIsValid = UPGD.ESTADOUPGD!='2' OR UPGD.ACT_SIV=2
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ha diligenciado "Notifica IAD" en 1, debe diligenciar todas las variables pertinentes de la segunda sección de la ficha'
			bIsValid = UPGD.NOTIF_IAD!='1' OR (!EMPTY(UPGD.HOSP_UNIVE) AND !EMPTY(UPGD.REG_EXCEPC) AND !EMPTY(UPGD.TOT_CAMAS ) AND !EMPTY(UPGD.COMITE_INF) AND !EMPTY(UPGD.BI_PROFESI) AND !EMPTY(UPGD.IAAS_ULTIM) AND !EMPTY(UPGD.INF_TENDEN) AND !EMPTY(UPGD.SOCIAL_TEN) AND !EMPTY(UPGD.LAB_MICROB) AND !EMPTY(UPGD.MICR_CDI) AND !EMPTY(UPGD.WHONET ) AND !EMPTY(UPGD.INFORM_PAT) AND !EMPTY(UPGD.LAB_CON_PE) AND !EMPTY(UPGD.LAB_REM_CE) AND !EMPTY(UPGD.LAB_REPORT))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si "La UPGD dispone de servicio de laboratorio de microbiología", debe diligenciar todas las variables de laboratorio'
			bIsValid = UPGD.LAB_MICROB!='1' OR (!EMPTY(UPGD.LAB_PROPIO) AND !EMPTY(UPGD.IDENT_GYE ) AND !EMPTY(UPGD.PRUE_SUCEP) AND !EMPTY(UPGD.LAB_AUTOMA) AND !EMPTY(UPGD.VITEK) AND !EMPTY(UPGD.MICROSCAN ) AND !EMPTY(UPGD.PHOENIX) AND !EMPTY(UPGD.LAB_CCI) AND !EMPTY(UPGD.LAB_CCE))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ha diligenciado "Notifica IAD" en 1, el nivel de complejidad de la institución debe ser superior a 1'
			bIsValid = UPGD.NOTIF_IAD!='1' OR INLIST(UPGD.NIV,2,3)
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ha diligenciado "Notifica ISO" en 1, debe diligenciar todas las variables de la sección "DATOS DE LA VIGILANCIA DE LAS INFECCIONES ASOCIADAS A PROCEDIMIENTOS MÉDICO QUIRÚRGICOS"'
			bIsValid = UPGD.NOTIF_ISO!='1' OR (!EMPTY(UPGD.SER_CESARE) AND !EMPTY(UPGD.SER_PAR_VA) AND !EMPTY(UPGD.SER_COLECI) AND !EMPTY(UPGD.SER_HERNIO) AND !EMPTY(UPGD.SER_REVASC))
		ENDIF

		IF bIsValid THEN
			sRuleViolated = 'Si se ha diligenciado "Notifica CAB" en 1, debe diligenciar todas las variables de la sección "DATOS DE LA VIGILANCIA DE CAB"'
			bIsValid = UPGD.NOTIF_CAB!='1' OR (!EMPTY(UPGD.QUIEN_VCAB) )
		ENDIF


		IF VARTYPE(gsErrorMsg)='C' AND !bIsValid THEN
			gsErrorMsg = sRuleViolated
		ENDIF
	ENDIF
	RETURN bIsValid
ENDFUNC


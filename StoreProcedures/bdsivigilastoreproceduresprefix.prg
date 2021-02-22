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

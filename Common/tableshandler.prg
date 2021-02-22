PROCEDURE fieldInfo

	LPARAMETERS sTargetTableName as string, sFieldName as String, aFieldStructure
	
	*Retrieves in the array aFieldStructure information about the table's field sFieldName that is supossed to;
	 belong to sTargetTableName 
	
	LOCAL nSelectedWorkArea as Byte, bSourceTableIsOpen as Boolean 
	LOCAL iField as Byte, nFields as Byte  
	
	DIMENSION aTargetFields(1)
	
	nSelectedWorkArea=SELECT()
	bSourceTableIsOpen= USED(JUSTSTEM(sTargetTableName))

	IF useTable(sTargetTableName)
		SELECT (JUSTSTEM(sTargetTableName)) 
		nFields = AFIELDS(aTargetFields)
		nTargetField = ASCAN(aTargetFields,sFieldName,-1,-1,1,15) 
		IF nTargetField !=0 THEN
			FOR iFiledSpec=1 TO 18
				DIMENSION aFieldStructure(iFiledSpec)
				aFieldStructure(iFiledSpec) = aTargetFields(nTargetField,iFiledSpec)
			ENDFOR
		ENDIF
	ENDIF
	
	IF !bSourceTableIsOpen THEN
		SELECT (JUSTSTEM(sTargetTableName))
		USE
	ENDIF
	SELECT (nSelectedWorkArea)
	
ENDPROC

PROCEDURE fieldInfoTst
	DIMENSION aTargetFieldStruc(1)
SET STEP ON 	
	DO fieldInfo WITH "..\PACIENTE","COD_EVE",aTargetFieldStruc
	DISPLAY MEMORY LIKE aTargetFieldStruc
ENDPROC


FUNCTION UseTable

	LPARAMETERS sTableNameToUse as String, sTableAlias as String, sCurrentAlias as String
	
	*Opens sTableNameToUse in first available workarea without selecting it. ;
	Returns .T. if it was posible to open sTableNameToUse or it was already opened, otherwise returns .F. ;
	sTableNameToUse may be a fully qualified file name but without extension
	
	*If sTableAlias  is supplied, creates an alias for the table
	*If sCurrentAlias is supplied, it will hold the current alias name for the table sTableNameToUse 
	
	
	LOCAL bReturnedValue AS Boolean 
		
	bReturnedValue=.T.
	IF !USED(JUSTSTEM(sTableNameToUse)) THEN
		IF FILE(sTableNameToUse + ".DBF") THEN
			IF VARTYPE(sTableAlias)='C' THEN
				TRY
					USE (sTableNameToUse) IN 0 ALIAS (sTableAlias)
					IF VARTYPE(sCurrentAlias)='C' THEN
						sCurrentAlias = sTableAlias
					ENDIF
				CATCH TO oException
					bReturnedValue=.F.
					IF VARTYPE(sCurrentAlias)='C' THEN
						sCurrentAlias = ''
					ENDIF
				ENDTRY
			ELSE
				USE (sTableNameToUse) IN 0
				IF VARTYPE(sCurrentAlias)='C' THEN
					sCurrentAlias = JUSTSTEM(sTableNameToUse)
				ENDIF
			ENDIF
		ELSE
			bReturnedValue=.F.
			IF VARTYPE(sCurrentAlias)='C' THEN
				sCurrentAlias = ''
			ENDIF
		ENDIF
	ELSE
		IF VARTYPE(sCurrentAlias)='C' THEN
			sCurrentAlias = JUSTSTEM(sTableNameToUse)
		ENDIF
	ENDIF
	RETURN bReturnedValue
ENDFUNC


PROCEDURE CloseTables

	LPARAMETERS tablesNameList as String
	*Closes the tables in tablesNameList -a comma separated list of table names
	
	LOCAL i as Byte
	LOCAL nTablesToClose as Number 
	
	nTablesToClose = ALINES(aTablesToClose,tablesNameList,1,",")
	FOR i=1 TO nTablesToClose 
		IF USED(aTablesToClose(i)) THEN
			SELECT (aTablesToClose(i))
			USE
		ENDIF
	ENDFOR
ENDPROC


FUNCTION fieldValue(sTargetFieldName AS String, sTargetTableNameAndPath as String, sFilter as String, sOrderBy as String, nRecords as Number)
	
	*Retorna el valor almacenado en el(los) campo(s) sTargetFieldName del registro de la tabla sTargetTableNameAndPath que cumpla la condición sFilter. ;
	Si hay más de un registro en sTargetTableNameAndPath que cumpla la condición, retorna el valor del primero encontrado.
	
	*sTargetFieldName puede ser una lista de campos separada por comas; en este caso, se retornará una cadena separada por comas con los valores ;
	de cada campo convertido a un string; si sTargetFieldName es un solo campo, retornará su valor sin conversión alguna
	
	*El parámetro opcional sOrderBy permite especificar un criterio de ordenación para los registros que cumplan la condición sFilter ;
	de tal forma que, si existen varios registros que la cumplan, el valor retornado sea el primero de acuerdo con ese orden.
	
	*El parámetro opcional nRecords contendrá el número de registros encontrados
	
	LOCAL vReturnedValue  AS VARIANT
	LOCAL nSelectedWorkArea as Integer, bSourceTableIsOpen as Boolean
		
	vReturnedValue = NULL
	
	nSelectedWorkArea=SELECT()
	sTargetTableName = JUSTSTEM(sTargetTableNameAndPath)
	bSourceTableIsOpen= USED(sTargetTableName) 

	IF FILE('&sTargetTableNameAndPath..DBF') THEN
		lcsSelectSqlCmd = 'SELECT ' + sTargetFieldName + ' FROM ' + sTargetTableNameAndPath 
		IF !EMPTY(sFilter)
			lcsSelectSqlCmd  = lcsSelectSqlCmd + ' WHERE ' + sFilter
		ENDIF
		IF !EMPTY(sOrderBy )
			lcsSelectSqlCmd  = lcsSelectSqlCmd + ' ORDER BY ' + sOrderBy 
		ENDIF

		lcsSelectSqlCmd  = lcsSelectSqlCmd  + ' INTO ARRAY aResult'
		&lcsSelectSqlCmd
		nRecords = _TALLY
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
			sTableNameToClose = JUSTFNAME(sTargetTableName)
			SELECT (sTableNameToClose)
			USE
		ENDIF
	ENDIF
	SELECT (nSelectedWorkArea)
	
	RETURN vReturnedValue
ENDFUNC


FUNCTION IsField

	LPARAMETERS sTableName as String, sFieldName as String
	
	LOCAL bReturnedValue AS Boolean 
	
	*Returns .T. if sFieldName is a sTableName field, otherwise returns .F.
	
	bReturnedValue=.F.
	IF UseTable(sTableName) THEN
		nSourceFields=AFIELDS(aSourceFields,SUBSTR(sTableName,RAT('\',sTableName)+1))
		IF ASCAN(aSourceFields,sFieldName ,-1,-1,-1,15) > 0 THEN
			bReturnedValue=.T.
		ENDIF
	ENDIF
	RETURN bReturnedValue

ENDFUNC

FUNCTION SelectTable

	LPARAMETERS sTableNameToUse as String, sOrderTag as String, bExclusive as Boolean, sAlias as String
	
	LOCAL bReturnedValue as Boolean 
	LOCAL sOldExclusive as String
	
	bReturnedValue = .F.
	
	sOldExclusive = SET("Exclusive")
	IF bExclusive THEN
		SET EXCLUSIVE ON
	ELSE
		SET EXCLUSIVE OFF
	ENDIF

	TRY
		IF VARTYPE(sAlias)='C' AND !EMPTY(sAlias) THEN
			bReturnedValue = UseTable(sTableNameToUse, sAlias)
		ELSE
			bReturnedValue = UseTable(sTableNameToUse)
		ENDIF
		IF  bReturnedValue THEN
			SELECT JUSTSTEM(sTableNameToUse)
			IF bExclusive THEN
				bReturnedValue = ISEXCLUSIVE()
			ENDIF
			IF VARTYPE(sOrderTag) = 'C'
				IF !EMPTY(sOrderTag )
					SET ORDER TO TAG (sOrderTag)
				ENDIF
			ENDIF
		ENDIF
	CATCH TO oException
	FINALLY
		IF bExclusive THEN
			SET EXCLUSIVE &sOldExclusive
		ENDIF
	ENDTRY
	RETURN bReturnedValue
ENDFUNC

PROCEDURE TransposeTable(sTargetDB as String, sTableName as String, sGroupByExpr as String, bNumberGroupByFields as Boolean, ;
						sExcludedFields as String, nMaxEntities as Byte)

	sOldSafety=SET("Safety")
	SET SAFETY OFF

	sOldDB = SET("Database")

	OPEN DATABASE &sTargetDB 
	SET DATABASE TO (sTargetDB)
	
	*Establece el número máximo de valores distintos que existen en la tabla a pivotear correspondientes a sGroupByExpr 
	SELECT &sGroupByExpr, COUNT(*) as n FROM '&sTableName' GROUP BY &sGroupByExpr ORDER BY n DESC INTO CURSOR rsGroupinValues NOFILTER
	IF VARTYPE(nMaxEntities) != 'L' THEN
		nRepetitions = MIN(rsGroupinValues.n,nMaxEntities)
	ELSE
		nRepetitions = rsGroupinValues.n
	ENDIF

	*Crea una tabla que almacenará los datos pivoteados resultantes
	nTableFields = AFIELDS(aTableFields,sTableName)

	nHeaders=ALINES(aHeaderFields,sGroupByExpr,1,',')
	sPivotTableName = "tbPivot" + sTableName 
	sSQLCmd = "CREATE TABLE " + sPivotTableName + " (" 
	FOR nField=1 TO nHeaders
		IF bNumberGroupByFields THEN
			sSQLCmd = sSQLCmd + "_0" + aTableFields(nField,1) + " "
		ELSE
			sSQLCmd = sSQLCmd + aTableFields(nField,1) + " "
		ENDIF
		sSQLCmd = sSQLCmd +  + aTableFields(nField,2) +	"(" + ALLTRIM(STR(aTableFields(nField,3))) + "), "
	NEXT 

	FOR nFieldRepetition=1 TO nRepetitions 
		FOR nField=nHeaders+1 TO nTableFields 
			IF !(aTableFields(nField,1) $ sExcludedFields) THEN
				sSQLCmd = sSQLCmd + "_" + ALLTRIM(STR(nFieldRepetition)) + "_" + aTableFields(nField,1) + " " + aTableFields(nField,2) +;
							"(" + ALLTRIM(STR(aTableFields(nField,3))) + "), "
			ENDIF
		NEXT 
	NEXT
	sSQLCmd = LEFT(sSQLCmd,LEN(sSQLCmd)-2) + ")"
	
	TRY
		REMOVE TABLE (sPivotTableName)
	CATCH TO oException
	ENDTRY
	&sSQLCmd

	*Transfiere las filas de cada entidad determinada por sGroupByExpr a columnas en la tabla pivote
	sSQLCmd = "INSERT INTO '" + sPivotTableName + "' ("
	IF bNumberGroupByFields THEN
		FOR nField=1 TO nHeaders
			sSQLCmd = sSQLCmd + "_0" + aHeaderFields(nField) + ", "
		NEXT
		sSQLCmd = LEFT(sSQLCmd,LEN(sSQLCmd)-2)
	ELSE
		sSQLCmd = sSQLCmd + sGroupByExpr 
	ENDIF
	sSQLCmd = sSQLCmd + ") SELECT " + sGroupByExpr + " FROM rsGroupinValues"
	&sSQLCmd

	SELECT &sGroupByExpr, COUNT(*) as n FROM '&sTableName' GROUP BY &sGroupByExpr INTO CURSOR rsGroupinValues NOFILTER
	SELECT * FROM '&sTableName' ORDER BY &sGroupByExpr INTO CURSOR rsSourceData
	SCAN
		IF VARTYPE(nMaxEntities) != 'L' THEN
			nFieldsToUpdate = MIN(rsGroupinValues.n,nMaxEntities)
		ELSE
			nFieldsToUpdate = rsGroupinValues.n
		ENDIF
		
		FOR nFieldRepetition=1 TO nFieldsToUpdate 
			sSQLCmd = "UPDATE " + sPivotTableName + " SET "
			FOR nField=nHeaders+1 TO nTableFields 
				IF !(aTableFields(nField,1) $ sExcludedFields) THEN
					sSQLCmd = sSQLCmd + "_" + ALLTRIM(STR(nFieldRepetition)) + "_" + aTableFields(nField,1) + " = " +;
								"rsSourceData." + aTableFields(nField,1) + ", " 
				ENDIF
			NEXT 
			sSQLCmd = LEFT(sSQLCmd,LEN(sSQLCmd)-2)
			sWHEREClause = ' WHERE .T.'
			FOR nField=1 TO nHeaders
				IF bNumberGroupByFields THEN
					sWHEREClause = sWHEREClause + " AND " + "_0" + aHeaderFields(nField) + " = '" + rsSourceData.&aHeaderFields(nField) + "'"
				ELSE
					sWHEREClause = sWHEREClause + " AND " + aHeaderFields(nField) + " = '" + rsSourceData.&aHeaderFields(nField) + "'"
				ENDIF
			NEXT
			sSQLCmd = sSQLCmd + sWHEREClause
			&sSQLCmd 

			SKIP 1 IN rsSourceData
		NEXT
		IF nFieldsToUpdate < rsGroupinValues.n THEN
			SKIP (rsGroupinValues.n - nFieldsToUpdate) -1 IN rsSourceData
		ELSE
			SKIP -1 IN rsSourceData
		ENDIF
		
		SKIP 1 IN rsGroupinValues
		SELECT rsSourceData
	ENDSCAN
		
	SET SAFETY &sOldSafety
	SET DATABASE TO &sOldDB
ENDPROC

PROCEDURE TransposeTableTst
*SET STEP ON
	SET DELETED ON 
	DO TransposeTable WITH 'SivigilaTemp', 'UPGD_UCIS', 'COD_PRE,COD_SUB', .T., ;
		'NOTIFICA,PERIODO,FLU_NOT,INMEDIATA,FECHACARGA,ESTADOTRAN,VERSION', 8
	SET DELETED OFF
ENDPROC


PROCEDURE splitRecords(sTableName as string, sFieldName as String, sFilter as String, sFieldNameAlias as String)

	*Particiona los registros de sTableName que cumplan la condición sFilter (opcional) según los grupos que puedan establecerse ;
	a partir del campo de tipo string sFieldName. Las particiones producidas quedarán almacenadas en recordsets de tipo NOFILTER ;
	que quedarán nombrados como sTableName_sFieldName(i); por ejemplo, si sFieldName es un campo de nombre GRUPO y los valores ;
	distintos que de él se encuentran en sTableName son GR1, GR2 y GR3, las particiones	resultantes quedarán en los recordsets ;
	sTableName_GRUPO_GR1, sTableName_GRUPO_GR2 y sTableName_GRUPO_GR3. Los grupos establecidos y los nombres de los ;
	recordsets resultantes quedan almacenados en un cursor de nombre rsTablesHandlerGroups con campos IDGrupo C(), RSName C(254)
	
	#DEFINE SPECIAL_CHARS "-."
	
	LOCAL bContinue as Boolean 
	bContinue = .T.
	
	LOCAL sSQLCmd AS String
	
	=SelectTable(sTableName)
	sTableName = JUSTSTEM(sTableName)
	
	*Establece cuáles son los grupos implicados por sFieldName 
	IF VARTYPE(sFilter)!='C' OR EMPTY(sFilter) THEN 
		sFilter = '.T.'
	ENDIF
	sFilter = "!EMPTY(" + sFieldName + ") AND " + sFilter
	
	TRY
		sSQLCmd = "SELECT DISTINCT " + sFieldName + " as IDGrupo, SPACE(254) AS RSName FROM " + sTableName + " WHERE " + sFilter +;
					" INTO CURSOR rsTablesHandlerGroups READWRITE" 
		&sSQLCmd 
	CATCH TO oException
		bContinue = .F.
	ENDTRY 
*SET STEP ON 	
	IF bContinue THEN 
		SELECT rsTablesHandlerGroups
		SCAN 
			IF EMPTY(sFieldNameAlias) THEN 
				sRSName = CHRTRAN(ALLTRIM(JUSTSTEM(sTableName) + "_" + sFieldName + "_" + rsTablesHandlerGroups.IDGrupo),SPECIAL_CHARS,"_")
			ELSE
				sRSName = CHRTRAN(ALLTRIM(JUSTSTEM(sTableName) + "_" + sFieldNameAlias + "_" + rsTablesHandlerGroups.IDGrupo),SPECIAL_CHARS,"_")
			ENDIF
			
			REPLACE RSName WITH sRSName
			sSQLCmd = "SELECT " + sTableName + ".* FROM " + sTableName + " WHERE " + sFieldName + "='" + rsTablesHandlerGroups.IDGrupo + "'" +;
						" AND " + sFilter + " INTO CURSOR " + rsTablesHandlerGroups.RSName + " NOFILTER"
			&sSQLCmd
		ENDSCAN 
	ENDIF 
ENDPROC


PROCEDURE splitRecordsTst()
	*SET STEP ON 
	*DO splitRecords WITH 'C:\INS\SIVIGILA\PACIENTE','COD_ASE', 'Año="2018"'
	DO splitRecords WITH 'C:\INS\SIVIGILA\PACIENTE','LEFT(COD_PRE,2)', 'Año="2018"', 'COD_DPTO_NOTIFICADOR'
ENDPROC


FUNCTION fieldToList(sTargetFieldName AS String, sTargetTableNameAndPath as String, sFilter as String, sOrderBy as String, nRecords as Number) as String
	
	*Retorna una lista separada por comas con los valores almacenados en el campo sTargetFieldName de los registros de la tabla sTargetTableNameAndPath ;
	que cumplan la condición sFilter.
	
	*El parámetro opcional sOrderBy permite especificar un criterio de ordenación para los registros que cumplan la condición sFilter ;
	de tal forma que, si existen varios registros que la cumplan, la lista retornada obedezca ese orden.
	
	*El parámetro opcional nRecords contendrá el número de registros encontrados y, por tanto, el número de elementos de la lista
	
		
	STORE '' TO sValuesList
	STORE SELECT() TO nSelectedWorkArea 
	STORE JUSTSTEM(sTargetTableNameAndPath) TO sTargetTableName
		
	LOCAL bSourceTableIsOpen as Boolean
	bSourceTableIsOpen = USED(sTargetTableName) 

	IF FILE('&sTargetTableNameAndPath..DBF') THEN
		lcsSelectSqlCmd = 'SELECT ' + sTargetFieldName + ' FROM ' + sTargetTableNameAndPath 
		IF !EMPTY(sFilter)
			lcsSelectSqlCmd  = lcsSelectSqlCmd + ' WHERE ' + sFilter
		ENDIF
		IF !EMPTY(sOrderBy )
			lcsSelectSqlCmd  = lcsSelectSqlCmd + ' ORDER BY ' + sOrderBy 
		ENDIF
		sRsResults = SYS(2015)
		lcsSelectSqlCmd  = lcsSelectSqlCmd  + ' INTO CURSOR ' + sRsResults 
		&lcsSelectSqlCmd
		nRecords = _TALLY
		
		IF _TALLY != 0
			SELECT (sRsResults)
			SCAN
				DO CASE 
					CASE VARTYPE(&sRsResults..&sTargetFieldName)='C'
						sValuesList  = sValuesList + &sRsResults..&sTargetFieldName
					CASE VARTYPE(&sRsResults..&sTargetFieldName)='D'
						sValuesList  = sValuesList  + DTOC(&sRsResults..&sTargetFieldName)
					CASE VARTYPE(&sRsResults..&sTargetFieldName)='N'
						sValuesList  = sValuesList  + ALLTRIM(STR(&sRsResults..&sTargetFieldName))
					CASE VARTYPE(&sRsResults..&sTargetFieldName)='L'
						sValuesList  = sValuesList  + TRANSFORM(&sRsResults..&sTargetFieldName,'Y')
				ENDCASE
				sValuesList  = sValuesList  + ","
			
			ENDSCAN 
			sValuesList  = LEFT(sValuesList ,LEN(sValuesList )-1)
		ENDIF
		USE IN (sRsResults)
		
		IF !bSourceTableIsOpen THEN
			sTableNameToClose = JUSTFNAME(sTargetTableName)
			SELECT (sTableNameToClose)
			USE
		ENDIF
	ENDIF
	SELECT (nSelectedWorkArea)
	
	RETURN sValuesList
ENDFUNC


PROCEDURE splitRecordsTst()
	*SET STEP ON 
	*DO splitRecords WITH 'C:\INS\SIVIGILA\PACIENTE','COD_ASE', 'Año="2018"'
	DO splitRecords WITH 'C:\INS\SIVIGILA\PACIENTE','LEFT(COD_PRE,2)', 'Año="2018"', 'COD_DPTO_NOTIFICADOR'
ENDPROC


PROCEDURE fieldToListTst()
	SET STEP ON 
	nRecords = 0
	
	*?fieldToList('pri_nom', '.\paciente', 'año="2020"', 'fec_aju', @nRecords)
	?fieldToList('pri_nom', '.\paciente', 'año="yyyy"', 'fec_aju', @nRecords)
	?'Longitud de la lista = ' + ALLTRIM(STR(nRecords))
ENDPROC 





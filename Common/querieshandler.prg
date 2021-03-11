#INCLUDE DevEnvironment.h


FUNCTION matchFields

	LPARAMETERS sFieldsList1 As String,  sQualifier1 As String, sFieldsList2 As String,  sQualifier2 As String, ;
				sSeparatorChar As String, sLinkerStr As String

	*Sean sFieldsList1 = FieldName_11,...,FieldName_1n y sFieldsList2 = FieldName_21,...,FieldName_2n listas de campos ;
	y sean sQualifier1 y sQualifier2  dos cualificadores, entonces, la función retorna un matching entre las dos listas ;
	de campos de la siguiente forma: ;
		sQualifier1.FieldName_11 = sQualifier2.FieldName_21 AND ... AND sQualifier1.FieldName_1n = sQualifier2.FieldName_2n
		
	*sSeparatorChar es el caracter separador de los campos en las listas sFieldsList1 y sFieldsList2; es decir, el separador ;
	de campos en esas listas NO necesariamente tiene que ser una ","
	
	*El parámetro opcional sLinkerStr (por default = 'AND') indica cómo deben encadenarse los campos de las listas para formar;
	la expresión resultante; por tanto, permite obtener matchings NO solamente de la forma Expr_1 AND ... AND Expr_n sino ;
	expresiones encadenadas de acuerdo a lo que indique sLinkerStr 
	
   
	IF VARTYPE(sLinkerStr)='C' THEN
		IF EMPTY(sLinkerStr) THEN
			sLinkerStr = " AND "
		ENDIF
	ELSE
		sLinkerStr = " AND "
	ENDIF

	sSetProcCmd = "SET PROCEDURE TO " + PATH_TO_DEVELOPMENT_ENVIRONMENT + "\SIVIGILAUtilities ADDITIVE"
	&sSetProcCmd
    sQualifiedList1 = qualifyFields(sFieldsList1, sSeparatorChar, sQualifier1)
    sQualifiedList2 = qualifyFields(sFieldsList2, sSeparatorChar, sQualifier2)
    
    nSourceFields1=ALINES(aSourceFields1, sQualifiedList1,1,sSeparatorChar)
    nSourceFields2=ALINES(aSourceFields2, sQualifiedList2,1,sSeparatorChar)
    
    sMatchedList = ""
    IF nSourceFields1 = nSourceFields2 THEN
        For nField = 1 To nSourceFields1
            sMatchedList = sMatchedList + sLinkerStr + aSourceFields1(nField) + "=" + aSourceFields2(nField)
        Next nField
    ENDIF
        
    RETURN SUBSTR(sMatchedList ,5)
ENDFUNC

PROCEDURE matchFieldsTst

   sFieldList1 = "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB" &&"semana, año, cod_pre, cod_sub"
   sFieldList2 = "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB" &&"semana, año, cod_pre, cod_sub"
   sQualifier1 = "SEMANA" &&"Paciente"
   sQualifier2 = "Recordsource"
   
   LOCAL sResult As String

   sResult = matchFields(sFieldList1, sQualifier1, sFieldList2, sQualifier2, "+")
   ?sResult

ENDPROC



FUNCTION fullCrossTables

	LPARAMETERS table1PathAndName AS String, table2PathAndName AS String, sExcludedFields as String, sJoinType as String, ;
				sFilterCondition AS String, SQLClause AS String, sListSeparator as String
	
	*Construye y ejecuta una instrucción SQL de la forma ;
		SELECT table1Name.* FROM table1Name AS table1Name001 sJoinType JOIN table2Name AS table2Name002 ;
		ON table1Name001.Fields(1)=table1Name002.Fields(1) AND ... AND table1Name001.Fields(n1)=table1Name002.Fields(n1) ;
		WHERE sFilterCondition INTO CURSOR rsJoinedRecs READWRITE
		
	*con n1 el número de campos de la tabla table1Name que no se encuentran en la lista opcional de camos separada por espacios sExcludedFields
	
	*Retorn el número de registros afectados por la ejecución de la instrucción SQL y, en SQLClause, la instrucción construida.;
	Si  table1PathAndName o table2PathAndName no se pueden abrir o se presenta un fallo inesperado al ejecutar la instrucción SQL, retorna -1;
	
	
	LOCAL table1Name as String, table2Name as String
	LOCAL nRecordsProcessed as Number 

	currentWHEREComplexity=SYS(3055)
	=SYS(3055,960)

	sCmd="SET PROCEDURE TO '" + PATH_TO_COMMON_LIB + "TablesHandler' ADDITIVE"
	&sCmd

	IF VARTYPE(sListSeparator)!='C' THEN
		sListSeparator = ' '
	ENDIF
	
	table1Name = JUSTSTEM(table1PathAndName)
	table2Name = JUSTSTEM(table2PathAndName)
	IF UseTable(table1PathAndName,table1Name + '001') AND useTable(table2PathAndName,table2Name + '002') THEN
	
		nTable1Fields=AFIELDS(aTable1Fields, (table1Name) + '001')
		nTable2Fields=AFIELDS(aTable2Fields, (table2Name) + '002')
		
		IF VARTYPE(sExcludedFields)!='C' THEN
			sExcludedFields = ''
		ENDIF
			
		IF VARTYPE(sFilterCondition)!='C' THEN
			sFilterCondition = '.T.'
		ENDIF

		SQLClause='SELECT L.* FROM ' + table1Name + '001' + ' AS L ' + sJoinType + ' JOIN ' + table2Name + '002 AS R ON '
		sJoinCondition=''
		FOR nField=1 TO nTable1Fields
			IF sListSeparator = ' ' THEN
				IF !(aTable1Fields(nField,1) $ sExcludedFields) THEN			
					IF ASCAN(aTable2Fields,aTable1Fields(nField,1),-1,-1,-1,15)>0 THEN
						sJoinCondition=sJoinCondition + 'L.' + aTable1Fields(nField,1) + '=' + 'R.'  + aTable1Fields(nField,1) + ' AND '
					ENDIF
				ENDIF
			ELSE
				IF !(sListSeparator + aTable1Fields(nField,1) + sListSeparator $ sListSeparator + sExcludedFields + sListSeparator) THEN			
					IF ASCAN(aTable2Fields,aTable1Fields(nField,1),-1,-1,-1,15)>0 THEN
						sJoinCondition=sJoinCondition + 'L.' + aTable1Fields(nField,1) + '=' + 'R.'  + aTable1Fields(nField,1) + ' AND '
					ENDIF
				ENDIF
			ENDIF			
		NEXT
		SQLClause=SQLClause + LEFT(sJoinCondition,LEN(sJoinCondition)- 4) + " WHERE " + sFilterCondition + " INTO CURSOR rsJoinedRecs READWRITE"
	
		TRY
			&SQLClause 
			nRecordsProcessed=_TALLY
		CATCH TO oException
			nRecordsProcessed=-1
			EXIT
		ENDTRY
	ELSE
		nRecordsProcessed = -1
	ENDIF
	=SYS(3055,currentWHEREComplexity)	
	
	RETURN nRecordsProcessed 
ENDFUNC


FUNCTION fullCrossTables2

	LPARAMETERS table1PathAndName AS String, table2PathAndName AS String, sIncludedFields as String, sJoinType as String, ;
				sFilterCondition AS String, SQLClause AS String, sResultsCursorName as String
	
	*Construye y ejecuta una instrucción SQL de la forma ;
		SELECT table1Name.* FROM table1Name AS table1Name001 sJoinType JOIN table2Name AS table2Name002 ;
		ON table1Name001.Fields(1)=table1Name002.Fields(1) AND ... AND table1Name001.Fields(n1)=table1Name002.Fields(n1) ;
		WHERE sFilterCondition INTO CURSOR rsJoinedRecs READWRITE
		
	*con n1 el número de campos de la tabla table1Name que se encuentran en la lista de camos separada por comas sIncludedFields
	
	*Retorna el número de registros afectados por la ejecución de la instrucción SQL y, en SQLClause, la instrucción construida.;
	Si  table1PathAndName o table2PathAndName no se pueden abrir o se presenta un fallo inesperado al ejecutar la instrucción SQL, retorna -1;
	
	
	LOCAL table1Name as String, table2Name as String
	LOCAL nRecordsProcessed as Number 
	LOCAL sOldProc as String
	
	currentWHEREComplexity=SYS(3055)
	=SYS(3055,960)
	
	sOldProc = SET("Procedure")
	sCmd="SET PROCEDURE TO '" + PATH_TO_COMMON_LIB + "TablesHandler'"
	&sCmd

	table1Name = JUSTSTEM(table1PathAndName)
	table2Name = JUSTSTEM(table2PathAndName)
	sTable1Alias = ''
	sTable2Alias = ''
	IF useTable(table1PathAndName,table1Name + '001', @sTable1Alias) AND useTable(table2PathAndName,table2Name + '002', @sTable2Alias) THEN
	
		nTable1Fields=AFIELDS(aTable1Fields, sTable1Alias)
		nTable2Fields=AFIELDS(aTable2Fields, sTable2Alias)
	
		IF VARTYPE(sIncludedFields)!='C' THEN
			sIncludedFields = ''
		ENDIF
			
		IF VARTYPE(sFilterCondition)!='C' THEN
			sFilterCondition = '.T.'
		ENDIF

		SQLClause='SELECT L.* FROM ' + sTable1Alias + ' AS L ' + sJoinType + ' JOIN ' + sTable2Alias + ' AS R ON '
		sJoinCondition=''
		FOR nField=1 TO nTable1Fields
			IF (',' + aTable1Fields(nField,1) + ',' $ ',' + sIncludedFields + ',') THEN			
				IF ASCAN(aTable2Fields,aTable1Fields(nField,1),-1,-1,-1,15)>0 THEN
					sJoinCondition=sJoinCondition + 'L.' + aTable1Fields(nField,1) + '=' + 'R.'  + aTable1Fields(nField,1) + ' AND '
				ENDIF
			ENDIF
		NEXT
		SQLClause=SQLClause + LEFT(sJoinCondition,LEN(sJoinCondition)- 4) + " WHERE " + sFilterCondition + " INTO CURSOR " + sResultsCursorName + " READWRITE"
	
		TRY
			&SQLClause 
			nRecordsProcessed=_TALLY
		CATCH TO oException
			nRecordsProcessed=-1
			EXIT
		ENDTRY
	ELSE
		nRecordsProcessed = -1
	ENDIF
	=SYS(3055,currentWHEREComplexity)	
	
	SET PROCEDURE TO &sOldProc
	RETURN nRecordsProcessed 
ENDFUNC


PROCEDURE fullCrossTablesTst

	#DEFINE sEXCLUDED_FIELDS " PERIODO FLU_NOT INMEDIATA NOTIFICA SEM_REC " 

	LOCAL nRecordsProcessed as Number
	LOCAL sSQLCmd as String
	
	*SET STEP ON 
	sSQLCmd = ''
	nRecordsProcessed = fullCrossTables("C:\INS\SIVIGILA\PACIENTE", ;
										"C:\Users\wilson.aguilar\Documents\ProyectoSivigila\SoporteUsuarios\MaterialAsistenciasTecnicas2014\Cesar\29_05_2014_BackupSivigila\PACIENTE", ;
										sEXCLUDED_FIELDS, "LEFT OUTER", "R.COD_EVE IS NULL",@sSQLCmd)

ENDPROC


PROCEDURE crossTables(table1PathAndName AS String, table2PathAndName AS String, sIncludedFields as String, sJoinType as String, ;
					sFilterCondition AS String, bExtractRECNO AS Boolean, SQLClause as String)
	
	*Construye una instrucción SQL de la forma ;
		SELECT table1Name.* FROM table1Name sJoinType JOIN table2Name ;
		ON table1Name.Fields(1)=table2Name.Fields(1) AND ... AND table1Name.Fields(n1)=table2Name.Fields(n1) ;
		WHERE sFilterCondition
		
	*con n1 el número de campos de la tabla table1Name que se encuentran en la lista de camos separada por comas sIncludedFields.;
	Si se pasa el parámetro opcional bExtractRECNO  y es .T. la instrcción SQL será de la forma SELECT table1Name.*, RECNO() as nReg FROM table1Name...
	
	*En SQLClause retorna la instrucción construida.
	
	LOCAL table1Name AS String, table2Name AS String
	
	DIMENSION aTable1Fields(1)

	table1Name = JUSTSTEM(table1PathAndName)
	table2Name = JUSTSTEM(table2PathAndName)
	
	nTable1Fields=ALINES(aTable1Fields, sIncludedFields, 1+4+8, ',') 

	IF VARTYPE(sFilterCondition)!='C' THEN
		sFilterCondition = '.T.'
	ENDIF

	SQLClause='SELECT ' + table1Name + '.* FROM '
	IF bExtractRECNO THEN
		SQLClause = SQLClause + ' (SELECT *,RECNO() AS nReg FROM "' + table1PathAndName + '" ) AS '
	ELSE
		SQLClause= SQLClause + '"' + table1PathAndName + '" AS ' 
	ENDIF
	SQLClause = SQLClause + table1Name + ' ' + sJoinType + ' JOIN "' + table2PathAndName + '" AS ' + table2Name + ' ON '
	sJoinCondition = ''
	FOR nField=1 TO nTable1Fields
		sJoinCondition = sJoinCondition + table1Name + '.' + aTable1Fields(nField) + '=' + table2Name + '.'  + aTable1Fields(nField) + ' AND '
	NEXT
	SQLClause=SQLClause + LEFT(sJoinCondition,LEN(sJoinCondition)- 4) + " WHERE " + sFilterCondition 
	
ENDFUNC


PROCEDURE crossTablesTst

	LOCAL SQLClause
	
	*SET STEP ON 
	CLEAR
	DO crossTables WITH 'UPGD', 'PLANO_UPGD', 'COD_PRE,COD_SUB', 'LEFT OUTER', 'PLANO_UPGD.COD_PRE IS NULL', .T. , SQLClause
	*DO crossTables WITH 'UPGD', 'PLANO_UPGD', 'COD_PRE,COD_SUB', 'INNER', 'PLANO_UPGD.COD_PRE IS NULL', , SQLClause
	?SQLClause
	&SQLClause
ENDPROC


FUNCTION concatFields(sFieldsList1 As String,  sQualifier1 As String, sFieldsList2 As String,  sQualifier2 As String, ;
						sSeparatorChar As String)

	*Sean sFieldsList1 = FieldName_11,...,FieldName_1n y sFieldsList2 = FieldName_21,...,FieldName_2n listas de campos ;
	y sean sQualifier1 y sQualifier2  dos cualificadores, entonces, la función retorna un matching entre las dos listas ;
	de campos de la siguiente forma: ;
		sQualifier1.FieldName_11 + ... + sQualifier1.FieldName_1n = sQualifier2.FieldName_21 + ... + sQualifier2.FieldName_2n
		
	*sSeparatorChar es el caracter separador de los campos en las listas sFieldsList1 y sFieldsList2; es decir, el separador ;
	de campos en esas listas NO necesariamente tiene que ser una ","
	
   
	sSetProcCmd = "SET PROCEDURE TO " + PATH_TO_DEVELOPMENT_ENVIRONMENT + "\SIVIGILAUtilities ADDITIVE"
	&sSetProcCmd
    sQualifiedList1 = qualifyFields(sFieldsList1, sSeparatorChar, sQualifier1)
    sQualifiedList2 = qualifyFields(sFieldsList2, sSeparatorChar, sQualifier2)
    
    sMatchedList = STRTRAN(sQualifiedList1 ,sSeparatorChar,'+') + " = " +  STRTRAN(sQualifiedList2,sSeparatorChar,'+')
        
    RETURN sMatchedList
ENDFUNC

PROCEDURE ConcatFieldsTst

   sFieldList1 = "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB" &&"semana, año, cod_pre, cod_sub"
   sFieldList2 = "AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB" &&"semana, año, cod_pre, cod_sub"
   sQualifier1 = "Paciente"
   sQualifier2 = "Recordsource"
   
   LOCAL sResult As String

   sResult = ConcatFields(sFieldList1, sQualifier1, sFieldList2, sQualifier2, '+')
   ?sResult

ENDPROC


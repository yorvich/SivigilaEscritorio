#INCLUDE AnomaliesSolver.h

DEFINE CLASS AnomaliesSolver as Custom

#DEFINE MAX_FILE_SIZE 1000000000
*Máximo tamaño de archivo que puede tratarse para extracción de duplicados sin necesidad de particionarlo

*This next one is for testing purposes
*#DEFINE MAX_FILE_SIZE 500000000

sSourceTableName=.NULL.
sDefaultKey = .NULL.

bIsCursor = .F.
*Indica si sSourceTableName es un CURSOR. En caso de que así sea, nunca se hará eliminación de duplicados particionando el cursor y, debido;
a su naturaleza, la eliminación de registros solo podrá ser lógica; en caso contrario, si el tamaño de la tabla fuente es superior a ;
MAX_FILE_SIZE, ella se particionará y se eliminarán duplicados en cada partición.

sClusterExpr = "SEMANA"
* Lista de campos separada por comas que sirven como criterios de agrupación. No se admiten expresiones que invlocren campos

sExcludedFields = " NOTIFICA FLU_NOT INMEDIATA PERIODO SEM_REC FECHACARGA ESTADOTRAN VERSION INGR_X_NI FEC_INICAR UNI_MODIF " +; && Campos de control
					"EST_INGR EST_NOTIF " +; && Campos de control
					"PAIS MALNUTRI  LAT_DIR LONG_DIR CONFGEODIR " +;
					"INGR_X_RA FEC_REACTI COD_UNI_SA TIPO_DONAN TIPO_PRUEB " +; && Campos deprecados
					"VEREDA NOM_DIL_FI PAIS BAR_VER DIR_RES " && Campos tipo Memo
					
sXlsFilePath = '.'


PROTECTED nInitRecs
nInitRecs = .NULL.

nDeletedRecs = 0
ErrorMessage = .NULL.
 

FUNCTION Init
	LPARAMETERS _sSourceTableName as String, _bIsCursor as Boolean 

	IF VARTYPE(_sSourceTableName)='C' THEN
		This.sSourceTableName=_sSourceTableName
	ENDIF
	
	IF VARTYPE(_bIsCursor)='L' THEN
		This.bIsCursor = _bIsCursor 
	ENDIF 
ENDFUNC


FUNCTION Destroy
	This.sSourceTableName=.NULL.
	This.sDefaultKey = .NULL.	
	This.nInitRecs = .NULL.
ENDFUNC


PROCEDURE deleteDuplicates

	LPARAMETERS bUseDefaultKey as Boolean, sSolverProcedure as String

	*Borra de la tabla sSourceTableName registros duplicados o repetidos.
	
	*Si bUseDefaultKey=.T., en la detección de duplicados se utiliza la clave por defecto establecida ;
	por la función setDefaultKey para sSourceFileNameAndPath; en caso contrario, se utilizan todos ;
	los campos de sSourceFileNameAndPath excepto los establecidos en This.sExcludedFields 

	*Si se pasa el parámetro opcional sSolverProcedure, los registros duplicados se resuelven aplicando ;
	ese procedimiento; en caso contrario, se resuelven eliminando todos los duplicados excepto uno ;
	seleccionado de manera arbitraria. ;
	sSolverProedure debe debe tener la siguiente interfaz: ;
	 	LPARAMETERS sSourceTableName as String, sRecordsToDeleteRSName as String, sDuplicatesCountingRSName as String
	
	LOCAL bProceedByDefault as Boolean
	bProceedByDefault = .T.
	
	IF !This.bIsCursor THEN 
		sOldCompatible = SET("Compatible")
		SET COMPATIBLE ON 
		nSourceFileSize = FSIZE(This.sSourceTableName + '.DBF')
		SET COMPATIBLE &sOldCompatible
	
		IF nSourceFileSize > MAX_FILE_SIZE THEN 
			bProceedByDefault = .F.
		ENDIF 
	ENDIF 
	
	IF !bProceedByDefault THEN 
		This.deleteDuplicatesByCluster(.F.,This.sClusterExpr)
	ELSE 
		sOldSetDeleted=SET("Deleted")
		SET DELETED ON
		currentWHEREComplexity=SYS(3055)
		=SYS(3055,960)
		
		bSourceTableWasOpen=.F.
		This.ErrorMessage=''
		bSourceTableIsOpenExclusively = .T.
		IF !USED(JUSTFNAME(This.sSourceTableName)) THEN
			TRY
				USE (This.sSourceTableName) EXCLUSIVE IN 0
				SELECT (JUSTFNAME(This.sSourceTableName))
			CATCH TO oException
				bSourceTableIsOpenExclusively = .F.
			ENDTRY
		ELSE
			SELECT (JUSTFNAME(This.sSourceTableName))
			bSourceTableWasOpen=.T.
			bSourceTableIsOpenExclusively = ISEXCLUSIVE(JUSTFNAME(This.sSourceTableName))
		ENDIF
		This.sSourceTableName = JUSTFNAME(This.sSourceTableName)
*SET STEP ON 
		IF !bSourceTableIsOpenExclusively THEN
			This.ErrorMessage="La tabla objetivo no está abierta o no se pudo abrir en forma exclusiva." + CHR(13) + CHR(10) + ;
								"Si está en una red, debe ejecutar este procedimiento de depuración en"	+ CHR(13) + CHR(10) + ;
								" el servidor y ningún usuario debe tener abierta la tabla objetivo o el sistema"
		ELSE
			IF bUseDefaultKey THEN
				* Utiliza los campos de la clave por defecto de sSourceTableName
				= This.setDefaultKey()
				DIMENSION SourceTableFields(1,1)
				nSourceTableFields=ALINES(SourceTableFields,This.sDefaultKey,1,',')
				DIMENSION SourceTableFields(nSourceTableFields,1)
			ELSE
				* Utiliza todos los campos de sSourceTableName excepto los incluidos en sExcludedFields 
				This.sDefaultKey = ''
				nSourceTableFields=AFIELDS(SourceTableFields)
				FOR nField=1 TO nSourceTableFields
					IF !(SourceTableFields(nField,1) $ This.sExcludedFields) AND SourceTableFields(nField, 2) != 'M' THEN
						This.sDefaultKey = This.sDefaultKey + SourceTableFields(nField,1) + ","
					ENDIF
				NEXT nField
				This.sDefaultKey = SUBSTR(This.sDefaultKey,1,LEN(This.sDefaultKey)-1)
			ENDIF

			*Establece si en sSourceTableName existen registros duplicados según la clave establecida para tales efectos
			WAIT "Estableciendo la cantidad de registros duplicados" WINDOW NOWAIT
			sDuplicatesCursorName=''
			This.nInitRecs = RECCOUNT(This.sSourceTableName )
			This.nDeletedRecs=0

			detectionResult = This.extractDupsAndNotDups()
			IF detectionResult!=0 THEN
				nDuplicatesCases=RECCOUNT("C_COUNT_DUPS")
			
				*Construye un cursor con los números de los registros que son suceptibles de ser eliminados. ;
				Inicialmente supone que todos los registros duplicados deberían eliminarse.
				SQLClause="SELECT A.* FROM (SELECT RECNO() AS nReg, .T. as BORRAR,* FROM " + This.sSourceTableName + ") AS A INNER JOIN C_COUNT_DUPS B ON " 
				sJoinCondition='.T.'
				FOR nField=1 TO nSourceTableFields
					IF !(SourceTableFields(nField,1) $ This.sExcludedFields) THEN
						sJoinCondition=sJoinCondition + " AND A." + SourceTableFields(nField,1) + "=" + ;
										"B."  + SourceTableFields(nField,1)
					ENDIF
				NEXT nField
				*SQLClause=SQLClause + sJoinCondition + " ORDER BY " + This.qualifyFields(This.sDefaultKey, ",", "A" ) + ;
							" INTO CURSOR rsRecordsToDelete READWRITE"
				SQLClause=SQLClause + sJoinCondition + " ORDER BY A." + STRTRAN(This.sDefaultKey,",",",A.") + " INTO CURSOR rsRecordsToDelete READWRITE"
				&SQLClause
			

				IF VARTYPE(sSolverProcedure)='C' THEN
					*Intenta resolver los registros duplicados aplicando sSolverProcedure
					DO &sSolverProcedure WITH (This.sSourceTableName), "rsRecordsToDelete", "C_COUNT_DUPS"

				ELSE
					*Intenta resolver los registros duplicados teniendo en cuenta la igualdad de todas las variables incluidas ;
					en la clave del registro definida por commaSeparatedKey . ;
					Según este criterio, si todas esas variables son iguales dentro del conjunto de duplicados, cualquiera de ellos;
					se escoje como registro válido
					SELECT rsRecordsToDelete
					nCase=0
					SCAN
						nCase=nCase+1
						sProcessingMsg="Resolviendo " + ALLTRIM(STR(nCase)) + " de " + ALLTRIM(STR(nDuplicatesCases)) + " casos"
						WAIT (sProcessingMsg) WINDOW NOWAIT
						
						REPLACE BORRAR WITH .F.
						nRecordsToSkip=C_COUNT_DUPS.Counter
						SKIP nRecordsToSkip - 1
						SKIP 1 IN C_COUNT_DUPS
						
					ENDSCAN
					WAIT CLEAR 
				ENDIF
				
				*Borra de sSourceTableName los registros marcados para borrar en rsRecordsToDelete
				sProcessingMsg="Eliminando los registros duplicados"
				WAIT (sProcessingMsg) WINDOW NOWAIT

				SELECT rsRecordsToDelete
				INDEX on nReg TAG nRegIdx

				SELECT (This.sSourceTableName)
				SET RELATION TO RECNO() INTO rsRecordsToDelete
				DELETE ALL FOR rsRecordsToDelete.BORRAR
				This.nDeletedRecs = _TALLY

				*Exporta el conjunto de registros depurado hacia un archivo excel
				IF This.nDeletedRecs>0 THEN
					This.exportToXLS(This.sSourceTableName,"rsRecordsToDelete","_DepuracionDuplicados", "Por duplicado")
				ENDIF

				*Borra físicamente de sSourceTableName los registros maracados para borrar  en rsRecordsToDelete
				SELECT (This.sSourceTableName)
				IF !This.bIsCursor THEN 
					PACK
				ENDIF 
				SET RELATION TO

				*Cierra/Elimina los objetos temporales generados durante el procesamiento
				SELECT rsRecordsToDelete
				USE
			ENDIF

			*Cierra/Elimina los objetos temporales generados durante el procesamiento
			IF !bSourceTableWasOpen THEN
				SELECT (This.sSourceTableName)
				USE
			ENDIF
			SELECT C_Count_Dups				
			USE

			=SYS(currentWHEREComplexity)
			SET DELETED &sOldSetDeleted
			WAIT CLEAR
			*RETURN nDuplicates
		ENDIF
	ENDIF
ENDPROC


PROCEDURE deleteDuplicatesByCluster

	LPARAMETERS bUseDefaultKey as Boolean, sClusterExpr as String, sSolverProcedure as String

	*Borra de la tabla sSourceTableName registros duplicados o repetidos.
	
	*Si bUseDefaultKey=.T., en la detección de duplicados se utiliza la clave por defecto establecida ;
	por la función setDefaultKey para sSourceFileNameAndPath; en caso contrario, se utilizan todos ;
	los campos de sSourceFileNameAndPath excepto los establecidos en This.sExcludedFields 

	*Si se pasa el parámetro opcional sSolverProcedure, los registros duplicados se resuelven aplicando ;
	ese procedimiento; en caso contrario, se resuelven eliminando todos los duplicados excepto uno ;
	seleccionado de manera arbitraria. ;
	sSolverProedure debe debe tener la siguiente interfaz: ;
	 	LPARAMETERS sSourceTableName as String, sRecordsToDeleteRSName as String, sDuplicatesCountingRSName as String

	
	sOldSetDeleted=SET("Deleted")
	SET DELETED ON
	currentWHEREComplexity=SYS(3055)
	=SYS(3055,960)

	bSourceTableWasOpen=.F.
	This.ErrorMessage=''
	bSourceTableIsOpenExclusively = .T.
	IF !USED(JUSTFNAME(This.sSourceTableName)) THEN
		TRY
			USE (This.sSourceTableName) EXCLUSIVE IN 0
			SELECT (JUSTFNAME(This.sSourceTableName))
		CATCH TO oException
			bSourceTableIsOpenExclusively = .F.
		ENDTRY
	ELSE
		SELECT (JUSTFNAME(This.sSourceTableName))
		bSourceTableWasOpen=.T.
		bSourceTableIsOpenExclusively = ISEXCLUSIVE(JUSTFNAME(This.sSourceTableName))
	ENDIF
	This.sSourceTableName = JUSTFNAME(This.sSourceTableName)

	IF !bSourceTableIsOpenExclusively THEN
		This.ErrorMessage="La tabla objetivo no está abierta o no se pudo abrir en forma exclusiva." + CHR(13) + CHR(10) + ;
							"Si está en una red, debe ejecutar este procedimiento de depuración en"	+ CHR(13) + CHR(10) + ;
							" el servidor y ningún usuario debe tener abierta la tabla objetivo o el sistema"
	ELSE
		IF bUseDefaultKey THEN
			* Utiliza los campos de la clave por defecto de sSourceTableName
			= This.setDefaultKey()
			DIMENSION SourceTableFields(1,1)
			nSourceTableFields=ALINES(SourceTableFields,This.sDefaultKey,1,',')
			DIMENSION SourceTableFields(nSourceTableFields,1)
		ELSE
			* Utiliza todos los campos de sSourceTableName excepto los incluidos en sExcludedFields 
			This.sDefaultKey = ''
			nSourceTableFields=AFIELDS(SourceTableFields)
			FOR nField=1 TO nSourceTableFields
				IF !(SourceTableFields(nField,1) $ This.sExcludedFields)  THEN
					This.sDefaultKey = This.sDefaultKey + SourceTableFields(nField,1) + ","
				ENDIF
			NEXT nField
			This.sDefaultKey = SUBSTR(This.sDefaultKey,1,LEN(This.sDefaultKey)-1)
		ENDIF

		*Establece si en sSourceTableName existen registros duplicados según la clave establecida para tales efectos
		sDuplicatesCursorName=''
		This.nInitRecs = RECCOUNT(This.sSourceTableName )
		This.nDeletedRecs = 0
		nDuplicatesCases = 0
*SET STEP ON 

		WAIT "Estableciendo grupos"  WINDOW NOWAIT
		SELECT &sClusterExpr AS IdGrupo, COUNT(*) AS N FROM (This.sSourceTableName) GROUP BY IdGrupo HAVING n>1 ORDER BY n DESC INTO CURSOR rsClusters
		SELECT rsClusters
		SCAN
			WAIT "Estableciendo la cantidad de registros duplicados para el grupo " + (rsClusters.IdGrupo) WINDOW NOWAIT
			
			*Establece cuáles son los registros duplicados del grupo rsClusters.IdGrupo aislándolos en un recordset temporal (sTmpWorkCursor)
			sFilter = sClusterExpr + '="' + rsClusters.IdGrupo + '"'
			sTmpWorkCursor = 'rs' + STRTRAN(rsClusters.IdGrupo, ' ','_')
			SQLClause = 'SELECT * FROM ' + This.sSourceTableName + ' WHERE ' + sClusterExpr + '="' + rsClusters.IdGrupo + '"' +;
						' INTO CURSOR ' + sTmpWorkCursor 
			&SQLClause
			sFormerSourceTableName = This.sSourceTableName
			This.sSourceTableName = sTmpWorkCursor 
			detectionResult = This.extractDupsAndNotDups()
			This.sSourceTableName = sFormerSourceTableName 
			
			IF detectionResult!=0 THEN
				*Resuelve los registros duplicados
				
				nDuplicatesCases = nDuplicatesCases +RECCOUNT("C_COUNT_DUPS")
			
				*Construye un cursor con los números de los registros que son suceptibles de ser eliminados. ;
				Inicialmente supone que todos los registros duplicados deberían eliminarse.
				IF EMPTY(sClusterExpr) THEN 
					SQLClause="SELECT A.* FROM (SELECT RECNO() AS nReg, .T. as BORRAR,* FROM " + This.sSourceTableName + ") AS A INNER JOIN C_COUNT_DUPS B ON " 
				ELSE
					SQLClause="SELECT A.* FROM (SELECT RECNO() AS nReg, .T. as BORRAR," + This.sDefaultKey + " FROM " + This.sSourceTableName + " WHERE " +;
								sClusterExpr + "='" + rsClusters.IdGrupo + "') AS A INNER JOIN C_COUNT_DUPS B ON " 
				ENDIF
				sJoinCondition='.T.'
				FOR nField=1 TO nSourceTableFields
					IF !(SourceTableFields(nField,1) $ This.sExcludedFields) THEN
						sJoinCondition=sJoinCondition + " AND A." + SourceTableFields(nField,1) + "=" + ;
										"B."  + SourceTableFields(nField,1)
					ENDIF
				NEXT nField
				SQLClause=SQLClause + sJoinCondition + " ORDER BY A." + STRTRAN(This.sDefaultKey,",",",A.") + " INTO CURSOR rsRecordsToDelete READWRITE"
				&SQLClause
			

				IF VARTYPE(sSolverProcedure)='C' THEN
					*Intenta resolver los registros duplicados aplicando sSolverProcedure
					DO &sSolverProcedure WITH (This.sSourceTableName), "rsRecordsToDelete", "C_COUNT_DUPS"

				ELSE
					*Intenta resolver los registros duplicados teniendo en cuenta la igualdad de todas las variables incluidas ;
					en la clave del registro definida por commaSeparatedKey . ;
					Según este criterio, si todas esas variables son iguales dentro del conjunto de duplicados, cualquiera de ellos;
					se escoje como registro válido
					SELECT rsRecordsToDelete
					nCase=0
					SCAN
						nCase=nCase+1
						sProcessingMsg="Resolviendo " + ALLTRIM(STR(nCase)) + " de " + ALLTRIM(STR(nDuplicatesCases)) + " casos"
						WAIT (sProcessingMsg) WINDOW NOWAIT
						
						REPLACE BORRAR WITH .F.
						nRecordsToSkip=C_COUNT_DUPS.Counter
						SKIP nRecordsToSkip - 1
						SKIP 1 IN C_COUNT_DUPS
						
					ENDSCAN
					WAIT CLEAR 
				ENDIF
				
				*Borra de sSourceTableName los registros marcados para borrar
				sProcessingMsg="Eliminando los registros duplicados"
				WAIT (sProcessingMsg) WINDOW NOWAIT

				SELECT rsRecordsToDelete
				INDEX on nReg TAG nRegIdx

				SELECT (This.sSourceTableName)
				SET RELATION TO RECNO() INTO rsRecordsToDelete
				DELETE ALL FOR rsRecordsToDelete.BORRAR
				This.nDeletedRecs = This.nDeletedRecs + _TALLY

				*Exporta el conjunto de registros depurado hacia un archivo excel
				IF This.nDeletedRecs>0 THEN
					This.exportToXLS(This.sSourceTableName,"rsRecordsToDelete","_DepuracionDuplicados_G" + ALLTRIM(rsClusters.IdGrupo), "Por duplicado")
				ENDIF
				
				*Cierra/Elimina los objetos temporales generados durante el procesamiento
				USE IN rsRecordsToDelete
			ENDIF
			USE IN (sTmpWorkCursor)
		ENDSCAN
		USE IN rsClusters
		
		*Borra físicamente de sSourceTableName los registros maracados para borrar  en rsRecordsToDelete
		SELECT (This.sSourceTableName)
		SET RELATION TO
		PACK

		*Cierra/Elimina los objetos temporales generados durante el procesamiento
		IF !bSourceTableWasOpen THEN
			SELECT (This.sSourceTableName)
			USE
		ENDIF
		USE IN C_Count_Dups				

		=SYS(currentWHEREComplexity)
		SET DELETED &sOldSetDeleted
		WAIT CLEAR
	ENDIF
ENDPROC

PROCEDURE deleteDuplicatesByCluster_v00

	LPARAMETERS bUseDefaultKey as Boolean, sClusterExpr as String, sSolverProcedure as String

	*Borra de la tabla sSourceTableName registros duplicados o repetidos.
	
	*Si bUseDefaultKey=.T., en la detección de duplicados se utiliza la clave por defecto establecida ;
	por la función setDefaultKey para sSourceFileNameAndPath; en caso contrario, se utilizan todos ;
	los campos de sSourceFileNameAndPath excepto los establecidos en This.sExcludedFields 

	*Si se pasa el parámetro opcional sSolverProcedure, los registros duplicados se resuelven aplicando ;
	ese procedimiento; en caso contrario, se resuelven eliminando todos los duplicados excepto uno ;
	seleccionado de manera arbitraria. ;
	sSolverProedure debe debe tener la siguiente interfaz: ;
	 	LPARAMETERS sSourceTableName as String, sRecordsToDeleteRSName as String, sDuplicatesCountingRSName as String

	
	sOldSetDeleted=SET("Deleted")
	SET DELETED ON
	currentWHEREComplexity=SYS(3055)
	=SYS(3055,960)

	bSourceTableWasOpen=.F.
	This.ErrorMessage=''
	bSourceTableIsOpenExclusively = .T.
	IF !USED(JUSTFNAME(This.sSourceTableName)) THEN
		TRY
			USE (This.sSourceTableName) EXCLUSIVE IN 0
			SELECT (JUSTFNAME(This.sSourceTableName))
		CATCH TO oException
			bSourceTableIsOpenExclusively = .F.
		ENDTRY
	ELSE
		SELECT (JUSTFNAME(This.sSourceTableName))
		bSourceTableWasOpen=.T.
		bSourceTableIsOpenExclusively = ISEXCLUSIVE(JUSTFNAME(This.sSourceTableName))
	ENDIF
	This.sSourceTableName = JUSTFNAME(This.sSourceTableName)
*SET STEP ON 
	IF !bSourceTableIsOpenExclusively THEN
		This.ErrorMessage="La tabla objetivo no está abierta o no se pudo abrir en forma exclusiva." + CHR(13) + CHR(10) + ;
							"Si está en una red, debe ejecutar este procedimiento de depuración en"	+ CHR(13) + CHR(10) + ;
							" el servidor y ningún usuario debe tener abierta la tabla objetivo o el sistema"
	ELSE
		IF bUseDefaultKey THEN
			* Utiliza los campos de la clave por defecto de sSourceTableName
			= This.setDefaultKey()
			DIMENSION SourceTableFields(1,1)
			nSourceTableFields=ALINES(SourceTableFields,This.sDefaultKey,1,',')
			DIMENSION SourceTableFields(nSourceTableFields,1)
		ELSE
			* Utiliza todos los campos de sSourceTableName excepto los incluidos en sExcludedFields 
			This.sDefaultKey = ''
			nSourceTableFields=AFIELDS(SourceTableFields)
			FOR nField=1 TO nSourceTableFields
				IF !(SourceTableFields(nField,1) $ This.sExcludedFields)  THEN
					This.sDefaultKey = This.sDefaultKey + SourceTableFields(nField,1) + ","
				ENDIF
			NEXT nField
			This.sDefaultKey = SUBSTR(This.sDefaultKey,1,LEN(This.sDefaultKey)-1)
		ENDIF

		*Establece si en sSourceTableName existen registros duplicados según la clave establecida para tales efectos
		sDuplicatesCursorName=''
		This.nInitRecs = RECCOUNT(This.sSourceTableName )
		This.nDeletedRecs = 0
		nDuplicatesCases = 0
*SET STEP ON 
		WAIT "Estableciendo grupos"  WINDOW NOWAIT
		*SELECT &sClusterExpr, COUNT(*) AS N FROM (This.sSourceTableName) GROUP BY &sClusterExpr HAVING n>1 ORDER BY n DESC INTO CURSOR rsClusters
		SELECT &sClusterExpr AS IdGrupo, COUNT(*) AS N FROM (This.sSourceTableName) GROUP BY IdGrupo HAVING n>1 ORDER BY n DESC INTO CURSOR rsClusters
		SELECT rsClusters
		SCAN
			WAIT "Estableciendo la cantidad de registros duplicados para el grupo " + (rsClusters.IdGrupo) WINDOW NOWAIT
			
			sFilter = sClusterExpr + '="' + rsClusters.IdGrupo + '"'
			detectionResult = This.extractDupsAndNotDups(sFilter)
			IF detectionResult!=0 THEN
				nDuplicatesCases = nDuplicatesCases +RECCOUNT("C_COUNT_DUPS")
			
				*Construye un cursor con los números de los registros que son suceptibles de ser eliminados. ;
				Inicialmente supone que todos los registros duplicados deberían eliminarse.
				IF EMPTY(sClusterExpr) THEN 
					SQLClause="SELECT A.* FROM (SELECT RECNO() AS nReg, .T. as BORRAR,* FROM " + This.sSourceTableName + ") AS A INNER JOIN C_COUNT_DUPS B ON " 
				ELSE
					SQLClause="SELECT A.* FROM (SELECT RECNO() AS nReg, .T. as BORRAR," + This.sDefaultKey + " FROM " + This.sSourceTableName + " WHERE " +;
								sClusterExpr + "='" + rsClusters.IdGrupo + "') AS A INNER JOIN C_COUNT_DUPS B ON " 
				ENDIF
				sJoinCondition='.T.'
				FOR nField=1 TO nSourceTableFields
					IF !(SourceTableFields(nField,1) $ This.sExcludedFields) THEN
						sJoinCondition=sJoinCondition + " AND A." + SourceTableFields(nField,1) + "=" + ;
										"B."  + SourceTableFields(nField,1)
					ENDIF
				NEXT nField
				SQLClause=SQLClause + sJoinCondition + " ORDER BY A." + STRTRAN(This.sDefaultKey,",",",A.") + " INTO CURSOR rsRecordsToDelete READWRITE"
				&SQLClause
			

				IF VARTYPE(sSolverProcedure)='C' THEN
					*Intenta resolver los registros duplicados aplicando sSolverProcedure
					DO &sSolverProcedure WITH (This.sSourceTableName), "rsRecordsToDelete", "C_COUNT_DUPS"

				ELSE
					*Intenta resolver los registros duplicados teniendo en cuenta la igualdad de todas las variables incluidas ;
					en la clave del registro definida por commaSeparatedKey . ;
					Según este criterio, si todas esas variables son iguales dentro del conjunto de duplicados, cualquiera de ellos;
					se escoje como registro válido
					SELECT rsRecordsToDelete
					nCase=0
					SCAN
						nCase=nCase+1
						sProcessingMsg="Resolviendo " + ALLTRIM(STR(nCase)) + " de " + ALLTRIM(STR(nDuplicatesCases)) + " casos"
						WAIT (sProcessingMsg) WINDOW NOWAIT
						
						REPLACE BORRAR WITH .F.
						nRecordsToSkip=C_COUNT_DUPS.Counter
						SKIP nRecordsToSkip - 1
						SKIP 1 IN C_COUNT_DUPS
						
					ENDSCAN
					WAIT CLEAR 
				ENDIF
				
				*Borra de sSourceTableName los registros marcados para borrar
				sProcessingMsg="Eliminando los registros duplicados"
				WAIT (sProcessingMsg) WINDOW NOWAIT

				SELECT rsRecordsToDelete
				INDEX on nReg TAG nRegIdx

				SELECT (This.sSourceTableName)
				SET RELATION TO RECNO() INTO rsRecordsToDelete
				DELETE ALL FOR rsRecordsToDelete.BORRAR
				This.nDeletedRecs = This.nDeletedRecs + _TALLY

				*Exporta el conjunto de registros depurado hacia un archivo excel
				IF EMPTY(sClusterExpr) THEN 
					IF This.nDeletedRecs>0 THEN
						This.exportToXLS(This.sSourceTableName,"rsRecordsToDelete","_DepuracionDuplicados" + rsClusters.IdGrupo, "Por duplicado")
					ENDIF
				ENDIF
				
				*Cierra/Elimina los objetos temporales generados durante el procesamiento
				USE IN rsRecordsToDelete
			ENDIF
		ENDSCAN
		
		*Borra físicamente de sSourceTableName los registros maracados para borrar  en rsRecordsToDelete
		SELECT (This.sSourceTableName)
		SET RELATION TO
		PACK

		*Cierra/Elimina los objetos temporales generados durante el procesamiento
		IF !bSourceTableWasOpen THEN
			SELECT (This.sSourceTableName)
			USE
		ENDIF
		USE IN C_Count_Dups				

		=SYS(currentWHEREComplexity)
		SET DELETED &sOldSetDeleted
		WAIT CLEAR
	ENDIF
ENDPROC

PROCEDURE deleteMissingData

	LPARAMETERS bUseDefaultKey as Boolean, sSolverProcedure as String

	*Borra de la tabla sSourceTableName registros que tengan datos faltantes en algunos de sus campos.
	
	*Si bUseDefaultKey=.T., en la detección de registros con datos faltantes se utiliza la clave por defecto establecida ;
	por la función getDefaultKey para sSourceTableName; en caso contrario, se utilizan todos ;
	los campos de sSourceTableName excepto los establecidos en sExcludedFields 

	*Si se pasa el parámetro opcional sSolverProcedure, los registros con datos faltantes se resuelven aplicando ;
	ese procedimiento; en caso contrario, se resuelven eliminando todos los registros con datos faltantes ;
	sSolverProedure debe debe la siguiente interfaz: ;
	 	LPARAMETERS sSourceTableName as String, sRecordsToDeleteRSName as String

	
	sOldSetDeleted=SET("Deleted")
	SET DELETED ON
	currentWHEREComplexity=SYS(3055)
	=SYS(3055,640)

	bSourceTableWasOpen=.F.
	This.ErrorMessage=''
	bSourceTableIsOpenExclusively = .T.
	IF !USED(This.sSourceTableName) THEN
		TRY
			USE (This.sSourceTableName) EXCLUSIVE IN 0
			SELECT (This.sSourceTableName)
		CATCH TO oException
			bSourceTableIsOpenExclusively = .F.
		ENDTRY
	ELSE
		SELECT (This.sSourceTableName)
		bSourceTableWasOpen=.T.
		bSourceTableIsOpenExclusively = ISEXCLUSIVE(This.sSourceTableName)
	ENDIF

	IF !bSourceTableIsOpenExclusively THEN
		This.ErrorMessage="La tabla objetivo no está abierta o no se pudo abrir en forma exclusiva." + CHR(13) + CHR(10) + ;
							"Si está en una red, debe ejecutar este procedimiento de depuración en"	+ CHR(13) + CHR(10) + ;
							" el servidor y ningún usuario debe tener abierta la tabla objetivo o el sistema"
	ELSE
		IF bUseDefaultKey THEN
			* Utiliza los campos de la clave por defecto de sSourceTableName
			= This.setDefaultKey()
			DIMENSION SourceTableFields(1,1)
			nSourceTableFields=ALINES(SourceTableFields,This.sDefaultKey,1,',')
			DIMENSION SourceTableFields(nSourceTableFields,1)
		ELSE
			* Utiliza todos los campos de sSourceTableName excepto los incluidos en sExcludedFields 
			This.sDefaultKey = ''
			nSourceTableFields=AFIELDS(SourceTableFields)
			FOR nField=1 TO nSourceTableFields
				IF !(SourceTableFields(nField,1) $ This.sExcludedFields) THEN
					This.sDefaultKey = This.sDefaultKey + SourceTableFields(nField,1) + ","
				ENDIF
			NEXT nField
			This.sDefaultKey = SUBSTR(This.sDefaultKey,1,LEN(This.sDefaultKey)-1)
		ENDIF

		*Establece si en sSourceTableName existen registros con datos faltantes en alguno de los campos de la clave ;
		establecida para tales efectos
		WAIT "Estableciendo la cantidad de registros con datos faltantes" WINDOW NOWAIT
		This.nInitRecs = RECCOUNT(This.sSourceTableName )
		This.nDeletedRecs=0
		detectionResult = This.extractMissingData()
		IF detectionResult!=0 THEN
			nMissingDataCases=RECCOUNT("C_MISSING_DATA")
			rsRecordsToDelete = "C_MISSING_DATA"

			IF VARTYPE(sSolverProcedure)='C' THEN
				*Intenta resolver los registros duplicados aplicando sSolverProcedure
				DO &sSolverProcedure WITH (This.sSourceTableName), "rsRecordsToDelete"
			ENDIF
			
			*Borra de sSourceTableName los registros marcados para borrar en rsRecordsToDelete
			sProcessingMsg="Eliminando los registros con datos faltantes"
			WAIT (sProcessingMsg) WINDOW NOWAIT

			SELECT (rsRecordsToDelete)
			INDEX on nReg TAG nRegIdx
			
			SELECT (This.sSourceTableName)
			SET RELATION TO RECNO() INTO (rsRecordsToDelete)
			DELETE ALL FOR &rsRecordsToDelete..BORRAR
			This.nDeletedRecs = _TALLY
			SET RELATION TO

			*Exporta el conjunto de registros depurado hacia un archivo excel
			IF This.nDeletedRecs>0 THEN
				This.exportToXLS(This.sSourceTableName, rsRecordsToDelete, "_DepuracionDatosFaltantes", "Por datos faltantes")
			ENDIF

			*Borra físicamente de sSourceTableName los registros maracados para borrar  en rsRecordsToDelete
			SELECT (This.sSourceTableName)
			PACK

		ENDIF

		*Cierra/Elimina los objetos temporales generados durante el procesamiento
		IF !bSourceTableWasOpen THEN
			SELECT (This.sSourceTableName)
			USE
		ENDIF
		SELECT C_MISSING_DATA
		USE

		=SYS(currentWHEREComplexity)
		SET DELETED &sOldSetDeleted
		WAIT CLEAR
	ENDIF
ENDPROC


PROCEDURE deleteDuplicatesByEpidemiologicalWeek

	LPARAMETERS sSolverProcedure as String

	*Borra de la tabla sSourceTableName registros repetidos. Se considera que un registro es repetido si todos ;
	los campos de sSourceTableName excepto los establecidos en This.sExcludedFields ampliado con los campos ;
	SEMANA y AÑO  son idénticos. 
	
	*Si se pasa el parámetro opcional sSolverProcedure, los registros repetidos se resuelven aplicando ;
	ese procedimiento; en caso contrario, se resuelven calculando la semana epidemiológica de;
	ocurrencia en función del evento y dejando en sSourceTableName solamente aquel que tenga esa semana bien calculada.;
	Los restantes registros que conforman el caso son eliminados físicamente de sSourceTableName y de las tablas;
	relacionadas de Eventos y Laboratorios.;
	sSolverProedure debe debe tener la siguiente interfaz: ;
	 	LPARAMETERS sSourceTableName as String, sRecordsToDeleteRSName as String, sDuplicatesCountingRSName as String


	sOldSetDeleted=SET("Deleted")
	SET DELETED ON
	currentWHEREComplexity=SYS(3055)
	=SYS(3055,640)

	bSourceTableWasOpen=.F.
	This.ErrorMessage=''
	bSourceTableIsOpenExclusively = .T.
	IF !USED(This.sSourceTableName) THEN
		TRY
			USE (This.sSourceTableName) EXCLUSIVE IN 0
			SELECT (This.sSourceTableName)
		CATCH TO oException
			bSourceTableIsOpenExclusively = .F.
		ENDTRY
	ELSE
		SELECT (This.sSourceTableName)
		bSourceTableWasOpen=.T.
		bSourceTableIsOpenExclusively = ISEXCLUSIVE(This.sSourceTableName)
	ENDIF

	IF !bSourceTableIsOpenExclusively THEN
		This.ErrorMessage="La tabla objetivo no está abierta o no se pudo abrir en forma exclusiva." + CHR(13) + CHR(10) + ;
							"Si está en una red, debe ejecutar este procedimiento de depuración en"	+ CHR(13) + CHR(10) + ;
							" el servidor y ningún usuario debe tener abierta la tabla objetivo o el sistema"
	ELSE
		* Utiliza todos los campos de sSourceTableName excepto los incluidos en sExcludedFields ampliado con los campos ;
		 SEMANA y AÑO
		This.sExcludedFields = This.sExcludedFields + " SEMANA AÑO "
		This.sDefaultKey = ''
		nSourceTableFields=AFIELDS(SourceTableFields)
		FOR nField=1 TO nSourceTableFields
			IF !(SourceTableFields(nField,1) $ This.sExcludedFields) THEN
				This.sDefaultKey = This.sDefaultKey + SourceTableFields(nField,1) + ","
			ENDIF
		NEXT nField
		This.sDefaultKey = SUBSTR(This.sDefaultKey,1,LEN(This.sDefaultKey)-1)

		*Establece si en sSourceTableName existen registros duplicados según la clave establecida para tales efectos
		WAIT "Estableciendo la cantidad de registros repetidos" WINDOW NOWAIT
		sDuplicatesCursorName=''
		This.nInitRecs = RECCOUNT(This.sSourceTableName )
		This.nDeletedRecs=0

		detectionResult = This.extractDupsAndNotDups(This.sSourceTableName  + ".COD_EVE!='000'")
		IF detectionResult!=0 THEN
			nDuplicatesCases=RECCOUNT("C_COUNT_DUPS")
			
			*Construye un cursor con los números de los registros que son suceptibles de ser eliminados. ;
			Inicialmente supone que todos los registros duplicados deberían eliminarse.
			SQLClause="SELECT A.* FROM (SELECT RECNO() AS nReg, .T. as BORRAR,* FROM " + This.sSourceTableName + ") AS A INNER JOIN C_COUNT_DUPS ON " 
			sJoinCondition='.T.'
			FOR nField=1 TO nSourceTableFields
				IF !(SourceTableFields(nField,1) $ This.sExcludedFields) THEN
					sJoinCondition=sJoinCondition + " AND A." + SourceTableFields(nField,1) + "=" + ;
									"C_COUNT_DUPS."  + SourceTableFields(nField,1)
				ENDIF
			NEXT nField
			SQLClause=SQLClause + sJoinCondition + " ORDER BY " + This.qualifyFields(This.sDefaultKey, ",", "A" ) + ;
						" INTO CURSOR rsRecordsToDelete READWRITE"
			&SQLClause

			IF VARTYPE(sSolverProcedure)='C' THEN
				*Intenta resolver los registros repetidos aplicando sSolverProcedure
				DO &sSolverProcedure WITH (This.sSourceTableName), "rsRecordsToDelete", "C_COUNT_DUPS"

			ELSE
				*Intenta resolver los registros repetidos teniendo en cuenta la semana epidemiológica. ;
				Según este criterio se escoje como registro válido aquel que tenga bien calculada la semana epidemiológica;
				en función del evento
				SELECT rsRecordsToDelete
				nCase=0
				DO WHILE !EOF()
					nCase=nCase+1
					sProcessingMsg="Resolviendo " + ALLTRIM(STR(nCase)) + " de " + ALLTRIM(STR(nDuplicatesCases)) + " casos"
					WAIT (sProcessingMsg) WINDOW NOWAIT
				
					*Calcula la semana y año epidemiológico de ocurrencia del caso teniendo en cuenta la fecha particular a partir;
					de la cual deben determinarse
					SET PROCEDURE TO EventsHandler ADDITIVE
					sWeekDeterminantFieldName = ''
					sDeterminantTableName = ''
					sEpidemiologicalWeek = ''
					sEpidemiologicalYear = ''
					IF getEpidemiologicalWeekDeterminants(rsRecordsToDelete.COD_EVE, @sWeekDeterminantFieldName,;
														 @sDeterminantTableName) THEN
						bContinue=0
						IF sDeterminantTableName=This.sSourceTableName THEN
							bContinue=getEpidemiologicalWeek(rsRecordsToDelete.&sWeekDeterminantFieldName,@sEpidemiologicalWeek,@sEpidemiologicalYear ) 
						ELSE
							SET PROCEDURE TO SivigilaUtilities ADDITIVE
							IF UseTable(sDeterminantTableName) THEN
								SELECT (sDeterminantTableName)
								SET ORDER TO TAG EVE
								SELECT rsRecordsToDelete
								SET RELATION TO AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE INTO (sDeterminantTableName)
								bContinue=getEpidemiologicalWeek(&sDeterminantTableName..&sWeekDeterminantFieldName, ;
																@sEpidemiologicalWeek,@sEpidemiologicalYear ) 
							ENDIF
						ENDIF
					ELSE
						bContinue=0
					ENDIF
					
					sourceEventCode = rsRecordsToDelete.COD_EVE
					sourceIdentType = rsRecordsToDelete.TIP_IDE
					sourceIdentNumber = rsRecordsToDelete.NUM_IDE
					sourceAdjustment = ALLTRIM(STR(VAL(rsRecordsToDelete.AJUSTE)))
					sourcePatientKey= sourceEventCode + sourceIdentType + sourceIdentNumber + sourceAdjustment 
					bCaseIsSolved=.F.

					IF bContinue >= 10 THEN
						*El criterio se puede utilizar por cuanto fue posible calcular la semana y año epidemiológico de ocurrencia del caso
						
						DO WHILE COD_EVE + TIP_IDE + NUM_IDE + ALLTRIM(STR(VAL(AJUSTE))) = sourcePatientKey 
							IF !bCaseIsSolved AND SEMANA=sEpidemiologicalWeek AND AÑO=sEpidemiologicalYear THEN
								REPLACE BORRAR WITH .F.
								bCaseIsSolved=.T.
							ENDIF
							SKIP
						ENDDO
					ELSE
						*El criterio no permite resolver el caso
						DO WHILE COD_EVE + TIP_IDE + NUM_IDE + ALLTRIM(STR(VAL(AJUSTE))) = sourcePatientKey 
							IF SEMANA=sEpidemiologicalWeek AND AÑO=sEpidemiologicalYear THEN
								REPLACE BORRAR WITH .F.
							ENDIF
							SKIP
						ENDDO
					ENDIF
					SKIP 1 IN C_COUNT_DUPS
				ENDDO
				WAIT CLEAR 
			ENDIF
			
			*Borra de sSourceTableName los registros marcados para borrar en rsRecordsToDelete
			sProcessingMsg="Eliminando los registros duplicados"
			WAIT (sProcessingMsg) WINDOW NOWAIT

			SELECT rsRecordsToDelete
			INDEX on nReg TAG nRegIdx

			SELECT (This.sSourceTableName)
			SET RELATION TO RECNO() INTO rsRecordsToDelete
			DELETE ALL FOR rsRecordsToDelete.BORRAR
			This.nDeletedRecs = _TALLY

			*Exporta el conjunto de registros depurado hacia un archivo excel
			IF This.nDeletedRecs>0 THEN
				This.exportToXLS(This.sSourceTableName,"rsRecordsToDelete","_DepuracionRepetidosSemanaepidemiologica",;
								 "Por semana epidemiológica")
			ENDIF

			*Borra físicamente de sSourceTableName los registros marcados para borrar  en rsRecordsToDelete
			SELECT (This.sSourceTableName)
			PACK
			SET RELATION TO

			*Borra de las tablas de eventos y de la tabla LABORATORIOS los registros de datos complementarios ;
			relacionados con los de sSourceTableName 
			SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
			=DeleteSIVIGILAComplementaryData("RSRecordsToDelete","AÑO+SEMANA+COD_PRE+COD_SUB+COD_EVE+TIP_IDE+NUM_IDE", ;
											"RSRecordsToDelete.Borrar", .T., This.sXlsFilePath, "_DepuracionRepetidosSemanaepidemiologica")
			
			*Cierra/Elimina los objetos temporales generados durante el procesamiento
			SELECT rsRecordsToDelete
			USE
		ENDIF

		*Cierra/Elimina los objetos temporales generados durante el procesamiento
		IF !bSourceTableWasOpen THEN
			SELECT (This.sSourceTableName)
			USE
		ENDIF
		SELECT C_Count_Dups				
		USE

		=SYS(currentWHEREComplexity)
		SET DELETED &sOldSetDeleted
		WAIT CLEAR
		*RETURN nDuplicates
	ENDIF
ENDPROC


PROCEDURE computeEpidemiologicalWeek

	LPARAMETERS sSolverProcedure as String, sFilterCondition as String

	* con el fin de que las instrucciones Try-Catch funcionen como se espera
	onError = ON("error")
	ON ERROR
	

	INSERT INTO LOGRECALCULO VALUES (This.sSourceTableName,PROGRAM(),"INICIO",DATETIME(),.F.)
	*Calcula la semana y año epidemiológicos de ocurrencia de los eventos de salud almacenados en sSourceTableName ;
	según las especificaciones que se encuentren en Tables.DeterminantesSemanaPorEvento.DBF
	
	* sFilterCondition es de la forma " VIGENCIA = '2010' "

	bSourceTableWasOpen=.F.
	This.ErrorMessage=''
	bSourceTableIsOpenExclusively = .T.
	IF !USED(This.sSourceTableName) THEN
		TRY
			USE (This.sSourceTableName) EXCLUSIVE IN 0
			SELECT (This.sSourceTableName)
		CATCH TO oException
			bSourceTableIsOpenExclusively = .F.
		ENDTRY
	ELSE
		SELECT (This.sSourceTableName)
		bSourceTableWasOpen=.T.
		bSourceTableIsOpenExclusively = ISEXCLUSIVE(This.sSourceTableName)
	ENDIF

	IF !bSourceTableIsOpenExclusively THEN
		This.ErrorMessage="La tabla objetivo no está abierta o no se pudo abrir en forma exclusiva." + CHR(13) + CHR(10) + ;
							"Si está en una red, debe ejecutar este procedimiento de depuración en"	+ CHR(13) + CHR(10) + ;
							" el servidor y ningún usuario debe tener abierta la tabla objetivo o el sistema"
	ELSE
		nSourceRecords=RECCOUNT()
		
		*Determina qué eventos diferentes están incluidos en sSourceTableName 
		sSelectSQLCmd = "SELECT A.EventCode as COD_EVE, COUNT(*) AS n FROM (SELECT PADL(COD_EVE,3,'0') as EventCode FROM " + ;
						This.sSourceTableName + " WHERE COD_EVE!='000') AS A GROUP BY A.EventCode INTO CURSOR rsTargetEvents"
		&sSelectSQLCmd
		
		IF _TALLY>0 THEN
			=This.setDefaultKey()
			sLinkerFields=STRTRAN(This.sDefaultKey,"AJUSTE,",'')
			sLinkerFieldsWithPlus=STRTRAN(sLinkerFields,',','+')
			SET PROCEDURE TO EventsHandler ADDITIVE

			nCase=0
			SELECT rsTargetEvents

			SCAN
				sourceEventCode = COD_EVE

				sProcessingMsg="Calculando semana epidemiológica para el evento " + sourceEventCode 
				WAIT (sProcessingMsg) WINDOW NOWAIT

				*Establece cuál es el campo base para determinar la semana epidemiológica de ocurrencia del evento;
				sourceEventCode  y de qué tabla debe tomarse
				sWeekDeterminantFieldName = ''
				sDeterminantTableName = ''
				sEpidemiologicalWeek = ''
				sEpidemiologicalYear = ''
				bContinue=getEpidemiologicalWeekDeterminants(rsTargetEvents.COD_EVE, @sWeekDeterminantFieldName, ;
															@sDeterminantTableName)
				IF bContinue THEN
					IF sDeterminantTableName=This.sSourceTableName THEN
						*El campo base para determinar la semana epidemiológica se debe tomar de la misma tabla en ;
						donde se encuentran los registros del evento sourceEventCode 
						
						* Establece el conjunto de registros a los que debe cambiársele la semana (registros objetivo)
						sSelectSQLCmd = "SELECT RECNO() AS nreg, " + This.sSourceTableName + "." + sWeekDeterminantFieldName + "," + ;
										This.qualifyFields(sLinkerFieldsWithPlus,'+',This.sSourceTableName) + " AS ClaveEncadenante , " + ;
										This.sDefaultkey +;
										" FROM " + This.sSourceTableName + " WHERE " + This.sSourceTableName + ".COD_EVE='" +;
										 + sourceEventCode + "' INTO CURSOR rsTargets NOFILTER"
						TRY
							&sSelectSQLCmd 
						CATCH TO oException
							bContinue=.F.
						ENDTRY
						
						IF bContinue THEN
							*Establece cuántas fechas diferentes existen en el conjunto de registros objetivo
							sSelectSQLCmd = "SELECT DISTINCT " + sWeekDeterminantFieldName + " FROM rsTargets INTO CURSOR rsDiffDates"
							&sSelectSQLCmd 

							*Establece cuál es la semana epidemiológica correspondiente a la fechas recien establecidas
							sSelectSQLCmd  = " SELECT " + sWeekDeterminantFieldName + ", CALENDARIO.SEMANA, CALENDARIO.VIGENCIA FROM "  +;
												" rsDiffDates INNER JOIN CALENDARIO ON  rsDiffDates." + sWeekDeterminantFieldName +;
												" BETWEEN CALENDARIO.DESDE AND CALENDARIO.HASTA INTO CURSOR rsNewWeekValues"
							&sSelectSQLCmd 
							
							* Establece el conjunto de registros objetivo con la nueva semana epidemiológica
							sSelectSQLCmd  = "SELECT '" + This.sSourceTableName + "' as TablaFuente, rsTargets.nReg,"  +;
											 " rsTargets.ClaveEncadenante, "  +;
											 This.qualifyFields(This.sDefaultKey, ",", "rsTargets" ) + ", " +;
											" PADR(rsNewWeekValues.SEMANA, 2, ' ') AS WEEKNEWVALUE, rsNewWeekValues.VIGENCIA AS YEARNEWVALUE, " +;
											" DATE() AS FechaRecalculo "   +;
											" FROM rsTargets INNER JOIN rsNewWeekValues ON rsTargets." + sWeekDeterminantFieldName  +;
											" =rsNewWeekValues." + sWeekDeterminantFieldName 

						ENDIF
					ELSE
						*El campo base para determinar la semana epidemiológica se debe tomar de una tabla diferente a aquella en ;
						donde se encuentran los registros del evento sourceEventCode 

						* Establece el conjunto de registros a los que debe cambiársele la semana (registros objetivo)
						sSelectSQLCmd = " SELECT DISTINCT A.nReg,  " + sDeterminantTableName + "." + sWeekDeterminantFieldName + "," +;
										" A.UnionKey as ClaveEncadenante, " + ;
										This.qualifyFields(This.sDefaultKey, ',', This.sSourceTableName) +;
										" FROM (SELECT RECNO() AS nreg, " + ;
										This.qualifyFields(sLinkerFieldsWithPlus, '+', This.sSourceTableName) + ;
										" AS UnionKey, " + ;
										This.qualifyFields(This.sDefaultKey, ',', This.sSourceTableName) + ;
										" FROM " + This.sSourceTableName + " WHERE " +;
										This.sSourceTableName + ".COD_EVE='" + sourceEventCode + "') AS A INNER JOIN " + sDeterminantTableName +;
										" ON A.UnionKey=" + This.qualifyFields(sLinkerFieldsWithPlus, '+',sDeterminantTableName) + ;
										" INTO CURSOR rsTargets NOFILTER"
						
						TRY
							&sSelectSQLCmd 
						CATCH TO oException
							bContinue=.F.
						ENDTRY
						
						IF bContinue THEN
							*Establece cuántas fechas diferentes existen en el conjunto de registros objetivo
							sSelectSQLCmd = "SELECT DISTINCT " + sWeekDeterminantFieldName + " FROM rsTargets INTO CURSOR rsDiffDates"
							&sSelectSQLCmd 

							*Establece cuál es la semana epidemiológica correspondiente a la fechas recien establecidas
							sSelectSQLCmd  = " SELECT " + sWeekDeterminantFieldName + ", CALENDARIO.SEMANA, CALENDARIO.VIGENCIA FROM "  +;
												" rsDiffDates INNER JOIN CALENDARIO ON  rsDiffDates." + sWeekDeterminantFieldName +;
												" BETWEEN CALENDARIO.DESDE AND CALENDARIO.HASTA INTO CURSOR rsNewWeekValues"
							&sSelectSQLCmd 

							* Establece el conjunto de registros objetivo con la nueva semana epidemiológica
							sSelectSQLCmd  = "SELECT '" + This.sSourceTableName + "' as TablaFuente, rsTargets.nReg," +;
											 " rsTargets.ClaveEncadenante, "  +;
											 " DATE() AS FechaRecalculo, "   +;
											 This.qualifyFields(This.sDefaultKey, ",", "rsTargets" ) + ", " +;
											 " PADR(rsNewWeekValues.SEMANA, 2, ' ') AS WEEKNEWVALUE, rsNewWeekValues.VIGENCIA AS YEARNEWVALUE, " +;
											 " FROM rsTargets INNER JOIN rsNewWeekValues ON rsTargets." + sWeekDeterminantFieldName  +;
											 " =rsNewWeekValues." + sWeekDeterminantFieldName
						ENDIF
					ENDIF
					
					IF bContinue THEN
						sSelectSQLCmd= sSelectSQLCmd + " INTO CURSOR rsRecalculatedWeeks NOFILTER"
						TRY
							&sSelectSQLCmd
						CATCH TO oException
							bContinue = .F.
						ENDTRY
						IF bContinue
							SELECT NReg, COUNT(*) as n FROM rsRecalculatedWeeks GROUP BY NReg HAVING COUNT(*)>1 INTO CURSOR rsNonCandidates
							sNewDefaulKey = STRTRAN(This.sDefaultKey, ",", "+")
							sNewDefaulKey = STRTRAN(sNewDefaulKey , "AÑO", "YEARNEWVALUE")
							sNewDefaulKey = STRTRAN(sNewDefaulKey , "SEMANA", "WEEKNEWVALUE")
							sNewLinkerFields = STRTRAN(sLinkerFieldsWithPlus, "AÑO", "YEARNEWVALUE")
							sNewLinkerFields = STRTRAN(sNewLinkerFields, "SEMANA", "WEEKNEWVALUE")
							
							sInsertSQLCmd = " INSERT INTO RecalculoDeSemanasEpidemiologicas SELECT TablaFuente, COD_EVE, NREG, WEEKNEWVALUE AS SEMANA, " +;
											" AÑO, FechaRecalculo, ClaveEncadenante, " + ;
											sNewLinkerFields + " AS NUEVACLAVEENCADENANTE, " +;
											STRTRAN(This.sDefaultKey, ",", "+") + " AS CLAVEPORDEFECTO, " +;
											sNewDefaulKey  + " AS NUEVACLAVEPORDEFECTO, " + ;
											" semana as SEMANAORIGINAL " +;
											" FROM rsRecalculatedWeeks WHERE " +;
											" INT(VAL(AÑO))*100 + INT(VAL(SEMANA)) <> INT(VAL(YEARNEWVALUE))*100 + INT(VAL(WEEKNEWVALUE))" +;
											" AND rsRecalculatedWeeks.NReg NOT IN (SELECT NReg FROM rsNonCandidates) "
							IF VARTYPE(sFilterCondition) = "C"
								IF !EMPTY(sFilterCondition)
									sInsertSQLCmd = sInsertSQLCmd  + " AND YEARNEWVALUE = " + sFilterCondition
								ENDIF
							ENDIF
							TRY
								&sInsertSQLCmd
							CATCH TO oException
								bContinue=.F.
							ENDTRY
						ENDIF
					ENDIF
				ENDIF
				SELECT rsTargetEvents
			ENDSCAN
						

			*Cierra/Elimina los objetos temporales generados durante el procesamiento
			SELECT rsTargetEvents
			USE

			SELECT rsNonCandidates
			USE

			SELECT rsRecalculatedWeeks 
			USE
			
			SELECT RecalculoDeSemanasEpidemiologicas 
			USE
		ENDIF

		*Cierra/Elimina los objetos temporales generados durante el procesamiento
		IF !bSourceTableWasOpen THEN
			SELECT (This.sSourceTableName)
			USE
		ENDIF
		WAIT CLEAR
		
		INSERT INTO LOGRECALCULO VALUES (This.sSourceTableName,PROGRAM(),"FIN",DATETIME(),.T.)
		
		* Una vez realizado el recálculo, se continua con el proceso de actualización
		SET PROCEDURE TO SivigilaUtilities ADDITIVE
		
		* Primero se eliminan los duplicados.
		
		* Segundo, se actualizan datos complementarios

		INSERT INTO LOGRECALCULO VALUES (This.sSourceTableName, "UpdateSIVIGILAComplementaryData","INICIO",DATETIME(),.T.)	
		
		UpdateSIVIGILAComplementaryData(This.sSourceTablename,;
									"RecalculoDeSemanasEpidemiologicas",;
									 sLinkerFieldsWithPlus, ;
									"",;
									"ClaveEncadenante",;
									"AÑO=RecalculoDeSemanasEpidemiologicas.AÑO,SEMANA=RecalculoDeSemanasEpidemiologicas.SEMANA",;
									"Actualizando datos de la tabla")	

		INSERT INTO LOGRECALCULO VALUES (This.sSourceTableName, "UpdateSIVIGILAComplementaryData","FIN",DATETIME(),.T.)	

		* Tercero, Actualizar datos básicos
	
		INSERT INTO LOGRECALCULO VALUES (This.sSourceTableName, "UpdateSivigilaBasicData","INICIO",DATETIME(),.F.)	
		
		UpdateSivigilaBasicData(This.sSourceTablename, ;
									" AÑO = RECALCULODESEMANASEPIDEMIOLOGICAS.AÑO, SEMANA = RECALCULODESEMANASEPIDEMIOLOGICAS.SEMANA ", ;
									" RecalculoDeSemanasEpidemiologicas ", ;
									" RECALCULODESEMANASEPIDEMIOLOGICAS.CLAVEENCADENANTE = " + This.qualifyFields(sLinkerFieldsWithPlus,'+',This.sSourceTableName))
									
		INSERT INTO LOGRECALCULO VALUES (This.sSourceTableName, "UpdateSivigilaBasicData","FIN",DATETIME(),.T.)	
		
		INSERT INTO LOGRECALCULO VALUES (This.sSourceTableName,"exportUpdatedRowsToXLS","INICIO",DATETIME(),.F.)	
		* Cuarto, Exportar resultados a excel
		exportUpdatedRowsToXLS("RecalculoDeSemanasEpidemiologicas", "_2010", "..", "")
		INSERT INTO LOGRECALCULO VALUES (This.sSourceTableName, "exportUpdatedRowsToXLS","FIN",DATETIME(),.T.)	
		
		* Quinto, Archivar en bodega de datos.
		INSERT INTO LOGRECALCULO VALUES (This.sSourceTableName, "sendDataToWareHouse","INICIO",DATETIME(),.F.)	
		sendDataToWareHouse("RecalculoDeSemanasEpidemiologicas")
		INSERT INTO LOGRECALCULO VALUES (This.sSourceTableName, "sendDataToWareHouse","FIN",DATETIME(),.F.)	

		ON ERROR &onError
	
	ENDIF
ENDPROC


FUNCTION setDefaultKey as String
	
	*Retorna en un string separado por comas los campos de la clave por defecto establecida ;
	para sSourceTableName 

	WITH THIS
	
		DO CASE  
			CASE .sSourceTableName = 'PACIENTE'
				.sDefaultKey = ' AJUSTE, AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB '

			CASE .sSourceTableName = 'EVENTOS_23'
				.sDefaultKey = ' COD_EVE, AÑO, SEMANA, COD_MUN, NUM_CON, COD_PRE, COD_SUB '

			CASE .sSourceTableName = 'EVENTOS_31'
				.sDefaultKey = ' AÑO, SEMANA, COD_PRE, COD_SUB, COD_EVE, COD_DEP, COD_MUN, ESPECIE_VE '

			CASE .sSourceTableName = 'EVENTOS_32'
				.sDefaultKey = ' MES, AÑO, COD_PRE, COD_SUB, COD_EVE, COD_DEP, COD_MUN '

			CASE .sSourceTableName = 'EVENTOS_43'  OR .sSourceTableName = 'EVENTOS_91' OR .sSourceTableName = 'EVENTOS_94' 
				.sDefaultKey = ' COD_EVE, AÑO, SEMANA, COD_MUN, NUM_CON, COD_PRE, COD_SUB '

			CASE .sSourceTableName = 'EVENTOS_80' OR .sSourceTableName = 'EVENTOS_81'  OR .sSourceTableName = 'EVENTOS_88'  OR .sSourceTableName = 'EVENTOS_90' 
				.sDefaultKey = ' MES, AÑO, COD_PRE, COD_SUB, COD_EVE, NUM_CON '

			CASE LIKE('EVENTOS_??',.sSourceTableName) 
				.sDefaultKey = ' AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB '

			CASE .sSourceTableName = 'LABORATORIOS'
				.sDefaultKey = 'AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, AGENTE, PRUEBA, COD_PRE, COD_SUB,' + ;
								'MUESTRA, RESULTADO'

			CASE .sSourceTableName = 'BROTES'
				.sDefaultKey = 'COD_EVE, AÑO, SEMANA, COD_MUN, NUM_CON, COD_PRE, COD_SUB '

			CASE .sSourceTableName = 'UPGD' OR .sSourceTableName = 'UPGD_UCIS'
				.sDefaultKey = 'COD_PRE, COD_SUB'

			CASE .sSourceTableName = 'TAL_HUM'
				.sDefaultKey = 'COD_PRE, COD_SUB, COD_ART, VALOR '

			CASE .sSourceTableName = 'EBA'
				.sDefaultKey = ' AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_DX '
		
		ENDCASE
	ENDWITH
ENDFUNC


PROCEDURE addExcludedField()

	LPARAMETERS sFieldnameToAdd as String
	
	This.sExcludedFields = This.sExcludedFields + UPPER(sFieldnameToAdd) + " "
ENDPROC


PROTECTED FUNCTION extractDupsAndNotDups

	LPARAMETERS sFilterCondition as String
	
	*Determina si existen registros duplicados en sSourceTableName teniendo en cuenta la clave que para tales efectos;
	se encuentre definida en sDefaultKey. El parámetro opcional sFilterCondition permite establecer una condición ;
	que deben satisfacer los registros de sSourceTableName para que sean tenidos en cuenta en la detección de duplicados.
	
	*Retorna:	(0) No se encontraron registros duplicados ;
				(10) Existen registros duplicados

	*Como efecto colateral, crea un cursor con el conteo de los registros duplicados en sSourceTableName. ;
	El nombre de esta cursor será C_COUNT_DUPS y sus campos son los mismos de sDefaultKey más un campo Counter;
	que contiene el conteo de duplicados

	LOCAL SqlClause AS String 
	
	_ReturnedValue = 0

	SqlClause = 'SELECT ' + This.sDefaultKey + ', COUNT(*) AS counter FROM ' + '"' + This.sSourceTableName + '"'
	IF VARTYPE(sFilterCondition)='C' THEN
		IF !EMPTY(sFilterCondition) THEN
			SqlClause = SqlClause  + " WHERE " + sFilterCondition
		ENDIF
	ENDIF
	SqlClause = SqlClause + ' GROUP BY ' + This.sDefaultKey + ' HAVING COUNT(*) > 1 INTO CURSOR C_COUNT_DUPS NOFILTER'
	*SET SAFETY OFF
	*=STRTOFILE(SqlClause, 'qryProblema.qpr')
	*SET SAFETY ON
	&SqlClause
	*DO qryProblema.qpr
	
	IF _TALLY > 0
		_ReturnedValue = _ReturnedValue + 10
	ENDIF

	RETURN _ReturnedValue 
	  
ENDFUNC

PROTECTED FUNCTION extractUniqueRecs(sFilterCondition as String) as Boolean 
	
	*Crea un cursor con los registros distintos de This.sSourceTableName teniendo en cuenta la clave que para tales efectos;
	se encuentre definida en This.sDefaultKey. El parámetro opcional sFilterCondition permite establecer una condición ;
	que deben satisfacer los registros de This.sSourceTableName para que sean tenidos en cuenta en la detección de duplicados.
	
	*El nombre del cursor creado será rsUniqueRecords y sus campos son los mismos de sDefaultKey 

	LOCAL SqlClause AS String 
	
	SqlClause = 'SELECT DISTINCT ' + This.sDefaultKey + ' FROM ' + '"' + This.sSourceTableName + '"'
	IF VARTYPE(sFilterCondition)='C' THEN
		IF !EMPTY(sFilterCondition) THEN
			SqlClause = SqlClause  + " WHERE " + sFilterCondition
		ENDIF
	ENDIF
	SqlClause = SqlClause + ' INTO CURSOR rsUniqueRecords NOFILTER'
	&SqlClause
	
	RETURN (_TALLY > 0) 
	  
ENDFUNC


PROTECTED FUNCTION extractMissingData

	*Determina si existen registros con datos faltantes en sSourceTableName teniendo en cuenta la clave que para tales efectos;
	se encuentre definida en sDefaultKey.
	
	*Retorna:	(0) No se encontraron registros con datos faltantes ;
				(10) Existen registros con datos faltantes

	*Como efecto colateral, crea un cursor con el conjunto de registros con datos faltantes en sSourceTableName. ;
	El nombre de esta cursor será C_MISSING_DATA y sus campos son los mismos de sDefaultKey más un campo nReg;
	que contiene el número del registro tal como se encuentra registrado en sSourceTableName y un campo BORRAR = .T.

	_ReturnedValue = 0

	DIMENSION SourceTableFields(1,1)
	nSourceTableFields=ALINES(SourceTableFields,This.sDefaultKey,1,',')
	DIMENSION SourceTableFields(nSourceTableFields,1)

	sWhereCondition = "(.F."
	FOR nField=1 TO nSourceTableFields
		IF !(SourceTableFields(nField,1) $ This.sExcludedFields) THEN
			sWhereCondition = sWhereCondition + " OR EMPTY(" + This.sSourceTableName + "." + SourceTableFields(nField,1) + ")" 
		ENDIF
	NEXT nField
	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	IF IsField(This.sSourceTableName, "COD_EVE") THEN
		sWhereCondition = sWhereCondition + ") AND " + (This.sSourceTableName) + "." + "COD_EVE!='" + NO_NOTIFICATION_EVENT_CODE + "'"
	ELSE
		sWhereCondition = sWhereCondition + ")"
	ENDIF

	SqlClause = 'SELECT ' + This.sDefaultKey + ', RECNO() AS nReg, .T. AS BORRAR ' + ;
				'FROM ' + '"' + This.sSourceTableName + '"' + ' WHERE ' + sWhereCondition +;
				' INTO CURSOR C_MISSING_DATA NOFILTER' 
	&SqlClause

	IF _TALLY > 0
		_ReturnedValue = _ReturnedValue + 10
	ENDIF

	RETURN _ReturnedValue 
	  
ENDFUNC


HIDDEN FUNCTION qualifyFields

	LPARAMETERS sFieldsList as String, sSeparatorChar as String, sQualifier AS String
	
	RETURN sQualifier + "." + STRTRAN(STRTRAN(sFieldsList,' ',''), sSeparatorChar, sSeparatorChar + sQualifier + ".")
	  
ENDFUNC


PROCEDURE exportToXLS

	LPARAMETERS rsToExportName AS String, rsDeletedRecordsName as String, sXLSFileNameSuffix as String, ;
				sStrForDeleted as String
	
	*Exporta los registros de rsToExportName cuyo número de registro aparezca como borrado en rsDeletedRecordsName ;
	a un archivo con formato XLS o CSV, dependiendo de si el número de registros en rsToExportName es menor o mayor;
	que nMaxXLSRows.
	
	*El archivo exportado tendrá nombre rsToExportName + sXLSFileNameSuffix y quedará ubicado en This.xlsFilePath . ;
	Además de todos los campos presentes en rsToExportName, agrega un campo inicial de nombre Borrado cuyos valores;
	son NO o sStrForDeleted dependiendo de si el registro ha sido borrado o no.
	

	&&#DEFINE nMaxRowsForXLS 65000
	&&#DEFINE sExportingMsg "Exportando a excel..."
	
	LOCAL sExportedFileType as String, xlsFileName as String
	
	sOldSetDeleted = SET("Deleted")
	WITH This
		IF !USED(rsToExportName) THEN
			USE (rsToExportName)
		ELSE
			SELECT (rsToExportName)
		ENDIF

		IF RECCOUNT(rsToExportName)>nMaxRowsForXLS THEN
			*El número de registros a exportar es de tal magnitud que se hace la exportacióon hacia un archivo CSV;
			que es leido en forma nativa por Excel 2007 o superior
			sExportedFileType='CSV'
		ELSE
			sExportedFileType='XL5'
		ENDIF

		SET DELETED OFF
		SQLClause="SELECT IIF(" + rsDeletedRecordsName + ".BORRAR,'" + sStrForDeleted + ;
					"',PADR('NO',LEN('" + sStrForDeleted + "'))) AS Borrado, A.* FROM " + ;
					"(SELECT RECNO() AS nReg, * FROM " + rsToExportName + ") AS A INNER JOIN " + rsDeletedRecordsName + ;
					" ON A.nReg = " + rsDeletedRecordsName + ".nReg INTO CURSOR rsDeletedRecords"
		&SQLClause
		SET DELETED &sOldSetDeleted
		SELECT rsDeletedRecords
		
		xlsFileName = rsToExportName  + sXLSFileNameSuffix
		WAIT sExportingMsg WINDOW NOWAIT
		sCopyToCmd = 'COPY TO "' + This.sXlsFilePath + '\' + xlsFileName + '" TYPE ' + sExportedFileType
		&sCopyToCmd
		USE
		
		WAIT CLEAR
	ENDWITH

ENDPROC


PROCEDURE ShowResults

	&&#DEFINE sWindowTitle "Resultados de depuración - SIVIGILA"	

	LOCAL sResultsMessage as String
	
	sResultsMessage= 'Número inicial de registros en la tabla: ' + ALLTRIM(STR(This.nInitRecs)) + CHR(13) 
	sResultsMessage = sResultsMessage + 'Número de registros eliminados: ' + ALLTRIM(STR(This.nDeletedRecs)) + CHR(13)
	sResultsMessage = sResultsMessage + 'Número de registros resultantes: ' + ALLTRIM(STR(This.nInitRecs - This.nDeletedRecs)) + CHR(13)
	
	IF THIS.nDeletedRecs > 0 THEN
		sResultsMessage = sResultsMessage + CHR(13) +;
						  'Se ha generado un archivo en formato compatible con Excel 2007 en: ' ;
						  + CHR(13) + This.sXlsFilePath 
	ENDIF
	
	MESSAGEBOX( sResultsMessage,  0 + 48 , sWindowTitle ) 

ENDPROC



PROTECTED PROCEDURE myDuplicatesSolverProc

	LPARAMETERS sSourceTableName as String, sRecordsToDeleteRSName as String, sDuplicatesCountingRSName as String
	
	RETURN
ENDPROC

PROCEDURE verifyUpgdsNotification

	* Investiga los códigos de UPGD existentes en las tablas de SIVIGILA y determina;
	1. UPGDs que tengan datos en alguna tabla de datos (paciente, eventos_??, brotes, laboratorios) y que ;
		aparezcan como inactivas en Tables.UPGDs ;
	2. UPGDs que tengan datos en alguna tabla de datos (paciente, eventos_??, brotes, laboratorios) y que ;
		no tengan un registro coincidente en Tables.UPGDs.;
	3. UPGDs que aparezcan en tables.UPGDs, que sean activas y que no tengan datos en ninguna tabla de datos del SIVIGILA;
		Todos los reusltados deben almacenarse en una tabla con los siguientes campos: COD_PRE, COD_SUB, Registros, Inconsistencia;
	 	en donde, Registros: nùmero de registros que se encontraron en las tablas de datos correspondientes a la upgd;
			Inconsistencia: 1 - Las del punto 1; 2 - Las del punto 2; 3 - Las del punto 3; 
	0. Las que estàn OK, o sea, estan activas y tienen datos en alguna tabla de datos

	LPARAMETERS sXlsFileName, sWhereClause, bBeSilent
	
	LOCAL sOldDeleted, sOldExclusive, sOldSafety, sLabelCase1, sLabelCase2, sLabelCase3, sLabelCase0
	
	sOldSetDeleted = SET("Deleted")	
	
	sOldSetExclusive = SET("Exclusive")
	
	sOldSafety = SET("Safety")
	
	SET DELETED ON
	SET EXCLUSIVE OFF	
	SET SAFETY OFF
	IF VARTYPE(sWhereClause) <> 'C' OR EMPTY(sWhereClause)
		sWhereClause = '.T. '
	ENDIF				
	bContinue = .T.
	
	*Determina para cada UPGD en qué semanas existen eventos notificados en función del código del evento

	*Considera los registros de datos básicos de eventos individuales
	sSQLSelectCmd = "SELECT COD_PRE,COD_SUB, AÑO, COUNT(*) AS CANTIDAD, -1 AS ESTADO FROM PACIENTE WHERE " + sWhereClause + ;
					" GROUP BY COD_PRE,COD_SUB, AÑO UNION ALL"
	*Agrega los registros de Brotes				
	sSQLSelectCmd = sSQLSelectCmd + " SELECT COD_PRE,COD_SUB, AÑO, COUNT(*) AS CANTIDAD, -1 AS ESTADO " + ;
					" FROM BROTES WHERE " + ;
					sWhereClause + " GROUP BY COD_PRE,COD_SUB, AÑO "
	*Agrega los registros de los eventos tipo seguimiento
	DO EventosConTablaDatosIndependiente.QPR
	SELECT DISTINCT "EVENTOS_" + PADL(FORMULARIO,2,'00') AS NombreTablaEventoSeguimiento FROM rsEventosConTablaDatosIndependiente ;
			INTO CURSOR rsFollowupEvents
	SELECT rsFollowupEvents
	SCAN
		sSQLSelectCmd = sSQLSelectCmd + "UNION ALL SELECT COD_PRE,COD_SUB, AÑO, COUNT(*) AS CANTIDAD, -1 AS ESTADO " + ;
						" FROM " + ;
						rsFollowupEvents.NombreTablaEventoSeguimiento +;
						" WHERE  " + sWhereClause + " GROUP BY COD_PRE,COD_SUB, AÑO "
	ENDSCAN
	
	* Agregar las UPgd que se encuentren en la tabla de laboratorios.
	sSQLSelectCmd = sSQLSelectCmd + "UNION ALL SELECT COD_PRE,COD_SUB, AÑO, COUNT(*) AS CANTIDAD, -1 AS ESTADO " + ;
						" FROM LABORATORIOS " + ;
						" WHERE  " + sWhereClause + " GROUP BY COD_PRE,COD_SUB, AÑO "
	
	sSQLSelectCmd = sSQLSelectCmd + " INTO CURSOR rsAllEventsRecordCount READWRITE"
	&sSQLSelectCmd

	TRY
		&sSQLSelectCmd
	CATCH TO oException
		bContinue = .F.
	ENDTRY

	* Generar resultados:	
	IF bContinue THEN
		* 0. Las que están OK.	
		UPDATE rsAllEventsRecordCount SET ESTADO = 0 FROM UPGD WHERE ;
			rsAllEventsRecordCount.COD_PRE = UPGD.COD_PRE AND;
			rsAllEventsRecordCount.COD_SUB = UPGD.COD_SUB AND;
			UPGD.ACT_SIV = 1

		* 1. Upgd Inactiva en la tabla UPGD y que tine registros reportados	
		UPDATE rsAllEventsRecordCount SET ESTADO = 1 FROM UPGD WHERE ;
			rsAllEventsRecordCount.COD_PRE = UPGD.COD_PRE AND;
			rsAllEventsRecordCount.COD_SUB = UPGD.COD_SUB AND;
			UPGD.ACT_SIV = 2		

		* 2. Upgd que no existe en Tables.upgd 
		UPDATE rsAllEventsRecordCount SET ESTADO = 2 FROM ;
				rsAllEventsRecordCount WHERE ;
				ESTADO = -1
				
		*	3. Upgd que existe en Tables.upgds, Activa y no ha reportadO
		INSERT INTO rsAllEventsRecordCount (COD_PRE, COD_SUB, CANTIDAD, ESTADO) ;
			SELECT COD_PRE, COD_SUB, 0 AS CANTIDAD, 3 AS ESTADO FROM UPGD WHERE COD_PRE+COD_SUB ;
			NOT IN;
			(SELECT COD_PRE + COD_SUB FROM rsAllEventsRecordCount);
			AND ACT_SIV = 1
			
		
		* Consolida la información final
		
		SELECT COD_PRE, COD_SUB, ESTADO, AÑO, SUM(CANTIDAD) AS REGISTROS FROM rsAllEventsRecordCount;
			WHERE NOT EMPTY(COD_PRE + COD_SUB);
			GROUP BY COD_PRE, COD_SUB, ESTADO, AÑO INTO CURSOR rsAllUpgdOnSivigilaTables

		sLabel = sLABEL_FOR_UNDETERMINATED_CONTENT 

		*Complementa los registros de conteos con la información descriptiva de UPGDs, MUNICIPIOS y DEPARTAMENTOS 
		SELECT DISTINCT rsAllUpgdOnSivigilaTables.COD_PRE,rsAllUpgdOnSivigilaTables.COD_SUB, REGISTROS , rsAllUpgdOnSivigilaTables.AÑO, ;
			IIF(ESTADO=0,sUPGD_ACTIVE_WITH_NOTIFICATIONS,IIF(ESTADO=1,sUPGD_INACTIVE_WITH_NOTIFICATIONS ,IIF(ESTADO=2,sUPGD_NOT_EXISTS_WITH_NOTIFICATIONS,IIF(ESTADO=3,sUPGD_ACTIVE_WITHOUT_NOTIFICATIONS ,'Indeterminado')))) as ESTADO,;
			ES_UNI_NOT, IIF(ISNULL(UPGD.RAZ_SOC),PADR(sLabel ,50,' '),ALLTRIM(UPGD.RAZ_SOC)) AS RAZ_SOC, ;
			IIF(ISNULL(UPGD.RES_NOT),PADR(sLabel ,50,' '),ALLTRIM(UPGD.RES_NOT) + ', Tel: ' + ALLTRIM(UPGD.TEL)) AS RES_NOTI, ;
			IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA,;
			IIF(UPGD.ESTADOUPGD='1','Abierta      ',IIF(UPGD.ESTADOUPGD='2','Cerrada      ','INDETERMINADO')) AS Estado_upgd,;
			IIF(ISNULL(UPGD.FEC_CAR),CTOD('//'),UPGD.FEC_CAR) AS FEC_CAR, ;
			IIF(ISNULL(MUNICIPIOS.COD_MUN),PADR(sLabel ,30,' '),MUNICIPIOS.COD_MUN) AS COD_MUN, ;
			IIF(ISNULL(MUNICIPIOS.NOM_MUN),PADR(sLabel ,30,' '),MUNICIPIOS.NOM_MUN) AS NOM_MUN, ;
			IIF(ISNULL(DEPTOS.COD_DEP),PADR(sLabel ,30,' '),DEPTOS.COD_DEP) AS COD_DEP, ;
			IIF(ISNULL(DEPTOS.NOM_DEP ),PADR(sLabel ,30,' '),DEPTOS.NOM_DEP ) AS NOM_DEP FROM ;
			rsAllUpgdOnSivigilaTables LEFT OUTER JOIN UPGD ON ;
			rsAllUpgdOnSivigilaTables.COD_PRE=UPGD.COD_PRE AND rsAllUpgdOnSivigilaTables.COD_SUB=UPGD.COD_SUB ;
			LEFT OUTER JOIN MUNICIPIOS ON LEFT(rsAllUpgdOnSivigilaTables.COD_PRE,5) = MUNICIPIOS.COD_MUN ;
			LEFT OUTER JOIN DEPTOS ON LEFT(rsAllUpgdOnSivigilaTables.COD_PRE,2) = DEPTOS.COD_DEP;
			INTO CURSOR rsUpgdFinal READWRITE
		
		oDataExporter=NEWOBJECT("DataExporter","DataExporter.Prg",.null.,'rsUpgdFinal')
		oDataExporter.sExportationPath = This.sXlsFilePath 
		oDataExporter.sExportedFileName = sXlsFileName 
		oDataExporter.exportToXLS()			
		IF NOT bBeSilent THEN
			oDataExporter.ShowXLSExportationResults()
		ENDIF
		RELEASE oDataExporter
	ENDIF
	
	SET DELETED &sOldSetDeleted
	SET EXCLUSIVE &sOldSetExclusive
	SET SAFETY &sOldSafety

ENDPROC		&& verifyUpgdsNotification
	
ENDDEFINE && DuplicatesSolver




DEFINE CLASS VariableTranslator as Custom

HIDDEN sDefaultKey AS String, sEventTablename as String
sDefaultKey=.NULL.
sEventTablename=.NULL.

sSourceTableName=.NULL.
*Nombre de la tabla que contiene los registros objeto de homologación

sDefaultKey = .NULL.

sXlsFilePath = '.'
*Ruta en donde se almacenará el archivo XLS que contiene los registros que fueron objeto de homologación

sFilterExpresion = .NULL.

sPathToSIVIGILAFiles = '.'
*Ruta en donde se encuentran los archivos de datos del Sistema SIVIGILA

bBeSilent=.F.
*Indica si los mensajes de procesamiento deben dirigirse a la GUI o NO(Default)

nUpdatedRecs = 0
ErrorMessage = .NULL.
 

FUNCTION Init
	LPARAMETERS _sSourceTableName as String

	IF VARTYPE(_sSourceTableName)='C' THEN
		This.sSourceTableName=_sSourceTableName
	ENDIF
ENDFUNC


FUNCTION Destroy
	This.sSourceTableName=.NULL.
	This.sDefaultKey = .NULL.	
ENDFUNC



PROCEDURE translateVariables

	LPARAMETERS sEventCode as String, bUseSelectorCriteria as Boolean
	
	*Ejecuta el proceso de homolgación de variables en los registros de This.sSourceTableName según se ;
	encuentra definido en Databases.AnomaliesDB.Views.vHomologaciones
	
	*sEventCode es el código de evento que permite seleccionar registros de This.sSourceTableName los cuales ;
	serán objeto de homologación. Si no se suministra sEventCode, la ejecución de la homologación no tendrá en cuenta ;
	el código de evento para seleccionar registros.
	
	*bUseSelectorCriteria (opcional) indica si debe utilizarse el criterio de selección que se encuentra ;
	establecido en Databases.AnomaliesDB.Views.vHomologaciones para ejecutar el proceso de homologación a ;
	los registros objetivo.
	
	
	LOCAL nLocalCounter as Number  
	
	
	sOldSetTalk=SET("Talk")
	SET TALK ON
	
	bSourceTableWasOpen=.F.
	IF !USED(This.sSourceTableName) THEN
		USE (This.sSourceTableName) IN 0
		SELECT (This.sSourceTableName)
	ELSE
		SELECT (This.sSourceTableName)
		bSourceTableWasOpen=.T.
	ENDIF

	*Establece en qué tabla se guardan los registros del evento con código sEventCode
	SET PROCEDURE TO EventsHandler ADDITIVE
	This.sEventTablename = GetWarehouseTableName(sEventCode)
		
	*Establece los campos de la clave por defecto para la tabla sEventTablename
	oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,This.sEventTablename)
	= oAnomaliesSolver.setDefaultKey()
	This.sDefaultKey=oAnomaliesSolver.sDefaultKey 
	
	*Elimina duplicados de la fuente de datos a convertir
	oAnomaliesSolver.sSourceTableName=This.sSourceTableName
	oAnomaliesSolver.sXlsFilePath = This.sXlsFilePath 
	oAnomaliesSolver.deleteDuplicates()
	RELEASE oAnomaliesSolver
	
	*Establece el conjunto de conversiones a aplicar en función del código de evento sEventCode 
	sSelectSQLCmd="SELECT * FROM anomaliesDB!vHomologaciones WHERE COD_EVE='" + sEventCode + "' INTO CURSOR rsVarsMapping"
	&sSelectSQLCmd
		
	*Ejecuta las conversiones
	IF !This.bBeSilent THEN
		WAIT "Homologando variables para el evento " + sEventCode WINDOW NOWAIT
	ENDIF
	
	nLocalCounter = 0
	dCurrentDateTime=DATETIME()
	
	* Crea un recordset que mantendrá las claves de los registros que van siendo homologados. Esto es necesario para ;
	que sobre un mismo registro no se haga más de una homologación debida, por ejemplo, a que para una misma variable;
	de un evento, se deban homologar varios valores que podrían conducir a que sobre un mismo registro se efectuen ;
	varios cambios sobre la misma variable cuando en realidad solo aplica uno.
	sSelectSQLCmd  = "SELECT SPACE(45) AS claveRegistroCambiado, SPACE(128) AS NombreVariable FROM " + This.sSourceTableName + ;
					" WHERE .F. INTO CURSOR rsUpdatedRecs READWRITE"
	&sSelectSQLCmd  
	
	SELECT rsVarsMapping
	SCAN
		*Establece el conjunto de registros candidatos a ser actualizados
		sSelectSQLCmd = "SELECT '" + This.sEventTablename + "' AS TablaObjetivo, " + STRTRAN(This.sDefaultKey,',','+') +;
						" AS claveRegistroCambiado, '" +	rsVarsMapping.NombreVariable + "' AS nombreVariable," +;
						"'" + rsVarsMapping.ValorOriginal + "' AS ValorOriginal,"  +;
						" RECNO() AS NumeroRegistroCambiado FROM " + This.sSourceTableName +;
						" WHERE " + This.sSourceTableName + ".COD_EVE='" + sEventCode + "' AND " + rsVarsMapping.NombreVariable + " = '" +;
						rsVarsMapping.ValorOriginal + "'" 
		IF bUseSelectorCriteria THEN
			sSelectSQLCmd = sSelectSQLCmd +	" AND (" + rsVarsMapping.CriterioSelector + ")" 
		ENDIF
		IF !ISNULL(This.sFilterExpresion) THEN
			sSelectSQLCmd = sSelectSQLCmd +	" AND (" + This.sFilterExpresion + ")" 
		ENDIF
		sSelectSQLCmd = sSelectSQLCmd + " AND " + STRTRAN(This.sDefaultKey,',','+') + "+'" + ALLTRIM(rsVarsMapping.NombreVariable) + ;
						"' NOT IN (SELECT ALLTRIM(claveRegistroCambiado) + ALLTRIM(NombreVariable) FROM rsUpdatedRecs)"
		sSelectSQLCmd = sSelectSQLCmd + " INTO CURSOR rsCandidatesRecs NOFILTER"
		&sSelectSQLCmd 
		
		IF _TALLY>0 THEN
			sInsertSQLCmd = "INSERT INTO DatosHomologados (TablaObjetivo, claveRegistroCambiado, nombreVariable, ValorOriginal, ValorNuevo, FechaHora, NumeroRegistroCambiado) " +;
							"SELECT TablaObjetivo, claveRegistroCambiado, nombreVariable," +;
							"ValorOriginal,'" + rsVarsMapping.ValorNuevo + "'" +;
							" AS valorNuevo,dCurrentDateTime AS FechaHora, NumeroRegistroCambiado FROM rsCandidatesRecs "
			&sInsertSQLCmd 
			
			IF _TALLY>0 THEN
				sUpdateSQLCmd = "UPDATE " + This.sSourceTableName + " SET "  +;
								+ rsVarsMapping.NombreVariable + " = '" +	rsVarsMapping.ValorNuevo + "'" +;
								" WHERE " + STRTRAN(This.sDefaultKey,',','+') + " IN (SELECT claveRegistroCambiado FROM rsCandidatesRecs)"
				&sUpdateSQLCmd 
				This.nUpdatedRecs = This.nUpdatedRecs + _TALLY
				nLocalCounter = nLocalCounter + _TALLY
				
				*Actualiza el recordset de registros ya homologados			
				sInsertSQLCmd = "INSERT INTO rsUpdatedRecs (claveRegistroCambiado, NombreVariable) SELECT claveRegistroCambiado,'" + ;
								 ALLTRIM(rsVarsMapping.NombreVariable) + "' FROM rsCandidatesRecs "
				&sInsertSQLCmd
			ENDIF
		ENDIF
	ENDSCAN
		
	*Exporta el conjunto de registros modificado hacia un archivo excel
	IF nLocalCounter  > 0 THEN
		This.exportToXLS(This.sSourceTableName, "DatosHomologados", "_HomologacionVariables_" + sEventCode, dCurrentDateTime)
	ENDIF

	*Cierra las tablas utilizadas durante el proceso
	IF !bSourceTableWasOpen THEN
		SELECT (This.sSourceTableName)
		USE
	ENDIF

	TRY
		SELECT DatosHomologados 
		USE
	CATCH TO oException
	ENDTRY
	
	
	SET TALK &sOldSetTalk
	WAIT CLEAR

ENDPROC


PROCEDURE translateVariablesEventFree

	LPARAMETERS bUseSelectorCriteria as Boolean
	
	*Ejecuta el proceso de homolgación de variables en los registros de This.sSourceTableName según se ;
	encuentra definido en Databases.AnomaliesDB.Views.vHomologaciones
	
	*bUseSelectorCriteria (opcional) indica si debe utilizarse el criterio de selección que se encuentra ;
	establecido en Databases.AnomaliesDB.Views.vHomologaciones para seleccionar los registros objeto de ;
	homologación
	
	
	LOCAL nLocalCounter as Number  
	
	
	sOldSetTalk=SET("Talk")
	SET TALK ON
	
	bSourceTableWasOpen=.F.
	IF !USED(This.sSourceTableName) THEN
		USE (This.sSourceTableName) IN 0
		SELECT (This.sSourceTableName)
	ELSE
		SELECT (This.sSourceTableName)
		bSourceTableWasOpen=.T.
	ENDIF

		
	*Establece los campos de la clave por defecto para la tabla This.sSourceTableName
	oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,This.sSourceTableName)
	= oAnomaliesSolver.setDefaultKey()
	This.sDefaultKey=oAnomaliesSolver.sDefaultKey 
	
	*Elimina duplicados de la fuente de datos a convertir
	*oAnomaliesSolver.sSourceTableName=This.sSourceTableName
	*oAnomaliesSolver.sXlsFilePath = This.sXlsFilePath 
	*oAnomaliesSolver.deleteDuplicates()
	*RELEASE oAnomaliesSolver
	
	*Establece el conjunto de conversiones a aplicar
	sSelectSQLCmd="SELECT * FROM anomaliesDB!vHomologaciones WHERE NombreTabla='" + This.sSourceTableName + "' INTO CURSOR rsVarsMapping"
	&sSelectSQLCmd
		
	*Ejecuta las conversiones
	IF !This.bBeSilent THEN
		WAIT "Homologando variables en la tabla " + This.sSourceTableName WINDOW NOWAIT
	ENDIF
	
	nLocalCounter = 0
	dCurrentDateTime=DATETIME()
	
	* Crea un recordset que mantendrá las claves de los registros que van siendo homologados. Esto es necesario para ;
	que sobre un mismo registro no se haga más de una homologación debida, por ejemplo, a que para una misma variable;
	de un evento, se deban homologar varios valores que podrían conducir a que sobre un mismo registro se efectuen ;
	varios cambios sobre la misma variable cuando en realidad solo aplica uno.
	sSelectSQLCmd  = "SELECT SPACE(45) AS claveRegistroCambiado, SPACE(128) AS NombreVariable FROM " + This.sSourceTableName + ;
					" WHERE .F. INTO CURSOR rsUpdatedRecs READWRITE"
	&sSelectSQLCmd  
	
	SELECT rsVarsMapping
	SCAN
		*Establece el conjunto de registros candidatos a ser actualizados
		sSelectSQLCmd = "SELECT '" + This.sSourceTableName+ "' AS TablaObjetivo, " + STRTRAN(This.sDefaultKey,',','+') +;
						" AS claveRegistroCambiado, '" +	rsVarsMapping.NombreVariable + "' AS nombreVariable," +;
						"'" + rsVarsMapping.ValorOriginal + "' AS ValorOriginal,"  +;
						" RECNO() AS NumeroRegistroCambiado FROM " + This.sSourceTableName +;
						" WHERE .T. "
		IF 	rsVarsMapping.ValorOriginal!='*' THEN
			sSelectSQLCmd = sSelectSQLCmd +	" AND " + rsVarsMapping.NombreVariable + " = '" +	rsVarsMapping.ValorOriginal + "'" 
		ENDIF
		IF bUseSelectorCriteria THEN
			sSelectSQLCmd = sSelectSQLCmd +	" AND (" + rsVarsMapping.CriterioSelector + ")" 
		ENDIF
		IF !ISNULL(This.sFilterExpresion) THEN
			sSelectSQLCmd = sSelectSQLCmd +	" AND (" + This.sFilterExpresion + ")" 
		ENDIF
		sSelectSQLCmd = sSelectSQLCmd + " AND " + STRTRAN(This.sDefaultKey,',','+') + "+'" + ALLTRIM(rsVarsMapping.NombreVariable) + ;
						"'  NOT IN (SELECT ALLTRIM(claveRegistroCambiado) + ALLTRIM(NombreVariable) FROM rsUpdatedRecs)"
		sSelectSQLCmd = sSelectSQLCmd + " INTO CURSOR rsCandidatesRecs NOFILTER"
		&sSelectSQLCmd 
		
		IF _TALLY>0 THEN
			sInsertSQLCmd = "INSERT INTO DatosHomologados (TablaObjetivo, claveRegistroCambiado, nombreVariable, ValorOriginal, ValorNuevo, FechaHora, NumeroRegistroCambiado) " +;
							"SELECT TablaObjetivo, claveRegistroCambiado, nombreVariable," +;
							"ValorOriginal,'" + rsVarsMapping.ValorNuevo + "'" +;
							" AS valorNuevo,dCurrentDateTime AS FechaHora, NumeroRegistroCambiado FROM rsCandidatesRecs "
			&sInsertSQLCmd 
			
			IF _TALLY>0 THEN
				sUpdateSQLCmd = "UPDATE " + This.sSourceTableName + " SET "  + rsVarsMapping.NombreVariable 
				IF rsVarsMapping.TipoValorNuevo = 'C' THEN
					sUpdateSQLCmd = sUpdateSQLCmd + " = '" + rsVarsMapping.ValorNuevo + "'" 
				ELSE
					sUpdateSQLCmd = sUpdateSQLCmd + " = " + rsVarsMapping.ValorNuevo
				ENDIF
				sUpdateSQLCmd = sUpdateSQLCmd + " WHERE " + STRTRAN(This.sDefaultKey,',','+') + " IN (SELECT claveRegistroCambiado FROM rsCandidatesRecs)"
				&sUpdateSQLCmd 
				This.nUpdatedRecs = This.nUpdatedRecs + _TALLY
				nLocalCounter = nLocalCounter + _TALLY
				
				*Actualiza el recordset de registros ya homologados			
				sInsertSQLCmd = "INSERT INTO rsUpdatedRecs (claveRegistroCambiado, NombreVariable) SELECT claveRegistroCambiado,'" + ;
								 ALLTRIM(rsVarsMapping.NombreVariable) + "' FROM rsCandidatesRecs "
				&sInsertSQLCmd
			ENDIF
		ENDIF
	ENDSCAN
		
	*Exporta el conjunto de registros modificado hacia un archivo excel
	IF nLocalCounter  > 0 THEN
		This.exportToXLS(This.sSourceTableName, "DatosHomologados", "_HomologacionVariables" , dCurrentDateTime)
	ENDIF

	*Cierra las tablas utilizadas durante el proceso
	IF !bSourceTableWasOpen THEN
		SELECT (This.sSourceTableName)
		USE
	ENDIF

	TRY
		SELECT DatosHomologados 
		USE
	CATCH TO oException
	ENDTRY
	
	
	SET TALK &sOldSetTalk
	WAIT CLEAR

ENDPROC


PROCEDURE ShowResults

	LOCAL sResultsMessage as String
	
	sResultsMessage= 'Número de homologaciones: ' + ALLTRIM(STR(This.nUpdatedRecs)) + CHR(13) 
	
	IF THIS.nUpdatedRecs> 0 THEN
		sResultsMessage = sResultsMessage + CHR(13) +;
						  'Se ha generado uno o más archivos en formato compatible con Excel 2007 en: ' ;
						  + CHR(13) + This.sXlsFilePath 
	ENDIF
	
	MESSAGEBOX( sResultsMessage,  0 + 48 , sWindowTitle ) 

ENDPROC


PROCEDURE exportToXLS

	LPARAMETERS rsToExportName AS String, rsModifiedRecordsName as String, sXLSFileNameSuffix as String, ;
				sDateTime AS Datetime
	
	*Exporta los registros de rsToExportName cuyo número de registro aparezca en rsModifiedRecordsName y que cumplan la ;
	condición rsModifiedRecordsName.FechaHora= sDateTime a un archivo con formato XLS o CSV, dependiendo de si el número ;
	de registros a exportar es menor o mayor que nMaxXLSRows.
	
	*El archivo exportado tendrá nombre rsToExportName + sXLSFileNameSuffix y quedará ubicado en This.xlsFilePath . ;
	
	
	LOCAL sExportedFileType as String, xlsFileName as String
	
	WITH This
		bRSToExportWasOpen=USED(rsToExportName)

		SQLClause="SELECT A.* FROM " + ;
					"(SELECT RECNO() AS nReg, * FROM " + rsToExportName + ") AS A INNER JOIN " + rsModifiedRecordsName + ;
					" ON A.nReg = " + rsModifiedRecordsName + ".NumeroRegistroCambiado WHERE " + rsModifiedRecordsName + ;
					".FechaHora=sDateTime INTO CURSOR rsExportableRecords"
		&SQLClause
		SELECT rsExportableRecords
		
		IF RECCOUNT()>nMaxRowsForXLS THEN
			*El número de registros a exportar es de tal magnitud que se hace la exportacióon hacia un archivo CSV;
			que es leido en forma nativa por Excel 2007 o superior
			sExportedFileType='CSV'
		ELSE
			sExportedFileType='XL5'
		ENDIF

		xlsFileName = rsToExportName  + sXLSFileNameSuffix
		WAIT sExportingMsg WINDOW NOWAIT
		sCopyToCmd="COPY TO [" + This.sXlsFilePath + "\" + xlsFileName + "] TYPE " + sExportedFileType
		&sCopyToCmd
		USE
		
		IF !bRSToExportWasOpen THEN
			SELECT (rsToExportName)
			USE
		ENDIF
		
		WAIT CLEAR
	ENDWITH

ENDPROC


PROCEDURE standarizeComparableEvents

	SET PROCEDURE TO EventsHandler ADDITIVE

	*Abre el conjunto de eventos homologables
	USE anomaliesDB!vEventosHomologables IN 0
	SELECT vEventosHomologables 
	
	*Ejecuta la traducción de variables para cada evento homologable
	SCAN
		*Establece en qué tabla se guardan los registros del evento con código sEventCode 
		This.sSourceTablename = GetWarehouseTableName(vEventosHomologables.COD_EVE)
		
		This.translateVariables(vEventosHomologables.COD_EVE ,.T.)
	ENDSCAN
	SELECT vEventosHomologables 
	USE
	
ENDPROC




ENDDEFINE && VariableTranslator 

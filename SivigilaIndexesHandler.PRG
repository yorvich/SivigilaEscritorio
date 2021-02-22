

PROCEDURE updateTableIndexMetaData

	LPARAMETERS sourceTableName, indexesTableName

	IF FILE(sourceTableName + '.DBF') THEN	
		IF !USED(sourceTableName) THEN
			USE (sourceTableName) IN 0
		ELSE
			SELECT (sourceTableName)
		ENDIF
		nTableIndexes=ATAGINFO(aTableIndexes,'',sourceTableName)
		delSQLClause="DELETE FROM " + indexesTableName + " WHERE TABLE_NAME='" + sourceTableName + "'"
		&delSQLClause
		FOR i=1 TO nTableIndexes
			INSERT INTO (indexesTableName) (table_name, tag_name, tag_expr) VALUES (sourceTableName, aTableIndexes(i,1), aTableIndexes(i,3) ) 
		ENDFOR
	ENDIF
ENDPROC

PROCEDURE updateSIVIGILATableIndexMetaData

	LPARAMETERS maxEvents as Byte
		
	DO updateTableIndexMetaData WITH 'UPGD','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'TAL_HUM','MASTER_INDEX'	
	DO updateTableIndexMetaData WITH 'DATOS_CE','MASTER_INDEX'	
	DO updateTableIndexMetaData WITH 'FM_GRADOS','MASTER_INDEX'	
	DO updateTableIndexMetaData WITH 'FM_UNIDADES','MASTER_INDEX'	
	DO updateTableIndexMetaData WITH 'ENTRA_01','MASTER_INDEX'	
	DO updateTableIndexMetaData WITH 'ENTRA_02','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'ENTRA_03','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'ARTICULOS','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'OCUPACIONES','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'UPGD_HAB','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'PAISES','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'DEPTOS','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'MUNICIPIOS','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'SECTORES','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'ASEGURADORAS','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'EVENTOS','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'CALENDARIO','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'NOTIFICADOS','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'CIE10','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'PRUEBAS','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'AGENTES','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'AGENTES_ETAS','MASTER_INDEX'
	
	FOR nEvent=1 TO maxEvents
		sEventFileName='EVENTOS_' + PADL(nEvent,2,'0')
		DO updateTableIndexMetaData WITH sEventFileName,'MASTER_INDEX'
	ENDFOR
	DO updateTableIndexMetaData WITH 'LABORATORIOS','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'BROTES','MASTER_INDEX'
	DO updateTableIndexMetaData WITH 'PACIENTE','MASTER_INDEX'
	
ENDPROC	


PROCEDURE updateSIVIGILATableIndexes

	LOCAL sResultMsg as string 
	sResultMsg = ''
	
	LOCAL bContinue as Boolean 
	bContinue = .T.
	
	CLOSE ALL
	
	
	vExclusiveState=SET("Exclusive")
	vTalkState=SET("Talk")
	vSafetyState=SET("Safety")
	
	SET EXCLUSIVE ON
	SET TALK ON
	SET SAFETY OFF
*SET STEP ON 
	
	* Dado que la sesión de datos 2 mantiene las tablas de registro de errores del manejador de errores del SIVIGILA;
	y por tanto los archivos de la base de datos BDSIVIGILA están abiertos, es necesario cerrarlos y restablecer la sesión de datos ;
	tal como se encontraba antes del cierre 
	sOldOnError = ON('ERROR')
	ON ERROR DO ErrHandler WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()
			
	SET DATASESSION TO 2
	sOldSafety = SET("Safety")
	SET SAFETY OFF
	CREATE VIEW errDataSessionView
	SET SAFETY &sOldSafety
	CLOSE ALL

	SET DATASESSION TO 1
	TRY
		OPEN DATABASE BDSivigila EXCLUSIVE
		VALIDATE DATABASE RECOVER NOCONSOLE
		bContinue = .T.
	CATCH TO oException
		bContinue = .F.
		IF oException.ErrorNo=1705THEN
			sResultMsg="Se produjo el siguiente problema al intentar reindexar: " +  CHR(13) +  CHR(10) +;
						"La base de datos del sistema se encuentra abierta por otro usuario/aplicativo."
		ELSE
			sResultMsg="Se produjo el siguiente problema al intentar reindexar: " +  CHR(13) +  CHR(10) +;
						"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
						"Instrucción que produjo el problema : " + oException.Procedure + " - " + oException.LineContents + ;
							" - Línea " + ALLTRIM(STR(oException.LineNo)) +CHR(13) +;
						"Detalles del problema : " + oException.Details + CHR(13) +;
						"Mensaje del problema : "  + (oException.Message) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) 
		ENDIF
		MESSAGEBOX( sResultMsg, 0 + 48 + 256, 'Error de indexación')
				
	ENDTRY
	
	IF bContinue THEN 
		*SELECT DISTINCT TABLE_NAME FROM MASTER_INDEX WHERE !LIKE('PLANO*',UPPER(TABLE_NAME)) INTO CURSOR rsSIVIGILATables
		SELECT DISTINCT TABLE_NAME FROM MASTER_INDEX WHERE ALLTRIM(TABLE_NAME) IN (SELECT ALLTRIM(DBF_NAME) FROM SIVIGILADataTables) ;
			INTO CURSOR rsSIVIGILATables
		SELECT rsSIVIGILATables
		SCAN
			sourceTableName=ALLTRIM(rsSIVIGILATables.TABLE_NAME)
			DEFINE WINDOW msgWindow AT 1,40 SIZE 5,100 FONT "Arial" TITLE "Generando indices para la tabla " + (sourceTableName) SYSTEM
			ACTIVATE WINDOW msgWindow 
			SET TALK WINDOW msgWindow

			USE (sourceTableName) IN 0
			SELECT (sourceTableName)
			DELETE TAG ALL
			
			SELECT * FROM MASTER_INDEX WHERE TABLE_NAME==(sourceTableName) INTO CURSOR rsSIVIGILATableIndexes
			SELECT rsSIVIGILATableIndexes
			SCAN
				sIndexExpression=ALLTRIM(TAG_EXPR)
				sTagName=ALLTRIM(TAG_NAME)
				SELECT (sourceTableName)
				TRY
					INDEX ON &sIndexExpression TAG &sTagName
				CATCH TO oException
					IF oException.ErrorNo=1561 THEN
						*La BD BDSivigila se ha vuelto inválida; sin embargo su recuperación es automática y está a cargo;
						del FoxPro
					ENDIF
				ENDTRY
				SELECT rsSIVIGILATableIndexes
			ENDSCAN
			SELECT rsSIVIGILATables
		ENDSCAN
		USE IN rsSIVIGILATables
		
		RELEASE WINDOWS msgWindow 
		CLOSE ALL
		SET EXCLUSIVE &vExclusiveState
		SET TALK &vTalkState
		SET SAFETY &vSafetyState
		CLOSE DATABASES
		
		MESSAGEBOX( "Se han generado todos los índices de todas las tablas del sistema." + CHR(13) + ;
					"El Sistema se cerrará.", 0 + 48 + 256, 'SIVIGILA' )
		QUIT
	ENDIF 

	SET DATASESSION TO 2
	SET VIEW TO errDataSessionView
	SET DATASESSION TO 1
	ON ERROR &sOldOnError
ENDPROC


PROCEDURE addIndexToSIVIGILADataTables

	LPARAMETERS sIndexExpression as String, sTagName as String

	CLOSE ALL
	vExclusiveState=SET("Exclusive")
	vTalkState=SET("Talk")
	vSafetyState=SET("Safety")
	
	SET EXCLUSIVE ON
	SET TALK ON
	SET SAFETY OFF
		
	SELECT DISTINCT TABLE_NAME FROM MASTER_INDEX WHERE (TABLE_NAME LIKE 'EVENTOS___' OR ;
		TABLE_NAME = 'PACIENTE' OR TABLE_NAME = 'BROTES' OR TABLE_NAME = 'LABORATORIO' OR ;
		TABLE_NAME = 'TAL_HUM' OR TABLE_NAME == 'UPGD') ;
		AND !(TABLE_NAME=='EVENTOS') INTO CURSOR rsSIVIGILATables
	SELECT rsSIVIGILATables
	SCAN
		sourceTableName=ALLTRIM(rsSIVIGILATables.TABLE_NAME)
		DEFINE WINDOW msgWindow AT 1,40 SIZE 5,100 FONT "Arial" TITLE "Agregando índice a la tabla " + (sourceTableName) SYSTEM
		ACTIVATE WINDOW msgWindow 
		SET TALK WINDOW msgWindow

		USE (sourceTableName) IN 0
		TRY 
			INDEX ON &sIndexExpression TAG &sTagName
		ENDTRY
		SELECT rsSIVIGILATableIndexes
		SELECT rsSIVIGILATables
	ENDSCAN
	RELEASE WINDOWS msgWindow 
	CLOSE ALL
	SET EXCLUSIVE &vExclusiveState
	SET TALK &vTalkState
	SET SAFETY &vSafetyState
	
	MESSAGEBOX( "Se ha agregado el índice a todas las tablas de datos del sistema." + CHR(13) + ;
				"El Sistema se cerrará.", 0 + 48 + 256, 'SIVIGILA' )
	&&QUIT

ENDPROC


PROCEDURE addIndexToTable

	LPARAMETERS sSourceTableName as string, sIndexExpression as String, sTagName as String

	LOCAL bSourceTableIsOpenByAnotherUser as Boolean
	
	DIMENSION aTableIndexes(1,1)


	sIndexExpression = CHRTRAN(sIndexExpression,' ','')

	vExclusiveState=SET("Exclusive")
	vTalkState=SET("Talk")
	vSafetyState=SET("Safety")

	SET EXCLUSIVE ON
	SET TALK ON
	SET SAFETY OFF

	bSourceTableIsOpen=.F.
	bSourceTableIsOpenByAnotherUser = .F.
	
	*Intenta abrir sSourceTableName en modo exclusivo para poder agregar el índice
	IF USED(sSourceTableName) THEN
		SELECT (sSourceTableName)
		nSourceTableOldWorkArea=SELECT(0)
		USE
		bSourceTableIsOpen=.T.
	ENDIF
	
	TRY
		USE (sSourceTableName) IN 0
		SELECT (sSourceTableName)	
	CATCH TO oException
		*sSourceTableName no se pudo abrir en modo exclusivo y por tanto se restablecen las condiciones ;
		de datos previas al intento. El error que se produce es el 1705 - File access is denied.
		bSourceTableIsOpenByAnotherUser = .T.
		
		SET EXCLUSIVE &vExclusiveState
		SET TALK &vTalkState
		SET SAFETY &vSafetyState

		IF bSourceTableIsOpen THEN
			USE (sSourceTableName) IN (nSourceTableOldWorkArea)
		ENDIF
	ENDTRY
	
	IF !bSourceTableIsOpenByAnotherUser THEN
		*Verifica si el índice ya existe en la tabla sSourceTableName 
		nTableIndexes=ATAGINFO(aTableIndexes)
		nTagName = ASCAN(aTableIndexes, sTagName , -1, -1 ,-1, 15)
		IF nTagName=0 THEN
			*El nombre del TAG no existe en la tabla.
			
			*Verifica si la clave sIndexExpression existe para algún índice de la tabla
			nKeyFields = ASCAN(aTableIndexes, sIndexExpression, -1, -1 ,-1, 15)
			IF nKeyFields=0 THEN
				*La clave no existe y se puede intentar el indexamiento
				DEFINE WINDOW msgWindow AT 1,40 SIZE 5,100 FONT "Arial" TITLE "Agregando índice a la tabla " + (sSourceTableName) SYSTEM
				ACTIVATE WINDOW msgWindow 
				SET TALK WINDOW msgWindow

				TRY 
					INDEX ON &sIndexExpression TAG &sTagName
				CATCH
				ENDTRY
				RELEASE WINDOWS msgWindow 
			ENDIF
		ENDIF
		
		USE
		
		SET EXCLUSIVE &vExclusiveState
		SET TALK &vTalkState
		SET SAFETY &vSafetyState
		
		IF bSourceTableIsOpen THEN
			USE (sSourceTableName) IN (nSourceTableOldWorkArea)
		ENDIF
	ENDIF

ENDPROC


PROCEDURE ErrHandler

	PARAMETERS nErrNumber, sErrMessage, sProgramName, nErrLine

	LOCAL sErrorMsg AS String
	
	sErrorMsg=	"Se ha producido un error inesperado al hacer backup." + CHR(13) + CHR(13) +;
				"Código interno de error: " + ALLTRIM(STR(nErrNumber)) + CHR(13) +;
				"Mensaje de error: "  + (sErrMessage) + CHR(13) +;
				"Módulo del error: "  + (sProgramName) + CHR(13) +;
				"Línea de error: "  + LTRIM(STR(nErrLine)) + CHR(13)
												
	MESSAGEBOX(sErrorMsg , 0 + 48 + 256, 'Error desconocido - SIVIGILA' )
	
ENDPROC
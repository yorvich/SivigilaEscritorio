#INCLUDE SIVIGILA.h

#DEFINE sBASICA_DATA_DISPENSABLE_FIELDS " ESTADOTRAN NUM_REG SUBE INMEDIATA AJUSTE NREG EST_NOTIF EST_INGR "
*Lista de campos que no deben tenerse en cuenta para resolver duplicados en tablas de datos básicos


FUNCTION solveDuplicates

	LPARAMETERS sSourceTableName as String, sXLSFilesPath as String

	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	SET PROCEDURE TO IMPORTAR_CLICK ADDITIVE
	
	sOldProcedureFile=SET("Procedure")

	DO CASE 
		CASE 'UPGD' $ sSourceTableName OR 'PLANO_UPGD' $ sSourceTableName
			*Establece si en sSourceTableName existen registros duplicados 
			sDuplicatesCursorName=''
			detectionResult = extractDupsAndNotDups(sSourceTableName,@sDuplicatesCursorName, ,'PLANO_UPGD')
			IF detectionResult=1 THEN
				nDuplicates=0
			ELSE
				nDuplicates=RECCOUNT(sDuplicatesCursorName)
				SELECT (sDuplicatesCursorName)
				SELECT nReg, .T. as BORRAR FROM &sDuplicatesCursorName INTO CURSOR rsRecordsToDelete READWRITE
				SELECT C_COUNT_DUPS
				SCAN
					sUPGDCode=C_COUNT_DUPS.COD_PRE
					*Intenta resolver los registros duplicados teniendo en cuenta la fecha de caracterización. ;
					Según este criterio se escoje como registro válido aquel que tenga la Fecha de caracterización;
					más reciente
					SELECT DISTINCT FEC_CAR FROM &sDuplicatesCursorName WHERE COD_PRE=C_COUNT_DUPS.COD_PRE AND ;
						COD_SUB=C_COUNT_DUPS.COD_SUB INTO ARRAY aDuplicatesDates
					IF _TALLY>1 THEN
						*Se puede utilizar el criterio de la Fecha de caracterización para resolver el duplicado;
						por cuanto existen registros duplicados pero con fechas de caracterización diferentes
						
						*Determina el número del registro que tenga la fecha de caracterización más reciente. ;
						Si existiesen registros con igual fecha de caracterización, se escogerá la UPGD que esté;
						activa en el sistema, si la hay.
						SELECT TOP 1 nReg FROM &sDuplicatesCursorName WHERE COD_PRE=C_COUNT_DUPS.COD_PRE AND ;
							COD_SUB=C_COUNT_DUPS.COD_SUB ORDER BY FEC_CAR DESC, ACT_SIV ASC INTO ARRAY aRecordsToRetain
						
						*Marca en la tabla rsRecordsToDelete el registro recien determinado de tal forma que no sea borrado
						UPDATE rsRecordsToDelete SET BORRAR = .F. WHERE nReg = aRecordsToRetain(1,1)
					ELSE
						*Intenta resolver los registros duplicados teniendo en cuenta que la UPGD aparezca como activa. ;
						Según este criterio se escoje como registro válido aquel que tenga el atributo ACT_SIV=1 (la UPGD está activa);
						y que sea la más reciente, esto es, que tenga el mayor número de registro
						SELECT nReg, ACT_SIV FROM &sDuplicatesCursorName WHERE COD_PRE=C_COUNT_DUPS.COD_PRE AND ;
							COD_SUB=C_COUNT_DUPS.COD_SUB AND ACT_SIV=1 ORDER BY nReg DESC INTO ARRAY aRecordsToRetain
						IF _TALLY>=1 THEN
							*Se puede utilizar el criterio de actividad
							
							*Marca en la tabla rsRecordsToDelete el primer registro seleccionado de tal forma que no sea borrado
							UPDATE rsRecordsToDelete SET BORRAR = .F. WHERE nReg = aRecordsToRetain(1,1)
						ELSE
							*El registro duplicado se debe resolver teniendo en cuenta el número de registro con el que almacenó ;
							en la tabla sSourceTableName. ;
							Según este criterio se escoje como registro válido aquel que tenga el número de registro más grande
							SELECT TOP 1 nReg FROM &sDuplicatesCursorName WHERE COD_PRE=C_COUNT_DUPS.COD_PRE AND ;
								COD_SUB=C_COUNT_DUPS.COD_SUB ORDER BY nReg DESC INTO ARRAY aRecordsToRetain
							
							*Marca en la tabla rsRecordsToDelete el registro recien determinado de tal forma que no sea borrado
							UPDATE rsRecordsToDelete SET BORRAR = .F. WHERE nReg = aRecordsToRetain(1,1)
						ENDIF
					ENDIF
				ENDSCAN
				
				*Borra de sSourceTableName los registros marcados para borrar en rsRecordsToDelete
				SELECT rsRecordsToDelete
				INDEX on nReg TAG nRegIdx
				
				SELECT SUBSTR(sSourceTableName,RAT('\',sSourceTableName)+1)
				SET RELATION TO RECNO() INTO rsRecordsToDelete
				DELETE ALL FOR rsRecordsToDelete.BORRAR
				SET RELATION TO

				*Cierra/Elimina los objetos temporales generados durante el procesamiento
				SELECT rsRecordsToDelete
				USE
			ENDIF

			*Cierra/Elimina los objetos temporales generados durante el procesamiento
			SELECT SUBSTR(sSourceTableName,RAT('\',sSourceTableName)+1)
			USE
			SELECT C_Dups_UPGD
			USE
			DELETE FILE 'C_Dups_UPGD.DBF'
			SELECT T_NonDups_UPGD
			USE
			DELETE FILE 'T_NonDups_UPGD.DBF'
			SELECT C_Count_Dups				
			USE

		CASE 'PLANO_PACIENTES' $ sSourceTableName
			*Establece si en sSourceTableName existen registros duplicados 
			sDuplicatesCursorName=''
			detectionResult = extractDupsAndNotDups(sSourceTableName,@sDuplicatesCursorName, ,'PLANO_PACIENTES')
			IF detectionResult=1 THEN
				nDuplicates=0
			ELSE
				nDuplicates=RECCOUNT(sDuplicatesCursorName)
				SELECT (sDuplicatesCursorName)
				INDEX ON AJUSTE + AÑO + SEMANA + COD_EVE + TIP_IDE + NUM_IDE + COD_PRE + COD_SUB TAG PK
				SELECT nReg, .T. as BORRAR FROM &sDuplicatesCursorName INTO CURSOR rsRecordsToDelete READWRITE

				SELECT C_COUNT_DUPS
				SCAN
					*Intenta resolver los registros duplicados teniendo en cuenta la igualdad de todas las variables no incluidas ;
					en la clave del registro definida en Tables.SIVIGILAPKs ;
					Según este criterio, si todas esas variables son iguales dentro del conjunto de duplicados, cualquiera de ellos;
					se escoje como registro válido
					sKeyFields = ALLTRIM(fieldValue("TAG_EXPR", "SIVIGILAPKs","TABLE_NAME='PACIENTE'"))
					sSQLCmd = "SELECT DISTINCT "
					DIMENSION aDuplicatesFields(1)
					nDuplicatesFields = AFIELDS(aDuplicatesFields,sDuplicatesCursorName)
					FOR nField=1 TO nDuplicatesFields 
						IF !(aDuplicatesFields(nField,1) $ sBASICA_DATA_DISPENSABLE_FIELDS) AND !(aDuplicatesFields(nField,1) $ sKeyFields)  THEN
							sSQLCmd = sSQLCmd + aDuplicatesFields(nField,1) + ','
						ENDIF
					NEXT nField
					sSQLCmd = LEFT(sSQLCmd,LEN(sSQLCmd)-1)
					
					sLinkerFields = ALLTRIM(fieldValue("TAG_EXPR", "MASTER_INDEX","TABLE_NAME='PLANO_PACIENTES' AND TAG_NAME='DUPLICATES'"))
					sSQLCmd = sSQLCmd + " FROM " + sDuplicatesCursorName + " WHERE " + sLinkerFields + "='"
								
					nLinkerFields = ALINES(aLinkerFields, sLinkerFields, 1, '+')
					FOR nField=1 TO nLinkerFields 
						sSQLCmd = sSQLCmd + C_COUNT_DUPS.&aLinkerFields(nField)
					NEXT nField
					sSQLCmd = sSQLCmd + "' INTO ARRAY aDuplicatesRecords"
					&sSQLCmd 
					
					IF _TALLY=1 THEN
						*Se puede utilizar el criterio de igualdad de todas las variables para resolver el duplicado
						
						*Determina el número del registro que tenga la fecha de caracterización más reciente. ;
						Si existiesen registros con igual fecha de caracterización, se escogerá la UPGD que esté;
						activa en el sistema, si la hay.
						SELECT TOP 1 nReg FROM &sDuplicatesCursorName WHERE AJUSTE + AÑO + SEMANA + COD_EVE + TIP_IDE + NUM_IDE + ;
						COD_PRE + COD_SUB = C_COUNT_DUPS.AJUSTE + C_COUNT_DUPS.AÑO + C_COUNT_DUPS.SEMANA + C_COUNT_DUPS.COD_EVE + ;
						C_COUNT_DUPS.TIP_IDE + C_COUNT_DUPS.NUM_IDE + C_COUNT_DUPS.COD_PRE + C_COUNT_DUPS.COD_SUB ;
						ORDER BY FEC_NOT INTO ARRAY aRecordsToRetain
						
						*Marca en la tabla rsRecordsToDelete el registro recien determinado de tal forma que no sea borrado
						UPDATE rsRecordsToDelete SET BORRAR = .F. WHERE nReg = aRecordsToRetain(1,1)
					ELSE
						*No se pueden resolver los duplicados
						SELECT nReg FROM &sDuplicatesCursorName WHERE AJUSTE + AÑO + SEMANA + COD_EVE + TIP_IDE + NUM_IDE + COD_PRE + COD_SUB = ;
							C_COUNT_DUPS.AJUSTE + C_COUNT_DUPS.AÑO + C_COUNT_DUPS.SEMANA + C_COUNT_DUPS.COD_EVE + C_COUNT_DUPS.TIP_IDE ;
							+ C_COUNT_DUPS.NUM_IDE + C_COUNT_DUPS.COD_PRE + C_COUNT_DUPS.COD_SUB INTO CURSOR rsNonDeletables
						UPDATE rsRecordsToDelete SET BORRAR = .F. FROM rsNonDeletables WHERE rsRecordsToDelete.nReg = rsNonDeletables.nReg 
					ENDIF
				ENDSCAN

				*Borra de sSourceTableName los registros marcados para borrar en rsRecordsToDelete
				SELECT rsRecordsToDelete
				INDEX on nReg TAG nRegIdx
				
				SELECT SUBSTR(sSourceTableName,RAT('\',sSourceTableName)+1)
				SET RELATION TO RECNO() INTO rsRecordsToDelete
				DELETE ALL FOR rsRecordsToDelete.BORRAR
				IF _TALLY>0 THEN
					IF VARTYPE(sXLSFilesPath)='C' THEN
						sOldDeleted=SET("Deleted")
						SET DELETED OFF
						sDuplicatesDeletedExcelFileName=sXLSFilesPath + '\' + SUBSTR(sSourceTableName,RAT('\',sSourceTableName)+1)+ '_duplicados'
						COPY TO [&sDuplicatesDeletedExcelFileName] XL5 FOR DELETED()
						SET DELETED &sOldDeleted
					ENDIF
				ENDIF
				SET RELATION TO

				*Cierra/Elimina los objetos temporales generados durante el procesamiento
				SELECT rsRecordsToDelete
				USE
			ENDIF

			*Cierra/Elimina los objetos temporales generados durante el procesamiento
			SELECT SUBSTR(sSourceTableName,RAT('\',sSourceTableName)+1)
			USE
			SELECT C_Dups_PACIENTES
			USE
			DELETE FILE 'C_Dups_PACIENTES.DBF'
			SELECT T_NonDups_PACIENTES
			USE
			DELETE FILE 'T_NonDups_PACIENTES.DBF'
			SELECT C_Count_Dups				
			USE

		CASE 'PLANO_BROTES' $ sSourceTableName
			*Establece si en sSourceTableName existen registros duplicados
			sDuplicatesCursorName=''
			detectionResult = extractDupsAndNotDups(sSourceTableName,@sDuplicatesCursorName, ,'PLANO_BROTES')
			IF detectionResult=1 THEN
				nDuplicates=0
			ELSE
				nDuplicates=RECCOUNT(sDuplicatesCursorName)
				SELECT (sDuplicatesCursorName)
				INDEX ON COD_EVE + AÑO + SEMANA + COD_MUN + NUM_CON + COD_PRE + COD_SUB TAG PK
				sDefaultKey = ''
				nSourceTableFields=AFIELDS(SourceTableFields)
				FOR nField=1 TO nSourceTableFields
					IF !(SourceTableFields(nField,1) $ " NREG NUM_REG SUBE COD_EVE AÑO SEMANA COD_MUN NUM_CON COD_PRE COD_SUB FEC_NOT ") THEN
						sDefaultKey = sDefaultKey + SourceTableFields(nField,1) + ","
					ENDIF
				NEXT nField
				sDefaultKey = SUBSTR(sDefaultKey,1,LEN(sDefaultKey)-1)

				SELECT nReg, .T. as BORRAR FROM &sDuplicatesCursorName INTO CURSOR rsRecordsToDelete READWRITE

				SELECT C_COUNT_DUPS
				SCAN
					*Intenta resolver los registros duplicados teniendo en cuenta la igualdad de todas las variables no incluidas ;
					en la clave del registro. ;
					Según este criterio, si todas esas variables son iguales dentro del conjunto de duplicados, cualquiera de ellos;
					se escoje como registro válido
					sSelectSQLCmd="SELECT DISTINCT " + sDefaultKey + " FROM &sDuplicatesCursorName WHERE " +;
									"COD_EVE + AÑO + SEMANA + COD_MUN + NUM_CON + COD_PRE + COD_SUB = " +;
									"C_COUNT_DUPS.COD_EVE + C_COUNT_DUPS.AÑO + C_COUNT_DUPS.SEMANA + " +;
									"C_COUNT_DUPS.COD_MUN + C_COUNT_DUPS.NUM_CON + C_COUNT_DUPS.COD_PRE + " +;
									"C_COUNT_DUPS.COD_SUB INTO ARRAY aDuplicatesRecords"
					&sSelectSQLCmd
					IF _TALLY=1 THEN
						*Se puede utilizar el criterio de igualdad de todas las variables para resolver el duplicado
						
						SELECT TOP 1 nReg FROM &sDuplicatesCursorName WHERE COD_EVE + AÑO + SEMANA + COD_MUN + NUM_CON + COD_PRE + COD_SUB = ;
						C_COUNT_DUPS.COD_EVE + C_COUNT_DUPS.AÑO + C_COUNT_DUPS.SEMANA + C_COUNT_DUPS.COD_MUN + C_COUNT_DUPS.NUM_CON ;
						+ C_COUNT_DUPS.COD_PRE + C_COUNT_DUPS.COD_SUB ;
						ORDER BY nReg INTO ARRAY aRecordsToRetain
						
						*Marca en la tabla rsRecordsToDelete el registro recien determinado de tal forma que no sea borrado
						UPDATE rsRecordsToDelete SET BORRAR = .F. WHERE nReg = aRecordsToRetain(1,1)
					ELSE
						*No se pueden resolver los duplicados
						sSelectSQLCmd="SELECT nReg FROM &sDuplicatesCursorName WHERE " +;
										"COD_EVE + AÑO + SEMANA + COD_MUN + NUM_CON + COD_PRE + COD_SUB = " +;
										"C_COUNT_DUPS.COD_EVE + C_COUNT_DUPS.AÑO + C_COUNT_DUPS.SEMANA + " +;
										"C_COUNT_DUPS.COD_MUN + C_COUNT_DUPS.NUM_CON + C_COUNT_DUPS.COD_PRE + " +;
										"C_COUNT_DUPS.COD_SUB INTO CURSOR rsNonDeletables"
						&sSelectSQLCmd
						UPDATE rsRecordsToDelete SET BORRAR = .F. FROM rsNonDeletables WHERE rsRecordsToDelete.nReg = rsNonDeletables.nReg 
					ENDIF
				ENDSCAN
				
				*Borra de sSourceTableName los registros marcados para borrar en rsRecordsToDelete
				SELECT rsRecordsToDelete
				INDEX on nReg TAG nRegIdx
				
				SELECT SUBSTR(sSourceTableName,RAT('\',sSourceTableName)+1)
				SET RELATION TO RECNO() INTO rsRecordsToDelete
				DELETE ALL FOR rsRecordsToDelete.BORRAR
				SET RELATION TO
				IF _TALLY>0 THEN
					IF VARTYPE(sXLSFilesPath)='C' THEN
						sOldDeleted=SET("Deleted")
						SET DELETED OFF
						sDuplicatesDeletedExcelFileName=sXLSFilesPath + '\' + SUBSTR(sSourceTableName,RAT('\',sSourceTableName)+1)+ '_duplicados'
						COPY TO [&sDuplicatesDeletedExcelFileName] XL5 FOR DELETED()
						SET DELETED &sOldDeleted
					ENDIF
				ENDIF

				*Cierra/Elimina los objetos temporales generados durante el procesamiento
				SELECT rsRecordsToDelete
				USE
				
				IF USED("rsNonDeletables") THEN
					SELECT rsNonDeletables
					USE
				ENDIF
			ENDIF

			*Cierra/Elimina los objetos temporales generados durante el procesamiento
			SELECT SUBSTR(sSourceTableName,RAT('\',sSourceTableName)+1)
			USE
			SELECT C_Dups_BROTES
			USE
			DELETE FILE 'C_Dups_BROTES.DBF'
			SELECT T_NonDups_BROTES
			USE
			DELETE FILE 'T_NonDups_BROTES.DBF'
			SELECT C_Count_Dups				
			USE

		CASE 'TAL_HUM' $ sSourceTableName
			*Establece si en sSourceTableName existen registros duplicados
			sDuplicatesCursorName=''
			detectionResult = extractDupsAndNotDups(sSourceTableName,@sDuplicatesCursorName, ,'PLANO_RHS')
			IF detectionResult=1 THEN
				nDuplicates=0
			ELSE
				nDuplicates=RECCOUNT(sDuplicatesCursorName)
				SELECT (sDuplicatesCursorName)
				INDEX ON COD_PRE + COD_SUB + COD_ART + VALOR  TAG PK
				SELECT nReg, .T. as BORRAR FROM &sDuplicatesCursorName INTO CURSOR rsRecordsToDelete READWRITE
				SELECT C_COUNT_DUPS
				&&nRegsToProcess=RECCOUNT()
				SCAN
					*Intenta resolver los registros duplicados teniendo en cuenta el número de registro con el que almacenó en la tabla ;
					sSourceTableName. ;
					Según este criterio se escoje como registro válido aquel que tenga el número de registro más grande
					&&nCurrentRec=RECNO()
					&&WAIT 'Procesando registro ' + STR(nCurrentRec) + ' de ' + STR(nRegsToProcess) WINDOW NOWAIT TIMEOUT 1
					SELECT TOP 1 nReg FROM &sDuplicatesCursorName WHERE COD_PRE + COD_SUB + COD_ART + VALOR = ;
					C_COUNT_DUPS.COD_PRE + C_COUNT_DUPS.COD_SUB + C_COUNT_DUPS.COD_ART + C_COUNT_DUPS.VALOR ;
					ORDER BY nReg DESC INTO ARRAY aRecordsToRetain
					
					*Marca el registro recien determinado en la tabla rsRecordsToDelete de tal forma que no sea borrado
					UPDATE rsRecordsToDelete SET BORRAR = .F. WHERE nReg = aRecordsToRetain(1,1)						
				ENDSCAN
				
				*Borra de sSourceTableName los registros marcados para borrar en rsRecordsToDelete
				SELECT rsRecordsToDelete
				INDEX on nReg TAG nRegIdx
				
				SELECT (sSourceTableName)
				SET RELATION TO RECNO() INTO rsRecordsToDelete
				DELETE ALL FOR rsRecordsToDelete.BORRAR
				SET RELATION TO

				*Cierra/Elimina los objetos temporales generados durante el procesamiento
				SELECT rsRecordsToDelete
				USE
			ENDIF
			*Cierra/Elimina los objetos temporales generados durante el procesamiento
			SELECT &sSourceTableName
			USE
			SELECT C_Dups_HUM
			USE
			DELETE FILE 'C_Dups_HUM.DBF'
			SELECT T_NonDups_HUM
			USE
			DELETE FILE 'T_NonDups_HUM.DBF'
			SELECT C_Count_Dups				
			USE
	ENDCASE
	SET PROCEDURE TO &sOldProcedureFile
	RETURN nDuplicates
ENDFUNC


PROCEDURE solveDuplicatesTst

	LPARAMETERS sSourceTableName as String
	
	SET PROCEDURE TO AnomaliesHandler
	?solveDuplicates(sSourceTableName)
ENDPROC


PROCEDURE correctTreatmentValues
	LPARAMETERS pathToSourceDataFile AS String
	
	* Corrije un error de compatibilidad hacia atrás que se presenta en el Sistema Sivigila 2010 debido al hecho de que los eventos ;
	SÍFILIS CONGÉNITA - Cód. 740 y SÍFILIS GESTACIONAL - Cód. 750 se almacenaban en la versión 2008 en la tabla Eventos_11.DBF y, ;
	a partir del versión 2010, el evento SÍFILIS CONGÉNITA - Cód. 740 empezó a almcenarse en la tabla Eventos_44.DBF
	
	*El error a corregir consiste en que las varibales rec_tto, tip_tto y med_adm no se migraron hacia la nueva tabla Eventos_44.DBF
	
	
	LOCAL bContinue AS Boolean
	LOCAL sErrorMsg AS String, sSourceTableName AS String, sUpdateSQLClause AS String
	LOCAL nTableFields AS Long
	DIMENSION aTableFields(1)

	lcOldOnError = ON("ERROR") && Save default error handler.
	ON ERROR
	lcOldSetExclusive=SET("Exclusive")

	sUnexpectedErrorMsg='Se ha producido un error inesperado durante la actualización.' + CHR(13) + CHR(13) +;
						'Verifique que la tabla fuente que se va a utilizar para hacer' + CHR(13) +;
						'la actualización no está siendo utilizada por ningún programa ' + CHR(13) +;
						'e intente de nuevo la actualización.' + CHR(13) + CHR(13) +;
						'El sistema se cerrará.'

	bContinue=.T.
	IF EMPTY(pathToSourceDataFile) THEN
		bContinue=.F.
		sErrorMsg='No seleccionó la carpeta desde donde se deben actualizar los datos'
	ELSE
		CLOSE ALL
		SET EXCLUSIVE ON
		
		sSourceTableName=(pathToSourceDataFile) + '\EVENTOS_11.DBF'
		IF FILE(sSourceTableName) THEN
			TRY
				*Verifica que en la tabla fuente existan los campos rec_tto, tip_tto y med_adm
				USE (sSourceTableName)
				nTableFields =AFIELDS(aTableFields)
				IF ASCAN(aTableFields,"rec_tto",-1,-1,-1,15)>0 AND ASCAN(aTableFields,"tip_tto ",-1,-1,-1,15)>0 ;
					AND ASCAN(aTableFields,"med_adm",-1,-1,-1,15)>0 THEN

					*Verifica que en la tabla destino existan los campos rec_tto, tip_tto y med_adm
					USE EVENTOS_44 IN 0
					nTableFields =AFIELDS(aTableFields, "EVENTOS_44")
					IF ASCAN(aTableFields,"rec_tto",-1,-1,-1,15)>0 AND ASCAN(aTableFields,"tip_tto ",-1,-1,-1,15)>0 ;
						AND ASCAN(aTableFields,"med_adm",-1,-1,-1,15)>0 THEN
					
						*Efectúa la actualización de los valores de las variables en la tabla Eventos_44
						sUpdateSQLClause="UPDATE EVENTOS_44 SET " + ;
										 "Tratamient=EVENTOS_11.rec_tto, Modatratam=EVENTOS_11.tip_tto, med_adm=EVENTOS_11.med_adm " + ;
										 "FROM EVENTOS_11 WHERE " + ;
										 "EVENTOS_44.SEMANA+EVENTOS_44.AÑO+EVENTOS_44.COD_PRE+EVENTOS_44.COD_SUB+EVENTOS_44.COD_EVE" + ;
										 "+EVENTOS_44.TIP_IDE+EVENTOS_44.NUM_IDE = EVENTOS_11.SEMANA+EVENTOS_11.AÑO+EVENTOS_11.COD_PRE" + ;
										 "+EVENTOS_11.COD_SUB+EVENTOS_11.COD_EVE+EVENTOS_11.TIP_IDE+EVENTOS_11.NUM_IDE"
						&sUpdateSQLClause
						sErrorMsg='Se actualizaron ' + ALLTRIM(STR(_TALLY)) + ' registros.'

					ELSE
						bContinue=.F.
						sErrorMsg='El archivo en donde se van actualizar los datos del evento Sífilis Congenita '  + ;
									+ 'no tiene las variables o campos rec_tto, tip_tto y med_adm. No hay nada que actualizar'
					ENDIF
				ELSE
					bContinue=.F.
					sErrorMsg='El archivo que contiene los datos del evento Sífilis Congenita no tiene las variables '  + ;
								'o campos rec_tto, tip_tto y med_adm. No hay nada que actualizar'
				ENDIF
			CATCH
				MESSAGEBOX( sUnexpectedErrorMsg, 0 + 48 + 256, 'Error de actualización SIVIGILA' )
				QUIT
			ENDTRY
		ELSE
			bContinue=.F.
			sErrorMsg='La carpeta seleccionada no contiene la tabla en donde se almacena la información del evento Sífilis Congenita. No hay nada que actualizar'
		ENDIF			
	ENDIF
	
	IF !bContinue THEN
		MESSAGEBOX( sErrorMsg, 0 + 16 + 256, 'Violación de actualización SIVIGILA' )
	ELSE
		MESSAGEBOX( sErrorMsg, 0 + 48 + 256, 'SIVIGILA' )
	ENDIF

	SET EXCLUSIVE &lcOldSetExclusive
	ON ERROR &lcOldOnError
ENDPROC


FUNCTION validateDataEntry

	LPARAMETERS sDataEntryTableName


	sOldError=ON("ERROR")
	ON ERROR
	
	nOldWorkArea=SELECT()
	
	nRetunedValue=1
	sInsertSQLClause="INSERT INTO t" + sDataEntryTableName + " FROM MEMVAR"

	USE t&sDataEntryTableName EXCLUSIVE IN 0
	TRY
		&sInsertSQLClause
	CATCH TO oException
		nRetunedValue=oException.ErrorNo
	FINALLY
		sDeleteSQLClause="DELETE FROM t" + sDataEntryTableName
		&sDeleteSQLClause
		
		SELECT t&sDataEntryTableName
		PACK
		USE 
	ENDTRY
			
	ON ERROR &sOldError		
	SELECT (nOldWorkArea)
	RETURN nRetunedValue
	
ENDFUNC


PROCEDURE cleanSIVIGILADuplicates

	LOCAL oAnomaliesSolver as Object 
	LOCAL sTargetTableName as String
	
	oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.)
	SET PROCEDURE TO MAIN ADDITIVE
	oAnomaliesSolver.sXlsFilePath = SIVIGILADefaultXLSPath()

	SELECT DISTINCT DBF_NAME FROM SIVIGILADataTables INTO CURSOR rsTargetTables
	SELECT rsTargetTables
	SCAN
		sTargetTableName = ALLTRIM(rsTargetTables.DBF_NAME)
		WAIT "Depurando datos en la tabla " + sTargetTableName  WINDOW NOWAIT
		oAnomaliesSolver.sSourceTableName = sTargetTableName
 		oAnomaliesSolver.deleteDuplicates()
 		WAIT CLEAR
	ENDSCAN
	RELEASE oAnomaliesSolver
	DO closeTables WITH 'rsTargetTables,SIVIGILADataTables' IN SIVIGILAUtilities
ENDPROC


PROCEDURE cleanSIVIGILAMissingData

	LPARAMETERS sTableNameList as String 
	
	LOCAL oAnomaliesSolver as Object 
	LOCAL sTargetTableName as String
	
	oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.)
	SET PROCEDURE TO MAIN ADDITIVE
	oAnomaliesSolver.sXlsFilePath = SIVIGILADefaultXLSPath()
	oAnomaliesSolver.addExcludedField("AJUSTE NUM_CON")

	IF VARTYPE(sTableNameList)='L' THEN
		SELECT DISTINCT DBF_NAME FROM SIVIGILADataTables WHERE sFilter INTO CURSOR rsTargetTables
	ELSE
		sTableNameList = ',' + sTableNameList + ','
		SELECT DISTINCT DBF_NAME FROM SIVIGILADataTables WHERE ',' + ALLTRIM(DBF_NAME) + ',' $ (sTableNameList) INTO CURSOR rsTargetTables
	ENDIF
	SELECT rsTargetTables
	SCAN
		sTargetTableName = ALLTRIM(rsTargetTables.DBF_NAME)
		WAIT "Depurando datos en la tabla " + sTargetTableName  WINDOW NOWAIT
		oAnomaliesSolver.sSourceTableName = sTargetTableName
		oAnomaliesSolver.deleteMissingData(.T.)
 		WAIT CLEAR
	ENDSCAN
	RELEASE oAnomaliesSolver
	DO closeTables WITH 'rsTargetTables,SIVIGILADataTables' IN SIVIGILAUtilities
ENDPROC



PROCEDURE DetectAdjusmentsDelays(sXLSPath as String)

	*Identifica casos de eventos individuales(SivigilaDB!Eventos.TIP_NOT='1') y ambiguos(SivigilaDB!Eventos.TIP_NOT='2') que ;
	no han sido ajustados y que deberían estarlo según la especificación de tiempos máximos permitidos de ajuste que se ;
	encuentran en la tabla eventos

	LOCAL sTbTmpXLSResults AS String
	
	sAño=""
	sSemana=""
	sCod_Eve=""
	sTip_Ide=""
	sNum_Ide=""
	sCod_Pre=""
	sCod_Sub=""
	dFec_Not = .NULL.
	iDias_Ajuste=0
	bGuia =.T.
	
	*Establece la fecha límite a partir de la cual se deben considerar los posibles casos que se encuentran sin ajustar a tiempo;
	;
		X			|										X			|						;
	----X-----------|---------------------------------------X-----------|--------------->		;
		X			|										X			|						;
		2017		29/03									2018		29/03
		
	IF DATE() =< CTOD('29/03/'+nc(YEAR(DATE())))
		* Las fechas de notificación a tener en cuenta se tomarán a partir de la fecha de cierre del año anterior que, por facilidad, ;
		se supone que es igual a la fecha de cierre del año en curso menos 365 días
		dFechaCero =  CTOD('29/03/'+nc(YEAR(DATE()))) - 365
	ELSE
		* Las fechas de notificación a tener en cuenta se tomarán a partir del 1 de enero del año en curso
		dFechaCero =  CTOD('01/01/'+nc(YEAR(DATE())))
	ENDIF
	
*SET STEP ON 	
	*Crea una tabla temporal para almacenar los resultados
	sTbTmpXLSResults = SYS(2015)
	CREATE TABLE (ADDBS(SYS(2023))+sTbTmpXLSResults) FREE (AÑO C(4), SEMANA C(2), COD_EVE c(4), TIP_IDE C(2) , NUM_IDE C(17),COD_PRE c(10), COD_SUB c(2))
	
	SET TALK ON 
	SELECT DISTINCT P.AÑO, P.SEMANA, P.COD_EVE, P.TIP_IDE, P.NUM_IDE, P.COD_PRE, P.COD_SUB, P.FEC_NOT, P.AJUSTE, E.DIAS_AJUSTE ;
		FROM Paciente P ;
		INNER JOIN (SELECT COD_EVE,DIAS_AJUSTE FROM Eventos WHERE DIAS_AJUSTE!=0) E ON P.COD_EVE = E.COD_EVE ;
		INNER JOIN (SELECT AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB FROM Paciente ;
		WHERE (fec_not >= dFechaCero) AND (Tip_cas='1' or Tip_cas='2') AND (Ajuste='0') ) S ;
		ON P.AÑO=S.AÑO AND P.SEMANA=S.SEMANA AND P.COD_EVE=S.COD_EVE AND P.TIP_IDE=S.TIP_IDE AND ;
		P.NUM_IDE=S.NUM_IDE AND P.COD_PRE=S.COD_PRE AND P.COD_SUB=S.COD_SUB  ;
		ORDER BY P.AÑO, P.SEMANA, P.COD_EVE, P.TIP_IDE, P.NUM_IDE, P.COD_PRE, P.COD_SUB, P.FEC_NOT INTO CURSOR rsCursor
	SET TALK OFF 


	SELECT rsCursor
	SCAN
		IF !EMPTY(sAño) then
			IF sAño=rsCursor.Año AND sSemana=rsCursor.Semana AND sCod_Eve=rsCursor.Cod_Eve AND sTip_Ide=rsCursor.Tip_Ide AND sNum_Ide=rsCursor.Num_Ide AND sCod_Pre=rsCursor.Cod_Pre AND sCod_Sub=rsCursor.Cod_Sub THEN
				*Si el ajuste es 3,4,5,6, D y diferente de 7, ya se realizo el ajuste valido no importa la fecha de notifiacion
				IF (rsCursor.Ajuste > '2') AND (rsCursor.Ajuste != '7') THEN 
					bGuia =.F.
				ENDIF
			ELSE
				IF bGuia AND iDias_Ajuste > 0 AND (DATE()- dFec_Not) > iDias_Ajuste THEN
					
					sCadSql = "INSERT INTO " + sTbTmpXLSResults + " (AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB) VALUES ('" + sAño + "','" + ;
							sSemana + "','" + sCod_Eve + "','" + sTip_Ide + "','" + sNum_Ide + "','" + sCod_Pre + "','" + sCod_Sub + "')"
					
					&sCadSql	
				ENDIF
				sAño = rsCursor.Año
				sSemana = rsCursor.Semana
				sCod_Eve = rsCursor.Cod_Eve
				sTip_Ide = rsCursor.Tip_Ide
				sNum_Ide = rsCursor.Num_Ide
				sCod_Pre = rsCursor.Cod_Pre
				sCod_Sub = rsCursor.Cod_Sub
				dFec_Not = rsCursor.Fec_Not
				iDias_Ajuste = rsCursor.Dias_Ajuste
				bGuia =.T. 	
			ENDIF
		ELSE
			sAño = rsCursor.Año
			sSemana = rsCursor.Semana
			sCod_Eve = rsCursor.Cod_Eve
			sTip_Ide = rsCursor.Tip_Ide
			sNum_Ide = rsCursor.Num_Ide
			sCod_Pre = rsCursor.Cod_Pre
			sCod_Sub = rsCursor.Cod_Sub
			dFec_Not = rsCursor.Fec_Not
			iDias_Ajuste = rsCursor.Dias_Ajuste
			bGuia =.T. 	
		ENDIF
	ENDSCAN

	IF !EMPTY(sAño) AND bGuia AND iDias_Ajuste > 0 AND (DATE()- dFec_Not) > iDias_Ajuste  THEN
		sCadSql = "INSERT INTO " + sTbTmpXLSResults + " (AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB) VALUES ('" + sAño + "','" + ;
			sSemana + "','" + sCod_Eve + "','" + sTip_Ide + "','" + sNum_Ide + "','" + sCod_Pre + "','" + sCod_Sub + "')"
					
		&sCadSql	
	ENDIF


	SELECT DISTINCT P.COD_EVE, P.FEC_NOT, P.SEMANA, P.AÑO, P.COD_PRE, P.COD_SUB, P.PRI_NOM , P.SEG_NOM, ;
		P.PRI_APE, P.SEG_APE, P.TIP_IDE, P.NUM_IDE, P.EDAD, P.SEXO, P.PAIS ;
		FROM Paciente P ;
		INNER JOIN &sTbTmpXLSResults X ON ;
		P.AÑO=X.AÑO AND P.SEMANA=X.SEMANA AND P.COD_EVE=X.COD_EVE AND P.TIP_IDE=X.TIP_IDE AND ;
		P.NUM_IDE=X.NUM_IDE AND P.COD_PRE=X.COD_PRE AND P.COD_SUB=X.COD_SUB ;
		ORDER BY P.AÑO, P.SEMANA, P.COD_EVE, P.TIP_IDE, P.NUM_IDE, P.COD_PRE, P.COD_SUB, P.FEC_NOT INTO CURSOR rsResultNotifPend

	DO Form frmSearchRec WITH 'rsResultNotifPend','.T.',sXLSPath, .T., "Casos pendientes de ajuste" 
	
	USE IN rsCursor
	USE IN (sTbTmpXLSResults)
ENDPROC


PROCEDURE correct_zScores_V01()
	
	* Corrige un error en el cálculo de los campos IMC,ZSCORE_PT,CLAS_PESO,ZSCORE_TE y CLAS_TALLA para el evento ;
	Desnutrición aguda en menores de cinco años (Cod. 113)

	LOCAL sOldProc as String
	sOldProc = SET("Procedure")
	
	LOCAL sOldDel as String
	sOldDel = SET("Deleted")
	
	LOCAL sOldBasicDataTag AS String 
	
	DIMENSION aOKValues(1) as Variant 

	SET PROCEDURE TO 
	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	SET PROCEDURE TO Utilities ADDITIVE
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE 
	
	SET DELETED ON 
	
	*Establece qué registros tienen la anomalía teniendo en cuenta que no aún hayan sido corregidos 
	=SelectTable('PACIENTE',,.F.)
	sOldBasicDataTag = TAG()
	SET ORDER TO TAG 'LINKER_IDX'

	sLinkingExpr = ALLTRIM(fieldValue('TAG_EXPR', 'MASTER_INDEX',"TABLE_NAME='EVENTOS_83' AND TAG_NAME='LINKER_IDX'"))
	sQualifiedLinkingExpr = qualifyFields(sLinkingExpr, '+', 'EVENTOS_83')
	sNationalDBQualifiedLinkingExpr = qualifyFields(sLinkingExpr, '+', 'ANOMALIAS_EVENTOS_83')
 	
	sScopeFilter = "VAL(EVENTOS_83.año)>=2018 AND VAL(EVENTOS_83.año)<=2019"
	SELECT EVENTOS_83.*, ANOMALIAS_EVENTOS_83.PESO_ACT AS PESO_INDICE, ANOMALIAS_EVENTOS_83.TALLA_ACT AS TALLA_INDICE;
		FROM EVENTOS_83 LEFT OUTER JOIN (SELECT * FROM tbCasosAnomalosDetectados WHERE Descripcion='113_ZSCORE' ) T ;
			ON &sQualifiedLinkingExpr = T.CLAVE ;
		LEFT OUTER JOIN ANOMALIAS_EVENTOS_83 ; 
			ON &sQualifiedLinkingExpr = &sNationalDBQualifiedLinkingExpr ;
		WHERE T.CLAVE IS NULL AND ;
			((EVENTOS_83.ZSCORE_PT-INT(EVENTOS_83.ZSCORE_PT))*10000=0 OR (EVENTOS_83.ZSCORE_TE-INT(EVENTOS_83.ZSCORE_TE))*10000=0) ;
			AND &sScopeFilter ;
		INTO CURSOR rsIncorrect_z_Scores 
	SELECT rsIncorrect_z_Scores 
	IF _tally > 0 THEN 
		*Crea dos cursores con el objeto de optimizar el proceso. Estos cursores son requeridos por el módulo BusinessRules.calculate_zScores_V03 
		SELECT * FROM OMSZscore_PesoTalla INTO CURSOR rsOMSZscore_PesoTalla
		SELECT * FROM OMSZscore_TallaEdad INTO CURSOR rsOMSZscore_TallaEdad
		
		SELECT rsIncorrect_z_Scores 
		SET RELATION TO &sLinkingExpr INTO PACIENTE ADDITIVE

		glDisableRules = .T.	
		SCAN 
			*Agrega un registro de ajuste (básicos y complementarios) con los valores corregidos. El registro queda listo para ser notificado. ;
			La operación tiene en cuenta la posibilidad de que no se encuentre un registro asociado en la tabla PACIENTE o que los valores de ;
			TALLA_INDICE y PESO_INDICE no se hayan podido encontrar en la BD nacional acumulada
			IF !EMPTY(PACIENTE.SEXO) AND !ISNULL(rsIncorrect_z_Scores.TALLA_INDICE) AND !ISNULL(rsIncorrect_z_Scores.PESO_INDICE) THEN 
				*SELECT rsIncorrect_z_Scores 
				SCATTER MEMVAR

				*Calcula el nuevo valor del Zscore Peso/Talla
				*DO calculate_zScores_V03 WITH m.PESO_ACT,m.TALLA_ACT,PACIENTE.SEXO,ageInMonths2(PACIENTE.FEC_NOT,PACIENTE.FECHA_NTO),aOKValues IN BusinessRules
				bSuccess = .F.
				DO calculate_zScores_V03 WITH m.PESO_INDICE,m.TALLA_INDICE,PACIENTE.SEXO,ageInMonths2(PACIENTE.FEC_NOT,PACIENTE.FECHA_NTO),aOKValues, bSuccess IN BusinessRules
				IF bSuccess THEN 
				
					m.AJUSTE = '7'
					m.FEC_AJU = DATE()
					m.NOTIFICA = 0
					m.IMC = aOKValues[1]
					m.PESO_ACT = m.PESO_INDICE
					m.TALLA_ACT = m.TALLA_INDICE
					m.ZSCORE_PT = aOKValues[2]
					m.CLASIF_PESO = aOKValues[3]
					m.ZSCORE_TE = aOKValues[4]
					m.CLASIF_TALLA = aOKValues[5]
					INSERT INTO EVENTOS_83 FROM MEMVAR

					SELECT PACIENTE
					SCATTER MEMVAR
					m.AJUSTE = '7'
					m.FEC_AJU = DATE()
					m.NOTIFICA = 0
					INSERT INTO PACIENTE FROM MEMVAR 
					
					*Guarda el rastro del registro que ha sido detectado y corregido
					SELECT rsIncorrect_z_Scores 
					TRY
						INSERT INTO tbCasosAnomalosDetectados (Clave, Fecha, Descripcion) VALUES (EVALUATE(sLinkingExpr), DATETIME(),'113_ZSCORE') 
					CATCH TO oException
					ENDTRY  
				ENDIF 
			ENDIF 
		ENDSCAN		
		glDisableRules = .F.
	ENDIF 
	
	*USE IN rsIncorrect_z_Scores 
	DO CloseTables WITH 'rsIncorrect_z_Scores,tbCasosAnomalosDetectados,OMSZscore_PesoTalla,rsOMSZscore_PesoTalla,OMSZscore_TallaEdad,rsOMSZscore_TallaEdad,ANOMALIAS_EVENTOS_83' ;
		IN SIVIGILAUtilities
	
	SELECT PACIENTE
	SET ORDER TO TAG &sOldBasicDataTag 

	SET PROCEDURE TO &sOldProc 
	SET DELETED &sOldDel 
ENDPROC


PROCEDURE undoZscores()
	
	* Corrige un error inducido por la aplicación de correct_zScores que se presenta cuando se adicionan registros de ajuste 7 que corrigen el zScore;
	pero no se ha tenido en cuenta la existencia de registros previos de ajuste 7 asociados al coaso corregido; Por tanto, el efecto neto de la corrección ;
	del zScore es que anula cualquier ajuste 7 previo que se haya hechoa un caso corregido.

	SELECT DISTINCT FECHA FROM tbCasosAnomalosDetectados INTO CURSOR rsChanges
	IF RECCOUNT()>0 THEN
		SELECT rsChanges
		SCAN 
			dDateOfChange = TTOD(rsChanges.Fecha)

			SELECT RECNO() as nReg, .T. AS BORRAR FROM PACIENTE ;
				WHERE AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB IN (SELECT Clave FROM tbCasosAnomalosDetectados) ;
				AND AJUSTE='7' AND FEC_AJU = dDateOfChange ;
				INTO CURSOR rsChangedRecsBD READWRITE 
			SELECT rsChangedRecsBD 
			INDEX ON nReg TAG nREG_IDX
				
			SELECT PACIENTE
			SET RELATION TO RECNO() INTO rsChangedRecsBD
			DELETE ALL FOR rsChangedRecsBD.BORRAR
			USE IN rsChangedRecsBD

			*Elimina los registros de datos complementarios que fueron ajustados en zScore
			SELECT RECNO() as nReg, .T. AS BORRAR FROM EVENTOS_83 ;
				WHERE AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB IN (SELECT Clave FROM tbCasosAnomalosDetectados) ;
				AND AJUSTE='7' AND FEC_AJU = dDateOfChange ;
				INTO CURSOR rsChangedRecsCD READWRITE 
			SELECT rsChangedRecsCD
			INDEX ON nReg TAG nREG_IDX
				
			SELECT EVENTOS_83 
			SET RELATION TO RECNO() INTO rsChangedRecsCD
			DELETE ALL FOR rsChangedRecsCD.BORRAR
			USE IN rsChangedRecsCD

			*Elimina los registros del log que daban cuenta de la corrección mal efectuada
			DELETE FROM tbCasosAnomalosDetectados WHERE TTOD(Fecha)=dDateOfChange 
			
			SELECT rsChanges
		ENDSCAN 
	ENDIF 
	USE IN rsChanges
ENDPROC



PROCEDURE correct_zScores_v02()
	
	* Corrige un error en el cálculo de los campos IMC,ZSCORE_PT,CLAS_PESO,ZSCORE_TE y CLAS_TALLA para el evento ;
	Desnutrición aguda en menores de cinco años (Cod. 113)

	LOCAL sOldProc as String
	sOldProc = SET("Procedure")
	
	LOCAL sOldDel as String
	sOldDel = SET("Deleted")
	
	LOCAL sOldBasicDataTag AS String 
	
	DIMENSION aOKValues(1) as Variant 

	SET PROCEDURE TO 
	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	SET PROCEDURE TO Utilities ADDITIVE
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE 
	
	SET DELETED ON 
	
	*Establece qué registros tienen la anomalía teniendo en cuenta que no aún hayan sido corregidos 
	=SelectTable('PACIENTE',,.F.)
	sOldBasicDataTag = TAG()
	SET ORDER TO TAG 'LINKER_IDX'

	sLinkingExpr = ALLTRIM(fieldValue('TAG_EXPR', 'MASTER_INDEX',"TABLE_NAME='EVENTOS_83' AND TAG_NAME='LINKER_IDX'"))
	sQualifiedLinkingExpr = qualifyFields(sLinkingExpr, '+', 'EVENTOS_83')
	sNationalDBQualifiedLinkingExpr = qualifyFields(sLinkingExpr, '+', 'ANOMALIAS_EVENTOS_83')

	sScopeFilter = "VAL(EVENTOS_83.año)>=2018 AND VAL(EVENTOS_83.año)<=2019"
	SELECT EVENTOS_83.*, ANOMALIAS_EVENTOS_83.PESO_ACT AS PESO_INDICE, ANOMALIAS_EVENTOS_83.TALLA_ACT AS TALLA_INDICE;
		FROM EVENTOS_83 LEFT OUTER JOIN (SELECT * FROM tbCasosAnomalosDetectados WHERE Descripcion='113_ZSCORE' ) T ;
			ON &sQualifiedLinkingExpr = T.CLAVE ;
		LEFT OUTER JOIN ANOMALIAS_EVENTOS_83 ; 
			ON &sQualifiedLinkingExpr = &sNationalDBQualifiedLinkingExpr ;
		WHERE T.CLAVE IS NULL AND ;
			((EVENTOS_83.ZSCORE_PT-INT(EVENTOS_83.ZSCORE_PT))*10000=0 OR (EVENTOS_83.ZSCORE_TE-INT(EVENTOS_83.ZSCORE_TE))*10000=0) ;
			AND &sScopeFilter ;
		INTO CURSOR rsIncorrect_z_Scores 
	SELECT rsIncorrect_z_Scores 
	
	IF _tally > 0 THEN 
		*Crea dos cursores con el objeto de optimizar el proceso. Estos cursores son requeridos por el módulo BusinessRules.calculate_zScores_V03 
		SELECT * FROM OMSZscore_PesoTalla INTO CURSOR rsOMSZscore_PesoTalla
		SELECT * FROM OMSZscore_TallaEdad INTO CURSOR rsOMSZscore_TallaEdad
		
		*Crea cursores de datos complementarios y básicos ordenados apropiadamente de tal forma que se permita insertar en las tablas ;
		PACIENTE y EVENTOS_83 registros de ajuste 7 que tengan en cuenta la posible existencia previa de ese tipo de ajustes para algún caso
		sSQLCmd = 'SELECT * FROM EVENTOS_83 WHERE ' + sLinkingExpr + ' IN (SELECT ' + sLinkingExpr + ' FROM rsIncorrect_z_Scores) ORDER BY ' +;
					STRTRAN(sLinkingExpr,"+",",") + ',FEC_AJU DESC INTO CURSOR rsCompDataTargets READWRITE'
		&sSQLCmd 	
		SELECT rsCompDataTargets
		sCmd = 'INDEX ON ' + sLinkingExpr + '+DTOC(FEC_AJU,1) TAG AUX_IDX DESCENDING'
		&sCmd
		
		sSQLCmd = 'SELECT * FROM PACIENTE WHERE ' + sLinkingExpr + ' IN (SELECT ' + sLinkingExpr + ' FROM rsIncorrect_z_Scores) ORDER BY ' +;
					STRTRAN(sLinkingExpr,"+",",") + ',FEC_AJU DESC INTO CURSOR rsBasicDataTargets READWRITE'
		&sSQLCmd 	
		SELECT rsBasicDataTargets
		sCmd = 'INDEX ON ' + sLinkingExpr + '+DTOC(FEC_AJU,1) TAG AUX_IDX DESCENDING'
		&sCmd
		
		*Encadena el recordset de puntajes incorrectos con los apropiados de datos básicos y complementarios
		SELECT rsIncorrect_z_Scores 
		SET RELATION TO &sLinkingExpr INTO PACIENTE ADDITIVE
		SET RELATION TO &sLinkingExpr INTO rsBasicDataTargets ADDITIVE
		SET RELATION TO &sLinkingExpr INTO rsCompDataTargets ADDITIVE

		glDisableRules = .T.	
		SCAN 
			*Agrega un registro de ajuste (básicos y complementarios) con los valores corregidos. El registro queda listo para ser notificado. ;
			La operación tiene en cuenta la posibilidad de que no se encuentre un registro asociado en la tabla PACIENTE o que los valores de ;
			TALLA_INDICE y PESO_INDICE no se hayan podido encontrar en la BD nacional acumulada
			IF !EMPTY(PACIENTE.SEXO) AND !ISNULL(rsIncorrect_z_Scores.TALLA_INDICE) AND !ISNULL(rsIncorrect_z_Scores.PESO_INDICE) THEN 
				*SELECT rsIncorrect_z_Scores 
				SCATTER MEMVAR

				*Calcula el nuevo valor del Zscore Peso/Talla
				*DO calculate_zScores_V03 WITH m.PESO_ACT,m.TALLA_ACT,PACIENTE.SEXO,ageInMonths2(PACIENTE.FEC_NOT,PACIENTE.FECHA_NTO),aOKValues IN BusinessRules
				bSuccess = .F.
				DO calculate_zScores_V03 WITH m.PESO_INDICE,m.TALLA_INDICE,PACIENTE.SEXO,ageInMonths2(PACIENTE.FEC_NOT,PACIENTE.FECHA_NTO),aOKValues, bSuccess IN BusinessRules
				IF bSuccess THEN 
				
					SELECT rsCompDataTargets
					SCATTER MEMVAR
					m.AJUSTE = '7'
					m.FEC_AJU = DATE()
					m.NOTIFICA = 0
					m.IMC = aOKValues[1]
					m.PESO_ACT = m.PESO_INDICE
					m.TALLA_ACT = m.TALLA_INDICE
					m.ZSCORE_PT = aOKValues[2]
					m.CLASIF_PESO = aOKValues[3]
					m.ZSCORE_TE = aOKValues[4]
					m.CLASIF_TALLA = aOKValues[5]
					INSERT INTO EVENTOS_83 FROM MEMVAR

					SELECT rsBasicDataTargets 
					SCATTER MEMVAR
					m.AJUSTE = '7'
					m.FEC_AJU = DATE()
					m.NOTIFICA = 0
					INSERT INTO PACIENTE FROM MEMVAR 
					
					*Guarda el rastro del registro que ha sido detectado y corregido
					SELECT rsIncorrect_z_Scores 
					TRY
						INSERT INTO tbCasosAnomalosDetectados (Clave, Fecha, Descripcion) VALUES (EVALUATE(sLinkingExpr), DATETIME(),'113_ZSCORE_REV2') 
					CATCH TO oException
					ENDTRY  
				ENDIF 
			ENDIF 
		ENDSCAN		
		glDisableRules = .F.
	ENDIF 
	
	*USE IN rsIncorrect_z_Scores 
	DO CloseTables WITH 'rsIncorrect_z_Scores,tbCasosAnomalosDetectados,OMSZscore_PesoTalla,rsOMSZscore_PesoTalla,OMSZscore_TallaEdad, ;
						rsOMSZscore_TallaEdad,ANOMALIAS_EVENTOS_83,rsBasicDataTargets,rsCompDataTargets ' IN SIVIGILAUtilities
	
	SELECT PACIENTE
	SET ORDER TO TAG &sOldBasicDataTag 

	SET PROCEDURE TO &sOldProc 
	SET DELETED &sOldDel 
ENDPROC



PROCEDURE correct_zScores()
	
	* Corrige un error en el cálculo de los campos IMC,ZSCORE_PT,CLAS_PESO,ZSCORE_TE y CLAS_TALLA para el evento ;
	Desnutrición aguda en menores de cinco años (Cod. 113)

	LOCAL sOldProc as String
	sOldProc = SET("Procedure")
	
	LOCAL sOldDel as String
	sOldDel = SET("Deleted")
	
	LOCAL sOldBasicDataTag AS String 
	
	DIMENSION aOKValues(1) as Variant 

	SET PROCEDURE TO 
	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	SET PROCEDURE TO Utilities ADDITIVE
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE 
	
	SET DELETED ON 
	
	*Establece qué registros tienen la anomalía teniendo en cuenta que no aún hayan sido corregidos 
	=SelectTable('PACIENTE',,.F.)
	sOldBasicDataTag = TAG()
	SET ORDER TO TAG 'LINKER_IDX'

	sLinkingExpr = ALLTRIM(fieldValue('TAG_EXPR', 'MASTER_INDEX',"TABLE_NAME='EVENTOS_83' AND TAG_NAME='LINKER_IDX'"))
	sQualifiedLinkingExpr = qualifyFields(sLinkingExpr, '+', 'EVENTOS_83')
	sNationalDBQualifiedLinkingExpr = qualifyFields(sLinkingExpr, '+', 'ANOMALIAS_EVENTOS_83')

	sScopeFilter = "VAL(EVENTOS_83.año)>=2018 AND VAL(EVENTOS_83.año)<=2019"
	SELECT EVENTOS_83.*, ANOMALIAS_EVENTOS_83.PESO_ACT AS PESO_INDICE, ANOMALIAS_EVENTOS_83.TALLA_ACT AS TALLA_INDICE,;
		&sQualifiedLinkingExpr AS LinkValue;
		FROM EVENTOS_83 LEFT OUTER JOIN (SELECT * FROM tbCasosAnomalosDetectados WHERE Descripcion='113_ZSCORE' ) T ;
			ON &sQualifiedLinkingExpr = T.CLAVE ;
		LEFT OUTER JOIN ANOMALIAS_EVENTOS_83 ; 
			ON &sQualifiedLinkingExpr = &sNationalDBQualifiedLinkingExpr ;
		WHERE T.CLAVE IS NULL AND ;
			((EVENTOS_83.ZSCORE_PT-INT(EVENTOS_83.ZSCORE_PT))*10000=0 OR (EVENTOS_83.ZSCORE_TE-INT(EVENTOS_83.ZSCORE_TE))*10000=0) ;
			AND &sScopeFilter ;
		INTO CURSOR rsIncorrect_z_Scores 
	SELECT rsIncorrect_z_Scores 

	IF _tally > 0 THEN 
		*Crea dos cursores con el objeto de optimizar el proceso. Estos cursores son requeridos por el módulo BusinessRules.calculate_zScores_V03 
		SELECT * FROM OMSZscore_PesoTalla INTO CURSOR rsOMSZscore_PesoTalla
		SELECT * FROM OMSZscore_TallaEdad INTO CURSOR rsOMSZscore_TallaEdad
		
		*Crea cursores de datos complementarios y básicos ordenados apropiadamente de tal forma que se permita insertar en las tablas ;
		PACIENTE y EVENTOS_83 registros de ajuste 7 que tengan en cuenta la posible existencia previa de ese tipo de ajustes para algún caso
		sSQLCmd = 'SELECT * FROM EVENTOS_83 WHERE ' + sLinkingExpr + ' IN (SELECT ' + sLinkingExpr + ' FROM rsIncorrect_z_Scores) ORDER BY ' +;
					STRTRAN(sLinkingExpr,"+",",") + ',FEC_AJU DESC INTO CURSOR rsCompDataTargets READWRITE'
		&sSQLCmd 	
		SELECT rsCompDataTargets
		sCmd = 'INDEX ON ' + sLinkingExpr + '+DTOC(FEC_AJU,1) TAG AUX_IDX DESCENDING'
		&sCmd
		
		sSQLCmd = 'SELECT * FROM PACIENTE WHERE ' + sLinkingExpr + ' IN (SELECT ' + sLinkingExpr + ' FROM rsIncorrect_z_Scores) ORDER BY ' +;
					STRTRAN(sLinkingExpr,"+",",") + ',FEC_AJU DESC INTO CURSOR rsBasicDataTargets READWRITE'
		&sSQLCmd 	
		SELECT rsBasicDataTargets
		sCmd = 'INDEX ON ' + sLinkingExpr + '+DTOC(FEC_AJU,1) TAG AUX_IDX DESCENDING'
		&sCmd
		
		*Encadena el recordset de puntajes incorrectos con los apropiados de datos básicos y complementarios
		SELECT rsIncorrect_z_Scores 
		SET RELATION TO &sLinkingExpr INTO PACIENTE ADDITIVE
		*SET RELATION TO &sLinkingExpr INTO rsBasicDataTargets ADDITIVE
		*SET RELATION TO &sLinkingExpr INTO rsCompDataTargets ADDITIVE

		glDisableRules = .T.	
		SCAN 
			*Agrega un registro de ajuste (básicos y complementarios) con los valores corregidos. El registro queda listo para ser notificado. ;
			La operación tiene en cuenta la posibilidad de que no se encuentre un registro asociado en la tabla PACIENTE o que los valores de ;
			TALLA_INDICE y PESO_INDICE no se hayan podido encontrar en la BD nacional acumulada
			IF !EMPTY(PACIENTE.SEXO) AND !ISNULL(rsIncorrect_z_Scores.TALLA_INDICE) AND !ISNULL(rsIncorrect_z_Scores.PESO_INDICE) THEN 
				*SELECT rsIncorrect_z_Scores 
				SCATTER MEMVAR

				*Calcula el nuevo valor del Zscore Peso/Talla
				*DO calculate_zScores_V03 WITH m.PESO_ACT,m.TALLA_ACT,PACIENTE.SEXO,ageInMonths2(PACIENTE.FEC_NOT,PACIENTE.FECHA_NTO),aOKValues IN BusinessRules
				bSuccess = .F.
				DO calculate_zScores_V03 WITH m.PESO_INDICE,m.TALLA_INDICE,PACIENTE.SEXO,ageInMonths2(PACIENTE.FEC_NOT,PACIENTE.FECHA_NTO),aOKValues, bSuccess IN BusinessRules
				IF bSuccess THEN 
				
					SELECT rsCompDataTargets
					LOCATE FOR (&sLinkingExpr = rsIncorrect_z_Scores.LinkValue)
					SCATTER MEMVAR
					m.AJUSTE = '7'
					m.FEC_AJU = DATE()
					m.NOTIFICA = 0
					m.IMC = aOKValues[1]
					m.PESO_ACT = m.PESO_INDICE
					m.TALLA_ACT = m.TALLA_INDICE
					m.ZSCORE_PT = aOKValues[2]
					m.CLASIF_PESO = aOKValues[3]
					m.ZSCORE_TE = aOKValues[4]
					m.CLASIF_TALLA = aOKValues[5]
					INSERT INTO EVENTOS_83 FROM MEMVAR

					SELECT rsBasicDataTargets 
					LOCATE FOR (&sLinkingExpr = rsIncorrect_z_Scores.LinkValue)
					SCATTER MEMVAR
					m.AJUSTE = '7'
					m.FEC_AJU = DATE()
					m.NOTIFICA = 0
					INSERT INTO PACIENTE FROM MEMVAR 
					
					*Guarda el rastro del registro que ha sido detectado y corregido
					SELECT rsIncorrect_z_Scores 
					TRY
						INSERT INTO tbCasosAnomalosDetectados (Clave, Fecha, Descripcion) VALUES (EVALUATE(sLinkingExpr), DATETIME(),'113_ZSCORE_REV2') 
					CATCH TO oException
					ENDTRY  
				ENDIF 
			ENDIF 
		ENDSCAN		
		glDisableRules = .F.
	ENDIF 
	
	*USE IN rsIncorrect_z_Scores 
	DO CloseTables WITH 'rsIncorrect_z_Scores,tbCasosAnomalosDetectados,OMSZscore_PesoTalla,rsOMSZscore_PesoTalla,OMSZscore_TallaEdad, ;
						rsOMSZscore_TallaEdad,ANOMALIAS_EVENTOS_83,rsBasicDataTargets,rsCompDataTargets ' IN SIVIGILAUtilities
	
	SELECT PACIENTE
	SET ORDER TO TAG &sOldBasicDataTag 

	SET PROCEDURE TO &sOldProc 
	SET DELETED &sOldDel 
ENDPROC


PROCEDURE UpdateGeografiaEventos

	glDisableRules=.t.

	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	=SelectTable('MUNICIPIOS',,.F.)
	SELECT Cod_Mun, Cod_Pais FROM municipios WHERE cod_mun in ('01105','01239','01249','01580','01589','01850','01241','01240','01243','01244') INTO ARRAY rsMuni


	
	
	sWhere2 = " in ('01105','01239','01249','01580','01589','01850','01241','01240','01243','01244') group by "
	
                                 
	
	sEvento= "UPDATE Eventos_00 SET VAR_CAMPO = '01VAR_PAIS' WHERE "
	sEvento= sEvento + "!EMPTY(Eventos_00.VAR_CAMPO) AND Eventos_00.VAR_CAMPO =='VAR_MUNICIPIO'" 	
	
	BEGIN TRANSACTION
	
		sC = "Select CodMun1 from Eventos_65 E Where " + "E.CodMun1" +  sWhere2 + " E.CodMun1 INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_65')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'CodMun1')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
		
		sC = "Select CodMun1 from Eventos_46 E where " + "E.CodMun1" +  sWhere2 + " E.CodMun1 INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_46')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'CodMun1')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF

		sC = "Select CodMun2 from Eventos_46 E where " + "E.CodMun2" +  sWhere2 + " E.CodMun2 INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_46')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'CodMun2')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF

		sC = "Select CodMun3 from Eventos_46 E Where " + "E.CodMun3" +  sWhere2 + " E.CodMun3 INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_46')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'CodMun3')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
		
		sC = "Select CodMun4 from Eventos_46 E Where " + "E.CodMun4" +  sWhere2 + " E.CodMun4 INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_46')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'CodMun4')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
		
		sC = "Select Cod_Mun_D from Eventos_42 E Where " + "E.Cod_Mun_D" + sWhere2  + " E.Cod_Mun_D INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_42')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'Cod_Mun_D')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
		
		sC = "Select CodMunInfe from Eventos_01 E Where " + "E.CodMunInfe " +  sWhere2 + " E.CodMunInfe INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_01')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'CodMunInfe')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
		
		sC = "Select Municipio from Eventos_21 E Where " + "E.Municipio " +  sWhere2 + " E.Municipio INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_21')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'Municipio')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
		
		sC = "Select Mun_Par from Eventos_06 E Where " + "E.Mun_Par " +  sWhere2 + " E.Mun_Par INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_06')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'Mun_Par')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
			
		sC = "Select Don_Mun from Eventos_06 E Where " + "E.Don_Mun " +  sWhere2 + " E.Don_Mun INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_06')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'Don_Mun')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
			
		sC = "Select Mun_Via from Eventos_10 E Where " + "E.Mun_Via " +  sWhere2 + " E.Mun_Via INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_10')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'Mun_Via')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
			
		sC = "Select Mun_Nac from Eventos_10 E Where " + "E.Mun_Nac " +  sWhere2 + " E.Mun_Nac INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_10')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'Mun_Nac')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
			
		sC = "Select CodMun from Eventos_85 E Where " + "E.CodMun " +  sWhere2 + " E.CodMun INTO ARRAY rsTtMuni"	 
		&sC
		IF _TALLY>0 THEN
			FOR nTt = 1 TO ALEN(rsTtMuni)
				FOR nIndice = 1 TO 10
					IF ALLTRIM(rsTtMuni[nTt]) = ALLTRIM(rsMuni[nIndice,1]) THEN
						sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_85')
						sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'CodMun')
						sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
						sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
						&sCmd
					ENDIF
				ENDFOR
			ENDFOR
		ENDIF
		
		*TIP_IDE --> COD_MUN
		*NUM_IDE --> NUM_CON(N0)                           
	
		sEvento= "UPDATE Eventos_00 SET VAR_CAMPO = '01VAR_PAIS' WHERE "
		sEvento= sEvento + "!EMPTY(Eventos_00.VAR_CAMPO) AND Eventos_00.VAR_CAMPO =='VAR_MUNICIPIO'"

		FOR nIndice = 1 TO 10
			sCmd = STRTRAN(sEvento,'Eventos_00','Eventos_43')
			sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'Cod_Mun')
			sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
			sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
			&sCmd
		ENDFOR


		FOR nIndice = 1 TO 10
			sCmd = STRTRAN(sEvento,'Eventos_00','BROTES')
			sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'Cod_Mun')
			sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
			sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
			&sCmd
		ENDFOR




		FOR nIndice = 1 TO 10
			sCmd = STRTRAN(sEvento,'Eventos_00','BROTES')
			sCmd = STRTRAN(sCmd, 'VAR_CAMPO', 'Mun_Pro')
			sCmd = STRTRAN(sCmd, 'VAR_PAIS', ALLTRIM(rsMuni[nIndice,2]))
			sCmd = STRTRAN(sCmd, 'VAR_MUNICIPIO', ALLTRIM(rsMuni[nIndice,1]))
			&sCmd
		ENDFOR

	
		FOR nIndice = 1 TO 10 
			sCmd = "update Paciente set Cod_Mun = '01" +  rsMuni[nIndice,2] + "' WHERE "
			sCmd = sCmd + "Cod_Mun ='" + rsMuni[nIndice,1] + "'"
			&sCmd
		ENDFOR

		FOR nIndice = 1 TO 10 
			sCmd = "update Paciente set Mun_Pro = '01" +  rsMuni[nIndice,2] + "' WHERE "
			sCmd = sCmd + "Mun_Pro ='" + rsMuni[nIndice,1] + "'"
			&sCmd
		ENDFOR


	END TRANSACTION 

	glDisableRules=.f.
ENDPROC


PROCEDURE detectDelays_362(nAnomalies AS Number, sXLSPath as String)
	
	*Crea un cursor de nombre rs_Notificaciones_Faltantes_362 en done se encuentran por cada UPGD activa en el sistema, los meses correspondientes ;
	al año de operación en curso, en donde se presentan retrasos en la notificación del evento colectivo 362 . ;
	Los campos del cursor son COD_PRE, COD_SUB, AÑO, MES. 
	
	*nAnomalies (out): contendrá el número de registros del cursor rs_Notificaciones_Faltantes_362 ;
	*sXLSPath (in): ruta hacia la cual deben exportarse los registreos del cursor rs_Notificaciones_Faltantes_362, si es necesario en función de una ;
					selección hecha por el usuario
	
	#DEFINE TARGET_EVENT '362 '
	
	STORE '' TO sSQLCmd  
	
	LOCAL nZeroMonth as Integer, nZeroYear as integer 

	STORE MONTH(DATE())-1 TO nFinalMonth
	STORE YEAR(DATE()) TO nFinalYear 
	
	LOCAL oCurrentEvent as Object
	oCurrentEvent = NEWOBJECT("Event","Event.Prg",.NULL.,TARGET_EVENT )
	
	LOCAL dZeroDate AS Date 
	
	IF DATE() =< CTOD(DB_CLOSURE_DAY + '/' + DB_CLOSURE_MONTH + '/' + ALLTRIM(STR(YEAR(DATE()))))
		* Las fechas de notificación a tener en cuenta se tomarán a partir de la fecha de cierre del año anterior que, por facilidad, ;
		se supone que es igual a la fecha de cierre del año en curso menos 365 días
		dZeroDate =  CTOD(DB_CLOSURE_DAY + '/' + DB_CLOSURE_MONTH + '/' + ALLTRIM(STR(YEAR(DATE())))) - 365
	ELSE
		* Las fechas de notificación a tener en cuenta se tomarán a partir del 1 de enero del año en curso
		dZeroDate =  CTOD('01/01/'+ ALLTRIM(STR(YEAR(DATE()))))
	ENDIF

	*Establece qué notificaciones mensuales colectivas se encuentran en el sistema correspondientes al periodo objetivo
	SELECT DISTINCT COD_PRE, COD_SUB, VAL(AÑO) as AÑO, VAL(MES) AS MES FROM (oCurrentEvent.sDC_TABLA) WHERE FEC_NOT >= dZeroDate INTO CURSOR rs362MonthlyNotifications

	* Establece el número total de meses correspondientes al período objetivo en los cuales se supondría que debería existir notificación del evento
	nZeroMonth = MONTH(dZeroDate)
	nZeroYear = YEAR(dZeroDate)

	sTargetYear =  ALLTRIM(STR(nZeroYear))
	IF nZeroYear != nFinalYear THEN 
		FOR iNextMonth=nZeroMonth TO 12
			sSQLCmd = sSQLCmd  + ' SELECT ' + sTargetYear + ' AS AÑO,' + ALLTRIM(STR(iNextMonth)) + ' AS MES FROM CALENDARIO UNION '
		NEXT 
	ENDIF 

	sTargetYear =  ALLTRIM(STR(nFinalYear))
	FOR iNextMonth=1 TO nFinalMonth 
		sSQLCmd = sSQLCmd  + ' SELECT ' + sTargetYear + ' AS AÑO,' + ALLTRIM(STR(iNextMonth)) + ' AS MES FROM CALENDARIO UNION '
	NEXT 
	sSQLCmd = LEFT(sSQLCmd,LEN(sSQLCmd)-6) + ' INTO CURSOR rsDueMonths'
	&sSQLCmd
	USE IN 'CALENDARIO'

	*Establece, en dos pasos, en qué meses hacen falta notificaciones
	*P1: Ejecuta el producto cartesiano entre UPGDs activas y meses en los que debería haber notificación
	SELECT U.*,rsDueMonths.* FROM ;
		(SELECT DISTINCT COD_PRE, COD_SUB, RAZ_SOC, ES_UNI_NOT AS TIPO_UNIDAD, ACT_SIV, SER_CESARE, SER_PAR_VA, SER_COLECI, SER_HERNIO, SER_REVASC FROM UPGD WHERE ACT_SIV=1) U,rsDueMonths  ;
		INTO CURSOR rsDueMonthsWithUPGDs
	USE IN 'UPGD'
	
	*P2: Establece por cada UPGD activa en qué mes hace falta notificación
	SELECT rsDueMonthsWithUPGDs.* FROM rsDueMonthsWithUPGDs LEFT OUTER JOIN rs362MonthlyNotifications ;
		ON rsDueMonthsWithUPGDs.AÑO=rs362MonthlyNotifications.AÑO AND rsDueMonthsWithUPGDs.MES=rs362MonthlyNotifications.MES ;
		WHERE rs362MonthlyNotifications.AÑO IS NULL INTO CURSOR rs_Notificaciones_Faltantes_362
	
	nAnomalies = _TALLY
	DO Form frmSearchRec WITH 'rs_Notificaciones_Faltantes_362','.T.',sXLSPath, .T., "Meses pendientes por notificar" 
	
	RELEASE oCurrentEvent
ENDPROC

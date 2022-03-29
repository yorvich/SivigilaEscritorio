#INCLUDE SIVIGILA.H

DEFINE CLASS SIVIGILADataUpdater AS Custom

sErrMsg = ''

sSIVIGILAMark = ''


FUNCTION Init

	This.sSIVIGILAMark = SIVIGILA_MARK
		
ENDFUNC


PROCEDURE updateDataFrom (masterTableName as String, sSourceFileNameAndPath as string, bAdjustmentCare as Boolean, oProgressBar as Object)
	
	*Actualiza los registros de la tabla masterTableName correspondientes al evento This.sEventCode de acuerdo con la información que venga ;
	en el archivo CSV sSourceFileNameAndPath.;
	Para ello, hace uso de una base de datos espejo de la del SIVIGILA en donde se cargan los registros del archivo sSourceFileNameAndPath,;
	se detectan cuáles de esos registros tienen información diferente en relación con los de masterTableName, se hacen las actualizaciones ;
	apropiadas en los campos de los registros de masterTableName y se deja rastro de la actualización efectuada en la tabla tbHistorialDeCambios.
	
	*bAdjustmentCare(opcional): si es .F., agrega el campo AJUSTE a la lista de campos sEXCLUDED_FIELDS (definido más adelante), con ello, ;
	a la hora de detectar diferencias entre registros de la tabla masterTableName y registros del correspondiente archivo sSourceFileNameAndPath,;
	no se considerará que diferencias en el campo AJUSTE impliquen diferencias entre los registros; si es .T., el campo AJUSTE no se ;
	agregará a la lista de campos sEXCLUDED_FIELDS y por tanto, diferencias en el campo AJUSTE, implicarán diferencias en los registros.
	
	*oProgressBar(opcional): muestra el progreso de la actualización de registros utilizando un termómetro
	
	*NOTAS: este procedimiento cierra todas las tablas y base de datos que se encuentren abiertas en la sesión de datos actual y no las restablece.;
	Al final de su ejecución cierra las tablas que ha utilizado.
	 

	#DEFINE sEXCLUDED_FIELDS " FEC_AJU PERIODO FLU_NOT INMEDIATA NOTIFICA FECHACARGA SEM_REC ESTADOTRAN VERSION INGR_X_NI EST_INGR EST_NOTIF "
	*Lista de campos que aún cuando sean diferentes en un registro de sSourceFileNameAndPath en relación con el correspondiente de masterTableName;
	no se tienen en cuenta para efectos de establecer diferencias entre registros


	#DEFINE sNON_UPDATABLE_FIELDS " PAIS TIP_CAS "
	*Lista de campos que NO son actualizables independientemente de que en un registro de sSourceFileNameAndPath se encuentren valores diferentes ;
	en relación con el registro correspondiente de masterTableName
	

	LOCAL bContinue as Boolean, sCandidatesRSName AS String, sExcludedFields as String
	LOCAL bAdjustmentChanged as Boolean 
	LOCAL sOldSafety AS String, sOldExclusive AS String, sOldPath as String, sOldProc as String
	LOCAL sSQLCmd as String, sCmd as String

*SET STEP ON 	
	bResult = .T.
	IF !DIRECTORY('.\DatosValidados') THEN
		bResult = .F.
	ELSE
		sOldSafety = SET("Safety")
		SET SAFETY OFF

		sOldExclusive = SET("Exclusive")
		SET EXCLUSIVE ON
		
		sOldDeleted = SET("Deleted")
		SET DELETED ON
		
		*sOldPath = SET("Path")
		sOldProc = SET("Procedure")
		
		glDisableRules = .T.
		
		*SET PATH TO '&sOldCurrentDir'
		SET PROCEDURE TO 
		
		CLOSE TABLES ALL
		CLOSE DATABASES ALL
		
		sCmd="SET PROCEDURE TO '" + PATH_TO_COMMON_LIB + "TablesHandler' ADDITIVE"
		&sCmd

		IF bAdjustmentCare THEN
			sExcludedFields = sEXCLUDED_FIELDS 
		ELSE
			sExcludedFields = ' AJUSTE ' + sEXCLUDED_FIELDS 
		ENDIF
*SET STEP ON 
		*Carga en la tabla espejo los registros que tienen información actualizada
		=useTable('.\DatosValidados\' + masterTableName)
		ZAP
		SET EXCLUSIVE &sOldExclusive 
		APPEND FROM '&sSourceFileNameAndPath' CSV
		USE
		
		*Establece qué registros tiene información realmente nueva en relación con los existentes en masterTableName
		sCmd="SET PROCEDURE TO '" + PATH_TO_COMMON_LIB + "QueriesHandler' ADDITIVE"
		&sCmd

		sSQLCmd = ''
		nRemainingRecords = fullCrossTables('.\DatosValidados\' + masterTableName, masterTableName, ;
											sExcludedFields, "LEFT OUTER", "R.COD_EVE IS NULL",@sSQLCmd)
		bContinue = (nRemainingRecords>0)
		sFormerMasterTableName = masterTableName		
		
		IF bContinue THEN
			* Configura las areas de trabajo con las tablas necesarias para llevar a cabo la operación. La tabla maestra masterTableName;
			será renombrada de acuerdo con los resultados arrojados por fullCrossTables

			sCandidatesRSName = "rsJoinedRecs"
			masterTableName = masterTableName + "002"
			
			SELECT (masterTableName)
			TRY
				SET ORDER TO LINKER_IDX
			CATCH TO oException
				bContinue = .F.
				bResult = .F.
			ENDTRY
			=ATAGINFO(aMasterIDXInfo)
			nMasterIdxInfoRow = ASCAN(aMasterIDXInfo,'LINKER_IDX',-1,-1,1,15)
			sMasterKeyFields = aMasterIDXInfo(nMasterIdxInfoRow, 3)
			nMasterKeyFields = OCCURS('+', sMasterKeyFields) + 1

			nMasterTablefields = AFIELDS(aMasterTableFieldsInfo)
			DIMENSION aMasterTableFields(1)
			DO Transpose WITH aMasterTableFieldsInfo,aMasterTableFields IN Utilities
			DIMENSION aMasterTableFields(1,nMasterTablefields)
			
			IF bContinue THEN
				SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
				
				nMasterTableRecords=RECCOUNT(masterTableName)
				nTraversedRecords = 0
								
				* Intenta actualizar los registros de masterTableName siempre y cuando los registros fuente tengan algún valor ;
				distinto en alguno de sus campos, en relación con los valores existentes en masterTableName
				SELECT (sCandidatesRSName) 
				nTraversedRecords = 0
				nUpdatedRecords = 0
				nAdjustedRecords = 0

				SCAN
					SCATTER MEMVAR
					
					= ALINES(aSearchedFields, sMasterKeyFields, 1 + 4 + 8, "+")
					FOR iField = 1 TO nMasterKeyFields 
						sGetFieldValue = sCandidatesRSName + "." + aSearchedFields(iField)
						sDefine_i_ValueFieldVar = "Value" + ALLTRIM(STR(iField)) + " = &sGetFieldValue"
						&sDefine_i_ValueFieldVar 
					ENDFOR

					sKeyValueToFind = ''
					FOR iField = 1 TO nMasterKeyFields 
						iFieldValue = "Value" + ALLTRIM(STR(iField))
						sKeyValueToFind = sKeyValueToFind + &iFieldValue
					ENDFOR

					sScanCondition = "&sMasterKeyFields" + " = '" + sKeyValueToFind + "'"
					SELECT (masterTableName)
					SEEK sKeyValueToFind 
					bTargetRecFound = FOUND()
					nMasterRecNumber = 0
					
					sCurrentCaseClasification = ''
					bFormerCase=.F.
					bPseudoFormerCase=.F.
					dAdjustmentDate = CTOD('')
					dLastSignificantAdjustmentDate = CTOD('')

					DO WHILE bTargetRecFound
					
						*Establece cuál es el el registro con la información más actual del caso para actualizarlo, si es pertinente
						
						IF VAL(&masterTableName..AJUSTE) = 0 AND !bFormerCase THEN
							*Se trata del registro o reporte inicial del caso
							bFormerCase=.T.
							IF EMPTY(sCurrentCaseClasification) AND isField(masterTableName, 'TIP_CAS') THEN
								sCurrentCaseClasification = &masterTableName..TIP_CAS
							ENDIF
							nMasterRecNumber = RECNO()
						ENDIF
						
						IF 	VAL(&masterTableName..AJUSTE) > VAL(CLINICAL_DISCARD_ADJUSTMENT) THEN
							*Se trata de un registro que contiene datos modificados del registro inicial del caso excepto en el tipo de caso
							IF isField(masterTableName, 'FEC_AJU') THEN
								IF &masterTableName..FEC_AJU >= dAdjustmentDate THEN
									bFormerCase = .T.
									nMasterRecNumber = RECNO()
									dAdjustmentDate = &masterTableName..FEC_AJU 
								ENDIF
							ELSE
								bFormerCase = .T.
								nMasterRecNumber = RECNO()
								dAdjustmentDate = CTOD('')
							ENDIF
						ENDIF
						
						IF 	(VAL(&masterTableName..AJUSTE) < VAL(CHANGE_ANY_VALUE_ADJUSTMENT)) AND (&masterTableName..AJUSTE <> '0' ;
							AND !EMPTY(&masterTableName..AJUSTE)) THEN
							*Se trata de un registro que contiene una modificación de la clasificación del caso originalmente registrada

							IF isField(masterTableName, 'FEC_AJU') THEN
								IF &masterTableName..FEC_AJU >= dLastSignificantAdjustmentDate THEN
									sCurrentCaseClasification = &masterTableName..AJUSTE
									dLastSignificantAdjustmentDate = &masterTableName..FEC_AJU
								ENDIF
							ENDIF
														
							IF !bFormerCase AND !bPseudoFormerCase THEN
								*Aún no se han capturado los datos de las variables asociadas al paciente, por tanto, se capturan pero el caso;
								no puede considerarse como el registro inicial del paciente
								bPseudoFormerCase=.T.
								nMasterRecNumber = RECNO()
							ENDIF	
						ENDIF
						SKIP 1
						bTargetRecFound = (&sScanCondition)
					ENDDO
*SET STEP ON 														
					IF nMasterRecNumber != 0 THEN
						GO (nMasterRecNumber)

						bAdjustmentChanged = .F.
						nAdjustmentColNumber = 0	
						IF This.detectChange(@aMasterTableFields, @bAdjustmentChanged, @nAdjustmentColNumber, sExcludedFields + sNON_UPDATABLE_FIELDS) THEN
							*Se detectó que el registro fuente actual presenta un cambio en algún campo en relación con los valores;
							existentes en el registro con la información más actual de masterTableName
							
							IF bAdjustmentChanged THEN
								*Se detectó un cambio en el campo AJUSTE, por tanto se agrega el registro con el nuevo ajuste a la tabla maestra,;
								-siempre y cuando cumpla ciertas condiciones-, estableciendo como fecha de ajuste, la fecha actual y asignando ;
								valores por defecto para ciertos campos
*SET STEP ON 
								IF !EMPTY(sCurrentCaseClasification) THEN
									*Dentro del grupo de registros que conforman el caso en masterTableName, se ha detectado una modificación ;
									en la clasificación inicial del caso
									IF sCurrentCaseClasification <> aMasterTableFields(3,nAdjustmentColNumber) AND ;
										aMasterTableFields(3,nAdjustmentColNumber) $ CONFIRMATION_OR_DISCARD_ADJUSTMENTS THEN
									
										m.FEC_AJU = DATE()
										m.PAIS = PAIS
										m.EstadoTran = EstadoTran
										m.Nom_dil_fi = sDEBUGGER_USER
										m.Tel_dil_fi = sDEBUGGER_PHONE_NUMBER
										INSERT INTO (ALIAS()) FROM MEMVAR
										
										nMasterRecNumber = RECNO()

										INSERT INTO tbHistorialDeCambios (IdRegistro,TB_Fuente,CampoFuent,ValorAnter,ValorNuevo,FechaCambi,TipoCampo,Clave_rec) ;
												VALUES (nMasterRecNumber,sFormerMasterTableName,aMasterTableFields(1,nAdjustmentColNumber), ;
												'NA', TRANSFORM(aMasterTableFields(3,nAdjustmentColNumber)), DATE(),	;
												aMasterTableFields(4,nAdjustmentColNumber), sKeyValueToFind)
										
										nAdjustedRecords = nAdjustedRecords + 1
									ENDIF
								ENDIF
							ELSE
								FOR nFields=1 TO ALEN(aMasterTableFields,2)
									IF aMasterTableFields(5,nFields) THEN
										*Actualiza el n-ésimo campo del registro en la tabla maestra y deja rastro de la operación efectuada siempre y cuando ;
										sea un campo actualizable según se define en la tabla tbDataUpdaterConfig
										IF NOT ISNULL(fieldValue('FIELDNAME','tbDataUpdaterConfig','COD_EVE + ALLTRIM(TABLENAME) + ALLTRIM(FIELDNAME)="' +;
															 &masterTableName..COD_EVE + sFormerMasterTableName + aMasterTableFields(1,nFields) + '"')) THEN
											REPLACE (aMasterTableFields(1,nFields)) WITH aMasterTableFields(3,nFields)
				
											INSERT INTO tbHistorialDeCambios (IdRegistro,TB_Fuente,CampoFuent,ValorAnter,ValorNuevo,FechaCambi,TipoCampo,Clave_rec) ;
													VALUES (nMasterRecNumber,sFormerMasterTableName,aMasterTableFields(1,nFields), ;
													TRANSFORM(aMasterTableFields(2,nFields)), TRANSFORM(aMasterTableFields(3,nFields)), DATE(),	;
													aMasterTableFields(4,nFields), sKeyValueToFind)
										ENDIF
									ENDIF
								NEXT
								nUpdatedRecords = nUpdatedRecords + 1
							ENDIF
					    ENDIF
						
						DIMENSION aMasterTableFields(1,nMasterTablefields)
						
						SELECT (sCandidatesRSName) 			
					ELSE
						*Se ha identificado en el archivo de actualizaciones, un registro que no se encuentra en la tabla masterTablename
						
						*glDisableRules = .T.
						INSERT INTO (masterTableName) FROM MEMVAR 
						*glDisableRules = .F.
					ENDIF

					nTraversedRecords = nTraversedRecords + 1
					IF VARTYPE(oProgressBar)!='L' THEN
						oProgressBar.Value = CEILING((nTraversedRecords/nRemainingRecords)*100)
					ENDIF

				ENDSCAN
						
				RELEASE oDataExporter
			ENDIF
		ENDIF
		
		DO CloseTables WITH "&masterTableName,&sCandidatesRSName,tbHistorialDeCambios," + sFormerMasterTableName + "001"
		CLOSE DATABASES ALL		
		
		*SET PATH TO '&sOldPath'
		SET SAFETY &sOldSafety
		SET DELETED &sOldDeleted
		
		sCmd = "SET PROCEDURE TO " + sOldProc
		&sCmd

		glDisableRules = .F.
	ENDIF
ENDPROC




HIDDEN FUNCTION detectChange(aSourceTableFields, bAdjustmentChanged AS Boolean, nAdjustmentColNumber as number, sExcludedFields as String)
	
	*Retorna .T. si los campos mencionados en aSourceTableFields que supuestamente pertenecen a la tabla abierta en el área actual;
	son iguales todos ellos a los correspondientes valores almacenados en variables de memoria, excepto por aquellas contenidas en ;
	la lista separada por espacios sExcludedFields; en caso contrario, retorna .F.
	
	*En caso de que se detecten diferencias, el arreglo aSourceTableFields será redimensionado agregándole cuatro filas que contendrán;
	en el n-ésimo campo con valor diferente, el valor actual del campo, en la tercera fila el de la correspondiente variable de memoria,;
	en la cuarta fila, el tipo de campo dado por VARTYPE() y, en la quinta fila, el valor .T. que indica que en ese campo se encontró diferencia.;
	por otra parte, si se detecta diferencia en el campo AJUSTE, bAdjustmentChanged será .T. y nAdjustmentColNumber será el número ;
	de la columna en aSourceTableFields correspondiente a ese campo
	
	
	LOCAL bIsEqual as Boolean 
	*Valor retornado por esta función
	
	LOCAL nSourceTableFields as Number
	*Número de campos mencionados en aSourceTableFields
	
	LOCAL nField as Integer 
	*Índice para recorrer arreglos de campos
	
	LOCAL sSourceTableName as String 
	*Nombre de la tabla abierta en el área de trabajo actual

	LOCAL nCurrentTableFields as Number 
	
	DIMENSION aCurrentTableFields(1)
	*Almacena la estructura de la tabla abierta en el área de trabajo actual
	
	bIsEqual=.F.
	sSourceTableName=ALIAS()
	IF !EMPTY(sSourceTableName) THEN
		DIMENSION aSourceTableFields(5,ALEN(aSourceTableFields,2))
		
		nSourceTableFields = ALEN(aSourceTableFields,2)
		
		*Obtiene la estructura de la tabla abierta en el área de trabajo actual
		nCurrentTableFields = AFIELDS(aCurrentTableFields)
		
		bIsEqual=.T.
		nField=1
		DO WHILE nField<=nSourceTableFields
		
			IF ASCAN(aCurrentTableFields,aSourceTableFields(1,nField),-1,-1,-1,15) > 0 THEN
				*El n-ésimo campo que aparece en aSourceTableFields efectivamente es un campo de la tabla actual
				
				sMemoryValue = "m." + aSourceTableFields(1,nField)
				IF VARTYPE(&sMemoryValue)<>'U' THEN
					sFieldValue = sSourceTableName + "." + aSourceTableFields(1,nField)
					IF  !(aSourceTableFields(1,nField) $ sExcludedFields) THEN
						IF &sMemoryValue!=&sFieldValue THEN
							*El valor del campo es diferente de la correspondiente variable de memoria
							aSourceTableFields(2,nField) = &sFieldValue 
							aSourceTableFields(3,nField) = &sMemoryValue
							aSourceTableFields(4,nField) = VARTYPE(&sFieldValue)
							aSourceTableFields(5,nField) = .T.
							bIsEqual=.F.
							
							IF UPPER(aSourceTableFields(1,nField)) = 'AJUSTE' THEN
								bAdjustmentChanged = .T.
								nAdjustmentColNumber = nField
							ENDIF
						ENDIF
					ENDIF
				ENDIF
			ENDIF
			nField = nField + 1
		ENDDO
		
		IF bIsEqual THEN
			DIMENSION aSourceTableFields(1,ALEN(aSourceTableFields,2))
		ENDIF
		
	ENDIF
	
	RETURN !bIsEqual
	
ENDFUNC



PROCEDURE updateBasicDataFieldFrom(masterTableName as String, sSourceFileNameAndPath as string, sFieldNames as string, oProgressBar as Object)
	
	*Actualiza los registros de la tabla de datos básicos masterTableName de acuerdo con la información que se encuentre en la tabla ;
	sSourceFileNameAndPath teniendo en cuenta aquellos registros de masterTableName que existan en sSourceFileNameAndPath según la clave ;
	{TIP_IDE, NUM_IDE, FEC_CON} y tales que masterTableName!sFieldName(i).Value !=  sSourceFileNameAndPath!sFieldName(i).Value con ;
	i = 1,...,n y n=Número de campos en la lista separada por comas sFieldNames
	
	*Para ello, cada vez que encuentre un caso que debe cambiarse en uno de los campos de sFieldNames, inserta en masterTableName -si es necesario- ;
	un registro de ajuste que queda identificado así: ;
	AJUSTE = CHANGE_ANY_VALUE_ADJUSTMENT ;
	FEC_AJU <-- DATE() ;
	NOTIFICA <-- 0 ;
	NOM_DIL_FI <-- "SISTEMA SIVIGLA " + Valor hexadecimal correspondiente al campo cambiado según se encuentre definido en el objeto de DB asociado. ;
	Si el caso ya tiene un registro de ajuste de esa naturaleza, entonces, en vez de insertar uno nuevo, lo actualiza mediante la siguiente asignación: ;
	AJUSTE = CHANGE_ANY_VALUE_ADJUSTMENT ;
	FEC_AJU <-- DATE() ;
	NOM_DIL_FI <-- NOM_DIL_FI + Valor hexadecimal correspondiente al campo cambiado según se encuentre definido en el objeto de DB asociado.
	
	*El método deja rastro de la actualización efectuada en la tabla tbHistorialDeCambios.
	
	*oProgressBar: previsto para futuras modificaciones. En la implementación actual no se usa.
	
	
	LOCAL bInsertAdjusmentRec as Boolean 
	*Indica si debe insertarse un registro de ajuste CHANGE_ANY_VALUE_ADJUSTMENT para el caso
	
	LOCAL sLastCaseAdjustment as string 
	*Valor de ajuste del último registro de un caso. Este registro corresponde a aquel que aparezca al final al ordenarlos por FEC_AJU
	
	LOCAL nLastCaseRec As Number
	*Número identificador (dado por RECNO()) del último registro de un caso. Este registro corresponde a aquel que aparezca al final ;
	al ordenarlos por FEC_AJU
	
	LOCAL sSQLCmd as String, sCmd as String
	sSQLCmd = ''
	sCmd = ''
	
	DIMENSION aFieldName(1)
	nFieldsToUpdate = ALINES(aFieldName, sFieldNames, 15, ",")
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\QueriesHandler') ADDITIVE
	SET PROCEDURE TO SivigilaUtilities ADDITIVE 
	
	=SelectTable(sSourceFileNameAndPath,'IDX_ID',.T.)
	sSourceTableName = ALIAS()
	
	=SelectTable(masterTableName,,.F.)
	sLinkingExpr = ALLTRIM(fieldValue('TAG_EXPR', 'MASTER_INDEX',"TABLE_NAME='" + masterTableName + "' AND TAG_NAME='LINKER_IDX'"))
	sMasterKeyFields = getKeyFields(masterTableName)

	SELECT RECNO() as nRec,* FROM &masterTableName INTO CURSOR rsMasterTbToUpdate
	
	*sFilter = "(AJUSTE='0' OR EMPTY(AJUSTE) OR AJUSTE='" + CHANGE_ANY_VALUE_ADJUSTMENT + "')" 

	*Teniendo en cuenta que el posible cambio a un registro tiene lugar si y solo si los campos de la clave usada para tales efectos son coincidentes;
	cruza las tablas maestra y fuente de cambios con base en dicha clave
	IF fullCrossTables2('rsMasterTbToUpdate', sSourceFileNameAndPath, 'TIP_IDE,NUM_IDE,FEC_CON', 'INNER', , @sSQLCmd, 'rsTmpBasicDataCandidates') > 0 THEN 
		glDisableRules = .T.

		*Ordena apropiadamente el cursor rsTmpBasicDataCandidates obtenido, de tal forma que se garantice insertar/actualizar en la tabla ;
		masterTableName registros de ajuste 7 teniendo en cuenta la posible existencia previa de ese tipo de ajustes para algún caso
		sSQLCmd = 'SELECT VAL(AJUSTE) as nAjuste,* FROM rsTmpBasicDataCandidates WHERE COD_EVE!="' + NO_NOTIFICATION_EVENT_CODE + '" ORDER BY ' +;
					STRTRAN(sLinkingExpr,"+",",") + ', nAjuste, FEC_AJU INTO CURSOR rsBasicDataCandidates READWRITE'
		&sSQLCmd 	
		SELECT rsBasicDataCandidates 
		SET RELATION TO TIP_IDE+NUM_IDE INTO (sSourceTableName) ADDITIVE 
		GO TOP

		DO WHILE !EOF()
			SCATTER MEMVAR 
			bRecHasChanged = .F.
			bInsertAdjusmentRec = .F.
			sLastCaseAdjustment = ''
			nLastCaseRec = 0
			
			sCurrentBasicDataKey = EVALUATE(getKeyFields(masterTableName))
			
			*Recorre los registros de datos básicos que conforman el caso identificado por sCurrentBasicDataKey 
			DO WHILE (sCurrentBasicDataKey == EVALUATE(sMasterKeyFields))
				FOR iFieldToUpdate=1 TO nFieldsToUpdate
					sFieldName = aFieldName[iFieldToUpdate]
					IF rsBasicDataCandidates.&sFieldName != &sSourceTableName..&sFieldName THEN
						
						*Hace el cambio efectivo en el campo sFieldName de la tabla masterTableName 
						SELECT (masterTableName)
						GO (rsBasicDataCandidates.nRec)
						REPLACE (sFieldName) WITH &sSourceTableName..&sFieldName
						bRecHasChanged = .T.
						
						*Establece que posiblemente se deba insertar un registro de ajuste CHANGE_ANY_VALUE_ADJUSTMENT para el caso ;
						que dé cuenta de la modificación realizada
						bInsertAdjusmentRec = .T.
																	
						sAssignCmd = "m." + sFieldName + "= &sSourceTableName..&sFieldName"
						&sAssignCmd

						*Deja rastro del cambio efectuado
						INSERT INTO tbHistorialDeCambios ( Usuario, IdRegistro, tb_Fuente, campoFuent, valorAnter, valorNuevo, FechaCambi, ;
								TipoCampo, Clave_rec) VALUES ('', rsBasicDataCandidates.nRec, masterTableName, sFieldName, rsBasicDataCandidates.&sFieldName, ;
								&sSourceTableName..&sFieldName, DATE(), VARTYPE(&sSourceTableName..&sFieldName), sCurrentBasicDataKey )
					ELSE
					ENDIF
				NEXT iFieldToUpdate
				
				IF bRecHasChanged THEN 
					REPLACE NOM_DIL_FI WITH ALLTRIM(NOM_DIL_FI) + ' ' + This.sSIVIGILAMark + '[C00000]'
				ENDIF
				
				SELECT rsBasicDataCandidates 
				sLastCaseAdjustment = rsBasicDataCandidates.AJUSTE
				nLastCaseRec = rsBasicDataCandidates.nRec
				
				SKIP 
			ENDDO 
			
			IF bInsertAdjusmentRec THEN 
				*Posiblemente debe insertarse un registro de ajuste CHANGE_ANY_VALUE_ADJUSTMENT  dadndo cuenta de la modificación efectuada por el sistema
				
				IF sLastCaseAdjustment != CHANGE_ANY_VALUE_ADJUSTMENT THEN 
					*Agrega un registro de ajuste tipo CHANGE_ANY_VALUE_ADJUSTMENT y lo adjudica al SISTEMA SIVIGILA
					
					m.AJUSTE = CHANGE_ANY_VALUE_ADJUSTMENT
					m.FEC_AJU = DATE()
					m.NOTIFICA = 0
					m.NOM_DIL_FI = 'SISTEMA SIVIGILA ' + This.sSIVIGILAMark + '[C00000]'
					
					INSERT INTO &masterTableName FROM MEMVAR 
					bInsertAdjusmentRec = .T.
				ELSE
					*Actualiza, si es del caso, el registro existente más reciente de ajuste CHANGE_ANY_VALUE_ADJUSTMENT 
					SELECT (masterTableName)
					GO (nLastCaseRec)
					IF !('[C00000]' $ NOM_DIL_FI) THEN 
						REPLACE NOM_DIL_FI WITH ALLTRIM(NOM_DIL_FI) + ' ' + This.sSIVIGILAMark + '[C00000]', FEC_AJU WITH DATE() 
					ENDIF
					
					SELECT rsBasicDataCandidates 
				ENDIF
			ENDIF 
		ENDDO 

		glDisableRules = .F.
	ELSE
		This.sErrMsg = 'No se pudo actualizar ningún registro pues no hay ninguno en la tabla maestra que se encuentre en el archivo de cambios.'
	ENDIF
	
	USE IN rsBasicDataCandidates
	USE IN (sSourceTableName)
ENDPROC

ENDDEFINE

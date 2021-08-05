#INCLUDE SIVIGILA.H


PROCEDURE addBasicData(candidatesRSName, duplicatesRSName, sourceFilePath, sourceFileName, ;
						existentsFilePath, existentsFileName, duplicatesFilePath, sResult as Boolean, ;
						sErrorMsg as String, sNotificationWeek as Number, bTraceNewRecs as boolean)

	* candidatesRSName: nombre de una tabla o cursor que contiene los registros que son candidatos a ;
		ser almacenados en la tabla maestra de Pacientes. Un registrto es candidato si ;
		ha superado las pruebas de validación

	* duplicatesRSName: nombre de una tabla o cursor que contiene los registros duplicados a ;
		ser almacenados en la tabla maestra de Pacientes. Un registro es duplicado si ;
		su clave de identificación es duplicada
						
	* sourceFilePath + sourceFileName: nombre y ubicación de la tabla fuente que contiene todos los ;
		registros (candidatos y no candidatos) que deberían ser almacenados en la tabla maestra de Pacientes					

	* existentsFilePath, existentsFileName: nombre y ubicación de la tabla en donde deben guardarse ;
		los registros que ya existen en la tabla maestra y que no pudieron ser grabados en ella
		
	* duplicatesFilePath: ruta de la carpeta en donde deben guardarse los registros de duplicatesRSName

	* sNotificationWeek (opcional): valor que debe asignarse a la semana de notificación (Tables.PACIENTE.NOTIFICA);
	para los registros nuevos que se adicionen a la tabla PACIENTE. El valor por defecto de este parámetro es 0.
	
	* bTraceNewRecs (opcional): indica si debe generarse un archivo excel en la carpeta ... que dé cuenta ;
	de todos los registros nuevos que se adicionaron a la tabla maestra de pacientes. El valor por defecto es .F.

	LOCAL bINGR_X_NI_IsField as Boolean, sQueryFields as String

	LOCAL masterTableName AS String 
	masterTableName = 'PACIENTE'

	LOCAL sErrorMsg as String 
	sErrorMsg = ''

	IF TOT_PTE >= 1
		
		IF VARTYPE(sNotificationWeek)='L' THEN
			sNotificationWeek = 0
		ENDIF
		
		* Configura las areas de trabajo con las tablas necesarias para llevar a cabo la operación
		SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
		SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\QueriesHandler') ADDITIVE
		
		IF !USED(masterTableName) THEN
			USE (masterTableName) IN 0
		ENDIF
		bContinue = UseTable(sourceFilePath + '\' + sourceFileName) AND UseTable(existentsFilePath + '\' + existentsFileName) 

		IF bContinue THEN
			CON_PTE = 0
			sResult = .T.
			
			nExistentRecords1 = 0
			nExistentRecords2 = 0

			* Agrega a la tabla maestra todos los registros candidatos nuevos que no ;
			correspondan a notificaciones negativas (cod_eve<>'000'). Un registro candidato es nuevo ;
			si su clave {AJUSTE, AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB} no se encuentra en ;
			la tabla maestra
			setNewRecsClause = "SELECT A.* FROM (SELECT *, " + ALLTRIM(STR(sNotificationWeek)) + " AS NOTIFICA FROM " + (candidatesRSName) ;
								+ " WHERE cod_eve!= '000') AS A LEFT OUTER JOIN paciente B ON "  ;
								+ " A.año+A.semana+A.cod_eve+A.tip_ide+A.num_ide+A.cod_pre+A.cod_sub+A.Ajuste = " ;
								+ " B.año+B.semana+B.cod_eve+B.tip_ide+B.num_ide+B.cod_pre+B.cod_sub+B.Ajuste" ;
								+ " WHERE B.cod_eve IS NULL " ;
								+ " INTO TABLE tmppacientes_a_importar"
			&setNewRecsClause

			nMasterTableRecords = RECCOUNT(masterTableName)
			nRemainingRecords = RECCOUNT(candidatesRSName)
			nRemainingDuplicatesRecords = CountTableRecords(duplicatesRSName)

			SELECT (masterTableName)
			TRY
				APPEND FROM tmppacientes_a_importar
			CATCH TO oException
				sResult=.F.
				sErrorMsg = sErrorMsg + oException.Message
			ENDTRY
				
			IF sResult THEN
				CON_PTE = CON_PTE  + CountTableRecords("tmppacientes_a_importar")	&&CON_PTE = CON_PTE + _TALLY
				* Incrementa el contador con la cantidad de registros agregados por el último Append
				
				nRemainingRecords = nRemainingRecords - CountTableRecords("tmppacientes_a_importar")	&&nRemainingRecords = nRemainingRecords - _TALLY
				
				* Marca los registros recien subidos a la tabla maestra de tal manera que no se tengan en cuenta ;
				en procesos posteriores
				=UpdateFieldsTable("PLANO_PACIENTES", "PLANO_PACIENTES.SUBE=1", "tmpPacientes_a_Importar", ;
									"AJUSTE, AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB")
				=TABLEUPDATE(1,.T.,sourceFileName)

				IF nRemainingRecords > 0 OR nRemainingDuplicatesRecords > 0 THEN
				
					*Descarta como registros candidatos a agregar a la tabla maestra de pacientes todos aquellos que no ;
					correspondan a notificaciones negativas (cod_eve<>'000'), que correspondan a ajustes <> '7' y que ya ;
					existan en la tabla maestra. Se considera que un registro candidato existe en la tabla maestra si la clave ;
					{AJUSTE, AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB, INGR_X_NI} ya se encuentra en la tabla maestra
					bINGR_X_NI_IsField = isField(candidatesRSName,"INGR_X_NI")									
						
					sSetProcCmd = "SET PROCEDURE TO '" + PATH_TO_COMMON_LIB + "QueriesHandler' ADDITIVE"
					&sSetProcCmd 

					sQueryFields = "AJUSTE , AÑO , SEMANA , COD_EVE , TIP_IDE , NUM_IDE , COD_PRE , COD_SUB"

					sSQLCmd = "SELECT DISTINCT " + qualifyFields(sQueryFields, ',', 'TND')
					IF bINGR_X_NI_IsField THEN
						sSQLCmd = sSQLCmd + ",TND.INGR_X_NI "
					ENDIF
					sSQLCmd = sSQLCmd + " FROM (SELECT " + sQueryFields 
					
					IF bINGR_X_NI_IsField THEN
						sSQLCmd = sSQLCmd + ",INGR_X_NI"
					ENDIF
					sSQLCmd = sSQLCmd + " FROM " + candidatesRSName +;
							" WHERE COD_EVE!= '000' AND AJUSTE IN (' ','0','3','4','5','6','D')) AS TND " +;
							" INNER JOIN " + masterTableName + " ON "  +;
							matchFields(sQueryFields, 'TND', sQueryFields, masterTableName, ",") + " "
							
					IF bINGR_X_NI_IsField THEN
						sSQLCmd = sSQLCmd + " AND TRANSFORM(" + candidatesRSName + ".INGR_X_NI,'Y') = TRANSFORM(" + masterTableName + ".INGR_X_NI,'Y') "
					ENDIF
					sSQLCmd = sSQLCmd +	" INTO CURSOR rsExistentRecords NOFILTER"

					&sSQLCmd 
					nExistentRecords1 = _TALLY

					IF nExistentRecords1>0 THEN
						* Marca los registros descartados de tal manera que no se tengan en cuenta en procesos posteriores
						IF bINGR_X_NI_IsField THEN
							=UpdateFieldsTable("PLANO_PACIENTES", "PLANO_PACIENTES.SUBE=1", "rsExistentRecords", sQueryFields + ", INGR_X_NI")
						ELSE
							=UpdateFieldsTable("PLANO_PACIENTES", "PLANO_PACIENTES.SUBE=1", "rsExistentRecords", sQueryFields)
						ENDIF
						=TABLEUPDATE(1,.T.,sourceFileName)
						nRemainingRecords = nRemainingRecords - nExistentRecords1
					ENDIF		

					IF nRemainingRecords > 0 OR nRemainingDuplicatesRecords > 0 THEN
					
						*Descarta como registros candidatos a agregar a la tabla maestra de pacientes todos aquellos que no ;
						correspondan a notificaciones negativas (cod_eve<>'000'), que correspondan a ajustes '7' y que ya ;
						existan en la tabla maestra. Se considera que un registro candidato existe en la tabla maestra si la clave ;
						{AJUSTE, AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB, FEC_AJU} ya se encuentra en la tabla maestra
						
						sQueryFields = "AJUSTE , AÑO , SEMANA , COD_EVE , TIP_IDE , NUM_IDE , COD_PRE , COD_SUB , FEC_AJU"
						sSQLCmd = "SELECT DISTINCT " + qualifyFields(sQueryFields, ',', 'TND')
						sSQLCmd = sSQLCmd + " FROM (SELECT " + sQueryFields + " FROM " + candidatesRSName + " WHERE COD_EVE!= '000'  AND " + ;
									"AJUSTE = '7') AS TND INNER JOIN " + masterTableName + " ON " + ;
									matchFields(sQueryFields, 'TND', sQueryFields, "PACIENTE", ",") + ;
									" INTO CURSOR rsExistentRecords NOFILTER"
						&sSQLCmd 

		   				nExistentRecords2 = CountTableRecords("rsExistentRecords")
						
						IF nExistentRecords2>0 THEN
							* Marca los registros descartados de tal manera que no se tengan en cuenta en procesos posteriores
							=UpdateFieldsTable("PLANO_PACIENTES", "PLANO_PACIENTES.SUBE=1", "rsExistentRecords", sQueryFields)
							=TABLEUPDATE(1,.T.,sourceFileName)
							nRemainingRecords=nRemainingRecords - nExistentRecords2
						ENDIF
					ENDIF
					
				    *Agrega los registros descartados a la tabla de registros existentes
				    IF nExistentRecords1 + nExistentRecords2 > 0 THEN
						INSERT INTO (existentsFileName) (ARCHIVO, REG, DES_ARC, COD_PRE, COD_SUB, EVENTO, TIP_IDE, NUM_IDE, EXISTE_EN, SEMANA, AÑO) ;
							SELECT 'N'+SUBSTR(ARC_MAE,2,7), RECNO(), 'Not. Individual', COD_PRE, COD_SUB, COD_EVE, TIP_IDE, NUM_IDE, masterTableName, SEMANA, AÑO ;
							FROM plano_pacientes WHERE plano_pacientes.sube = 1
					ENDIF

					IF nRemainingRecords > 0 OR nRemainingDuplicatesRecords > 0 THEN
						* Intenta agregar los registros restantes siempre y cuando cumplan ciertas condiciones
						SELECT * FROM (sourceFileName) WHERE SUBE=0 INTO CURSOR remainingRecords NOFILTER
						SELECT remainingRecords  
*SET STEP ON 
						SCAN WHILE sResult
							SCATTER MEMVAR
							m.NOTIFICA = sNotificationWeek
							recordNumber=RECNO()
							sourceUPGD_Pre=COD_PRE
							sourceUPGD_Sub=COD_SUB
							sourceEventCode=COD_EVE
							sourceIdentType=TIP_IDE
							sourceIdentNumber=NUM_IDE
							sourceAdjustment=AJUSTE 
							sourceYear=AÑO
							sourceEventWeek=SEMANA
							sourcePatientKey = sourceYear + sourceEventWeek + sourceEventCode + sourceIdentType + sourceIdentNumber + ;
												sourceUPGD_Pre + sourceUPGD_Sub + sourceAdjustment 
							
							SELECT (masterTableName)
							SEEK (sourcePatientKey) ORDER TAG SEARCH2 &&AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB+AJUSTE                                           
							
							IF sourceEventCode = NO_NOTIFICATION_EVENT_CODE THEN 
								* SE TRATA DE UNA NOTIFICACION NEGATIVA
							   
							   IF COD_PRE+COD_SUB+SEMANA+AÑO != sourceUPGD_Pre + sourceUPGD_Sub + sourceEventWeek + sourceYear 
							      *La unidad que hizo la notificación negativa (COD_PRE+COD_SUB) es diferente ;
							       de la unidad que está notificando actualmente (sourceUPGD_Pre + sourceUPGD_Sub)
							   
							      AGR_REG=0
							      
							      * Determina si la notificación negativa no se encuentra ya almacenada en la tabla maestra;
							      	en cuyo caso, no debe volver a almacenarse (AGR_REG=1)
							      
							      DO WHILE !EOF() AND AJUSTE+AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE=sourcePatientKey
							         IF COD_PRE+COD_SUB=sourceUPGD_Pre + sourceUPGD_Sub
							            AGR_REG=1
							            EXIT
							         ENDIF
							         SKIP
							      ENDDO
							      IF AGR_REG=0
									TRY
										INSERT INTO PACIENTE FROM MEMVAR
										CON_PTE=CON_PTE+1
									CATCH TO oException
										sResult=.F.
										sErrorMsg = sErrorMsg + oException.Message
									ENDTRY
							      ELSE
							         errorMsg=masterTableName + ' - UPGD QUE REPORTO: ' + COD_PRE + '-' + COD_SUB + ;
							         			', PROCEDENCIA: '+MUN_PRO+', RESIDENCIA: '+COD_MUN
							         
							         INSERT INTO (existentsFileName) (ARCHIVO, REG, DES_ARC, COD_PRE, COD_SUB, EVENTO, ;
							         	TIP_IDE, NUM_IDE, EXISTE_EN, SEMANA, AÑO) VALUES ('N'+SUBSTR(ARC_MAE,2,7), ;
							         	recordNumber, 'Not. Individual', sourceUPGD_Pre, sourceUPGD_Sub, ;
							         	sourceEventCode, sourceIdentType, sourceIdentNumber, errorMsg, ;
							         	sourceEventWeek, sourceYear)
							      ENDIF
							   ELSE
							      errorMsg=masterTableName + ' - UPGD QUE REPORTO: ' + COD_PRE + '-' + COD_SUB + ;
							      			', PROCEDENCIA: ' + MUN_PRO + ', RESIDENCIA: ' + COD_MUN
							      
							      INSERT INTO (existentsFileName) (ARCHIVO, REG, DES_ARC, COD_PRE, COD_SUB, EVENTO, ;
							      	 TIP_IDE, NUM_IDE, EXISTE_EN, SEMANA, AÑO) ;
							         VALUES ('N'+SUBSTR(ARC_MAE,2,7), recordNumber, 'Not. Individual', ;
							         sourceUPGD_Pre, sourceUPGD_Sub, sourceEventCode, sourceIdentType, ;
							         sourceIdentNumber, errorMsg, sourceEventWeek, sourceYear)
							   ENDIF
							ELSE
								* Podría tratarse de una novedad sobre un ajuste previamente reportado
					 			
					 			IF FOUND() THEN
									* Cambia el reporte del ajuste existente en la tabla maestra por el nuevo que se está informando
									IF remainingRecords.FEC_AJU  >= &masterTableName..FEC_AJU THEN
										*El registro candidato posiblemente tenga datos más frescos que el registro existente en la tabla maestra
										
										TRY
											GATHER MEMVAR
											CON_PTE = CON_PTE + 1
										CATCH TO oException
											sResult=.F.
											sErrorMsg = sErrorMsg + oException.Message
										ENDTRY
									ENDIF
									
								    errorMsg =	masterTableName + ' - UPGD QUE REPORTO: ' + COD_PRE + '-' + COD_SUB + ;
								    			', PROCEDENCIA: ' + MUN_PRO + ', RESIDENCIA: ' + COD_MUN
								      
									INSERT INTO (existentsFileName) (ARCHIVO, REG, DES_ARC, COD_PRE, COD_SUB, EVENTO, ;
								    			TIP_IDE, NUM_IDE, EXISTE_EN, SEMANA, AÑO) ;
								    			VALUES ('N'+SUBSTR(ARC_MAE,2,7), recordNumber, 'Not. Individual', ;
								    			sourceUPGD_Pre, sourceUPGD_Sub, sourceEventCode, sourceIdentType, ;
												sourceIdentNumber, errorMsg, sourceEventWeek, sourceYear)
								ELSE
									TRY
										INSERT INTO (masterTableName) FROM MEMVAR
										CON_PTE=CON_PTE+1
									CATCH TO oException
										sResult=.F.
										sErrorMsg = sErrorMsg + oException.Message
									ENDTRY
								ENDIF
								
							ENDIF
							SELECT RemainingRecords
						ENDSCAN
					ENDIF

					IF sResult THEN
						*Agrega los registros duplicados
						
						SELECT (masterTableName)
						nLastRecNumber = RECCOUNT()
						TRY
							APPEND FROM (duplicatesRSName)
							IF _TALLY > 0 THEN
								*Asigna el valor del campo NOTIFICA a los registros recien agregados. El procesamiento es secuencial;
								por cuanto debe evitarse un problema con el bloqueo de registros
								sOldTag = TAG()
								SET ORDER TO
								GO (nLastRecNumber + 1)
								DO WHILE !EOF()
									sCmd = 'REPLACE NOTIFICA WITH ' + ALLTRIM(STR(sNotificationWeek)) 
									&sCmd
									SKIP 
								ENDDO
								SET ORDER TO TAG &sOldTag 
							ENDIF
						CATCH TO oException
							sResult=.F.
							sErrorMsg = sErrorMsg + oException.Message
						ENDTRY

						IF sResult THEN
							* Incrementa el contador con la cantidad de registros agregados por el último Append
							CON_PTE = CON_PTE + CountTableRecords(duplicatesRSName)	
							
							*Exporta los registros duplicados hacia un archivo excel para posterior corrección
							UseTable(duplicatesRSName)
							SELECT (duplicatesRSName)
							IF RECCOUNT(duplicatesRSName) > 0 THEN
								duplicatesExcelFileName=duplicatesFilePath + '\' + sourceFileName + '_DUPLICADOS'
								COPY TO [&duplicatesExcelFileName] XL5
								
								IF bTraceNewRecs THEN
									*Consolida en una única tabla todos los registros adicionados a la tabla maestra
									UseTable("tmppacientes_a_importar")
									SELECT tmppacientes_a_importar
									TRY
										APPEND FROM (duplicatesRSName)
									CATCH TO oException
										sErrorMsg = sErrorMsg + oException.Message
									ENDTRY
								ENDIF
							ENDIF
						ENDIF
					ENDIF
				ENDIF
				
				IF bTraceNewRecs THEN
					*Exporta los registros adicionados a la tabla maestra hacia un archivo excel
					UseTable("tmppacientes_a_importar")
					SELECT tmppacientes_a_importar
					IF RECCOUNT()>0 THEN 
						sNewRecsExcelFileName = duplicatesFilePath + '\' + sourceFileName + '_REGISTROS_NUEVOS'
						sCmd = 'COPY TO "' + sNewRecsExcelFileName + '" XL5'
						&sCmd
					ENDIF 
				ENDIF

			ENDIF

			*Borra las tabla temporales utilizadas durante el procesamiento
			DO DeleteTable WITH 'tmppacientes_a_importar'
			
			*Actualiza la tabla de registros EXISTENTES
			= TABLEUPDATE(1,.T.,existentsFileName,aImportErrors)
		ENDIF
	ENDIF

	*CLOSE TABLES
	DO CloseTables WITH "Eventos,&sourceFileName,&existentsFileName"
	DO DeleteTable WITH 'T_NonDups_Pacientes'
	DO DeleteTable WITH 'T_NonOrphans_Pacientes'
	DO DeleteTable WITH 'C_DUPS_Pacientes'
ENDPROC


FUNCTION addDuplicatesBasicData(sMasterTableName as String, sDuplicatesRSName as String, sNotificationWeek as String, sErrMsg as String ) as Boolean 
	
	LOCAL sOldTag as String 
	
	LOCAL bResult as Boolean 
	bResult = .T.
	
	SELECT (sMasterTableName)
	nLastRecNumber = RECCOUNT()
	TRY
		APPEND FROM (sDuplicatesRSName)
		IF _TALLY > 0 THEN
			*Asigna el valor del campo NOTIFICA a los registros recien agregados. El procesamiento es secuencial;
			por cuanto debe evitarse un problema con el bloqueo de registros
			sOldTag = TAG()
			SET ORDER TO
			GO (nLastRecNumber + 1)
			DO WHILE !EOF()
				sCmd = 'REPLACE NOTIFICA WITH ' + ALLTRIM(STR(sNotificationWeek)) 
				&sCmd
				SKIP 
			ENDDO
			SET ORDER TO TAG &sOldTag 
		ENDIF
	CATCH TO oException
		bResult=.F.
		sErrMsg = sErrMsg + oException.Message
	ENDTRY

	RETURN bResult
ENDPROC 
*-------------------------------------------------------------------------------------------------------------
PROCEDURE addComplementaryData(candidatesRSName, duplicatesRSName, sourceFilePath, sourceFileName,;
								existentsFilePath, existentsFileName, eventSuffix, duplicatesFilePath, sResult as Boolean, ;
								sErrorMsg as String, sNotificationWeek as Number)

	* candidatesRSName: nombre de una tabla o cursor que contiene los registros que son candidatos a ;
		ser almacenados en la tabla maestra de Eventos_xx. Un registro es candidato si ;
		ha superado las pruebas de validación

	* duplicatesRSName: nombre de una tabla o cursor que contiene los registros duplicados a ;
		ser almacenados en la tabla maestra de Pacientes. Un registrto es duplicados si ;
		su clave de identificación es duplicada
						
	* sourceFilePath + sourceFileName: nombre y ubicación de la tabla fuente que contiene todos los ;
		registros (candidatos y no candidatos) que deberían ser almacenados en la tabla maestra de Eventos_xx					

	* existentsFilePath, existentsFileName: nombre y ubicación de la tabla en donde deben guardarse ;
		los registros que ya existen en la tabla maestra y que no pudieron ser grabados en ella

	* eventSuffix: sufijo identificador de la tabla maestra de eventos en la que deben ser grabados ;
		los resgistros
						
	* duplicatesFilePath: ruta de la carpeta en donde deben guardarse los registros de duplicatesRSName

	* sNotificationWeek (opcional): valor que debe asignarse a la semana de notificación (Tables.Eventos_xx.NOTIFICA);
	para los registros nuevos que se adicionen a la tabla Eventos_xx. El valor por defecto de este parámetro es 0.

	IF TOT_DAT  >= 1

		IF VARTYPE(sNotificationWeek)='L' THEN
			sNotificationWeek = 0
		ENDIF
		
		SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
		SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
		
		* Configura las areas de trabajo con las tablas necesarias para llevar a cabo la operación
		eventMasterTableName='EVENTOS_' + eventSuffix
		
		bContinue = UseTable(eventMasterTableName) AND UseTable(sourceFilePath + '\' + sourceFileName) AND ;
					UseTable(existentsFilePath + '\' + existentsFileName) AND UseTable(candidatesRSName)

		IF bContinue THEN
			* Agrega a la tabla maestra de eventos_xx todos los registros candidatos nuevos. ;
			En general, un registro candidato es nuevo si su clave {AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB, AJUSTE} ;
			no se encuentra en la tabla maestra (el campo ajuste puede que no venga en el recordset de candidatos). ;
			Sin embargo, para ciertos eventos, la clave será diferente ;
			según se indica: ;
				EventSuffix = '31' {año+semana+cod_pre+cod_sub+cod_eve+cod_dep+cod_mun+especie_ve+ajuste};
				EventSuffix = '32' {mes+año+cod_pre+cod_sub+cod_eve+cod_dep+cod_mun} ;
				EventSuffix in {'23','43','91','94'} = {cod_eve+año+semana+ALLTRIM(cod_mun)+ALLTRIM(num_con)+ALLTRIM(cod_pre)+ALLTRIM(cod_sub)}
			
			sResult = .T.
			tagForSeek = 'LINKER_IDX'

			DO CASE
				CASE EventSuffix = '23' OR EventSuffix = '43' OR EventSuffix = '91' OR EventSuffix = '94'
					&& Enfermedades transmitidas por alimentos (Colectiva), Hepatitis A, Fiebre Tifoide
					setNewRecsClause='SELECT A.*, ' + ALLTRIM(STR(sNotificationWeek)) + ' AS NOTIFICA FROM ' ;
						+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + eventMasterTableName;
						+ ' B ON ( A.cod_eve = B.cod_eve';
						+ ' AND A.año = B.año';
						+ ' AND A.semana = B.semana';
						+ ' AND A.cod_mun = B.cod_mun';
						+ ' AND A.num_con = B.num_con';
						+ ' AND A.cod_pre = B.cod_pre';
						+ ' AND A.cod_sub = B.cod_sub)';
						+ ' WHERE B.cod_eve IS NULL INTO TABLE tmpEventos_a_importar.dbf'  
						
				CASE EventSuffix = '31'
					&& Vigilancia entomológica de la malaria
					setNewRecsClause='SELECT A.*, ' + ALLTRIM(STR(sNotificationWeek)) + ' AS NOTIFICA FROM ' ;
						+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + eventMasterTableName;
						+ ' B ON (A.año      = B.año';
						+ ' AND A.semana     = B.semana ';
						+ ' AND A.cod_pre    = B.cod_pre';
						+ ' AND A.cod_sub    = B.cod_sub';
						+ ' AND A.cod_eve    = B.cod_eve';
						+ ' AND A.cod_dep    = B.cod_dep';
						+ ' AND A.cod_mun    = B.cod_mun';
						+ ' AND A.especie_ve = B.especie_ve';
						+ ' AND A.ajuste     = B.ajuste)';
						+ ' WHERE B.cod_eve IS NULL INTO TABLE tmpEventos_a_importar.dbf'  

				CASE EventSuffix = '32'
					&& Informe mensual de actividades para la prevención y control de la rabia
					setNewRecsClause='SELECT A.*, ' + ALLTRIM(STR(sNotificationWeek)) + ' AS NOTIFICA FROM ' ;
						+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + eventMasterTableName;
						+ ' B ON A.mes+A.año+A.cod_pre+A.cod_sub+A.cod_eve+A.cod_dep+A.cod_mun = ' ;
						+ ' B.mes+B.año+B.cod_pre+B.cod_sub+B.cod_eve+B.cod_dep+B.cod_mun ';
						+ ' WHERE B.cod_eve IS NULL INTO TABLE tmpEventos_a_importar.dbf'  
												
				CASE EventSuffix = '80' OR EventSuffix = '81' OR EventSuffix = '88'  OR EventSuffix = '90' 
					&& IAD - Infecciones asociadas a dispositivos
					&& CAB - Infecciones asociadas consumo de antibioticos
					&& Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico - Colectivo
					setNewRecsClause='SELECT A.*, ' + ALLTRIM(STR(sNotificationWeek)) + ' AS NOTIFICA FROM ' ;
						+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + eventMasterTableName;
						+ ' B ON A.mes+A.año+A.cod_pre+A.cod_sub+A.cod_eve+A.num_con = ';
						+ ' B.mes+B.año+B.cod_pre+B.cod_sub+B.cod_eve+B.num_con ';
						+ ' WHERE B.cod_eve IS NULL INTO TABLE tmpEventos_a_importar.dbf'  
				
				OTHERWISE
					setNewRecsClause = 'SELECT A.*, ' + ALLTRIM(STR(sNotificationWeek)) + ' AS NOTIFICA FROM ' ;
									+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + eventMasterTableName;
									+ ' B ON A.año+A.semana+A.cod_eve+A.tip_ide+A.num_ide+A.cod_pre+A.cod_sub'
					IF isField(candidatesRSName,'AJUSTE') THEN 
						setNewRecsClause = setNewRecsClause + '+A.ajuste ' 
					ENDIF 
					setNewRecsClause = setNewRecsClause + ' = B.año+B.semana+B.cod_eve+B.tip_ide+B.num_ide+B.cod_pre+B.cod_sub'
					IF isField(candidatesRSName,'AJUSTE') THEN 
						setNewRecsClause = setNewRecsClause + '+B.ajuste ' 
					ENDIF
					setNewRecsClause = setNewRecsClause + ' WHERE B.cod_eve IS NULL' +;
										' INTO TABLE tmpEventos_a_importar.dbf'  
			ENDCASE

			&setNewRecsClause
			SELECT (eventMasterTableName)
			TRY
				APPEND FROM tmpEventos_a_importar.dbf
			CATCH TO oException
				sResult=.F.
				sErrorMsg = sErrorMsg + oException.Message
			ENDTRY
			
			IF sResult THEN
				CON_DAT = CON_DAT + CountTableRecords("tmpEventos_a_importar")		&&CON_DAT = CON_DAT + _TALLY
				* Incrementa el contador con la cantidad de registros agregados por el último Append
				
				* Marca los registros recien subidos a la tabla maestra de tal manera que no se tengan en cuenta ;
				en procesos posteriores
				DO CASE
					CASE EventSuffix = '23' OR EventSuffix = '43' OR EventSuffix = '91' OR EventSuffix = '94'
						&& Enfermedades transmitidas por alimentos (Colectiva), Hepatitis A, Fiebre Tifoide
						sKeyToMakeUpdates="COD_EVE, AÑO, SEMANA, COD_MUN, NUM_CON, COD_PRE, COD_SUB"

					CASE EventSuffix = '31'
						&& Vigilancia entomológica de la malaria
						sKeyToMakeUpdates="AÑO, SEMANA, COD_PRE, COD_SUB, COD_EVE, COD_DEP, COD_MUN, ESPECIE_VE, AJUSTE"

					CASE EventSuffix = '32'
						&& Informe mensual de actividades para la prevención y control de la rabia
						sKeyToMakeUpdates="MES, AÑO, COD_PRE, COD_SUB, COD_EVE, COD_DEP, COD_MUN"
						
					CASE EventSuffix = '80' OR EventSuffix = '81' OR EventSuffix = '88' OR EventSuffix = '90'
						&& IAD - Infecciones asociadas a dispositivos
						&& CAB - Infecciones asociadas consumo de antibioticos
						&& Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico - Colectivo
						sKeyToMakeUpdates="MES, AÑO, COD_PRE, COD_SUB, COD_EVE, NUM_CON"

					OTHERWISE
						sKeyToMakeUpdates="AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB"
				ENDCASE
				=UpdateFieldsTable(sourceFileName , sourceFileName + ".SUBE=1", "tmpEventos_a_importar", sKeyToMakeUpdates)
				=TABLEUPDATE(1,.T.,sourceFileName)
				
				* Intenta agregar los registros restantes
				DO CASE
					CASE EventSuffix = '23' OR EventSuffix = '43' OR EventSuffix = '91' OR EventSuffix = '94'
						&& Enfermedades transmitidas por alimentos (Colectiva), Hepatitis A, Fiebre Tifoide
						tagForSeek = 'SEM'
					CASE EventSuffix = '31'
						&& Vigilancia entomológica de la malaria
						tagForSeek = 'IMPORTIDX'
					CASE EventSuffix = '32' OR EventSuffix = '80' OR EventSuffix = '81' OR EventSuffix = '88' OR EventSuffix = '90'
						&& Prevención y control de la rabia (Informe mensual de actividades para la) (Cod. 680) o 
						&& IAD - Infecciones asociadas a dispositivos (Cod. 353 o Cod. 359) o
						&& CAB - Infecciones asociadas consumo de antibioticos (Cod. 354)
						&& Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico - Colectivo
						tagForSeek = 'EVE'
				ENDCASE

				SELECT (eventMasterTableName)	
				SET ORDER TO TAG &TagForSeek

				setCandidatesRecordsClause='SELECT * FROM ' + sourceFileName + ' WHERE SUBE=0 INTO CURSOR remainingRecords NOFILTER'
				&setCandidatesRecordsClause
				SELECT remainingRecords  
				DIMENSION aSourceTablefields(1)
				nSourceTablefields=AFIELDS(aSourceTablefields)

				SCAN WHILE sResult
					SCATTER MEMVAR
					m.NOTIFICA = sNotificationWeek
					recordNumber=RECNO()
					sourceUPGD_Pre=COD_PRE
					sourceUPGD_Sub=COD_SUB
					sourceEventCode=COD_EVE
					sourceIdentType=TIP_IDE
					sourceIdentNumber=NUM_IDE
					sourceAdjustment=AJUSTE 
					sourceYear=AÑO
					sourceEventWeek=SEMANA
					
					*Fija la clave de búsqueda de registros en la tabla maestra de Eventos_xx dependiendo del ;
					tipo de evento
					DO CASE
						CASE EventSuffix = '23' OR EventSuffix = '43' OR EventSuffix = '91' OR EventSuffix = '94'
							&& Enfermedades transmitidas por alimentos (Colectiva), Hepatitis A, Fiebre Tifoide
							tagForSeek = 'SEM'
							sourceTownCode=COD_MUN
							sourceIdentNumber=NUM_CON
							sourceEventKey=sourceEventCode  + sourceYear + ALLTRIM(sourceEventWeek) + ALLTRIM(sourceTownCode) + ;
											ALLTRIM(sourceIdentNumber) + ALLTRIM(sourceUPGD_Pre) + ALLTRIM(sourceUPGD_Sub)
						CASE EventSuffix = '31'
							&& Vigilancia entomológica de la malaria
							tagForSeek = 'IMPORTIDX'
							sourceStateCode=COD_DEP
							sourceTownCode=COD_MUN
							sourceSpecie=ESPECIE_VE
							sourceEventKey=sourceYear + sourceEventWeek + sourceUPGD_Pre + sourceUPGD_Sub + ;
											sourceEventCode + sourceStateCode + sourceTownCode + ;
											sourceSpecie + sourceAdjustment
						CASE EventSuffix = '32'
							&& Informe mensual de actividades para la prevención y control de la rabia
							sourceMonth=MES
							sourceStateCode=COD_DEP
							sourceTownCode=COD_MUN
							sourceEventKey=sourceMonth + sourceYear + sourceUPGD_Pre + sourceUPGD_Sub +;
											sourceEventCode + sourceStateCode + sourceTownCode
						CASE EventSuffix = '80' OR EventSuffix = '81' OR EventSuffix = '88' OR EventSuffix = '90'
							&& IAD - Infecciones asociadas a dispositivos
							&& CAB - Infecciones asociadas consumo de antibioticos
							&& Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico - Colectivo
							sourceMonth=MES
							sourceIdentNumber=NUM_CON
							sourceEventKey=sourceMonth + sourceYear + sourceUPGD_Pre + sourceUPGD_Sub +;
											sourceEventCode + sourceIdentNumber
						OTHERWISE
							sourceEventKey=	sourceYear+sourceEventWeek+sourceEventCode+ ;
											sourceIdentType+sourceIdentNumber+sourceUPGD_Pre+sourceUPGD_Sub
					ENDCASE
 
					SELECT (eventMasterTableName)
					SEEK sourceEventKey
					IF FOUND() THEN
						IF isField('remainingRecords','FEC_AJU') THEN
							IF remainingRecords.FEC_AJU  >= &eventMasterTableName..FEC_AJU THEN
								*El registro candidato posiblemente tenga datos más frescos que el registro existente en la tabla maestra
								IF detectChange(@aSourceTablefields) THEN
									TRY
								    	GATHER MEMVAR
								    	CON_DAT = CON_DAT + 1
									CATCH TO oException
										sResult=.F.
										sErrorMsg = sErrorMsg + oException.Message
									ENDTRY
							    ENDIF
							ENDIF
						ENDIF
					ELSE
						TRY
							INSERT INTO (eventMasterTableName) FROM MEMVAR
							CON_DAT = CON_DAT + 1
						CATCH TO oException
							sResult=.F.
							sErrorMsg = sErrorMsg + oException.Message
						ENDTRY
					ENDIF
			    	SELECT RemainingRecords
				ENDSCAN
				
				IF sResult THEN
					*Agrega los registros duplicados
					SELECT (eventMasterTableName )
					nLastRecNumber = RECCOUNT()
					TRY
						APPEND FROM (duplicatesRSName)
						IF _TALLY > 0 THEN
							*Asigna el valor del campo NOTIFICA a los registros recien agregados. El procesamiento es secuencial;
							por cuanto debe evitarse un problema con el bloqueo de registros
							sOldTag = TAG()
							SET ORDER TO
							GO (nLastRecNumber + 1)
							DO WHILE !EOF()
								sCmd = 'REPLACE NOTIFICA WITH ' + ALLTRIM(STR(sNotificationWeek)) 
								&sCmd
								SKIP 
							ENDDO
							SET ORDER TO TAG &sOldTag 
						ENDIF
					CATCH TO oException
						sResult=.F.
						sErrorMsg = sErrorMsg + oException.Message
					ENDTRY
					
					IF sResult THEN
						CON_DAT = CON_DAT + CountTableRecords(duplicatesRSName)		&&CON_DAT = CON_DAT + _TALLY
						* Incrementa el contador con la cantidad de registros agregados por el último Append
						
						*Exporta los registros duplicados hacia un archivo excel para posterior corrección
						USE (duplicatesRSName) IN 0
						SELECT (duplicatesRSName)
						IF RECCOUNT(duplicatesRSName) > 0 THEN
							duplicatesExcelFileName=duplicatesFilePath + '\' + sourceFileName + '_DUPLICADOS'
							COPY TO [&duplicatesExcelFileName] XL5
						ENDIF
					ENDIF
				ENDIF
			ENDIF

			*Borra las tabla temporales utilizadas durante el procesamiento
			DO DeleteTable WITH 'tmpEventos_a_importar'
					 		
		ENDIF
	ENDIF

	*CLOSE TABLES
	DO CloseTables WITH "Eventos,&sourceFileName,&existentsFileName"
	DO DeleteTable WITH candidatesRSName
	DO DeleteTable WITH candidatesRSName + "_aux"
	DO DeleteTable WITH duplicatesRSName
	DO DeleteTable WITH duplicatesRSName + "_aux"
ENDPROC

*---------------------------------------------------------------------------------------------------------
PROCEDURE addUPGD
	LPARAMETERS candidatesRSName, duplicatesRSName, sourceFilePath, sourceFileName, ;
				existentsFilePath, existentsFileName, duplicatesFilePath, sResult as Boolean, ;
				sErrorMsg as String

	* candidatesRSName: nombre de una tabla o cursor que contiene los registros que son candidatos a ;
		ser almacenados en la tabla maestra UPGD. Un registro es candidato si ;
		ha superado las pruebas de validación
						
	* duplicatesRSName: nombre de una tabla o cursor que contiene los registros duplicados a ;
		ser almacenados en la tabla maestra de Pacientes. Un registrto es duplicados si ;
		su clave de identificación es duplicada
						
	* sourceFilePath + sourceFileName: nombre y ubicación de la tabla fuente que contiene todos los ;
		registros (candidatos y no candidatos) que deberían ser almacenados en la tabla maestra ;
		UPGD					

	* existentsFilePath, existentsFileName: nombre y ubicación de la tabla en donde deben guardarse ;
		los registros que ya existen en la tabla maestra y que no pudieron ser grabados en ella

	* duplicatesFilePath: ruta de la carpeta en donde deben guardarse los registros de duplicatesRSName
	

	* Configura las areas de trabajo con las tablas necesarias para llevar a cabo la operación
	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	masterTableName='UPGD'
	bContinue = UseTable(sourceFilePath + '\' + sourceFileName) AND UseTable(existentsFilePath + '\' + existentsFileName) ;
				AND UseTable(MasterTableName)

	IF bContinue AND (RECCOUNT(sourceFileName) >= 1)
							
		CON_UPGD=0
		sResult = .T.
*SET STEP ON 		
		* Agrega a la tabla maestra UPGD todos los registros candidatos nuevos. ;
		Un registro candidato es nuevo si su clave {COD_PRE, COD_SUB} no se encuentra en la tabla maestra. 
		setNewRecsClause='SELECT ' + (candidatesRSName) + '.*, 0 AS NOTIFICA FROM ' ;
			+ (candidatesRSName) + ' LEFT OUTER JOIN ' + MasterTableName;
			+ ' ON ( ( ' + (candidatesRSName)+'.COD_PRE = ' + MasterTableName + '.COD_PRE)';
			+ ' AND ' + (candidatesRSName)+'.COD_SUB = ' + MasterTableName + '.COD_SUB)';
			+ ' WHERE ' + MasterTableName +'.COD_PRE IS NULL INTO TABLE tmpUPGDs_a_importar.dbf'  
		&setNewRecsClause
		SELECT (MasterTableName)
		SET ORDER TO TAG cod1
		TRY
			APPEND FROM tmpUPGDs_a_importar.dbf
		CATCH TO oException
			sResult=.F.
			sErrorMsg = sErrorMsg + oException.Message
		ENDTRY
		
		IF sResult THEN
			CON_UPGD = CON_UPGD + CountTableRecords("tmpUPGDs_a_importar")		&&CON_UPGD = CON_UPGD + _TALLY

			* Marca los registros recien subidos a la tabla maestra de tal manera que no se tengan en cuenta ;
			en procesos posteriores
			markImportedRecordsClause='UPDATE ' + sourceFileName + ' SET ' + sourceFileName + '.sube = 1 ' ;
				+ ' FROM tmpUPGDs_a_importar WHERE';
				+ ' (' + sourceFileName +'.COD_PRE = tmpUPGDs_a_importar .COD_PRE ) AND ';
				+ ' (' + sourceFileName +'.COD_SUB = tmpUPGDs_a_importar .COD_SUB ) '
			&markImportedRecordsClause	
			=TABLEUPDATE(1,.T.,sourceFileName)
			
			* Intenta agregar los registros restantes siempre y cuando cumplan ciertas condiciones
			setCandidatesRecordsClause='SELECT * FROM ' + sourceFileName + ' WHERE SUBE=0 ' ;
				+ ' INTO CURSOR remainingRecords NOFILTER'
			&setCandidatesRecordsClause
			SELECT remainingRecords  
			DIMENSION aSourceTablefields(1)
			nSourceTablefields=AFIELDS(aSourceTablefields)
			
			SCAN WHILE sResult
				SCATTER MEMVAR
				m.NOTIFICA=NOTIFICAR
				recordNumber=RECNO()
				sourceUPGD_Pre=COD_PRE
				sourceUPGD_Sub=COD_SUB
				sourceUPGDDefinitionDate=FEC_CAR
				
				*Fija la clave de búsqueda de registros en la tabla maestra UPGD
				sourceEventKey=	sourceUPGD_Pre + sourceUPGD_Sub

				SELECT (MasterTableName)
				SEEK sourceEventKey
				IF sourceUPGDDefinitionDate >= FEC_CAR THEN
					*La UPGD de la tabla Maestra ha sido objeto de recaracterización por parte de la Unidad que reporta;
					y sus datos deben ser actualizados.
					*Comentado por:	Wilson Aguilar	Fecha: 18/05/2019;
					La parte IF del código siguiente se eliminó con motivo de la visita técnica efectuada a la Secretaría de salud Bogotá el día 17/05/2019
					&&IF detectChange(@aSourceTablefields) THEN
					TRY
						GATHER MEMVAR
						CON_UPGD = CON_UPGD + 1	
					CATCH TO oException
						sResult=.F.
						sErrorMsg = sErrorMsg + oException.Message
					ENDTRY
					&&ENDIF
				ENDIF
				SELECT RemainingRecords
			ENDSCAN

			IF sResult THEN
				*Agrega los registros duplicados
				SELECT (masterTableName)
				TRY
					APPEND FROM (duplicatesRSName)
					&&CON_UPGD = CON_UPGD + _TALLY
				CATCH TO oException
					sResult=.F.
					sErrorMsg = sErrorMsg + oException.Message
				ENDTRY
				IF sResult THEN
					CON_UPGD = CON_UPGD + CountTableRecords(duplicatesRSName)		&&CON_UPGD = CON_UPGD + _TALLY
					* Incrementa el contador con la cantidad de registros agregados por el último Append
					
					*Exporta los registros duplicados hacia un archivo excel para posterior corrección
					USE (duplicatesRSName) IN 0
					SELECT (duplicatesRSName)
					IF RECCOUNT(duplicatesRSName) > 0 THEN
						duplicatesExcelFileName=duplicatesFilePath + '\' + sourceFileName + '_DUPLICADOS'
						COPY TO [&duplicatesExcelFileName] XL5
					ENDIF
				ENDIF
			ENDIF
		ENDIF

		*Borra las tabla temporales utilizadas durante el procesamiento
		DO DeleteTable WITH 'tmpUPGDs_a_importar'

	ENDIF

	*CLOSE TABLES
	DO CloseTables WITH "Eventos,&sourceFileName,&existentsFileName"
	DO DeleteTable WITH 'T_NonDups_UPGD'
	DO DeleteTable WITH 'C_DUPS_UPGD'
ENDPROC
*-----------------------------------------------------------------------------------------------------------

PROCEDURE addHumanResources
	LPARAMETERS candidatesRSName, duplicatesRSName, sourceFilePath, sourceFileName, ;
				existentsFilePath, existentsFileName, duplicatesFilePath, sResult as Boolean, ;
				sErrorMsg as String

	* candidatesRSName: nombre de una tabla o cursor que contiene los registros que son candidatos a ;
		ser almacenados en la tabla maestra TAL_HUM. Un registro es candidato si ;
		ha superado las pruebas de validación
						
	* duplicatesRSName: nombre de una tabla o cursor que contiene los registros duplicados a ;
		ser almacenados en la tabla maestra de Pacientes. Un registrto es duplicados si ;
		su clave de identificación es duplicada
						
	* sourceFilePath + sourceFileName: nombre y ubicación de la tabla fuente que contiene todos los ;
		registros (candidatos y no candidatos) que deberían ser almacenados en la tabla maestra ;
		TAL_HUM					

	* existentsFilePath, existentsFileName: nombre y ubicación de la tabla en donde deben guardarse ;
		los registros que ya existen en la tabla maestra y que no pudieron ser grabados en ella

	* duplicatesFilePath: ruta de la carpeta en donde deben guardarse los registros de duplicatesRSName


	* Si ningún registro fué leído, regresar
	IF TOT_TH >= 1

		SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
								
		* Configura las areas de trabajo con las tablas necesarias para llevar a cabo la operación
		masterTableName='TAL_HUM'
		IF !USED(MasterTableName) THEN
			USE (MasterTableName) IN 0
		ENDIF
		IF !USED(sourceFileName) THEN
			USE [&sourceFilePath\&sourceFileName] IN 0
		ENDIF
		IF !USED(existentsFileName) THEN
			USE [&existentsFilePath\&existentsFileName] IN 0
		ENDIF
		
		CON_TH=0
		sResult = .T.

		* Agrega a la tabla maestra TAL_HUM todos los registros candidatos nuevos. ;
		Un registro candidato es nuevo si su clave {cod_pre, cod_sub, cod_art} no se encuentra en la tabla maestra. 
		setNewRecsClause='SELECT A.*, 0 AS NOTIFICA FROM ' ;
			+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + MasterTableName;
			+ ' B ON ( A.COD_PRE = B.COD_PRE ';
			+ ' AND A.COD_SUB = B.COD_SUB ';
			+ ' AND A.COD_ART = B.COD_ART )';
			+ ' WHERE ISNULL(B.COD_PRE) INTO TABLE tmpTAL_HUM_a_importar.dbf'  
		&setNewRecsClause
		SELECT tmpTAL_HUM_a_importar
		INDEX ON COD_PRE+COD_SUB+COD_ART TAG LINKER_IDX
		
		SELECT (MasterTableName)
		SET ORDER TO TAG COD2
		TRY
			APPEND FROM tmpTAL_HUM_a_importar.dbf
		CATCH TO oException
			sResult=.F.
			sErrorMsg = sErrorMsg + oException.Message
		ENDTRY

		IF sResult THEN
			CON_TH = CON_TH + CountTableRecords("tmpTAL_HUM_a_importar")		&&CON_TH = CON_TH + _TALLY
			* Incrementa el contador con la cantidad de registros agregados por el último Append

			* Marca los registros recien subidos a la tabla maestra de tal manera que no se tengan en cuenta ;
			en procesos posteriores
			*markImportedRecordsClause='UPDATE ' + sourceFileName + ' SET ' + sourceFileName + '.sube = 1 ' ;
				+ ' FROM tmpTAL_HUM_a_importar WHERE';
				+ ' (' + sourceFileName +'.COD_PRE = tmpTAL_HUM_a_importar.COD_PRE ) AND ';
				+ ' (' + sourceFileName +'.COD_SUB = tmpTAL_HUM_a_importar.COD_SUB )  AND ' ;
				+ ' (' + sourceFileName +'.COD_ART = tmpTAL_HUM_a_importar.COD_ART ) '
			*&markImportedRecordsClause	
			SELECT (sourceFileName)
			SET RELATION TO COD_PRE+COD_SUB+COD_ART INTO Tmptal_hum_a_importar ADDITIVE
			REPLACE sube WITH 1 FOR COD_PRE=tmpTAL_HUM_a_importar.COD_PRE AND ;
				COD_SUB=tmpTAL_HUM_a_importar.COD_SUB AND COD_ART=tmpTAL_HUM_a_importar.COD_ART ALL
			
			=TABLEUPDATE(1,.T.,sourceFileName)
						
			* Intenta agregar los registros restantes siempre y cuando cumplan ciertas condiciones
			setCandidatesRecordsClause='SELECT * FROM ' + sourceFileName + ' WHERE SUBE=0 ' ;
				+ ' INTO CURSOR remainingRecords NOFILTER'
			&setCandidatesRecordsClause
			SELECT remainingRecords  
			DIMENSION aSourceTablefields(1)
			nSourceTablefields=AFIELDS(aSourceTablefields)

			SCAN WHILE sResult
				SCATTER MEMVAR
				m.NOTIFICA=NOTIFICAR
				recordNumber=RECNO()
				sourceUPGD_Pre=COD_PRE
				sourceUPGD_Sub=COD_SUB
				sourceAsset=COD_ART 
				
				*Fija la clave de búsqueda de registros en la tabla maestra TAL_HUM 
				sourceEventKey=	sourceUPGD_Pre + sourceUPGD_Sub + sourceAsset

				SELECT (MasterTableName)
				SEEK sourceEventKey
				IF FOUND() THEN				
					IF detectChange(@aSourceTablefields) THEN
						TRY
							GATHER MEMVAR
							CON_TH = CON_TH + 1
						CATCH TO oException
							sResult=.F.
							sErrorMsg = sErrorMsg + oException.Message
						ENDTRY
					ENDIF
				ELSE
					TRY
						INSERT INTO (MasterTableName) FROM MEMVAR
						CON_TH = CON_TH + 1
					CATCH TO oException
						sResult=.F.
						sErrorMsg = sErrorMsg + oException.Message
					ENDTRY
				ENDIF
				SELECT RemainingRecords
			ENDSCAN

			IF sResult THEN
			
				*Agrega los registros duplicados
				SELECT (masterTableName)
				TRY
					APPEND FROM (duplicatesRSName)
					&&CON_TH = CON_TH + _TALLY
				CATCH TO oException
					sResult=.F.
					sErrorMsg = sErrorMsg + oException.Message
				ENDTRY
				IF sResult THEN
					CON_TH = CON_TH + CountTableRecords(duplicatesRSName)		&&CON_TH = CON_TH + _TALLY
					* Incrementa el contador con la cantidad de registros agregados por el último Append
					
					*Exporta los registros duplicados hacia un archivo excel para posterior corrección
					USE (duplicatesRSName) IN 0
					SELECT (duplicatesRSName)
					IF RECCOUNT(duplicatesRSName) > 0 THEN
						duplicatesExcelFileName=duplicatesFilePath + '\' + sourceFileName + '_DUPLICADOS'
						COPY TO [&duplicatesExcelFileName] XL5
					ENDIF
				ENDIF
			ENDIF
		ENDIF
		
		*Borra las tabla temporales utilizadas durante el procesamiento
		DO DeleteTable WITH 'tmpTAL_HUM_a_importar'
		
	ENDIF

	*CLOSE TABLES
	DO CloseTables WITH "Eventos,&sourceFileName,&existentsFileName"
	DO DeleteTable WITH 'T_NonDups_RHS'
	DO DeleteTable WITH 'C_DUPS_RHS'
ENDPROC

*------------------------------------------------------------------
PROCEDURE addUCI
	LPARAMETERS candidatesRSName, duplicatesRSName, sourceFilePath, sourceFileName, ;
				existentsFilePath, existentsFileName, duplicatesFilePath, sResult as Boolean, ;
				sErrorMsg as String

	* candidatesRSName: nombre de una tabla o cursor que contiene los registros que son candidatos a ;
		ser almacenados en la tabla maestra UPGD_UCIS. Un registro es candidato si ;
		ha superado las pruebas de validación
						
	* duplicatesRSName: nombre de una tabla o cursor que contiene los registros duplicados a ;
		ser almacenados en la tabla maestra de Pacientes. Un registrto es duplicados si ;
		su clave de identificación es duplicada
						
	* sourceFilePath + sourceFileName: nombre y ubicación de la tabla fuente que contiene todos los ;
		registros (candidatos y no candidatos) que deberían ser almacenados en la tabla maestra UPGD_UCIS					

	* existentsFilePath, existentsFileName: nombre y ubicación de la tabla en donde deben guardarse ;
		los registros que ya existen en la tabla maestra y que no pudieron ser grabados en ella

	* duplicatesFilePath: ruta de la carpeta en donde deben guardarse los registros de duplicatesRSName


	* Si ningún registro fué leído, regresar
	IF total_uci_cargadas >= 1

		SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
								
		* Configura las areas de trabajo con las tablas necesarias para llevar a cabo la operación
		masterTableName='UPGD_UCIS'
		IF !USED(MasterTableName) THEN
			USE (MasterTableName) IN 0
		ENDIF
		IF !USED(sourceFileName) THEN
			USE [&sourceFilePath\&sourceFileName] IN 0
		ENDIF
		IF !USED(existentsFileName) THEN
			USE [&existentsFilePath\&existentsFileName] IN 0
		ENDIF
		
		CON_UCI = 0
		sResult = .T.

		* Agrega a la tabla maestra UPGD_UCIS todos los registros candidatos nuevos. ;
		Un registro candidato es nuevo si su clave {cod_pre, cod_sub, id_uci} no se encuentra en la tabla maestra. 
		setNewRecsClause='SELECT A.*, 0 AS NOTIFICA FROM ' ;
			+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + MasterTableName;
			+ ' B ON ( A.COD_PRE = B.COD_PRE ';
			+ ' AND A.COD_SUB = B.COD_SUB ';
			+ ' AND A.ID_UCI = B.ID_UCI )';
			+ ' WHERE ISNULL(B.COD_PRE) INTO TABLE tmpUPGD_UCIS_a_importar.dbf'  
		&setNewRecsClause
		SELECT tmpUPGD_UCIS_a_importar
		INDEX ON COD_PRE+COD_SUB+STR(ID_UCI) TAG LINKER_IDX
		
		SELECT (MasterTableName)
		TRY
			APPEND FROM tmpUPGD_UCIS_a_importar.dbf
		CATCH TO oException
			sResult=.F.
			sErrorMsg = sErrorMsg + oException.Message
		ENDTRY

		IF sResult THEN
			CON_UCI = CON_UCI + CountTableRecords("tmpUPGD_UCIS_a_importar")	
			* Incrementa el contador con la cantidad de registros agregados por el último Append
			
			sSQLCmd = "UPDATE " + sourceFileName + " SET SUBE = 1 FROM " + sourceFileName + " INNER JOIN TmpUPGD_UCIS_a_importar ON " +;
				sourceFileName+ ".COD_PRE + " + sourceFileName + ".COD_SUB + STR(" + sourceFileName +".ID_UCI) = " +;
				"TmpUPGD_UCIS_a_importar.COD_PRE + TmpUPGD_UCIS_a_importar.COD_SUB + STR(TmpUPGD_UCIS_a_importar.ID_UCI)"
			&sSQLCmd 
			=TABLEUPDATE(1,.T.,sourceFileName)
						
			* Intenta agregar los registros restantes siempre y cuando cumplan ciertas condiciones
			SELECT (MasterTableName)
			SET ORDER TO TAG PK
			
			setCandidatesRecordsClause='SELECT * FROM ' + sourceFileName + ' WHERE SUBE=0 INTO CURSOR remainingRecords NOFILTER'
			&setCandidatesRecordsClause
			SELECT remainingRecords  
			DIMENSION aSourceTablefields(1)
			nSourceTablefields=AFIELDS(aSourceTablefields)

			SCAN WHILE sResult
				SCATTER MEMVAR
				m.NOTIFICA=NOTIFICAR
				recordNumber=RECNO()
				sourceUPGD_Pre=COD_PRE
				sourceUPGD_Sub=COD_SUB
				sourceID_UCI=ID_UCI
				
				*Fija la clave de búsqueda de registros en la tabla maestra UPGD_UCIS 
				sourceEventKey=	sourceUPGD_Pre + sourceUPGD_Sub + STR(sourceID_UCI)

				SELECT (MasterTableName)
				SEEK sourceEventKey 
				IF FOUND() THEN	
					IF 	m.FEC_AJU >= &MasterTableName..FEC_AJU THEN
						*Los datos del registro que se está cargando son más recientes que los del registro existente en la tabla maestra
						IF detectChange(@aSourceTablefields) THEN
							TRY
								GATHER MEMVAR
								CON_UCI = CON_UCI + 1
							CATCH TO oException
								sResult=.F.
								sErrorMsg = sErrorMsg + oException.Message
							ENDTRY
						ENDIF
					ENDIF
				ELSE
					TRY
						INSERT INTO (MasterTableName) FROM MEMVAR
						CON_UCI = CON_UCI + 1
					CATCH TO oException
						sResult=.F.
						sErrorMsg = sErrorMsg + oException.Message
					ENDTRY
				ENDIF
				SELECT RemainingRecords
			ENDSCAN

			IF sResult THEN
				*Cometario por:	Wilson Aguilar	Fecha: 11/01/2017;
				La siguiente parte de código se modificó de tal manera que los registros duplicados no se tengan en cuenta para cargar hacia la tabla ;
				maestra por cuanto ello genera un error de violación de unicidad del índice de la tabla maestra que, además impide que se carguen ;
				todos los datos de notificación del paquete completo
				
				*Agrega los registros duplicados
				SELECT (masterTableName)
				TRY
					*APPEND FROM (duplicatesRSName)
				CATCH TO oException
					sResult=.F.
					sErrorMsg = sErrorMsg + oException.Message
				ENDTRY
				IF sResult THEN
					CON_UCI = CON_UCI + CountTableRecords(duplicatesRSName)
					* Incrementa el contador con la cantidad de registros agregados por el último Append
					
					*Exporta los registros duplicados hacia un archivo excel para posterior corrección
					USE (duplicatesRSName) IN 0
					SELECT (duplicatesRSName)
					IF RECCOUNT(duplicatesRSName) > 0 THEN
						duplicatesExcelFileName=duplicatesFilePath + '\' + sourceFileName + '_DUPLICADOS'
						COPY TO [&duplicatesExcelFileName] XL5
					ENDIF
				ENDIF
			ENDIF
		ENDIF
		
		*Borra las tabla temporales utilizadas durante el procesamiento
		DO DeleteTable WITH 'tmpUPGD_UCIS_a_importar'
		
	ENDIF

	*CLOSE TABLES
	DO CloseTables WITH "Eventos,&sourceFileName,&existentsFileName"
	DO DeleteTable WITH 'T_NonDups_UCI'
	DO DeleteTable WITH 'C_DUPS_UCI'
ENDPROC

*------------------------------------------------------------------
PROCEDURE addCollectiveEvent
	LPARAMETERS candidatesRSName, duplicatesRSName, sourceFilePath, sourceFileName, ;
				existentsFilePath, existentsFileName, duplicatesFilePath, sResult as Boolean, ;
				sErrorMsg as String, sNotificationWeek as Number

	* CollectiveEvent -> (BROTES) 
	* candidatesRSName: nombre de una tabla o cursor que contiene los registros que son candidatos a ;
		ser almacenados en la tabla maestra BROTES. Un registro es candidato si ;
		ha superado las pruebas de validación

	* duplicatesRSName: nombre de una tabla o cursor que contiene los registros duplicados a ;
		ser almacenados en la tabla maestra de Pacientes. Un registrto es duplicados si ;
		su clave de identificación es duplicada
						
	* sourceFilePath + sourceFileName: nombre y ubicación de la tabla fuente que contiene todos los ;
		registros (candidatos y no candidatos) que deberían ser almacenados en la tabla maestra ;
		BROTES

	* existentsFilePath, existentsFileName: nombre y ubicación de la tabla en donde deben guardarse ;
		los registros que ya existen en la tabla maestra y que no pudieron ser grabados en ella

	* duplicatesFilePath: ruta de la carpeta en donde deben guardarse los registros de duplicatesRSName

	* sNotificationWeek (opcional): valor que debe asignarse a la semana de notificación (Tables.Eventos_xx.NOTIFICA);
	para los registros nuevos que se adicionen a la tabla Eventos_xx. El valor por defecto de este parámetro es 0.

	IF TOT_BRO >= 1

		IF VARTYPE(sNotificationWeek)='L' THEN
			sNotificationWeek = 0
		ENDIF
		
		SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
							
		* Configura las areas de trabajo con las tablas necesarias para llevar a cabo la operación
		masterTableName='BROTES'
		IF !USED(MasterTableName) THEN
			USE (MasterTableName) IN 0 
		ENDIF
		IF !USED(sourceFileName) THEN
			USE [&sourceFilePath\&sourceFileName] IN 0
		ENDIF
		IF !USED(existentsFileName) THEN
			USE [&existentsFilePath\&existentsFileName] IN 0
		ENDIF
		IF !USED('EVENTOS')
			USE EVENTOS IN 0 ORDER COD
		ENDIF
		SELECT (masterTableName)
		SET RELATION TO cod_eve INTO eventos	

		CON_BRO=0		&& Contador de registros
		sResult = .T.
		
		* Agrega a la tabla maestra BROTES todos los registros candidatos nuevos. ;
		Un registro candidato es nuevo si su clave ;
		{COD_EVE,AÑO,SEMANA,ALLTRIM(COD_MUN),ALLTRIM(COD_PRE), ALLTRIM(COD_SUB)} no se encuentra en la tabla maestra. ;
		[candidatesRSName -> MasterTableName ]
		
		setNewRecsClause='SELECT A.*, ' + ALLTRIM(STR(sNotificationWeek)) + ' AS NOTIFICA FROM ' ;
			+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + MasterTableName + ' B ON ( ';
			+ 'A.COD_EVE = B.COD_EVE AND ';
			+ 'A.AÑO     = B.AÑO     AND ';
			+ 'A.SEMANA  = B.SEMANA  AND ';
			+ 'A.COD_MUN = B.COD_MUN AND ';		
			+ 'A.NUM_CON = B.NUM_CON AND ';
			+ 'A.COD_PRE = B.COD_PRE AND ';
			+ 'A.COD_SUB = B.COD_SUB ) ';		
			+ 'WHERE ISNULL(B.COD_EVE) INTO TABLE tmpBROTES_a_importar.dbf'  
			
		&setNewRecsClause
		SELECT (MasterTableName)

		TRY
			APPEND FROM tmpBROTES_a_importar.dbf
		CATCH TO oException
			sResult=.F.
			sErrorMsg = sErrorMsg + oException.Message
		ENDTRY
		
		IF sResult THEN
			SET ORDER TO TAG SEM
			
			CON_BRO = CON_BRO + CountTableRecords("tmpBROTES_a_importar")	&&CON_BRO = CON_BRO + _TALLY

			* Marca los registros recien subidos a la tabla maestra de tal manera que no se tengan en cuenta ;
			en procesos posteriores;
			[sourceFileName]->[tmpBROTES_a_importar]
			
			markImportedRecordsClause='UPDATE ' + sourceFileName + ' SET ' + sourceFileName + '.sube = 1 ' ;
				+ ' FROM tmpBROTES_a_importar WHERE (';
				+ ( sourceFileName ) + '.COD_EVE  = tmpBROTES_a_importar.COD_EVE';
				+ ' AND ' + ( sourceFileName ) + '.AÑO = tmpBROTES_a_importar.AÑO';
				+ ' AND ' + ( sourceFileName ) + '.SEMANA  = tmpBROTES_a_importar.SEMANA';
				+ ' AND ' + ( sourceFileName ) + '.COD_MUN = tmpBROTES_a_importar.COD_MUN';		
				+ ' AND ' + ( sourceFileName ) + '.NUM_CON = tmpBROTES_a_importar.NUM_CON';
				+ ' AND ' + ( sourceFileName ) + '.COD_PRE = tmpBROTES_a_importar.COD_PRE';
				+ ' AND ' + ( sourceFileName ) + '.COD_SUB = tmpBROTES_a_importar.COD_SUB)'

			&markImportedRecordsClause	
			=TABLEUPDATE(1,.T.,sourceFileName)
			
			* Intenta agregar los registros restantes siempre y cuando cumplan ciertas condiciones
			setCandidatesRecordsClause='SELECT * FROM ' + sourceFileName + ' WHERE SUBE=0 ' ;
				+ ' INTO CURSOR remainingRecords NOFILTER'
			&setCandidatesRecordsClause
			SELECT remainingRecords  
			
			SCAN WHILE sResult
				SCATTER MEMVAR
				m.NOTIFICA = sNotificationWeek
				recordNumber = RECNO()

				sourceUPGD_Pre       = ALLTRIM(COD_PRE)
				sourceUPGD_Sub       = ALLTRIM(COD_SUB)
				sourceEventCode      = COD_EVE
				sourceYear           = AÑO
				sourceEventWeek      = ALLTRIM(SEMANA)
				sourceTownCode       = ALLTRIM(COD_MUN)
				sourceIdentNumber    = ALLTRIM(NUM_CON)
				sourceOriginTownCode = MUN_PRO
				sourceAdjustment     = AJUSTE 

				*Fija la clave de búsqueda de registros en la tabla maestra BROTES
				sourceCollectiveEventKey =	sourceEventCode + sourceYear + sourceEventWeek + sourceTownCode + sourceIdentNumber +;
											sourceUPGD_Pre + sourceUPGD_Sub   
				
				SELECT (MasterTableName)
				SET ORDER TO TAG SEM
				SEEK sourceCollectiveEventKey
				
				AGR_REG=0
	            DO WHILE !EOF() AND COD_EVE+AÑO+ALLTRIM(SEMANA)+ALLTRIM(COD_MUN)+ALLTRIM(NUM_CON)+ALLTRIM(COD_PRE)+ALLTRIM(COD_SUB) = sourceCollectiveEventKey 
	               IF ajuste = sourceAdjustment     
	                  AGR_REG=1
	                  EXIT
	               endif
	               SKIP
	            ENDDO
	            IF AGR_REG=0
					TRY
						INSERT INTO (MasterTableName) FROM MEMVAR
						CON_BRO=CON_BRO+1
					CATCH TO oException
						sResult=.F.
						sErrorMsg = sErrorMsg + oException.Message
					ENDTRY
	            ELSE
	          		ErrorMessage = MasterTableName + ' - UPGD QUE REPORTO: ' + COD_PRE + '-' + COD_SUB + ;
	          						', PROCEDENCIA: ' + sourceOriginTownCode 
				
	               INSERT INTO (existentsFileName) (ARCHIVO, REG, DES_ARC, COD_PRE, COD_SUB, EVENTO, EXISTE_EN, SEMANA, AÑO);
	                  VALUES ('B'+SUBSTR(ARC_MAE,2,7) ,recordNumber , 'Not. Colectiva', sourceUPGD_Pre, sourceUPGD_Sub ,;
	                  			sourceEventCode, ErrorMessage , sourceEventWeek, sourceYear)
	            ENDIF
				SELECT RemainingRecords
			ENDSCAN

			IF sResult THEN
				*Agrega los registros duplicados
				SELECT (masterTableName)
				nLastRecNumber = RECCOUNT()
				TRY
					APPEND FROM (duplicatesRSName)
					IF _TALLY > 0 THEN
						*Asigna el valor del campo NOTIFICA a los registros recien agregados. El procesamiento es secuencial;
						por cuanto debe evitarse un problema con el bloqueo de registros
						sOldTag = TAG()
						SET ORDER TO
						GO (nLastRecNumber + 1)
						DO WHILE !EOF()
							sCmd = 'REPLACE NOTIFICA WITH ' + ALLTRIM(STR(sNotificationWeek)) 
							&sCmd
							SKIP 
						ENDDO
						SET ORDER TO TAG &sOldTag 
					ENDIF
				CATCH TO oException
					sResult=.F.
					sErrorMsg = sErrorMsg + oException.Message
				ENDTRY
				IF sResult THEN
				
					CON_BRO = CON_BRO + CountTableRecords(duplicatesRSName)
					* Incrementa el contador con la cantidad de registros agregados por el último Append
					
					*Exporta los registros duplicados hacia un archivo excel para posterior corrección
					USE (duplicatesRSName) IN 0
					SELECT (duplicatesRSName)
					IF RECCOUNT(duplicatesRSName) > 0 THEN
						duplicatesExcelFileName=duplicatesFilePath + '\' + sourceFileName + '_DUPLICADOS'
						COPY TO [&duplicatesExcelFileName] XL5
					ENDIF
				ENDIF
			ENDIF
		ENDIF
		
		*Borra las tablas temporales utilizadas durante el procesamiento
		DO DeleteTable WITH 'tmpBROTES_a_importar'

		*Actualiza la tabla de registros EXISTENTES
		= TABLEUPDATE(1,.T.,existentsFileName,aImportErrors)
		
	ENDIF	

	DO CloseTables WITH "Eventos,&sourceFileName,&existentsFileName"
	DO DeleteTable WITH 'T_NonDups_Brotes'
	DO DeleteTable WITH 'T_NonOrphans_Brotes'
	DO DeleteTable WITH 'C_DUPS_Brotes'
ENDPROC


*------------------------------------------------------------------
PROCEDURE addLaboratories
	LPARAMETERS candidatesRSName, duplicatesRSName, sourceFilePath, sourceFileName,;
				existentsFilePath, existentsFileName, duplicatesFilePath, sResult as Boolean, ;
				sErrorMsg as String, sNotificationWeek as Number

	* candidatesRSName: nombre de una tabla o cursor que contiene los registros que son candidatos a ;
		ser almacenados en la tabla maestra LABORATORIOS. Un registro es candidato si ;
		ha superado las pruebas de validación
						
	* duplicatesRSName: nombre de una tabla o cursor que contiene los registros duplicados a ;
		ser almacenados en la tabla maestra de Pacientes. Un registrto es duplicados si ;
		su clave de identificación es duplicada
						
	* sourceFilePath + sourceFileName: nombre y ubicación de la tabla fuente que contiene todos los ;
		registros (candidatos y no candidatos) que deberían ser almacenados en la tabla maestra ;
		LABORATORIOS

	* existentsFilePath, existentsFileName: nombre y ubicación de la tabla en donde deben guardarse ;
		los registros que ya existen en la tabla maestra y que no pudieron ser grabados en ella

	* duplicatesFilePath: ruta de la carpeta en donde deben guardarse los registros de duplicatesRSName

	* sNotificationWeek (opcional): valor que debe asignarse a la semana de notificación (Tables.LABORATORIOS.NOTIFICA);
	para los registros nuevos que se adicionen a la tabla LABORATORIOS. El valor por defecto de este parámetro es 0.

	IF TOT_LAB >= 1

		IF VARTYPE(sNotificationWeek)='L' THEN
			sNotificationWeek = 0
		ENDIF

		SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
		
		* Configura las areas de trabajo con las tablas necesarias para llevar a cabo la operación
		masterTableName='LABORATORIOS'
		IF !USED(MasterTableName) THEN
			USE (MasterTableName) IN 0 ORDER TAG AGENTE	
		ENDIF
		IF !USED(sourceFileName) THEN
			USE [&sourceFilePath\&sourceFileName] IN 0
		ENDIF
		IF !USED(existentsFileName) THEN
			USE [&existentsFilePath\&existentsFileName] IN 0
		ENDIF

		CON_LAB=0
		sResult = .T.

		* Agrega a la tabla maestra masterTableName todos los registros candidatos nuevos. ;
		Un registro candidato es nuevo si su clave {AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, AGENTE, PRUEBA)} no ;
		se encuentra en la tabla maestra. 
		setNewRecsClause='SELECT A.*, ' + ALLTRIM(STR(sNotificationWeek)) + ' AS NOTIFICA FROM ' ;
			+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + MasterTableName + ' B ON ' ;
			+ 'A.año+A.semana+A.cod_eve+A.tip_ide+A.num_ide+A.agente+A.prueba = ' ;
			+ 'B.año+B.semana+B.cod_eve+B.tip_ide+B.num_ide+B.agente+B.prueba' ;	
			+ ' WHERE B.COD_EVE IS NULL INTO TABLE tmpLABS_a_importar.dbf' 
		&setNewRecsClause
		
		SELECT (MasterTableName)
		TRY
			APPEND FROM tmpLABS_a_importar.dbf
		CATCH TO oException
			sResult=.F.
			sErrorMsg = sErrorMsg + oException.Message
		ENDTRY
		
		IF sResult THEN
			* Incrementa el contador con la cantidad de registros agregados por el último Append
			CON_LAB = CON_LAB + CountTableRecords("tmpLABS_a_importar")

			* Marca los registros recien subidos a la tabla maestra de tal manera que no se tengan en cuenta en procesos posteriores
			=UpdateFieldsTable(sourceFileName, sourceFileName + '.sube = 1 ', 'tmpLABS_a_importar', 'AÑO,SEMANA,COD_EVE,TIP_IDE,NUM_IDE,AGENTE,PRUEBA')
			=TABLEUPDATE(1,.T.,sourceFileName)
			
			* Intenta agregar los registros restantes siempre y cuando cumplan ciertas condiciones
			SELECT (MasterTableName)
			SET ORDER TO TAG SEARCH3

			setCandidatesRecordsClause='SELECT * FROM ' + sourceFileName + ' WHERE SUBE=0 INTO CURSOR remainingRecords NOFILTER'
			&setCandidatesRecordsClause
			SELECT remainingRecords  
			DIMENSION aSourceTablefields(1)
			nSourceTablefields=AFIELDS(aSourceTablefields)
*SET STEP ON 		
			SCAN WHILE sResult
				SCATTER MEMVAR

				*Fija la clave de búsqueda de registros en la tabla maestra LABORATORIOS
				sourceLabKey =	remainingRecords.AÑO + remainingRecords.SEMANA + remainingRecords.COD_EVE + remainingRecords.TIP_IDE + ;
								remainingRecords.NUM_IDE + remainingRecords.COD_PRE + remainingRecords.COD_SUB + remainingRecords.MUESTRA + ;
								remainingRecords.PRUEBA + DTOC(remainingRecords.FEC_EXA,1) + DTOC(remainingRecords.FEC_REC,1)
				
				SELECT (MasterTableName)
				SEEK sourceLabKey
				IF FOUND() THEN	
					IF 	remainingRecords.FEC_AJU >= &MasterTableName..FEC_AJU THEN
						*Los datos del registro que se está cargando son más recientes que los del registro existente en la tabla maestra
						IF detectChange(@aSourceTablefields) THEN
							TRY
								GATHER MEMVAR
								CON_LAB = CON_LAB + 1
							CATCH TO oException
								sResult=.F.
								sErrorMsg = sErrorMsg + oException.Message
							ENDTRY
						ENDIF
					ENDIF
				ELSE
					TRY
						INSERT INTO (MasterTableName) FROM MEMVAR
						CON_LAB = CON_LAB + 1
					CATCH TO oException
						sResult=.F.
						sErrorMsg = sErrorMsg + oException.Message
					ENDTRY
				ENDIF
				SELECT RemainingRecords
			ENDSCAN

			IF sResult THEN
				*Agrega los registros duplicados
				SELECT (masterTableName)
				nLastRecNumber = RECCOUNT()
				TRY
					APPEND FROM (duplicatesRSName)
					IF _TALLY > 0 THEN
						*Asigna el valor del campo NOTIFICA a los registros recien agregados. El procesamiento es secuencial;
						por cuanto debe evitarse un problema con el bloqueo de registros
						sOldTag = TAG()
						SET ORDER TO
						GO (nLastRecNumber + 1)
						DO WHILE !EOF()
							sCmd = 'REPLACE NOTIFICA WITH ' + ALLTRIM(STR(sNotificationWeek)) 
							&sCmd
							SKIP 
						ENDDO
						SET ORDER TO TAG &sOldTag 
					ENDIF
				CATCH TO oException
					sResult=.F.
					sErrorMsg = sErrorMsg + oException.Message
				ENDTRY
				IF sResult THEN
					* Incrementa el contador con la cantidad de registros agregados por el último Append
					CON_LAB = CON_LAB + CountTableRecords(duplicatesRSName)
					
					*Exporta los registros duplicados hacia un archivo excel para posterior corrección
					USE (duplicatesRSName) IN 0
					SELECT (duplicatesRSName)
					IF RECCOUNT(duplicatesRSName) > 0 THEN
						duplicatesExcelFileName=duplicatesFilePath + '\' + sourceFileName + '_DUPLICADOS'
						COPY TO [&duplicatesExcelFileName] XL5
					ENDIF
				ENDIF
			ENDIF
		ENDIF

		*Borra las tabla temporales utilizadas durante el procesamiento
		DO DeleteTable WITH 'tmpLABS_a_importar'

		*Actualiza la tabla de registros EXISTENTES
		= TABLEUPDATE(1,.T.,existentsFileName,aImportErrors)

	ENDIF

	*CLOSE TABLES
	DO CloseTables WITH "Eventos,&sourceFileName,&existentsFileName"
	DO DeleteTable WITH 'T_NonDups_LAB'
	DO DeleteTable WITH 'C_DUPS_LAB'
ENDPROC


PROCEDURE DeleteTable

	LPARAMETERS tableName as String, bWithCDX as Boolean 

	IF USED(tableName) THEN
		SELECT (tableName)
		USE
	ENDIF
	DELETE FILE (tableName) + '.dbf'
	IF bWithCDX THEN
		DELETE FILE (tableName) + '.CDX'
	ENDIF
ENDPROC


PROCEDURE addContacts(candidatesRSName, duplicatesRSName, sourceFilePath, sourceFileName, existentsFilePath, ;
					existentsFileName, duplicatesFilePath, sResult as Boolean, sErrorMsg as String, sNotificationWeek as Number)

	* candidatesRSName: nombre de una tabla o cursor que contiene los registros que son candidatos a ;
		ser almacenados en la tabla maestra CONTACTOS. Un registro es candidato si ;
		ha superado las pruebas de validación
						
	* duplicatesRSName: nombre de una tabla o cursor que contiene los registros duplicados a ;
		ser almacenados en la tabla maestra de CONTACTOS. Un registro es duplicados si ;
		su clave de identificación es duplicada
						
	* sourceFilePath + sourceFileName: nombre y ubicación de la tabla fuente que contiene todos los ;
		registros (candidatos y no candidatos) que deberían ser almacenados en la tabla maestra ;
		CONTACTOS

	* existentsFilePath, existentsFileName: nombre y ubicación de la tabla en donde deben guardarse ;
		los registros que ya existen en la tabla maestra y que no pudieron ser grabados en ella

	* duplicatesFilePath: ruta de la carpeta en donde deben guardarse los registros de duplicatesRSName

	* sNotificationWeek (opcional): valor que debe asignarse a la semana de notificación (Tables.CONTACTOS.NOTIFICA);
	para los registros nuevos que se adicionen a la tabla CONTACTOS. El valor por defecto de este parámetro es 0.

	LOCAL nNewRecordsAdded AS Number 
	
	IF TOT_CONTACTOS >= 1

		IF VARTYPE(sNotificationWeek)='L' THEN
			sNotificationWeek = 0
		ENDIF

		SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
		
		* Configura las areas de trabajo con las tablas necesarias para llevar a cabo la operación
		masterTableName = 'CONTACTOS'
		=SelectTable(masterTableName,'PK',.F.)
		
		=SelectTable('&sourceFilePath\&sourceFileName',,.F.)
		=SelectTable('&existentsFilePath\&existentsFileName',,.F.)

		CON_CONTACTO = 0
		sResult = .T.

		* Agrega a la tabla maestra masterTableName todos los registros candidatos nuevos. ;
		Un registro candidato es nuevo si su clave {TIP_IDE+NUM_IDE+TIPIDE_POS+NUMIDE_POS+COD_EVEPOS} no ;
		se encuentra en la tabla maestra. 
		setNewRecsClause='SELECT A.*, ' + ALLTRIM(STR(sNotificationWeek)) + ' AS NOTIFICA FROM ' ;
			+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + MasterTableName + ' B'+;
			+ ' ON (A.TIP_IDE = B.TIP_IDE ';
			+ ' AND A.NUM_IDE = B.NUM_IDE ';
			+ ' AND A.TIPIDE_POS = B.TIPIDE_POS ';		
			+ ' AND A.NUMIDE_POS = B.NUMIDE_POS ';
			+ ' AND A.COD_EVEPOS = B.COD_EVEPOS )';		
			+ ' WHERE B.COD_EVEPOS IS NULL INTO TABLE tmpCONTACTOS_a_importar.dbf' 
		&setNewRecsClause

		nNewRecordsAdded = 0
		SELECT tmpCONTACTOS_a_importar
		SCAN
			SCATTER MEMVAR 
			IF !INDEXSEEK(m.ID,.F.,MasterTableName,'PK') THEN 
				*El registro a agregar es realmente nuevo por cuanto tampoco infringe la clave PK de la tabla MasterTableName
				TRY 
					INSERT INTO (MasterTableName) FROM MEMVAR
					
					*Declara que el registro ha sido transferido a la tabla MasterTableName de tal manera que no se tenga en cuenta ;
					en procesos posteriores
					REPLACE SUBE WITH 1 FOR NUM_REG=tmpCONTACTOS_a_importar.NUM_REG IN (sourceFileName)
					
					nNewRecordsAdded = nNewRecordsAdded + 1
				CATCH TO oException
				ENDTRY
			ENDIF 
		ENDSCAN 
		SELECT (MasterTableName)
		
		IF sResult THEN
			* Incrementa el contador con la cantidad de registros nuevos agregados
			CON_CONTACTO = CON_CONTACTO + nNewRecordsAdded 

			* Actualiza/refresca la tabla fuente 
			=TABLEUPDATE(1,.T.,sourceFileName)
			
			* Intenta agregar los registros restantes siempre y cuando cumplan ciertas condiciones
			SELECT (MasterTableName)
			SET ORDER TO TAG PK_REAL

			setCandidatesRecordsClause='SELECT * FROM ' + sourceFileName + ' WHERE SUBE=0 INTO CURSOR remainingRecords NOFILTER'
			&setCandidatesRecordsClause
			SELECT remainingRecords  
			DIMENSION aSourceTablefields(1)
			nSourceTablefields=AFIELDS(aSourceTablefields)

			SCAN WHILE sResult
				SCATTER MEMVAR

				*Fija la clave de búsqueda de registros en la tabla maestra 
				sSourceKey = remainingRecords.TIP_IDE + remainingRecords.NUM_IDE + remainingRecords.TIPIDE_POS + remainingRecords.NUMIDE_POS + remainingRecords.COD_EVEPOS
				
				SELECT (MasterTableName)
				SEEK sSourceKey
				IF FOUND() THEN	
					IF 	remainingRecords.FEC_AJU >= &MasterTableName..FEC_AJU THEN
						*Los datos del registro que se está cargando son más recientes que los del registro existente en la tabla maestra
						IF detectChange(@aSourceTablefields) THEN
							TRY
								GATHER MEMVAR
								CON_CONTACTO = CON_CONTACTO + 1
							CATCH TO oException
								sResult=.F.
								sErrorMsg = sErrorMsg + oException.Message
							ENDTRY
						ENDIF
					ENDIF
				ELSE
					*El registro es nuevo según la clave PK_REAL pero infringiría la clave PK de la tabla MasterTableName, es decir, según la clave PK;
					el registro sí existe
					sSourceKey = remainingRecords.ID
					SEEK sSourceKey ORDER TAG PK
					IF FOUND() THEN	
						IF 	remainingRecords.FEC_AJU >= &MasterTableName..FEC_AJU THEN
							*Los datos del registro que se está cargando son más recientes que los del registro existente en la tabla maestra
							IF detectChange(@aSourceTablefields) THEN
								TRY
									GATHER MEMVAR
									CON_CONTACTO = CON_CONTACTO + 1
								CATCH TO oException
									sResult=.F.
									sErrorMsg = sErrorMsg + oException.Message
								ENDTRY
							ENDIF
						ENDIF
					ENDIF
				ENDIF
				SELECT RemainingRecords
			ENDSCAN
		ENDIF

		*Borra las tabla temporales utilizadas durante el procesamiento
		DO DeleteTable WITH 'tmpCONTACTOS_a_importar'

		*Actualiza la tabla de registros EXISTENTES
		= TABLEUPDATE(1,.T.,existentsFileName,aImportErrors)

	ENDIF

	DO CloseTables WITH "Eventos,&sourceFileName,&existentsFileName"
ENDPROC

PROCEDURE addFollowUps(candidatesRSName, duplicatesRSName, sourceFilePath, sourceFileName, existentsFilePath, ;
						existentsFileName, duplicatesFilePath, sResult as Boolean, sErrorMsg as String, sNotificationWeek as Number)

	* candidatesRSName: nombre de una tabla o cursor que contiene los registros que son candidatos a ;
		ser almacenados en la tabla maestra SEGUIMIENTOS. Un registro es candidato si ;
		ha superado las pruebas de validación
						
	* duplicatesRSName: nombre de una tabla o cursor que contiene los registros duplicados a ;
		ser almacenados en la tabla maestra de SEGUIMIENTOS. Un registro es duplicados si ;
		su clave de identificación es duplicada
						
	* sourceFilePath + sourceFileName: nombre y ubicación de la tabla fuente que contiene todos los ;
		registros (candidatos y no candidatos) que deberían ser almacenados en la tabla maestra ;
		SEGUIMIENTOS

	* existentsFilePath, existentsFileName: nombre y ubicación de la tabla en donde deben guardarse ;
		los registros que ya existen en la tabla maestra y que no pudieron ser grabados en ella

	* duplicatesFilePath: ruta de la carpeta en donde deben guardarse los registros de duplicatesRSName

	* sNotificationWeek (opcional): valor que debe asignarse a la semana de notificación (Tables.SEGUIMIENTOS.NOTIFICA);
	para los registros nuevos que se adicionen a la tabla SEGUIMIENTOS. El valor por defecto de este parámetro es 0.

	LOCAL CON_SEGUIMIENTOS as Number, nNewRecordsAdded as Number 
	STORE 0 TO CON_SEGUIMIENTOS
	
	LOCAL bResult as Boolean 
	STORE .T. TO bResult

	IF TOT_SEGUIMIENTOS >= 1

		IF VARTYPE(sNotificationWeek)='L' THEN
			sNotificationWeek = 0
		ENDIF

		SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
		
		* Configura las areas de trabajo con las tablas necesarias para llevar a cabo la operación
		masterTableName = 'SeguimientoContactos'
		=SelectTable(masterTableName,'PK',.F.)
		
		=SelectTable('&sourceFilePath\&sourceFileName',,.F.)
		=SelectTable('&existentsFilePath\&existentsFileName',,.F.)

		* Agrega a la tabla maestra masterTableName todos los registros candidatos nuevos. ;
		Un registro candidato es nuevo si su clave {ID_PERSONA+FEC_SEGUMI} no se encuentra en la tabla maestra. 
		setNewRecsClause='SELECT A.*, ' + ALLTRIM(STR(sNotificationWeek)) + ' AS NOTIFICA FROM ' ;
			+ (candidatesRSName) + ' A LEFT OUTER JOIN ' + MasterTableName + ' B'+;
			+ ' ON (A.ID_PERSONA = B.ID_PERSONA AND A.FEC_SEGUMI = B.FEC_SEGUMI)';		
			+ ' WHERE B.ID IS NULL INTO TABLE tmpSEGUIMIENTOS_a_importar.dbf' 
		&setNewRecsClause

		nNewRecordsAdded = 0
		SELECT tmpSEGUIMIENTOS_a_importar
		SCAN
			SCATTER MEMVAR 
			IF !INDEXSEEK(m.ID,.F.,MasterTableName,'PK') THEN 
				*El registro a agregar es realmente nuevo por cuanto tampoco infringe la clave PK de la tabla MasterTableName
				TRY 
					INSERT INTO (MasterTableName) FROM MEMVAR
					
					*Declara que el registro ha sido transferido a la tabla MasterTableName de tal manera que no se tenga en cuenta ;
					en procesos posteriores
					REPLACE SUBE WITH 1 FOR NUM_REG=tmpSEGUIMIENTOS_a_importar.NUM_REG IN (sourceFileName)
					
					nNewRecordsAdded = nNewRecordsAdded + 1
				CATCH TO oException
				ENDTRY
			ENDIF 
		ENDSCAN 
		SELECT (MasterTableName)
		
		IF bResult THEN
			* Incrementa el contador con la cantidad de registros agregados por el último Append
			CON_SEGUIMIENTOS = CON_SEGUIMIENTOS + nNewRecordsAdded 

			* Actualiza/refresca la tabla fuente 
			=TABLEUPDATE(1,.T.,sourceFileName)
			
			* Intenta agregar los registros restantes siempre y cuando cumplan ciertas condiciones
			SELECT (MasterTableName)
			SET ORDER TO TAG PK_REAL

			setCandidatesRecordsClause='SELECT * FROM ' + sourceFileName + ' WHERE SUBE=0 INTO CURSOR remainingRecords NOFILTER'
			&setCandidatesRecordsClause
			SELECT remainingRecords  
			DIMENSION aSourceTablefields(1)
			nSourceTablefields=AFIELDS(aSourceTablefields)
*SET STEP ON 		
			SCAN WHILE bResult
				SCATTER MEMVAR

				*Fija la clave de búsqueda de registros en la tabla maestra LABORATORIOS
				sSourceKey = remainingRecords.ID_PERSONA + DTOC(remainingRecords.FEC_SEGUMI,1)
				
				SELECT (MasterTableName)
				SEEK sSourceKey
				IF FOUND() THEN	
					IF 	remainingRecords.FEC_AJU >= &MasterTableName..FEC_AJU THEN
						*Los datos del registro que se está cargando son más recientes que los del registro existente en la tabla maestra
						IF detectChange(@aSourceTablefields) THEN
							TRY
								GATHER MEMVAR
								CON_SEGUIMIENTOS = CON_SEGUIMIENTOS + 1
							CATCH TO oException
								bResult=.F.
								sErrorMsg = sErrorMsg + oException.Message
							ENDTRY
						ENDIF
					ENDIF
				ELSE
					*El registro es nuevo según la clave PK_REAL pero infringiría la clave PK de la tabla MasterTableName, es decir, según la clave PK;
					el registro sí existe
					sSourceKey = remainingRecords.ID
					SEEK sSourceKey ORDER TAG PK
					IF FOUND() THEN	
						IF 	remainingRecords.FEC_AJU >= &MasterTableName..FEC_AJU THEN
							*Los datos del registro que se está cargando son más recientes que los del registro existente en la tabla maestra
							IF detectChange(@aSourceTablefields) THEN
								TRY
									GATHER MEMVAR
									CON_SEGUIMIENTOS = CON_SEGUIMIENTOS + 1
								CATCH TO oException
									sResult=.F.
									sErrorMsg = sErrorMsg + oException.Message
								ENDTRY
							ENDIF
						ENDIF
					ENDIF 
				ENDIF
				SELECT RemainingRecords
			ENDSCAN
		ENDIF

		*Borra las tabla temporales utilizadas durante el procesamiento
		DO DeleteTable WITH 'tmpSEGUIMIENTOS_a_importar'

		*Actualiza la tabla de registros EXISTENTES
		= TABLEUPDATE(1,.T.,existentsFileName,aImportErrors)

	ENDIF

	DO CloseTables WITH "Eventos,&sourceFileName,&existentsFileName"
ENDPROC


PROCEDURE DeleteTable

	LPARAMETERS tableName as String, bWithCDX as Boolean 

	IF USED(tableName) THEN
		SELECT (tableName)
		USE
	ENDIF
	DELETE FILE (tableName) + '.dbf'
	IF bWithCDX THEN
		DELETE FILE (tableName) + '.CDX'
	ENDIF
ENDPROC

PROCEDURE CloseTables

	LPARAMETERS tablesNameList as String
	
	LOCAL i as Byte
	
	nTablesToClose = ALINES(aTablesToClose,tablesNameList,1,",")
	FOR i=1 TO nTablesToClose 
		IF USED(aTablesToClose(i)) THEN
			SELECT (aTablesToClose(i))
			USE
		ENDIF
	ENDFOR
ENDPROC


PROCEDURE migrateData(pathToSourceDataFiles AS String, sNotifierKind as String,  sNotifierId as String)
	
	*Migra información de las tablas de datos desde un Sistema Sivigila anterior hacia el Sistema Sivigila actual, incluyendo ;
	desde la versión 2008 en adelante. Se supone que las tablas de datos se encuentran en pathToSourceDataFiles. ;
	Luego de finalizada la migración, el sistema se cierra.
	
	*Si la entidad Notificadora caracterizada por el par (sNotifierKind, sNotifierId) corresponde el Instituto Nacional de Salud, ;
	permite hacer migraciones sin que las tablas del Sistema receptor estén vacías; en caso contrario, la migración ;
	procederá únicamente si las tablas del receptor están vacías excepto la tabla GENERAL.
	
	
	LOCAL bContinue AS Boolean
	LOCAL sErrorMsg AS String, SQLClause AS String, cmdAppendFrom AS String
	LOCAL nEventsToTransfer AS Long, nEventToLoad AS Long, i AS Long
	LOCAL sEventSuffix AS String
	LOCAL sPostProcedure as String
	
	LOCAL bDoTransfer307 AS Boolean 
	*Indica si es necesario transformar datos del evento 307 - Vigilancia integrada de rabia humana. aplica solo para la versión 2018 del sistema
	
	lcOldOnError = ON("ERROR") && Save default error handler.
	ON ERROR

	bContinue=.T.
	bCloseApp=.T.
	
	bDoTransfer307 = .T.
*SET STEP ON 	
	IF EMPTY(pathToSourceDataFiles) THEN
		bContinue=.F.
		sErrorMsg='No seleccionó la carpeta desde donde se deben migrar los datos'
	ELSE
		CLOSE ALL
		SET EXCLUSIVE ON
		
		*Verifica que las tablas del sistema hacia donde se va a migrar información estén vacías.
		*Solo si la entidad Notificadora es el Instituto Nacional de Salud, permite hacer migraciones sin que;
		las tablas del Sistema estén vacías
		TRY
			vSUFile = FILETOSTR('su') 
		CATCH TO oException
		ENDTRY
		IF !(sNotifierKind = UND_ID and sNotifierId = INS_ID AND IIF(VARTYPE(vSUFile)='C',vSUFile = CHR(13)+chr(10)+CHR(7),.F.)) THEN
			DO CountRecords WITH nSivigila2010Events + N_EVENTS_AFTER_SIVIGILA2010_LAUNCHING IN SivigilaUtilities
			SELECT * FROM CONTEOREGISTROS WHERE REGS > 0 AND NOMBRE!='GENERAL' INTO ARRAY aDBRecords
			IF _TALLY > 0 THEN
				bContinue=.F.
				sErrorMsg='Las tablas de este sistema no están vacías y no se puede hacer la migración'
			ENDIF
		ELSE
			bCloseApp=.F.
		ENDIF
		RELEASE vSUFile 
		
		IF 	bContinue THEN
			*Establece el número de tablas de datos complementarios de eventos de salud que se deben migrar
			nEventsToTransfer = ADIR(aEventsToTransfer, pathToSourceDataFiles + '\Eventos_??.DBF')
			
			*Con base en el número de tablas de datos complementarios de eventos de salud, establece la versión;
			 del sistema desde el cual se está haciendo la migración
			IF nEventsToTransfer=0 THEN
				bContinue=.F.
				sErrorMsg='La carpeta seleccionada no contiene tablas del sistema SIVIGILA. No hay nada que migrar'
			ELSE
				IF nEventsToTransfer = nSivigila2008Events THEN
					nSivigilaVersion=_VERSION2008
				ELSE
					IF nEventsToTransfer >= nSivigila2010Events THEN
						nSivigilaVersion=_VERSION2010
					ELSE
						bContinue=.F.
						sErrorMsg='No se pudo establecer desde qué versión del sistema SIVIGILA se está haciendo la migración'
					ENDIF
				ENDIF
*SET STEP ON 
				*Para la versión 2018 del sistema, verifica si las unidades departamentales o el INS, ya han aplicado una utilidad de corrección ;
				de migración del evento 307
				*IF sNotifierKind = UND_ID OR sNotifierKind = UNN_ID THEN
				*	sDummyFileToCheckFor = '"' + ADDBS(pathToSourceDataFiles) + '\DUMMY.DBF"'
				*	IF !FILE(sDummyFileToCheckFor) THEN
				*		bContinue=.F.
				*		sErrorMsg='Al backup desde no se está migrando, no se le ha aplicado la ' + CR +;
				*					'utilidad de corrección de migración del evento 307.' + CR_LF +;
				*					'Es condición necesaria para la migración haber aplicado ' + CR +;
				*					'esa utilidad.'
				*	ELSE
				*		bDoTransfer307 = .F.
				*	ENDIF
				*ENDIF

				IF bContinue THEN
					*Desactiva el registro de la versión del Sistema con que fueron grabados los registros
					RELEASE gsSivigilaRecordVersion

					*Desactiva el registro de la fecha inicial de caracterización de UPGDs
					gsUPGDSetupDate = CTOD('')
				
					*Desactiva la validación de campos de las tablas de datos del sistema SIVIGILA
					glDisableRules=.T.
					
					*Desactiva la asignación de un valor por defecto para los campos EstadoTran y Est_Ingr(Estado de ingreso) en las tablas en donde exista
					gsTransferState = ''
					gsInState = 0

					FOR nEventToLoad=1 TO nEventsToTransfer 
						IF FILE((pathToSourceDataFiles) + '\EVENTOS_' + PADL(nEventToLoad,2,'0') + '.DBF') THEN
							sEventSuffix = PADL(INT(VAL(SUBSTR(aEventsToTransfer(nEventToLoad,1),9,2))),2,'0')
							IF nSivigilaVersion=_VERSION2010 THEN
								DO migrateTable WITH (pathToSourceDataFiles) + '\EVENTOS_' + sEventSuffix + '.DBF', 'EVENTOS_' + sEventSuffix
							ELSE
								*Verifica si los datos de los eventos que vienen en la tabla EVENTOS_xx (xx=EventSuffix) deben ser;
								cargados en la tabla maestra EVENTOS_xx o en una equivalente en el sistema versión 2010
								SQLClause =	"SELECT * FROM mapeoTablas2008vs2010 WHERE VAL('" + sEventSuffix + "')"	+ ;
											"= Form2008 INTO ARRAY aTablesMapping"
								&SQLClause 
								IF _TALLY>0 THEN
									*En la tabla EVENTOS_xx (xx=EventSuffix) pueden existir datos de eventos que deben almacenarse en;
									una tabla maestra diferente a EVENTOS_xx
									vTalkState=SET("Talk")
									vSafetyState=SET("Safety")
									SET TALK ON
									SET SAFETY OFF
									
									FOR i=1 TO _TALLY  
										targetEventSuffix = PADL(ALLTRIM(STR(aTablesMapping(i,3))),2,'0')
										IF !USED("EVENTOS_&targetEventSuffix") THEN
											USE 'EVENTOS_' + targetEventSuffix 
										ELSE
											SELECT 'EVENTOS_' + targetEventSuffix 
										ENDIF
										
										sourceTable=pathToSourceDataFiles + "\EVENTOS_" + sEventSuffix + ".dbf"
										cmdAppendFrom="APPEND FROM '" + sourceTable + "' FOR COD_EVE='" + aTablesMapping(i,1) + "'"

										WAIT 'Migrando registros desde ' + sourceTable WINDOW NOWAIT
										&cmdAppendFrom
									ENDFOR
									SET TALK &vTalkState
									SET SAFETY &vSafetyState
								ELSE
									DO migrateTable WITH (pathToSourceDataFiles) + '\EVENTOS_' + sEventSuffix + '.DBF', 'EVENTOS_' + sEventSuffix
								ENDIF
							ENDIF
						ENDIF
					NEXT

					*La siguiente parte de código se desactivó en la versión 2014 del Sistema debido al proyecto de ampliación de UPGDs ;
					ejecutado durante el año 2013 en todo el País
					&&IF nEventsToTransfer >= nSivigila2010Events + N_EVENTS_AFTER_SIVIGILA2010_LAUNCHING THEN
						*Se están migrando datos desde un sistema que tiene igual número de tablas de eventos que el actual;
						por tanto, no debe tomar acciones post-migración en la tabla de UPGDs migrada
					&&	DO migrateTable WITH (pathToSourceDataFiles) + '\UPGD.DBF', 'UPGD'
					&&ELSE
						*Se deben desactivar todas las UPGDs migradas
						&&sPostProcedure='REPLACE ACT_SIV WITH 2 ALL'
						&&DO migrateTable WITH (pathToSourceDataFiles) + '\UPGD.DBF', 'UPGD',sPostProcedure
					&&ENDIF
					
					*La siguiente parte de código se cambió en la versión 2015 del Sistema debido a que ya no se considera necesario hacer;
					el cálculo de Fecha Inicial de Caracterización en la tabla de UPGDs ;
					DO migrateTable WITH (pathToSourceDataFiles) + '\UPGD.DBF', 'UPGD', 'DO setUPGDsSetupDate IN UPGDsHandler'

					*Comentado por:	Wilson Aguilar	Fecha:16/01/2018;
					Se decidió eliminar esta parte de código debido a la incidencia con ID=1064
					*La versión 2018 del sistema elimina de la tabla UPGD registros que con correspondan a la entidad territorial/UPGD notificadora
					*DO CASE
					*	CASE sNotifierKind = UPGD_ID
					*		sDeleteFilterExpr = "NOT LIKE('" + SUBSTR(ALLTRIM(sNotifierId),1,UPGD_CODE_LENGHT) + "',COD_PRE)"
					*	CASE sNotifierKind = UNM_ID
					*		sDeleteFilterExpr = "NOT LIKE('" + SUBSTR(ALLTRIM(sNotifierId),1,UNM_CODE_LENGHT) + REPLICATE('?',UPGD_CODE_LENGHT-UNM_CODE_LENGHT) + "',COD_PRE)"
					*	CASE sNotifierKind = UND_ID
					*		sDeleteFilterExpr = "NOT LIKE('" + SUBSTR(ALLTRIM(sNotifierId),1,UND_CODE_LENGHT) + REPLICATE('?',UPGD_CODE_LENGHT-UND_CODE_LENGHT) + "',COD_PRE)"
					*ENDCASE
					*sPostProcedure = 'DO bulkDeleteUPGDs WITH "' + sDeleteFilterExpr + '" IN UPGDsHandler'
					*DO migrateTable WITH (pathToSourceDataFiles) + '\UPGD.DBF', 'UPGD', sPostProcedure 
					
					DO migrateTable WITH (pathToSourceDataFiles) + '\UPGD.DBF', 'UPGD'
					DO migrateTable WITH (pathToSourceDataFiles) + '\PACIENTE.DBF', 'PACIENTE'
					DO migrateTable WITH (pathToSourceDataFiles) + '\LABORATORIOS.DBF', 'LABORATORIOS'
					DO migrateTable WITH (pathToSourceDataFiles) + '\BROTES.DBF', 'BROTES'
					DO migrateTable WITH (pathToSourceDataFiles) + '\TAL_HUM.DBF', 'TAL_HUM'
					DO migrateTable WITH (pathToSourceDataFiles) + '\AJUSTES.DBF', 'AJUSTES'
					DO migrateTable WITH (pathToSourceDataFiles) + '\NOTIFICADOS.DBF', 'NOTIFICADOS'
					DO migrateTable WITH (pathToSourceDataFiles) + '\UPGD_UCIS.DBF', 'UPGD_UCIS', 'DO UPGD_UCIS_PostProc'
					DO migrateTable WITH (pathToSourceDataFiles) + '\CONTACTOS.DBF', 'CONTACTOS', , .T.
					DO migrateTable WITH (pathToSourceDataFiles) + '\SEGUIMIENTOCONTACTOS.DBF', 'SEGUIMIENTOCONTACTOS', , .T.
					DO migrateTable WITH (pathToSourceDataFiles) + '\tbCasosAnomalosDetectados.DBF', 'tbCasosAnomalosDetectados'
					
					*Homologa variables entre versiones para la información recien migrada
					oVariableTranslator = NEWOBJECT("VariableTranslator","AnomaliesSolver.Prg",.null.)
					SET PROCEDURE TO MAIN ADDITIVE
					oVariableTranslator.sXlsFilePath = SIVIGILADefaultXLSPath()
					oVariableTranslator.standarizeComparableEvents()
					
					*oVariableTranslator.sSourceTableName="PACIENTE"
					oVariableTranslator.translateVariablesEventFree

					*oVariableTranslator.sSourceTableName="EVENTOS_88"
					oVariableTranslator.translateVariablesEventFree(.T.)
					
					IF oVariableTranslator.nUpdatedRecs>0 THEN
						oVariableTranslator.ShowResults()
					ELSE
						WAIT  "No se encontraron registros susceptibles de ser homologados" WINDOW NOWAIT TIMEOUT 5
					ENDIF

					RELEASE oVariableTranslator 
*SET STEP ON 
					 **JLGB referencia requerimiento REQ204
					WAIT  "Ejecutando ajustes post-migración" WINDOW NOWAIT
					*Comentado por:	Wilson Aguilar		Fecha:22/10/2017;
					La siguiente parte de código fue modificada para corregir un error que se producía al volver a migrar a partir de un backup ;
					en donde ya se habían efectuado las transformaciones de datos

					*Ejecuta transformaciones programadas al migrar
					DO TransformOnMigration WITH pathToSourceDataFiles

					WAIT CLEAR
					
					*Ejecuta tareas de limpieza de datos en las tablas recien migradas
					*DO cleanSIVIGILADuplicates IN AnomaliesHandler
					*DO cleanSIVIGILAMissingData WITH "PACIENTE,TAL_HUM" IN AnomaliesHandler

					*Ejecuta una migración especial para registros del evento 307 - vigilancia integrada de rabia ... introducida para la versión 2018 del sistema que 
					IF bDoTransfer307 THEN
						IF nEventsToTransfer < n_SIVIGILA_EVENTS THEN
							*Se está migrando desde una versión antigua del sivigila y, por tanto, debe efectuarse la migración especial
							DO adjust_307_Cases
							DO transfer_307_ToTargetTables 
						ENDIF
					ENDIF					
					
					*JLGB 28/02/2018 ajuste del calendario epidemiologico
					*WAIT  "Verificando integridad de datos" WINDOW NOWAIT
					*oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,.null.)
					*oAnomaliesSolver.WeekYearAdjustment()
					*WAIT CLEAR
					
					sErrorMsg='Migración finalizada.' + CHR(13)
					IF bCloseApp THEN
						sErrorMsg = sErrorMsg + 'El sistema se cerrará.'
					ENDIF
					
					*Reactiva la validación de campos de tablas
					glDisableRules=.F.

					*Reactiva la asignación de un valor por defecto para los campos EstadoTran y Est_Ingr(Estado de ingreso) en las tablas en donde exista
					RELEASE gsTransferState, gsInState  
					
					*Reactiva el registro de la versión del Sistema con que son grabados los registros
					DO resetSivigilaVersion IN Main
					
					*Reactiva el registro de la fecha inicial de caracterización de UPGDs
					RELEASE gsUPGDSetupDate
				ENDIF
			ENDIF
		ENDIF
		CLOSE ALL
		SET EXCLUSIVE OFF
	ENDIF

	IF !bContinue THEN
		MESSAGEBOX( sErrorMsg, 0 + 16 + 256, 'Violación de migración SIVIGILA' )
		DO BASES
	ELSE
		MESSAGEBOX( sErrorMsg, 0 + 48 + 256, 'SIVIGILA' )
		ON ERROR &lcOldOnError
		IF bCloseApp THEN
			QUIT
		ELSE
			DO BASES
		ENDIF
	ENDIF

ENDPROC


PROCEDURE migrateTable(sourceFileAndPath AS String, targetTableName as string, postProcedures as String, bSequential as Boolean )

	IF FILE(sourceFileAndPath) THEN
		vSafetyState=SET("Safety")
		vTalkState=SET("Talk")
		SET TALK ON
		SET SAFETY OFF
		
		WAIT 'Migrando registros desde ' + sourceFileAndPath WINDOW NOWAIT
		IF USED(targetTableName) THEN
			SELECT (targetTableName)
		ELSE
			USE (targetTableName)
		ENDIF
		
		IF !bSequential THEN 
			APPEND FROM (sourceFileAndPath)
		ELSE
			nNewRecordsAdded = 0
			USE (sourceFileAndPath) ALIAS tbPorAgregar IN 0
			SELECT tbPorAgregar
			SCAN
				SCATTER MEMVAR 
				TRY 
					INSERT INTO (targetTableName) FROM MEMVAR
					nNewRecordsAdded = nNewRecordsAdded + 1
				CATCH TO oException
				ENDTRY
			ENDSCAN 
			USE IN 'tbPorAgregar'
		ENDIF 
		
		IF VARTYPE(postProcedures)='C' AND !EMPTY(postProcedures) THEN
			&postProcedures
		ENDIF
		
		SET TALK &vTalkState
		SET SAFETY &vSafetyState
	ENDIF
ENDPROC


FUNCTION detectChange(aSourceTableFields)
	
	*Returns .T. if the aSourceTableFields that belong to the current table are ALL equal to memory variables ;
	except those in sExcludedFields; otherwise returns .F.
	
	sExcludedFields = " NOTIFICA FLU_NOT INMEDIATA NUM_REG SUBE SEM_REC FECHACARGA ESTADOTRAN EST_INGR EST_NOTIF "

	DIMENSION aCurrentTableFields(1)
	
	bIsEqual=.F.
	sSourceTableName=ALIAS()
	IF !EMPTY(sSourceTableName) THEN
		nSourceTableFields=ALEN(aSourceTableFields,1)
		nCurrentTableFields=AFIELDS(aCurrentTableFields)
		bIsEqual=.T.
		nField=1
		DO WHILE bIsEqual AND nField<=nSourceTableFields
			IF ASCAN(aCurrentTableFields,aSourceTableFields(nField,1),-1,-1,-1,15) > 0 THEN
				sMemoryValue="m." + aSourceTableFields(nField,1)
				IF VARTYPE(&sMemoryValue)<>'U' THEN
					sFieldValue=sSourceTableName + "." + aSourceTableFields(nField,1)
					IF  !(aSourceTableFields(nField,1) $ sExcludedFields) THEN
						
						IF VARTYPE(&sFieldValue) = 'D' THEN 
							*Obliga a que valores string vacíos de sMemoryValue, sean interpretados como fechas 
							IF VARTYPE(&sMemoryValue) = 'C' AND EMPTY(&sMemoryValue) THEN
								sMemoryValue = "CTOD('')"
							ENDIF
						ENDIF
						
						IF &sMemoryValue!=&sFieldValue THEN
							bIsEqual=.F.
						ENDIF
					ENDIF
				ENDIF
			ENDIF
			nField = nField + 1
		ENDDO
	ENDIF
	
	RETURN !bIsEqual
	
ENDFUNC


PROCEDURE validateSIVIGILAData(masterTableName as String, sFilter as string, bAddRecs as Boolean, bResult as Boolean, sSummarizeBy as String, ;
								sTracePath as String, oProgressBar as Object)
	
	*Valida los registros de la tabla masterTableName que cumplan la condición sFilter (opcional) estableciendo si cumplen o no las reglas de;
	validación establecidas para ellos en la base de datos del SIVIGILA. Para ello, hace uso de una base de datos espejo de la del SIVIGILA ;
	en donde se van transfiriendo uno a uno los registros de masterTableName; si en la transferencia de un registro, ocurre un fallo por violación;
	de la regla de validación establecida, se captura la información detallada de la regla violentada y se deja registro de ella y de la totalidad;
	del registro que la incumple en un archivo excel de nombre masterTableName + ResultadosCarga que quedará almacenado en la carpeta sTracePath 
	
	*bAddRecs: indica si deben agregarse a la tabla espejo de masterTableName los registros que cumplan con las reglas de validación. En caso de;
	que no se pase este parámetro, la tabla espejo masterTableName será desocupada previa la ejecución de la validación de registros.
	
	*bResult: parámetro de salida que indicará si todos los registros que se validaron cumplieron con las reglas de validación. En caso de que;
	al menos un registro validado no cumpla con las reglas de validación, tomará el valor .F.
	
	*sSummarizeBy (opcional): lista de campos de masterTablename separada por comas que permite crear y mantener una tabla consolidado de resultados;
	de nombre tbTmpConsolidadoResultadosCarga y cuyos campos serán los expecificados en este parámetro más los del siguiente conjunto ;
	{TablaFuente C(254), RecId N, Resultado_ C(254)}.

	*oProgressBar(opcional): muestra el progreso de la validación de registros utilizando un termómetro
	
	*NOTAS: este procedimiento cierra todas las tablas y base de datos que se encuentren abiertas en la sesión de datos actual y no las restablece;
	al final de su ejecución cierra las tablas que ha utilizado excepto el cursor tbTmpConsolidadoResultadosCarga, si es que se ha pasado el ;
	parámetro opcional sSummarizeBy 

	LOCAL bContinue as Boolean, sCandidatesRSName AS String

	LOCAL sErrorMsg as String 
	sErrorMsg = ''

	LOCAL nLoadedRecords as Number
	LOCAL sOldSafety AS String, sOldExclusive AS String, sOldProc as String
	LOCAL sSQLCmd as String, sCmd as String
*SET STEP ON 	
	bResult = .T.
	IF !DIRECTORY('.\DatosValidados') THEN
		bResult = .F.
	ELSE
		sOldCurrentDir = SYS(5) + SYS(2003)
		
		sOldSafety = SET("Safety")
		SET SAFETY OFF

		sOldProc = SET("Procedure")
		SET PROCEDURE TO 
		
		CLOSE TABLES ALL
		CLOSE DATABASES 
		
		sCmd="SET PROCEDURE TO '" + PATH_TO_COMMON_LIB + "TablesHandler' ADDITIVE"
		&sCmd

		*Crea una tabla auxiliar en donde se cargarán temporalmente los registros a validar. Esta tabla también servirá para hacer ;
		el rastreo de la operación
		IF EMPTY(sFilter) THEN
			sFilter = '.T.'
		ENDIF
		sCandidatesRSName = 'rs' + masterTableName + 'Source'
		sSQLCmd = "SELECT *,RECNO() AS RecId, SPACE(254) AS Resultado_ FROM '" + masterTableName + "' WHERE " + sFilter + " INTO CURSOR " + sCandidatesRSName + " READWRITE"
		&sSQLCmd
		
		SELECT (masterTableName)
		USE
		
		*Crea una tabla consolidada de resultados
		SET EXCLUSIVE ON
		IF VARTYPE(sSummarizeBy)='C' THEN
			IF !useTable('tbTmpConsolidadoResultadosCarga') THEN
				*La tabla consolidada aún no se ha creado
				SELECT &sSummarizeBy, SPACE(254) as TablaFuente, RECNO() AS RecId, SPACE(254) AS Resultado_ FROM ;
						(masterTablename) WHERE .F. INTO TABLE tbTmpConsolidadoResultadosCarga
				SELECT (masterTableName) 
				USE
			ENDIF
		ENDIF

		sOldExclusive = SET("Exclusive")
		bContinue = UseTable('.\DatosValidados\' + masterTableName) 
		SET EXCLUSIVE &sOldExclusive

		IF bContinue THEN
		
			SELECT (masterTableName)
			IF !bAddRecs THEN
				TRY
					ZAP
				CATCH TO oException
					bResult = .F.
				ENDTRY
			ENDIF
		
			nLoadedRecords=0
			nMasterTableRecords=RECCOUNT(masterTableName)
			nRemainingRecords=RECCOUNT(sCandidatesRSName)
			
			IF nRemainingRecords > 0 THEN

				*Fija las condiciones para activar el Buffering a nivel de la tabla espejjo masterTableName de tal modo que se pueda tener  ;
				 control en la aplicación de las reglas de validación 
				SET MULTILOCKS ON
				=CURSORSETPROP("Buffering",4,masterTableName)
			
				* Intenta agregar los registros siempre y cuando cumplan las reglas de validación de la tabla maestra
				SELECT (sCandidatesRSName) 
				nTraversedRecords = 0

				SCAN
					SCATTER MEMVAR
					
					TRY
						INSERT INTO (masterTableName) FROM MEMVAR
						nLoadedRecords = nLoadedRecords + 1
						sErrorMsg = "OK"
					CATCH TO oException
						sErrorMsg = oException.Message
						IF VARTYPE(gsErrorMsg)='C' THEN
							sErrorMsg = sErrorMsg + gsErrorMsg
							gsErrorMsg = ''
						ENDIF
					ENDTRY
					RELEASE oException
					
					bDataImportedOK = TABLEUPDATE(1,.T.,masterTableName)
					IF !bDataImportedOK 
						sErrorMsg = gsErrorMsg
						gsErrorMsg = ''
						=TABLEREVERT(.F.,masterTableName)
					ENDIF 

					SELECT (sCandidatesRSName) 
					REPLACE RESULTADO_ WITH sErrorMsg 
					
					*Agrega los campos especificados en sSummarizeBy a la tabla consolidada de resultados
					IF VARTYPE(sSummarizeBy)='C' THEN
						SCATTER MEMVAR
						m.TablaFuent = masterTableName
						INSERT INTO tbTmpConsolidadoResultadosCarga FROM MEMVAR
					ENDIF
											
					IF VARTYPE(oProgressBar)!='L' THEN
						nTraversedRecords = nTraversedRecords + 1
						oProgressBar.Value = CEILING((nTraversedRecords/nRemainingRecords)*100)
					ENDIF
				ENDSCAN
						
				*Exporta la tabla de rastreo y la tabla consolidada de resultados hacia un archivo excel
				sDataExporterFileNameAndPath = PATH_TO_COMMON_LIB + "DataExporter.Prg"
				oDataExporter=NEWOBJECT("DataExporter",sDataExporterFileNameAndPath,.null.)
				oDataExporter.sExportationPath = sTracePath
				oDataExporter.sSourceTableName = sCandidatesRSName
				oDataExporter.sExportedFileName = masterTableName + "_ResultadosCarga"
				oDataExporter.exportToXLS()
				
				oDataExporter.sSourceTableName = 'tbTmpConsolidadoResultadosCarga'
				oDataExporter.sExportedFileName = 'ConsolidadoResultadosCarga'
				oDataExporter.exportToXLS()
				
				RELEASE oDataExporter
			ENDIF
			
		ENDIF
		DO CloseTables WITH "&masterTableName,&sCandidatesRSName"

		sCmd = "SET PROCEDURE TO " + sOldProc
		&sCmd
		SET SAFETY &sOldSafety
	ENDIF
ENDPROC


PROCEDURE validateSIVIGILADataTst

	LOCAL bResult as Boolean 
	
	SET SAFETY ON
	gsErrorMsg = ''
	
SET STEP ON 		
	DO validateSIVIGILAData WITH 'PACIENTE','AÑO="2020"', .F., bResult, 'AÑO,SEMANA,COD_EVE,TIP_IDE,NUM_IDE,COD_PRE,COD_SUB', ;
								 FULLPATH('') + 'DatosValidados'
	*DO validateSIVIGILAData WITH 'EVENTOS_37','AÑO="2014"', .F., bResult, 'AÑO,SEMANA,COD_EVE,TIP_IDE,NUM_IDE,COD_PRE,COD_SUB', ;
								 FULLPATH('') + 'DatosValidados'
	RELEASE gsErrorMsg 
ENDPROC


PROCEDURE makeIniFile()
	
	*Produce un archivo de esquema -Schema.ini- tal y como se define en http://msdn.microsoft.com/en-us/library/ms709353(v=vs.85).aspx ;
	para todas las tablas que se encuentren definidas en tables.SIVIGILADataTables y tables.SIVIGILATablesStruc

	#DEFINE sCOMMON_HEADER  "ColNameHeader=True" + CR_LF + "CharacterSet=1252" + CR_LF + "Format=CSVDelimited" + CR_LF + "DateTimeFormat=mm.dd.yy" + CR_LF 

	LOCAL nSchemaFileHandler as Number
	LOCAL sTableSchema as String
	LOCAL nColCounter as Number 
	
	sSetCmd = 'SET PROCEDURE TO ' + PATH_TO_COMMON_LIB + 'TablesHandler.prg ADDITIVE'
	&sSetCmd
	
	SELECT DISTINCT DBF_NAME FROM  SIVIGILADataTables INTO CURSOR rsSIVIGILADataTables
	IF _TALLY> 0 AND useTable('SIVIGILATablesStruc') THEN

		nSchemaFileHandler = FCREATE("Schema.ini")
		SELECT rsSIVIGILADataTables
		SCAN
			sTableSchema = "[" + ALLTRIM(rsSIVIGILADataTables.DBF_NAME) + "]" + CR_LF + sCOMMON_HEADER
			
			SELECT SIVIGILATablesStruc
			nColCounter = 0
			sScanCondition = 'TABLE_NAME = "' + ALLTRIM(rsSIVIGILADataTables.DBF_NAME) + '"' 
			SCAN FOR &sScanCondition
				nColCounter = nColCounter + 1
				sTableSchema = sTableSchema + "Col" + ALLTRIM(STR(nColCounter)) + "=" + ALLTRIM(SIVIGILATablesStruc.FIELD_NAME)
				DO CASE
					CASE SIVIGILATablesStruc.FIELD_TYPE = 'C' OR SIVIGILATablesStruc.FIELD_TYPE = 'V'
						sTableSchema = sTableSchema + " Char Width " + ALLTRIM(STR(SIVIGILATablesStruc.FIELD_LEN)) 
					CASE SIVIGILATablesStruc.FIELD_TYPE = 'D'
						sTableSchema = sTableSchema + " Date "
					CASE SIVIGILATablesStruc.FIELD_TYPE = 'N'
						sTableSchema = sTableSchema + " Decimal Precision " + ALLTRIM(STR(SIVIGILATablesStruc.FIELD_LEN)) + " Scale " + ;
										ALLTRIM(STR(SIVIGILATablesStruc.FIELD_DEC))
					CASE SIVIGILATablesStruc.FIELD_TYPE = 'L'
						sTableSchema = sTableSchema + " Bit "
					CASE SIVIGILATablesStruc.FIELD_TYPE = 'M'
						sTableSchema = sTableSchema + " Char Width 254" 
				ENDCASE
				sTableSchema = sTableSchema + CR_LF 
			ENDSCAN
			=FPUTS(nSchemaFileHandler,sTableSchema)
		ENDSCAN
		FCLOSE(nSchemaFileHandler)
	ENDIF
ENDPROC


PROCEDURE makeIniFileTst()
	*SET STEP ON 
	DO makeIniFile
ENDPROC


PROCEDURE UPGD_UCIS_PostProc
	
	*Corrije un problema inducido por el actualizador KB00039: es necesario actualizar la tabla de UPGD_UCIS del cliente conservando aquellas ;
	que ya se encuentren en ella, agregando las que no tenga y actualizando las que tengan una caracterización más antigua ;
	que la que se encuentra en la tabla UPGD_UCIS de esta versión.
	 
	*Establece qué UCIs tiene el ciente y qué UCIs no

	sOldDeleted = SET("Deleted")
	DO SelectTable WITH 'UPGD_UCIS' IN PATH_TO_COMMON_LIB + 'TablesHandler'
	PACK
	USE

	USE UPGD_UCIS AGAIN ALIAS tbDestino IN 0
		
	SET DELETED ON
	*Establece que UCIs ya tiene el cliente
	SELECT UPGD_UCIS_NAL.* FROM UPGD_UCIS_NAL INNER JOIN tbDestino ON ;
		UPGD_UCIS_NAL.COD_PRE = tbDestino.COD_PRE AND UPGD_UCIS_NAL.COD_SUB = tbDestino.COD_SUB AND UPGD_UCIS_NAL.ID_UCI = tbDestino.ID_UCI ;
		INTO CURSOR rsExistentes NOFILTER 
		
	*Establece que UCIs NO tiene el cliente		
	SELECT DISTINCT UPGD_UCIS_NAL.* FROM UPGD_UCIS_NAL LEFT OUTER JOIN rsExistentes ON ;
		UPGD_UCIS_NAL.COD_PRE = rsExistentes.COD_PRE AND UPGD_UCIS_NAL.COD_SUB = rsExistentes.COD_SUB AND UPGD_UCIS_NAL.ID_UCI = rsExistentes.ID_UCI ;
		INNER JOIN EVENTOS_88 ON UPGD_UCIS_NAL.COD_PRE = EVENTOS_88.COD_PRE AND UPGD_UCIS_NAL.COD_SUB = EVENTOS_88.COD_SUB ;
		WHERE rsExistentes.COD_PRE IS NULL INTO CURSOR rsFaltantes
	
	*Agrega a la tabla del cliente las UCIs que no tenga
	SELECT tbDestino 
	TRY
		APPEND FROM DBF('rsFaltantes')
	CATCH TO oException
	ENDTRY
	
	*Actualiza en la tabla del cliente las UCI con caracterización antigua
	TRY
		UPDATE tbDestino SET tbDestino.TIPO_UCI = rsExistentes.TIPO_UCI , tbDestino.TOT_UCI = rsExistentes.TOT_UCI , ;
			tbDestino.COMP_ESPAC = rsExistentes.COMP_ESPAC , tbDestino.N_CAM_INTE = rsExistentes.N_CAM_INTE , ;
			tbDestino.SUB_TIPUCI = rsExistentes.SUB_TIPUCI , tbDestino.ACTIVA = rsExistentes.ACTIVA , tbDestino.AJUSTE = rsExistentes.AJUSTE ,;
			tbDestino.FEC_AJU = rsExistentes.FEC_AJU FROM tbDestino INNER JOIN rsExistentes ON ;
			tbDestino.COD_PRE = rsExistentes.COD_PRE AND tbDestino.COD_SUB = rsExistentes.COD_SUB AND tbDestino.ID_UCI = rsExistentes.ID_UCI ;
			WHERE tbDestino.FEC_AJU < rsExistentes.FEC_AJU
	CATCH TO oException
	ENDTRY

	DO CloseTables WITH "Eventos_88,tbDestino,UPGD_UCIS_NAL,rsExistentes,rsFaltantes"	
	SET DELETED &sOldDeleted

ENDPROC  


PROCEDURE TransformOnMigration(sSourcePath as string)

	*Aplica a la BD ubicada en sSourcePath, todas las transformaciones activas de datos que se encuentren en la tabla TransferirCampos y que;
	 deban ejecutarse cuando se está efectuando una migración (TransferirCampos!Fields("param01") IN {1,3})

	LOCAL sTransformationsTableName as String 
	sTransformationsTableName = SYS(2015)
	
	DIMENSION aTablesMapping(1)

	USE (ADDBS(sSourcePath) + "Transferircampos") IN 0 AGAIN ALIAS (sTransformationsTableName) 

	*Establece qué transformaciones deben aplicarse a la BD fuente, es decir, aquellas que aún no se han aplicado a esa BD; ellas están identificadas,;
	entre otras cosas, por ser transformaciones que existen en la tabla Transferircampos pero que NO se encuentran en la correspondiente tabla;
	de la BD fuente
	SELECT DISTINCT tborigen FROM Transferircampos WHERE activo AND (Param01=1 OR Param01=3) AND Transferircampos.ID NOT IN ;
		(SELECT ID FROM (sTransformationsTableName)) INTO ARRAY aTablesMapping
		
	IF _TALLY > 0 THEN
		nNoTransformations = _TALLY
		FOR i=1 TO nNoTransformations 
			sTablaOrigen = ALLTRIM(aTablesMapping[i,1])

			*Ejecuta todas las transformaciones definidas para la tabla sTablaOrigen que no se hayan aplicado aún en la BD fuente
			SQLClause =	"SELECT * FROM transferircampos WHERE tborigen ='" + sTablaOrigen + "' AND (Param01=1 OR Param01=3) and Activo " +;
						" AND Transferircampos.ID NOT IN (SELECT ID FROM " + sTransformationsTableName + ") INTO CURSOR rsTableMapping NOFILTER"
			&SQLClause 
			
			IF _TALLY > 0 THEN
				sOldSetTalk = SET("Talk")
				SET TALK ON
				SELECT rsTableMapping
				GO TOP
				DO WHILE !EOF() 
					sTablaOrigen = ALLTRIM(rsTableMapping.TbOrigen) 
					sTablaDestino = ALLTRIM(rsTableMapping.TbDestino) 
					sComando = ALLTRIM(rsTableMapping.Comando) 
					sCondicion =  ALLTRIM(rsTableMapping.Condicion) 
					sTablaOrigenPosCmd  = ALLTRIM(rsTableMapping.TOPosCmd)
					sTablaDestinoPreCmd = ALLTRIM(rsTableMapping.TDPreCmd) 
					sTablaDestinoPosCmd = ALLTRIM(rsTableMapping.TDPosCmd)
					nTipoCmd = rsTableMapping.Param01
					lReglasValidacion = rsTableMapping.Param02
					
					
					IF NOT lReglasValidacion THEN
						*Desactiva la validación de campos de tablas
						glDisableRules=.T.
					ENDIF
					

					IF NOT EMPTY(sTablaDestinoPreCmd ) THEN 
						nSimbolo = AT("#",sTablaDestinoPreCmd ) &&busca si el simbolo # esta en la cadena del mensaje BD
						DO WHILE nSimbolo>0
							sTablaDestinoPreCmd = STUFF(sTablaDestinoPreCmd , nSimbolo, 1, '"')     && remplaza # en sMsgMensaje, por el mensaje variable sMensaje
							nSimbolo = AT("#",sTablaDestinoPreCmd ) &&busca si el simbolo # esta en la cadena del mensaje BD
						ENDDO 
						&sTablaDestinoPreCmd 
					ENDIF

					IF NOT EMPTY(sComando ) THEN 

						IF NOT EMPTY(sCondicion) THEN
							nCadWhere = AT("where", LOWER(sCondicion))
							
							IF nCadWhere = 0 THEN
								sCondicion = "Where " +  sCondicion
							ENDIF
							sComando = sComando  + " " +  sCondicion
						ENDIF
						
						WAIT "Ejecutando: " + LEFT(sComando,50) + " ..." WINDOW NOWAIT
						&sComando 
						WAIT CLEAR 
					ENDIF
					
					IF NOT EMPTY(sTablaOrigenPosCmd) THEN 
						WAIT "Ejecutando: " + LEFT(sTablaOrigenPosCmd ,50) + " ..." WINDOW NOWAIT
						&sTablaOrigenPosCmd 
						WAIT CLEAR
					ENDIF
					IF NOT EMPTY(sTablaDestinoPosCmd) THEN 
						WAIT "Ejecutando: " + LEFT(sTablaDestinoPosCmd,50) + " ..." WINDOW NOWAIT
						&sTablaDestinoPosCmd
						WAIT CLEAR
					ENDIF
					
					IF NOT lReglasValidacion THEN
						*Activa la validación de campos de tablas
						glDisableRules=.F.
					ENDIF

					SELECT rsTableMapping	
					SKIP
				ENDDO
				
				*Actualiza en sTablaOrigen, el campo EstadoTran de los registros involucrados en la transformación, declarándolos como registros transformados
				glDisableRules=.T.
				SQLClauseNew =	"UPDATE " + sTablaOrigen  + " SET estadotran = '" + TRANSFORMATION_DONE + "' WHERE estadotran = '" + TRANSFORMATION_IN_PROGESS + "'"
				WAIT "Actualizando el estado de migración ..." WINDOW NOWAIT
				&SQLClauseNew
				WAIT CLEAR 
				glDisableRules=.F.
				
				DO CloseTables WITH 'rsTableMapping,sTransformationsTableName'
				SET TALK &sOldSetTalk 
			ENDIF
		ENDFOR 
	ENDIF
ENDPROC 



PROCEDURE TransformOnStartingOrFinish
	LPARAMETERS bStart as Boolean 
	*bStart = .T., Ejecuta todas las transformaciones activas de datos que se encuentren en la tabla TransferirCampos al inicial el aplicativo
	*bStart = .F., Ejecuta todas las transformaciones activas de datos que se encuentren en la tabla TransferirCampos al finalizar el aplicativo
	 
	IF bStart then
		SELECT * FROM Transferircampos WHERE activo AND Param01=4 INTO ARRAY aTablesMapping
	ELSE
		SELECT * FROM Transferircampos WHERE activo AND Param01=5 INTO ARRAY aTablesMapping
	endif
	
	IF _TALLY > 0 THEN
		nNoTransformations = _TALLY
		FOR i=1 TO nNoTransformations 
			sTablaOrigen = ALLTRIM(aTablesMapping[i,4])
			sTablaDestino = ALLTRIM(aTablesMapping[i,5]) 
			sComando = ALLTRIM(aTablesMapping[i,6]) 
			sCondicion =  ALLTRIM(aTablesMapping[i,7]) 
			sTablaOrigenPosCmd  = ALLTRIM(aTablesMapping[i,8])
			sTablaDestinoPreCmd = ALLTRIM(aTablesMapping[i,9]) 
			sTablaDestinoPosCmd = ALLTRIM(aTablesMapping[i,10])

			lReglasValidacion = aTablesMapping[i,12]
			
			IF NOT lReglasValidacion THEN
				*Desactiva la validación de campos de tablas
				glDisableRules=.T.
			ENDIF
				

			IF NOT EMPTY(sTablaDestinoPreCmd ) THEN 
				nSimbolo = AT("#",sTablaDestinoPreCmd ) &&busca si el simbolo # esta en la cadena del mensaje BD
				DO WHILE nSimbolo>0
					sTablaDestinoPreCmd = STUFF(sTablaDestinoPreCmd , nSimbolo, 1, '"')     && remplaza # en sMsgMensaje, por el mensaje variable sMensaje
					nSimbolo = AT("#",sTablaDestinoPreCmd ) &&busca si el simbolo # esta en la cadena del mensaje BD
				ENDDO 
				&sTablaDestinoPreCmd 
			ENDIF

			IF NOT EMPTY(sComando) THEN 

				IF NOT EMPTY(sCondicion) THEN
					nCadWhere = AT("where", LOWER(sCondicion))
					
					IF nCadWhere = 0 THEN
						sCondicion = "Where " +  sCondicion
					ENDIF
					sComando = sComando  + " " +  sCondicion
				ENDIF
				
				WAIT "Ejecutando: " + LEFT(sComando,50) + " ..." WINDOW NOWAIT
				&sComando 
				WAIT CLEAR 
			ENDIF
			IF NOT EMPTY(sTablaOrigenPosCmd) THEN 
				WAIT "Ejecutando: " + LEFT(sTablaOrigenPosCmd ,50) + " ..." WINDOW NOWAIT
				&sTablaOrigenPosCmd 
				WAIT CLEAR
			ENDIF
			IF NOT EMPTY(sTablaDestinoPosCmd) THEN 
				WAIT "Ejecutando: " + LEFT(sTablaDestinoPosCmd,50) + " ..." WINDOW NOWAIT
				&sTablaDestinoPosCmd
				WAIT CLEAR
			ENDIF
				
			IF NOT lReglasValidacion THEN
				*Activa la validación de campos de tablas
				glDisableRules=.F.
			ENDIF
		ENDFOR 	
	ENDIF
	
	
ENDPROC 


PROCEDURE TransformOnPlainsLoad(nTransformationType as Number, sPlainsFilesPath as String)

	*Ejecuta todas las transformaciones activas de datos que se encuentren en la tabla TransferirCampos y que deban ejecutarse;
	cuando se está efectuando una transferencia desde archivos planos de notificación (TransferirCampos!Fields("param01") IN {2,3})
	
	*Si se pasa el parámetro opcional nTransformationType (IN {2,3}), solo se ejecutan las transformaciones que estrictamente ;
	tengan TransferirCampos!Fields("param01") = nTransformationType). Esto permite, programar que aún cuando existan transfomraciones ;
	que deban ejecutarse tanto al cargar como al migrar (=3), se apliquen selectivamente solamente unas u otras al momento de cargar planos.

	LOCAL sFilter as String
	sFilter = '(Param01 = 2 OR Param01 = 3)'
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	
	IF VARTYPE(nTransformationType) = 'N' AND BETWEEN(nTransformationType,2,3) THEN 
		sFilter = 'Param01 = ' + ALLTRIM(STR(nTransformationType))
	ENDIF
	
	IF VARTYPE(sPlainsFilesPath) = 'C' AND !EMPTY(sPlainsFilesPath) THEN 
		sPlainsFilesPath = ADDBS(sPlainsFilesPath)
	ELSE 
		sPlainsFilesPath = ''
	ENDIF 
	
	
	SQLClause =	"SELECT * FROM transferircampos WHERE " +  sFilter + " and Activo INTO ARRAY aTransformations"
	&SQLClause 
	IF _TALLY > 0 THEN
		nNoItems = _TALLY
		FOR i=1 TO nNoItems
			sTablaOrigen = ALLTRIM(aTransformations[i,4])
			sTablaDestino = ALLTRIM(aTransformations[i,5])
			sComando = ALLTRIM(aTransformations[i,6])
			sCondicion =  ALLTRIM(aTransformations[i,7])
			sTablaOrigenPosCmd  = ALLTRIM(aTransformations[i,8])
			sTablaDestinoPreCmd = ALLTRIM(aTransformations[i,9])
			sTablaDestinoPosCmd = ALLTRIM(aTransformations[i,10])
			nTipoCmd = aTransformations[i,11]
			lReglasValidacion = aTransformations[i,12]
			
			
			IF NOT lReglasValidacion THEN
				*Desactiva la validación de campos de tablas
				glDisableRules=.T.
			ENDIF
			
			*Garantiza que las tablas origen y destino se encuentren disponibles para las operaciones que se van a efectuar con ellas
			=SelectTable(sPlainsFilesPath + sTablaOrigen,'',.F. )
			=SelectTable(sPlainsFilesPath + sTablaDestino,'',.F. )
			
			IF NOT EMPTY(sTablaDestinoPreCmd ) THEN
				nSimbolo = AT("#",sTablaDestinoPreCmd) &&busca si el simbolo # esta en la cadena del mensaje BD
				DO WHILE nSimbolo>0
					sTablaDestinoPreCmd = STUFF(sTablaDestinoPreCmd , nSimbolo, 1, '"')     && remplasa # en sMsgMensaje, por el mensaje variable sMensaje
					nSimbolo = AT("#",sTablaDestinoPreCmd) &&busca si el simbolo # esta en la cadena del mensaje BD
				ENDDO 
				WAIT "Ejecutando: " + LEFT(sTablaDestinoPreCmd,50) + " ..." WINDOW NOWAIT
				&sTablaDestinoPreCmd 
				WAIT CLEAR
			ENDIF
			DOEVENTS 
			
			IF NOT EMPTY(sComando ) THEN 
				IF NOT EMPTY(sCondicion) THEN
					nCadWhere = AT("where", LOWER(sCondicion))
					IF nCadWhere = 0 THEN
						sCondicion = "Where " +  sCondicion
					ENDIF
					sComando = sComando  + " " +  sCondicion
				ENDIF
				
				WAIT "Ejecutando: " + LEFT(sComando,50) + " ..." WINDOW NOWAIT
				&sComando
				WAIT CLEAR
			ENDIF

			IF NOT EMPTY(sTablaOrigenPosCmd) THEN 
				WAIT "Ejecutando: " + LEFT(sTablaOrigenPosCmd ,50) + " ..." WINDOW NOWAIT
				&sTablaOrigenPosCmd 
				WAIT CLEAR
			ENDIF

			IF NOT EMPTY(sTablaDestinoPosCmd) THEN 
				WAIT "Ejecutando: " + LEFT(sTablaDestinoPosCmd,50) + " ..." WINDOW NOWAIT
				&sTablaDestinoPosCmd
				WAIT CLEAR
			ENDIF
			
			IF NOT lReglasValidacion THEN
				*Activa la validación de campos de tablas
				glDisableRules=.F.
			ENDIF

			*Actualiza en sTablaOrigen, el campo EstadoTran, si es que existe, de los registros involucrados en la transformación,;
			declarándolos como registros transformados
			glDisableRules=.T.
			TRY 
				SQLClauseNew =	"UPDATE " + sTablaOrigen  + " SET estadotran = '" + TRANSFORMATION_DONE + "' WHERE estadotran = '" + TRANSFORMATION_IN_PROGESS + "'"
				WAIT "Actualizando el estado de migración ..." WINDOW NOWAIT
				&SQLClauseNew
			CATCH TO oException
			ENDTRY 
			WAIT CLEAR 
			glDisableRules=.F.
		ENDFOR
	ENDIF	
ENDPROC 



****************************************************************************************************************************************
*La siguiente parte de código fue implementada con motivo de la aparición de la versión 2018 específicamente para satisfacer la necesidad;
de migrar los casos del evento 307 hacia casos de uno de los eventos Rabia humana (Cod. 670) o Agresiones por animales potencialmente transmisores ;
de Rabia (Cod. 300)

#DEFINE SOURCE_EVENT_CODE "307"

PROCEDURE adjust_307_Cases()

	*Ajusta casos del evento 307 en la forma usual con el objeto de tener una linea base de registros cuya clasificación final indique;
	si un caso corresponde, luego de procesamiento de ajustes, al evento Rabia humana o al evento Agresiones por animales...;
	Esta línea base corresponde básicamente a la que se obtendría si se produjera un archivo XLs del evento 307 y queda disponible en un;
	recordset de nombre rsAdjustedCases 

	LOCAL sOldTalk as String

	sSetProc = 'SET PROCEDURE TO ' + PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAMessenger ADDITIVE'
	&sSetProc

	sOldTalk = SET("Talk")
	SET TALK ON
		
	=showProgressMessage('Procesando ajustes del evento 307')

	sCDTable = 'Eventos_64' 
	*Tabla de datos complementarios para el evento 307

	*Establece los campos de datos básicos que se deben extraer. Los campos son renombrados en donde quiera que sea neceario para evitar;
	posibles duplicidades de nombre con campos que se deben extraer de otras tablas distintas a PACIENTE
	sIndividualDataSQL='SELECT A.cod_eve AS COD_EVE_, A.fec_not, A.semana AS SEMANA_, A.año AS AÑO_, A.cod_pre AS COD_PRE_, A.cod_sub AS COD_SUB_,' + ;
						' A.pri_nom AS pri_nom_, A.seg_nom AS seg_nom_, A.pri_ape AS pri_ape_, A.seg_ape AS seg_ape_,' + ;
						' A.tip_ide AS tip_ide_, A.num_ide AS num_ide_, A.edad AS edad_, A.uni_med AS uni_med_,' + ;
						' A.sexo AS sexo_, ' + ;
						' LEFT(A.cod_mun,2) AS cod_dpto_o, RIGHT(A.cod_mun,3) AS cod_mun_o, A.area AS area_,' + ;
						' A.localidad AS localidad_, A.cen_poblad AS cen_pobla_, A.vereda AS vereda_,' + ;
						' A.bar_ver AS bar_ver_, A.dir_res AS dir_res_, A.ocupacion AS ocupacion_,' + ;
						' A.tip_ss AS tip_ss_, A.cod_ase AS cod_ase_, A.per_etn AS per_etn_, A.estrato AS estrato_,' + ;
						' A.GP_DISCAPA, A.GP_DESPLAZ, A.GP_MIGRANT, A.GP_CARCELA,' + ;
						' A.GP_GESTAN, A.SEM_GES as SEM_GES_, A.GP_INDIGEN, A.GP_POBICFB AS GP_POBICBF, A.GP_MAD_COM,' + ;
						' A.GP_DESMOVI, A.GP_PSIQUIA, A.GP_VIC_VIO, A.GP_OTROS, A.FUENTE AS FUENTE_,' + ;
						' LEFT(A.mun_pro,2) AS cod_dpto_r,' + ;
						' RIGHT(A.mun_pro,3) AS cod_mun_r, A.fec_con AS fec_con_, A.ini_sin AS ini_sin_,' + ;
						' A.tip_cas AS tip_cas_, A.pac_hos AS pac_hos_,' + ;
						' A.fec_hos AS fec_hos_, A.con_fin AS con_fin_, A.fec_def AS fec_def_, A.ajuste AS ajuste_,' + ;
						' A.telefono AS telefono_, A.fecha_nto AS fecha_nto_,' + ;
						' A.cer_def AS cer_def_, A.cbmte AS cbmte_, A.UNI_MODIF, L.raz_soc AS nuni_modif,' + ;
						' A.nom_dil_fi AS nom_dil_f_,' + ;
						' A.tel_dil_fi AS tel_dil_f_, A.fec_aju AS fec_aju_, A.FM_FUERZA, A.FM_UNIDAD, A.FM_GRADO, A.Version'
						
	sComplementaryDataSQL = 'H.*'

	sFROM_SQL =	' FROM Paciente A  LEFT OUTER JOIN UPGD L ON A.UNI_MODIF == L.cod_pre + L.cod_sub '

	* Agrega las columnas de la tabla sCDTable a la consulta de extracción
	sFROM_SQL =	sFROM_SQL + '  LEFT OUTER JOIN (SELECT * FROM ' + sCDTable + ' ORDER BY FEC_AJU) AS H ON A.cod_eve = H.cod_eve AND' +;
		' A.año = H.año AND A.semana = H.semana AND ' +;
		' A.tip_ide = H.tip_ide AND A.num_ide = H.num_ide  AND ' + ;
		' A.cod_pre = H.cod_pre AND A.cod_sub = H.cod_sub '
				
	XlsQuery =  sIndividualDataSQL  + "," + sComplementaryDataSQL + sFROM_SQL 
					

	*Establece las condiciones de filtrado para la extracción de registros
	sWHERE_SQL =' WHERE A.COD_EVE ="' + SOURCE_EVENT_CODE + '"'

	XlsQuery =  XlsQuery + sWHERE_SQL + ' INTO CURSOR rsSourceRecords READWRITE'

	* Se ejecuta el Query que extrae los datos de paciente y complementarios para el evento 307
	*=STRTOFILE(XlsQuery, "rs_fuente.qpr")
	&XlsQuery 

	=showProgressMessage('Procesando ' + STR(RECCOUNT('rsSourceRecords')) + ' registros de PACIENTE')

	&&?'Inicio: ' + TIME()
	IF _TALLY > 0
		* se encontraron registros. Se procesan  entonces los ajustes que quedarán en un cursor de nombre rsAdjustedCases;

		oAdjustmentsProcessor = NEWOBJECT("AdjustmentsProcessor","AdjustmentsProcessor.PRG")
		oAdjustmentsProcessor.sSourceTablename = 'rsSourceRecords'
		oAdjustmentsProcessor.sAdjustmentFieldName = 'AJUSTE_'
		oAdjustmentsProcessor.sAdjustmentDateFieldName = 'FEC_AJU_'
		oAdjustmentsProcessor.processAdjustments('AÑO_+SEMANA_+COD_EVE_+TIP_IDE_+NUM_IDE_+COD_PRE_+COD_SUB_')
		RELEASE oAdjustmentsProcessor
		
		SELECT rsAdjustedCases

	ENDIF
	
	SET TALK &sOldTalk
	=hideProgressMessage()
ENDPROC


PROCEDURE transfer_307_ToTargetTables()

	*A partir de la línea base de registros (rsAdjustedCases) construida por el PROC adjustCases, migra los registros de la tabla ;
	fuente del evento 307 hacia las tablas de datos complementarios de uno de los evento 670 (EVENTOS_35) o 300(EVENTOS_05)
	
	LOCAL sOldTalk as String

	sSetProc = 'SET PROCEDURE TO ' + PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAMessenger ADDITIVE'
	&sSetProc

	sSetProc = 'SET PROCEDURE TO ' + PATH_TO_COMMON_LIB + '\TablesHandler ADDITIVE'
	&sSetProc

	sOldTalk = SET("Talk")
	SET TALK ON
	
	glDisableRules = .T.
	
	IF SelectTable('rsAdjustedCases',,.F.) THEN 
		=showProgressMessage('Discriminando casos de rabia y de agresiones')
	
		*Establece cuáles son los casos de rabia humana supuestamente "confirmados"
		SELECT * FROM rsAdjustedCases WHERE CLAS_NOTIF='2' INTO CURSOR rsCasosAjustadosRabia NOFILTER
		
		*Establece cuáles de los supúestos casos confirmados corresponden a casos que involucran algún registro del evento 300 (agresiones...)
		SELECT Eventos_64.* FROM eventos_64 INNER JOIN Rscasosajustadosrabia ;
		   ON ( ( ( ( (  Rscasosajustadosrabia.cod_eve_ = Eventos_64.cod_eve;
		   AND  Rscasosajustadosrabia.semana_ = Eventos_64.semana );
		   AND  Rscasosajustadosrabia.año_ = Eventos_64.año );
		   AND  Rscasosajustadosrabia.cod_pre_ = Eventos_64.cod_pre );
		   AND  Rscasosajustadosrabia.cod_sub_ = Eventos_64.cod_sub );
		   AND  Rscasosajustadosrabia.tip_ide_ = Eventos_64.tip_ide );
		   AND  Rscasosajustadosrabia.num_ide_ = Eventos_64.num_ide;
			WHERE  Eventos_64.clas_notif = "1"	INTO CURSOR rsCasosAgresiones NOFILTER
			
		*Establece cuales son los casos que realmente son de rabia humana en cuanto se encuentran en el conjunto de casos ajustados de rabia ;
		y no se encuentran en el conjunto de casos de agresiones recién establecido. Estos casos corresponderán entonces a los que NO tienen;
		ningún registro de agresiones asociado o, desde otro punto de vista, solamente incluyen registros en donde el valor del campo ;
		CLAS_NOTIF es '2'
		SELECT Eventos_64.*	 FROM eventos_64 INNER JOIN Rscasosajustadosrabia ;
			   ON ( ( ( ( (  Rscasosajustadosrabia.cod_eve_ = Eventos_64.cod_eve;
			   AND  Rscasosajustadosrabia.semana_ = Eventos_64.semana );
			   AND  Rscasosajustadosrabia.año_ = Eventos_64.año );
			   AND  Rscasosajustadosrabia.cod_pre_ = Eventos_64.cod_pre );
			   AND  Rscasosajustadosrabia.cod_sub_ = Eventos_64.cod_sub );
			   AND  Rscasosajustadosrabia.tip_ide_ = Eventos_64.tip_ide );
			   AND  Rscasosajustadosrabia.num_ide_ = Eventos_64.num_ide ;
			    LEFT OUTER JOIN rsCasosAgresiones Rscasosagresiones ;
			   ON  Rscasosagresiones.cod_eve = Eventos_64.cod_eve;
			   AND  Eventos_64.semana = Rscasosagresiones.semana;
			   AND  Eventos_64.año = Rscasosagresiones.año;
			   AND  Eventos_64.cod_pre = Rscasosagresiones.cod_pre;
			   AND  Eventos_64.cod_sub = Rscasosagresiones.cod_sub;
			   AND  Eventos_64.tip_ide = Rscasosagresiones.tip_ide;
			   AND  Eventos_64.num_ide = Rscasosagresiones.num_ide;
			 WHERE  Rscasosagresiones.cod_eve IS NULL INTO CURSOR rsCasosConfirmadosRabia NOFILTER 
			 
		*Establece cuáles son los registros que se se deben agregar a la tabla del evento 670 - Rabia humana
		SELECT Eventos_64.* FROM Eventos_64 INNER JOIN rsCasosConfirmadosRabia rsCasosConfirmadosRabia ;
			   ON ( ( ( ( (  rsCasosConfirmadosRabia.cod_eve = Eventos_64.cod_eve;
			   AND  rsCasosConfirmadosRabia.semana = Eventos_64.semana );
			   AND  rsCasosConfirmadosRabia.año = Eventos_64.año );
			   AND  rsCasosConfirmadosRabia.cod_pre = Eventos_64.cod_pre );
			   AND  rsCasosConfirmadosRabia.cod_sub = Eventos_64.cod_sub );
			   AND  rsCasosConfirmadosRabia.tip_ide = Eventos_64.tip_ide );
			   AND  rsCasosConfirmadosRabia.num_ide = Eventos_64.num_ide INTO CURSOR rsPorAgregar670 READWRITE
		
		*Establece cuáles son los registros que se se deben agregar a la tabla del evento 300 - Agresiones...
		SELECT Eventos_64.* FROM Eventos_64 LEFT OUTER JOIN rsCasosConfirmadosRabia rsCasosConfirmadosRabia ;
			   ON ( ( ( ( (  rsCasosConfirmadosRabia.cod_eve = Eventos_64.cod_eve;
			   AND  rsCasosConfirmadosRabia.semana = Eventos_64.semana );
			   AND  rsCasosConfirmadosRabia.año = Eventos_64.año );
			   AND  rsCasosConfirmadosRabia.cod_pre = Eventos_64.cod_pre );
			   AND  rsCasosConfirmadosRabia.cod_sub = Eventos_64.cod_sub );
			   AND  rsCasosConfirmadosRabia.tip_ide = Eventos_64.tip_ide );
			   AND  rsCasosConfirmadosRabia.num_ide = Eventos_64.num_ide ;
			   WHERE rsCasosConfirmadosRabia.cod_eve is null INTO CURSOR rsPorAgregar300 READWRITE
		
		*Agrega los registros a las tablas destino
		=showProgressMessage('Agregando casos de rabia')
		=SelectTable('EVENTOS_35',,.F.)
		APPEND FROM DBF('rsPorAgregar670')
		REPLACE COD_EVE WITH '670' FOR COD_EVE='307'

		SELECT DISTINCT PACIENTE.* FROM PACIENTE INNER JOIN rsPorAgregar670 ON ;
			rsPorAgregar670.cod_eve = PACIENTE.cod_eve ;
		   AND  rsPorAgregar670.semana = PACIENTE.semana ;
		   AND  rsPorAgregar670.año = PACIENTE.año ;
		   AND  rsPorAgregar670.cod_pre = PACIENTE.cod_pre ;
		   AND  rsPorAgregar670.cod_sub = PACIENTE.cod_sub ;
		   AND  rsPorAgregar670.tip_ide = PACIENTE.tip_ide ;
		   AND  rsPorAgregar670.num_ide = PACIENTE.num_ide INTO CURSOR rsDBPorAgregar670 READWRITE
		REPLACE COD_EVE WITH '670' ALL
		INSERT INTO PACIENTE SELECT * FROM rsDBPorAgregar670 
		
		=showProgressMessage('Agregando casos de agresiones')
		=SelectTable('EVENTOS_05',,.F.)
		APPEND FROM DBF('rsPorAgregar300')
		REPLACE COD_EVE WITH '300' FOR COD_EVE='307'

		SELECT DISTINCT PACIENTE.* FROM PACIENTE INNER JOIN rsPorAgregar300 ON ;
			rsPorAgregar300.cod_eve = PACIENTE.cod_eve;
		   AND  rsPorAgregar300.semana = PACIENTE.semana ;
		   AND  rsPorAgregar300.año = PACIENTE.año ;
		   AND  rsPorAgregar300.cod_pre = PACIENTE.cod_pre ;
		   AND  rsPorAgregar300.cod_sub = PACIENTE.cod_sub ;
		   AND  rsPorAgregar300.tip_ide = PACIENTE.tip_ide ;
		   AND  rsPorAgregar300.num_ide = PACIENTE.num_ide INTO CURSOR rsDBPorAgregar300 READWRITE
		REPLACE COD_EVE WITH '300' ALL
		INSERT INTO PACIENTE SELECT * FROM rsDBPorAgregar300 

		DO CloseTables WITH 'rsPorAgregar670,rsPorAgregar300,rsCasosConfirmadosRabia,rsCasosAjustadosRabia,rsCasosAgresiones,rsCasosAjustadosRabia,' +;
							'rsDBPorAgregar670,rsDBPorAgregar300' IN (PATH_TO_COMMON_LIB + 'TablesHandler')
			
		SET TALK &sOldTalk
		=hideProgressMessage()
	ENDIF
ENDPROC



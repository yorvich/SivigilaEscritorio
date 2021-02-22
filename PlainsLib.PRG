#INCLUDE SIVIGILA.h
#INCLUDE GlobalConst.h

#DEFINE GARBAGE_CHARS "+.\/:*?',{}<>_&;^()=#$%[]" + '"' 

PROCEDURE exportForInsurers

	LPARAMETERS sExportationPath as String, sEntityKind as Byte , sEntityCode as string, sNotifierKind as String,  sNotifierId as String, ;
				sNotificationWeek as String, sNotificationYear AS Integer, nSivigilaEvents as Byte, sInsurerCode as string, ;
				bBeSilent as Boolean, sSIVIGILAVersion as String
	
	*sEntityKind :  1-->Pais, 2-->Depto


	PUBLIC sInsurerPlainsFilePath AS String

	DIMENSION aPlainTableKeys(1)
	* Contiene los campos que conforman las claves de las tablas suceptibles de ser notificadas por;
	el sistema.
	*Estas claves son idénticas tanto para las tablas del sitema como para las tablas PLANOS_... que;
	son utilizadas en los procesos de notificación/consolidación del Sistema ;
	son utilizadas en el proceso de notificación

	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
	
	SET TALK ON
	DO cleanTheHouse IN IMPORTAR_CLICK
	
	sInitTime=TIME()
	DO initPlainTableKeysArray WITH aPlainTableKeys IN importar_click

	sInsurerPlainsFilePath = "\RETROALIMENTACION A ASEGURADORAS"
	sInsurerPlainsFilePath = sExportationPath + sInsurerPlainsFilePath
	sInsurerPlainsFilePath = sInsurerPlainsFilePath + "_" + STRTRAN(TTOC(DATETIME(),3),":","_")
	
	*Establece las condiciones de filtrado que deben cumplirse para exportar la información
	sDynamicConditions = "RecordSource.AÑO='" + IIF(sNotificationYear=0,'',ALLTRIM(STR(sNotificationYear))) + "'"
	IF sInsurerCode != 'TODAS' AND !EMPTY(sInsurerCode) THEN
		sDynamicConditions = sDynamicConditions  + " AND RecordSource.COD_ASE=='" + sInsurerCode + "'"
	ENDIF
	IF sEntityKind = 2 THEN
		sDynamicConditions = sDynamicConditions  + " AND LEFT(RecordSource.COD_MUN,2)='" + sEntityCode + "'"
	ENDIF


	*Crea la tabla PLANO en donde se almacenarán en forma temporal los datos a exportar
	DO CreateExportationPlainTable

	SELECT PACIENTE
	
	*Establece a qué aseguradoras debe exportárseles información excluyendo las notificaciones negativas
	sSQLClause= 'SELECT DISTINCT RecordSource.COD_ASE, ASEGURADORAS.NOM_ASE '
	IF sEntityKind = 2 THEN
		sSQLClause = sSQLClause + ', DEPTOS.NOM_DEP '
	ENDIF
	sSQLClause = sSQLClause + ' FROM PACIENTE AS RecordSource '
	IF sInsurerCode = 'TODAS' OR !EMPTY(sInsurerCode) THEN
		sSQLClause = sSQLClause + ' LEFT OUTER JOIN '
	ELSE
		sSQLClause = sSQLClause + ' INNER JOIN '
	ENDIF
	sSQLClause = sSQLClause + ' ASEGURADORAS ON RecordSource.COD_ASE=ASEGURADORAS.COD_ASE'

	IF sEntityKind = 2 THEN
		sSQLClause = sSQLClause + ' LEFT OUTER JOIN DEPTOS ON LEFT(RecordSource.cod_mun,2) = DEPTOS.cod_dep '
	ENDIF
	sSQLClause = sSQLClause + " WHERE RecordSource.COD_EVE NOT IN (SELECT TOP 1 '000' AS CodNotificacionNegativa FROM EVENTOS ORDER BY COD_EVE) " +;
				" AND !EMPTY(RecordSource.COD_ASE) AND ASEGURADORAS.RETROALIME AND " + sDynamicConditions + " INTO CURSOR cTargetInsurers"
	
	*La siguiente condición que obligaba a que se exportase información correspondiente;
	a UPGDs activas en el Sistema, fu desactivada por solicitud de Marcela Benavides del INS;
	el 15/07/2010	
	&&			 " AND (recordSource.cod_pre + " + "recordSource.cod_sub" + ;
	&&			 " IN (SELECT DISTINCT cod_pre+cod_sub AS CodUPGD FROM UPGD WHERE ACT_SIV = 1))" 
	&sSQLClause

	IF _TALLY>0 THEN
		IF bBeSilent THEN
			sResultsFileName="ResultadosRetroAseguradoras_" + ALLTRIM(CHRTRAN(DTOC(DATE()),"/","")) + ".txt"
			nResultsFileHandler=FCREATE(sExportationPath + '\' + sResultsFileName)
		ENDIF
		nWinrarBatFileHandler=FCREATE(sExportationPath + '\' + "GenerarZipsConPasswords_" + ALLTRIM(CHRTRAN(DTOC(DATE()),"/","")) + ".bat")

		SELECT cTargetInsurers
		sFormerInsurerPlainsFilePath  = sInsurerPlainsFilePath 
		sFormersDynamicConditions = sDynamicConditions
		SCAN
			*Crea la carpeta en donde se almacenarán los archivos resultado de la exportación
			IF sEntityKind = 1 THEN
				sEntityName='PAIS'
			ELSE
				sEntityName=ALLTRIM(cTargetInsurers.NOM_DEP)
			ENDIF

			sInsurerName = CHRTRAN(ALLTRIM(cTargetInsurers.NOM_ASE), specialChars, '') + '_' + ALLTRIM(cTargetInsurers.COD_ASE)
			sInsurerName = CHRTRAN(CHRTRAN(CHRTRAN(CHRTRAN(CHRTRAN(CHRTRAN(sInsurerName,'Á','A'),'É','E'),'Í','I'),'Ó','O'),'Ú','U'),'Ñ','N')
			IF ISNULL(sInsurerName) THEN
				sInsurerPlainsFilePath = sFormerInsurerPlainsFilePath + '\' + sEntityName + '\' + 'DESCONOCIDA' + "_" + ALLTRIM(cTargetInsurers.COD_ASE)
				sInsurerName='DESCONOCIDA' + cTargetInsurers.COD_ASE
			ELSE
				sInsurerPlainsFilePath = sFormerInsurerPlainsFilePath + '\' + sEntityName + '\' + sInsurerName 
			ENDIF
			IF !DIRECTORY(sInsurerPlainsFilePath)
				MKDIR (sInsurerPlainsFilePath)
			ENDIF

			*Fija los nombres de los archivos planos que almacenarán la información exportada desde las tablas del Sistema
			sUPGDsFileNameAndPath = ''
			sUCIsFileNameAndPath = ''
			sHumanResourcesFileNameAndPath = ''
			sPatientsFileNameAndPath = ''
			sEventsDataFileNameAndPath = ''
			sCollectiveEventsFileNameAndPath = ''
			sTotalsFileNameAndPath = ''
			sControlFileNameAndPath = ''
			sSIVIGILAFileNameAndPath = ''
			*A partir de la versión 2012 ya no se genera el archivo W
			sWEBPatientsFileNameAndPath=''
			sIRAFileNameAndPath = ''
			sNotifierFileNamePrefix=''
			DO setOutputTxtFileNames WITH sNotifierKind, sNotifierId, sNotificationWeek, sInsurerPlainsFilePath, sNotifierFileNamePrefix,;
						sUPGDsFileNameAndPath, sHumanResourcesFileNameAndPath, sPatientsFileNameAndPath, ;
						sEventsDataFileNameAndPath, sCollectiveEventsFileNameAndPath, sTotalsFileNameAndPath ,;
						sControlFileNameAndPath, sSIVIGILAFileNameAndPath, sWEBPatientsFileNameAndPath ,;
						sIRAFileNameAndPath, sUCIsFileNameAndPath



			* Genera el archivo N de datos básicos de notificación individual periódica semanal
			SELECT PLANO
			ZAP

			=showProgressMessage('Generando Archivo Plano de Notificación Individual para ' + sInsurerName)
			sDynamicConditions = sFormersDynamicConditions + " AND RecordSource.COD_ASE=='" + cTargetInsurers.COD_ASE + "'" + ;
								 " AND RecordSource.COD_EVE!='000' "
			*La siguiente condición que obligaba a que se exportase información correspondiente;
			a UPGDs activas en el Sistema, fue desactivada por solicitud de Marcela Benavides del INS;
			el 15/07/2010	
			&&					 
			&&					  AND (recordSource.cod_pre + " + "recordSource.cod_sub" + ;
			&&					 " IN (SELECT DISTINCT cod_pre+cod_sub AS CodUPGD FROM UPGD WHERE ACT_SIV = 1))" 

			exportationIsOK = ExportFromQRY('PTE', sDynamicConditions, , sInsurerPlainsFilePath, .F., , , .T.) 
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
								'Existen registros duplicados en la fuente de notificación individual.' + CHR(13) + ;
								'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
								'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sInsurerPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ELSE
					sResultMsg='Existen registros duplicados en la fuente de notificación individual para la aseguradora ' + sInsurerName 
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF
			SELECT PLANO
			COPY TO (sPatientsFileNameAndPath ) SDF
			nNExportedRegs=_TALLY

			*Establece una línea base de pacientes para los cuales se exportarán los registros de datos complementarios, laboratorios, UPGDs;
			y talento humano.
			sSQLClause= 'SELECT DISTINCT RecordSource.AÑO, RecordSource.SEMANA, RecordSource.COD_EVE, RecordSource.TIP_IDE, RecordSource.NUM_IDE, ';
						+ 'RecordSource.COD_PRE, RecordSource.COD_SUB FROM PACIENTE AS RecordSource ';
						+ " WHERE  " + sDynamicConditions +	" INTO CURSOR cPatientsBaseLine NOFILTER"
			&sSQLClause

			* Genera el archivo D de datos complementarios de notificación periódica semanal obligatoria;
			incluyendo la información de resultados de laboratorio
			SELECT PLANO
			ZAP

			sEventKeyExpression = ASCAN(aPlainTableKeys, 'PLANO_EXX', -1, -1 ,-1, 15)
			sEventKeyWithCommas = ALLTRIM(aPlainTableKeys(sEventKeyExpression ,2))
			sEventKeyWithPlus = 'RecordSource.' + STRTRAN(sEventKeyWithCommas, ', ' , '+RecordSource.')
			sPatientsBaseLineKeyWithPlus = 'cPatientsBaseLine.' + STRTRAN(sEventKeyWithCommas, ', ' , '+cPatientsBaseLine.')

			*Establece el conjunto de tablas de eventos presentes en la línea base de pacientes para los cuales se exportarán los ;
			registros de datos complementarios.
			SELECT DISTINCT INT(VAL(Eventos.Formulario)) AS eventToExport FROM Eventos WHERE  !EMPTY(Eventos.formulario) ;
				AND Eventos.COD_EVE IN (SELECT DISTINCT cPatientsBaseLine.COD_EVE FROM cPatientsBaseLine ) ;
				INTO CURSOR cEventTablesToExport
			nEventTablesToExport=_TALLY

			IF nEventTablesToExport>0 THEN
				SELECT cEventTablesToExport
				SCAN
					IF eventToExport != 23 AND eventToExport != 31 AND eventToExport != 32 AND eventToExport != 43 AND eventToExport != 94 THEN
						* Estos eventos no tienen todos los atributos del conjunto {AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB}
						
						=showProgressMessage('Generando Archivo Plano del Grupo de Eventos ' + PADL(eventToExport ,2,'0') + ' para ' + sInsurerName)
						sEventTableName='EVENTOS_' + PADL(eventToExport ,2,'0')
						IF eventToExport != 9 THEN
							exportationIsOK = ExportFromQRY('E'+PADL(eventToExport ,2,'0'), sEventKeyWithPlus + ;
															' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', , +;
															sInsurerPlainsFilePath, .F., , , .T.)
						ELSE
							* El evento 9 requiere tratamiento especial para lograr tiempos óptimos de procesamiento						
							SET PROCEDURE TO SivigilaUtilities ADDITIVE
							sOptimizedQuery=GetQuerySQLClause("eventos_09_Aseguradoras.qpr")
							exportationIsOK = ExportFromQRY('E'+PADL(eventToExport ,2,'0'), ,, sInsurerPlainsFilePath, .F., ;
															 "," , , .T., sOptimizedQuery)
						ENDIF
						
						IF !exportationIsOK THEN
							IF !bBeSilent THEN					
								MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
											'Existen registros duplicados en la fuente de datos complementarios.' + CHR(13) + ;
											'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
											'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sInsurerPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
							ELSE
								sResultMsg='Existen registros duplicados en la fuente de datos complementarios para ' + sInsurerName + ;
											+ ' - Tabla ' + sEventTableName
								=FPUTS(nResultsFileHandler,sResultMsg)
							ENDIF
						ENDIF
					ENDIF
					SELECT cEventTablesToExport
				ENDSCAN
			ENDIF
			
			=showProgressMessage('Generando Archivo Plano de Laboratorios para ' + sInsurerName)
			exportationIsOK = ExportFromQRY('LAB', sEventKeyWithPlus + ' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)',, ;
											sInsurerPlainsFilePath, .F., , , .T.)
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
								'Existen registros duplicados en la fuente de notificación laboratorios.' + CHR(13) + ;
								'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
								'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sInsurerPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ELSE
					sResultMsg='Existen registros duplicados en la fuente de notificación laboratorios para ' + sInsurerName 
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF

			=showProgressMessage('Generando Archivo Plano de Contactos para ' + sInsurerName)
			exportationIsOK = ExportFromQRY('CNTA', 'TIPIDE_POS+ALLTRIM(NUMIDE_POS)+COD_EVEPOS IN (SELECT TIP_IDE+ALLTRIM(NUM_IDE)+COD_EVE FROM cPatientsBaseLine)', , ;
											sInsurerPlainsFilePath, .F., , , .T.)
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
								'Existen registros duplicados en la fuente de notificación contactos.' + CHR(13) + ;
								'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
								'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sInsurerPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ELSE
					sResultMsg='Existen registros duplicados en la fuente de notificación contactos para ' + sInsurerName 
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF

			=showProgressMessage('Generando Archivo Plano de Seguimiento a Contactos para ' + sInsurerName)
			
			*La siguiente exportación hase uso del cursor C_CNTA construido por ExportFromQRY cuando generó el plano de contactos
			exportationIsOK = ExportFromQRY('SGCN', 'ID_PERSONA IN (SELECT ID FROM C_CNTA)', , sInsurerPlainsFilePath, .F., , , .T.)
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
								'Existen registros duplicados en la fuente de notificación seguimiento a contactos.' + CHR(13) + ;
								'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
								'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sInsurerPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ELSE
					sResultMsg='Existen registros duplicados en la fuente de notificación seguimiento a contactos para ' + sInsurerName 
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF

			SELECT PLANO
			COPY TO (sEventsDataFileNameAndPath) SDF
			nDExportedRegs=_TALLY


			* Genera el archivo U contenedor de datos de caracterización de UPGD
			SELECT PLANO
			ZAP

			sUPGDKeyExpression = ASCAN(aPlainTableKeys, 'PLANO_UPGD', -1, -1 ,-1, 15)
			sUPGDKeyWithCommas = aPlainTableKeys(sUPGDKeyExpression,2)
			sUPGDKeyWithPlus = 'RecordSource.' + STRTRAN(sUPGDKeyWithCommas, ', ' , '+RecordSource.')
			sPatientsBaseLineKeyWithPlus = 'cPatientsBaseLine.' + STRTRAN(sUPGDKeyWithCommas, ', ' , '+cPatientsBaseLine.')

			=showProgressMessage('Generando Archivo Plano de las UPGD para ' + sInsurerName)
			exportationIsOK = ExportFromQRY('UPGD' , sUPGDKeyWithPlus + ' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', , ;
											 sInsurerPlainsFilePath, .F., , , .T.)
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
								'Existen registros duplicados en la fuente de UPGDs.' + CHR(13) + ;
								'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
								'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sInsurerPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ELSE
					sResultMsg='Existen registros duplicados en la fuente de UPGDs para ' + sInsurerName 
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF
			SELECT PLANO
			COPY TO (sUPGDsFileNameAndPath) SDF
			nUExportedRegs=_TALLY


			* Genera el archivo V contenedor de datos de caracterización de UCI en las UPGD
			SELECT PLANO
			ZAP

			=showProgressMessage('Generando Archivo Plano de caracterización de UCI para ' + sInsurerName)
			exportationIsOK = ExportFromQRY('UCI' , sUPGDKeyWithPlus + ;
											' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', , sInsurerPlainsFilePath, .F., , , .T.)
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
								'Existen registros duplicados en la fuente de caracterización de UCI.' + CHR(13) + ;
								'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
								'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sInsurerPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ELSE
					sResultMsg='Existen registros duplicados en la fuente  de caracterización de UCI ' + sInsurerName 
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF
			SELECT PLANO
			COPY TO (sUCIsFileNameAndPath) SDF
			nVExportedRegs = _TALLY


			* Genera el archivo R contenedor de datos sobre el talento humano disponible en las UPGD
			SELECT PLANO
			ZAP

			=showProgressMessage('Generando Archivo Plano de Talento Humano / Servicios para ' + sInsurerName)
			exportationIsOK = ExportFromQRY('TAL' , sUPGDKeyWithPlus + ;
											' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', , ;
											 sInsurerPlainsFilePath, .F., , , .T.)
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
								'Existen registros duplicados en la fuente de Talento Humano / Servicios.' + CHR(13) + ;
								'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
								'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sInsurerPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ELSE
					sResultMsg='Existen registros duplicados en la fuente de Talento Humano / Servicios para ' + sInsurerName 
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF
			SELECT PLANO
			COPY TO (sHumanResourcesFileNameAndPath) SDF
			nRExportedRegs=_TALLY


			*Genera el archivo SIVIGILA contenedor de datos de cuándo y con qué versión fueron generados ;
			los archivos anterioreS
			DO makeSIVIGILAVersionFile WITH (sSIVIGILAFileNameAndPath), sSIVIGILAVersion 

			*Genera el archivo de control
			DO makeControlFile WITH sControlFileNameAndPath, sNotifierId, sNotifierFileNamePrefix, nUExportedRegs, ;
									nRExportedRegs, nNExportedRegs, -1, -1,	nDExportedRegs, -1, -1, nVExportedRegs

			*Codifica los archivos recien producidos
			sSivigilaCryptoFileNameAndPath = PATH_TO_SIVIGILACRYPTO + "SivigilaCrypto.prg"
			oCodifier  = NEWOBJECT( "SivigilaCrypto", sSivigilaCryptoFileNameAndPath, .NULL., sInsurerPlainsFilePath, "*.Txt", sInsurerPlainsFilePath)
			oCodifier.BatchCodify
			RELEASE oCodifier

			*Produce un archivo empaquetado en formato ZIP para envío
			SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
			TRY
				sZipFileName=sInsurerName + '.ZIP'
				sResultMsg=''

				DO zip WITH (sInsurerPlainsFilePath), '*.TXT', sFormerInsurerPlainsFilePath + '\' + sEntityName, (sZipFileName), sResultMsg, .F.
				IF EMPTY(sResultMsg) THEN
					*COPY FILE (sZipFileName) TO sFormerInsurerPlainsFilePath + '\' + sEntityName
				ELSE
					sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
								sResultMsg + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
								"Debe crear usted mismo el archivo ZIP para envío."
					=showErrorMessage(sResultMsg, 0)
				ENDIF
				*DELETE FILE (sZipFileName)
			CATCH TO oException
				sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
							"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
							"Mensaje del problema : "  + (oException.Message) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
							"Debe crear usted mismo el archivo ZIP para envío."
				=showErrorMessage(sResultMsg, 0)
			ENDTRY
*SET STEP ON 
			sInsurerPwd = fieldValue('PWD', 'ASEGURADORASPwds',"COD_ASE='" + cTargetInsurers.COD_ASE + "'")
			IF !ISNULL(sInsurerPwd) THEN
				sRarCmd = 'rar.exe u -ep ' + '"' + sFormerInsurerPlainsFilePath + '\' + sEntityName + '\' + sInsurerName + '" "' + ;
							sInsurerPlainsFilePath + '\' + '*.TXT" -p' + sInsurerPwd
				=FPUTS(nWinrarBatFileHandler,sRarCmd )
			ENDIF
			
			DO cleanTheHouse IN IMPORTAR_CLICK
			SELECT cTargetInsurers
		ENDSCAN
		
		sEndTime=TIME()
		DO cleanTheHouse IN IMPORTAR_CLICK
		sNotificatioMsg='Se ha generado la información de retroalimentación para las aseguradoras.' + CHR(13) + + CHR(13) +;
						'Los archivos planos han sido ubicados en la carpeta: ' + sFormerInsurerPlainsFilePath
		=showErrorMessage(sNotificatioMsg, 0)
		
		=FCLOSE(nWinrarBatFileHandler)
		IF bBeSilent THEN
			=FPUTS(nResultsFileHandler,sNotificatioMsg)
			=FPUTS(nResultsFileHandler,'Hora de inicio: ' + sInitTime)
			=FPUTS(nResultsFileHandler,'Hora de finalización: ' + sEndTime)			
			=FCLOSE(nResultsFileHandler)
		ENDIF
	ELSE
		sNotificatioMsg='ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
						'No se encontraron registros que satisfagan los criterios suministrados' + CHR(13) + ;
						'para la retroalimentación o no se encontraron UPGDs activas en el'  + CHR(13) + ;
						'Sistema que cumplan con esos criterios'
		=showErrorMessage(sNotificatioMsg, 0)
	ENDIF
	
	SET TALK OFF
ENDPROC


PROCEDURE prisonsFeedback

	LPARAMETERS sExportationPath as String, sEntityKind as Byte , sEntityCode as string, sNotifierKind as String,  sNotifierId as String, ;
				sNotificationWeek as String, sNotificationYear AS Integer, bBeSilent as Boolean, sSIVIGILAVersion as String
	
	*sEntityKind :  1-->Pais, 2-->Depto


	PUBLIC sPrisonPlainsFilePath AS String

	DIMENSION aPlainTableKeys(1)
	* Contiene los campos que conforman las claves de las tablas suceptibles de ser notificadas por;
	el sistema.
	*Estas claves son idénticas tanto para las tablas del sitema como para las tablas PLANOS_... que;
	son utilizadas en los procesos de notificación/consolidación del Sistema ;
	son utilizadas en el proceso de notificación

	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE

	SET TALK ON
	
	sInitTime=TIME()
	DO initPlainTableKeysArray WITH aPlainTableKeys IN importar_click

	sPrisonPlainsFilePath = "\RETROALIMENTACION A CÁRCELES"
	sPrisonPlainsFilePath = sExportationPath + sPrisonPlainsFilePath

	*Establece las condiciones de filtrado que deben cumplirse para exportar la información
	sDynamicConditions = "RecordSource.AÑO='" + ALLTRIM(STR(sNotificationYear)) + "'" +;
						 " AND (RecordSource.COD_SUB=='99' OR RecordSource.GP_CARCELA=='1')"
	IF sEntityKind = 2 THEN
		sDynamicConditions = sDynamicConditions  + " AND LEFT(RecordSource.COD_MUN,2)='" + sEntityCode + "'"
	ENDIF


	*Establece si hay registros para exportar
	sSQLClause= 'SELECT DISTINCT RecordSource.COD_PRE'
	IF sEntityKind = 2 THEN
		sSQLClause = sSQLClause + ', DEPTOS.NOM_DEP '
	ENDIF
	sSQLClause = sSQLClause + ' FROM PACIENTE AS RecordSource '
	IF sEntityKind = 2 THEN
		sSQLClause = sSQLClause + ' LEFT OUTER JOIN DEPTOS ON LEFT(RecordSource.cod_mun,2) = DEPTOS.cod_dep '
	ENDIF
	sSQLClause = sSQLClause + '	WHERE RecordSource.COD_EVE!="000" AND ' + sDynamicConditions 
	sSQLClause = sSQLClause + " INTO CURSOR rsTargetPrisons"
	&sSQLClause

	IF _TALLY>0 THEN
		*Crea la tabla PLANO en donde se almacenarán en forma temporal los datos a exportar
		DO CreateExportationPlainTable

		IF bBeSilent THEN
			sResultsFileName="Resultados_Retro_Cárceles_" + ALLTRIM(CHRTRAN(DTOC(DATE()),"/","")) + ".txt"
			nResultsFileHandler=FCREATE(sExportationPath + '\' + sResultsFileName)
		ENDIF

		sFormerPrisonPlainsFilePath = sPrisonPlainsFilePath 
		sFormersDynamicConditions = sDynamicConditions

		*Crea la carpeta en donde se almacenarán los archivos resultado de la exportación
		IF sEntityKind = 1 THEN
			sEntityName='PAIS'
		ELSE
			sEntityName=ALLTRIM(rsTargetPrisons.NOM_DEP)
		ENDIF
		sPrisonName = 'Cárceles'
		sPrisonPlainsFilePath = sFormerPrisonPlainsFilePath + '\' + sEntityName 
		IF !DIRECTORY(sPrisonPlainsFilePath)
			MKDIR (sPrisonPlainsFilePath)
		ENDIF

		*Fija los nombres de los archivos planos que almacenarán la información exportada desde las tablas del Sistema
		sUPGDsFileNameAndPath = ''
		sUCIsFileNameAndPath = ''
		sHumanResourcesFileNameAndPath = ''
		sPatientsFileNameAndPath = ''
		sEventsDataFileNameAndPath = ''
		sCollectiveEventsFileNameAndPath = ''
		sTotalsFileNameAndPath = ''
		sControlFileNameAndPath = ''
		sSIVIGILAFileNameAndPath = ''
		*A partir de la versión 2012 ya no se genera el archivo W
		sWEBPatientsFileNameAndPath=''
		sIRAFileNameAndPath = ''
		sNotifierFileNamePrefix=''
		DO setOutputTxtFileNames WITH sNotifierKind, sNotifierId, sNotificationWeek, sPrisonPlainsFilePath, sNotifierFileNamePrefix,;
					sUPGDsFileNameAndPath, sHumanResourcesFileNameAndPath, sPatientsFileNameAndPath, ;
					sEventsDataFileNameAndPath, sCollectiveEventsFileNameAndPath, sTotalsFileNameAndPath ,;
					sControlFileNameAndPath, sSIVIGILAFileNameAndPath, sWEBPatientsFileNameAndPath ,;
					sIRAFileNameAndPath, sUCIsFileNameAndPath


		* Genera el archivo N de datos básicos de notificación individual periódica semanal
		SELECT PLANO
		ZAP

		TIT('Generando Archivo Plano de Notificación Individual para ' + sPrisonName)
		sDynamicConditions = sFormersDynamicConditions + " AND RecordSource.COD_EVE!='000' "

		exportationIsOK = ExportFromQRY('PTE', sDynamicConditions, , sPrisonPlainsFilePath, .F.) 
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros duplicados en la fuente de notificación individual.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sPrisonPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg='Existen registros duplicados en la fuente de notificación individual para cárceles'
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		SELECT PLANO
		COPY TO (sPatientsFileNameAndPath ) SDF
		nNExportedRegs=_TALLY

		*Establece una línea base de pacientes para los cuales se exportarán los registros de datos complementarios, laboratorios, UPGDs;
		y talento humano.
		sSQLClause= 'SELECT DISTINCT RecordSource.AÑO, RecordSource.SEMANA, RecordSource.COD_EVE, RecordSource.TIP_IDE, RecordSource.NUM_IDE, ';
					+ 'RecordSource.COD_PRE, RecordSource.COD_SUB FROM PACIENTE AS RecordSource ';
					+ " WHERE  " + sDynamicConditions +	" INTO CURSOR rsPatientsBaseLine NOFILTER"
		&sSQLClause

		* Genera el archivo D de datos complementarios de notificación periódica semanal obligatoria;
		incluyendo la información de resultados de laboratorio
		SELECT PLANO
		ZAP

		sEventKeyExpression = ASCAN(aPlainTableKeys, 'PLANO_EXX', -1, -1 ,-1, 15)
		sEventKeyWithCommas = ALLTRIM(aPlainTableKeys(sEventKeyExpression ,2))
		sEventKeyWithPlus = 'RecordSource.' + STRTRAN(sEventKeyWithCommas, ', ' , '+RecordSource.')
		sPatientsBaseLineKeyWithPlus = 'rsPatientsBaseLine.' + STRTRAN(sEventKeyWithCommas, ', ' , '+rsPatientsBaseLine.')

		*Establece el conjunto de tablas de eventos presentes en la línea base de pacientes para los cuales se exportarán los ;
		registros de datos complementarios.
		SELECT DISTINCT INT(VAL(Eventos.Formulario)) AS CDTableToExport FROM Eventos WHERE  !EMPTY(Eventos.formulario) ;
			AND Eventos.COD_EVE IN (SELECT DISTINCT rsPatientsBaseLine.COD_EVE FROM rsPatientsBaseLine ) ;
			INTO CURSOR rsEventTablesToExport
		nEventTablesToExport=_TALLY

		IF nEventTablesToExport>0 THEN
			SELECT rsEventTablesToExport
			SCAN
				IF CDTableToExport != 23 AND CDTableToExport != 31 AND CDTableToExport != 32 AND CDTableToExport != 43 ;
					AND CDTableToExport != 80 AND CDTableToExport != 81 AND CDTableToExport != 88 AND CDTableToExport != 94 THEN
					* Estos eventos no tienen todos los atributos del conjunto {AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB}
					
					TIT('Generando Archivo Plano del Grupo de Eventos ' + PADL(CDTableToExport ,2,'0') + ' para ' + sPrisonName)
					sCDTableNameToExport='EVENTOS_' + PADL(CDTableToExport ,2,'0')
					exportationIsOK = ExportFromQRY('E'+PADL(CDTableToExport ,2,'0'), sEventKeyWithPlus + ;
													' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM rsPatientsBaseLine)', , ;
													 sPrisonPlainsFilePath, .F.)
					
					IF !exportationIsOK THEN
						IF !bBeSilent THEN					
							MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
										'Existen registros duplicados en la fuente de datos complementarios.' + CHR(13) + ;
										'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
										'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sPrisonPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
						ELSE
							sResultMsg='Existen registros duplicados en la fuente de datos complementarios para ' + sPrisonName + ;
										+ ' - Tabla ' + sCDTableNameToExport
							=FPUTS(nResultsFileHandler,sResultMsg)
						ENDIF
					ENDIF
				ENDIF
				SELECT rsEventTablesToExport
			ENDSCAN
		ENDIF
		
		TIT('Generando Archivo Plano de Laboratorios para ' + sPrisonName)
		exportationIsOK = ExportFromQRY('LAB', sEventKeyWithPlus + ' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM rsPatientsBaseLine)', , ;
										 sPrisonPlainsFilePath, .F.)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros duplicados en la fuente de notificación laboratorios.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sPrisonPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg='Existen registros duplicados en la fuente de notificación laboratorios para ' + sPrisonName 
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		SELECT PLANO
		COPY TO (sEventsDataFileNameAndPath) SDF
		nDExportedRegs=_TALLY


		* Genera el archivo U contenedor de datos de caracterización de UPGD
		SELECT PLANO
		ZAP

		sUPGDKeyExpression = ASCAN(aPlainTableKeys, 'PLANO_UPGD', -1, -1 ,-1, 15)
		sUPGDKeyWithCommas = aPlainTableKeys(sUPGDKeyExpression,2)
		sUPGDKeyWithPlus = 'RecordSource.' + STRTRAN(sUPGDKeyWithCommas, ', ' , '+RecordSource.')
		sPatientsBaseLineKeyWithPlus = 'rsPatientsBaseLine.' + STRTRAN(sUPGDKeyWithCommas, ', ' , '+rsPatientsBaseLine.')

		TIT('Generando Archivo Plano de las UPGD para ' + sPrisonName)
		exportationIsOK = ExportFromQRY('UPGD' , sUPGDKeyWithPlus + ' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM rsPatientsBaseLine)', , ;
										 sPrisonPlainsFilePath, .F.)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros duplicados en la fuente de UPGDs.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sPrisonPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg='Existen registros duplicados en la fuente de UPGDs para ' + sPrisonName 
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		SELECT PLANO
		COPY TO (sUPGDsFileNameAndPath) SDF
		nUExportedRegs=_TALLY


		* Genera el archivo V contenedor de datos de caracterización de UCI en las UPGD
		SELECT PLANO
		ZAP

		TIT('Generando Archivo Plano de caracterización de UCI para ' + sPrisonName)
		exportationIsOK = ExportFromQRY('UCI' , sUPGDKeyWithPlus + ;
										' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM rsPatientsBaseLine)', , ;
										 sPrisonPlainsFilePath, .F.)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros duplicados en la fuente de caracterización de UCI.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sPrisonPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg='Existen registros duplicados en la fuente  de caracterización de UCI ' + sPrisonName 
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		SELECT PLANO
		COPY TO (sUCIsFileNameAndPath) SDF
		nVExportedRegs = _TALLY


		* Genera el archivo R contenedor de datos sobre el talento humano disponible en las UPGD
		SELECT PLANO
		ZAP

		TIT('Generando Archivo Plano de Talento Humano / Servicios para ' + sPrisonName)
		exportationIsOK = ExportFromQRY('TAL' , sUPGDKeyWithPlus + ;
										' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM rsPatientsBaseLine)', , ;
										 sPrisonPlainsFilePath, .F.)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros duplicados en la fuente de Talento Humano / Servicios.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'duplicados para los fines que se considere pertinentes en:' + CHR(13) + sPrisonPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg='Existen registros duplicados en la fuente de Talento Humano / Servicios para ' + sPrisonName 
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		SELECT PLANO
		COPY TO (sHumanResourcesFileNameAndPath) SDF
		nRExportedRegs=_TALLY


		*Genera el archivo SIVIGILA contenedor de datos de cuándo y con qué versión fueron generados ;
		los archivos anterioreS
		DO makeSIVIGILAVersionFile WITH (sSIVIGILAFileNameAndPath), sSIVIGILAVersion 

		*Genera el archivo de control
		DO makeControlFile WITH sControlFileNameAndPath, sNotifierId, sNotifierFileNamePrefix, nUExportedRegs, ;
								nRExportedRegs, nNExportedRegs, -1, -1,	nDExportedRegs, -1, -1, nVExportedRegs

		*Codifica los archivos recien producidos
		sSivigilaCryptoFileNameAndPath = PATH_TO_SIVIGILACRYPTO + "SivigilaCrypto.prg"
		oCodifier  = NEWOBJECT( "SivigilaCrypto", sSivigilaCryptoFileNameAndPath, .NULL., sPrisonPlainsFilePath, "*.Txt", sPrisonPlainsFilePath)
		oCodifier.BatchCodify
		RELEASE oCodifier

		*Produce un archivo empaquetado en formato ZIP para envío
		SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		TRY
			sZipFileName=sPrisonName + '.ZIP'
			sResultMsg=''

			DO zip WITH (sPrisonPlainsFilePath), '*.TXT', sFormerPrisonPlainsFilePath + '\' + sEntityName, (sZipFileName), sResultMsg, .F.
			IF EMPTY(sResultMsg) THEN
				*COPY FILE (sZipFileName) TO sFormerPrisonPlainsFilePath + '\' + sEntityName
			ELSE
				sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
							sResultMsg + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
							"Debe crear usted mismo el archivo ZIP para envío."
				=showErrorMessage(sResultMsg, 0)
			ENDIF
			*DELETE FILE (sZipFileName)
		CATCH TO oException
			sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
						"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
						"Mensaje del problema : "  + (oException.Message) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
						"Debe crear usted mismo el archivo ZIP para envío."
			=showErrorMessage(sResultMsg, 0)
		ENDTRY

		DO cleanTheHouse IN IMPORTAR_CLICK
		
		sEndTime=TIME()
		sNotificatioMsg='Se ha generado la información de retroalimentación para cárceles.' + CHR(13) + + CHR(13) +;
						'Los archivos planos han sido ubicados en la carpeta: ' + sFormerPrisonPlainsFilePath
		=showErrorMessage(sNotificatioMsg, 0)
		
		IF bBeSilent THEN
			=FPUTS(nResultsFileHandler,sNotificatioMsg)
			=FPUTS(nResultsFileHandler,'Hora de inicio: ' + sInitTime)
			=FPUTS(nResultsFileHandler,'Hora de finalización: ' + sEndTime)			
			=FCLOSE(nResultsFileHandler)
		ENDIF
	ELSE
		sNotificatioMsg='ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
						'No se encontraron registros que satisfagan los criterios suministrados' + CHR(13) + ;
						'para la retroalimentación o no se encontraron UPGDs activas en el'  + CHR(13) + ;
						'Sistema que cumplan con esos criterios'
		=showErrorMessage(sNotificatioMsg, 0)
	ENDIF
	
	SET TALK OFF
ENDPROC

PROCEDURE territorialUnitFeedback(sExportationPath as String, sEntityKind as Byte , sEntityCode as string, sTargetYear as String, ;
								sNotifierKind as String,  sNotifierId as String, sNotificationWeek as String, ;
								bDoNotShowTerminationMsg as Boolean, bBeSilent as Boolean, ;
								bDoNotUseOriginMunicipality as Boolean, bIncludeOnlyForeignUPGDs as Boolean, sPwd AS String )
	
	*Crea archivos planos de retroalimentación para una entidad territorial identificada por el par (sEntityKind, sEntityCode) en donde;
	sEntityKind es el identificador del tipo de entidad (2=Depto 3=Municipio) y sEntityCode es el valor o código que identifica a;
	la entidad. Los archivos planos que se crean corresponderán a la vigencia sTargetYear y los registros que;
	se incluyan en ellos obedecerán la configuración que se encuentra en Tables.tbConfiguracionRetroalimentacion
	
	*sExportationPath: ruta en donde quedarán almacenados ls archivos planos de retroalimentación
	
	*sNotifierKind, sNotifierId, sNotificationWeek: utilizados para establecer los nombres de los archivos ;
	planos a crear así: ;
		sNotifierKind, sNotifierId: identificadores del tipo de unidad que notifica y código de la unidad ;
		sNotificationWeek: identificador de la semana epidemiológica, normalmente será 'RA' ;

		 
	*bDoNotShowTerminationMsg: indica si debe mostrarse o no el mensaje que indica que la ejecución de este PROCEDURE ha finalizado
	
	*bBeSilent: si es .T. redirecciona todos los mensajes que se produzcan durante el procesamiento hacia un archivo texto que;
	se almacenará en sExportationPath; en caso contrario, los mensajes se muestran en la salida estándar
	
	*bDoNotUseOriginMunicipality: indica si en la extracción de datos debe usarse el municipio de procedencia o de residencia;
	el valor por defecto es .F., es decir, se usa el municipio de procedencia

	*bIncludeOnlyForeignUPGDs: indica si en la extracción de datos deben incluirse solamente UPGDs que NO pertenezcan al departamento;
	el valor por defecto es .F., es decir, se incluyen tanto UPGDs que pertenecen al departamento como UPGDs que no pertenecen a él

	*sPwd: si se pasa este parámetro opcional, el paquete zip de archivos planos resultante del procesamiento quedará con;
	este password asignado
	 
	LOCAL sTerritorialUnitPlainsFilePath  AS String
	LOCAL nExportedRecs as string
	

	DIMENSION aPlainTableKeys(1)
	* Contiene los campos que conforman las claves de las tablas suceptibles de ser notificadas por;
	el sistema.
	*Estas claves son idénticas tanto para las tablas del sistema como para las tablas PLANOS_... que;
	son utilizadas en los procesos de notificación/consolidación del Sistema ;

	sInitTime=TIME()

	nBaseLineRecs = 0
		
	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	
	IF !(VARTYPE(sPwd)='C' AND !EMPTY(sPwd)) THEN 
		sPwd = ''
	ENDIF 
	
	DO cleanTheHouse IN IMPORTAR_CLICK

	DO initPlainTableKeysArray WITH aPlainTableKeys IN importar_click

*SET STEP ON 
	*Establece las condiciones de filtrado que deben cumplirse para exportar la información y la tabla de referencia;
	en donde se encuentra la información de la entidad territorial

	IF sEntityKind = 2 THEN
		sEntityLevel = "DEPARTAMENTO"
		IF bUseResidenceMunicipality = 0 THEN
			*Usa el departamento de procedencia como criterio para extraer los datos
			sConcept = "Procedencia"
		ELSE
			*Usa el departamento de residencia como criterio para extraer los datos
			sConcept = "Residencia"
		ENDIF
	ELSE
		sEntityLevel = "MUNICIPIO"
		IF bUseResidenceMunicipality = 0 THEN
			*Usa el municipio de procedencia como criterio para extraer los datos
			sConcept = "Procedencia"
		ELSE
			*Usa el municipio de residencia como criterio para extraer los datos
			sConcept = "Residencia"
		ENDIF
	ENDIF
	sIndividualFilter = fieldValue('EXPR_CAMPO', 'tbConfiguracionRetroalimentacion', 'NOM_TABLA="PACIENTE"  AND CONCEPTO="' + sConcept + ;
						'" AND NIVEL="' + sEntityLevel  + '"')
	sCollectiveFilter = fieldValue('EXPR_CAMPO', 'tbConfiguracionRetroalimentacion', 'NOM_TABLA="BROTES"  AND CONCEPTO="' + sConcept + ;
						'" AND NIVEL="' + sEntityLevel  + '"')

	IF sEntityKind = 2 THEN
		sIndividualLinkCondition = sIndividualFilter + ' = DEPTOS.cod_dep '
		sCollectiveLinkCondition = sCollectiveFilter + ' = DEPTOS.cod_dep '
		
		sTerritorialUnitLookupName='DEPTOS'
		sTerritorialUnitFieldName='DEPTOS.NOM_DEP AS NombreEntidad'
	ELSE
		sIndividualLinkCondition = sIndividualFilter + ' = MUNICIPIOS.cod_mun '
		sCollectiveLinkCondition = sCollectiveFilter + ' = MUNICIPIOS.cod_mun '
		
		sTerritorialUnitLookupName='MUNICIPIOS'
		sTerritorialUnitFieldName='MUNICIPIOS.NOM_MUN AS NombreEntidad'
	ENDIF
	sIndividualTerritorialUnitFieldCode = sIndividualFilter + " AS CodEntidad"
	sCollectiveTerritorialUnitFieldCode = sCollectiveFilter + " AS CodEntidad"
*SET STEP ON 
	sIndividualFilter = sIndividualFilter + "='" + sEntityCode + "'" + IIF(sTargetYear!='0', " AND RecordSource.AÑO = '" + sTargetYear + "'", '') 
	sCollectiveFilter = sCollectiveFilter + "='" + sEntityCode + "'" + IIF(sTargetYear!='0', " AND RecordSource.AÑO = '" + sTargetYear + "'", '') 

	IF (sEntityKind = 2) AND bIncludeOnlyForeignUPGDs THEN
		sIndividualFilter = sIndividualFilter + " AND LEFT(RecordSource.COD_PRE,2)!='" + sEntityCode + "'"
		sCollectiveFilter = sCollectiveFilter + " AND LEFT(RecordSource.COD_PRE,2)!='" + sEntityCode + "'"
	ENDIF

	sIndividualFilter = sIndividualFilter + " AND " + fieldValue('FILTRO', 'tbConfiguracionRetroalimentacion', 'NOM_TABLA="PACIENTE"  AND CONCEPTO="' + sConcept + ;
						'" AND NIVEL="' + sEntityLevel  + '"')
	sCollectiveFilter = sCollectiveFilter + " AND " + fieldValue('FILTRO', 'tbConfiguracionRetroalimentacion', 'NOM_TABLA="BROTES"  AND CONCEPTO="' + sConcept + ;
						'" AND NIVEL="' + sEntityLevel  + '"')


	*Establece una línea base de pacientes para los cuales se exportarán los registros de datos complementarios, laboratorios, UPGDs;
	y talento humano teniendo en cuenta a qué entidades territoriales debe exportárseles información y excluyendo las notificaciones;
	negativas y la información correspondiente a UPGDs inactivas en el Sistema
	sSQLClause= "SELECT DISTINCT RecordSource.AÑO, RecordSource.SEMANA, RecordSource.COD_EVE, RecordSource.TIP_IDE," +;
				" RecordSource.NUM_IDE, RecordSource.COD_PRE, RecordSource.COD_SUB," +;
				sIndividualTerritorialUnitFieldCode + "," + sTerritorialUnitFieldName +;
				" FROM PACIENTE AS RecordSource LEFT OUTER JOIN " + sTerritorialUnitLookupName +;
				" ON " + sIndividualLinkCondition + " WHERE RecordSource.COD_EVE " +;
				" NOT IN (SELECT TOP 1 '000' AS CodNotificacionNegativa FROM EVENTOS ORDER BY COD_EVE) " +;
				" AND " + sIndividualFilter +	" INTO CURSOR cPatientsBaseLine"
				
	*La siguiente condición que obligaba a que se exportase información correspondiente;
	a UPGDs inactivas en el Sistema, fue desactivada por solicitud de Marcela Benavides del INS;
	el 15/07/2010	
	&&" AND (recordSource.cod_pre + " + "recordSource.cod_sub" + ;
	&&" IN (SELECT DISTINCT cod_pre+cod_sub AS CodUPGD FROM UPGD WHERE ACT_SIV = 1))" 
	&sSQLClause
	nBaseLineRecs = nBaseLineRecs + _TALLY
	
	*Establece una línea base de brotes para los cuales se exportarán los registros de datos complementarios teniendo en ;
	cuenta a qué entidades territoriales debe exportárseles información y excluyendo la información correspondiente a ;
	UPGDs inactivas en el Sistema
	sSQLClause = "SELECT DISTINCT RecordSource.AÑO, RecordSource.SEMANA, RecordSource.COD_EVE, RecordSource.COD_MUN," +;
				" RecordSource.NUM_CON, RecordSource.COD_PRE, RecordSource.COD_SUB," +;
				sCollectiveTerritorialUnitFieldCode + "," + sTerritorialUnitFieldName +;
				" FROM BROTES AS RecordSource LEFT OUTER JOIN " + sTerritorialUnitLookupName +;
				" ON " + sCollectiveLinkCondition + " WHERE " + sCollectiveFilter +	" INTO CURSOR cOutbreaksBaseLine"
	&sSQLClause
	nBaseLineRecs = nBaseLineRecs + _TALLY
	
	IF nBaseLineRecs = 0 THEN
		sNotificatioMsg='ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
						'No se encontraron registros que satisfagan los criterios suministrados' + CHR(13) + ;
						'para la retroalimentación.'
		IF !bDoNotShowTerminationMsg THEN
			=showErrorMessage(sNotificatioMsg, 0)
		ENDIF
	ELSE
	
		SELECT cPatientsBaseLine

		*Crea la carpeta en donde se almacenarán los archivos resultado de la exportación
		IF ISNULL(cPatientsBaseLine.NombreEntidad) THEN
			sTerritorialUnitName = 'DESCONOCIDA'
		ELSE
			sTerritorialUnitName = ALLTRIM(cPatientsBaseLine.NombreEntidad)
		ENDIF
		sTerritorialUnitName = ALLTRIM(CHRTRAN(ALLTRIM(sTerritorialUnitName), GARBAGE_CHARS, ''))
		sTerritorialUnitPlainsFilePath = sExportationPath + '\' + sTerritorialUnitName + "_" + ;
										 ALLTRIM(cPatientsBaseLine.CodEntidad)
		IF !DIRECTORY(sTerritorialUnitPlainsFilePath)
			MKDIR (sTerritorialUnitPlainsFilePath)
		ENDIF

		IF bBeSilent THEN
			sResultsFileName = "ResultadosRetroalimentacion_" + ALLTRIM(CHRTRAN(DTOC(DATE()),"/","")) + ".csv"
			nResultsFileHandler = FCREATE(ADDBS(sTerritorialUnitPlainsFilePath) + sResultsFileName)
		ENDIF

		*Fija los nombres de los archivos planos que almacenarán la información exportada desde las tablas del Sistema
		sUPGDsFileNameAndPath = ''
		sUCIsFileNameAndPath = ''
		sHumanResourcesFileNameAndPath = ''
		sPatientsFileNameAndPath = ''
		sEventsDataFileNameAndPath = ''
		sCollectiveEventsFileNameAndPath = ''
		sTotalsFileNameAndPath = ''
		sControlFileNameAndPath = ''
		sSIVIGILAFileNameAndPath = ''
		*A partir de la versión 2012 ya no se genera el archivo W		
		sWEBPatientsFileNameAndPath=''
		sIRAFileNameAndPath = ''
		sNotifierFileNamePrefix=''
		DO setOutputTxtFileNames WITH sNotifierKind, sNotifierId, sNotificationWeek, sTerritorialUnitPlainsFilePath, sNotifierFileNamePrefix,;
					sUPGDsFileNameAndPath, sHumanResourcesFileNameAndPath, sPatientsFileNameAndPath, ;
					sEventsDataFileNameAndPath, sCollectiveEventsFileNameAndPath, sTotalsFileNameAndPath ,;
					sControlFileNameAndPath, sSIVIGILAFileNameAndPath, sWEBPatientsFileNameAndPath ,;
					sIRAFileNameAndPath, sUCIsFileNameAndPath 

		* Genera el archivo N de datos básicos de notificación individual periódica semanal
		TIT('Generado Archivo Plano de Notificación Individual para ' + sTerritorialUnitName)
		nOutFileHandler = FCREATE(sPatientsFileNameAndPath + '.txt')
		nNExportedRegs = 0
		exportationIsOK = ExportFromQRYToPlainFile('PTE', sIndividualFilter + " AND RecordSource.COD_EVE " +;
													" NOT IN (SELECT TOP 1 '000' AS CodNotificacionNegativa FROM EVENTOS ORDER BY COD_EVE) ", ;
													 sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nNExportedRegs) 
		=FCLOSE(nOutFileHandler)
				
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13)  + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de notificación individual.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de notificación individual.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		
		* Genera el archivo D de datos complementarios de notificación periódica semanal obligatoria;
		incluyendo la información de resultados de laboratorio. Se excluyen eventos que no obedecen el patrón DB + DC inclusive;
		si han sido borrados del sistema
		sOldDeleted = SET("Deleted")
		SET DELETED OFF
		DO EventosConTablaDatosIndependiente.qpr
		SET DELETED &sOldDeleted 
		
		nOutFileHandler = FCREATE(sEventsDataFileNameAndPath + '.txt')
		nDExportedRegs = 0
		USE BDSivigila!vTablasDeDC IN 0
		SELECT vTablasDeDC
		SCAN
			eventToExport = vTablasDeDC.ID_DC_TABLA
			SELECT rsEventosConTablaDatosIndependiente
			IF eventToExport != 23 AND EMPTY(LOOKUP(FORMULARIO,ALLTRIM(STR(eventToExport)),FORMULARIO)) THEN
				*La tabla de datos complementarios no corresponde a una de un evento con tabla de datos independiente ni a la tabla;
				EVENTOS_23, que contiene datos complementarios de un brote

				sEventKeyExpression = ASCAN(aPlainTableKeys, 'PLANO_EXX', -1, -1 ,-1, 15)
				sEventKeyWithCommas = ALLTRIM(aPlainTableKeys(sEventKeyExpression ,2))
				sEventKeyWithPlus = 'RecordSource.' + STRTRAN(sEventKeyWithCommas, ', ' , '+RecordSource.')
				sPatientsBaseLineKeyWithPlus = 'cPatientsBaseLine.' + STRTRAN(sEventKeyWithCommas, ', ' , '+cPatientsBaseLine.')
				
				TIT('Generando Archivo Plano del Grupo de Eventos ' + PADL(eventToExport ,2,'0') + ' para ' + sTerritorialUnitName)
				exportationIsOK = ExportFromQRYToPlainFile('E'+PADL(eventToExport ,2,'0'), sEventKeyWithPlus + ;
												' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', ;
												 sTerritorialUnitPlainsFilePath, .F., ,.T. , .T., , , nOutFileHandler, @nDExportedRegs )
												 
				IF !exportationIsOK THEN
					IF !bBeSilent THEN
						MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
									'Existen registros repetidos o duplicados en la fuente de datos complementarios.' + CHR(13) + ;
									'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
									'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
					ELSE
						sResultMsg=	'Existen registros repetidos o duplicados en la fuente de datos complementarios.' + CHR(13) + ;
									'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
									'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
									sTerritorialUnitPlainsFilePath
						=FPUTS(nResultsFileHandler,sResultMsg)
					ENDIF
				ENDIF
			ELSE
				IF eventToExport != 43 THEN
					* Los datos de la tabla eventos 43 (Morbilidad por IRA) se generan en un archivo independiente					
					DO CASE
						CASE  eventToExport = 23
							*Se trata de la tabla de datos complementarios del evento Enfermedades transmitidas por alimentos  ETAs (Colectiva),;
							 Hepatitis A, Fiebre Tifoide (Cód. 350, 330, 320)
							
							sFormerEventKeyWithPlus = getKeyFields('EVENTOS_'+PADL(eventToExport ,2,'0'))
							*sEventKeyWithCommas no corresponde estrictamente a la clave de EVENTOS_23 o EVENTOS_31 si no que contiene;
							el mayor número	de atributos de la clave que están también presentes en la línea base de pacientes
							
							sEventKeyWithPlus = 'RecordSource.' + STRTRAN(sFormerEventKeyWithPlus, '+' , '+RecordSource.')
							sOutbreaksBaseLineKeyWithPlus = 'cOutbreaksBaseLine.' + STRTRAN(sFormerEventKeyWithPlus, '+' , '+cOutbreaksBaseLine.')
							TIT('Generando Archivo Plano del Grupo de Eventos ' + PADL(eventToExport ,2,'0') + ' para ' + sTerritorialUnitName)
							TRY
								exportationIsOK = ExportFromQRYToPlainFile('E'+PADL(eventToExport ,2,'0'), sIndividualFilter + " AND " + sEventKeyWithPlus + ;
																' IN (SELECT &sOutbreaksBaseLineKeyWithPlus FROM cOutbreaksBaseLine)',  ;
																 sTerritorialUnitPlainsFilePath, .F., ,.T. , .T., , , nOutFileHandler, @nDExportedRegs)

							CATCH TO oException
							ENDTRY
						OTHERWISE
							*Se trata de un evento con tabla de datos independiente
							
							sCompDataTableName = "EVENTOS_" + PADL(eventToExport ,2,'0')
							sCompDataFilter = fieldValue('EXPR_CAMPO', 'tbConfiguracionRetroalimentacion', 'NOM_TABLA="' + sCompDataTableName + ;
														'"  AND CONCEPTO="' + sConcept + '" AND NIVEL="' + sEntityLevel  + '"')

							IF ISNULL(sCompDataFilter) THEN
								sCompDataFilter = sIndividualFilter
							ELSE
								sCompDataFilter = sCompDataFilter + "='" + sEntityCode + "'" + IIF(sTargetYear!='0', " AND RecordSource.AÑO = '" + sTargetYear + "'", '') 
							ENDIF

							IF (sEntityKind = 2) AND bIncludeOnlyForeignUPGDs THEN
								sCompDataFilter = sCompDataFilter + " AND LEFT(RecordSource.COD_PRE,2)!='" + sEntityCode + "'"
							ENDIF
							
							TIT('Generando Archivo Plano del Grupo de Eventos ' + PADL(eventToExport ,2,'0') + ' para ' + sTerritorialUnitName)
							TRY
								exportationIsOK = ExportFromQRYToPlainFile('E'+PADL(eventToExport ,2,'0'), sCompDataFilter, ;
																sTerritorialUnitPlainsFilePath, .F., ,.T. , .T., , , nOutFileHandler, @nDExportedRegs)
							CATCH TO oException
							ENDTRY
					ENDCASE
				ENDIF
			ENDIF
		ENDSCAN 

		* Agrega al archivo D la información de resultados de laboratorio
		sEventKeyExpression = ASCAN(aPlainTableKeys, 'PLANO_EXX', -1, -1 ,-1, 15)
		sEventKeyWithCommas = ALLTRIM(aPlainTableKeys(sEventKeyExpression ,2))
		sEventKeyWithPlus = 'RecordSource.' + STRTRAN(sEventKeyWithCommas, ', ' , '+RecordSource.')
		sPatientsBaseLineKeyWithPlus = 'cPatientsBaseLine.' + STRTRAN(sEventKeyWithCommas, ', ' , '+cPatientsBaseLine.')

		TIT('Generando Archivo Plano de Laboratorios para ' + sTerritorialUnitName)
		exportationIsOK = ExportFromQRYToPlainFile('LAB', sEventKeyWithPlus + ' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)',  ;
										 sTerritorialUnitPlainsFilePath, .F., ,.T. , .T., , , nOutFileHandler, @nDExportedRegs)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de notificación laboratorios.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de notificación laboratorios.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
			
		ENDIF
		=FCLOSE(nOutFileHandler)


		* Genera el archivo B de datos de notificación colectiva periódica semanal obligatoria
		nOutFileHandler = FCREATE(sCollectiveEventsFileNameAndPath + '.txt')
		nBExportedRegs = 0

		TIT('Generando Archivo Plano de Notificación Colectiva para ' + sTerritorialUnitName)
		exportationIsOK = ExportFromQRYToPlainFile('BRO', sCollectiveFilter, sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nBExportedRegs)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de notificación colectiva.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de notificación colectiva.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		=FCLOSE(nOutFileHandler)


		* Genera el archivo U contenedor de datos de caracterización de UPGD

		sUPGDKeyExpression = ASCAN(aPlainTableKeys, 'PLANO_UPGD', -1, -1 ,-1, 15)
		sUPGDKeyWithCommas = aPlainTableKeys(sUPGDKeyExpression,2)
		sUPGDKeyWithPlus = 'RecordSource.' + STRTRAN(sUPGDKeyWithCommas, ', ' , '+RecordSource.')
		sPatientsBaseLineKeyWithPlus = 'cPatientsBaseLine.' + STRTRAN(sUPGDKeyWithCommas, ', ' , '+cPatientsBaseLine.')

		TIT('Generando Archivo Plano de las UPGD para ' + sTerritorialUnitName)
		nOutFileHandler = FCREATE(sUPGDsFileNameAndPath + '.txt')
		nUExportedRegs = 0

		exportationIsOK = ExportFromQRYToPlainFile('UPGD' , sUPGDKeyWithPlus + ' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', ;
										 sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nUExportedRegs )
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de UPGDs.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de UPGDs.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		=FCLOSE(nOutFileHandler)


		* Genera el archivo V contenedor de datos de caracterización de UCI en las UPGD
		TIT('Generando Archivo Plano de caracterización de UCI para ' + sTerritorialUnitName)
		nOutFileHandler = FCREATE(sUCIsFileNameAndPath + '.txt')
		nVExportedRegs = 0

		exportationIsOK = ExportFromQRYToPlainFile('UCI' , sUPGDKeyWithPlus + ;
										' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', ;
										 sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nVExportedRegs )
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de caracterización de UCI.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de caracterización de UCI.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		=FCLOSE(nOutFileHandler)

		* Genera el archivo R contenedor de datos sobre el talento humano disponible en las UPGD
		nOutFileHandler = FCREATE(sHumanResourcesFileNameAndPath + '.txt')
		nRExportedRegs = 0
		TIT('Generando Archivo Plano de Talento Humano / Servicios para ' + sTerritorialUnitName)
		exportationIsOK = ExportFromQRYToPlainFile('TAL' , sUPGDKeyWithPlus + ;
										' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', ;
										 sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nRExportedRegs )
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de Talento Humano / Servicios.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de Talento Humano / Servicios.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		=FCLOSE(nOutFileHandler)


		* Genera el archivo I contenedor de datos del evento colectivo morbilidad por IRA (Vigilancia Centinela ESI - IRAG);

		TIT('Generando Archivo Plano del Grupo de Eventos 43 para ' + sTerritorialUnitName)
		nOutFileHandler = FCREATE(sIRAFileNameAndPath + '.txt')
		nIExportedRegs = 0
		TRY
			exportationIsOK = ExportFromQRYToPlainFile('E43', sIndividualFilter, sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nIExportedRegs )
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
								'Existen registros repetidos o duplicados en la fuente de Morbilidad por IRA.' + CHR(13) + ;
								'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
								'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
								sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ELSE
					sResultMsg=	'Existen registros repetidos o duplicados en la fuente de Morbilidad por IRA.' + CHR(13) + ;
								'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
								'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
								sTerritorialUnitPlainsFilePath
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF
		CATCH TO oException
			nIExportedRegs = 0
		ENDTRY
		=FCLOSE(nOutFileHandler)

		*Genera el archivo SIVIGILA contenedor de datos de cuándo y con qué versión fueron generados ;
		los archivos anterioreS
		DO makeSIVIGILAVersionFile WITH (sSIVIGILAFileNameAndPath), APP_NAME 

		*Genera el archivo de control
		DO makeControlFile WITH sControlFileNameAndPath, sNotifierId, sNotifierFileNamePrefix, nUExportedRegs, ;
								nRExportedRegs, nNExportedRegs, nBExportedRegs,	-1, nDExportedRegs, -1, nIExportedRegs, nVExportedRegs

		*Codifica los archivos recien producidos
		sSivigilaCryptoFileNameAndPath = PATH_TO_SIVIGILACRYPTO + "SivigilaCrypto.prg"
		oCodifier  = NEWOBJECT( "SivigilaCrypto", sSivigilaCryptoFileNameAndPath, .NULL., sTerritorialUnitPlainsFilePath, "*.Txt", sTerritorialUnitPlainsFilePath)
		oCodifier.BatchCodify
		RELEASE oCodifier

		*Produce un archivo empaquetado en formato ZIP para envío
		SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		TRY
			sZipFileName=sTerritorialUnitName + '.ZIP'
			sResultMsg=''
			*DO zip WITH (sTerritorialUnitPlainsFilePath), '*.TXT', sExportationPath , (sZipFileName), sResultMsg
			DO ArchiveFiles WITH (sTerritorialUnitPlainsFilePath), '*.TXT', sExportationPath , (sZipFileName), sResultMsg, , sPwd IN Backup
			IF EMPTY(sResultMsg) THEN
				*COPY FILE (sZipFileName) TO (sExportationPath) 
			ELSE
				sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
							sResultMsg + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
							"Debe crear usted mismo el archivo ZIP para envío."
				IF !bBeSilent THEN
					=showErrorMessage(sResultMsg, 0)
				ELSE
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF
			*DELETE FILE (sZipFileName)
		CATCH TO oException
			sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
						"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
						"Mensaje del problema : "  + (oException.Message) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
						"Debe crear usted mismo el archivo ZIP para envío."
			IF !bBeSilent THEN
				=showErrorMessage(sResultMsg, 0)
			ELSE
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDTRY
		
		sNotificatioMsg='Se ha generado la información de retroalimentación para la entidad territorial.' + CHR(13) + + CHR(13) +;
						'Los archivos planos han sido ubicados en la carpeta: ' + sTerritorialUnitPlainsFilePath 
		IF !bDoNotShowTerminationMsg THEN
			=showErrorMessage(sNotificatioMsg, 0)
		ENDIF

		sEndTime=TIME()
		IF bBeSilent THEN
			=FPUTS(nResultsFileHandler,CR_LF + sNotificatioMsg)
			=FPUTS(nResultsFileHandler,'Hora de inicio: ' + sInitTime)
			=FPUTS(nResultsFileHandler,'Hora de finalización: ' + sEndTime)			
			=FCLOSE(nResultsFileHandler)
		ENDIF

		DO cleanTheHouse IN IMPORTAR_CLICK
	ENDIF
ENDPROC


PROCEDURE makeNotification(sExportationPath as String, sNotificationWeek as Byte, sNotificationYear as Integer, ;
							sNotifierKind as String,  sNotifierId as String, bDoNotShowTerminationMsg as Boolean, ;
							sSIVIGILAVersion as String)
	

	LOCAL sOldProc AS STRING
	sOldProc = SET("Procedure")
	
	LOCAL sOldDeleted as String
		
	PUBLIC sPlainsFilePath AS String

	SET PROCEDURE TO 
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	SET PROCEDURE TO (PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAMessenger') ADDITIVE
	SET PROCEDURE TO (PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAUtilities') ADDITIVE

	DO cleanTheHouse IN IMPORTAR_CLICK

	*Crea la tabla PLANO en donde se almacenarán en forma temporal los datos a exportar
	DO CreateExportationPlainTable

	*Crea la carpeta en donde se almacenarán los archivos resultado de la exportación
	sPlainsFilePath = sExportationPath + '\PLANOS_SEMANA_' + PADL(sNotificationWeek,2,'0') + '_RENOTIFICADO'
	IF !DIRECTORY(sPlainsFilePath)
		MKDIR (sPlainsFilePath)
	ELSE
		sDelCmd='DELETE FILE ' + '"' + sPlainsFilePath + '\*.*"'
		&sDelCmd
	ENDIF

	*Fija los nombres de los archivos planos que almacenarán la información exportada desde las tablas del Sistema
	sUPGDsFileNameAndPath = ''
	sUCIsFileNameAndPath = ''
	sHumanResourcesFileNameAndPath = ''
	sPatientsFileNameAndPath = ''
	sEventsDataFileNameAndPath = ''
	sCollectiveEventsFileNameAndPath = ''
	sTotalsFileNameAndPath = ''
	sControlFileNameAndPath = ''
	sSIVIGILAFileNameAndPath = ''
	
	* A partir de la versión 2012 no se genera el archivo W
	sWEBPatientsFileNameAndPath=''
	sIRAFileNameAndPath = ''
	sNotifierFileNamePrefix=''
	DO setOutputTxtFileNames WITH sNotifierKind, sNotifierId, sNotificationWeek, sPlainsFilePath, sNotifierFileNamePrefix,;
				sUPGDsFileNameAndPath, sHumanResourcesFileNameAndPath, sPatientsFileNameAndPath, ;
				sEventsDataFileNameAndPath, sCollectiveEventsFileNameAndPath, sTotalsFileNameAndPath ,;
				sControlFileNameAndPath, sSIVIGILAFileNameAndPath, sWEBPatientsFileNameAndPath ,;
				sIRAFileNameAndPath, sUCIsFileNameAndPath

	SET TALK ON

	*Inicializa la tabla SivigilaDataTables de tal forma que refleje que ningún archivo ha sido producido
	UPDATE SivigilaDataTables SET EXPORTED = .F.
	
	*Establece una línea base de pacientes para los cuales se exportarán los registros de datos complementarios, laboratorios, UPGDs;
	y talento humano teniendo en cuenta las condiciones de filtrado sDataFilter
	=showProgressMessage('Estableciendo una línea base de Notificación Individual')
	sDataFilter='recordSource.NOTIFICA=' + ALLTRIM(STR(sNotificationWeek)) + ' AND YEAR(recordSource.Fec_aju) =' + ALLTRIM(STR(sNotificationYear)) 
	sSQLClause= "SELECT DISTINCT RecordSource.AÑO, RecordSource.SEMANA, RecordSource.COD_EVE, RecordSource.TIP_IDE," +;
				" RecordSource.NUM_IDE, RecordSource.COD_PRE, RecordSource.COD_SUB" +;
				" FROM PACIENTE AS RecordSource WHERE " + sDataFilter +;
				" INTO CURSOR cPatientsBaseLine NOFILTER"
	&sSQLClause
 	 	
 	bCheckEnabledUPGD=.F.
 	bNotCheckDuplicates=.T.
 	sFieldDelimiter=","
 	*Por default, si la exportación de datos se refiere a volver a notificar datos ya notificados, ;
 	entonces se exporta la información de todas las UPGDs para las que exista algún tipo de notificación ;
 	sin tener en cuenta si están activas o no. ;
 	Además, se obvia la comprobación de existencia de duplicados y los campos de los archivos texto ;
 	resultantes del	proceso, se delimitarán por comas

	IF _TALLY>0 THEN
		* Genera el archivo N de datos básicos de notificación individual periódica semanal
		=showProgressMessage('Generando Archivo Plano de Notificación Individual')
		exportationIsOK = ExportFromQRY('PTE', sDataFilter,	, sPlainsFilePath, bCheckEnabledUPGD, ;
										sFieldDelimiter , , bNotCheckDuplicates) 
		SELECT PLANO
		COPY TO (sPatientsFileNameAndPath) SDF
		nNExportedRegs=_TALLY
		
		UPDATE SivigilaDataTables SET EXPORTED = .T. WHERE SHORTNAME = 'PTE'
	ELSE
		nNExportedRegs=0
	ENDIF
	
 	*Establece las UPGDs que se encuentran en la línea base de pacientes
 	sSQLClause= "SELECT DISTINCT RecordSource.COD_PRE, RecordSource.COD_SUB" +;
				" FROM cPatientsBaseLine AS RecordSource INTO CURSOR cUPGDsBaseLine READWRITE"
	&sSQLClause

	*Establece una línea base de BROTES para los cuales se exportarán los registros teniendo en cuenta;
	las condiciones de filtrado sDataFilter
	=showProgressMessage('Estableciendo una línea base de Notificación Colectiva')
	sDataFilter='recordSource.NOTIFICA=' + ALLTRIM(STR(sNotificationWeek)) + ' AND YEAR(recordSource.Fec_aju) =' + ALLTRIM(STR(sNotificationYear)) 
	sSQLClause= "SELECT DISTINCT RecordSource.AÑO, RecordSource.SEMANA, RecordSource.COD_EVE, RecordSource.COD_MUN," +;
				" RecordSource.NUM_CON, RecordSource.COD_PRE, RecordSource.COD_SUB" +;
				" FROM BROTES AS RecordSource WHERE " + sDataFilter +;
				" INTO CURSOR cCollectiveEventsBaseLine"
	&sSQLClause
	nCollectiveEventsBaseLineRegs=_TALLY
	
	* Genera el archivo D de datos complementarios incluyendo la información de resultados de laboratorio incluyendo, ;
	registros que han sido borrados del sistema
	SELECT PLANO
	ZAP

	sOldDeleted = SET("Deleted")
	SET DELETED OFF

	SELECT DISTINCT DBF_NAME, DEPENDS_ON, SHORTNAME, LINKING_EX, SINGLE_NOT FROM SIVIGILADataTables WHERE Activo INTO CURSOR rsDataTables
	SELECT rsDataTables
	SCAN 
		IF rsDataTables.DEPENDS_ON = 'PACIENTE' THEN
			*La tabla de datos complementarios corresponde a una de un evento individual que obedece el esquema DB + DC

			sEventKeyWithPlus = qualifyFields(rsDataTables.LINKING_EX, '+', 'RecordSource')
			sPatientsBaseLineKeyWithPlus = qualifyFields(rsDataTables.LINKING_EX, '+', 'cPatientsBaseLine')
			
			=showProgressMessage('Generando Archivo Plano del Grupo de Eventos ' + rsDataTables.SHORTNAME)
			exportationIsOK = ExportFromQRY(ALLTRIM(rsDataTables.SHORTNAME) , sEventKeyWithPlus + ;
											' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)',;
											, sPlainsFilePath, bCheckEnabledUPGD, sFieldDelimiter , , ;
											bNotCheckDuplicates)
			UPDATE SivigilaDataTables SET EXPORTED = .T. WHERE SHORTNAME = ALLTRIM(rsDataTables.SHORTNAME)
		ELSE
			IF !rsDataTables.SINGLE_NOT THEN
				* El evento NO debe generarse en un archivo independiente					
				
				=showProgressMessage('Generando Archivo Plano del Grupo de Eventos ' + rsDataTables.SHORTNAME)
				
				IF rsDataTables.DEPENDS_ON = 'BROTES' THEN
					*Los datos complementarios de este evento están ligados a un brote
					sEventKeyWithPlus = qualifyFields(rsDataTables.LINKING_EX, '+', 'RecordSource')
					sCollectiveEventsBaseLineKeyWithPlus = qualifyFields(rsDataTables.LINKING_EX, '+', 'cCollectiveEventsBaseLine')

					sDataFilter='recordSource.NOTIFICA=' + ALLTRIM(STR(sNotificationWeek)) + ' AND ' + sEventKeyWithPlus + ;
								' IN (SELECT ' + sCollectiveEventsBaseLineKeyWithPlus + ' FROM cCollectiveEventsBaseLine)'

				ELSE
					sDataFilter='recordSource.NOTIFICA=' + ALLTRIM(STR(sNotificationWeek)) + ' AND YEAR(recordSource.Fec_aju) =' + ALLTRIM(STR(sNotificationYear)) 
				ENDIF
				exportationIsOK = ExportFromQRY(ALLTRIM(rsDataTables.SHORTNAME), sDataFilter,;
												, sPlainsFilePath, bCheckEnabledUPGD, sFieldDelimiter, , ;
												bNotCheckDuplicates)
												
				UPDATE SivigilaDataTables SET EXPORTED = .T. WHERE SHORTNAME = ALLTRIM(rsDataTables.SHORTNAME)
			ENDIF
		ENDIF
	ENDSCAN 

	SELECT PLANO
	COPY TO (sEventsDataFileNameAndPath) SDF
	nDExportedRegs=_TALLY

	*Actualiza la línea base de UPGDs
 	sSQLClause= "INSERT INTO cUPGDsBaseLine (COD_PRE, COD_SUB) SELECT DISTINCT STREXTRACT(PARTE1,',',',',2) AS COD_PRE," +;
 				" STREXTRACT(PARTE1,',',',',3) AS COD_SUB FROM PLANO"
	&sSQLClause


	* Genera el archivo B de datos de notificación colectiva periódica semanal obligatoria
	SELECT PLANO
	ZAP

	IF nCollectiveEventsBaseLineRegs>0 THEN
		=showProgressMessage('Generando Archivo Plano de Notificación Colectiva')
		sDataFilter='recordSource.NOTIFICA=' + ALLTRIM(STR(sNotificationWeek)) + ' AND YEAR(recordSource.Fec_aju) =' + ALLTRIM(STR(sNotificationYear))
		exportationIsOK = ExportFromQRY('BRO' , sDataFilter, , sPlainsFilePath,;
										 bCheckEnabledUPGD, sFieldDelimiter , , bNotCheckDuplicates)
		SELECT PLANO
		COPY TO (sCollectiveEventsFileNameAndPath) SDF
		nBExportedRegs=_TALLY
		
		UPDATE SivigilaDataTables SET EXPORTED = .T. WHERE SHORTNAME = 'BRO'
	ELSE
		nBExportedRegs=0
	ENDIF
	
	
	*Actualiza la línea base de UPGDs
 	sSQLClause= "INSERT INTO cUPGDsBaseLine (COD_PRE, COD_SUB) SELECT DISTINCT STREXTRACT(PARTE1,',',',',2) AS COD_PRE," +;
 				" STREXTRACT(PARTE1,',',',',3) AS COD_SUB FROM PLANO"
	&sSQLClause


	* Genera el archivo U contenedor de datos de caracterización de UPGD
	SELECT PLANO
	ZAP

	nUExportedRegs = 0
	SELECT rsDataTables
	LOCATE FOR DBF_NAME = 'UPGD'
	IF FOUND() THEN 
		sUPGDKeyWithPlus1 = fieldValue('TAG_EXPR','SivigilaPKs','TABLE_NAME="' + ALLTRIM(rsDataTables.DBF_NAME) + '"')
		IF !ISNULL(sUPGDKeyWithPlus1) THEN 
			sUPGDKeyWithPlus = qualifyFields(sUPGDKeyWithPlus1, '+', 'RecordSource')
			sUPGDsBaseLineKeyWithPlus = qualifyFields(sUPGDKeyWithPlus1, '+', 'cUPGDsBaseLine')

			=showProgressMessage('Generando Archivo Plano de las UPGD')
			exportationIsOK = ExportFromQRY(ALLTRIM(rsDataTables.SHORTNAME), sUPGDKeyWithPlus + ' IN (SELECT &sUPGDsBaseLineKeyWithPlus FROM cUPGDsBaseLine)',;
							  				, sPlainsFilePath, bCheckEnabledUPGD, sFieldDelimiter , , ;
							  				bNotCheckDuplicates)
			SELECT PLANO
			COPY TO (sUPGDsFileNameAndPath) SDF
			nUExportedRegs=_TALLY
			
			UPDATE SivigilaDataTables SET EXPORTED = .T. WHERE SHORTNAME = ALLTRIM(rsDataTables.SHORTNAME)
		ENDIF
	ENDIF
	
	* Genera el archivo V contenedor de datos de caracterización de UCI en las UPGD
	SELECT PLANO
	ZAP

	SELECT rsDataTables
	LOCATE FOR DBF_NAME = 'UPGD_UCIS'
	nVExportedRegs = 0
	IF FOUND() THEN
		sUCIKeyWithPlus = qualifyFields(rsDataTables.LINKING_EX, '+', 'RecordSource')
		sUPGDsBaseLineKeyWithPlus = qualifyFields(rsDataTables.LINKING_EX, '+', 'cUPGDsBaseLine')
	
		=showProgressMessage('Generando Archivo Plano de caracterización de UCI')
		exportationIsOK = ExportFromQRY(ALLTRIM(rsDataTables.SHORTNAME) , sUCIKeyWithPlus + ;
										' IN (SELECT &sUPGDsBaseLineKeyWithPlus FROM cUPGDsBaseLine)',;
						  				, sPlainsFilePath, bCheckEnabledUPGD, sFieldDelimiter , ,;
						  				bNotCheckDuplicates)
		SELECT PLANO
		COPY TO (sUCIsFileNameAndPath) SDF
		nVExportedRegs = _TALLY
		
		UPDATE SivigilaDataTables SET EXPORTED = .T. WHERE SHORTNAME = ALLTRIM(rsDataTables.SHORTNAME)
	ENDIF
	
	* Genera el archivo R contenedor de datos sobre el talento humano disponible en las UPGD
	SELECT PLANO
	ZAP

	SELECT rsDataTables
	LOCATE FOR DBF_NAME = 'TAL_HUM'
	nRExportedRegs = 0
	IF FOUND() THEN
		sTALKeyWithPlus = qualifyFields(rsDataTables.LINKING_EX, '+', 'RecordSource')
		sUPGDsBaseLineKeyWithPlus = qualifyFields(rsDataTables.LINKING_EX, '+', 'cUPGDsBaseLine')

		=showProgressMessage('Generando Archivo Plano de Talento Humano / Servicios')
		exportationIsOK = ExportFromQRY(ALLTRIM(rsDataTables.SHORTNAME) , sUPGDKeyWithPlus + ;
										' IN (SELECT &sUPGDsBaseLineKeyWithPlus FROM cUPGDsBaseLine)',;
						  				, sPlainsFilePath, bCheckEnabledUPGD, sFieldDelimiter , ,;
						  				bNotCheckDuplicates)
		SELECT PLANO
		COPY TO (sHumanResourcesFileNameAndPath) SDF
		nRExportedRegs = _TALLY
		
		UPDATE SivigilaDataTables SET EXPORTED = .T. WHERE SHORTNAME = ALLTRIM(rsDataTables.SHORTNAME)
	ENDIF 

	* Genera el archivo I contenedor de datos del evento colectivo morbilidad por IRA (Vigilancia Centinela ESI - IRAG);
	SELECT PLANO
	ZAP

	SELECT DISTINCT DBF_NAME, DEPENDS_ON, SHORTNAME, LINKING_EX, SINGLE_NOT FROM SIVIGILADataTables WHERE Activo ;
		AND SINGLE_NOT AND !EXPORTED INTO CURSOR rsDataTables
	SELECT rsDataTables
	SCAN
		sDataFilter='recordSource.NOTIFICA=' + ALLTRIM(STR(sNotificationWeek)) + ' AND YEAR(recordSource.Fec_aju) =' + ALLTRIM(STR(sNotificationYear)) 
		=showProgressMessage('Generando Archivo Plano del Grupo de Eventos ' + rsDataTables.SHORTNAME)
		exportationIsOK = ExportFromQRY(ALLTRIM(rsDataTables.SHORTNAME), sDataFilter,;
										, sPlainsFilePath, bCheckEnabledUPGD, sFieldDelimiter , , ;
										bNotCheckDuplicates)
		SELECT PLANO
		COPY TO (sIRAFileNameAndPath) SDF
		nIExportedRegs = _TALLY
		
		UPDATE SivigilaDataTables SET EXPORTED = .T. WHERE SHORTNAME = ALLTRIM(rsDataTables.SHORTNAME)
	ENDSCAN


	* Genera el archivo W de notificación datos básicos para la Web 
	* A partir de la versión2012, ya no se genera el archivo W
	*IF nNExportedRegs>0 THEN
	*	SELECT PLANO
	*	ZAP

	*	TIT('Generando Archivo Plano de notificación datos básicos para la Web')
	*	sDataFilter='recordSource.NOTIFICA=' + ALLTRIM(STR(sNotificationWeek)) + " AND YEAR(recordSource.Fec_aju) =" + ALLTRIM(STR(sNotificationYear)) 
	*	sFieldDelimiter=";"
	*	exportationIsOK = ExportFromQRY('PTW', sDataFilter,	@aPlainTableKeys, sPlainsFilePath, bCheckEnabledUPGD, ;
										sFieldDelimiter, .T. , bNotCheckDuplicates) 
	*	SELECT PLANO
	*	sCopyCmd="COPY TO " + '"' + sWEBPatientsFileNameAndPath + '"' + " DELIMITED WITH "
	*	sCopyCmd=sCopyCmd+ '"' + '"' + ' WITH CHARACTER' + '"' + '"'
	*	&sCopyCmd
	*	nWExportedRegs=_TALLY
	*ELSE
	*	nWExportedRegs=0
	*ENDIF

	*Genera el archivo SIVIGILA contenedor de datos de cuándo y con qué versión fueron generados ;
	los archivos anterioreS
	DO makeSIVIGILAVersionFile WITH (sSIVIGILAFileNameAndPath), sSIVIGILAVersion 

	*Genera el archivo de control
	DO makeControlFile WITH sControlFileNameAndPath, sNotifierId, sNotifierFileNamePrefix, nUExportedRegs, ;
							nRExportedRegs, nNExportedRegs, nBExportedRegs,	-1, nDExportedRegs, -1, nIExportedRegs, nVExportedRegs

	*Codifica los archivos recien producidos
	sSivigilaCryptoFileNameAndPath = PATH_TO_SIVIGILACRYPTO + "SivigilaCrypto.prg"
	oCodifier  = NEWOBJECT( "SivigilaCrypto", sSivigilaCryptoFileNameAndPath, .NULL., sPlainsFilePath, "*.Txt", sPlainsFilePath)
	oCodifier.BatchCodify
	RELEASE oCodifier

	*Produce un archivo empaquetado en formato ZIP para envío
	TRY
		sZipFileName = STREXTRACT(sPlainsFilePath,'\','',OCCURS('\',sPlainsFilePath)) + '.ZIP'
		sResultMsg=''
		DO zip WITH (sPlainsFilePath), '*.TXT', sExportationPath , (sZipFileName), sResultMsg
		IF EMPTY(sResultMsg) THEN
			*COPY FILE (sZipFileName) TO (sExportationPath) 
		ELSE
			sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
						sResultMsg + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
						"Debe crear usted mismo el archivo ZIP para envío."
			=showErrorMessage(sResultMsg, 0)
		ENDIF
		*DELETE FILE (sZipFileName)
	CATCH TO oException
		sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
					"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
					"Mensaje del problema : "  + (oException.Message) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
					"Debe crear usted mismo el archivo ZIP para envío."
		=showErrorMessage(sResultMsg, 0)
	ENDTRY

	
	IF !bDoNotShowTerminationMsg THEN
		=showProgressMessage('Proceso Terminado')
		=showErrorMessage('SE HAN GENERADO LOS SIGUIENTES ARCHIVOS DE TEXTO!'+CHR(13)+CHR(13)+;
		   RIGHT(sUPGDsFileNameAndPath,8)+ '.TXT con ' +TRANS(nUExportedRegs,'@l 99999999')+' - Registros de UPGD'+CHR(13)+;
		   RIGHT(sHumanResourcesFileNameAndPath,8)+ '.TXT con ' +TRANS(nRExportedRegs,'@l 99999999')+' - Registros de Talento H / Servicios'+CHR(13)+;
		   RIGHT(sPatientsFileNameAndPath,8)+ '.TXT con ' +TRANS(nNExportedRegs,'@l 99999999')+' - Registros de Not. Individual'+CHR(13)+;
		   RIGHT(sEventsDataFileNameAndPath,8)+ '.TXT con ' +TRANS(nDExportedRegs,'@l 99999999')+' - Registros de Datos Complementarios'+CHR(13)+;
		   RIGHT(sCollectiveEventsFileNameAndPath,8)+ '.TXT con ' +TRANS(nBExportedRegs,'@l 99999999')+' - Registros de Not. Colectiva'+CHR(13)+;
		   RIGHT(sIRAFileNameAndPath,8)+ '.TXT con ' +TRANS(nIExportedRegs,'@l 99999999')+' - Registros de Morb. por IRA' + CHR(13) +;
		   'UBÍQUELOS EN '+ sPlainsFilePath + CHR(13) + CHR(13) + ;
		   'POR FAVOR ENVIE EL ARCHIVO COMPRIMIDO: ' + CHR(13) + sPlainsFilePath + '.ZIP' + CHR(13)+;
		   'AL INSTITUTO NACIONAL DE SALUD MAIL: sivigila@ins.gov.co',0)
		   
		   *A partir de la versión 2012 no se genera el archivo W
		   *RIGHT(sWEBPatientsFileNameAndPath,8)+ '.TXT con ' +TRANS(nWExportedRegs,'@l 99999999')+' - Registros de Not. WEB'+CHR(13)
	ENDIF
	
	DO cleanTheHouse IN IMPORTAR_CLICK
	SET TALK OFF
	
	SET PROCEDURE TO &sOldProc 
	SET DELETED &sOldDeleted 
ENDPROC


PROCEDURE makeSIVIGILAVersionFile
	LPARAMETERS sSIVIGILAFileNameAndPath as String, sSIVIGILAVersion as String
	
	SET DEVICE TO FILE (sSIVIGILAFileNameAndPath)
	@1,5 SAY 'INSTITUTO NACIONAL DE SALUD'
	&&@2,5 SAY 'SIVIGILA - ' + sSIVIGILAVersion 
	@2,5 SAY sSIVIGILAVersion 
	@3,5 SAY 'FECHA DE GENERACIÓN: '+TF(DATE())
	@4,5 SAY 'VERSIÓN: ' + NC(nCurrentMajor) + '.' + NC(nCurrentMinor) + '.' + NC(nCurrentBuild)
	SET DEVICE TO SCREEN
ENDPROC


PROCEDURE makeControlFile
	LPARAMETERS sControlFileNameAndPath as String, sNotifierId as String, sNotifierFileNamePrefix as String, ;
				nUExportedRegs as Numeric, nRExportedRegs as Numeric, nNExportedRegs as Numeric, nBExportedRegs as Numeric, ;
				nTExportedRegs as Numeric, nDExportedRegs as Numeric, nWExportedRegs  as Numeric, nIExportedRegs as Numeric, ;
				nVExportedRegs as Numeric
				

		L=0
		SET DEVICE TO FILE (sControlFileNameAndPath)
		IF nUExportedRegs>0
		   @L+0,0 SAY LEFT(sNotifierId ,10)+','+RIGHT(sNotifierId ,2)+','+TF(DATE())+','+'U'+sNotifierFileNamePrefix+','+TRANS(nUExportedRegs,'@l 9999999')
		   L=L+1
		ENDIF
		IF nRExportedRegs>0
		   @L+0,0 SAY LEFT(sNotifierId ,10)+','+RIGHT(sNotifierId ,2)+','+TF(DATE())+','+'R'+sNotifierFileNamePrefix+','+TRANS(nRExportedRegs,'@l 9999999')
		   L=L+1
		ENDIF
		IF nNExportedRegs>0
		   @L+0,0 SAY LEFT(sNotifierId ,10)+','+RIGHT(sNotifierId ,2)+','+TF(DATE())+','+'N'+sNotifierFileNamePrefix+','+TRANS(nNExportedRegs,'@l 9999999')
		   L=L+1
		ENDIF
		IF nBExportedRegs>0
		   @L+0,0 SAY LEFT(sNotifierId ,10)+','+RIGHT(sNotifierId ,2)+','+TF(DATE())+','+'B'+sNotifierFileNamePrefix+','+TRANS(nBExportedRegs,'@l 9999999')
		   L=L+1
		ENDIF
		IF nTExportedRegs>0
		   @L+0,0 SAY LEFT(sNotifierId ,10)+','+RIGHT(sNotifierId ,2)+','+TF(DATE())+','+'T'+sNotifierFileNamePrefix+','+TRANS(nTExportedRegs,'@l 9999999')
		   L=L+1
		ENDIF
		IF nDExportedRegs>0
		   @L+0,0 SAY LEFT(sNotifierId ,10)+','+RIGHT(sNotifierId ,2)+','+TF(DATE())+','+'D'+sNotifierFileNamePrefix+','+TRANS(nDExportedRegs,'@l 9999999')
		   L=L+1
		ENDIF
		*A partir de la versión 2012 ya no se genera el archivo W
		*IF nWExportedRegs>0
		*   @L+0,0 SAY LEFT(sNotifierId ,10)+','+RIGHT(sNotifierId ,2)+','+TF(DATE())+','+'W'+sNotifierFileNamePrefix+','+TRANS(nWExportedRegs,'@l 9999999')
		*   L=L+1
		*ENDIF
		IF nIExportedRegs>0
		   @L+0,0 SAY LEFT(sNotifierId ,10)+','+RIGHT(sNotifierId ,2)+','+TF(DATE())+','+'I'+sNotifierFileNamePrefix+','+TRANS(nIExportedRegs,'@l 9999999')
		   L=L+1
		ENDIF
		IF nVExportedRegs>0
		   @L+0,0 SAY LEFT(sNotifierId ,10)+','+RIGHT(sNotifierId ,2)+','+TF(DATE())+','+'V'+sNotifierFileNamePrefix+','+TRANS(nVExportedRegs,'@l 9999999')
		   L=L+1
		ENDIF
		SET DEVICE TO SCREEN
ENDPROC


PROCEDURE setOutputTxtFileNames

	LPARAMETERS sNotifierKind as String,  sNotifierId as String, sNotificationWeek as String, sPlainsFilePath as String, ;
				sNotifierFileNamePrefix as String, sUPGDsFileNameAndPath as String, sHumanResourcesFileNameAndPath as String,;
				sPatientsFileNameAndPath, sEventsDataFileNameAndPath  as String, sCollectiveEventsFileNameAndPath as String,;
				sTotalsFileNameAndPath as String, sControlFileNameAndPath as String, sSIVIGILAFileNameAndPath as String,;
				sWEBPatientsFileNameAndPath as String, sIRAFileNameAndPath as String, sUCIsFileNameAndPath AS String
	

	*Establece el prefijo identificador de la Unidad que notifica para efectos de nombrar los archivos;
	planos resultantes del procesamiento
	DO CASE
		CASE sNotifierKind = UPGD_ID
			*El nombre del archivo plano debe ser SIGLA + CODIGO UPGD +  'RA'.txt;
			 por tanto inmediatamente después de la Sigla deben venir los cinco dígitos asignados consecutivamente;
			 al prestador de servicios de salud
			sNotifierFileNamePrefix =SUBSTR(ALLTRIM(sNotifierId),6,5) + PADL(sNotificationWeek  ,2,'0')
		CASE sNotifierKind = UNM_ID
			*El nombre del archivo plano debe ser SIGLA + CODIGO MUNICIPIO +  SEMANA EPIDEMIOLÓGICA.txt;
			 por tanto inmediatamente después de la Sigla deben venir los cinco dígitos asignados por el DANE para ;
			 identificar los municipios
			sNotifierFileNamePrefix =SUBSTR(ALLTRIM(sNotifierId),1,5) + PADL(sNotificationWeek  ,2,'0')
		CASE sNotifierKind = UND_ID
			*El nombre del archivo plano debe ser SIGLA + CODIGO DEPARTAMENTO +  SEMANA EPIDEMIOLÓGICA.txt;
			 por tanto inmediatamente después de la Sigla deben venir los dos dígitos asignados por el DANE para;
			 identificar los departamentos (Se debe agregar 3 ceros al código del departamento)
			sNotifierFileNamePrefix =SUBSTR(ALLTRIM(sNotifierId),1,2) + '000' + PADL(sNotificationWeek  ,2,'0')
	ENDCASE
	sUPGDsFileNameAndPath			= sPlainsFilePath +'\U'+sNotifierFileNamePrefix
	sUCIsFileNameAndPath			= sPlainsFilePath +'\V'+sNotifierFileNamePrefix
	sHumanResourcesFileNameAndPath	= sPlainsFilePath +'\R'+sNotifierFileNamePrefix 
	sPatientsFileNameAndPath		= sPlainsFilePath +'\N'+sNotifierFileNamePrefix 
	sEventsDataFileNameAndPath		= sPlainsFilePath +'\D'+sNotifierFileNamePrefix 
	sCollectiveEventsFileNameAndPath= sPlainsFilePath +'\B'+sNotifierFileNamePrefix 
	sTotalsFileNameAndPath			= sPlainsFilePath +'\T'+sNotifierFileNamePrefix 
	sControlFileNameAndPath			= sPlainsFilePath +'\C'+sNotifierFileNamePrefix 
	sSIVIGILAFileNameAndPath		= sPlainsFilePath +'\SIVIGILA.TXT'
	sWEBPatientsFileNameAndPath		= sPlainsFilePath +'\W'+sNotifierFileNamePrefix 
	sIRAFileNameAndPath				= sPlainsFilePath +'\I'+sNotifierFileNamePrefix 
ENDPROC


PROCEDURE addDupsToErrorTable

	PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName, errorMsg

	* RSName: nombre de una tabla o cursor que contiene los registros que se deben agregar a la tabla ;
		ERRORES
	* sourceFileName: nombre y ubicación de la tabla fuente que contiene todos los ;
		registros (duplicados y no duplicados que deberían ser almacenados en la tabla maestra ;
		de Pacientes					
	*sourceFileType: indicador del tipo de archivo fuente así: ;
		N = datos básicos de notificación individual ;
		B = datos de notificación colectiva ;
		U = datos de caracterización de unidades primarias generadoras del dato UPGD ;
		R = datos sobre el talento humano ;
		D = datos complementarios (o de eventos de salud) ;

	* errorsFilePath, errorsFileName: nombre y ubicación de la tabla en donde deben guardarse ;
	los registros duplicados 

	* errorMsg (OPCIONAL): mensaje de error con el que deben agregarse los registros de RSName a la tabla ERRORES
				
	* Abre la tabla de reporte de Errores
	IF !USED(errorsFileName) THEN
		USE (errorsFilePath+ '\' +  errorsFileName) IN 0
	ENDIF

	*Fija la descripción del archivo fuente para efectos de reporte del error
	DO CASE
		CASE sourceFileType='N'
			sourceFileDescription='Not. Individual'
		CASE sourceFileType='B'
			sourceFileDescription='Not. Colectiva '
		CASE sourceFileType='U'
			sourceFileDescription='Caracterización de unidades primarias'
		CASE sourceFileType='R'
			sourceFileDescription='Datos sobre el talento humano'
		CASE sourceFileType='D'
			sourceFileDescription='Datos complementarios'
		CASE sourceFileType='V'
			sourceFileDescription='Datos sobre UCI en UPGD'
	ENDCASE

	IF PCOUNT()=6 THEN
		lErrorMsg = "'" + errorMsg + "' + " + "COD_PRE + '-' + COD_SUB"
	ELSE
		lErrorMsg = "'UPGD DE DONDE PROVIENEN LOS DUPLICADOS: ' + COD_PRE + '-' + COD_SUB"
	ENDIF
		
		
	insertRecsClause='INSERT INTO ERRORES (ARCHIVO, REG, DES_ARC, COD_PRE, COD_SUB, EVENTO,' ;
		+ 'TIP_IDE, NUM_IDE, VARIABLE) SELECT ' + '"' + sourceFileType + '"+' + 'SUBSTR(' + '"' + sourceFileName + '"' + ',2,7) AS ARCHIVO,' ;
		+ 'nReg,' + '"' + sourceFileDescription + '"' + ' AS DES_ARC,COD_PRE,COD_SUB,COD_EVE,TIP_IDE,NUM_IDE' ;
		+ ',' + lErrorMsg + ' FROM ' + (RSName)
	&insertRecsClause	

ENDPROC


*-----------------------------------------------------------------------
PROCEDURE makeImportTables(_FilePath, _SvgVersion, nMajor as Byte, nMinor as Byte, nBuild as byte)
	* Autor: Wilson Aguilar
	* Crea en la carpeta _FilePath las tablas DBF necesarias para cargar los datos de archivos planos;
	* teniendo en cuenta la versión del sistema _SvgVersion con la que fueron generados.
	
	*Si se pasan los parámetros opcinales nMajor, nMinor y nBuild y _SvgVersion=_VERSION2010 ;
	también se tendrá en cuenta el identificador específico de versión dado por nMajor.nMinor.nBuild

	LOCAL sOldExclusive as String


	WAIT WINDOW NOWAIT ' Creando tablas intermedias para cargar los planos ...'

	sOldExclusive = SET("Exclusive")
	SET EXCLUSIVE ON
	
	*Verifica la existencia de la carpeta que se va a utilizar para crear las tablas intermedias en donde se;
	cargará la información de los archivos planos
	IF !DIRECTORY(_FilePath)
		SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		=showErrorMessage('No existe la carpeta ' + _Filepath,1)
		RETURN
	ENDIF
*SET STEP ON 
	IF _SvgVersion = _VERSION2010 THEN
		*Las tablas de importación se crean con base en los queries de exportación usados en la versión 2010

		IF PCOUNT()=2 THEN
			nMajor=0
			nMinor=0
			nBuild=0
		ENDIF

		SELECT DISTINCT impTableNa, ShortName FROM ExportQueries WHERE Activo INTO CURSOR rsTablesToMake		
		SELECT rsTablesToMake
		SCAN
			_EventFName = ALLTRIM(rsTablesToMake.impTableNa)
			compleTableName = ''
			makeImporTableSQLClause = GetQryString(UPPER(rsTablesToMake.ShortName), @compleTableName, nMajor, nMinor, nBuild)

			IF !EMPTY(makeImporTableSQLClause) THEN
				* SE AGREGAN LOS CAMPOS FALTANES, NECESARIOS PARA LA IMPORTACION 
				modifyImporTableSQLClause(compleTableName, @makeImporTableSQLClause)
				makeImporTableSQLClause= makeImporTableSQLClause+ " WHERE .F. INTO CURS C_TEMP "
				&makeImporTableSQLClause

				SELECT C_TEMP
				COPY STRUCTURE TO [&_FilePath\&_EventFName]
				createPlainTableIndex(_FilePath + '\', _EventFName)
			ENDIF

		ENDSCAN

		checkAndClose('rsTablesToMake')

		*Crea la tabla para importar los eventos de búsqueda activa cuya definición para la versión 2010;
		es igual a la de la versión 2008
		_EventFName = 'PLANO_EBA'
		createPlainTable(_FilePath + '\', _EventFName )
		
	ENDIF

	IF _SvgVersion = _VERSION2008 THEN
		*Las tablas de importación se crean con base en las definiciones de tablas de la versión 2008
		
		SELECT DISTINCT table_name FROM MASTER_PLAINS INTO CURSOR C_FILES2008
		
		SELECT C_FILES2008
		SCAN
			SCATTER MEMVAR
			_EventFName = ALLTRIM(m.table_name)
			createPlainTable(_FilePath + '\', _EventFName )
			createPlainTableIndex(_FilePath + '\', _EventFName)
		ENDSCAN
		checkAndClose('MASTER_PLAINS')
		checkAndClose('C_FILES2008')
	ENDIF

	*Crea la tabla intermedia PLANO_EVENTOS necesaria para cargar los datos complementarios de los eventos antes;
	de ser enviados a las tablas PLANO_Exx y PLANO_LAB
	_FILE = '[' + _FilePath + '\PLANO_EVENTOS]'
	CREATE TABLE &_FILE FREE (;
						SEMANA C(2),;
						AÑO C(4),;
						COD_PRE C(10),;
						COD_SUB C(2),;
						COD_EVE C(4),;
						TIP_IDE C(2),;
						NUM_IDE C(17),;
						CONTROL C(12))
	createPlainTableIndex(_FilePath + '\', 'PLANO_EVENTOS')

	SET EXCLUSIVE &sOldExclusive
ENDPROC

*----------------------------------------------------------------------------------------------------------------
PROCEDURE modifyImporTableSQLClause(TName, sQueryString)
	* Agrega nuevas columnas al string (sQueryString) que crea la tabla (TName) destinada a almacenar los planos
	
	LOCAL sNewColumns
	sNewColumns = ""

	DO CASE
		CASE  'PACIENTE' $ TName 
			sNewColumns = ", SPACE(5) AS COD_MUN, SPACE(5) AS MUN_PRO, SPACE(50) AS PAIS, SPACE(6) AS CLA_PER, 0 AS SUBE, 0000000 AS NUM_REG FROM "

		CASE 'T_NONDUPS' $ TName 
			sNewColumns = ", SPACE(6) AS CLA_PER, 0 AS SUBE FROM "

		CASE 'BROTES' $ TName
			sNewColumns = ", SPACE(5) AS MUN_PRO, 0000000 AS NUM_REG, 0 AS SUBE FROM "

		CASE ('LABORATORIOS' $ TName) OR ('UPGD' $ TName) OR ('TAL_HUM' $ TName) OR ('CONTACTOS' $ TName)
			sNewColumns = ", 0000000 AS NUM_REG, 0 AS SUBE FROM "
			
		CASE 'EVENTOS' $ TName
			sNewColumns = ", 0000000 AS NUM_REG, 0 AS SUBE FROM "
			IF 'EVENTOS_29' $ TName THEN
				sNewColumns = ", 0000000 AS NUM_REG, 0 AS SUBE, SPACE(5) AS COD_MUN FROM "
			ENDIF
	ENDCASE
	
	IF !EMPTY(sNewColumns)
		sQueryString=  STRTRAN(sQueryString, "FROM", sNewColumns)
	ENDIF
	
ENDPROC

*-----------------------------------------------------------------------
FUNCTION ExportFromQry

	PARAMETERS	TableShortName, DynamicConditions, A_PlainTableKeys, duplicatesFilePath, ;
				checkEnableUPGD as Boolean, sDelimiterChar as String, bRemoveTrailingSpaces AS Boolean, ;
				bDoNotCheckDuplicates as Boolean, sUserDefinedQuery as String, ;
				bNotOrdinaryNotification as Boolean, sTargetFileNameAndPath as String

	* Guarda en una tabla de nombre PLANO la totalidad de los registros de la tabla TableShortName que satisfagan las condiciones;
	DynamicConditions. Cada registro corresponde a la concatenación de los campos definidos en el query de ;
	exportación para la tabla TableShortName según se encuentre en Tables.ExportQueries o en sUserDefinedQuery, si es que se pasa ;
	este parámetro opcional. Si se pasa el parámetro opcional sTargetFileNameAndPath, los registros se esportarán hacia el archivo ;
	texto especificado en ese parámetro.
	
	* checkEnableUPGD: Si es igual a .T., se exportan solamente los registros de TableShortName que pertenezcan;
	a UPGDs activas en el sistema SIVIGILA
	
	* sDelimiterChar: (opcional) caracter utilizado para delimitar los campos de los registros exportados de ;
	TableShortName. Por default = ","
	
	* bNotOrdinaryNotification: (opcional) Si es .F., cada registro exportado es marcado como correspondiente a una notificación ;
	rutinaria; en este caso, el query de exportación utilizado debe tener un campo llamado INMEDIATA de tipo C(2) que asumirá ;
	el valor 'NO'

	DIMENSION aPLANOFields(1)
	LOCAL nPLANOFields as Number 
	*Utilizadas para determinar el número de campos en la tabla PLANO

	LOCAL bSuccess AS Boolean 

	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE

	bSuccess = .T.
	
	MaxRowSize = 254
	StringForInsert = ''

	_Cursor = 'C_' + TableShortName

	*Garantiza que la exportación siempre se lleve a cabo de acuerdo con la última versión del sistema definida;
	por sus identificadores nMajor.nMinor.nBuild según se encuentran definidos en main
	DO resetSivigilaVersion	IN main
	
	* Recupera el query que se va a utilizar para exportar los datos
	TableLongName=''
	IF VARTYPE(sUserDefinedQuery)= 'C' AND !EMPTY(sUserDefinedQuery ) then
		_Xecute = sUserDefinedQuery 
	ELSE
		_Xecute = GetQryString(TableShortName, @TableLongName, nMajor, nMinor, nBuild)
	ENDIF
	
	IF _Xecute == 'NIL'
		RETURN .F.
	ENDIF


	*Construye un cursor con los datos que se deben exportar teniendo en cuenta que pertenezcan;
	a UPGDs activas en el sistema SIVIGILA
	IF !EMPTY(DynamicConditions)
		_Xecute = _Xecute + ' WHERE ' + DynamicConditions 
	ELSE
		_Xecute = _Xecute + ' WHERE .T.' 
	ENDIF
	IF checkEnableUPGD THEN
		_Xecute = _Xecute + ' AND (recordSource.cod_pre + ' + 'recordSource.cod_sub' + ;
				 ' IN (SELECT DISTINCT cod_pre+cod_sub AS CodUPGD FROM UPGD WHERE ACT_SIV = 1))' 
	ENDIF
	_Xecute = _Xecute + ' INTO CURSOR ' + _Cursor + ' READWRITE'

	&_Xecute
	IF _TALLY > 0
		
		IF !bNotOrdinaryNotification THEN
			SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
			IF isField(_Cursor,"INMEDIATA") THEN
				REPLACE ALL INMEDIATA WITH 'NO'
			ENDIF
		ENDIF
		
		* Determina si los registros que se van a exportar contienen duplicados
		IF !bDoNotCheckDuplicates THEN
			SELECT  IMPTABLENA FROM ExportQueries WHERE Activo AND ShortName=(TableShortName) INTO ARRAY importTableName
			SELECT  TABLENAME FROM ExportQueries WHERE Activo AND ShortName=(TableShortName) INTO ARRAY tableName
			duplicatesRecords=''

			SET PROCEDURE TO IMPORTAR_CLICK ADDITIVE
			nSourceWithDuplicates=extractDupsAndNotDups(_Cursor,@duplicatesRecords, ,importTableName(1,1))
			IF nSourceWithDuplicates>9 THEN
				* Existen registros duplicados en la fuente de exportación
				xlsFileName=duplicatesFilePath + ALLTRIM(tableName(1,1)) + '_DUPLICADOS'
				COPY TO (xlsFileName) XL5
				bSuccess = .F.
			ENDIF
			RELEASE PROCEDURE IMPORTAR_CLICK 
		ENDIF
		
		SELECT &_Cursor
		CursorFieldsQRows =  AFIELDS(A_CursorFields, _Cursor)
*SET STEP ON 
		SCAN 
			FOR I = 1 TO CursorFieldsQRows
				CursorField = exprToTransformToStr (A_CursorFields(I,1), A_CursorFields(I,2), bRemoveTrailingSpaces, ;
													A_CursorFields(I,3), A_CursorFields(I,4))
				IF VARTYPE(sDelimiterChar)!='C' THEN
					StringForInsert = StringForInsert + NVL(&CursorField,'') + ","
				ELSE
					StringForInsert = StringForInsert + NVL(&CursorField,'') + sDelimiterChar 
				ENDIF
			ENDFOR
			StringForInsert  = SUBSTR(StringForInsert, 1, LEN(StringForInsert)-1) 
			
			*Establece cuántos segmentos de longitud MaxRowSize hay en StringForInsert  
			nSegments = CEILING(LEN(StringForInsert)/MaxRowSize)
			sSQLCmd = 'INSERT INTO PLANO VALUES ('
			nNextCutBegin = 1
			FOR i = 1 TO nSegments
				sSQLCmd = sSQLCmd + 'SUBSTR(StringForInsert,' + ALLTRIM(STR(nNextCutBegin)) + ',' + ALLTRIM(STR(MaxRowSize)) + ')' + ','
				nNextCutBegin = nNextCutBegin + MaxRowSize 
			NEXT i
			
			*Establece cuántos campos tiene la tabla PLANO
			=SelectTable('PLANO',,.F.)
			nPLANOFields = AFIELDS(aPLANOFields,'PLANO')
			FOR i = 1 TO nPLANOFields - nSegments 
				sSQLCmd = sSQLCmd + "''" + ","
			NEXT i
			sSQLCmd = SUBSTR(sSQLCmd, 1, LEN(sSQLCmd)-1) + ")"
			
			*Inserta la línea StringForInsert en la tabla PLANO
			&sSQLCmd
			
			StringForInsert = ''
		ENDSCAN

		IF !bDoNotCheckDuplicates THEN
			*Cierra/Elimina las tablas temporales generadas en la detección de duplicados
			IF nSourceWithDuplicates!=0 THEN
				SELECT ALLTRIM('T_NONDUPS_' + TableShortName)
				USE
				DELETE FILE ALLTRIM('T_NONDUPS_' + TableShortName)) + '.DBF'
				SELECT (duplicatesRecords)
				USE
				DELETE FILE &duplicatesRecords..DBF
				SELECT C_Count_Dups				
				USE
			ENDIF
		ENDIF
	ENDIF

	IF VARTYPE(sTargetFileNameAndPath)='C' THEN
		SELECT PLANO
		sCopyCmd = "COPY TO '" + sTargetFileNameAndPath + "' DELIMITED WITH "
		sCopyCmd = sCopyCmd + '"' + '"' + ' WITH CHARACTER' + '"' + '"'
		&sCopyCmd
	ENDIF
	
	RETURN bSuccess

ENDFUNC

*-----------------------------------------------------------------------------------------
FUNCTION exprToTransformToStr(_FldName, _Fldtype, bRemoveTrailingSpaces as Boolean, nLength as Integer, nDecimals as Integer) as String

	*Retorna una expresión string que transforma el campo _FldName de tipo _Fldtype en un string. 
	*Si _Fldtype es C (Char) or V (Variant) y el parámetro opcional bRemoveTrailingSpaces es .T., a _FldName se le aplica;
	la función de transformación TRIM
*SET STEP ON 
	DO CASE
		CASE _Fldtype = 'C' OR _Fldtype = 'V'
			IF bRemoveTrailingSpaces THEN
				RETURN 'TRIM(' + _FldName + ')'
			ELSE
				RETURN _FldName
			ENDIF
		CASE _Fldtype = 'N' OR _Fldtype = 'I'
			RETURN 'NC(' + _FldName + ',' + ALLTRIM(STR(nLength)) + ',' + ALLTRIM(STR(nDecimals)) + ')'

		CASE _Fldtype = 'D'
			RETURN 'TF('+ _FldName + ')'

		CASE _Fldtype = 'L'
			RETURN 'TRANSFORM('+ _FldName + ',"Y")'
		
		CASE _Fldtype = 'T'
			RETURN 'TTOC('+ _FldName + ')'
				
		OTHERWISE
			RETURN "'NIL'"
	ENDCASE

ENDFUNC


*-----------------------------------------------------------------------------------------
FUNCTION GetQryString

	PARAMETERS _Tname, completeTname,  nMajor as Byte, nMinor as Byte, nBuild as byte, bGetXLSQuery as boolean, ;
				bLaunchPreCmds as Boolean, sLinkerExpr as String, bGetXlsLabQuery as Boolean, nFoundQueries as Number, ;
				sPostCmdToExec as String 

	* Recupera de la tabla ExportQueries la sentencia SQL correspondiente al Query de exportación de la tabla _Tname;
	para producir archivos planos y retorna en completeTname, como efecto de lado, el nombre largo de _Tname. 

	*Si se pasa el parámetro opcional bGetXLSQuery y es .T., retornará más bien la sentencia SQL de exportación para ;
	producir archivos XLS; además, si se encuentran definiciones adicionales de queries de exportación para la tabla _Tname,;
	los retornará en un string en donde cada query se separará del otro por el caracter '|';
	Si se pasa el parámetro opcional bGetXlsLabQuery y es .T., retornará más bien la sentencia SQL de exportación para ;
	producir archivos XLS de dados de laboratorio.
	*El parámetro bGetXlsLabQuery  tiene precedencia sobre el parámetro bGetXLSQuery, de tal forma que si el primero es .T.,;
	se retornará el string de exportación de datos de laboratorio independientemente del valor asumido por el seguno.

	*Si se pasan los parámetros opcinales nMajor, nMinor y nBuild se tendrá en cuenta el identificador específico ;
	de versión dado por nMajor.nMinor.nBuild para seleccionar el query de exportación

	STORE '' TO sQryFileName
	
	LOCAL bContinue as Boolean
	LOCAL sXLSQuery as String
	LOCAL sReturnedValue as String
	
	sReturnedValue = ''
	sPostCmdToExec = ''
	IF PCOUNT()=2 THEN
		nMajor=0
		nMinor=0
		nBuild=0
	ENDIF


	*Selecciona el query de exportación apropiado dependiendo de nMajor.nMinor.nBuild
	SELECT QryFile, TableName, Major, Minor, Build, ExcelQuery, Pre_cmd, LinkExpr, Xls_qry2, XlsLab_qry, Post_cmd  FROM ExportQueries ;
		WHERE Activo AND shortname == _Tname AND Major <= nMajor ORDER BY  Major DESC, Minor DESC, Build DESC INTO CURSOR rsCandidates1
	SELECT rsCandidates1
	bContinue = .T.
	SCAN WHILE bContinue
		SET PROCEDURE TO SivigilaSystem ADDITIVE
		IF compareVersions(ALLTRIM(TRANSFORM(rsCandidates1.Major)) + '.' + ALLTRIM(TRANSFORM(rsCandidates1.Minor)) + '.' + ALLTRIM(TRANSFORM(rsCandidates1.Build)), ;
							ALLTRIM(TRANSFORM(nMajor)) + '.' + ALLTRIM(TRANSFORM(nMinor)) + '.' + ALLTRIM(TRANSFORM(nBuild))) = -1 THEN
			bContinue = .T.
		ELSE
			bContinue = .F.
			nFoundQueries = 1
			sPostCmdToExec = rsCandidates1.Post_cmd
			sQryFileName = rsCandidates1.QryFile
			IF !bGetXlsLabQuery THEN
				IF EMPTY(rsCandidates1.Xls_qry2) THEN
					sReturnedValue = rsCandidates1.ExcelQuery
				ELSE
					sReturnedValue = rsCandidates1.ExcelQuery + '|' + rsCandidates1.Xls_qry2
					nFoundQueries = nFoundQueries + 1
				ENDIF
			ELSE
				sReturnedValue = rsCandidates1.XlsLab_qry
			ENDIF
			completeTname = rsCandidates1.TableName
			sLinkerExpr = ALLTRIM(rsCandidates1.LinkExpr)
			IF bLaunchPreCmds AND !EMPTY(rsCandidates1.Pre_cmd) THEN
				*Se deben ejecutar comandos previos a la utilización del query 
				sCmd = rsCandidates1.Pre_cmd
				&sCmd
			ENDIF
		ENDIF
	ENDSCAN
	
	IF !bGetXlsLabQuery THEN
		IF !bGetXLSQuery THEN
			Xstring1 = ''
			Xstring2 = ''
			IF !EMPTY(sQryFileName) THEN 
				FileHandler = FOPEN(sQryFileName)
				IF FileHandler > 0
					DO WHILE .T.
						Xstring2 = fgets(FileHandler)
						IF !EMPTY(Xstring2)
							XString1 = XString1 + STRTRAN(XString2,';','')
						ELSE
							EXIT
						ENDIF
					ENDDO
					FCLOSE(FileHandler)
				ENDIF
			ENDIF
			sReturnedValue = XString1
		ENDIF
	ENDIF
		
	RETURN sReturnedValue
ENDFUNC

PROCEDURE GetQryStringTst
	
	LOCAL sCompleteDCTableName as String
	LOCAL sXLSQuery as strong
	LOCAL nExportQueries as Byte

SET STEP ON 
	CLEAR 
	*sXLSQuery = GetQryString('UCI',@sCompleteDCTableName,16,1,2)
	*sXLSQuery = GetQryString('E07',@sCompleteDCTableName,17,0,1,.T.,,,,@nExportQueries)
	*sXLSQuery = GetQryString('E07',@sCompleteDCTableName,17,0,1,.F.,,,.T.)
	
	*sXLSQuery = GetQryString('E88',@sCompleteDCTableName,17,0,1,.T.,,,,@nExportQueries)
	sXLSQuery = GetQryString('E93',@sCompleteDCTableName,17,0,1,.T.,,,,@nExportQueries)
	?sXLSQuery
	IF nExportQueries=1 
		&sXLSQuery
	ENDIF
	
ENDPROC


*-------------------------------------------------------------------------------
FUNCTION CreatePlainTable

	PARAMETERS TPATH, TNAME
	* Crea la tabla TNAME en la carpeta TPATH según la estructura definida para ella en Tables.MASTER_PLAINS


	IF USED(TNAME)
		RETURN .T.
	ENDIF

	SELECT FIELD_NAME, FIELD_TYPE, FIELD_LEN, FIELD_DEC FROM MASTER_PLAINS WHERE TABLE_NAME == TNAME INTO ARRAY A_STRUCT

	IF _TALLY > 0
		WAIT WINDOW NOWAIT 'Creando tabla ' + TNAME
		_SqlString = 'CREATE TABLE [' + TPATH + TNAME + '] FREE (' 
		
		FOR iField=1 TO ALEN(A_STRUCT,1)
			_SqlString = _SqlString + ALLTRIM(A_STRUCT(iField,1)) + " " +  A_STRUCT(iField,2) + "(" + ;
						PADL(INT(A_STRUCT(iField,3)),3,' ')
			IF  A_STRUCT(iField,2)="N" THEN
				_SqlString = _SqlString + "," + PADL(INT(A_STRUCT(iField,4)),2," ") 
			ENDIF
			_SqlString = _SqlString + "), "
		NEXT iField
		_SqlString = SUBSTR(_SqlString,1,LEN(_SqlString)-2) + ")"

		&_sqlString
		
		SELECT MASTER_PLAINS
		USE
		SELECT &TNAME
		USE
		RETURN .t.
	ENDIF

	SELECT MASTER_PLAINS
	USE
	WAIT WINDOW NOWAIT 'No se ha definido estructura para ' + TNAME
	RETURN .t.
ENDFUNC


*-----------------------------------------------------------------------------------------
FUNCTION createPlainTableIndex(sTablePath AS String, sTargetTableName AS String)
	*Crea índices para la tabla sTargetTableName que se encuentra en la carpeta sTablePath, según se encuentren definidos en Tables.MASTER_INDEX
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	
	LOCAL sSqlCmd as String 
	
	LOCAL nOldSelected as Number 
	STORE SELECT() TO nOldSelected

	DIMENSION aIndexExpression(1)

	SELECT TAG_NAME, TAG_EXPR, EXTRAS FROM MASTER_INDEX	WHERE TABLE_NAME  = sTargetTableName INTO ARRAY aIndexExpression
	IF _TALLY = 0 THEN
		*Utiliza índices por default
		SELECT TAG_NAME, TAG_EXPR, EXTRAS FROM MASTER_INDEX	WHERE TABLE_NAME  = 'PLANO_EXX' INTO ARRAY aIndexExpression
	ENDIF
	
	IF _TALLY > 0
		WAIT WINDOW NOWAIT 'Creando índice para ' + sTargetTableName
		=SelectTable(ADDBS(sTablePath) + sTargetTableName,'',.T.)
		FOR I = 1 TO ALEN(aIndexExpression,1)
			sSqlCmd = 'INDEX ON ' + aIndexExpression(I,2) + ' TAG ' + aIndexExpression(I,1) + ' ' + aIndexExpression(I,3)
			TRY 
				&sSqlCmd
			CATCH TO oException
			ENDTRY 
		ENDFOR
	ENDIF

	USE IN (sTargetTableName)
	USE IN MASTER_INDEX
	
	SELECT (nOldSelected)
	RELEASE aIndexExpression
	
	RETURN .T.
ENDFUNC

*---------------------------------------------------------------------------------------------
FUNCTION initEventConditions(tableName as String, tablePath as String) as String 

*El Archivo Plano D, contiene los datos complementarios de todos los eventos de salud ;
Dependiendo del evento específico al que corresponda un registro del archivo, ;
los datos correspondientes se cargarán en una tabla PLANO_Exx con una estructura propia definida ;
en el Documento "Estructura de los Registros para Notificación de Datos complementarios". ;
Sin embargo cualquiera que sea el registro del archivo, cumple las siguientes reglas generales: ;
Todos los registros, cualquiera sea el evento, inician con los campos Semana, Año, ;
Código del prestador de servicios de salud, Código de prestador de servicios de salud - Sub índice, ;
Código Evento, Tipo Identificación y Número Identificación. De ahí en adelante, los campos son ;
diferentes dependiendo del evento al que pertenezca el registro.

*Dado que inicialmente, se cargan en la tabla PLANO_EVENTOS los campos comunes y el primer campo ;
no comun de todos los registros del archivo D, la subsiguinte carga de registros hacia una tabla ;
PLANO_Exx debe descartar la información de datos de exámenes de laboratorio del evento Exx ;
por cuanto esos registros deben cargarse en la tabla LABORATORIOS.

* Por tanto deben establecerse condiciones de filtrado que permitan no cargar en PLANO_Exx, ;
los registros correspondientes a exámenes de laboratorio. Esta condiciones dependerán del tipo de campo del ;
primer campo no común de la tabla PLANO_Exx.

	&&#DEFINE firstUncommonFieldOrdinalPos 8
	
	USE [&tablePath\&tableName] IN 0 AGAIN ALIAS (tableName) + "dummy"
	AFIELDS(targetFields,(tableName) + "dummy")
	targetFieldName=targetFields(firstUncommonFieldOrdinalPos ,1) 
	targetFieldType=targetFields(firstUncommonFieldOrdinalPos ,2) 
	targetFieldSize=targetFields(firstUncommonFieldOrdinalPos ,3) 
	LabIndicator = ALLTRIM(SUBSTR('LABORATORIOS',1,TargetFieldSize))

	DO CASE
		CASE targetFieldType='C' OR targetFieldType='V'
			excludeCondition='!(ALLTRIM(UPPER(' + targetFieldName + ')) $ "' + LabIndicator + '")' 
		CASE targetFieldType='D'	
			excludeCondition='!EMPTY(' + targetFieldName  + ')'
		CASE targetFieldType='N'	
			excludeCondition= targetFieldName  + '!=0'
	ENDCASE
	
	SELECT (tableName) + "dummy"
	USE

	RETURN excludeCondition
ENDFUNC


*-------------------------------------------------- 
FUNCTION addEvents
	PARAMETERS _Path, _PlainFileName

	_EventFileName = 'EVENTOS_' + RIGHT(_PlainFileName,2)

	checkAndSelect(_EventFileName)
	checkAndSelect(_RTA_CONSOLIDA+'\EXISTENTES')
	checkAndSelect(_PlainFileName)
ENDFUNC



*--------------------------------------------------------
FUNCTION checkAndSelect
	
	PARAMETERS sTableToCheck, lCheckFileExist AS Boolean
	
	IF PCOUNT()=2 THEN
		IF lCheckFileExist THEN
			IF !FILE(sTableToCheck + '.dbf')
				RETURN .F.
			ENDIF
		ENDIF
	ENDIF
	
	IF !USED(sTableToCheck)	
		SELECT 0
		USE (sTableToCheck)
	ELSE
		SELECT (sTableToCheck)
	ENDIF
	RETURN .T.
ENDFUNC

*--------------------------------------------------------
FUNCTION checkAndClose
PARAMETERS _eTable

IF USED(_eTable)	
	SELECT (_eTable)
	USE
ENDIF

RETURN .T.

*--------------------------------------------------------
PROCEDURE addNewTableToMasterPlains
	PARAMETERS FName, _Version

	CLOSE TABLES

	IF !('PLANO_' $ FName)
		WAIT WINDOW NOWAIT 'El archivo origen no es un PLANO...'
		RETURN
	ENDIF

	IF !FILE(FName+'.dbf')
		MESSAGEBOX('Archivo ' + FName + ' No encontrado')
		RETURN
	ENDIF

	USE MASTER_PLAINS EXCLUSIVE

	SELECT 0
	USE (FName) 
	_Alias = ALIAS()
		
	SELECT (_Alias)
	COPY STRUCTURE exte TO TMP_STRUCT

	DELETE FROM MASTER_PLAINS WHERE table_name = _Alias

	SELECT MASTER_PLAINS
	PACK

	APPEND FROM TMP_STRUCT
	REPLACE ALL table_name WITH _Alias, field_seq WITH RECNO()*10, table_ver WITH _Version;
		FOR EMPTY(table_name)

	CLOSE tables

ENDPROC


FUNCTION extractOrphans
	LPARAMETERS entityPlainFileName, eventsPlainFileName, orphansRSName, NonOrphansRSName, nSivigilaVersion, nEntityKind as Byte

	*Selecciona de entityPlainFileName los registros que no tienen coincidentes en eventsPlainFileName utilizando como "clave" de coincidencia;
	el conjunto de atributos {AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB}, si se trata de un paciente o ;
	{AÑO, SEMANA, COD_EVE, COD_MUN, NUM_CON, COD_PRE, COD_SUB}, si se trata de un Brote.;
	Un registro es huerfano en entityPlainFileName si no se encuentra un registro relacionado en eventsPlainFileName cuando la definicón del evento;
	obliga a que se tenga uno.
	*LOS VALORES RETORNADOS SON:	(0) NO HAY REGISTROS EN entityPlainFileName;
									(10) TODOS LOS REGISTROS EN entityPlainFileName SON HUERFANOS	;
									(1) TODOS  LOS REGISTROS EN entityPlainFileName SON NO HUERFANOS;
									(11) EN entityPlainFileName HAY REGISTROS DE LOS DOS TIPOS
	
	*orphansRSName: nombre del cursor que contendrá los registros huerfanos en entityPlainFileName. ;
	El nombre de este cursor será C_Orphans_ + ALLTRIM(SUBSTR(PlainFileName,7));

	*NonOrphansRSName: nombre de la tabla que contendrá los registros NO huerfanos en entityPlainFileName. ;
	El nombre de este cursor será T_NonOrphans_ + ALLTRIM(SUBSTR(PlainFileName,7));
	
	*nSivigilaVersion: una de las constantes _VERSION2008 o _VERSION2010. Se utiliza para determinar qué tabla de eventos debe tenerse en cuenta;
	para saber si un evento particular requiere o no de la existencia de datos complementarios
	
	*nEntityKind: Tipo de entidad para la cual se pretende determinar los registros huerfanos (1-->Paciente; 2-->Brote o evento colectivo) 
	
	
	_ReturnedValue = 0
		
	orphansRSName='C_Orphans_' + ALLTRIM(SUBSTR(entityPlainFileName,7))
	NonOrphansRSName='T_NonOrphans_' + ALLTRIM(SUBSTR(entityPlainFileName,7))
	IF nEntityKind = 1 THEN
		sKeyToDetectOrphans='AÑO + SEMANA + COD_EVE + TIP_IDE + NUM_IDE + COD_PRE + COD_SUB'
	ELSE
		sKeyToDetectOrphans='AÑO + SEMANA + COD_EVE + COD_MUN + NUM_CON + COD_PRE + COD_SUB'
	ENDIF

	*Determina, dependiendo de la versión del Sistema SIVIGILA y del tipo de entidad, qué consulta debe utilizarse para determinar ;
	los registros sin datos complementarios
	SET PROCEDURE TO SivigilaUtilities ADDITIVE
	IF nSivigilaVersion=_VERSION2008 THEN
		IF nEntityKind = 1 THEN
			sSetOrphansSQLClause=GetQuerySQLClause('PacientesHuerfanos_Ver2008.QPR')
		ELSE
			sSetOrphansSQLClause=GetQuerySQLClause('BrotesHuerfanos_Ver2008.QPR')
		ENDIF
	ELSE
		IF nEntityKind = 1 THEN
			IF !isField(entityPlainFileName, "INMEDIATA") THEN
				sSetOrphansSQLClause=GetQuerySQLClause('PacientesHuerfanos_Ver2010.QPR')
			ELSE
				sSetOrphansSQLClause=GetQuerySQLClause('PacientesHuerfanos_NotiInmediataVer2010.QPR')
			ENDIF
		ELSE
			IF !isField(entityPlainFileName, "INMEDIATA") THEN
				sSetOrphansSQLClause=GetQuerySQLClause('BrotesHuerfanos_Ver2010.QPR')
			ELSE
				sSetOrphansSQLClause=GetQuerySQLClause('BrotesHuerfanos_NotiInmediataVer2010.QPR')
			ENDIF
		ENDIF
	ENDIF
	RELEASE PROCEDURE SivigilaUtilities

	*Crea el cursor que contiene los registros huerfanos
	sSetOrphansSQLClause=sSetOrphansSQLClause + ' INTO CURSOR '  + orphansRSName + ' NOFILTER' 
	&sSetOrphansSQLClause

	IF _TALLY > 0
		_ReturnedValue = _ReturnedValue + 10
	ENDIF

	*Crea la tabla que contiene los registros NO huerfanos
	SqlClause = 'SELECT *, RECNO() AS nReg FROM ' + entityPlainFileName + ' WHERE ' + sKeyToDetectOrphans +;
				' NOT IN (SELECT ' + sKeyToDetectOrphans + ' FROM ' + orphansRSName  + ') INTO TABLE ' + NonOrphansRSName
	&SqlClause

	IF _TALLY > 0
		_ReturnedValue = _ReturnedValue + 1
	ENDIF
	
	SELECT (orphansRSName)
	RETURN _ReturnedValue 
ENDFUNC


FUNCTION getQueryFields

	PARAMETERS sTableShortName as String, nStartField as Byte, sPrefix as String, ;
				nMajor as Byte, nMinor as Byte, nBuild as byte
	
	* Retorna los campos del Query de exportación correspondiente a la tabla sTableShortName a partir;
	del campo nStartField, según la	especificación definida en Tables.ExportQueries. ;
	sTableShortName debe ser uno de los valores de Tables.ExportQueries.Fields.ShortName
	
	*Si sPrefix != '', la lista de campos se retorna cualificada con sPrefix.
	
	*Si se pasan los parámetros opcinales nMajor, nMinor y nBuild se tendrá en cuenta ;
	el identificador específico de versión dado por nMajor.nMinor.nBuild para seleccionar ;
	los campos del query

	sTableLongName=''
	IF PCOUNT()<=3 THEN
		nMajor=0
		nMinor=0
		nBuild=0
	ENDIF
	sSQLClause=GetQryString(sTableShortName,sTableLongName, nMajor , nMinor, nBuild)
	sFieldList = SUBSTR(sSQLClause,8,RAT('FROM',sSQLClause)-8)

	IF nStartField >1 THEN
		sFieldList = SUBSTR(sFieldList ,AT(',',sFieldList,nStartField-1)+1)
	ENDIF
	IF !EMPTY(sPrefix) THEN
		sFieldList = STRTRAN(sFieldList,'RecordSource',sPrefix,-1,-1,1)
	ENDIF

	RETURN sFieldList
ENDFUNC


PROCEDURE CreateExportationPlainTable

	*Crea una tabla de nombre PLANO con campos de tipo C(254) que se utiliza para almacenar los datos concatenados almacenados;
	en los campos de las tablas de datos complementarios, datos básicos, laboratorios y upgds, correspondientes a los eventos ;
	de salud manejados en el sistema SIVIGILA. Los campos de la tabla PLANO se nombran como PARTE1, ..., PARTE20.
	
	CREATE TABLE PLANO FREE (PARTE1 V(254), PARTE2 V(254), PARTE3 V(254),  PARTE4 V(254), ;
							PARTE5 V(254), PARTE6 V(254), PARTE7 V(254),  PARTE8 V(254), ;
							PARTE9 V(254), PARTE10 V(254), PARTE11 V(254), PARTE12 V(254), ;
							PARTE13 V(254), PARTE14 V(254), PARTE15 V(254), PARTE16 V(254), ;
							PARTE17 V(254), PARTE18 V(254), PARTE19 V(254), PARTE20 V(254))

ENDPROC


PROCEDURE activateNonActiveUPGDsWithNotification

	* Activa en forma automática UPGDs que tengan datos por notificar en alguna de las tablas de datos del Sistema.
	* Luego de su ejecución todas las tablas de datos del Sistema y las tablas o cursores intermedios utilizados ;
	en el procesamiento quedan cerrados


	* Establece el filtro que determina cuáles UPGDs se encuentra actualmente con datos pendientes por notificar
	sFilter = "NOTIFICA = 0"

	* Establece el conjunto de UPGDs que tiene datos pendientes por notificar (incluy el caso particular del evento Morbilidad por IRAg - 995;
	cuyos datos se almacenan de manera independiente en la tabla EVENTOS_43. 
	sSQLSelectCmd = "SELECT DISTINCT COD_PRE, COD_SUB FROM PACIENTE WHERE " + sFilter + " UNION " +;
					"SELECT DISTINCT COD_PRE, COD_SUB FROM BROTES WHERE " + sFilter + " UNION " +;
					"SELECT DISTINCT COD_PRE, COD_SUB FROM EVENTOS_43 WHERE " + sFilter
					
	DO EventosConTablaDatosIndependiente.QPR
	SELECT DISTINCT "EVENTOS_" + PADL(FORMULARIO,2,'00') AS NombreTablaEventoSeguimiento FROM rsEventosConTablaDatosIndependiente ;
		INTO CURSOR rsOddEvents
	SELECT rsOddEvents
	SCAN
		sSQLSelectCmd = sSQLSelectCmd + " UNION SELECT DISTINCT COD_PRE,COD_SUB FROM " + ;
						rsOddEvents.NombreTablaEventoSeguimiento +;
						" WHERE  " + sFilter 
	ENDSCAN
	sSQLSelectCmd = sSQLSelectCmd + " INTO CURSOR rsCandidatesUPGDs_01"
	&sSQLSelectCmd
	
	SELECT DISTINCT COD_PRE, COD_SUB FROM rsCandidatesUPGDs_01 INTO CURSOR rsCandidatesUPGDs
	IF _TALLY>0 THEN
		* Activa automáticamente las UPGDs que no lo están y tienen datos por notificar
		glDisableRules=.T.
		UPDATE UPGD SET act_siv = 1 FROM rsCandidatesUPGDs WHERE UPGD.COD_PRE=rsCandidatesUPGDs.COD_PRE AND ;
			UPGD.COD_SUB=rsCandidatesUPGDs.COD_SUB AND UPGD.Act_Siv=2
		glDisableRules=.F.
	ENDIF
	
	SELECT rsOddEvents
	SCAN
		DO CloseTables WITH rsOddEvents.NombreTablaEventoSeguimiento IN SivigilaUtilities
	ENDSCAN
	DO CloseTables WITH 'rsEventosConTablaDatosIndependiente, rsCandidatesUPGDs_01, rsCandidatesUPGDs, rsOddEvents, UPGD, EVENTOS, PACIENTE, BROTES' IN SivigilaUtilities
	
ENDPROC


PROCEDURE loadPlainFile

	LPARAMETERS sSourceFileNameAndPath as String, sTargetTableNameAndPath as String, sFilterCondition as String, ;
				sBackupFileNameAndPath as String, sMsgFileKind as String

	*Carga los datos contenidos en el archivo plano sSourceFileNameAndPath, en donde se supone que los campos están delimitados ;
	por comas, en la tabla sTargetTableNameAndPath. Si se pasa el parámetro opcional sFilterCondition, solamente se cargan ;
	los datos que cumplan esa condición.
	
	*Si se pasa el parámetro opcional sBackupFileNameAndPath, produce una copia en excel de los datos cargados con una fila ;
	de encabezado igual a los nombres de campos de la tabla sTargetTableNameAndPath. 
	
	*sMsgFileKind es un sufijo que se adiciona al mensaje de procesamiento "Por favor espere... Cargando el archivo texto de "
	
	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
	=showProgressMessage('Por favor espere...' + CHR(13) + 'Cargando el archivo texto de ' + sMsgFileKind)

   	SET EXCLUSIVE ON
   	USE (sTargetTableNameAndPath) EXCLUSIVE IN 0
   	SELECT JUSTSTEM(sTargetTableNameAndPath)
   	ZAP
   	SET EXCLUSIVE OFF
   	sAppendCmd = "APPEND FROM '" + sSourceFileNameAndPath + "'"
   	IF VARTYPE(sFilterCondition)='C' THEN
	   	sAppendCmd = sAppendCmd + " FOR " + sFilterCondition
	ENDIF
	sAppendCmd = sAppendCmd + " DELIMITED"
	&sAppendCmd
	
	IF VARTYPE(sBackupFileNameAndPath)='C' THEN
		oDataExporter=NEWOBJECT("DataExporter","DataExporter.Prg",.null.)
		oDataExporter.sExportationPath = JUSTPATH(sBackupFileNameAndPath) 
		oDataExporter.sSourceTableName = JUSTSTEM(sTargetTableNameAndPath)
		oDataExporter.sExportedFileName = JUSTSTEM(sBackupFileNameAndPath)
		oDataExporter.exportToXLS()		
		RELEASE oDataExporter
	ENDIF
	
ENDPROC


PROCEDURE loadDataTable

	LPARAMETERS sSuffixUserMessage as string, sTargetTableName as String, sIdFieldName as String, sNameFieldName as String
	
	*Carga en sTargetTableName los registros contenidos en un archivo plano seleccionado en forma interactiva por el usuario. ;
	Se supone que en el archivo plano, los campos están separados por tabuladores
	
	*sIdFieldName es el nombre del campo identificador de registros en sTargetTableName y sNameFieldName es el nombre del campo ;
	que contiene los nombres identificados por el identificador; por ejemplo, el campo identificador podría llamarse CODIGO en sTargetTableName ;
	y el campo de nombres podría llamarse NOMBRE_SECTOR. 
	
	*sSuffixUserMessage es un sufijo que agrega al cuadro de dialogo que permite seleccionar el archivo plano; de este modo el ;
	mensaje para el usuario será de la forma "Seleccione el archivo texto delimitado por TABs de " + sSuffixUserMessage

	sSourceFileName=GETFILE("Texto:Txt","","Seleccionar",0,"Seleccione el archivo texto delimitado por TABs de " + sSuffixUserMessage)

	nOldSelected=SELECT()

	IF !EMPTY(sSourceFileName) THEN
		TRY
			SELECT * FROM (sTargetTableName) WHERE .F. INTO CURSOR rsSectorsToLoad READWRITE
			APPEND FROM [&sSourceFileName] DELIMITED WITH TAB
			REPLACE rsSectorsToLoad.&sNameFieldName WITH STRTRAN(rsSectorsToLoad.&sNameFieldName,",","_") ALL
			sSQLCmd = "INSERT INTO " + sTargetTableName + " SELECT * FROM rsSectorsToLoad WHERE " + ;
				"!EMPTY(rsSectorsToLoad." + sIdFieldName + ") AND !EMPTY(rsSectorsToLoad." + sNameFieldName + ") AND !EMPTY(rsSectorsToLoad.cod_dep) AND " + ;
				"!EMPTY(rsSectorsToLoad.cod_mun) AND  " + ; 
				"(rsSectorsToLoad.cod_dep + rsSectorsToLoad.cod_mun IN (SELECT MUNICIPIOS.COD_MUN FROM MUNICIPIOS)) AND " + ;
				"(rsSectorsToLoad.cod_dep IN (SELECT DEPTOS.COD_DEP FROM DEPTOS)) AND " + ;
				"(rsSectorsToLoad." + sIdFieldName + " + rsSectorsToLoad.cod_dep + rsSectorsToLoad.cod_mun NOT IN " + ;
				"(SELECT " + sTargetTableName + "." + sIdFieldName + "+" + sTargetTableName + "." + "cod_dep + " + sTargetTableName + ".cod_mun as PK FROM " + sTargetTableName + "))"
			&sSQLCmd 
			IF _TALLY>0 THEN
				MESSAGEBOX("Se agregaron " + ALLTRIM(STR(_TALLY)) + " registros", 0 + 48 + 256, 'Aviso de importación SIVIGILA')
			ELSE
				MESSAGEBOX("No se encontró ningún registro nuevo para agregar", 0 + 48 + 256, 'Aviso de importación SIVIGILA')
			ENDIF
			
		CATCH TO oException
			MESSAGEBOX(oException.Message, 0 + 48 + 256, 'Error de importación SIVIGILA')
		FINALLY
			IF USED("rsSectorsToLoad") THEN
				SELECT rsSectorsToLoad
				USE
			ENDIF
			SELECT (nOldSelected)
		ENDTRY
	ENDIF

ENDPROC


FUNCTION ExportFromQryToPlainFile(TableShortName, DynamicConditions, duplicatesFilePath, ;
								checkEnableUPGD as Boolean, sDelimiterChar as String, bRemoveTrailingSpaces AS Boolean, ;
								bDoNotCheckDuplicates as Boolean, sUserDefinedQuery as String, ;
								bNotOrdinaryNotification as Boolean, nFileHandler as Number, nExportedLines as Number)

	* Guarda en un archivo plano apuntado por nFileHandler, la totalidad de los registros de la tabla TableShortName que satisfagan ;
	las condiciones	DynamicConditions. Cada registro corresponde a la concatenación de los campos definidos en el query de ;
	exportación para la tabla TableShortName según se encuentre en Tables.ExportQueries o en sUserDefinedQuery, si es que se pasa ;
	este parámetro opcional. Como efecto colateral, retorna en nExportedLines el número de registros exportados hacia el plano.
	
	* checkEnableUPGD: Si es igual a .T., se exportan solamente los registros de TableShortName que pertenezcan;
	a UPGDs activas en el sistema SIVIGILA
	
	* sDelimiterChar: (opcional) caracter utilizado para delimitar los campos de los registros exportados de ;
	TableShortName. Por default = ","
	
	* bRemoveTrailingSpaces: (opcional) Si es .T., se eliminan espacios sobrantes de cada campo de cada registro exportado

	* bDoNotCheckDuplicates: (opcional) Si es .T., se detecta en el conjunto de registros a exportar, cuales son duplicados según ;
	la clave ...
	
	* bNotOrdinaryNotification: (opcional) Si es .F., cada registro exportado es marcado como correspondiente a una notificación ;
	rutinaria; en este caso, el query de exportación utilizado debe tener un campo llamado INMEDIATA de tipo C(2) que asumirá ;
	el valor 'NO'

	LOCAL bSuccess AS Boolean 
	bSuccess = .T.

	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE

	StringForInsert = ''

	_Cursor = 'C_' + TableShortName

	*Garantiza que la exportación siempre se lleve a cabo de acuerdo con la última versión del sistema definida;
	por sus identificadores nMajor.nMinor.nBuild según se encuentran definidos en main
	DO resetSivigilaVersion	IN main
	
	* Recupera el query que se va a utilizar para exportar los datos
	TableLongName=''
	IF VARTYPE(sUserDefinedQuery)= 'C' AND !EMPTY(sUserDefinedQuery ) then
		_Xecute = sUserDefinedQuery 
	ELSE
		_Xecute = GetQryString(TableShortName, @TableLongName, nMajor, nMinor, nBuild)
	ENDIF
	
	IF _Xecute == 'NIL'
		RETURN .F.
	ENDIF


	*Construye un cursor con los datos que se deben exportar teniendo en cuenta que pertenezcan;
	a UPGDs activas en el sistema SIVIGILA
	IF !EMPTY(DynamicConditions)
		_Xecute = _Xecute + ' WHERE ' + DynamicConditions 
	ELSE
		_Xecute = _Xecute + ' WHERE .T.' 
	ENDIF
	IF checkEnableUPGD THEN
		_Xecute = _Xecute + ' AND (recordSource.cod_pre + ' + 'recordSource.cod_sub' + ;
				 ' IN (SELECT DISTINCT cod_pre+cod_sub AS CodUPGD FROM UPGD WHERE ACT_SIV = 1))' 
	ENDIF
	_Xecute = _Xecute + ' INTO CURSOR ' + _Cursor + ' READWRITE'

	&_Xecute
	IF _TALLY > 0
						
		IF !bNotOrdinaryNotification THEN
			SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
			IF isField(_Cursor,"INMEDIATA") THEN
				REPLACE ALL INMEDIATA WITH 'NO'
			ENDIF
		ENDIF
		
		* Determina si los registros que se van a exportar contienen duplicados
		IF !bDoNotCheckDuplicates THEN
			SELECT  IMPTABLENA FROM ExportQueries WHERE Activo AND ShortName=(TableShortName) INTO ARRAY importTableName
			SELECT  TABLENAME FROM ExportQueries WHERE Activo AND ShortName=(TableShortName) INTO ARRAY tableName
			duplicatesRecords=''

			SET PROCEDURE TO IMPORTAR_CLICK ADDITIVE
			nSourceWithDuplicates=extractDupsAndNotDups(_Cursor,@duplicatesRecords, ,importTableName(1,1))
			IF nSourceWithDuplicates>9 THEN
				* Existen registros duplicados en la fuente de exportación
				xlsFileName=duplicatesFilePath + ALLTRIM(tableName(1,1)) + '_DUPLICADOS'
				COPY TO (xlsFileName) XL5
				bSuccess = .F.
			ENDIF
			RELEASE PROCEDURE IMPORTAR_CLICK 
		ENDIF
		
		SELECT &_Cursor
		CursorFieldsQRows =  AFIELDS(A_CursorFields, _Cursor)
*SET STEP ON 
		SCAN 
			FOR I = 1 TO CursorFieldsQRows
				CursorField = exprToTransformToStr (A_CursorFields(I,1), A_CursorFields(I,2), bRemoveTrailingSpaces, ;
													A_CursorFields(I,3), A_CursorFields(I,4))
				IF VARTYPE(sDelimiterChar)!='C' THEN
					StringForInsert = StringForInsert + NVL(&CursorField,'') + ","
				ELSE
					StringForInsert = StringForInsert + NVL(&CursorField,'') + sDelimiterChar 
				ENDIF
			ENDFOR
			StringForInsert  = SUBSTR(StringForInsert, 1, LEN(StringForInsert)-1) 
			
			*Inserta la línea StringForInsert en el archivo plano
			=FPUTS(nFileHandler, StringForInsert)
			nExportedLines = nExportedLines + 1
			
			StringForInsert = ''
		ENDSCAN

		IF !bDoNotCheckDuplicates THEN
			*Cierra/Elimina las tablas temporales generadas en la detección de duplicados
			IF nSourceWithDuplicates!=0 THEN
				SELECT ALLTRIM('T_NONDUPS_' + TableShortName)
				USE
				DELETE FILE ALLTRIM('T_NONDUPS_' + TableShortName)) + '.DBF'
				SELECT (duplicatesRecords)
				USE
				DELETE FILE &duplicatesRecords..DBF
				SELECT C_Count_Dups				
				USE
			ENDIF
		ENDIF
	ENDIF

	RETURN bSuccess

ENDFUNC


PROCEDURE territorialUnitFeedback_V02(sExportationPath as String, sEntityKind as Byte , sEntityCode as string, sFilter as String, ;
								sNotifierKind as String,  sNotifierId as String, sNotificationWeek as String, ;
								bDoNotShowTerminationMsg as Boolean, bBeSilent as Boolean, ;
								bDoNotUseOriginMunicipality as Boolean, bIncludeOnlyForeignUPGDs as Boolean, sPwd AS String )
	
	*Crea archivos planos de retroalimentación para una entidad territorial identificada por el par (sEntityKind, sEntityCode) en donde;
	sEntityKind es el identificador del tipo de entidad (2=Depto 3=Municipio) y sEntityCode es el valor o código que identifica a;
	la entidad. Los archivos planos que se crean corresponderán a la vigencia sTargetYear y los registros que;
	se incluyan en ellos obedecerán la configuración que se encuentra en Tables.tbConfiguracionRetroalimentacion
	
	*sExportationPath: ruta en donde quedarán almacenados ls archivos planos de retroalimentación
	
	*sNotifierKind, sNotifierId, sNotificationWeek: utilizados para establecer los nombres de los archivos ;
	planos a crear así: ;
		sNotifierKind, sNotifierId: identificadores del tipo de unidad que notifica y código de la unidad ;
		sNotificationWeek: identificador de la semana epidemiológica, normalmente será 'RA' ;

		 
	*bDoNotShowTerminationMsg: indica si debe mostrarse o no el mensaje que indica que la ejecución de este PROCEDURE ha finalizado
	
	*bBeSilent: si es .T. redirecciona todos los mensajes que se produzcan durante el procesamiento hacia un archivo texto que;
	se almacenará en sExportationPath; en caso contrario, los mensajes se muestran en la salida estándar
	
	*bDoNotUseOriginMunicipality: indica si en la extracción de datos debe usarse el municipio de procedencia o de residencia;
	el valor por defecto es .F., es decir, se usa el municipio de procedencia

	*bIncludeOnlyForeignUPGDs: indica si en la extracción de datos deben incluirse solamente UPGDs que NO pertenezcan al departamento;
	el valor por defecto es .F., es decir, se incluyen tanto UPGDs que pertenecen al departamento como UPGDs que no pertenecen a él

	*sPwd: si se pasa este parámetro opcional, el paquete zip de archivos planos resultante del procesamiento quedará con;
	este password asignado
	 
	LOCAL sTerritorialUnitPlainsFilePath  AS String
	LOCAL nExportedRecs as string
	

	DIMENSION aPlainTableKeys(1)
	* Contiene los campos que conforman las claves de las tablas suceptibles de ser notificadas por;
	el sistema.
	*Estas claves son idénticas tanto para las tablas del sistema como para las tablas PLANOS_... que;
	son utilizadas en los procesos de notificación/consolidación del Sistema ;

	sInitTime=TIME()

	nBaseLineRecs = 0
		
	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
	
	IF !(VARTYPE(sPwd)='C' AND !EMPTY(sPwd)) THEN 
		sPwd = ''
	ENDIF 
	
	DO cleanTheHouse IN IMPORTAR_CLICK

	DO initPlainTableKeysArray WITH aPlainTableKeys IN importar_click

*SET STEP ON 
	*Establece las condiciones de filtrado que deben cumplirse para exportar la información y la tabla de referencia;
	en donde se encuentra la información de la entidad territorial

	IF sEntityKind = 2 THEN
		sEntityLevel = "DEPARTAMENTO"
		IF bUseResidenceMunicipality = 0 THEN
			*Usa el departamento de procedencia como criterio para extraer los datos
			sConcept = "Procedencia"
		ELSE
			*Usa el departamento de residencia como criterio para extraer los datos
			sConcept = "Residencia"
		ENDIF
	ELSE
		sEntityLevel = "MUNICIPIO"
		IF bUseResidenceMunicipality = 0 THEN
			*Usa el municipio de procedencia como criterio para extraer los datos
			sConcept = "Procedencia"
		ELSE
			*Usa el municipio de residencia como criterio para extraer los datos
			sConcept = "Residencia"
		ENDIF
	ENDIF
	sIndividualFilter = fieldValue('EXPR_CAMPO', 'tbConfiguracionRetroalimentacion', 'NOM_TABLA="PACIENTE"  AND CONCEPTO="' + sConcept + ;
						'" AND NIVEL="' + sEntityLevel  + '"')
	sCollectiveFilter = fieldValue('EXPR_CAMPO', 'tbConfiguracionRetroalimentacion', 'NOM_TABLA="BROTES"  AND CONCEPTO="' + sConcept + ;
						'" AND NIVEL="' + sEntityLevel  + '"')

	IF sEntityKind = 2 THEN
		sIndividualLinkCondition = sIndividualFilter + ' = DEPTOS.cod_dep '
		sCollectiveLinkCondition = sCollectiveFilter + ' = DEPTOS.cod_dep '
		
		sTerritorialUnitLookupName='DEPTOS'
		sTerritorialUnitFieldName='DEPTOS.NOM_DEP AS NombreEntidad'
	ELSE
		sIndividualLinkCondition = sIndividualFilter + ' = MUNICIPIOS.cod_mun '
		sCollectiveLinkCondition = sCollectiveFilter + ' = MUNICIPIOS.cod_mun '
		
		sTerritorialUnitLookupName='MUNICIPIOS'
		sTerritorialUnitFieldName='MUNICIPIOS.NOM_MUN AS NombreEntidad'
	ENDIF
	sIndividualTerritorialUnitFieldCode = sIndividualFilter + " AS CodEntidad"
	sCollectiveTerritorialUnitFieldCode = sCollectiveFilter + " AS CodEntidad"
*SET STEP ON 
	sIndividualFilter = sIndividualFilter + "='" + sEntityCode + "' AND " + sFilter
	sCollectiveFilter = sCollectiveFilter + "='" + sEntityCode + "' AND " + sFilter

	IF (sEntityKind = 2) AND bIncludeOnlyForeignUPGDs THEN
		sIndividualFilter = sIndividualFilter + " AND LEFT(RecordSource.COD_PRE,2)!='" + sEntityCode + "'"
		sCollectiveFilter = sCollectiveFilter + " AND LEFT(RecordSource.COD_PRE,2)!='" + sEntityCode + "'"
	ENDIF

	sIndividualFilter = sIndividualFilter + " AND " + fieldValue('FILTRO', 'tbConfiguracionRetroalimentacion', 'NOM_TABLA="PACIENTE"  AND CONCEPTO="' + sConcept + ;
						'" AND NIVEL="' + sEntityLevel  + '"')
	sCollectiveFilter = sCollectiveFilter + " AND " + fieldValue('FILTRO', 'tbConfiguracionRetroalimentacion', 'NOM_TABLA="BROTES"  AND CONCEPTO="' + sConcept + ;
						'" AND NIVEL="' + sEntityLevel  + '"')


	*Establece una línea base de pacientes para los cuales se exportarán los registros de datos complementarios, laboratorios, UPGDs;
	y talento humano teniendo en cuenta a qué entidades territoriales debe exportárseles información y excluyendo las notificaciones;
	negativas y la información correspondiente a UPGDs inactivas en el Sistema
	sSQLClause= "SELECT DISTINCT RecordSource.AÑO, RecordSource.SEMANA, RecordSource.COD_EVE, RecordSource.TIP_IDE," +;
				" RecordSource.NUM_IDE, RecordSource.COD_PRE, RecordSource.COD_SUB," +;
				sIndividualTerritorialUnitFieldCode + "," + sTerritorialUnitFieldName +;
				" FROM PACIENTE AS RecordSource LEFT OUTER JOIN " + sTerritorialUnitLookupName +;
				" ON " + sIndividualLinkCondition + " WHERE RecordSource.COD_EVE " +;
				" NOT IN (SELECT TOP 1 '000' AS CodNotificacionNegativa FROM EVENTOS ORDER BY COD_EVE) " +;
				" AND " + sIndividualFilter +	" INTO CURSOR cPatientsBaseLine"
				
	*La siguiente condición que obligaba a que se exportase información correspondiente;
	a UPGDs inactivas en el Sistema, fue desactivada por solicitud de Marcela Benavides del INS;
	el 15/07/2010	
	&&" AND (recordSource.cod_pre + " + "recordSource.cod_sub" + ;
	&&" IN (SELECT DISTINCT cod_pre+cod_sub AS CodUPGD FROM UPGD WHERE ACT_SIV = 1))" 
	&sSQLClause
	nBaseLineRecs = nBaseLineRecs + _TALLY
	
	*Establece una línea base de brotes para los cuales se exportarán los registros de datos complementarios teniendo en ;
	cuenta a qué entidades territoriales debe exportárseles información y excluyendo la información correspondiente a ;
	UPGDs inactivas en el Sistema
	sSQLClause = "SELECT DISTINCT RecordSource.AÑO, RecordSource.SEMANA, RecordSource.COD_EVE, RecordSource.COD_MUN," +;
				" RecordSource.NUM_CON, RecordSource.COD_PRE, RecordSource.COD_SUB," +;
				sCollectiveTerritorialUnitFieldCode + "," + sTerritorialUnitFieldName +;
				" FROM BROTES AS RecordSource LEFT OUTER JOIN " + sTerritorialUnitLookupName +;
				" ON " + sCollectiveLinkCondition + " WHERE " + sCollectiveFilter +	" INTO CURSOR cOutbreaksBaseLine"
	&sSQLClause
	nBaseLineRecs = nBaseLineRecs + _TALLY
	
	IF nBaseLineRecs = 0 THEN
		sNotificatioMsg='ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
						'No se encontraron registros que satisfagan los criterios suministrados' + CHR(13) + ;
						'para la retroalimentación.'
		IF !bDoNotShowTerminationMsg THEN
			=showErrorMessage(sNotificatioMsg, 0)
		ENDIF
	ELSE
	
		SELECT cPatientsBaseLine

		*Crea la carpeta en donde se almacenarán los archivos resultado de la exportación
		IF ISNULL(cPatientsBaseLine.NombreEntidad) THEN
			sTerritorialUnitName = 'DESCONOCIDA'
		ELSE
			sTerritorialUnitName = ALLTRIM(cPatientsBaseLine.NombreEntidad)
		ENDIF
		sTerritorialUnitName = ALLTRIM(CHRTRAN(ALLTRIM(sTerritorialUnitName), GARBAGE_CHARS, ''))
		sTerritorialUnitPlainsFilePath = sExportationPath + '\' + sTerritorialUnitName + "_" + ;
										 ALLTRIM(cPatientsBaseLine.CodEntidad)
		IF !DIRECTORY(sTerritorialUnitPlainsFilePath)
			MKDIR (sTerritorialUnitPlainsFilePath)
		ENDIF

		IF bBeSilent THEN
			sResultsFileName = "ResultadosRetroalimentacion_" + ALLTRIM(CHRTRAN(DTOC(DATE()),"/","")) + ".csv"
			nResultsFileHandler = FCREATE(ADDBS(sTerritorialUnitPlainsFilePath) + sResultsFileName)
		ENDIF

		*Fija los nombres de los archivos planos que almacenarán la información exportada desde las tablas del Sistema
		sUPGDsFileNameAndPath = ''
		sUCIsFileNameAndPath = ''
		sHumanResourcesFileNameAndPath = ''
		sPatientsFileNameAndPath = ''
		sEventsDataFileNameAndPath = ''
		sCollectiveEventsFileNameAndPath = ''
		sTotalsFileNameAndPath = ''
		sControlFileNameAndPath = ''
		sSIVIGILAFileNameAndPath = ''
		*A partir de la versión 2012 ya no se genera el archivo W		
		sWEBPatientsFileNameAndPath=''
		sIRAFileNameAndPath = ''
		sNotifierFileNamePrefix=''
		DO setOutputTxtFileNames WITH sNotifierKind, sNotifierId, sNotificationWeek, sTerritorialUnitPlainsFilePath, sNotifierFileNamePrefix,;
					sUPGDsFileNameAndPath, sHumanResourcesFileNameAndPath, sPatientsFileNameAndPath, ;
					sEventsDataFileNameAndPath, sCollectiveEventsFileNameAndPath, sTotalsFileNameAndPath ,;
					sControlFileNameAndPath, sSIVIGILAFileNameAndPath, sWEBPatientsFileNameAndPath ,;
					sIRAFileNameAndPath, sUCIsFileNameAndPath 

		* Genera el archivo N de datos básicos de notificación individual periódica semanal
		TIT('Generado Archivo Plano de Notificación Individual para ' + sTerritorialUnitName)
		nOutFileHandler = FCREATE(sPatientsFileNameAndPath + '.txt')
		nNExportedRegs = 0
		exportationIsOK = ExportFromQRYToPlainFile('PTE', sIndividualFilter + " AND RecordSource.COD_EVE " +;
													" NOT IN (SELECT TOP 1 '000' AS CodNotificacionNegativa FROM EVENTOS ORDER BY COD_EVE) ", ;
													 sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nNExportedRegs) 
		=FCLOSE(nOutFileHandler)
				
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13)  + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de notificación individual.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de notificación individual.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		
		* Genera el archivo D de datos complementarios de notificación periódica semanal obligatoria;
		incluyendo la información de resultados de laboratorio. Se excluyen eventos que no obedecen el patrón DB + DC inclusive;
		si han sido borrados del sistema
		sOldDeleted = SET("Deleted")
		SET DELETED OFF
		DO EventosConTablaDatosIndependiente.qpr
		SET DELETED &sOldDeleted 
		
		nOutFileHandler = FCREATE(sEventsDataFileNameAndPath + '.txt')
		nDExportedRegs = 0
		USE BDSivigila!vTablasDeDC IN 0
		SELECT vTablasDeDC
		SCAN
			eventToExport = vTablasDeDC.ID_DC_TABLA
			SELECT rsEventosConTablaDatosIndependiente
			IF eventToExport != 23 AND EMPTY(LOOKUP(FORMULARIO,ALLTRIM(STR(eventToExport)),FORMULARIO)) THEN
				*La tabla de datos complementarios no corresponde a una de un evento con tabla de datos independiente ni a la tabla;
				EVENTOS_23, que contiene datos complementarios de un brote

				sEventKeyExpression = ASCAN(aPlainTableKeys, 'PLANO_EXX', -1, -1 ,-1, 15)
				sEventKeyWithCommas = ALLTRIM(aPlainTableKeys(sEventKeyExpression ,2))
				sEventKeyWithPlus = 'RecordSource.' + STRTRAN(sEventKeyWithCommas, ', ' , '+RecordSource.')
				sPatientsBaseLineKeyWithPlus = 'cPatientsBaseLine.' + STRTRAN(sEventKeyWithCommas, ', ' , '+cPatientsBaseLine.')
				
				TIT('Generando Archivo Plano del Grupo de Eventos ' + PADL(eventToExport ,2,'0') + ' para ' + sTerritorialUnitName)
				exportationIsOK = ExportFromQRYToPlainFile('E'+PADL(eventToExport ,2,'0'), sEventKeyWithPlus + ;
												' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', ;
												 sTerritorialUnitPlainsFilePath, .F., ,.T. , .T., , , nOutFileHandler, @nDExportedRegs )
												 
				IF !exportationIsOK THEN
					IF !bBeSilent THEN
						MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
									'Existen registros repetidos o duplicados en la fuente de datos complementarios.' + CHR(13) + ;
									'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
									'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
					ELSE
						sResultMsg=	'Existen registros repetidos o duplicados en la fuente de datos complementarios.' + CHR(13) + ;
									'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
									'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
									sTerritorialUnitPlainsFilePath
						=FPUTS(nResultsFileHandler,sResultMsg)
					ENDIF
				ENDIF
			ELSE
				IF eventToExport != 43 THEN
					* Los datos de la tabla eventos 43 (Morbilidad por IRA) se generan en un archivo independiente					
					DO CASE
						CASE  eventToExport = 23
							*Se trata de la tabla de datos complementarios del evento Enfermedades transmitidas por alimentos  ETAs (Colectiva),;
							 Hepatitis A, Fiebre Tifoide (Cód. 350, 330, 320)
							
							sFormerEventKeyWithPlus = getKeyFields('EVENTOS_'+PADL(eventToExport ,2,'0'))
							*sEventKeyWithCommas no corresponde estrictamente a la clave de EVENTOS_23 o EVENTOS_31 si no que contiene;
							el mayor número	de atributos de la clave que están también presentes en la línea base de pacientes
							
							sEventKeyWithPlus = 'RecordSource.' + STRTRAN(sFormerEventKeyWithPlus, '+' , '+RecordSource.')
							sOutbreaksBaseLineKeyWithPlus = 'cOutbreaksBaseLine.' + STRTRAN(sFormerEventKeyWithPlus, '+' , '+cOutbreaksBaseLine.')
							TIT('Generando Archivo Plano del Grupo de Eventos ' + PADL(eventToExport ,2,'0') + ' para ' + sTerritorialUnitName)
							TRY
								exportationIsOK = ExportFromQRYToPlainFile('E'+PADL(eventToExport ,2,'0'), sIndividualFilter + " AND " + sEventKeyWithPlus + ;
																' IN (SELECT &sOutbreaksBaseLineKeyWithPlus FROM cOutbreaksBaseLine)',  ;
																 sTerritorialUnitPlainsFilePath, .F., ,.T. , .T., , , nOutFileHandler, @nDExportedRegs)

							CATCH TO oException
							ENDTRY
						OTHERWISE
							*Se trata de un evento con tabla de datos independiente
							
							sCompDataTableName = "EVENTOS_" + PADL(eventToExport ,2,'0')
							sCompDataFilter = fieldValue('EXPR_CAMPO', 'tbConfiguracionRetroalimentacion', 'NOM_TABLA="' + sCompDataTableName + ;
														'"  AND CONCEPTO="' + sConcept + '" AND NIVEL="' + sEntityLevel  + '"')

							IF ISNULL(sCompDataFilter) THEN
								sCompDataFilter = sIndividualFilter
							ELSE
								sCompDataFilter = sCompDataFilter + "='" + sEntityCode + "' AND " + sFilter
							ENDIF

							IF (sEntityKind = 2) AND bIncludeOnlyForeignUPGDs THEN
								sCompDataFilter = sCompDataFilter + " AND LEFT(RecordSource.COD_PRE,2)!='" + sEntityCode + "'"
							ENDIF
							
							TIT('Generando Archivo Plano del Grupo de Eventos ' + PADL(eventToExport ,2,'0') + ' para ' + sTerritorialUnitName)
							TRY
								exportationIsOK = ExportFromQRYToPlainFile('E'+PADL(eventToExport ,2,'0'), sCompDataFilter, ;
																sTerritorialUnitPlainsFilePath, .F., ,.T. , .T., , , nOutFileHandler, @nDExportedRegs)
							CATCH TO oException
							ENDTRY
					ENDCASE
				ENDIF
			ENDIF
		ENDSCAN 

		* Agrega al archivo D la información de resultados de laboratorio
		sEventKeyExpression = ASCAN(aPlainTableKeys, 'PLANO_EXX', -1, -1 ,-1, 15)
		sEventKeyWithCommas = ALLTRIM(aPlainTableKeys(sEventKeyExpression ,2))
		sEventKeyWithPlus = 'RecordSource.' + STRTRAN(sEventKeyWithCommas, ', ' , '+RecordSource.')
		sPatientsBaseLineKeyWithPlus = 'cPatientsBaseLine.' + STRTRAN(sEventKeyWithCommas, ', ' , '+cPatientsBaseLine.')

		TIT('Generando Archivo Plano de Laboratorios para ' + sTerritorialUnitName)
		exportationIsOK = ExportFromQRYToPlainFile('LAB', sEventKeyWithPlus + ' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)',  ;
										 sTerritorialUnitPlainsFilePath, .F., ,.T. , .T., , , nOutFileHandler, @nDExportedRegs)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de notificación laboratorios.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de notificación laboratorios.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
			
		ENDIF
		=FCLOSE(nOutFileHandler)


		* Genera el archivo B de datos de notificación colectiva periódica semanal obligatoria
		nOutFileHandler = FCREATE(sCollectiveEventsFileNameAndPath + '.txt')
		nBExportedRegs = 0

		TIT('Generando Archivo Plano de Notificación Colectiva para ' + sTerritorialUnitName)
		exportationIsOK = ExportFromQRYToPlainFile('BRO', sCollectiveFilter, sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nBExportedRegs)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de notificación colectiva.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de notificación colectiva.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		=FCLOSE(nOutFileHandler)


		* Genera el archivo U contenedor de datos de caracterización de UPGD

		sUPGDKeyExpression = ASCAN(aPlainTableKeys, 'PLANO_UPGD', -1, -1 ,-1, 15)
		sUPGDKeyWithCommas = aPlainTableKeys(sUPGDKeyExpression,2)
		sUPGDKeyWithPlus = 'RecordSource.' + STRTRAN(sUPGDKeyWithCommas, ', ' , '+RecordSource.')
		sPatientsBaseLineKeyWithPlus = 'cPatientsBaseLine.' + STRTRAN(sUPGDKeyWithCommas, ', ' , '+cPatientsBaseLine.')

		TIT('Generando Archivo Plano de las UPGD para ' + sTerritorialUnitName)
		nOutFileHandler = FCREATE(sUPGDsFileNameAndPath + '.txt')
		nUExportedRegs = 0

		exportationIsOK = ExportFromQRYToPlainFile('UPGD' , sUPGDKeyWithPlus + ' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', ;
										 sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nUExportedRegs )
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de UPGDs.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de UPGDs.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		=FCLOSE(nOutFileHandler)


		* Genera el archivo V contenedor de datos de caracterización de UCI en las UPGD
		TIT('Generando Archivo Plano de caracterización de UCI para ' + sTerritorialUnitName)
		nOutFileHandler = FCREATE(sUCIsFileNameAndPath + '.txt')
		nVExportedRegs = 0

		exportationIsOK = ExportFromQRYToPlainFile('UCI' , sUPGDKeyWithPlus + ;
										' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', ;
										 sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nVExportedRegs )
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de caracterización de UCI.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de caracterización de UCI.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		=FCLOSE(nOutFileHandler)

		* Genera el archivo R contenedor de datos sobre el talento humano disponible en las UPGD
		nOutFileHandler = FCREATE(sHumanResourcesFileNameAndPath + '.txt')
		nRExportedRegs = 0
		TIT('Generando Archivo Plano de Talento Humano / Servicios para ' + sTerritorialUnitName)
		exportationIsOK = ExportFromQRYToPlainFile('TAL' , sUPGDKeyWithPlus + ;
										' IN (SELECT &sPatientsBaseLineKeyWithPlus FROM cPatientsBaseLine)', ;
										 sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nRExportedRegs )
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de Talento Humano / Servicios.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ELSE
				sResultMsg=	'Existen registros repetidos o duplicados en la fuente de Talento Humano / Servicios.' + CHR(13) + ;
							'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
							sTerritorialUnitPlainsFilePath
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDIF
		=FCLOSE(nOutFileHandler)


		* Genera el archivo I contenedor de datos del evento colectivo morbilidad por IRA (Vigilancia Centinela ESI - IRAG);

		TIT('Generando Archivo Plano del Grupo de Eventos 43 para ' + sTerritorialUnitName)
		nOutFileHandler = FCREATE(sIRAFileNameAndPath + '.txt')
		nIExportedRegs = 0
		TRY
			exportationIsOK = ExportFromQRYToPlainFile('E43', sIndividualFilter, sTerritorialUnitPlainsFilePath, .F., , .T., .T., , , nOutFileHandler, @nIExportedRegs )
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
								'Existen registros repetidos o duplicados en la fuente de Morbilidad por IRA.' + CHR(13) + ;
								'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
								'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
								sTerritorialUnitPlainsFilePath  , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ELSE
					sResultMsg=	'Existen registros repetidos o duplicados en la fuente de Morbilidad por IRA.' + CHR(13) + ;
								'El proceso continuó y se guardó un archivo excel con los registros '  + CHR(13) + ;
								'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + ;
								sTerritorialUnitPlainsFilePath
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF
		CATCH TO oException
			nIExportedRegs = 0
		ENDTRY
		=FCLOSE(nOutFileHandler)

		*Genera el archivo SIVIGILA contenedor de datos de cuándo y con qué versión fueron generados ;
		los archivos anterioreS
		DO makeSIVIGILAVersionFile WITH (sSIVIGILAFileNameAndPath), APP_NAME 

		*Genera el archivo de control
		DO makeControlFile WITH sControlFileNameAndPath, sNotifierId, sNotifierFileNamePrefix, nUExportedRegs, ;
								nRExportedRegs, nNExportedRegs, nBExportedRegs,	-1, nDExportedRegs, -1, nIExportedRegs, nVExportedRegs

		*Codifica los archivos recien producidos
		sSivigilaCryptoFileNameAndPath = PATH_TO_SIVIGILACRYPTO + "SivigilaCrypto.prg"
		oCodifier  = NEWOBJECT( "SivigilaCrypto", sSivigilaCryptoFileNameAndPath, .NULL., sTerritorialUnitPlainsFilePath, "*.Txt", sTerritorialUnitPlainsFilePath)
		oCodifier.BatchCodify
		RELEASE oCodifier

		*Produce un archivo empaquetado en formato ZIP para envío
		SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		TRY
			sZipFileName=sTerritorialUnitName + '.ZIP'
			sResultMsg=''
			*DO zip WITH (sTerritorialUnitPlainsFilePath), '*.TXT', sExportationPath , (sZipFileName), sResultMsg
			DO ArchiveFiles WITH (sTerritorialUnitPlainsFilePath), '*.TXT', sExportationPath , (sZipFileName), sResultMsg, , sPwd IN Backup
			IF EMPTY(sResultMsg) THEN
				*COPY FILE (sZipFileName) TO (sExportationPath) 
			ELSE
				sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
							sResultMsg + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
							"Debe crear usted mismo el archivo ZIP para envío."
				IF !bBeSilent THEN
					=showErrorMessage(sResultMsg, 0)
				ELSE
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ENDIF
			*DELETE FILE (sZipFileName)
		CATCH TO oException
			sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
						"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
						"Mensaje del problema : "  + (oException.Message) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
						"Debe crear usted mismo el archivo ZIP para envío."
			IF !bBeSilent THEN
				=showErrorMessage(sResultMsg, 0)
			ELSE
				=FPUTS(nResultsFileHandler,sResultMsg)
			ENDIF
		ENDTRY
		
		sNotificatioMsg='Se ha generado la información de retroalimentación para la entidad territorial.' + CHR(13) + + CHR(13) +;
						'Los archivos planos han sido ubicados en la carpeta: ' + sTerritorialUnitPlainsFilePath 
		IF !bDoNotShowTerminationMsg THEN
			=showErrorMessage(sNotificatioMsg, 0)
		ENDIF

		sEndTime=TIME()
		IF bBeSilent THEN
			=FPUTS(nResultsFileHandler,CR_LF + sNotificatioMsg)
			=FPUTS(nResultsFileHandler,'Hora de inicio: ' + sInitTime)
			=FPUTS(nResultsFileHandler,'Hora de finalización: ' + sEndTime)			
			=FCLOSE(nResultsFileHandler)
		ENDIF

		DO cleanTheHouse IN IMPORTAR_CLICK
	ENDIF
ENDPROC

#INCLUDE SIVIGILA.h
#INCLUDE GlobalConst.h

#DEFINE GARBAGE_CHARS "+.\/:*?',{}<>_&;^()=#$%[]" + '"' 
#DEFINE DEFAULT_BASIC_DATA_TABLE "PACIENTE"

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
	DO cleanTheHouse
		
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
			
			DO cleanTheHouse
			SELECT cTargetInsurers
		ENDSCAN
		
		sEndTime=TIME()
		DO cleanTheHouse
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

		DO cleanTheHouse
		
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
	
	DO cleanTheHouse

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

		DO cleanTheHouse
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

	DO cleanTheHouse

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
	
	DO cleanTheHouse
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
	*IF !USED(errorsFileName) THEN
	*	USE (errorsFilePath+ '\' +  errorsFileName) IN 0
	*ENDIF

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
		
		
	insertRecsClause='INSERT INTO "' + errorsFilePath + '\ERRORES" (ARCHIVO, REG, DES_ARC, COD_PRE, COD_SUB, EVENTO,' ;
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

	IF _SvgVersion = _VERSION2010 THEN
		*Las tablas de importación se crean con base en los queries de exportación usados en la versión 2010

		IF PCOUNT()=2 THEN
			nMajor=0
			nMinor=0
			nBuild=0
		ENDIF

		SELECT DISTINCT impTableNa, ShortName, BD_source FROM ExportQueries WHERE Activo INTO CURSOR rsTablesToMake		
		SELECT rsTablesToMake
		SCAN
			_EventFName = ALLTRIM(rsTablesToMake.impTableNa)
			compleTableName = ''
			makeImporTableSQLClause = GetQryString(UPPER(rsTablesToMake.ShortName), @compleTableName, nMajor, nMinor, nBuild)

			IF !EMPTY(makeImporTableSQLClause) THEN
				* SE AGREGAN LOS CAMPOS FALTANES, NECESARIOS PARA LA IMPORTACION 
				modifyImporTableSQLClause(compleTableName, @makeImporTableSQLClause)
				makeImporTableSQLClause = makeImporTableSQLClause+ " WHERE .F. INTO CURS C_TEMP "
				
				IF !EMPTY(rsTablesToMake.BD_source) THEN 
					makeImporTableSQLClause = STRTRAN(makeImporTableSQLClause, DEFAULT_BASIC_DATA_TABLE, rsTablesToMake.BD_source,-1,-1,1)
				ENDIF
				
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
						NUM_IDE C(20),;
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
				TRY 
					COPY TO (xlsFileName) XL5
				CATCH TO oException
				ENDTRY 
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

	*Crea índices para la tabla sTargetTableName que se encuentra en la carpeta sTablePath, según se encuentren;
	 definidos en Tables.MASTER_INDEX
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	
	LOCAL sSqlCmd as String 
	
	LOCAL nOldSelected as Number 
	STORE SELECT() TO nOldSelected

	DIMENSION aIndexExpression(1)

	SELECT TAG_NAME, TAG_EXPR, EXTRAS FROM MASTER_INDEX	WHERE TABLE_NAME  = '&sTargetTableName' INTO ARRAY aIndexExpression
	IF _TALLY = 0 THEN
		IF 'PLANO_E' $ sTargetTableName THEN 
			*Utiliza índices por default
			SELECT TAG_NAME, TAG_EXPR, EXTRAS FROM MASTER_INDEX	WHERE TABLE_NAME  = 'PLANO_EXX' INTO ARRAY aIndexExpression
		ENDIF
	ENDIF

	IF _TALLY > 0
		WAIT WINDOW NOWAIT 'Creando índice para ' + sTargetTableName
		=SelectTable(ADDBS(sTablePath) + sTargetTableName,'',.T.)
		FOR I = 1 TO ALEN(aIndexExpression,1)
			sSqlCmd = 'INDEX ON ' + ALLTRIM(aIndexExpression(I,2)) + ' TAG ' + LEFT(ALLTRIM(aIndexExpression(I,1)) + '_' + aIndexExpression(I,3),10)
			TRY 
				&sSqlCmd
			CATCH TO oException
			ENDTRY 
		ENDFOR
	ENDIF

	TRY
		USE IN (sTargetTableName)
	CATCH TO oException
	ENDTRY
	
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

	IF VARTYPE(nExportedLines)!='N' THEN 
		nExportedLines = 0
	ENDIF 
	
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
	
	DO cleanTheHouse

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

		DO cleanTheHouse
	ENDIF
ENDPROC


PROCEDURE MakeOrdinaryNotification (sExportationPath as String, sNotificationWeek as String, sIsImmediateNotif as String, ;
									bBeSilent as Boolean, nResultsFileHandler as Number, bIncludeUPGDs as Boolean)

	*sExportationPath: ruta de la carpeta en la que quedarán los archivos resultado de la notificación. Algunos archivos quedarán;
	directamente en esta ruta, otros quedarán en la subcarpeta PLANOS_SEMANA_nn o _INMEDIATA_nn con nn=sNotificationWeek 
	 
	*sNotificationWeek: semana epidemiológica de notificación: 01 a 53 o RA, en caso de tratarse de producción de archivos de retroalimentación
	
	*sIsImmediateNotif: si es no vacía, indica que la notificación es de naturaleza inmediata
	
		* bBeSilent: si es .F. los mensajes del procesamiento se dirigen a la interfaz de usuario (valor por defecto);
				 en caso contrario, se dirigen a un archivo plano de resultados	
				 
	*sResultsFileName: nombre y ruta del archivo texto que contendrá los resultados de procesamiento en caso de que bBeSilent sea .T.

	*bIncludeUPGDs: si es .T., genera el archivo plano de notificación U; en caso contrario (default), no lo genera. 

	LOCAL oDataExporter as Object 
	oDataExporter=NEWOBJECT("DataExporter","DataExporter.Prg",.null.)
	*An object to export intermediate results to XLS

	LOCAL oNotifierUnit as Object 
	oNotifierUnit = NEWOBJECT("NotifierUnit","NotifierUnit.fxp")

	LOCAL sOldProcedure as String

	LOCAL sDirListing as String
	sDirListing = ''

	LOCAL sWeeklyTargetPath as String
	*Ruta hacia la cual se exportarán los archivos correspondientes a la semana m.SEM_REP

	LOCAL sZippedExportationPath as String
	sZippedExportationPath = sExportationPath + "\PLANOS_SIVIGILA_4_0_" + STRTRAN(TTOC(DATETIME(),3),":","_")
	*Ruta hacia la cual se exportarán los paquetes de archivos correspondientes a la semana m.SEM_REP ;
	con destino al sistema 4.0

	LOCAL sInformatUnitCode as String 
	*Código de la unidad notificadora según aparece registrado en el campo GENERAL.UNI_NOT
		
	DIMENSION aPlains(1)


	sOldProcedure = SET("Procedure")

	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	SET PROCEDURE TO (PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAMessenger ') ADDITIVE


	sInitTime=TIME()

	DO cleanTheHouse

	IF VARTYPE(sIsImmediateNotif)!='C' THEN 
		sIsImmediateNotif = ''
	ENDIF 
	
	*Comentado por: Wilson Aguilar Fecha: 30/11/2009;
	Fija el Path de la carpeta en donde se almacenarán los archivos que resultarán como producto ;
	del proceso de exportación
	sWeeklyTargetPath = sExportationPath + '\PLANOS_SEMANA_' + TRANSFORM(sNotificationWeek,'@L 99')
	IF !EMPTY(sIsImmediateNotif) THEN
		sWeeklyTargetPath = sWeeklyTargetPath + '_INMEDIATA_' + STRTRAN(TTOC(DATETIME(),3),":","_")
	ELSE
		IF !bBeSilent THEN	
			nExistentPlainDir = ADIR(aPlains, sWeeklyTargetPath + '*' + ALLTRIM(STR(YEAR(DATE()))) + '*',"D") 
			IF nExistentPlainDir >=1 THEN
				FOR iPlainDir = 1 TO MIN(6,nExistentPlainDir)
					sDirListing = sDirListing + aPlains(iPlainDir ,1) + chr(13)
				NEXT iPlainDir 
				sDirListing = sDirListing + '...'
				IF getYesOrNo('Se ha encontrado una o más notificaciones generadas para la semana objetivo:' + chr(13) + chr(13) + ;
					sDirListing + chr(13) + chr(13) + 'Desea Continuar de todos modos',"SIVIGILA") = _NO THEN
					RETURN
				ENDIF
			ENDIF
		ENDIF
		sWeeklyTargetPath = sWeeklyTargetPath + '_' + STRTRAN(TTOC(DATETIME(),3),":","_")
	ENDIF
	sWeeklyTargetPath = ADDBS(sWeeklyTargetPath)
	IF !DIRECTORY(sWeeklyTargetPath)
	   MKDIR (sWeeklyTargetPath)
	ENDIF
	oDataExporter.sExportationPath = sWeeklyTargetPath

	*Comentado por: Wilson Aguilar Fecha: 30/11/2009;
	Fija los nombres de los archivos que resultarán como producto del proceso de exportación

	*Se ha de tener en cuenta las siguientes siglas para los nombres de los archivos a remitir ;
	por parte de las Unidades Primarias Generadoras del Dato (UPGD) y las Unidades Notificadoras (UN);

	*Sigla;
	U		Archivo plano contenedor de datos de caracterización de UPGD.;
	R		Archivo plano contenedor de datos sobre el talento humano disponible en las UPGD;
	N		Archivo plano contenedor de datos básicos de notificación individual periódica semanal;
	B		Archivo plano contenedor de datos de notificación colectiva periódica semanal obligatoria;
	D		Archivo plano contenedor de datos complementarios de notificación periódica semanal obligatoria;
	C		Archivo plano de control;
	T		Archivo plano  contenedor de datos agrupados de los eventos de notificación individual periódica semanal obligatoria.	1;
	I		Archivo plano contenedor de datos del evento colectivo morbilidad por IRA (Vigilancia Centinela ESI - IRAG);
	V		Archivo plano contenedor de datos de caracterización de UCI en UPGD;
	SIVIGILA	Archivo plano contenedor de datos de cuándo y con qué versión fueron generados los archivos anteriore;


	*Establece el prefijo identificador de la Unidad que notifica para efectos de nombrar los archivos;
	planos resultantes del procesamiento
	sInformatUnitCode = fieldValue("UNI_NOT","GENERAL")

	
	WITH oNotifierUnit
		DO CASE 
			CASE .bIsUND
				*El nombre del archivo plano debe ser SIGLA + CODIGO DEPARTAMENTO +  SEMANA EPIDEMIOLÓGICA.txt;
				 por tanto inmediatamente después de la Sigla deben venir los dos dígitos asignados por el DANE para;
				 identificar los departamentos (Se debe agregar 3 ceros al código del departamento)
				TA=SUBSTR(ALLTRIM(sInformatUnitCode),1,2) + '000' + PADL(sNotificationWeek,2,'0')
			CASE .bIsUPGD
				*El nombre del archivo plano debe ser SIGLA + CODIGO UPGD +  SEMANA EPIDEMIOLÓGICA.txt;
				 por tanto inmediatamente después de la Sigla deben venir los cinco dígitos asignados consecutivamente;
				 al prestador de servicios de salud
				TA = SUBSTR(ALLTRIM(sInformatUnitCode),6,5) + PADL(sNotificationWeek,2,'0')
			CASE .bIsUNM
				*El nombre del archivo plano debe ser SIGLA + CODIGO MUNICIPIO +  SEMANA EPIDEMIOLÓGICA.txt;
				 por tanto inmediatamente después de la Sigla deben venir los cinco dígitos asignados por el DANE para ;
				 identificar los municipios
				TA=SUBSTR(ALLTRIM(sInformatUnitCode),1,5) + PADL(sNotificationWeek,2,'0')
		ENDCASE  
	ENDWITH
	RELEASE oNotifierUnit

	TA = TA + '.txt'
	CAR=sWeeklyTargetPath+'U'+TA
	TAL=sWeeklyTargetPath+'R'+TA
	PTE=sWeeklyTargetPath+'N'+TA
	DAT=sWeeklyTargetPath+'D'+TA
	BRO=sWeeklyTargetPath+'B'+TA
	TOT=sWeeklyTargetPath+'T'+TA
	CON=sWeeklyTargetPath+'C'+TA
	INS=sWeeklyTargetPath+'SIVIGILA.TXT'
	STT=sWeeklyTargetPath+'S'+TA		&& (7) NOMBRE DEL ARCHIVO DE ESTRUCTURAS
	PTW=sWeeklyTargetPath+'W'+TA		&& (?) NOMBRE DEL ARCHIVO PACIENTE WEB
	IRA=sWeeklyTargetPath+'I'+TA
	UCIFileName=sWeeklyTargetPath+'V'+TA 


	*Comentado por: Wilson Aguilar Fecha: 30/11/2009;
	Inicializa los contadores de registros exportados por tipo de archivo
	REG_P=0
	REG_D=0
	REG_B=0
	REG_C=0
	REG_T=0
	REG_U=0
	REG_W=0		&&(?) CONTADOR DEL ARCHIVO PACIENTE WEB
	REG_I=0		&&(?) CONTADOR DE MORBILIDAD POR IRA
	REG_V=0

	*Comentado por: Wilson Aguilar	Fecha: 04/02/2015;
	La siguiente parte de código se cambió por una que exporta la información sin tener en cuenta que las UPGDs involucradas estén activas;
	en este sentido, la modificación hecha obliga a que se exporten todos los datos pendientes por notificar sin importar si las UPGDs ;
	correspondientes a ellos están o no activas en el sistema. Con ésto se supera el error con código ... 
	&&bCheckEnabledUPGD=.T.
	&&IF VARTYPE(m.SEM_REP)="C" THEN
	&&	bCheckEnabledUPGD=!(m.SEM_REP='RA')
	&&ENDIF
	*Por default, en la exportación de información, se tienen en cuenta solamente las UPGDs que están activas en el Sistema;
	sin embargo, si la exportación de datos se refiere a una retroalimentación de información, entonces se exporta la información;
	de todas las UPGDs sin tener en cuenta si están activas o no.
	bCheckEnabledUPGD = .F.


	*Comentado por: Wilson Aguilar	Fecha: 04/02/2015;
	En concordancia con comentario de la parte de código antecedente, la siguiente parte de código hubo de cambiarse;
	con el objeto de mantener la activación automática siempre y cuando no se tratará de una retroalimentación de información.
	* Activa automáticamente UPGDs que figuren como inactivas pero que tengan datos por Notificar siempre y cuando;
	la exportación de datos NO se refiera a una retroalimentación de información
	&&IF bCheckEnabledUPGD THEN
	&&	DO activateNonActiveUPGDsWithNotification IN plainsLib
	&&ENDIF
	bIsFeedback = .F.
	IF VARTYPE(sNotificationWeek)="C" THEN
		bIsFeedback = (sNotificationWeek='RA')
	ENDIF
	IF !bIsFeedback THEN
		DO activateNonActiveUPGDsWithNotification IN plainsLib
	ENDIF

	IF !EMPTY(sIsImmediateNotif) THEN
		sIsImmediateNotif = " AND RecordSource.INMEDIATA='SI' AND RecordSource.EST_NOTIF=" + NOT_NOTIFIED
	ENDIF


	* Genera el archivo N de datos básicos de notificación individual periódica semanal
	=showProgressMessage('Generado Archivo Plano de Notificación Individual')
	nCurrentFileHandler = FCREATE(PTE)
	exportationIsOK = ExportFromQryToPlainFile('PTE', FIL_NOT + FIL_EVE + FIL_VIG + sIsImmediateNotif, sWeeklyTargetPath, ;
						bCheckEnabledUPGD,,.T.,,,!EMPTY(sIsImmediateNotif), nCurrentFileHandler, @REG_P ) 
	IF !exportationIsOK THEN
		IF !bBeSilent THEN
			MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
					'Existen registros repetidos o duplicados en la fuente de notificación individual.' + CHR(13) + ;
					'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
					'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
		ENDIF
	ENDIF

	oDataExporter.sSourceTableName = "C_PTE"
	oDataExporter.sExportedFileName = "DatosBasicosIndividuales"
	oDataExporter.exportToXLS()

	*Cierra el archivo plano N
	=FCLOSE(nCurrentFileHandler)
	=FFLUSH(nCurrentFileHandler)
	
	* Genera el archivo D de datos complementarios de notificación periódica semanal obligatoria;
	incluyendo la información de resultados de laboratorio
	nCurrentFileHandler = FCREATE(DAT)
	USE BDSivigila!vTablasDeDC IN 0
	SELECT vTablasDeDC
	SCAN
		eventToExport = vTablasDeDC.ID_DC_TABLA
		IF eventToExport != 43
			* El evento 43 (Morbilidad por IRA) se genera en un archivo independiente
			=showProgressMessage('Generando Archivo Plano del Grupo de Eventos ' + PADL(eventToExport ,2,'0'))
			
			exportationIsOK = ExportFromQryToPlainFile('E'+PADL(eventToExport ,2,'0'), FIL_NOT + FIL_EVE + FIL_VIG + sIsImmediateNotif, ;
														sWeeklyTargetPath, bCheckEnabledUPGD,,.T.,,,,nCurrentFileHandler, @REG_D )
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de datos complementarios.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ENDIF
			ENDIF
		ENDIF
	ENDSCAN

	IF !bIsFeedback THEN
		*Se debe producir archivos de notificación rutinaria para contactos y seguimientos
		
		=showProgressMessage('Generando Archivo Plano de Laboratorios')

		exportationIsOK = ExportFromQryToPlainFile('LAB' , FIL_NOT + FIL_EVE + FIL_VIG + sIsImmediateNotif, sWeeklyTargetPath, ;
													bCheckEnabledUPGD,,.T.,,,,nCurrentFileHandler, @REG_D)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
						'Existen registros repetidos o duplicados en la fuente de notificación laboratorios.' + CHR(13) + ;
						'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
						'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ENDIF
		ENDIF


		=showProgressMessage('Generando Archivo Plano de Contactos')

		exportationIsOK = ExportFromQryToPlainFile('CNTA' , FIL_NOT + FIL_EVE + FIL_VIG + sIsImmediateNotif, sWeeklyTargetPath, ;
													bCheckEnabledUPGD,,.T.,.T.,,,nCurrentFileHandler, @REG_D)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
						'Existen registros repetidos o duplicados en la fuente de notificación contactos.' + CHR(13) + ;
						'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
						'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ENDIF
		ENDIF

		=showProgressMessage('Generando Archivo Plano de Seguimiento a contactos')

		exportationIsOK = ExportFromQryToPlainFile('SGCN' , FIL_NOT + FIL_EVE + FIL_VIG + sIsImmediateNotif, sWeeklyTargetPath, ;
													bCheckEnabledUPGD,,.T.,.T.,,,nCurrentFileHandler, @REG_D)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
						'Existen registros repetidos o duplicados en la fuente de notificación seguimiento a contactos.' + CHR(13) + ;
						'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
						'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ENDIF
		ENDIF
		
	ELSE
		=showProgressMessage('Generando Archivo Plano de Laboratorios')
		exportationIsOK = ExportFromQryToPlainFile('LAB' , FIL_NOT + FIL_EVE + FIL_VIG + sIsImmediateNotif, sWeeklyTargetPath, ;
												bCheckEnabledUPGD,,.T.,,,,nCurrentFileHandler, @REG_D)
		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
						'Existen registros repetidos o duplicados en la fuente de notificación laboratorios.' + CHR(13) + ;
						'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
						'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ENDIF
		ENDIF
	ENDIF 

	*Cierra el archivo plano D de datos complementarios
	=FCLOSE(nCurrentFileHandler)
	=FFLUSH(nCurrentFileHandler)

	* Genera el archivo B de datos de notificación colectiva periódica semanal obligatoria
	=showProgressMessage('Generando Archivo Plano de Notificación Colectiva')
	nCurrentFileHandler = FCREATE(BRO)
	exportationIsOK = ExportFromQryToPlainFile('BRO', FIL_NOT + FIL_EVE + FIL_VIG + sIsImmediateNotif, sWeeklyTargetPath, ;
												bCheckEnabledUPGD,,.T.,,,,nCurrentFileHandler, @REG_B)
	IF !exportationIsOK THEN
		IF !bBeSilent THEN
			MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
					'Existen registros repetidos o duplicados en la fuente de notificación colectiva.' + CHR(13) + ;
					'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
					'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
		ENDIF
	ENDIF

	oDataExporter.sSourceTableName = "C_BRO"
	oDataExporter.sExportedFileName = "DatosColectivos"
	oDataExporter.exportToXLS()

	*Cierra el archivo plano B
	=FCLOSE(nCurrentFileHandler)
	=FFLUSH(nCurrentFileHandler)


	* Genera el archivo U contenedor de datos de caracterización de UPGD
	IF !bIsFeedback OR bIncludeUPGDs THEN
		nCurrentFileHandler = FCREATE(CAR)
	
		=showProgressMessage('Generando Archivo Plano de las UPGD')
		* Comentado por: wilson Aguilar	Fecha: 23/04/2011 ;
		La siguiente parte de código fue modificada por solicitud del Grupo SIVIGILA del INS del 05/04/2011 en donde se indicó que en los procesos de;
		notificación se extrajera la información de todas las UPGDs; por tanto, en la llamada a ExportFromQRY se suprimió el filtro FIL_NOT
		*exportationIsOK = ExportFromQRY('UPGD' , FIL_NOT, @A_PlainTableKeys, sWeeklyTargetPath, .F.)
		exportationIsOK = ExportFromQryToPlainFile('UPGD', '', sWeeklyTargetPath, .F.,,.T.,,,,nCurrentFileHandler, @REG_U)

		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de UPGDs.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ENDIF 
		ENDIF

		*Cierra el archivo plano U 
		=FCLOSE(nCurrentFileHandler)
		=FFLUSH(nCurrentFileHandler)
	ENDIF

	* Genera el archivo V contenedor de datos de UCI por UPGD
	IF !bIsFeedback  OR bIncludeUPGDs THEN
		nCurrentFileHandler = FCREATE(UCIFileName)
	
		=showProgressMessage('Generando Archivo Plano de UCI por UPGD')
		exportationIsOK = ExportFromQryToPlainFile('UCI' , FIL_NOT, sWeeklyTargetPath, .F.,,.T.,,,,nCurrentFileHandler, @REG_V)

		IF !exportationIsOK THEN
			IF !bBeSilent THEN
				MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
						'Existen registros repetidos o duplicados en la fuente de UCI por UPGDs.' + CHR(13) + ;
						'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
						'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
			ENDIF
		ENDIF

		*Cierra el archivo plano V
		=FCLOSE(nCurrentFileHandler)
		=FFLUSH(nCurrentFileHandler)
	ENDIF

	* Si se trata de una notificación rutinaria, genera el archivo R contenedor de datos sobre el talento humano disponible en las UPGD
	IF !bIsFeedback THEN
		IF EMPTY(sIsImmediateNotif) THEN
			nCurrentFileHandler = FCREATE(TAL)
		
			=showProgressMessage('Generando Archivo Plano de Talento Humano / Servicios')

			exportationIsOK = ExportFromQryToPlainFile('TAL' , 'recordSource.NOTIFICA=0 AND recordSource.ESTADO=0', sWeeklyTargetPath, ;
														bCheckEnabledUPGD,,.T.,,,,nCurrentFileHandler, @REG_T )
			IF !exportationIsOK THEN
				IF !bBeSilent THEN
					MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
							'Existen registros repetidos o duplicados en la fuente de Talento Humano / Servicios.' + CHR(13) + ;
							'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
							'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
				ENDIF
			ENDIF
			
			*Cierra el archivo plano R
			=FCLOSE(nCurrentFileHandler)
			=FFLUSH(nCurrentFileHandler)
		ELSE
			REG_T = 0
		ENDIF
	ENDIF

	* Genera el archivo I contenedor de datos del evento colectivo morbilidad por IRA (Vigilancia Centinela ESI - IRAG)
	nCurrentFileHandler = FCREATE(IRA)
	
	=showProgressMessage('Generando Archivo Plano Morbilidad por IRA')
	
	exportationIsOK  = ExportFromQryToPlainFile('E43' , FIL_NOT + FIL_EVE + FIL_VIG + sIsImmediateNotif, sWeeklyTargetPath, ;
												bCheckEnabledUPGD,,.T.,,,,nCurrentFileHandler, @REG_I)
	IF !exportationIsOK THEN
		IF !bBeSilent THEN
			MESSAGEBOX( 'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + + CHR(13) +;
					'Existen registros repetidos o duplicados en la fuente de Morbilidad por IRA.' + CHR(13) + ;
					'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
					'repetidos o duplicados para los fines que se considere pertinentes en:' + CHR(13) + sWeeklyTargetPath , 0 + 16 + 256, 'Aviso de exportación SIVIGILA')
		ENDIF
	ENDIF
	oDataExporter.sSourceTableName = "C_E43"
	oDataExporter.sExportedFileName = "DatosIrag"
	oDataExporter.exportToXLS()

	*Cierra el archivo plano I
	=FCLOSE(nCurrentFileHandler)
	=FFLUSH(nCurrentFileHandler)

	* Genera el archivo T contenedor de datos agrupados de los eventos de notificación individual y de brotes periódica semanal obligatoria
	=showProgressMessage('Generando Archivo T...')
	SELECT COD_EVE, DATE() AS FEC_NOT, SEMANA, AÑO , COD_PRE, COD_SUB, SUM(VAL(GRU_1)) AS GRU_1, SUM(VAL(GRU_2)) AS GRU_2,;
		SUM(VAL(GRUPO_3)) AS GRU_3,	SUM(VAL(GRUPO_4)) AS GRU_4,	SUM(VAL(GRUPO_5)) AS GRU_5,	SUM(VAL(GRUPO_6)) AS GRU_6, ;
		SUM(VAL(GRUPO_7)) AS GRU_7,	SUM(VAL(GRUPO_8)) AS GRU_8,	SUM(VAL(GRUPO_9)) AS GRU_9,	SUM(VAL(GRUPO_10)) AS GRU_10, ;
		SUM(VAL(GRUPO_11)) AS GRU_11, SUM(VAL(GRUPO_12)) AS GRU_12,	SUM(VAL(GRUPO_13)) AS GRU_13, SUM(VAL(GRUPO_14)) AS GRU_14, ;
		SUM(VAL(GRUPO_15)) AS GRU_15, SUM(VAL(GRUPO_16)) AS GRU_16,	SUM(VAL(GRUPO_17)) AS GRU_17, SUM(VAL(GRUPO_18)) AS GRU_18, ;
		SUM(VAL(CAS_PRO)) AS CAS_PRO,  SUM(VAL(CAS_CONL)) AS CAS_CONL, SUM(VAL(CAS_CONC)) AS CAS_CONC,;
		SUM(VAL(CAS_CONN)) AS CAS_CONN, SUM(VAL(HOMBRES)) AS HOMBRES, SUM(VAL(MUJERES)) AS MUJERES,;
		SUM(VAL(VIVOS)) AS VIVOS, SUM(VAL(MUERTOS)) AS MUERTOS, MUN_PRO;
		FROM BROTES AS recordSource;
		WHERE &FIL_NOT  &FIL_EVE &FIL_VIG &sIsImmediateNotif;
		GROUP BY AÑO, COD_PRE, COD_SUB, SEMANA, MUN_PRO, COD_EVE;
		INTO CURSOR rsConteoBrotes READWRITE 

	SELECT COD_EVE, DATE() AS FEC_NOT, SEMANA, AÑO , COD_PRE, COD_SUB, ;
		SUM(IIF(UNI_MED!='1',1,0)) AS GRU_1,;
		SUM(IIF(UNI_MED='1',IIF(BETWEE(VAL(EDAD),1,4),1,0),0)) AS GRU_2,;
		SUM(IIF(UNI_MED='1',IIF(BETWEE(VAL(EDAD),5,14),1,0),0)) AS GRU_3,;
		SUM(IIF(UNI_MED='1',IIF(BETWEE(VAL(EDAD),15,44),1,0),0)) AS GRU_4,;
		SUM(IIF(UNI_MED='1',IIF(BETWEE(VAL(EDAD),45,64),1,0),0)) AS GRU_5,;
		SUM(IIF(UNI_MED='1',IIF(VAL(EDAD)>=65,1,0),0)) AS GRU_6,;
		SUM(IIF(tip_cas='1',1,0)) AS cas_pro,;
		SUM(IIF(tip_cas='2',1,0)) AS cas_conl,;
		SUM(IIF(tip_cas='3',1,0)) AS cas_conc,;
		SUM(IIF(tip_cas='4',1,0)) AS cas_conn,;
		SUM(IIF(CON_FIN='1',1,0)) AS VIVOS,;
		SUM(IIF(CON_FIN='2',1,0)) AS MUERTOS, COD_MUN as mun_pro;
		FROM paciente as recordSource;
		WHERE &FIL_NOT&FIL_EVE &FIL_VIG &sIsImmediateNotif AND COD_EVE!='000 ';
		GROUP BY AÑO, COD_PRE, COD_SUB, SEMANA, COD_MUN, COD_EVE;
		INTO CURSOR rsConteoIndividuales

	SELECT rsConteoBrotes 
	APPEND FROM DBF('rsConteoIndividuales')

	nCurrentFileHandler = FCREATE(TOT)
	SELECT rsConteoBrotes 
	GO TOP
	SCAN
	   sRecStream = COD_EVE+','+TF(FEC_NOT)+','+SEMANA+','+AÑO+','+COD_PRE+','+COD_SUB+','+NC(GRU_1)+','+NC(GRU_2)+','+;
	      NC(GRU_3)+','+NC(GRU_4)+','+NC(GRU_5)+','+NC(GRU_6)+','+NC(CAS_PRO)+','+NC(CAS_CONL)+','+NC(CAS_CONC)+','+NC(CAS_CONN)+','+;
	      NC(HOMBRES)+','+NC(MUJERES)+','+NC(VIVOS)+','+NC(MUERTOS)+','+ LEFT(MUN_PRO,2)+','+RIGHT(MUN_PRO,3)

		=FPUTS(nCurrentFileHandler, sRecStream)
	ENDSCAN
	REG_C = RECCOUNT('rsConteoBrotes')
	USE IN rsConteoBrotes
	USE IN rsConteoIndividuales
	

	*Cierra el archivo plano T
	=FCLOSE(nCurrentFileHandler)
	=FFLUSH(nCurrentFileHandler)

	*Genera el archivo XLS de notificación negativa de eventos 
	IF SelectTable("rsNegativeNotificationReport",,.F.) THEN
		=showProgressMessage('Generando Archivo XLS de notificación negativa de eventos')
		oDataExporter.sSourceTableName = "rsNegativeNotificationReport"
		oDataExporter.sExportedFileName = "Eventos_Notificacion_Negativa_Semana_" + TRANSFORM(sNotificationWeek,'@L 99') + "_" + STRTRAN(TTOC(DATETIME(),3),":","_")
		oDataExporter.exportToXLS()
	ENDIF

	*Genera el archivo SIVIGILA contenedor de datos de cuándo y con qué versión fueron generados ;
	los archivos planos de notificación o retroalimentación
	DO makeSIVIGILAVersionFile WITH (INS), (_ver) IN PlainsLib

	L=0
	SET DEVICE TO FILE (CON)
	IF REG_U<>0
	   @L+0,0 SAY LEFT(sInformatUnitCode,10)+','+RIGHT(sInformatUnitCode,2)+','+TF(DATE())+','+'U'+JUSTSTEM(TA)+','+TRANS(REG_U,'@l 9999999')
	   L=L+1
	ENDIF
	IF REG_T<>0
	   @L+0,0 SAY LEFT(sInformatUnitCode,10)+','+RIGHT(sInformatUnitCode,2)+','+TF(DATE())+','+'R'+JUSTSTEM(TA)+','+TRANS(REG_T,'@l 9999999')
	   L=L+1
	ENDIF
	IF REG_P<>0
	   @L+0,0 SAY LEFT(sInformatUnitCode,10)+','+RIGHT(sInformatUnitCode,2)+','+TF(DATE())+','+'N'+JUSTSTEM(TA)+','+TRANS(REG_P,'@l 9999999')
	   L=L+1
	ENDIF
	IF REG_B<>0
	   @L+0,0 SAY LEFT(sInformatUnitCode,10)+','+RIGHT(sInformatUnitCode,2)+','+TF(DATE())+','+'B'+JUSTSTEM(TA)+','+TRANS(REG_B,'@l 9999999')
	   L=L+1
	ENDIF
	IF REG_C<>0
	   @L+0,0 SAY LEFT(sInformatUnitCode,10)+','+RIGHT(sInformatUnitCode,2)+','+TF(DATE())+','+'T'+JUSTSTEM(TA)+','+TRANS(REG_C,'@l 9999999')
	   L=L+1
	ENDIF
	IF REG_D<>0
	   @L+0,0 SAY LEFT(sInformatUnitCode,10)+','+RIGHT(sInformatUnitCode,2)+','+TF(DATE())+','+'D'+JUSTSTEM(TA)+','+TRANS(REG_D,'@l 9999999')
	   L=L+1
	ENDIF

	* Comentado por: wilson aguilar	Fecha: 20/10/2011 ;
	El Grupo SIVIGILA del INS decidió que con la veersión 2012, el Sistema ya no debe generar el archivo W. 
	&&IF REG_W<>0
	&&   @L+0,0 SAY LEFT(_un,10)+','+RIGHT(_un,2)+','+TF(DATE())+','+'W'+TA+','+TRANS(REG_W,'@l 9999999')
	&&   L=L+1
	&&ENDIF

	IF REG_I<>0
	   @L+0,0 SAY LEFT(sInformatUnitCode,10)+','+RIGHT(sInformatUnitCode,2)+','+TF(DATE())+','+'I'+JUSTSTEM(TA)+','+TRANS(REG_I,'@l 9999999')
	   L=L+1
	ENDIF
	IF REG_V<>0
	   @L+0,0 SAY LEFT(sInformatUnitCode,10)+','+RIGHT(sInformatUnitCode,2)+','+TF(DATE())+','+'V'+JUSTSTEM(TA)+','+TRANS(REG_V,'@l 9999999')
	   L=L+1
	ENDIF

	SET DEVICE TO SCREEN 
	*SET STEP ON 
	*Codifica los archivos recien producidos
	sSivigilaCryptoFileNameAndPath = PATH_TO_SIVIGILACRYPTO + "SivigilaCrypto.prg"
	oCodifier  = NEWOBJECT( "SivigilaCrypto", sSivigilaCryptoFileNameAndPath, .NULL., sWeeklyTargetPath, "*.Txt", sWeeklyTargetPath)
	oCodifier.BatchCodify
	RELEASE oCodifier
SET STEP ON 
	*Produce un archivo empaquetado en formato ZIP para envío a la Unidad Jerarquíca Superior
	bClaimNotificationIsFinished=.T.
	TRY
		*Establece el nombre del archivo ZIP correspondiente a los planos generados
		sPlainsFilesPath = JUSTPATH(sWeeklyTargetPath)
		IF !EMPTY(sIsImmediateNotif) THEN
			sTargetZIPFileName = getZipFileName(PADL(sNotificationWeek,2,'0') + '_INMEDIATA')
		ELSE
			sTargetZIPFileName = getZipFileName(PADL(sNotificationWeek,2,'0'))
		ENDIF 
		IF !bIsFeedback THEN 
			sTargetXLSZIPFileName = sTargetZIPFileName + '_XLS'
			sTargetZIPFileName = sTargetZIPFileName + '.ZIP'
			sTargetXLSZIPFileName = sTargetXLSZIPFileName  + '.ZIP'
		ELSE 
			sTargetZIPFileName = JUSTFNAME(sPlainsFilesPath) + '.ZIP'
			sTargetXLSZIPFileName = JUSTFNAME(sPlainsFilesPath) + '_XLS' + '.ZIP'
		ENDIF 
				
		sResultMsg=''
		lFileZip=.F. &&Para procesos de enviar correo verificar si el archivo zip fue creado
		IF !bIsFeedback THEN 
			DO ArchiveFiles WITH sWeeklyTargetPath, '*.TXT', sZippedExportationPath, sTargetZIPFileName, sResultMsg IN backup
			DO ArchiveFiles  WITH sWeeklyTargetPath, '*.XLS', sZippedExportationPath, sTargetXLSZIPFileName , sResultMsg, .T. IN backup
		ELSE 
			DO ArchiveFiles WITH sWeeklyTargetPath, '*.TXT', sExportationPath + '\', sTargetZIPFileName, sResultMsg IN backup
			DO ArchiveFiles  WITH sWeeklyTargetPath, '*.XLS', sExportationPath + '\', sTargetXLSZIPFileName , sResultMsg, .T. IN backup
		ENDIF 
		
		IF EMPTY(sResultMsg) THEN
			lFileZip=.T.
		ELSE
			bClaimNotificationIsFinished = .F.
			sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
						sResultMsg + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
						"Debe crear usted mismo el archivo ZIP para envío a la Unidad Jerarquíca Superior."
		ENDIF
	CATCH TO oException
		bClaimNotificationIsFinished = .F.
		sResultMsg="Se produjo el siguiente problema al intentar crear el archivo ZIP correspondiente a los planos: " +  CHR(13) +  CHR(10) +;
					"Código interno del problema : " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
					"Mensaje del problema : "  + (oException.Message) + CHR(13)  +  CHR(10) + CHR(13)  +  CHR(10) +;
					"Debe crear usted mismo el archivo ZIP para envío a la Unidad Jerarquíca Superior."
	ENDTRY

	*SET STEP ON 
	*Actualiza el campo NOTIFICA de las tablas maestras con el número de la semana que se notificó
	IF opc_exp = ORDINARY_NOTIFICATION THEN
		*Se trata de una notificación de información usual, esto es, no es de caracter inmediato ni es;
		una retroalimentación
		
		*Establece la condición de actualización de los registros de las tablas maestras
		updateCondition = 'NOTIFICA=0'
		
		*Comentado por wilson Aguilar 03/12/2009;
		La siguiente parte del código debe incorporarse si el INS decide que las notifcaciones de caracter;
		inmediato no sean re-notificadas, es decir, si decide que una vez que se ha hecho una ;
		notificación inmediata, en un tiempo posterior no pueda volver a realizarse la notificación de ;
		los registros que fueron objeto de notificación inmediata. En ese caso, también tendrá que;
		cambiarse la condición del IF anterior por IF opc_pexp<>2 THEN ...
		*IF !EMPTY(sIsImmediateNotif) then
		*	updateCondition=updateCondition + ' AND ' + sIsImmediateNotif
		*ENDIF
		
		=showProgressMessage('Cerrando registros...')

		IF VARTYPE(glDisableRules)='L' THEN
			lOldDisableRules=glDisableRules
		ELSE
			lOldDisableRules=.F.
		ENDIF
		glDisableRules=.T.
		
		sDataTablaToUpdate = 'PACIENTE'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE NOTIFICA WITH sNotificationWeek, EST_NOTIF WITH nORDINARY_NOTIFIED FOR NOTIFICA=0 AND EST_INGR!=2

		USE BDSivigila!vTablasDeDC IN 0
		SELECT vTablasDeDC
		SCAN
			sDataTablaToUpdate = ALLTRIM(vTablasDeDC.DC_TABLA)
			DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
			IF vTablasDeDC.ID_DC_TABLA != 47 THEN
				REPLACE NOTIFICA WITH sNotificationWeek, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition
			ELSE
				*La tabla EVENTOS_47 almacena registros de un evento cuyos datos pueden ser cargados desde un sistema externo
				REPLACE NOTIFICA WITH sNotificationWeek, EST_NOTIF WITH nORDINARY_NOTIFIED FOR NOTIFICA=0 AND EST_INGR!=2
			ENDIF
		ENDSCAN

		sDataTablaToUpdate = 'BROTES'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE NOTIFICA WITH sNotificationWeek, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

		sDataTablaToUpdate = 'LABORATORIOS'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE NOTIFICA WITH sNotificationWeek, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

		sDataTablaToUpdate = 'CONTACTOS'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE NOTIFICA WITH sNotificationWeek, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

		sDataTablaToUpdate = 'SEGUIMIENTOCONTACTOS'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE NOTIFICA WITH sNotificationWeek, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

		sDataTablaToUpdate = 'UPGD'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE NOTIFICA WITH sNotificationWeek, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

		sDataTablaToUpdate = 'TAL_HUM'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE NOTIFICA WITH sNotificationWeek, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

		sDataTablaToUpdate = 'UPGD_UCIS'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		REPLACE NOTIFICA WITH sNotificationWeek, EST_NOTIF WITH nORDINARY_NOTIFIED FOR &updateCondition

		sDataTablaToUpdate = 'GENERAL'
		DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
		IF VAL(SEMANA)<sNotificationWeek
		   REPLACE SEMANA WITH nc(sNotificationWeek)
		   FLUSH
		   _SEM_RP=VAL(SEMANA)
		ENDIF
		glDisableRules=lOldDisableRules
	ELSE
		IF opc_exp = INMEDIATE_NOTIFICATION THEN
			*Se trata de una notificación de información de caracter inmediato 
			
			*Establece la condición de actualización de los registros de las tablas maestras
			updateCondition = ' INMEDIATA="SI" AND EST_NOTIF=' + NOT_NOTIFIED
			
			=showProgressMessage('Cerrando registros...')

			IF VARTYPE(glDisableRules)='L' THEN
				lOldDisableRules=glDisableRules
			ELSE
				lOldDisableRules=.F.
			ENDIF
			glDisableRules=.T.
			
			sDataTablaToUpdate = 'PACIENTE'
			DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
			REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR NOTIFICA=0 AND &updateCondition 

			USE BDSivigila!vTablasDeDC IN 0
			SELECT vTablasDeDC
			SCAN
				sDataTablaToUpdate =  ALLTRIM(vTablasDeDC.DC_TABLA)
				DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
				REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR &updateCondition
			ENDSCAN

			sDataTablaToUpdate = 'BROTES'
			DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
			REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR &updateCondition

			sDataTablaToUpdate = 'LABORATORIOS'
			DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
			REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR &updateCondition

			sDataTablaToUpdate = 'CONTACTOS'
			DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
			REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR &updateCondition

			sDataTablaToUpdate = 'SEGUIMIENTOCONTACTOS'
			DO SelectTable WITH sDataTablaToUpdate IN PATH_TO_COMMON_LIB + 'TablesHandler'
			REPLACE EST_NOTIF WITH VAL(INMEDIATE_NOTIFIED) FOR &updateCondition

			glDisableRules=lOldDisableRules
		ENDIF
	ENDIF
	CLOSE TABLES ALL
	=showProgressMessage('Producción de archivos planos finalizada')



	*-------------------------------------------------------------------------
	* Actualizacion Autor: José Luis Gonzalez, Fecha: 23/10/2013;
		Propósito: si se trata de una "Notificacion Inmediata" enviar por correo electronico; 
		detalles de la notificacion y los archivos adjuntos, como base el archivos el zip ;
		o los archivos individuales si no se logro general el zip
	IF !EMPTY(sIsImmediateNotif) THEN
		IF lFileZip THEN
			sAdjuntosSendMail = sZippedExportationPath + '\' + sTargetZIPFileName
		ELSE
			sAdjuntosSendMail = bro + ".TXT;" + car + ".TXT;" + tal + ".TXT;" + dat + ".TXT;" + tot + ".TXT;" + con + ".TXT;" + stt + ".TXT;" + ptw + ".TXT;" + ins 
		ENDIF
	*SET STEP ON 
		*Codigo para comunicacion inmediata : 12
		IF !_SCREEN.bTestMode THEN
			sSendMailCmd = "DO SendMailManual WITH 12, '', '' , sAdjuntosSendMail IN " + PATH_TO_SIVIGILAMailer + "sendmail.prg"
			&sSendMailCmd
		ENDIF
	ENDIF
	*-------------------------------------------------------------------------

	xx=' con ' &&+ CHR(9)

	=showProgressMessage('Proceso Terminado')
	sPlainsExportedMsg = 'SE HAN GENERADO LOS SIGUIENTES ARCHIVOS DE TEXTO!'+CHR(13)+CHR(13)+;
						   IIF(!bIsFeedback,JUSTFNAME(CAR)+xx+TRANS(REG_U,'@l 99999999')+' - Registros de UPGD'+CHR(13),'')+;
						   IIF(!bIsFeedback,JUSTFNAME(TAL)+xx+TRANS(REG_T,'@l 99999999')+' - Registros de Talento H / Servicios'+CHR(13),'')+;
						   JUSTFNAME(PTE)+xx+TRANS(REG_P,'@l 99999999')+' - Registros de Not. Individual'+CHR(13)+;
						   JUSTFNAME(DAT)+xx+TRANS(REG_D,'@l 99999999')+' - Registros de Datos Complementarios'+CHR(13)+;
						   JUSTFNAME(BRO)+xx+TRANS(REG_B,'@l 99999999')+' - Registros de Not. Colectiva'+CHR(13)+;
						   JUSTFNAME(TOT)+xx+TRANS(REG_C,'@l 99999999')+' - Registros de Not. Colectiva Reporte'+CHR(13)+;
						   JUSTFNAME(IRA)+xx+TRANS(REG_I,'@l 99999999')+' - Registros de Morb. por IRA'+CHR(13)+;
						   IIF(!bIsFeedback,JUSTFNAME(UCIFileName)+xx+TRANS(REG_V,'@l 99999999')+' - Registros de UCI por UPGD','')+CHR(13)+;
						   JUSTFNAME(CON)+xx+TRANS(L,'@l 99999999')+' - Registros de Control'+CHR(13)+CHR(13)+;
						   'UBÍQUELOS EN ' + sWeeklyTargetPath + CHR(13) + CHR(13) +;
						   'POR FAVOR ENVIE LOS ARCHIVOS COMPRIMIDOS A SU ENTIDAD JERARQUICA SUPERIOR'
							* Comentado por: wilson aguilar	Fecha: 20/10/2011 ;
							El Grupo SIVIGILA del INS decidió que con la veersión 2012, el Sistema ya no debe generar el archivo W;
							por tanto la siguiente línea de código que hace parte del mensaje para el usuario, se suprimió
						   &&RIGHT(PTW,8)+xx+TRANS(REG_W,'@l 99999999')+' - Registros de Not. WEB'+CHR(13)+;

	IF !bBeSilent THEN
		=showErrorMessage(sPlainsExportedMsg,0)	
	ELSE
		=FPUTS(nResultsFileHandler,sPlainsExportedMsg)
	ENDIF

	EXPO=1

	IF !bClaimNotificationIsFinished THEN
		SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		IF !bBeSilent THEN
			=showErrorMessage(sResultMsg, 0)
		ELSE
			=FPUTS(nResultsFileHandler,sResultMsg)
		ENDIF
	ENDIF

	sEndTime=TIME()
	IF bBeSilent THEN
		=FPUTS(nResultsFileHandler,'Hora de inicio: ' + sInitTime)
		=FPUTS(nResultsFileHandler,'Hora de finalización: ' + sEndTime)			
	ENDIF

	DO cleanTheHouse

	SET PROCEDURE TO &sOldProcedure 	

	=hideProgressMessage()

ENDPROC 



*-------------------------------------------------
PROCEDURE processCollective(bBeSilent as Boolean, nResultsFileHandler as Number )
	*Carga el archivo plano B hacia la tabla temporal intermedia sTmpTableName 
	
	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_BROTES'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano N

	LOCAL nOrphansCollectives as Byte 
	nOrphansCollectives = 1
	
	LOCAL sErrorMsg as String 

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ADDBS(sXLSFilesPath)+"DatosColectivos", ;
		'Not. Colectiva' IN PlainsLib

	*Corrije los posibles registros duplicados que se encuentren en la tabla PLANO_BROTES con el fin de evitar ;
	la carga de duplicados
	IF n_version = _VERSION2010 THEN
		SET PROCEDURE TO AnomaliesHandler ADDITIVE
		sSourceTableName=_RTA_CONSOLIDA+'\'+sTmpTableName
		=solveDuplicates(sSourceTableName,sXLSFilesPath)
		USE (sSourceTableName)
	ENDIF
	
	*Dado que algunos campos son exportados luego de ser sometidos a transformaciones,;
	ejecuta las	transformaciones inversas necesarias para actualizar los datos recien cargados;
	de tal forma que se restablezcan sus valores originales
	IF n_version = _VERSION2008
		REPLACE MUN_PRO WITH DEP_PRO+MUN_PRO ALL
	ENDIF

	IF n_version = _VERSION2010
		REPLACE MUN_PRO WITH cod_dpto_p+ cod_mun_pr ALL
	ENDIF
*SET STEP ON 
	*Selecciona de la tabla de brotes solamente aquellos que tengan datos complementarios.
	orphansCollectivesCursorName=''
	noOrphansCollectivesCursorName=''
	SET PROCEDURE TO PlainsLib ADDITIVE
	IF !USED('PLANO_EVENTOS') THEN
		USE _RTA_CONSOLIDA + '\PLANO_EVENTOS' IN 0
	ENDIF
	nOrphansCollectives=extractOrphans(sTmpTableName,'PLANO_EVENTOS',@orphansCollectivesCursorName,@noOrphansCollectivesCursorName,n_version,2)
	
	IF nOrphansCollectives> 9 THEN
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_OUTBREAK_PLAIN_FILE_WITHOUT_CD , 0 + 16 + 256, 'Error de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_OUTBREAK_PLAIN_FILE_WITHOUT_CD )
		ENDIF
		
		orphansExcelFileName=sXLSFilesPath + '\' + sTmpTableName + '_SinComplementarios_o_Inexistentes'
		COPY TO [&orphansExcelFileName] XL5

		* Agrega los registros sin datos complementarios a la tabla de registro de ERRORES;
		(addDupsToErrorTable sirve para este propósito aún cuando su nombre no lo refleje)
		DO addDupsToErrorTable WITH orphansCollectivesCursorName, ARC_MAE, 'B', _RTA_CONSOLIDA,;
			 'ERRORES', 'UPGD DE DONDE PROVIENEN LOS REGISTROS DE BROTES SIN DATOS COMPLEMENTARIOS: ' IN PlainsLib

		markOrpahnsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
				+ ' FROM ' + orphansCollectivesCursorName+ ' WHERE ';
				+  sTmpTableName + '.ajuste  = ' + orphansCollectivesCursorName+ '.ajuste AND ';
				+  sTmpTableName + '.año     = ' + orphansCollectivesCursorName+ '.año AND ';
				+  sTmpTableName + '.semana  = ' + orphansCollectivesCursorName+ '.semana AND ';
				+  sTmpTableName + '.cod_eve = ' + orphansCollectivesCursorName+ '.cod_eve AND ';
				+  sTmpTableName + '.num_con = ' + orphansCollectivesCursorName+ '.num_con AND ';
				+  sTmpTableName + '.cod_mun = ' + orphansCollectivesCursorName+ '.cod_mun AND ';
				+  sTmpTableName + '.cod_pre = ' + orphansCollectivesCursorName+ '.cod_pre AND ';
				+  sTmpTableName + '.cod_sub = ' + orphansCollectivesCursorName+ '.cod_sub ';
		
		&markOrpahnsRecordsClause
	ENDIF


	 * SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
		LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
	collectivesDuplicatesCursorName=''	
	XtractResult = extractDupsAndNotDups(noOrphansCollectivesCursorName,@collectivesDuplicatesCursorName)

	IF XtractResult = 0		&& NO SE EXTRAJERON REGISTROS
		RETURN
	ENDIF

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_OUTBREAK_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_OUTBREAK_PLAIN_FILE_WITH_DUPLICATES)		
		ENDIF
	
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH collectivesDuplicatesCursorName, ARC_MAE, 'B', ;
									_RTA_CONSOLIDA, 'ERRORES' IN PlainsLib
		
		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
				+ ' FROM ' + collectivesDuplicatesCursorName + ' WHERE ';
				+  sTmpTableName + '.COD_EVE = ' + collectivesDuplicatesCursorName + '.COD_EVE AND ';
				+  sTmpTableName + '.AÑO     = ' + collectivesDuplicatesCursorName + '.AÑO     AND ';
				+  sTmpTableName + '.SEMANA  = ' + collectivesDuplicatesCursorName + '.SEMANA  AND ';
				+  sTmpTableName + '.COD_MUN = ' + collectivesDuplicatesCursorName + '.COD_MUN AND ';
				+  sTmpTableName + '.NUM_CON = ' + collectivesDuplicatesCursorName + '.NUM_CON AND ';
				+  sTmpTableName + '.COD_PRE = ' + collectivesDuplicatesCursorName + '.COD_PRE AND ';
				+  sTmpTableName + '.COD_SUB = ' + collectivesDuplicatesCursorName + '.COD_SUB'
		
		&markDupsRecordsClause
	ENDIF

ENDPROC


*-------------------------------------------------
PROCEDURE processTH(bBeSilent as Boolean, nResultsFileHandler as Number )
	*Carga el archivo plano N hacia la tabla temporal intermedia sTmpTableName 
	
	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_RHS'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano 
	
	LOCAL sErrorMsg as String 

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ,'Talento H/Servicios' IN PlainsLib

	*Elimina registros duplicados en la tabla PLANO_THS
	oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,sTmpTableName)
	sSIVIGILACurrentDir=SYS(5)+SYS(2003)
	SET DEFAULT TO (_RTA_CONSOLIDA )
	oAnomaliesSolver.sExcludedFields = " NUM_REG SUBE "
	oAnomaliesSolver.sXlsFilePath = sXLSFilesPath
	oAnomaliesSolver.deleteDuplicates()
	RELEASE oAnomaliesSolver
	SET DEFAULT TO (sSIVIGILACurrentDir)
	
	SELECT (sTmpTableName)
	R_IMP=RECCOUNT()
	tot_th=RECCOUNT()

	DELETE ALL FOR EMPTY(cod_pre)
	 
	* SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
	LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
	humanResourcesDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(sTmpTableName,@humanResourcesDuplicatesCursorName)

	IF XtractResult = 0		&& NO SE EXTRAJERON REGISTROS
		RETURN
	ENDIF

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_TH_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_TH_PLAIN_FILE_WITH_DUPLICATES)
		ENDIF
	
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH humanResourcesDuplicatesCursorName, ARC_MAE, 'R', ;
			_RTA_CONSOLIDA, 'ERRORES' IN PlainsLib
		
		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
			+ ' FROM ' + humanResourcesDuplicatesCursorName + ' WHERE ';
			+  sTmpTableName + '.cod_pre  = ' + humanResourcesDuplicatesCursorName + '.cod_pre  AND ';
			+  sTmpTableName + '.cod_sub = ' + humanResourcesDuplicatesCursorName + '.cod_sub AND ';
			+  sTmpTableName + '.cod_art = ' + humanResourcesDuplicatesCursorName + '.cod_art '
		
		&markDupsRecordsClause
	ENDIF

ENDPROC

*-------------------------------------------------
PROCEDURE processUCI(bBeSilent as Boolean, nResultsFileHandler as Number )
	*Carga el archivo plano V hacia la tabla temporal intermedia sTmpTableName 
	
	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_UCI'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano 
	
	LOCAL sErrorMsg as String 

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ,'UCI en UPGD' IN PlainsLib

	*Elimina registros duplicados en la tabla PLANO_UCI
	oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,sTmpTableName)
	sSIVIGILACurrentDir=SYS(5)+SYS(2003)
	SET DEFAULT TO (_RTA_CONSOLIDA )
	oAnomaliesSolver.sExcludedFields = " NUM_REG SUBE "
	oAnomaliesSolver.sXlsFilePath = sXLSFilesPath
	oAnomaliesSolver.deleteDuplicates()
	RELEASE oAnomaliesSolver
	SET DEFAULT TO (sSIVIGILACurrentDir)
	
	SELECT (sTmpTableName)
	R_IMP=RECCOUNT()
	total_uci_cargadas=R_IMP

	DELETE ALL FOR EMPTY(cod_pre)
	 
	* SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
	LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
	UCIDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(sTmpTableName,@UCIDuplicatesCursorName)

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_UCI_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_UCI_PLAIN_FILE_WITH_DUPLICATES)
		ENDIF
	
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH UCIDuplicatesCursorName, ARC_MAE, 'V', _RTA_CONSOLIDA, 'ERRORES' IN PlainsLib
		
		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
			+ ' FROM ' + UCIDuplicatesCursorName + ' WHERE ';
			+  sTmpTableName + '.ID_UCI  = ' + UCIDuplicatesCursorName + '.ID_UCI'
		
		&markDupsRecordsClause
	ENDIF

ENDPROC

*-----------------------------------------------------------------------------------------------------
PROCEDURE processUPGD(bBeSilent as Boolean, nResultsFileHandler as Number )
	*Carga el archivo plano U hacia la tabla temporal intermedia sTmpTableName 
	
	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_UPGD'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano 
	
	LOCAL sErrorMsg as String 
		
	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ,'UPGDs' IN PlainsLib
	tot_upgd=RECCOUNT()

	R_IMP=RECCOUNT()
	DELETE ALL FOR EMPTY(FEC_CAR)
	IF _TALLY>0
		sErrorMsg = 'SE HAN DESCARTADO ' + NC(_TALLY) + ' UPGD POR DATOS INCOMPLETOS'
		IF !bBeSilent THEN	
			=showErrorMessage(sErrorMsg,0)
		ELSE
			=FPUTS(nResultsFileHandler,sErrorMsg)
		ENDIF
	ENDIF

	*Suprime de la tabla PLANO_UPGD los registros que aparecen como activos y que no tengan registros relacionados ;
	en ninguno de los archivos N, D, B o I
	sSelectSQLCmd="SELECT COD_PRE + COD_SUB as codigoUPGD FROM '" +_RTA_CONSOLIDA + "\PLANO_PACIENTES'" +;
					" UNION SELECT COD_PRE + COD_SUB codigoUPGD FROM '" + _RTA_CONSOLIDA + "\PLANO_BROTES'" 
	IF FILE(_RTA_CONSOLIDA + '\PLANO_E43.DBF') THEN
		sSelectSQLCmd = sSelectSQLCmd + " UNION SELECT COD_PRE + COD_SUB codigoUPGD FROM '" + _RTA_CONSOLIDA + "\PLANO_E43'"
	ENDIF
	sSelectSQLCmd = sSelectSQLCmd + " UNION SELECT COD_PRE + COD_SUB codigoUPGD FROM '" + _RTA_CONSOLIDA + "\PLANO_EVENTOS' INTO TABLE rsActiveUPGDs"
	&sSelectSQLCmd
	DELETE FROM _RTA_CONSOLIDA + '\PLANO_UPGD' WHERE ACT_SIV=1 AND (COD_PRE + COD_SUB NOT IN (SELECT codigoUPGD FROM rsActiveUPGDs))
	IF _TALLY>0
		sErrorMsg = 'SE HAN DESCARTADO ' + NC(_TALLY) + ' UPGD POR CUANTO FIGURAN COMO ACTIVAS EN EL ARCHIVO PLANO PERO NO TIENEN REGISTROS EN NINGUNO DE LOS ARCHIVOS N, D, B o I'
		IF !bBeSilent THEN
			=showErrorMessage(sErrorMsg,0)
		ELSE
			=FPUTS(nResultsFileHandler,sErrorMsg)
		ENDIF
	ENDIF
	
	*Activa todas las UPGDs del archivo PLANO_UPGD que están reportando datos en uno de los ;
	archivos N, D, B o I
	UPDATE _RTA_CONSOLIDA + '\PLANO_UPGD' SET ACT_SIV = 1 WHERE COD_PRE + COD_SUB IN (SELECT codigoUPGD FROM rsActiveUPGDs)

	*Corrije los posibles registros duplicados que se encuentren en la tabla PLANO_UPGD con el fin de evitar ;
	la carga de duplicados
	SET PROCEDURE TO AnomaliesHandler ADDITIVE
	=solveDuplicates(sTmpTableName)
	sSourceTableName=_RTA_CONSOLIDA+'\'+sTmpTableName
	USE (sSourceTableName)
	
	* SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
		LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
	UPGDsDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(sTmpTableName,@UPGDsDuplicatesCursorName)

	IF XtractResult = 0		&& NO SE EXTRAJERON REGISTROS
		RETURN
	ENDIF

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_UPGD_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_UPGD_PLAIN_FILE_WITH_DUPLICATES)
		ENDIF
	
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH UPGDsDuplicatesCursorName, ARC_MAE, 'U', _RTA_CONSOLIDA,;
			 'ERRORES' IN PlainsLib

		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
				+ ' FROM ' + UPGDsDuplicatesCursorName+ ' WHERE ';
				+  sTmpTableName + '.cod_pre  = ' + UPGDsDuplicatesCursorName+ '.cod_pre AND ';
				+  sTmpTableName + '.cod_sub = ' + UPGDsDuplicatesCursorName+ '.cod_sub '
		
		&markDupsRecordsClause
	ENDIF
 
ENDPROC


*-----------------------------------------------------------------------------------------------------
PROCEDURE processIndividual(bBeSilent as Boolean, nResultsFileHandler as Number )

	*Carga el archivo plano N hacia la tabla temporal intermedia sTmpTableName 
	
	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_PACIENTES'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano N
	
	LOCAL sErrorMsg as String 

	LOCAL nOrphansPatients as Byte 
	nOrphansPatients = 1
	
	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ADDBS(sXLSFilesPath)+"DatosBasicosIndividuales", ;
							'Not. Individual' IN PlainsLib

	**JLGB referencia requerimiento REQ204
	*DO TransferMigrateAllFields IN TransferDatafields.prg
	DO TransformOnPlainsLoad IN TransferDataHandler WITH 2, (_RTA_CONSOLIDA)

	*Corrije los posibles registros duplicados que se encuentren en la tabla PLANO_PACIENTES con el fin de evitar ;
	la carga de duplicados
	IF n_version = _VERSION2010 THEN
		SET PROCEDURE TO AnomaliesHandler ADDITIVE
		sSourceTableName=_RTA_CONSOLIDA+'\'+sTmpTableName
		=solveDuplicates(sSourceTableName,sXLSFilesPath)
		USE (sSourceTableName)
	ENDIF
	tot_pte=RECCOUNT()


	*Dado que algunos campos son exportados luego de ser sometidos a transformaciones,;
	ejecuta las	transformaciones inversas necesarias para actualizar los datos recien cargados;
	de tal forma que se restablezcan sus valores originales
	IF n_version = _VERSION2008
		REPLACE COD_MUN WITH COD_DEP+COD_MUN, MUN_PRO WITH DEP_PRO+MUN_PRO ALL
	ENDIF

	IF n_version = _VERSION2010
		REPLACE COD_MUN WITH cod_dpto_o + cod_mun_o, ;
				MUN_PRO WITH cod_dpto_r + cod_mun_r ALL

		UPDATE &sTmpTableName SET PAIS=PAISES.pais FROM PAISES WHERE &sTmpTableName..cod_pais_o=PAISES.CodNumPais
	ENDIF
*SET STEP ON 
	*Selecciona de la tabla de pacientes solamente aquellos que tengan datos complementarios.
	orphansPatientsCursorName=''
	noOrphansPatientsCursorName=''
	SET PROCEDURE TO PlainsLib ADDITIVE
	IF !USED('PLANO_EVENTOS') THEN
		USE _RTA_CONSOLIDA + '\PLANO_EVENTOS' IN 0
	ENDIF
	nOrphansPatients=extractOrphans(sTmpTableName,'PLANO_EVENTOS',@orphansPatientsCursorName,@noOrphansPatientsCursorName,n_version, 1)
	
	IF nOrphansPatients > 9 THEN
		IF !bBeSilent THEN		
			MESSAGEBOX(WARNING_INDIVIDUAL_PLAIN_FILE_WITHOUT_CD, 0 + 16 + 256, 'Error de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_INDIVIDUAL_PLAIN_FILE_WITHOUT_CD)
		ENDIF
		
		orphansExcelFileName=sXLSFilesPath + '\' + sTmpTableName + '_SinComplementarios_o_Inexistentes'
		COPY TO [&orphansExcelFileName] XL5

		* Agrega los registros sin datos complementarios a la tabla de registro de ERRORES;
		(addDupsToErrorTable sirve para este propósito aún cuando su nombre no lo refleje)
		DO addDupsToErrorTable WITH orphansPatientsCursorName, ARC_MAE, 'N', _RTA_CONSOLIDA,;
			 'ERRORES', 'UPGD DE DONDE PROVIENEN LOS REGISTROS SIN DATOS COMPLEMENTARIOS: ' IN PlainsLib

		markOrpahnsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
				+ ' FROM ' + orphansPatientsCursorName + ' WHERE ';
				+  sTmpTableName + '.ajuste  = ' + orphansPatientsCursorName+ '.ajuste AND ';
				+  sTmpTableName + '.año     = ' + orphansPatientsCursorName+ '.año AND ';
				+  sTmpTableName + '.semana  = ' + orphansPatientsCursorName+ '.semana AND ';
				+  sTmpTableName + '.cod_eve = ' + orphansPatientsCursorName+ '.cod_eve AND ';
				+  sTmpTableName + '.num_ide = ' + orphansPatientsCursorName+ '.num_ide AND ';
				+  sTmpTableName + '.tip_ide = ' + orphansPatientsCursorName+ '.tip_ide AND ';
				+  sTmpTableName + '.cod_pre = ' + orphansPatientsCursorName+ '.cod_pre AND ';
				+  sTmpTableName + '.cod_sub = ' + orphansPatientsCursorName+ '.cod_sub ';
		
		&markOrpahnsRecordsClause
	ENDIF

	* SE CREAN UN CURSOR Y UNA TABLA, UNO CON LOS REGISTROS DUPLICADOS Y EL OTRO CON	LOS REGISTROS NO DUPLICADOS PARA SUBIR ... ;
		C_DUPS, T_NONDUPS_[Abreviatura de la tabla de la que proviene] 
	patientDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(noOrphansPatientsCursorName,@patientDuplicatesCursorName)
	DO addIndexToTable WITH 'T_NONDUPS_PACIENTES',;
							"AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB+AJUSTE+TRANSFORM(INGR_X_NI,'Y')",;
							"LINKER2_IX" IN SivigilaIndexesHandler
			
	IF XtractResult = 0		&& NO SE EXTRAJERON REGISTROS
		RETURN
	ENDIF

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN
			MESSAGEBOX(WARNING_INDIVIDUAL_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_INDIVIDUAL_PLAIN_FILE_WITH_DUPLICATES)
		ENDIF
 	
		DO addDupsToErrorTable WITH patientDuplicatesCursorName, ARC_MAE, 'N', _RTA_CONSOLIDA,;
			 'ERRORES' IN PlainsLib

		SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
		=UpdateFieldsTable("PLANO_PACIENTES", "PLANO_PACIENTES.SUBE=1", patientDuplicatesCursorName, ;
							"AJUSTE, AÑO, SEMANA, COD_EVE, TIP_IDE, NUM_IDE, COD_PRE, COD_SUB")
	ENDIF

ENDPROC
*-----------------------------------------------------------------------------------------------------
PROCEDURE processIRA(bBeSilent as Boolean, nResultsFileHandler as Number )
	* Carga el archivo plano I hacia la tabla temporal intermedia sTmpTableName 

	LOCAL sTmpTableName as String
	sTmpTableName = 'PLANO_E43'
	*Nombre de la tabla temporal intermedia en donde se carga el archivo plano I

	LOCAL sErrorMsg as String 

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, , ADDBS(sXLSFilesPath)+"DatosIrag", ;
							'Morb.por IRAG' IN PlainsLib
	tot_dat = tot_dat + _TALLY	

    * SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
		LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
	iraDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(sTmpTableName,@iraDuplicatesCursorName)

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		IF !bBeSilent THEN		
			MESSAGEBOX( WARNING_IRA_PLAIN_FILE_WITH_DUPLICATES, 0 + 16 + 256, 'Aviso de importación SIVIGILA')
		ELSE
			=FPUTS(nResultsFileHandler,WARNING_IRA_PLAIN_FILE_WITH_DUPLICATES)
		ENDIF
	
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH iraDuplicatesCursorName, ARC_MAE, 'D', _RTA_CONSOLIDA,;
			 'ERRORES' IN PlainsLib

		* Se marcan en la tala origen los registros que se subieron a errores en el paso anterior
		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
						+ ' FROM ' + IRADuplicatesCursorName + ' WHERE ';
						+  sTmpTableName  + '.año     = ' + iraDuplicatesCursorName + '.año AND ';
						+  sTmpTableName  + '.semana  = ' + iraDuplicatesCursorName + '.semana AND ';
						+  sTmpTableName  + '.cod_eve = ' + iraDuplicatesCursorName + '.cod_eve AND ';
						+  sTmpTableName  + '.cod_mun = ' + iraDuplicatesCursorName + '.cod_mun AND ';
						+  sTmpTableName  + '.num_con = ' + iraDuplicatesCursorName + '.num_con '
				
		&markDupsRecordsClause
	ENDIF
ENDPROC

*-----------------------------------------------------------------------------------------------------
PROCEDURE processComplementary(SivigilaVersion, bBeSilent as Boolean, nResultsFileHandler as Number )

	*Carga los datos complementarios de todos los eventos, datos de laboratorios, contactos y seguimientos
	
	LOCAL sErrorMsg as String 
	
	LOCAL sDuplicatesKey as String 
	sDuplicatesKey = ''
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\QueriesHandler') ADDITIVE

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+'PLANO_EVENTOS', , ,'Datos complementarios' IN PlainsLib

	*De acuerdo con la version del Sistema SIVIGILA, determina qué tabla de definición de eventos debe;
	utilizarse para	cargar la información
	IF SivigilaVersion=_VERSION2008 THEN
		eventsDefinitonTableName='EVENTOS2008'
	ELSE
		eventsDefinitonTableName='EVENTOS'
	ENDIF

	SELECT DISTINCT COD_EVE FROM PLANO_EVENTOS WHERE CONTROL NOT LIKE 'CONTACTOS%' AND CONTROL NOT LIKE 'SEGUIMIENTO%' INTO CURSOR C_EVENTS
	SELECT C_EVENTS
	SCAN
		_EVENTO = C_EVENTS.COD_EVE

		*Establece, según el código de evento, en qué tablas maestras (indicadas por el número de formulario);
		debe almacenarse la información
		SELECT FORMULARIO, NOM_EVE FROM &eventsDefinitonTableName WHERE ;
			ALLTRIM(&eventsDefinitonTableName..COD_EVE) == ALLTRIM(_EVENTO) ;
			AND !EMPTY(&eventsDefinitonTableName..FORMULARIO) INTO ARRAY A_EVENTO

		IF _TALLY > 0
			sPlainTableName = 'PLANO_E'+PADL(ALLTRIM(A_EVENTO),2,'0')	
			WAIT WINDOW NOWAIT "EVENTO ... " + A_Evento(2)

			IF !FILE(_RTA_CONSOLIDA + '\' + sPlainTableName + '.DBF')
				sErrorMsg = 'No existe ' + sPlainTableName + ' en ' + _RTA_CONSOLIDA 
				IF !bBeSilent THEN 
					MESSAGEBOX(sErrorMsg)
				ELSE
					=FPUTS(nResultsFileHandler,sErrorMsg)
				ENDIF
				LOOP
			ENDIF
			
			IF !USED(sPlainTableName )
				SELECT 0
				USE [&_RTA_CONSOLIDA\&sPlainTableName] EXCLUSIVE
			ENDIF
			
			_condition = ' FOR COD_EVE = _EVENTO '

			* SE BUSCA LA CONDICION EN EL ARRAY PARA QUE FILTRE LABORATORIOS
			_condition = _condition + ' AND ' + initEventConditions(sPlainTableName, _RTA_CONSOLIDA)
			
			SELECT &sPlainTableName
			
			*Comentado por Wilson Aguilar 11/12/2009;
			La siguiente instrucción hace que no se carguen los registros de todos los eventos que;
			tienen como destino una misma tabla, por tanto, se suprimió
			&&ZAP
			APPEND FROM (NOM_ARC) DELIMITED &_condition
			
			*Dado que para algunos eventos algunos campos son exportados luego de ser sometidos a transformaciones,;
			ejecuta las	transformaciones inversas necesarias para actualizar los datos recien cargados;
			de tal forma que se restablezcan sus valores originales
			IF SivigilaVersion= _VERSION2010 THEN
				IF sPlainTableName = 'PLANO_E29' THEN
					REPLACE COD_MUN WITH cod_dpto_n + cod_mun_na ALL
				ENDIF
			ENDIF
			
			IF SivigilaVersion= _VERSION2008 THEN
				IF sPlainTableName = 'PLANO_E29' THEN
					REPLACE COD_MUN WITH DEP + MUN ALL
				ENDIF
			ENDIF
			tot_dat = tot_dat + _TALLY

			 * SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
				LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS 
			eventDuplicatesCursorName = ''
			XtractResult = extractDupsAndNotDups(sPlainTableName,@eventDuplicatesCursorName,,,@sDuplicatesKey )

			* Si XtractResult = 10 u 11, hay duplicados para registrar
			IF XtractResult > 9 
				IF !bBeSilent THEN
					MESSAGEBOX( WARNING_COMPLEMENTARY_PLAIN_FILE_WITH_DUPLICATES_PART1 + _EVENTO + ;
								WARNING_COMPLEMENTARY_PLAIN_FILE_WITH_DUPLICATES_PART2 , 0 + 16 + 256, 'Aviso de importación SIVIGILA')
				ELSE
					=FPUTS(nResultsFileHandler, WARNING_COMPLEMENTARY_PLAIN_FILE_WITH_DUPLICATES_PART1 + _EVENTO + ;
							WARNING_COMPLEMENTARY_PLAIN_FILE_WITH_DUPLICATES_PART2)	
				ENDIF
				
				* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
				DO addDupsToErrorTable WITH eventDuplicatesCursorName, ARC_MAE, 'D', _RTA_CONSOLIDA, 'ERRORES' IN PlainsLib

				markDupsRecordsClause = 'UPDATE ' + sPlainTableName + ' SET ' + sPlainTableName + '.sube = 1 ' ;
												+ ' FROM ' + eventDuplicatesCursorName + ' WHERE ' +;
												matchFields(sDuplicatesKey, sPlainTableName, sDuplicatesKey, eventDuplicatesCursorName, ",")
				&markDupsRecordsClause
			ENDIF
		ENDIF
		SELECT C_EVENTS
	ENDSCAN

	* Una vez finalizado el proceso de encontrar los duplicados y el correspondiente marcado, se	;
	continúa con los demás registros que vienen en este archivo: 
	
	*Se cargan los resultados de laboratorio contenidos en el archivo plano D
	sTmpTableName = 'PLANO_LAB'

	DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, 'LIKE("LABORATORIO*",CONTROL)', ,'Laboratorios' IN PlainsLib
	REPLACE NUM_REG WITH RECNO() ALL
	
	*Elimina registros duplicados en la tabla PLANO_LAB
	oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,sTmpTableName)
	sSIVIGILACurrentDir=SYS(5)+SYS(2003)
	SET DEFAULT TO (_RTA_CONSOLIDA )
	oAnomaliesSolver.sExcludedFields = " NUM_REG SUBE "
	oAnomaliesSolver.sXlsFilePath = sXLSFilesPath
	oAnomaliesSolver.deleteDuplicates()
	RELEASE oAnomaliesSolver
	SET DEFAULT TO (sSIVIGILACurrentDir)
	
	SELECT (sTmpTableName)
	tot_lab=RECCOUNT()

	* SE CREAN DOS CURSORES, UNO CON LOS REGISTROS DUPLICADOS Y OTRO CON	;
		LOS REGISTROS NO DUPLICADOS PARA SUBIR ... C_DUPS T_NONDUPS_* 
	labsDuplicatesCursorName=''
	XtractResult = extractDupsAndNotDups(sTmpTableName,@labsDuplicatesCursorName)

	* Si XtractResult = 10 u 11, hay duplicados para registrar
	IF XtractResult > 9 
		* PARAMETERS RSName, sourceFileName, sourceFileType, errorsFilePath, errorsFileName
		DO addDupsToErrorTable WITH labsDuplicatesCursorName, ARC_MAE, 'D', _RTA_CONSOLIDA,;
			 'ERRORES' IN PlainsLib
		
		markDupsRecordsClause='UPDATE ' + sTmpTableName + ' SET ' + sTmpTableName + '.sube = 1 ' ;
			+ ' FROM ' + labsDuplicatesCursorName + ' WHERE ';
			+  sTmpTableName  + '.año = ' + labsDuplicatesCursorName + '.año AND ';
			+  sTmpTableName  + '.semana = ' + labsDuplicatesCursorName + '.semana AND ';
			+  sTmpTableName  + '.cod_eve = ' + labsDuplicatesCursorName + '.cod_eve AND ';
			+  sTmpTableName  + '.tip_ide = ' + labsDuplicatesCursorName + '.tip_ide AND ';
			+  sTmpTableName  + '.num_ide = ' + labsDuplicatesCursorName + '.num_ide AND ';
			+  sTmpTableName  + '.AGENTE  = ' + labsDuplicatesCursorName + '.AGENTE  AND ';
			+  sTmpTableName  + '.PRUEBA  = ' + labsDuplicatesCursorName + '.PRUEBA  '
					
		&markDupsRecordsClause
	ENDIF

	*Se cargan los registros de contactos contenidos en el archivo plano D
	sTmpTableName = 'PLANO_CONTACTOS'

	IF FILE(_RTA_CONSOLIDA+'\' + sTmpTableName + '.DBF') THEN
		DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, 'LIKE("CONTACTOS",CONTROL)', ,'Contactos' IN PlainsLib
		REPLACE NUM_REG WITH RECNO() ALL
		TOT_CONTACTOS = RECCOUNT(sTmpTableName)
	ELSE
		TOT_CONTACTOS = 0
	ENDIF 

	*Se cargan los registros de seguimiento a contactos contenidos en el archivo plano D
	sTmpTableName = 'PLANO_SEGUIMIENTOS'

	IF FILE(_RTA_CONSOLIDA+'\'+sTmpTableName + '.DBF') THEN
		DO loadPlainFile WITH NOM_ARC, _RTA_CONSOLIDA+'\'+sTmpTableName, 'LIKE("SEGUIMIENTO*",CONTROL)', ,'SeguimientoContactos' IN PlainsLib
		REPLACE NUM_REG WITH RECNO() ALL
		TOT_SEGUIMIENTOS = RECCOUNT(sTmpTableName)
	ELSE
		TOT_SEGUIMIENTOS = 0
	ENDIF 
ENDPROC



*-----------------------------------------------------------------------------------------------------------------------
PROCEDURE cleanTheHouse
	DELETE FILE 'T_Non*.DBF'
	DELETE FILE 'C_DUPS_*.DBF'
	DELETE FILE 'T_ReportingUPGDs.DBF'
ENDPROC

*-----------------------------------------------------------------------------------------------------
FUNCTION extractDupsAndNotDups

	LPARAMETERS PlainFileName, duplicatesRSName, aPlainTableKeys, sKeyIDToUse, sUsedKey as string

	*Determina si existen registros duplicados en PlainFileName teniendo en cuenta la clave que para tales efectos;
	se encuentre definida en MASTER_INDEX.EXTRAS o de acuerdo con la indicada por sKeyIDToUse, ;
	si es que se recibe este parámetro opcional. En caso de que no exista un registro que defina la clave para ;
	detectar duplicados en la tabla MASTER_INDEX se usa la definida para MASTER_INDEX.Table_name='PLANO_EXX'
	
	*Retorna:	(1) Todos los registros son únicos	;
				(10) Todos los registros son duplicados	;
				(11) Existen registros duplicados y NO duplicados

	*Como efecto colateral, crea ;
	1. Una tabla que contiene los registros NO duplicados en  PlainFileName. ;
	El nombre de esta tabla será T_NONDUPS_ + ALLTRIM(SUBSTR(PlainFileName,RAT('_',PlainFileName)+1));
	;
	2. Un cursor que contiene el conteo de los registros duplicados en PlainFileName. ;
	El nombre de esta curosr será C_COUNT_DUPS ;
	;
	3. Una tabla que contendrá los registros duplicados en PlainFileName. ;
	El nombre de esta tabla será C_DUPS_ + ALLTRIM(SUBSTR(PlainFileName,RAT('_',PlainFileName)+1)) y ;
	quedará almacenado en duplicatesRSName;
	;
	El parámetro opcional sKeyIDToUse permite que se puedan detectar registros duplicados en PlainFileName ;
	sin utilizar la clave por defecto establecida en la tabla MASTER_INDEX.EXTRAS; así, por ejemplo, se pueden;
	detectar duplicados en la tabla UPGD utilizando la clave de detección de duplicados establecida ;
	para PLANO_UPGD; en este caso se debería hacer la llamada;
	=extractDupsAndNotDups('UPGD',...,'PLANO_UPGD')
	
	*El parámetro aPlainTableKeys no se utiliza.

	_ReturnedValue = 0

	IF VARTYPE(sKeyIDToUse)='C' AND !EMPTY(sKeyIDToUse) THEN 
		SELECT TAG_EXPR FROM MASTER_INDEX WHERE EXTRAS='DUPLICATES' AND TABLE_NAME='&sKeyIDToUse' INTO CURSOR rsKeysForDups
	ELSE 
		SELECT TAG_EXPR FROM MASTER_INDEX WHERE EXTRAS='DUPLICATES' AND TABLE_NAME='&PlainFileName' INTO CURSOR rsKeysForDups
		IF _TALLY = 0 THEN
			SELECT TAG_EXPR FROM MASTER_INDEX WHERE EXTRAS='DUPLICATES' AND TABLE_NAME='PLANO_EXX' INTO CURSOR rsKeysForDups
		ENDIF
	ENDIF 
	IF _TALLY > 0 THEN 
	
		KeyWithCommas = ALLTRIM(STRTRAN(rsKeysForDups.TAG_EXPR,'+',','))
		IF VARTYPE(sUsedKey) = 'C' THEN 
			sUsedKey = KeyWithCommas
		ENDIF 
		SqlClause = 'SELECT ' + KeyWithCommas + ', COUNT(*) AS counter ' + ;
					'FROM ' + '"' + PlainFileName + '"' + ' GROUP BY ' + KeyWithCommas + ;
					' HAVING COUNT(*) > 1 INTO CURS C_COUNT_DUPS NOFILTER' 
		&SqlClause

		IF _TALLY > 0
			_ReturnedValue = _ReturnedValue + 10
		ENDIF


		bNRegIsPresent = isField(JUSTSTEM(PlainFileName),'nReg')
		DO crossTables WITH PlainFileName, 'C_COUNT_DUPS', KeyWithCommas, 'LEFT OUTER', 'C_COUNT_DUPS.' + LEFT(KeyWithCommas,AT(',',KeyWithCommas)-1) + ' IS NULL', ;
							!bNRegIsPresent, SqlClause IN (PATH_TO_COMMON_LIB + 'QueriesHandler')
		SqlClause = SqlClause + ' INTO TABLE T_NONDUPS_' + ALLTRIM(SUBSTR(PlainFileName,RAT('_',PlainFileName)+1))
		&SqlClause

		IF _TALLY > 0
			_ReturnedValue = _ReturnedValue + 1
		ENDIF

		duplicatesRSName = 'C_DUPS_' + ALLTRIM(SUBSTR(PlainFileName,RAT('_',PlainFileName)+1))
		DO crossTables WITH PlainFileName, 'C_COUNT_DUPS', KeyWithCommas, 'INNER', , !bNRegIsPresent, SqlClause IN (PATH_TO_COMMON_LIB + 'QueriesHandler')
		SqlClause = SqlClause + ' INTO TABLE ' + duplicatesRSName
		&SqlClause

		SELECT (duplicatesRSName)
	ENDIF 
	USE IN 'rsKeysForDups'
			
	RETURN _ReturnedValue 
		  
ENDFUNC

PROCEDURE LoadAndValidatePlains

	LPARAMETERS bBeSilent as Boolean, nResultsFileHandler as Number, nSivigilaEdition as Number, nMajorID as Number, nMinorID as Number, nBuildID as Number, ;
				sTargetPath as string, sPlainFilesPath as string
	
	* Crea en la carpeta sTargetPath las tablas intermedias de importacion de archivos planos de notificación de acuerdo con la ;
	edición nSivigilaEdition  y version del sistema con la que fueron generados (dada por nMajorID.nMinorID.nBuildID). Las tablas;
	quedarán cargadas con los registros provenientes de archivos planos ubicados en sPlainFilesPath.
	
	*nSivigilaEdition: es igual a _VERSION2008 o _VERSION2010 según se encuentra en Sivigila.h
	
	LOCAL sPlainFileNameAndPath as String 
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE

	DO makeImportTables WITH sTargetPath, nSivigilaEdition, nMajorID, nMinorID, nBuildID 

	CLOSE TABLES ALL

	*Establece el orden en que deben procesarse los archivos planos según se indica:	;
		Nombre				Orden;
		D*.txt, I*.txt,		1	;
		N*.txt				2	;
		B*.txt				3	;
		U*.txt				4	;
		R*.txt				5	;
		*.*					6
	SELECT	*,IIF(LIKE('D*',ARCHIVO) OR LIKE('I*',ARCHIVO),1, ;
			IIF(LIKE('N*',ARCHIVO),2,IIF(LIKE('B*',ARCHIVO),3,IIF(LIKE('U*',ARCHIVO),4,IIF(LIKE('R*',ARCHIVO),5,6))))) as nPriority ;
			FROM sTargetPath + '\PLANO_CONTROL' ORDER BY nPriority INTO CURSOR rsPLANO_CONTROL
	SELECT rsPLANO_CONTROL

	SET DATE TO YMD
	SET MARK TO '-'

	SCAN
		SCATTER MEMVAR
		NREG=RECNO()
		sPlainFileNameAndPath = ADDBS(ALLTRIM(sPlainFilesPath)) + rsPLANO_CONTROL.ARCHIVO
		R_IMP=0
		IF !FILE(sPlainFileNameAndPath)
	      	INSERT INTO "&sTargetPath\ERRORES" (ARCHIVO, REG, DES_ARC, COD_PRE, COD_SUB, VARIABLE);
	        	VALUES (m.ARCHIVO, NREG, 'Control', m.COD_PRE, m.COD_SUB, 'No se encuentra el archivo '+m.ARCHIVO)
		ELSE
			DO CASE
				CASE LEFT(ARCHIVO,1)='U'
					DO processUPGD WITH bBeSilent, nResultsFileHandler

				CASE LEFT(ARCHIVO,1)='R'
					DO processTH WITH bBeSilent, nResultsFileHandler

				CASE LEFT(ARCHIVO,1)='V'
					DO processUCI WITH bBeSilent, nResultsFileHandler

				CASE LEFT(ARCHIVO,1)='N'
					DO processIndividual WITH bBeSilent, nResultsFileHandler

			  	CASE LEFT(ARCHIVO,1)='B'
				  	DO processCollective WITH bBeSilent, nResultsFileHandler

		  		CASE LEFT(ARCHIVO,1)='D'
				  	DO processComplementary WITH nSivigilaEdition, bBeSilent, nResultsFileHandler 
			  	
		  		CASE LEFT(ARCHIVO,1)='I'
					* Archivo contenador de datos colectivo Morbilidad por IRA (995, 43)
					* a partir de la versión 2010
					DO processIRA WITH bBeSilent, nResultsFileHandler
			  		
			ENDCASE
		ENDIF
		SELECT rsPLANO_CONTROL
	ENDSCAN
	
	*Crea una tabla con las UPGDs que tienen datos en alguno de los archivos planos N, D, B o I;
	de tal forma que esas UPGDs se activen automáticamente en el sistema. Este tratamiento es necesario;
	por cuanto existen situaciones en donde el archivo U no es procesado por processUPGD por no venir;
	relacionado en el archivo de control C
	bNFileIsPresent=.T.
	sSourcePlainFile=sTargetPath + "\PLANO_PACIENTES.DBF"
	sSourcePlainTable="PLANO_PACIENTES"
	IF !USED(sSourcePlainTable) THEN
		IF FILE(sSourcePlainFile) THEN
			USE (sSourcePlainFile) IN 0
		ELSE
			bNFileIsPresent=.F.
		ENDIF
	ENDIF
	
	bDFileIsPresent=.T.
	sSourcePlainFile=sTargetPath + "\PLANO_EVENTOS.DBF"
	sSourcePlainTable="PLANO_EVENTOS"
	IF !USED(sSourcePlainTable) THEN
		IF FILE(sSourcePlainFile) THEN
			USE (sSourcePlainFile) IN 0
		ELSE
			bDFileIsPresent=.F.
		ENDIF
	ENDIF

	bIFileIsPresent=.T.
	sSourcePlainFile=sTargetPath + "\PLANO_E43.DBF"
	sSourcePlainTable="PLANO_E43"
	IF !USED(sSourcePlainTable) THEN
		IF FILE(sSourcePlainFile) THEN
			USE (sSourcePlainFile) IN 0
		ELSE
			bIFileIsPresent=.F.
		ENDIF
	ENDIF

	bBFileIsPresent=.T.
	sSourcePlainFile=sTargetPath + "\PLANO_BROTES.DBF"
	sSourcePlainTable="PLANO_BROTES"
	IF !USED(sSourcePlainTable) THEN
		IF FILE(sSourcePlainFile) THEN
			USE (sSourcePlainFile) IN 0
		ELSE
			bBFileIsPresent=.F.
		ENDIF
	ENDIF

	sSQLClause=''
	IF bNFileIsPresent THEN
		sSQLClause = "SELECT DISTINCT COD_PRE,COD_SUB FROM PLANO_PACIENTES "
	ENDIF
	
	IF bDFileIsPresent THEN
		sSQLClause = sSQLClause + " UNION SELECT DISTINCT COD_PRE,COD_SUB FROM PLANO_EVENTOS "
	ENDIF
	
	IF bIFileIsPresent THEN
		sSQLClause = sSQLClause + " UNION SELECT DISTINCT COD_PRE,COD_SUB FROM PLANO_E43 "
	ENDIF

	IF bBFileIsPresent THEN
		sSQLClause = sSQLClause + " UNION SELECT DISTINCT COD_PRE,COD_SUB FROM PLANO_BROTES "
	ENDIF
	
	IF !EMPTY(sSQLClause) THEN
		sSQLClause = sSQLClause + "	INTO TABLE T_ReportingUPGDs"
	ELSE
		sSQLClause = "SELECT DISTINCT COD_PRE,COD_SUB FROM PLANO_CONTROL WHERE .F. INTO TABLE T_ReportingUPGDs"
	ENDIF
	&sSQLClause

	DO produceSummary WITH sTargetPath

	CLOSE TABLES ALL
	WAIT "Se han cargado los archivos planos"  WINDOW TIMEOUT 1

ENDPROC


PROCEDURE LoadAndValidatePlainsTst

	*LPARAMETERS bBeSilent as Boolean, nResultsFileHandler as Number, nSivigilaEdition as Number,  nMajorID as Number, nMinorID as Number, nBuildID as Number 
	SET STEP ON 
	
	SET SAFETY OFF 
	CLOSE TABLES ALL 
	DO LoadAndValidatePlains WITH .F., , _VERSION2010, 18, 3, 0, 'C:\Users\wilso\Documents\SIVIGILA - 2018\ARCHIVOS DE CONSOLIDACION\VERSION 2010', ;
									'D:\tmp\EnCurso\CORDOBA\NOTIFICACION SEMANA 13\AYAPEL'
	
	SET SAFETY ON 
ENDPROC 


PROCEDURE produceSummary(sPlainsTablesPath as string)
	
	* Genera el conteo de los registros que se llevaron a las tablas 'PLANO_...' desde los archivos planos de notificación. 
	*sPlainsTablesPath (in):	ruta en donde se encuentran las tablas hacia las que se cargaron los archivos planos
	
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	
	makeSummary=.T.
	IF !USED('PLANO_UPGD') THEN
		IF FILE([&sPlainsTablesPath\PLANO_UPGD.DBF]) THEN	
			USE [&sPlainsTablesPath\PLANO_UPGD]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT PLANO_UPGD
	ENDIF
	IF makeSummary THEN
		TOT_UPGD = RECCOUNT()
		UPDATE [&sPlainsTablesPath\PLANO_CONTROL] SET REG_IMP = TOT_UPGD WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='U'
	ENDIF
	
	makeSummary=.T.
	IF !USED('PLANO_RHS') THEN	
		IF FILE([&sPlainsTablesPath\PLANO_RHS.DBF]) THEN
			USE [&sPlainsTablesPath\PLANO_RHS]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT  PLANO_RHS
	ENDIF
	IF makeSummary THEN
		TOT_TH = RECCOUNT()
		UPDATE [&sPlainsTablesPath\PLANO_CONTROL] SET REG_IMP = TOT_TH WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='R'
	ENDIF

	makeSummary=.T.
	IF !USED('PLANO_PACIENTES') THEN	
		IF FILE([&sPlainsTablesPath\PLANO_PACIENTES.DBF]) THEN
			USE [&sPlainsTablesPath\PLANO_PACIENTES]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT PLANO_PACIENTES
	ENDIF
	IF makeSummary THEN
		TOT_PTE = RECCOUNT('PLANO_PACIENTES')
		UPDATE [&sPlainsTablesPath\PLANO_CONTROL] SET REG_IMP = TOT_PTE WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='N'
	ENDIF

	makeSummary=.T.
	IF !USED('PLANO_LAB') THEN	
		IF FILE ([&sPlainsTablesPath\PLANO_LAB.DBF]) THEN
			USE [&sPlainsTablesPath\PLANO_LAB]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT PLANO_LAB
	ENDIF
	IF makeSummary THEN
		TOT_LAB = RECCOUNT('PLANO_LAB')
	ENDIF

	IF SelectTable('&sPlainsTablesPath\PLANO_CONTACTOS',,.F.) THEN
		TOT_CONTACTOS = RECCOUNT('PLANO_CONTACTOS')
	ENDIF

	IF SelectTable('&sPlainsTablesPath\PLANO_SEGUIMIENTOS',,.F.) THEN
		TOT_SEGUIMIENTOS = RECCOUNT('PLANO_SEGUIMIENTOS')
	ENDIF

	makeSummary=.T.
	IF !USED('PLANO_EVENTOS') THEN	
		IF FILE([&sPlainsTablesPath\PLANO_EVENTOS.DBF]) THEN
			USE [&sPlainsTablesPath\PLANO_EVENTOS]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT PLANO_EVENTOS
	ENDIF
	IF makeSummary THEN
		TOT_DAT = RECCOUNT('PLANO_EVENTOS') - TOT_LAB
		UPDATE [&sPlainsTablesPath\PLANO_CONTROL] SET REG_IMP = TOT_DAT + TOT_LAB +  TOT_CONTACTOS + TOT_SEGUIMIENTOS WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='D'
	ENDIF

	makeSummary=.T.
	IF !USED('PLANO_BROTES') THEN	
		IF FILE ([&sPlainsTablesPath\PLANO_BROTES.DBF]) THEN
			USE [&sPlainsTablesPath\PLANO_BROTES]
		ELSE
			makeSummary=.F.
		ENDIF
	ELSE
		SELECT PLANO_BROTES
	ENDIF
	IF makeSummary THEN
		TOT_BRO = RECCOUNT('PLANO_BROTES')
		UPDATE [&sPlainsTablesPath\PLANO_CONTROL] SET REG_IMP = TOT_BRO WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='B'
	ENDIF
	
	IF n_version = _VERSION2010
		* Existe un archivo individual para Morbiliad por IRA (995,43)
		makeSummary=.T.
		IF !USED('PLANO_E43') THEN	
			IF FILE([&sPlainsTablesPath\PLANO_E43.DBF]) THEN
				USE [&sPlainsTablesPath\PLANO_E43]
			ELSE
				makeSummary=.F.
			ENDIF
		ELSE
			SELECT PLANO_E43
		ENDIF
		IF makeSummary THEN
			TOT_IRA = RECCOUNT()
			UPDATE [&sPlainsTablesPath\PLANO_CONTROL] SET REG_IMP = TOT_IRA WHERE LEFT(PLANO_CONTROL.ARCHIVO,1)='I'
			
			* Actualiza el contador de datos complementarios teniendo en cuenta que, aún cuando los ;
			datos del evento Morbiliad por IRA - 995 vienen en un archivo individual, la transferencia hacia;
			la base de datos maestra se realiza dentro del procesamiento general de tablas de datos complementarios;
			esto es, tablas cuyo nombre es de la forma EVENTOS_xx con xx=01, 02, ..., _MaxEvents
			TOT_DAT = TOT_DAT + TOT_IRA
		ENDIF
	ENDIF

ENDPROC

FUNCTION getZipFileName(sSuffix as String) as String 

	LOCAL ReturnedValue as String 
	ReturnedValue  = ''
	
	LOCAL oNotifierUnit as Object 
	oNotifierUnit=NEWOBJECT("NotifierUnit","NotifierUnit.fxp")
	
	WITH oNotifierUnit
		DO CASE 
			CASE .bIsUND
				ReturnedValue = LEFT(.sID,2) + '_' + sSuffix
			CASE .bIsUPGD
				ReturnedValue = .sID + '_' + sSuffix  
			CASE .bIsUNM
				ReturnedValue = LEFT(.sID,5) + '_' + sSuffix
		ENDCASE  
	ENDWITH
	RELEASE oNotifierUnit

	RETURN ReturnedValue 
ENDFUNC 

DEFINE CLASS PlainsLoader as Custom 

	sSourcePath = .NULL.
	sRealSourcePath = ''
	sXLSPath = .NULL.
	sErrMsg = ''
	
	FUNCTION Init(_PlainsPath as string)
		IF !DIRECTORY('&_PlainsPath')
			MKDIR "&_PlainsPath"
		ENDIF
		This.sSourcePath = ADDBS(_PlainsPath)
	ENDFUNC

	FUNCTION Destroy
	ENDFUNC

	HIDDEN PROCEDURE createPlainTableIndex(sTablePath AS String, sTargetTableName AS String)

		*Crea índices para la tabla sTargetTableName que se encuentra en la carpeta sTablePath, según se encuentren;
		 definidos en Tables.MASTER_INDEX
		
		SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
		
		LOCAL sSqlCmd as String 
		
		LOCAL nOldSelected as Number 
		STORE SELECT() TO nOldSelected

		DIMENSION aIndexExpression(1)

		SELECT TAG_NAME, TAG_EXPR, EXTRAS FROM MASTER_INDEX	WHERE TABLE_NAME  = '&sTargetTableName' INTO ARRAY aIndexExpression
		IF _TALLY = 0 THEN
			IF 'PLANO_E' $ sTargetTableName THEN 
				*Utiliza índices por default
				SELECT TAG_NAME, TAG_EXPR, EXTRAS FROM MASTER_INDEX	WHERE TABLE_NAME  = 'PLANO_EXX' INTO ARRAY aIndexExpression
			ENDIF
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

		TRY
			USE IN (sTargetTableName)
		CATCH TO oException
		ENDTRY
		
		USE IN MASTER_INDEX
		
		SELECT (nOldSelected)
		RELEASE aIndexExpression
	ENDPROC 

	PROCEDURE makeImportTables(_SvgVersion, nMajor as Byte, nMinor as Byte, nBuild as byte)
		* Autor: Wilson Aguilar
		* Crea en la carpeta This.sSourcePath las tablas DBF necesarias para cargar los datos de archivos planos;
		* teniendo en cuenta la versión del sistema _SvgVersion con la que fueron generados.
		
		*Si se pasan los parámetros opcinales nMajor, nMinor y nBuild y _SvgVersion=_VERSION2010 ;
		también se tendrá en cuenta el identificador específico de versión dado por nMajor.nMinor.nBuild

		LOCAL sOldExclusive as String

		LOCAL sSuffix as String
		sSuffix = ''
		
		WAIT WINDOW NOWAIT ' Creando tablas intermedias para cargar los planos ...'

		sOldExclusive = SET("Exclusive")
		SET EXCLUSIVE ON
		
		IF _SvgVersion = _VERSION2010 THEN
			*Las tablas de importación se crean con base en los queries de exportación usados en la versión 2010

			IF VARTYPE(nMajor) != 'N' OR VARTYPE(nMinor) != 'N' OR VARTYPE(nBuild) != 'N' THEN
				nMajor=0
				nMinor=0
				nBuild=0
			ENDIF
			sSuffix = ALLTRIM(STR(nMajor)) + '_' + ALLTRIM(STR(nMinor)) + '_' + ALLTRIM(STR(nBuild))
			
			This.sRealSourcePath = This.sSourcePath + ADDBS(sSuffix) 
			sRealSourcePath = This.sRealSourcePath
			IF !DIRECTORY('&sRealSourcePath')
				MKDIR "&sRealSourcePath"
			ENDIF
		
			SELECT DISTINCT impTableNa, ShortName, BD_source FROM ExportQueries WHERE Activo INTO CURSOR rsTablesToMake		
			SELECT rsTablesToMake
			SCAN
				sTableNameToCreate = ALLTRIM(rsTablesToMake.impTableNa)
				
				IF !FILE('"' + This.sRealSourcePath + sTableNameToCreate + '.DBF"') THEN
					compleTableName = ''
					makeImporTableSQLClause = GetQryString(UPPER(rsTablesToMake.ShortName), @compleTableName, nMajor, nMinor, nBuild)

					IF !EMPTY(makeImporTableSQLClause) THEN
						* SE AGREGAN LOS CAMPOS FALTANES, NECESARIOS PARA LA IMPORTACION 
						modifyImporTableSQLClause(compleTableName, @makeImporTableSQLClause)
						makeImporTableSQLClause = makeImporTableSQLClause+ " WHERE .F. INTO CURSOR C_TEMP "
						
						IF !EMPTY(rsTablesToMake.BD_source) THEN 
							makeImporTableSQLClause = STRTRAN(makeImporTableSQLClause, DEFAULT_BASIC_DATA_TABLE, rsTablesToMake.BD_source,-1,-1,1)
						ENDIF
						
						&makeImporTableSQLClause

						SELECT C_TEMP
						sCmd = 'COPY STRUCTURE TO "' + This.sRealSourcePath + sTableNameToCreate + '"'
						&sCmd
						This.createPlainTableIndex(This.sRealSourcePath, sTableNameToCreate)
					ENDIF
				ENDIF 
			ENDSCAN
			
			USE IN 'rsTablesToMake'

		ENDIF

		IF _SvgVersion = _VERSION2008 THEN
			*Las tablas de importación se crean con base en las definiciones de tablas de la versión 2008
			
			This.sRealSourcePath = This.sSourcePath 
			
			SELECT DISTINCT table_name FROM MASTER_PLAINS INTO CURSOR C_FILES2008
			
			SELECT C_FILES2008
			SCAN
				SCATTER MEMVAR
				sTableNameToCreate = ALLTRIM(m.table_name)
				createPlainTable(This.sSourcePath, sTableNameToCreate )
				This.createPlainTableIndex(This.sSourcePath, sTableNameToCreate)
			ENDSCAN
			USE IN 'MASTER_PLAINS'
			USE IN 'C_FILES2008'
		ENDIF

		*Crea la tabla intermedia PLANO_EVENTOS necesaria para cargar los datos complementarios de los eventos antes;
		de ser enviados a las tablas PLANO_Exx y PLANO_LAB
		_FILE = This.sRealSourcePath + 'PLANO_EVENTOS'
		CREATE TABLE "&_FILE" FREE (;
							SEMANA C(2),;
							AÑO C(4),;
							COD_PRE C(10),;
							COD_SUB C(2),;
							COD_EVE C(4),;
							TIP_IDE C(2),;
							NUM_IDE C(20),;
							CONTROL C(12))
		This.createPlainTableIndex(This.sRealSourcePath, 'PLANO_EVENTOS')

		SET EXCLUSIVE &sOldExclusive
	ENDPROC

ENDDEFINE 



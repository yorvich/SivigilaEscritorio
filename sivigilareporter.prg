#INCLUDE SIVIGILAReporter.H
#INCLUDE SIVIGILA.H

DEFINE CLASS SIVIGILAReporter as Custom

sReportTitle=''

sExcludedTableEvents = " 32 80 81 88 90 "
*Las tablas de datos complementarios incluidas en esta lista, no contienen el campo SEMANA, por tanto, no pueden ;
tenerse en cuenta al elaborar informes que involucren la semana epidemiológica


sXlsFilePath = '.'
sDataPath = '.'
sReportsPath = '.\REPORTES\'

sInitWeek = '1'
sEndWeek = '53'

HIDDEN sOldDefaultDir
sOldDefaultDir =FULLPATH('')

HIDDEN sTmpPath
sTmpPath = '.\REPORTES\Tmp\'

sOldProcedure=.NULL.
sOldOnError=ON("ERROR")

bIncludeUIs = .T.
*Indica si en los reportes en donde ello aplique, debe tenerse en cuenta o no las unidades notificadoras. Por default, ;
se tienen en cuenta.

sErrorMsg = ''
sResultsMsg = ''

HIDDEN aOutputFiles(1,4) as String 
HIDDEN nOutputFiles as Byte 
nOutputFiles = 0

FUNCTION Init
	setPathCmd="SET PATH TO [" + This.sDataPath + "] ADDITIVE"
	&setPathCmd
	
	TRY 
		MKDIR (This.sTmpPath) 
	CATCH TO oException
	ENDTRY 
		
	TRY 
		SET PROCEDURE TO UPGDsHandler ADDITIVE
	CATCH TO oException
	ENDTRY 
	This.sOldProcedure=SET("Procedure")
ENDFUNC


FUNCTION Destroy
ENDFUNC


PROCEDURE  makeCummulativeNotificationReport

	LPARAMETERS sWhereClause as String, bReportNotNotifiedWeeks as Byte, sTargetYear as String

	LOCAL sUPGDsFilter as String
	
	sOldOnEsc=ON("ESCAPE")
	ON KEY LABEL ESC
		
	sUserMessage='Procesando datos...'
	WAIT WINDOW (sUserMessage) NOWAIT

	sUPGDsFilter = " .T. "
	IF AT("#",sWhereClause)>0 THEN
		 sUPGDsFilter = sUPGDsFilter + SUBSTR(sWhereClause,AT("#",sWhereClause)+1)
	ENDIF
	sWhereClause = STRTRAN(sWhereClause, "#", "")

	*Determina para cada UPGD en qué semanas existen eventos notificados

	*Considera los registros de datos básicos de eventos individuales que no sean ajustes
	sSQLSelectCmd = "SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA FROM PACIENTE WHERE " + sWhereClause + ;
					" GROUP BY COD_PRE,COD_SUB,SEMANA UNION "
	*Agrega los registros de Brotes				
	sSQLSelectCmd = sSQLSelectCmd + "SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA FROM BROTES WHERE " + ;
					sWhereClause + "GROUP BY COD_PRE,COD_SUB,SEMANA "
	*Agrega los registros de los eventos con tabla de datos independiente (eventos tipo seguimiento, encuesta,etc.)
	DO EventosConTablaDatosIndependiente.QPR
	SELECT DISTINCT "EVENTOS_" + PADL(FORMULARIO,2,'00') AS NombreTablaEventoSeguimiento FROM rsEventosConTablaDatosIndependiente WHERE ;
		!(rsEventosConTablaDatosIndependiente.FORMULARIO $ This.sExcludedTableEvents) INTO CURSOR rsFollowupEvents
	SELECT rsFollowupEvents
	SCAN
		sSQLSelectCmd = sSQLSelectCmd + "UNION SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA FROM " + ;
						rsFollowupEvents.NombreTablaEventoSeguimiento +;
						" WHERE  " + sWhereClause + " GROUP BY COD_PRE,COD_SUB,SEMANA "
	ENDSCAN
	sSQLSelectCmd = sSQLSelectCmd + " INTO CURSOR rsAllEventsRecordCount"
	&sSQLSelectCmd

	*Reagrupa los registros de conteos de tal modo que se agrupen posibles duplicados por {COD_PRE,COD_SUB,SEMANA}
	SELECT rsAllEventsRecordCount.COD_PRE,rsAllEventsRecordCount.COD_SUB,rsAllEventsRecordCount.SEMANA, ;
		ALLTRIM(UPGD.RAZ_SOC) AS RAZ_SOC,ALLTRIM(UPGD.RES_NOT) + ', Tel: ' + ALLTRIM(UPGD.TEL) AS RES_NOTI, ;
		IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA_ FROM ;
		rsAllEventsRecordCount LEFT OUTER JOIN UPGD ON ;
		rsAllEventsRecordCount.COD_PRE=UPGD.COD_PRE AND rsAllEventsRecordCount.COD_SUB=UPGD.COD_SUB ;
		GROUP BY rsAllEventsRecordCount.COD_PRE,rsAllEventsRecordCount.COD_SUB,rsAllEventsRecordCount.SEMANA ;
		INTO CURSOR rsNotifiedWeeks READWRITE
	SELECT rsNotifiedWeeks	
	INDEX ON cod_pre+cod_sub+semana TAG UPGD_IDX
	sSourceReportCursor = "rsNotifiedWeeks" 

	IF bReportNotNotifiedWeeks=1 THEN
		*Determina para cada UPGD en qué semanas NO existen eventos notificados con base en el recordset de eventos notificados;
		por semana
		
		IF VARTYPE(sTargetYear)='C' THEN
			*Establece el número total de semanas correspondientes a la vigencia
			sSelectSQLCmd = "SELECT PADL((SEMANA ),2,'0') as SEMANA FROM CALENDARIO WHERE VIGENCIA = '" + ;
							sTargetYear + "' AND BETWEEN(SEMANA," +	This.sInitWeek  + "," + This.sEndWeek  + ")" + ;
							"INTO CURSOR rsEpidemiologicalWeeks"
			&sSelectSQLCmd

			*Ejecuta el producto cartesiano entre el total de semanas de la vigencia y las semanas con eventos notificados
			SELECT RsAllEventsRecordcount.*,rsEpidemiologicalWeeks.* FROM RsAllEventsRecordcount,rsEpidemiologicalWeeks ;
				INTO CURSOR rsCrossProduct

			*Establece en qué semanas NO se produjo notificación
			SELECT DISTINCT rsCrossProduct.COD_PRE,rsCrossProduct.COD_SUB,rsCrossProduct.semana_b AS SEMANA FROM ;
				rsCrossProduct WHERE rsCrossProduct.COD_PRE + rsCrossProduct.COD_SUB + rsCrossProduct.semana_b ;
				NOT IN (SELECT DISTINCT COD_PRE + COD_SUB + SEMANA FROM rsAllEventsRecordCount) AND &sUPGDsFilter;
				INTO CURSOR rsNotNotifiedWeeks 

			*Establece la fuente de datos para el reporte
			SELECT rsNotNotifiedWeeks.COD_PRE,rsNotNotifiedWeeks.COD_SUB,rsNotNotifiedWeeks.SEMANA, ;
				ALLTRIM(UPGD.RAZ_SOC) AS RAZ_SOC,ALLTRIM(UPGD.RES_NOT) + ', Tel: ' + ALLTRIM(UPGD.TEL) AS RES_NOTI, ;
				IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA_ FROM ;
				rsNotNotifiedWeeks LEFT OUTER JOIN UPGD ON ;
				rsNotNotifiedWeeks.COD_PRE=UPGD.COD_PRE AND rsNotNotifiedWeeks.COD_SUB=UPGD.COD_SUB ;
				GROUP BY rsNotNotifiedWeeks.COD_PRE,rsNotNotifiedWeeks.COD_SUB,rsNotNotifiedWeeks.SEMANA ;
				INTO CURSOR rsNOTNotifiedWeeksSpec READWRITE

			SELECT rsNOTNotifiedWeeksSpec	
			INDEX ON cod_pre+cod_sub+semana TAG UPGD_IDX
			sSourceReportCursor = "rsNOTNotifiedWeeksSpec" 
		ENDIF
	ENDIF

	IF RECCOUNT()=0
		This.sErrorMsg ='No se encontraron datos para producir el reporte'
	ELSE
		This.sReportTitle=IIF(bReportNotNotifiedWeeks=0,'REPORTE DE CUMPLIMIENTO ACUMULADO SEMANAL','REPORTE DE INCUMPLIMIENTO ACUMULADO SEMANAL')

		SELECT (sSourceReportCursor)
		SET ORDER TO TAG UPGD_IDX
		sRunReportCmd="REPORT FORM " + This.sReportsPath + CUMMULATIVE_REPORT_FILENAME + " TO PRINTER PROMPT PREVIEW "
		&sRunReportCmd

		*Crea una tabla temporal para almacenar los resultados del reporte que serán exportados hacia un archivo excel
		sWeekFieldNamesToCreate=''
		FOR nFieldToCreate=1 TO VAL(This.sEndWeek)
		   sWeekFieldNamesToCreate = sWeekFieldNamesToCreate + 'S' + TRANSFORM(nFieldToCreate,'@L 99') + ' N(3,0), '
		ENDFOR
		sWeekFieldNamesToCreate = SUBSTR(sWeekFieldNamesToCreate,1,LEN(sWeekFieldNamesToCreate)-2)
		CREATE TABLE XLSWeekNotificationResultsTable FREE (cod_pre c(10), cod_sub c(2), raz_soc c(120) NULL, ;
			resp_noti c(100) NULL, ACTIVA_ C(13) , &sWeekFieldNamesToCreate)
		INDEX ON cod_pre+cod_sub TAG UPGD_IDX


		SELECT (sSourceReportCursor)
		SCAN
			sCurrentCOD_SUB=COD_SUB
			sCurrentCOD_PRE=COD_PRE
			sCurrentUPGDName = RAZ_SOC
			sCurrentPersonResposible = RES_NOTI
			sWeekFieldNameToCheck = 'S' + TRANSFORM(SEMANA,'@l 99')
			SELECT XLSWeekNotificationResultsTable
			SEEK sCurrentCOD_PRE+sCurrentCOD_SUB
			IF !FOUND()
				INSERT INTO XLSWeekNotificationResultsTable (cod_pre, cod_sub, raz_soc, resp_noti, ACTIVA_) VALUES ;
					(sCurrentCOD_PRE,sCurrentCOD_SUB,sCurrentUPGDName ,sCurrentPersonResposible, &sSourceReportCursor..ACTIVA_ )
			ENDIF
			REPLACE &sWeekFieldNameToCheck  WITH 1 &&IIF(bReportNotNotifiedWeeks=0,1,0)
		ENDSCAN

		This.exportToXLS("XLSWeekNotificationResultsTable", IIF(bReportNotNotifiedWeeks=1,'SEMANAS SIN NOTIFICAR','SEMANAS NOTIFICADAS'))
	ENDIF
	ON KEY LABEL ESC &sOldOnEsc
ENDPROC


PROCEDURE makeSingleWeekFullfilmentReport(sWhereClause as String, sTargetWeek as String, nDisagregationLevel as String, ;
											nTargetReport as String)
														
	*Produce un reporte de cumplimiento semanal para la semana sTargetWeek teniendo en cuenta las notificaciones que cumplan las condiciones sWhereClause;
	En la generación del reporte se tiene en cuenta tanto notificaciones positivas como negativas de eventos individuales y colectivos.
	
	*nDisagregationLevel (in) :  2-->Departamental	3-->Municipio	4-->UPGD
	
	*nTargetReport (in) : Tipo de detalle a generar así: ;
						  2-->Muestra departamentos	3-->Muesta municipios por cada departamento	4-->Muestra UPGDs por cada municipio


	LOCAL sUPGDsFilter AS String
	*No se usa en la implementación actual
	
	sOldOnEsc=ON("ESCAPE")
	ON KEY LABEL ESC

	sOldSetTalk=SET("Talk")
	SET TALK ON
			
	sUserMessage='Procesando datos...'
	WAIT WINDOW (sUserMessage) NOWAIT

	sUPGDsFilter = " .T. "
	IF AT("#",sWhereClause)>0 THEN
		 sUPGDsFilter = sUPGDsFilter + SUBSTR(sWhereClause,AT("#",sWhereClause)+1)
	ENDIF
	sWhereClause = STRTRAN(sWhereClause, "#", "")

	*Determina qué UPGDs que cumplen las condiciones de filtrado sWhereClause, tienen eventos notificados

	*Considera los registros de datos básicos de eventos individuales que no sean ajustes
	sSQLSelectCmd = "SELECT COD_PRE,COD_SUB,'NO' AS EnSilencio FROM PACIENTE WHERE " + sWhereClause + ;
					" GROUP BY COD_PRE,COD_SUB UNION "
	*Agrega los registros de Brotes				
	sSQLSelectCmd = sSQLSelectCmd + "SELECT COD_PRE,COD_SUB,'NO' AS EnSilencio FROM BROTES WHERE " + ;
					sWhereClause + "GROUP BY COD_PRE,COD_SUB "
	*Agrega los registros de los eventos que tienen su propia tabla de almacenamiento
	DO EventosConTablaDatosIndependiente.QPR
	SELECT DISTINCT "EVENTOS_" + PADL(FORMULARIO,2,'00') AS NombreTablaEventoSeguimiento FROM rsEventosConTablaDatosIndependiente WHERE ;
		!(rsEventosConTablaDatosIndependiente.FORMULARIO $ This.sExcludedTableEvents) INTO CURSOR rsFollowupEvents
	SELECT rsFollowupEvents
	SCAN
		sSQLSelectCmd = sSQLSelectCmd + "UNION SELECT COD_PRE,COD_SUB,'NO' AS EnSilencio FROM " + ;
						rsFollowupEvents.NombreTablaEventoSeguimiento +;
						" WHERE " + sWhereClause + " GROUP BY COD_PRE,COD_SUB "
	ENDSCAN
	sSQLSelectCmd = sSQLSelectCmd + " INTO CURSOR rsWithNotification"
	&sSQLSelectCmd

	IF RECCOUNT()=0
		This.sErrorMsg ='No se encontraron datos para producir el reporte'
	ELSE
		*Asigna características descriptivas a las UPGD recien establecidas (razón social, teléfono, etc.)
		SELECT rsWithNotification.COD_PRE+rsWithNotification.COD_SUB AS COD_UPGD, ;
			ALLTRIM(UPGD.RAZ_SOC) AS RAZ_SOC,ALLTRIM(UPGD.RES_NOT) + ', Tel: ' + ALLTRIM(UPGD.TEL) AS RES_NOTI, ;
			IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA_, ;
			IIF(UPGD.ES_UNI_NOT="1",'UI  ','UPGD') AS TIPO_UNI, ;
			 MUNICIPIOS.COD_MUN, ;
			MUNICIPIOS.NOM_MUN, DEPTOS.COD_DEP, DEPTOS.NOM_DEP, EnSilencio ;
			FROM rsWithNotification ;
				LEFT OUTER JOIN UPGD ON ;
					rsWithNotification.COD_PRE=UPGD.COD_PRE AND rsWithNotification.COD_SUB=UPGD.COD_SUB ;
				LEFT OUTER JOIN MUNICIPIOS ON LEFT(rsWithNotification.COD_PRE,5) = MUNICIPIOS.COD_MUN ;
				LEFT OUTER JOIN DEPTOS ON LEFT(rsWithNotification.COD_PRE,2) = DEPTOS.COD_DEP ;
			WHERE MUNICIPIOS.ACTIVO ;
			INTO CURSOR rsUPGDsWithNotification READWRITE
		SELECT rsUPGDsWithNotification 	
*SET STEP ON 
		*Agrupa las UPGDs según el nivel de desagregación requerido
		DO CASE
			CASE nDisagregationLevel = 2 OR nDisagregationLevel = 3
				*Construye la fuente de datos para el reporte en cuatro pasos
				
				*P1: Selecciona de las notificaciones, campos correspondientes a entidades territoriales hasta el nivel de municipio;
				en este caso, todos los municipios han cumplido
   				SELECT COD_DEP,NOM_DEP,COD_MUN,NOM_MUN,EnSilencio FROM rsUPGDsWithNotification ;
   					INTO TABLE (ADDBS(SYS(2023)) + 'rsSourceReportCursorTmp1')
   				
   				*P2: Selecciona todos los municipios que NO han cumplido (se encuentran en silencio)
   				SELECT DEPTOS.COD_DEP,DEPTOS.NOM_DEP,MUNICIPIOS.COD_MUN,MUNICIPIOS.NOM_MUN,'SÍ' as EnSilencio   ;
   					FROM MUNICIPIOS INNER JOIN DEPTOS ON;
   						LEFT(MUNICIPIOS.COD_MUN,2) =  DEPTOS.COD_DEP  ;
   						LEFT OUTER JOIN rsSourceReportCursorTmp1 ON ;
   						MUNICIPIOS.COD_MUN = rsSourceReportCursorTmp1.COD_MUN ; 
   					WHERE MUNICIPIOS.ACTIVO AND COD_DANE!='99999' AND rsSourceReportCursorTmp1.COD_MUN IS NULL;
   					INTO CURSOR rsSourceReportCursorTmp2
   				
   				*P3: Agrupa tanto los municipios que han cumplido como los que no	
   				SELECT * FROM rsSourceReportCursorTmp1 UNION SELECT * FROM rsSourceReportCursorTmp2 INTO CURSOR rsSourceReportCursorTmp3
   				
   				*P4: ordena el recordset recien construido de tal forma que sea apropiado para la generación del reporte
	   			SELECT * FROM rsSourceReportCursorTmp3 ORDER BY COD_DEP,EnSilencio,COD_MUN INTO CURSOR rsSourceReportCursor
   					
			CASE nDisagregationLevel = 4
				*Construye la fuente de datos para el reporte en cuatro pasos
				
				*P1: Selecciona todas las notificaciones; en este caso, todas las UPGDs o UIs que han cumplido
				SELECT * FROM rsUPGDsWithNotification INTO TABLE (ADDBS(SYS(2023)) + 'rsSourceReportCursorTmp1')
				
				*P2: Selecciona todas las UPGDs o UIs que NO han cumplido (se encuentran en silencio)
				sSQLCmd = "SELECT UPGD.COD_PRE+UPGD.COD_SUB AS COD_UPGD,ALLTRIM(UPGD.RAZ_SOC) AS RAZ_SOC," +;
							"ALLTRIM(UPGD.RES_NOT) + " + " ', Tel: ' + ALLTRIM(UPGD.TEL) AS RES_NOTI," +;
   							"IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA_," +;
   							"IIF(UPGD.ES_UNI_NOT='1','UI  ','UPGD') AS TIPO_UNI," +;
   							"MUNICIPIOS.COD_MUN, MUNICIPIOS.NOM_MUN, DEPTOS.COD_DEP, DEPTOS.NOM_DEP, 'SÍ' as EnSilencio " +;
   							"FROM UPGD " +;
   							"LEFT OUTER JOIN rsSourceReportCursorTmp1 " +;
								"ON UPGD.COD_PRE+UPGD.COD_SUB=rsSourceReportCursorTmp1.COD_UPGD " +;
							"LEFT OUTER JOIN MUNICIPIOS ON LEFT(UPGD.COD_PRE,5) = MUNICIPIOS.COD_MUN " +;
							"LEFT OUTER JOIN DEPTOS ON LEFT(UPGD.COD_PRE,2) = DEPTOS.COD_DEP " +;
							" WHERE (UPGD.ACT_SIV=1 AND rsSourceReportCursorTmp1.COD_UPGD IS NULL) AND " + sUPGDsFilter 
				*IF this.bIncludeUIs THEN
				*	sSQLCmd = sSQLCmd + " AND (" + getUICondition() + ")"
				*ENDIF
				sSQLCmd = sSQLCmd + " INTO CURSOR rsSourceReportCursorTmp2"
				&sSQLCmd

   				*P3: Agrupa tanto las UPGDs Y UIs que han cumplido como los que no	
   				SELECT * FROM rsSourceReportCursorTmp1 UNION SELECT * FROM rsSourceReportCursorTmp2 INTO CURSOR rsSourceReportCursorTmp3
   				
   				*P4: ordena el recordset recien construido de tal forma que sea apropiado para la generación del reporte
	   			SELECT * FROM rsSourceReportCursorTmp3 ORDER BY COD_DEP,COD_MUN,TIPO_UNI DESC,EnSilencio INTO CURSOR rsSourceReportCursor
				
		ENDCASE
		
		*ejecuta el reporte apropiado 
		This.sReportTitle='CUMPLIMIENTO EN LA NOTIFICACION DE LA SEMANA: ' + sTargetWeek 

		DO CASE
			CASE nTargetReport = 2
				sReportName = WEEKLY_CUMMULATIVE_REPORT_DEP_FILENAME
			CASE nTargetReport = 3
				sReportName = WEEKLY_CUMMULATIVE_REPORT_MUN_FILENAME
			CASE nTargetReport = 4						
				sReportName = WEEKLY_CUMMULATIVE_REPORT_UPGD_FILENAME
		ENDCASE
		sRunReportCmd="REPORT FORM " + This.sReportsPath + sReportName + " TO PRINTER PROMPT PREVIEW "
		&sRunReportCmd
*SET STEP ON 
		*exporta los datos con los que se construyó el reporte hacia un archivo excel
		*This.exportToXLS("rsSourceReportCursor", "REPORTE DE CUMPLIMIENTO SEMANAL DE NOTIFICACIÓN_S" + sTargetWeek + '_' +;
						+ STRTRAN(DTOC(DATE()),"/","_") + '_T' + STRTRAN(TIME(),":","_"))

		*Exporta el recordset de notificaciones consolidadas hacia un archivo excel
		oDataExporter = NEWOBJECT("DataExporter","DataExporter.Prg",.null.,'rsSourceReportCursor')
		oDataExporter.sExportationPath = This.sXlsFilePath
		oDataExporter.sExportedFileName =  "REPORTE DE CUMPLIMIENTO SEMANAL DE NOTIFICACIÓN_S" + sTargetWeek + '_' +;
											STRTRAN(DTOC(DATE()),"/","_") + '_T' + STRTRAN(TIME(),":","_")
		oDataExporter.exportToXLS()

		IF oDataExporter.nInitRecs>0 THEN
			oDataExporter.ShowXLSExportationResults()
		ELSE
			WAIT  "No se encontraron registros para exportar" WINDOW NOWAIT TIMEOUT 5
		ENDIF

		RELEASE oDataExporter

	ENDIF
	ON KEY LABEL ESC &sOldOnEsc
	SET TALK &sOldSetTalk
ENDPROC


PROCEDURE ShowResults

	This.sResultsMsg = This.sResultsMsg	+ CHR(13) +;
						'Se ha generado un archivo con los resultado del Informe en formato compatible con Excel 2007 en: ' ;
						+ CHR(13) + This.sXlsFilePath 
	MESSAGEBOX( This.sResultsMsg,  0 + 48 , sWindowTitle ) 

ENDPROC

PROCEDURE exportToXLS


	LPARAMETERS rsToExportName AS String, sXLSFileName as String
	
	*Exporta los registros de rsToExportName a un archivo con formato XLS o CSV, dependiendo de ;
	si el número de registros en rsToExportName es menor o mayor que nMaxXLSRows.
	
	*El archivo exportado tendrá nombre rsToExportName + sXLSFileNameSuffix y quedará ubicado en This.xlsFilePath . ;
	

	LOCAL sExportedFileType as String, xlsFileName as String

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

		WAIT sExportingMsg WINDOW NOWAIT
		sCopyToCmd = 'COPY TO "' + ADDBS(This.sXlsFilePath) + sXLSFileName + '" TYPE ' + sExportedFileType
		&sCopyToCmd
		USE
		
		WAIT CLEAR
	ENDWITH

ENDPROC


PROCEDURE  makeEpidemiologicalSilenceReport(sWhereClause as String, sTargetYear as String)

	LOCAL sUPGDsFilter AS String
	
	sOldOnEsc=ON("ESCAPE")
	ON KEY LABEL ESC

	sOldSetTalk=SET("Talk")
	SET TALK ON

	sUserMessage='Procesando datos...'
	WAIT WINDOW (sUserMessage) NOWAIT

	SET PROCEDURE TO SIVIGILAUtilities ADDITIVE

	sUPGDsFilter = " .T. "
	IF AT("#",sWhereClause)>0 THEN
		 sUPGDsFilter = sUPGDsFilter + SUBSTR(sWhereClause,AT("#",sWhereClause)+1)
	ENDIF
	sWhereClause = STRTRAN(sWhereClause, "#", "")

	*Determina para cada UPGD en qué semanas existen eventos notificados clasificando las notificaciones entre;
	negativas y positivas en función del código del evento

	*Considera los registros de datos básicos de eventos individuales
	*sSQLSelectCmd = "SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA," + ;
					"IIF(COD_EVE='" + NO_NOTIFICATION_EVENT_CODE + "',1,0) AS NOTIFICACION_NEGATIVA " + ;
					" FROM PACIENTE WHERE " + sWhereClause + ;
					" GROUP BY COD_PRE,COD_SUB,SEMANA INTO CURSOR rsIndividualNotifications READWRITE"

	sSQLSelectCmd = "SELECT DISTINCT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA_, COD_EVE FROM PACIENTE WHERE " + ;
					sWhereClause + " ORDER BY COD_PRE,COD_SUB,SEMANA_,COD_EVE INTO CURSOR rsInitIndividualNotifications NOFILTER"
	&sSQLSelectCmd 

	sSQLSelectCmd = "SELECT COD_PRE,COD_SUB,SEMANA_ AS SEMANA," + ;
					"IIF(COD_EVE='" + NO_NOTIFICATION_EVENT_CODE + "',1,0) AS NOTIFICACION_NEGATIVA " + ;
					" FROM rsInitIndividualNotifications " + ;
					" GROUP BY COD_PRE,COD_SUB,SEMANA INTO CURSOR rsIndividualNotifications READWRITE"
	&sSQLSelectCmd 
*SET STEP ON 
	*Considera los registros de Brotes				
	sSQLSelectCmd = "SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA," + ;
					"0 AS NOTIFICACION_NEGATIVA FROM BROTES WHERE " + ;
					sWhereClause + "GROUP BY COD_PRE,COD_SUB,SEMANA  INTO CURSOR rsCollectiveNotifications"
	&sSQLSelectCmd 
	
	*Considera los registros de los eventos tipo seguimiento
	DO EventosConTablaDatosIndependiente.QPR
	SELECT DISTINCT "EVENTOS_" + PADL(FORMULARIO,2,'00') AS NombreTablaEventoSeguimiento FROM rsEventosConTablaDatosIndependiente WHERE ;
		!(rsEventosConTablaDatosIndependiente.FORMULARIO $ This.sExcludedTableEvents) INTO CURSOR rsFollowupEvents
	SELECT rsFollowupEvents
	sSQLSelectCmd = ""
	SCAN
		sSQLSelectCmd = sSQLSelectCmd + "SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA," + ;
						"0 AS NOTIFICACION_NEGATIVA  FROM " + ;
						rsFollowupEvents.NombreTablaEventoSeguimiento +;
						" WHERE  " + sWhereClause + " GROUP BY COD_PRE,COD_SUB,SEMANA UNION "
	ENDSCAN
	sSQLSelectCmd = LEFT(sSQLSelectCmd ,LEN(sSQLSelectCmd) - 6)
	sSQLSelectCmd = sSQLSelectCmd + " INTO CURSOR rsFollowupsNotifications"
	&sSQLSelectCmd

	* Actualiza las notificaciones individuales tomando en cuenta las notificaciones colectivas y de eventos de seguimiento ;
	de tal forma que notificaciones positivas sobreescriban notificaciones negativas individuales. De este modo, se podrá ;
	construir el conjunto de todas las notificaciones acumuladas por UPGD independientemente de si son individuales, de brotes ;
	o de eventos de seguimiento
	=UpdateFieldsTable("rsIndividualNotifications", "rsIndividualNotifications.NOTIFICACION_NEGATIVA=0", "rsCollectiveNotifications", ;
						"COD_PRE, COD_SUB, SEMANA")
	=UpdateFieldsTable("rsIndividualNotifications", "rsIndividualNotifications.NOTIFICACION_NEGATIVA=0", "rsFollowupsNotifications", ;
						"COD_PRE, COD_SUB, SEMANA")

	*Consolida los registros de semanas notificadas (en 3 pasos)
	*Paso 1
	SELECT * FROM rsIndividualNotifications UNION SELECT * FROM rsCollectiveNotifications ;
		UNION SELECT * FROM rsFollowupsNotifications INTO CURSOR rsAllEventsRecordCount

	**Paso 2: establece el número total de semanas correspondientes al año objetivo
	sSelectSQLCmd = "SELECT PADL((SEMANA ),2,'0') as SEMANA, DESDE, HASTA FROM CALENDARIO WHERE VIGENCIA = '" + ;
					sTargetYear + "' AND BETWEEN(SEMANA," +	This.sInitWeek  + "," + This.sEndWeek  + ")" + ;
					"INTO CURSOR rsEpidemiologicalWeeks"
	&sSelectSQLCmd
	

	*Paso 3: complementa los registros de conteos con la información descriptiva de UPGDs, MUNICIPIOS, DEPARTAMENTOS y Límites de ;
	semanas epidemiológicas
	SELECT rsAllEventsRecordCount.COD_PRE,rsAllEventsRecordCount.COD_SUB,rsAllEventsRecordCount.SEMANA, ;
		rsAllEventsRecordCount.NOTIFICACION_NEGATIVA, ;
		ALLTRIM(UPGD.RAZ_SOC) AS RAZ_SOC,ALLTRIM(UPGD.RES_NOT) + ', Tel: ' + ALLTRIM(UPGD.TEL) AS RES_NOTI, ;
		IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA_, ;
		UPGD.FEC_INICAR, rsEpidemiologicalWeeks.DESDE, rsEpidemiologicalWeeks.HASTA, ;
		MUNICIPIOS.COD_MUN, MUNICIPIOS.NOM_MUN, DEPTOS.COD_DEP, DEPTOS.NOM_DEP FROM ;
		rsAllEventsRecordCount LEFT OUTER JOIN UPGD ON ;
		rsAllEventsRecordCount.COD_PRE=UPGD.COD_PRE AND rsAllEventsRecordCount.COD_SUB=UPGD.COD_SUB ;
		LEFT OUTER JOIN rsEpidemiologicalWeeks ON rsAllEventsRecordCount.SEMANA = rsEpidemiologicalWeeks.SEMANA ;
		LEFT OUTER JOIN MUNICIPIOS ON LEFT(rsAllEventsRecordCount.COD_PRE,5) = MUNICIPIOS.COD_MUN ;
		LEFT OUTER JOIN DEPTOS ON LEFT(rsAllEventsRecordCount.COD_PRE,2) = DEPTOS.COD_DEP ;
		GROUP BY rsAllEventsRecordCount.COD_PRE,rsAllEventsRecordCount.COD_SUB,rsAllEventsRecordCount.SEMANA ;
		INTO CURSOR rsNotifiedWeeks READWRITE
	SELECT rsNotifiedWeeks	
	sSourceReportCursor = "rsNotifiedWeeks" 


	*Determina para cada UPGD en qué semanas NO existen eventos notificados con base en el recordset de eventos notificados;
	por semana (en 5 pasos)

	*Paso 1: ejecuta el producto cartesiano entre el total de semanas de la vigencia y las semanas con eventos notificados
	SELECT RsAllEventsRecordcount.*,rsEpidemiologicalWeeks.* FROM RsAllEventsRecordcount,rsEpidemiologicalWeeks ;
		INTO CURSOR rsCrossProduct

	*Paso 2: establece en qué semanas NO se produjo notificación
	SELECT DISTINCT rsCrossProduct.COD_PRE,rsCrossProduct.COD_SUB,rsCrossProduct.semana_b AS SEMANA FROM ;
		rsCrossProduct WHERE rsCrossProduct.COD_PRE + rsCrossProduct.COD_SUB + rsCrossProduct.semana_b ;
		NOT IN (SELECT DISTINCT COD_PRE + COD_SUB + SEMANA FROM rsAllEventsRecordCount) AND &sUPGDsFilter  ;
		INTO CURSOR rsNotNotifiedWeeks READWRITE
		
	*Paso 3: agrega al recordset rsNotNotifiedWeeks recien creado, las UPGDs que se encuentran activas en el Sistema y ;
	que no han tenido notificación alguna para el período considerado. Esto obedece al hecho de que, hasta este punto, ;
	se ha consultado los datos de notificaciones existentes en las tablas PACIENTE, BROTES y tablas de eventos ;
	tipo seguimiento o encuesta; sin embargo, cabe la posibilidad de que existan UPGDs activas que nunca hayan ;
	notificado y deben aparecer, en consecuencia, silenciosas para todo el período considerado
	INSERT INTO rsNotNotifiedWeeks SELECT COD_PRE, COD_SUB, rsEpidemiologicalWeeks.SEMANA ;
		 FROM UPGD, rsEpidemiologicalWeeks  WHERE ACT_SIV=1 AND &sUPGDsFilter AND COD_PRE + COD_SUB ;
		 NOT IN(SELECT rsAllEventsRecordCount.COD_PRE+rsAllEventsRecordCount.COD_SUB FROM rsAllEventsRecordCount)

	*Paso 4: establece la fuente de datos para el reporte
	SELECT rsNotNotifiedWeeks.COD_PRE,rsNotNotifiedWeeks.COD_SUB,rsNotNotifiedWeeks.SEMANA, ;
		-1 AS NOTIFICACION_NEGATIVA, ;
		ALLTRIM(UPGD.RAZ_SOC) AS RAZ_SOC,ALLTRIM(UPGD.RES_NOT) + ', Tel: ' + ALLTRIM(UPGD.TEL) AS RES_NOTI, ;
		IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA_, ; 
		UPGD.FEC_INICAR, rsEpidemiologicalWeeks.DESDE, rsEpidemiologicalWeeks.HASTA, ;
		MUNICIPIOS.COD_MUN, MUNICIPIOS.NOM_MUN, DEPTOS.COD_DEP, DEPTOS.NOM_DEP FROM ;
		rsNotNotifiedWeeks LEFT OUTER JOIN UPGD ON ;
		rsNotNotifiedWeeks.COD_PRE=UPGD.COD_PRE AND rsNotNotifiedWeeks.COD_SUB=UPGD.COD_SUB ;
		LEFT OUTER JOIN rsEpidemiologicalWeeks ON rsNotNotifiedWeeks.SEMANA = rsEpidemiologicalWeeks.SEMANA ;
		LEFT OUTER JOIN MUNICIPIOS ON LEFT(rsNotNotifiedWeeks .COD_PRE,5) = MUNICIPIOS.COD_MUN ;
		LEFT OUTER JOIN DEPTOS ON LEFT(rsNotNotifiedWeeks .COD_PRE,2) = DEPTOS.COD_DEP ;
		GROUP BY rsNotNotifiedWeeks.COD_PRE,rsNotNotifiedWeeks.COD_SUB,rsNotNotifiedWeeks.SEMANA ;
		INTO CURSOR rsNOTNotifiedWeeksSpec READWRITE

	SELECT rsNOTNotifiedWeeksSpec	

	*Paso 5: consolida los registros de semanas notificadas y no notificadas
	SELECT * FROM rsNotifiedWeeks UNION SELECT * FROM rsNOTNotifiedWeeksSpec INTO CURSOR sSourceReportCursor 

	IF RECCOUNT()=0
		This.sErrorMsg ='No se encontraron datos para producir el reporte'
	ELSE
*SET STEP ON 
		*Crea una tabla temporal que servirá para almacenar los resultados del reporte y exportarlos hacia un archivo excel
		sWeekFieldNamesToCreate=''
		FOR nFieldToCreate=1 TO MAX_EPIDEMIOLOGICAL_WEEKS &&VAL(This.sEndWeek)
		   sWeekFieldNamesToCreate = sWeekFieldNamesToCreate + 'S' + TRANSFORM(nFieldToCreate,'@L 99') + ' C(1), '
		ENDFOR
		sWeekFieldNamesToCreate = SUBSTR(sWeekFieldNamesToCreate,1,LEN(sWeekFieldNamesToCreate)-2)
		CREATE TABLE (This.sTmpPath + 'XLSWeekNotificationResultsTable') FREE (COD_DEP c(2) NULL, NOM_DEP C(60) NULL, COD_MUN c(5) NULL, NOM_MUN C(60) NULL,;
				cod_pre c(10), cod_sub c(2), raz_soc c(120) NULL, ;
				resp_noti c(100) NULL, FEC_INICAR D NULL, ACTIVA_ C(13) NULL, CUENTA_SEM N(3), N_SEM_POS N(3), N_SEM_NEG N(3), N_SEM_SIL N(3), ;
				PORCEN_POS N(7,3), PORCEN_NEG N(7,3), PORCEN_SIL N(7,3), ;
				RIESGO_SIL N(1), RIESGO_NEG N(1), &sWeekFieldNamesToCreate)
		INDEX ON cod_pre+cod_sub TAG UPGD_IDX
		
		*Almacena en la tabla temporal recien creada, los resultados en formato apropiado para ser mostrados en el reporte: ;
		Las semanas con notificación negativa aparecerán marcadas con un signo '-' ;
		Las semanas con notificación positiva aparecerán marcadas con un signo '+' ;
		Las semanas en silencio aparecerán marcadas con un vacío
		SELECT sSourceReportCursor
		SCAN
			sWeekFieldNameToCheck = 'S' + TRANSFORM(SEMANA,'@l 99')
			SELECT XLSWeekNotificationResultsTable
			SEEK sSourceReportCursor.COD_PRE + sSourceReportCursor.COD_SUB
			IF !FOUND()
				INSERT INTO XLSWeekNotificationResultsTable (COD_DEP, NOM_DEP, COD_MUN, NOM_MUN, ;
					cod_pre, cod_sub, raz_soc, resp_noti, FEC_INICAR , ACTIVA_) VALUES ;
					(sSourceReportCursor.COD_DEP,sSourceReportCursor.NOM_DEP,sSourceReportCursor.COD_MUN, ;
					sSourceReportCursor.NOM_MUN, sSourceReportCursor.COD_PRE,sSourceReportCursor.COD_SUB,;
					sSourceReportCursor.RAZ_SOC, sSourceReportCursor.RES_NOTI, sSourceReportCursor.FEC_INICAR, ;
					sSourceReportCursor.ACTIVA_ )
			ENDIF
			REPLACE &sWeekFieldNameToCheck  WITH IIF(sSourceReportCursor.NOTIFICACION_NEGATIVA=1,'-',IIF(sSourceReportCursor.NOTIFICACION_NEGATIVA =0,'+','')) 

			*Actualiza las columnas de semanas con notificación positiva, negativa y silenciosa
			IF  sSourceReportCursor.FEC_INICAR <= sSourceReportCursor.DESDE THEN
				REPLACE CUENTA_SEM WITH CUENTA_SEM + 1
				REPLACE N_SEM_POS WITH N_SEM_POS + IIF(sSourceReportCursor.NOTIFICACION_NEGATIVA=0 ,1,0)
				REPLACE N_SEM_NEG WITH N_SEM_NEG + IIF(sSourceReportCursor.NOTIFICACION_NEGATIVA=1,1,0)
				REPLACE N_SEM_SIL WITH N_SEM_SIL + IIF(sSourceReportCursor.NOTIFICACION_NEGATIVA=-1,1,0)
			ENDIF
		ENDSCAN

		SELECT XLSWeekNotificationResultsTable
		*nTotalweeks=VAL(This.sEndWeek)-VAL(This.sInitWeek)+1

		*Determina para cada UPGD si se encentra en alto riesgo por silencio epidemiológico o notificación negativa
		REPLACE RIESGO_SIL WITH 1 FOR N_SEM_SIL > CEILING((CUENTA_SEM * HIGH_RISK_PERCENTAGE)/100)
		REPLACE RIESGO_NEG WITH 1 FOR N_SEM_NEG > CEILING((CUENTA_SEM * HIGH_RISK_PERCENTAGE)/100)

		*Actualiza las columnas de porcentaje de semanas con notificación positiva, negativa y silenciosa
		REPLACE PORCEN_POS WITH N_SEM_POS/CUENTA_SEM, PORCEN_NEG WITH N_SEM_NEG/CUENTA_SEM, PORCEN_SIL WITH N_SEM_SIL/CUENTA_SEM FOR CUENTA_SEM>0
		  
		*Marca en la tabla temporal las semanas que no se encuentran en el alcance del informe con el caracter CHR(149)-Un punto
		sFieldsToSetToNA = ''
		FOR nFieldToSetToNA=VAL(This.sEndWeek)+1  TO MAX_EPIDEMIOLOGICAL_WEEKS 
			sFieldsToSetToNA=sFieldsToSetToNA + 'S' + TRANSFORM(nFieldToSetToNA,'@L 99') + ' WITH CHR(149), '
		NEXT nFieldToSetToNA
		sFieldsToSetToNA = LEFT(sFieldsToSetToNA,LEN(sFieldsToSetToNA)-2)
		SELECT XLSWeekNotificationResultsTable 
		IF !EMPTY(sFieldsToSetToNA ) THEN
			sReplaceCmd="REPLACE " + sFieldsToSetToNA + " ALL"
			&sReplaceCmd
		ENDIF
		
		*Ejecuta el reporte
		This.sReportTitle='CUMPLIMIENTO EN LA NOTIFICACIÓN SEMANAL - POSITIVA, NEGATIVA, SILENCIO SEMANAS ' + ;
							This.sInitWeek + ' - ' + This.sEndWeek

		SELECT XLSWeekNotificationResultsTable 
		SET ORDER TO TAG UPGD_IDX
		sRunReportCmd="REPORT FORM " + This.sReportsPath + SILENT_REPORT_UPGD_FILENAME + " TO PRINTER PROMPT PREVIEW "
		&sRunReportCmd

		*exporta los resultados de la tabla temporal hacia un archivo excel
		This.exportToXLS("XLSWeekNotificationResultsTable", "REPORTE DE NOTIFICACION POSITIVA NEGATIVA SILENCIO")
		
		*Borra archivos temporales de procesamiento
		sDelCmd = 'DELETE FILE ' + This.sTmpPath + 'XLSWeekNotificationResultsTable.*'
		&sDelCmd
	ENDIF
	ON KEY LABEL ESC &sOldOnEsc
	SET TALK &sOldSetTalk
ENDPROC


PROCEDURE  makeFullfilmentReport

	LPARAMETERS sWhereClause as String, sTargetYear as String

	LOCAL sUPGDsFilter AS String

	sOldOnEsc=ON("ESCAPE")
	ON KEY LABEL ESC

	sOldSetTalk=SET("Talk")
	SET TALK ON

	sUserMessage='Procesando datos...'
	WAIT WINDOW (sUserMessage) NOWAIT

	sUPGDsFilter = " .T. "
	IF AT("#",sWhereClause)>0 THEN
		 sUPGDsFilter = sUPGDsFilter + SUBSTR(sWhereClause,AT("#",sWhereClause)+1)
	ENDIF
	sWhereClause = STRTRAN(sWhereClause, "#", "")

	*Determina para cada UPGD en qué semanas existen eventos notificados en función del código del evento

	*Considera los registros de datos básicos de eventos individuales
	sSQLSelectCmd = "SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA," + ;
					"1 AS NOTIFICACION_EXISTENTE FROM PACIENTE WHERE " + sWhereClause + ;
					" GROUP BY COD_PRE,COD_SUB,SEMANA UNION "
	*Agrega los registros de Brotes				
	sSQLSelectCmd = sSQLSelectCmd + "SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA," + ;
					"1 AS NOTIFICACION_EXISTENTE FROM BROTES WHERE " + ;
					sWhereClause + "GROUP BY COD_PRE,COD_SUB,SEMANA "
	*Agrega los registros de los eventos tipo seguimiento
	DO EventosConTablaDatosIndependiente.QPR
	SELECT DISTINCT "EVENTOS_" + PADL(FORMULARIO,2,'00') AS NombreTablaEventoSeguimiento FROM rsEventosConTablaDatosIndependiente WHERE ;
		!(rsEventosConTablaDatosIndependiente.FORMULARIO $ This.sExcludedTableEvents) INTO CURSOR rsFollowupEvents
	SELECT rsFollowupEvents
	SCAN
		sSQLSelectCmd = sSQLSelectCmd + "UNION SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA," + ;
						"1 AS NOTIFICACION_EXISTENTE FROM " + ;
						rsFollowupEvents.NombreTablaEventoSeguimiento +;
						" WHERE  " + sWhereClause + " GROUP BY COD_PRE,COD_SUB,SEMANA "
	ENDSCAN
	sSQLSelectCmd = sSQLSelectCmd + " INTO CURSOR rsAllEventsRecordCount"
	&sSQLSelectCmd

	*Complementa los registros de conteos con la información descriptiva de UPGDs, MUNICIPIOS y DEPARTAMENTOS 
	SELECT rsAllEventsRecordCount.COD_PRE,rsAllEventsRecordCount.COD_SUB,rsAllEventsRecordCount.SEMANA, ;
		rsAllEventsRecordCount.NOTIFICACION_EXISTENTE, ;
		ALLTRIM(UPGD.RAZ_SOC) AS RAZ_SOC,ALLTRIM(UPGD.RES_NOT) + ', Tel: ' + ALLTRIM(UPGD.TEL) AS RES_NOTI, ;
		IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA_, ;
		UPGD.FEC_CAR, MUNICIPIOS.COD_MUN, MUNICIPIOS.NOM_MUN, DEPTOS.COD_DEP, DEPTOS.NOM_DEP FROM ;
		rsAllEventsRecordCount LEFT OUTER JOIN UPGD ON ;
		rsAllEventsRecordCount.COD_PRE=UPGD.COD_PRE AND rsAllEventsRecordCount.COD_SUB=UPGD.COD_SUB ;
		LEFT OUTER JOIN MUNICIPIOS ON LEFT(rsAllEventsRecordCount.COD_PRE,5) = MUNICIPIOS.COD_MUN ;
		LEFT OUTER JOIN DEPTOS ON LEFT(rsAllEventsRecordCount.COD_PRE,2) = DEPTOS.COD_DEP ;
		GROUP BY rsAllEventsRecordCount.COD_PRE,rsAllEventsRecordCount.COD_SUB,rsAllEventsRecordCount.SEMANA ;
		INTO CURSOR rsNotifiedWeeks READWRITE
	SELECT rsNotifiedWeeks	
	sSourceReportCursor = "rsNotifiedWeeks" 


	*Determina para cada UPGD en qué semanas NO existen eventos notificados con base en el recordset de eventos notificados;
	por semana (en 6 pasos)
	
	*Paso 1: Establece el número total de semanas correspondientes al año objetivo
	sSelectSQLCmd = "SELECT PADL((SEMANA ),2,'0') as SEMANA FROM CALENDARIO WHERE VIGENCIA = '" + ;
					sTargetYear + "' AND BETWEEN(SEMANA," +	This.sInitWeek  + "," + This.sEndWeek  + ")" + ;
					"INTO CURSOR rsEpidemiologicalWeeks"
	&sSelectSQLCmd

	*Paso 2: Ejecuta el producto cartesiano entre el total de semanas de la vigencia y las semanas con eventos notificados
	SELECT RsAllEventsRecordcount.*,rsEpidemiologicalWeeks.* FROM RsAllEventsRecordcount,rsEpidemiologicalWeeks ;
		INTO CURSOR rsCrossProduct

	*Paso 3: Establece en qué semanas NO se produjo notificación
	SELECT DISTINCT rsCrossProduct.COD_PRE,rsCrossProduct.COD_SUB,rsCrossProduct.semana_b AS SEMANA FROM ;
		rsCrossProduct WHERE rsCrossProduct.COD_PRE + rsCrossProduct.COD_SUB + rsCrossProduct.semana_b ;
		NOT IN (SELECT DISTINCT COD_PRE + COD_SUB + SEMANA FROM rsAllEventsRecordCount)  AND &sUPGDsFilter ;
		INTO CURSOR rsNotNotifiedWeeks READWRITE

	*Paso 4: agrega al recordset rsNotNotifiedWeeks recien creado, las UPGDs que se encuentran activas en el Sistema y ;
	que no han tenido notificación alguna para el período considerado. Esto obedece al hecho de que, hasta este punto, ;
	se ha consultado los datos de notificaciones existentes en las tablas PACIENTE, BROTES y tablas de eventos ;
	tipo seguimiento o encuesta; sin embargo, cabe la posibilidad de que existan UPGDs activas que nunca hayan ;
	notificado y deben aparecer, en consecuencia, silenciosas para todo el período considerado
	INSERT INTO rsNotNotifiedWeeks SELECT COD_PRE, COD_SUB, rsEpidemiologicalWeeks.SEMANA ;
		 FROM UPGD, rsEpidemiologicalWeeks  WHERE ACT_SIV=1 AND &sUPGDsFilter AND COD_PRE + COD_SUB ;
		 NOT IN(SELECT rsAllEventsRecordCount.COD_PRE+rsAllEventsRecordCount.COD_SUB FROM rsAllEventsRecordCount)

	*Paso 5: Establece la fuente de datos para el reporte
	SELECT rsNotNotifiedWeeks.COD_PRE,rsNotNotifiedWeeks.COD_SUB,rsNotNotifiedWeeks.SEMANA, ;
		-1 AS NOTIFICACION_EXISTENTE, ;
		ALLTRIM(UPGD.RAZ_SOC) AS RAZ_SOC,ALLTRIM(UPGD.RES_NOT) + ', Tel: ' + ALLTRIM(UPGD.TEL) AS RES_NOTI, ;
		IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA_, ; 
		UPGD.FEC_CAR, MUNICIPIOS.COD_MUN, MUNICIPIOS.NOM_MUN, DEPTOS.COD_DEP, DEPTOS.NOM_DEP FROM ;
		rsNotNotifiedWeeks LEFT OUTER JOIN UPGD ON ;
		rsNotNotifiedWeeks.COD_PRE=UPGD.COD_PRE AND rsNotNotifiedWeeks.COD_SUB=UPGD.COD_SUB ;
		LEFT OUTER JOIN MUNICIPIOS ON LEFT(rsNotNotifiedWeeks .COD_PRE,5) = MUNICIPIOS.COD_MUN ;
		LEFT OUTER JOIN DEPTOS ON LEFT(rsNotNotifiedWeeks .COD_PRE,2) = DEPTOS.COD_DEP ;
		GROUP BY rsNotNotifiedWeeks.COD_PRE,rsNotNotifiedWeeks.COD_SUB,rsNotNotifiedWeeks.SEMANA ;
		INTO CURSOR rsNOTNotifiedWeeksSpec READWRITE

	SELECT rsNOTNotifiedWeeksSpec	
	
	*Paso 6: Consolida los registros de semanas notificadas y no notificadas
	SELECT * FROM rsNotifiedWeeks UNION SELECT * FROM rsNOTNotifiedWeeksSpec INTO CURSOR sSourceReportCursor READWRITE
 
	IF RECCOUNT()=0
		This.sErrorMsg ='No se encontraron datos para producir el reporte'
	ELSE

		*Establece si las notificaciones encontradas por semana fueron realizadas a tiempo o no
		
		*Establece el listado total de notificaciones
		*Considera los registros de datos básicos de eventos individuales
		sSQLSelectCmd = "SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA,FechaCarga FROM PACIENTE WHERE " + sWhereClause + " UNION "
		*Agrega los registros de Brotes				
		sSQLSelectCmd = sSQLSelectCmd + "SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA,FechaCarga FROM BROTES WHERE " + ;
						sWhereClause
		*Agrega los registros de los eventos tipo seguimiento
		DO EventosConTablaDatosIndependiente.QPR
		SELECT DISTINCT "EVENTOS_" + PADL(FORMULARIO,2,'00') AS NombreTablaEventoSeguimiento FROM rsEventosConTablaDatosIndependiente WHERE ;
			!(rsEventosConTablaDatosIndependiente.FORMULARIO $ This.sExcludedTableEvents) INTO CURSOR rsFollowupEvents
		SELECT rsFollowupEvents
		SCAN
			sSQLSelectCmd = sSQLSelectCmd + "UNION SELECT COD_PRE,COD_SUB,PADL(ALLTRIM(SEMANA),2,'0') AS SEMANA,FechaCarga FROM " + ;
							rsFollowupEvents.NombreTablaEventoSeguimiento +;
							" WHERE  " + sWhereClause
		ENDSCAN
		sSQLSelectCmd = sSQLSelectCmd + " INTO CURSOR rsAllNotifications"
		&sSQLSelectCmd
		

		*Establece las fechas límite aceptables de notificación por semana epidemiológica
		sSelectSQLCmd = "SELECT PADL((SEMANA -1),2,'0') as SEMANA, DESDE, HASTA FROM CALENDARIO WHERE VIGENCIA = '" + ;
						sTargetYear + "' AND BETWEEN(SEMANA," +	ALLTRIM(STR(VAL(This.sInitWeek)+1))  + "," + ALLTRIM(STR(VAL(This.sEndWeek)+1)) + ")" + ;
						"INTO CURSOR rsEpidemiologicalWeeks READWRITE"
		&sSelectSQLCmd
		INDEX ON SEMANA TAG SEMANA_IDX
		
		*Determina si las notificaciones por UPGD fueron realizas a tiempo o no. Las semanas en las cuales no hubo notificación de ninguna;
		naturaleza, que ya se encuentran establecidas en sSourceReportCursor, son automáticamente semanas en donde la UPGD no cumplió
		SELECT sSourceReportCursor 
		SET RELATION TO SEMANA INTO rsEpidemiologicalWeeks
		SCAN
			IF sSourceReportCursor.NOTIFICACION_EXISTENTE = 1 THEN
				*Se encontró una notificación para la semana actual -sSourceReportCursor.SEMANA; por tanto, debe establecerse si tal notificación;
				fue o no realizada a tiempo teniendo en cuenta los límites de fehcas determinado por el Calendario epidemiológico.
				sSelectSQLCmd = "SELECT FechaCarga FROM  rsAllNotifications WHERE COD_PRE ='" + sSourceReportCursor.COD_PRE +;
								"' AND COD_SUB ='" + sSourceReportCursor.COD_SUB + "' and SEMANA=='" + sSourceReportCursor.SEMANA +;
								"' AND BETWEEN(FechaCarga,CTOD('" + DTOC(rsEpidemiologicalWeeks.DESDE) +  "'),CTOD('" +;
								DTOC(rsEpidemiologicalWeeks.HASTA) +  "')) INTO ARRAY aOnTimeNotifications"
				&sSelectSQLCmd 			
				IF _TALLY = 0 THEN
					REPLACE sSourceReportCursor.NOTIFICACION_EXISTENTE WITH -1
				ENDIF
			ENDIF
		ENDSCAN
		
		*Crea una tabla temporal que servirá para almacenar los resultados del reporte y exportarlos hacia un archivo excel
		sWeekFieldNamesToCreate=''
		FOR nFieldToCreate=1 TO MAX_EPIDEMIOLOGICAL_WEEKS &&VAL(This.sEndWeek)
		   sWeekFieldNamesToCreate = sWeekFieldNamesToCreate + 'S' + TRANSFORM(nFieldToCreate,'@L 99') + ' C(1), '
		ENDFOR
		sWeekFieldNamesToCreate = SUBSTR(sWeekFieldNamesToCreate,1,LEN(sWeekFieldNamesToCreate)-2)
		CREATE TABLE XLSWeekNotificationResultsTable FREE (COD_DEP c(2) NULL, NOM_DEP C(60) NULL, COD_MUN c(5) NULL, NOM_MUN C(60) NULL,;
				cod_pre c(10), cod_sub c(2), raz_soc c(120) NULL, ;
				resp_noti c(100) NULL, FEC_CAR D NULL, ACTIVA_ C(13) NULL, N_SEM_INC N(3), N_SEM_CUM N(3), ;
				PORCEN_INC N(7,3), PORCEN_CUM N(7,3), &sWeekFieldNamesToCreate)
				*RIESGO_SIL N(1), RIESGO_NEG N(1), &sWeekFieldNamesToCreate)
		INDEX ON cod_pre+cod_sub TAG UPGD_IDX
		
		*Almacena en la tabla temporal recien creada, los resultados en formato apropiado para ser mostrados en el reporte: ;
		Las semanas en donde hubo cumplimiento aparecerán marcadas con un signo '*' ;
		Las semanas en donde hubo incumplimiento aparecerán marcadas con un vacío
		SELECT sSourceReportCursor
		SCAN
			sWeekFieldNameToCheck = 'S' + TRANSFORM(SEMANA,'@l 99')
			SELECT XLSWeekNotificationResultsTable
			SEEK sSourceReportCursor.COD_PRE + sSourceReportCursor.COD_SUB
			IF !FOUND()
				INSERT INTO XLSWeekNotificationResultsTable (COD_DEP, NOM_DEP, COD_MUN, NOM_MUN, ;
					cod_pre, cod_sub, raz_soc, resp_noti, FEC_CAR , ACTIVA_) VALUES ;
					(sSourceReportCursor.COD_DEP,sSourceReportCursor.NOM_DEP,sSourceReportCursor.COD_MUN, ;
					sSourceReportCursor.NOM_MUN, sSourceReportCursor.COD_PRE,sSourceReportCursor.COD_SUB,;
					sSourceReportCursor.RAZ_SOC, sSourceReportCursor.RES_NOTI, sSourceReportCursor.FEC_CAR, ;
					sSourceReportCursor.ACTIVA_ )
			ENDIF
			REPLACE &sWeekFieldNameToCheck  WITH IIF(sSourceReportCursor.NOTIFICACION_EXISTENTE = 1,'*','')
			
			*Actualiza parcialmente las columans de porcentaje de semanas con notificación positiva, negativa y silenciosa
			REPLACE N_SEM_INC WITH N_SEM_INC + IIF(sSourceReportCursor.NOTIFICACION_EXISTENTE = -1,1,0)
			REPLACE N_SEM_CUM WITH N_SEM_CUM + IIF(sSourceReportCursor.NOTIFICACION_EXISTENTE = 1,1,0)
		ENDSCAN
		
		SELECT XLSWeekNotificationResultsTable
		nTotalweeks=VAL(This.sEndWeek)-VAL(This.sInitWeek)+1

		*Determina para cada UPGD si se encentra en alto riesgo por silencio epidemiológico o notificación negativa
		*REPLACE RIESGO_SIL WITH 1 FOR N_SEM_SIL > CEILING((nTotalweeks * HIGH_RISK_PERCENTAGE)/100)
		*REPLACE RIESGO_NEG WITH 1 FOR N_SEM_NEG > CEILING((nTotalweeks * HIGH_RISK_PERCENTAGE)/100)

		*Actualiza en forma definitiva las columans de porcentaje de semanas con notificación positiva, negativa y silenciosa
		REPLACE PORCEN_INC WITH N_SEM_INC/nTotalweeks, PORCEN_CUM WITH N_SEM_CUM/nTotalweeks ALL
		  
		*Marca en la tabla temporal las semanas que no se encuentran en el alcance del informe con el caracter CHR(149)-Un punto
		sFieldsToSetToNA = ''
		FOR nFieldToSetToNA=VAL(This.sEndWeek)+1  TO MAX_EPIDEMIOLOGICAL_WEEKS 
			sFieldsToSetToNA=sFieldsToSetToNA + 'S' + TRANSFORM(nFieldToSetToNA,'@L 99') + ' WITH CHR(149), '
		NEXT nFieldToSetToNA
		sFieldsToSetToNA = LEFT(sFieldsToSetToNA,LEN(sFieldsToSetToNA)-2)
		SELECT XLSWeekNotificationResultsTable 
		IF !EMPTY(sFieldsToSetToNA ) THEN
			sReplaceCmd="REPLACE " + sFieldsToSetToNA + " ALL"
			&sReplaceCmd
		ENDIF
		
		*Ejecuta el reporte
		This.sReportTitle='CUMPLIMIENTO EN LA NOTIFICACIÓN SEMANAL' + ;
							This.sInitWeek + ' - ' + This.sEndWeek

		SELECT XLSWeekNotificationResultsTable 
		SET ORDER TO TAG UPGD_IDX
		sRunReportCmd="REPORT FORM " + This.sReportsPath + FULFILLMENT_REPORT_UPGD_FILENAME + " TO PRINTER PROMPT PREVIEW "
		&sRunReportCmd
		
		*exporta los resultados de la tabla temporal hacia un archivo excel
		This.exportToXLS("XLSWeekNotificationResultsTable", "REPORTE DE OPORTUNIDAD EN LA RECEPCIÓN DE NOTIFICACIÓN")
	ENDIF
	ON KEY LABEL ESC &sOldOnEsc
	SET TALK &sOldSetTalk
ENDPROC


PROCEDURE makeInternalControlEventsReport(sTargetYear as Number, sPrefixFileName as String)

	*Elabora un reporte excel de notificación de casos ocurridos en el año sTargetYear y notificados desde la semana This.sInitWeek ;
	hasta la semana This.sEndWeek  para los eventos en eliminación y control internacional según se encuentran definidos en Tables.Eventos. 
	
	*El archivo excel resultante quedará guardado en la carpeta This.sXlsFilePath con el nombre ;
	 sPrefixFileName + "_" + "Erradicacion_Control_Internal_" + sTargetYear + "_Semana_" +_This.sInitWeek + "a" +  This.sEndWeek
	 

	sOldOnEsc=ON("ESCAPE")
	ON KEY LABEL ESC
		
	sUserMessage='Procesando datos...'
	WAIT WINDOW (sUserMessage) NOWAIT

	IF VARTYPE(sPrefixFileName)!='C' THEN
		sPrefixFileName = ''
	ELSE
		sPrefixFileName = sPrefixFileName + '_'
	ENDIF

	*Determina en qué semanas existen eventos notificados

	*sSQLSelectCmd = "SELECT VIGENCIA,SEMANA,DESDE,HASTA FROM CALENDARIO WHERE VIGENCIA='" + sTargetYear + ;
					 "' AND BETWEEN(SEMANA," + This.sInitWeek + "," + This.sEndWeek  + ") INTO ARRAY aCalendarWeeks"
	*&sSQLSelectCmd
	*nLastWeekRow = ALEN(aCalendarWeeks,1)
	
	*Considera los registros de datos básicos de eventos individuales que no sean ajustes
	*sWhereClause = "PACIENTE.AJUSTE='0' AND YEAR(PACIENTE.FEC_AJU)=" + sTargetYear + " AND (BETWEEN(PACIENTE.NOTIFICA," + This.sInitWeek + "," +  This.sEndWeek + ")" + ;
					" OR ((BETWEEN(PACIENTE.FechaCarga,CTOD('" + DTOC(aCalendarWeeks[1,3]) + "'),CTOD('" + DTOC(aCalendarWeeks[nLastWeekRow,4]) + "'))" +;
					" OR (BETWEEN(PACIENTE.FEC_AJU,CTOD('" + DTOC(aCalendarWeeks[1,3]) + "'),CTOD('" + DTOC(aCalendarWeeks[nLastWeekRow,4]) + "'))))))"
	sWhereClause = "PACIENTE.AJUSTE='0' AND YEAR(PACIENTE.FEC_AJU)=" + sTargetYear + " AND BETWEEN(PACIENTE.NOTIFICA," + This.sInitWeek + "," +  This.sEndWeek + ")"
	sSQLSelectCmd = "SELECT PACIENTE.COD_EVE,PACIENTE.NOTIFICA,PACIENTE.FEC_AJU,EVENTOS.TIP_NOT,tbAgrupacioneventos.IDGrupo,tbAgrupacionEventos.Label, " +;
					" 1 AS CASOS FROM PACIENTE INNER JOIN EVENTOS ON PACIENTE.COD_EVE=EVENTOS.COD_EVE " +;
					" INNER JOIN tbAgrupacionEventos ON EVENTOS.COD_EVE=tbAgrupacionEventos.COD_EVE WHERE " + ;
					" EVENTOS.CONTROL AND tbAgrupacionEventos.ACTIVO AND " + sWhereClause + " INTO CURSOR rsIndividualNotifications READWRITE"
	&sSQLSelectCmd

	*Agrega los registros de eventos colectivos  que no sean ajustes
	sWhereClause = "BROTES.AJUSTE='0' AND YEAR(BROTES.FEC_AJU)=" + sTargetYear + " AND BETWEEN(BROTES.NOTIFICA," + This.sInitWeek + "," +  This.sEndWeek + ")"
	sSQLSelectCmd = "SELECT BROTES.COD_EVE,BROTES.NOTIFICA,BROTES.FEC_AJU,EVENTOS.TIP_NOT,tbAgrupacioneventos.IDGrupo,tbAgrupacionEventos.Label, " +;
					" VAL(BROTES.CAS_CONL) + VAL(BROTES.CAS_CONC) + VAL(BROTES.CAS_CONN) AS CASOS FROM BROTES INNER JOIN EVENTOS ON BROTES.COD_EVE=EVENTOS.COD_EVE " +;
					" INNER JOIN tbAgrupacionEventos ON EVENTOS.COD_EVE=tbAgrupacionEventos.COD_EVE WHERE " + ;
					" EVENTOS.CONTROL AND tbAgrupacionEventos.ACTIVO AND " + sWhereClause + " INTO CURSOR rsOutbreaksNotifications READWRITE"
	&sSQLSelectCmd
	
	*Consolida un solo recordset(rsOutbreaksNotifications) con las notificaciones individuales y de brotes
	INSERT INTO rsOutbreaksNotifications SELECT * FROM rsIndividualNotifications 

	*Agrega a las notificaciones consolidadas los registros necesarios que den cuenta de la no tenencia de casos para ciertos eventos. Esto es necesario, ;
	por cuanto puede suceder que para el intervalo de tiempo determinado por (This.sInitWeek, This.sEndWeek), se tenga cero (0) casos para uno o más ;
	eventos y para éstos, la celda correspondiente a la fila SEMANA con columna LABEL en el reporte debe aparecer en ceros 
	SELECT tbAgrupacionEventos.*,EVENTOS.TIP_NOT FROM tbAgrupacionEventos INNER JOIN EVENTOS ON tbAgrupacionEventos.COD_EVE=EVENTOS.COD_EVE ;
		WHERE tbAgrupacionEventos.ACTIVO AND tbAgrupacionEventos.COD_EVE NOT IN (SELECT DISTINCT rsOutbreaksNotifications.COD_EVE FROM rsOutbreaksNotifications) ;
		INTO CURSOR rsEventsWithoutCases NOFILTER
	IF _TALLy > 0 THEN
		SELECT rsEventsWithoutCases 
		nTargetWeeks = ABS(VAL(This.sEndWeek)-VAL(This.sInitWeek)) + 1
		nInitWeek = VAL(This.sInitWeek)
		nEndWeek =  VAL(This.sEndWeek)
		FOR nWeek = nInitWeek  TO nEndWeek 
			 SELECT COD_EVE,nWeek,CTOD(''),TIP_NOT,IDGrupo,LABEL,0 AS CASOS FROM rsEventsWithoutCases INTO ARRAY aEventsWithoutCases 
			 INSERT INTO rsOutbreaksNotifications FROM ARRAY aEventsWithoutCases 
		ENDFOR
	ENDIF	
	
	*Construye una tabla de referencias cruzadas con base en las notificaciones consolidadas
	SELECT rsOutbreaksNotifications.Notifica as SEMANA, rsOutbreaksNotifications.Label, SUM(rsOutbreaksNotifications.Casos);
    FROM rsOutbreaksNotifications GROUP BY rsOutbreaksNotifications.Notifica, rsOutbreaksNotifications.Label;
    ORDER BY rsOutbreaksNotifications.Notifica, rsOutbreaksNotifications.Label INTO CURSOR SYS(2015)
	
	sDoCmd = "DO '" + PATH_TO_VFP_ADD_ONS + "VFPXTAB.PRG' WITH 'rsReport'"
	&sDoCmd
	
	*To do: en la actual implementación hace falta considerar el caso en el que todos los eventos de la tabla tbAgrupacionEventos han tenido ;
	casos pero hay una semana del período determinado por (This.sInitWeek, This.sEndWeek) en el que no ocurrió ninguno de los eventos. En esta situación, ;
	la actual implementación no muestra esa semana en el reporte y ella debería sair con cero en todos los eventos
	
	*Exporta el recordset de notificaciones consolidadas hacia un archivo excel
	oDataExporter = NEWOBJECT("DataExporter","DataExporter.Prg",.null.,'rsReport')
	oDataExporter.sExportationPath = This.sXlsFilePath
	oDataExporter.sExportedFileName = sPrefixFileName + "Erradicacion_Control_Internal_" + sTargetYear + "_Semana_" + This.sInitWeek + "a" +  This.sEndWeek
	oDataExporter.exportToXLS()

	IF oDataExporter.nInitRecs>0 THEN
		oDataExporter.ShowXLSExportationResults()
	ELSE
		WAIT  "No se encontraron registros para exportar" WINDOW NOWAIT TIMEOUT 5
	ENDIF

	RELEASE oDataExporter

	ON KEY LABEL ESC &sOldOnEsc

ENDPROC


PROCEDURE makeNHSN_NNISReport(sNotificationsXLSFileNameAndPath as String,  sNHSN_NNIS_XLSFileNameAndPath as String, sBasePivotTable as String)

	LOCAL oExcel

	oExcel = NewObject("AutomationServer", "AutomationServer.prg", "",  "Excel.Application", .T.)
		
	IF VarType(oExcel.oServer)<>"O"
		This.sErrorMsg = ERR_CREATING_EXCEL_AUTOMATION_SERVER_MESSSAGE
	ELSE
		oExcel.makeMeVisible()
		
		*Abre el archivo excel de notificaciones
		oExcel.OpenXLSFile(sNotificationsXLSFileNameAndPath)
		
		IF EMPTY(oExcel.sErrorMsg) THEN
		
			*Abre el archivo excel que contiene el reporte predefinido basado en el índice NHSN
			oExcel.OpenXLSFile(sNHSN_NNIS_XLSFileNameAndPath)
			
			IF EMPTY(oExcel.sErrorMsg) THEN
				*Actualiza los datos fuente y la tabla dinámica contenida en el archivo excel que contiene el reporte predefinido
				
				*1: Limpia los datos fuente de la tabla dinámica
				oExcel.CleanWorksheet(JUSTFNAME(sNHSN_NNIS_XLSFileNameAndPath), 'DatosNotificacion')

				*2: Copia los datos de notificaciones hacia la fuente de la tabla dinámica
				oExcel.CopyCells(JUSTFNAME(sNotificationsXLSFileNameAndPath),1,JUSTFNAME(sNHSN_NNIS_XLSFileNameAndPath),'DatosNotificacion')
				oExcel.CloseXLSFile(JUSTFNAME(sNotificationsXLSFileNameAndPath))
				
				*oExcel.makeMeVisible()
				
				*3: Actualiza la tabla dinámica
				oExcel.NameRange(JUSTFNAME(sNHSN_NNIS_XLSFileNameAndPath),'DatosNotificacion','Notificaciones')
				oExcel.UpdatePivotTable(JUSTFNAME(sNHSN_NNIS_XLSFileNameAndPath),'Estadisticas',sBasePivotTable)
				
				oExcel.makeMeInvisible()
				oExcel.CloseXLSFile(JUSTFNAME(sNHSN_NNIS_XLSFileNameAndPath),.T., ADDBS(This.sXlsFilePath) + JUSTFNAME(sNHSN_NNIS_XLSFileNameAndPath))
			ELSE
				This.sErrorMsg = oExcel.sErrorMsg
			ENDIF
		ELSE
			This.sErrorMsg = oExcel.sErrorMsg
		ENDIF
	ENDIF

	RELEASE oExcel		
ENDPROC


PROCEDURE makeNegativeNotificationReport(sTargetYear as Number, sPrefixFileName as String)

	*Elabora un reporte excel de notificación negativa de casos ocurridos en el año sTargetYear y notificados para la semana This.sInitWeek ;
	para los eventos que admiten ser notificados negativamente según se encuentran configurados en Tables.Eventos. 
	
	*El archivo excel resultante quedará guardado en la carpeta This.sXlsFilePath con el nombre ;
	sPrefixFileName + "_" + "Eventos_Notificacion_Negativa_" + sTargetYear + "_Semana_" +_This.sInitWeek
	 

	sOldOnEsc=ON("ESCAPE")
	ON KEY LABEL ESC
		
	sUserMessage='Procesando datos...'
	WAIT WINDOW (sUserMessage) NOWAIT

	IF VARTYPE(sPrefixFileName)!='C' THEN
		sPrefixFileName = ''
	ELSE
		sPrefixFileName = sPrefixFileName + '_'
	ENDIF

	*Determina en qué semanas existen eventos notificados

	*sSQLSelectCmd = "SELECT VIGENCIA,SEMANA,DESDE,HASTA FROM CALENDARIO WHERE VIGENCIA='" + sTargetYear + ;
					 "' AND BETWEEN(SEMANA," + This.sInitWeek + "," + This.sEndWeek  + ") INTO ARRAY aCalendarWeeks"
	*&sSQLSelectCmd
	*nLastWeekRow = ALEN(aCalendarWeeks,1)
	
	*Considera los registros de datos básicos de eventos individuales que no sean ajustes
	*sWhereClause = "PACIENTE.AJUSTE='0' AND YEAR(PACIENTE.FEC_AJU)=" + sTargetYear + " AND (BETWEEN(PACIENTE.NOTIFICA," + This.sInitWeek + "," +  This.sEndWeek + ")" + ;
					" OR ((BETWEEN(PACIENTE.FechaCarga,CTOD('" + DTOC(aCalendarWeeks[1,3]) + "'),CTOD('" + DTOC(aCalendarWeeks[nLastWeekRow,4]) + "'))" +;
					" OR (BETWEEN(PACIENTE.FEC_AJU,CTOD('" + DTOC(aCalendarWeeks[1,3]) + "'),CTOD('" + DTOC(aCalendarWeeks[nLastWeekRow,4]) + "'))))))"
	sWhereClause = "PACIENTE.AJUSTE='0' AND YEAR(PACIENTE.FEC_AJU)=" + sTargetYear + " AND PACIENTE.NOTIFICA=0"
	sSQLSelectCmd = "SELECT PACIENTE.COD_PRE, PACIENTE.COD_SUB, PACIENTE.COD_EVE, " +;
					"COUNT(*) AS CASOS FROM PACIENTE INNER JOIN EVENTOS ON PACIENTE.COD_EVE=EVENTOS.COD_EVE " +;
					" WHERE EVENTOS.ADMITE_NOTIF_NEGATIVA AND " + sWhereClause + " GROUP BY COD_PRE,COD_SUB,PACIENTE.COD_EVE UNION "

	*Agrega los registros de eventos colectivos  que no sean ajustes
	sWhereClause = "BROTES.AJUSTE='0' AND YEAR(BROTES.FEC_AJU)=" + sTargetYear + " AND BROTES.NOTIFICA=0"
	sSQLSelectCmd = sSQLSelectCmd  + "SELECT BROTES.COD_PRE, BROTES.COD_SUB, BROTES.COD_EVE, " +;
					"SUM(VAL(BROTES.CAS_CONL) + VAL(BROTES.CAS_CONC) + VAL(BROTES.CAS_CONN)) AS CASOS " +;
					" FROM BROTES INNER JOIN EVENTOS ON BROTES.COD_EVE=EVENTOS.COD_EVE WHERE " + ;
					" EVENTOS.ADMITE_NOTIF_NEGATIVA AND " + sWhereClause + " GROUP BY COD_PRE,COD_SUB,BROTES.COD_EVE "
	sSQLSelectCmd = sSQLSelectCmd + " INTO CURSOR rsAllEventsRecordCount"
	&sSQLSelectCmd

	*Complementa los registros de conteos con la información descriptiva de UPGDs, MUNICIPIOS y DEPARTAMENTOS 
	SELECT rsAllEventsRecordCount.COD_PRE,rsAllEventsRecordCount.COD_SUB, rsAllEventsRecordCount.COD_EVE, rsAllEventsRecordCount.CASOS, ;
		ALLTRIM(UPGD.RAZ_SOC) AS RAZ_SOC,ALLTRIM(UPGD.RES_NOT) + ', Tel: ' + ALLTRIM(UPGD.TEL) AS RES_NOTI, ;
		IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA_, ;
		UPGD.FEC_CAR, MUNICIPIOS.COD_MUN, MUNICIPIOS.NOM_MUN, DEPTOS.COD_DEP, DEPTOS.NOM_DEP FROM ;
		rsAllEventsRecordCount LEFT OUTER JOIN UPGD ON ;
		rsAllEventsRecordCount.COD_PRE=UPGD.COD_PRE AND rsAllEventsRecordCount.COD_SUB=UPGD.COD_SUB ;
		LEFT OUTER JOIN MUNICIPIOS ON LEFT(rsAllEventsRecordCount.COD_PRE,5) = MUNICIPIOS.COD_MUN ;
		LEFT OUTER JOIN DEPTOS ON LEFT(rsAllEventsRecordCount.COD_PRE,2) = DEPTOS.COD_DEP ;
		INTO CURSOR rsNotifiedCases READWRITE
	sSourceReportCursor = "rsNotifiedCases" 

	*Determina para cada UPGD en qué semanas NO existen eventos notificados con base en el recordset de eventos notificados;
	por semana (en 6 pasos)
	
	*Paso 1: Establece el número total de eventos que admiten notificación negativa
	sSelectSQLCmd = "SELECT COD_EVE FROM EVENTOS WHERE EVENTOS.ADMITE_NOTIF_NEGATIVA INTO CURSOR rsTargetEvents"
	&sSelectSQLCmd

	*Paso 2: Ejecuta el producto cartesiano entre los eventos objetivo y las noificaciones encontradas
	SELECT RsAllEventsRecordcount.*,rsTargetEvents.* FROM RsAllEventsRecordcount,rsTargetEvents ;
		INTO CURSOR rsCrossProduct

	*Paso 3: Establece para qué eventos NO se encontró notificación
	SELECT DISTINCT rsCrossProduct.COD_PRE,rsCrossProduct.COD_SUB,rsCrossProduct.cod_eve_b AS COD_EVE FROM ;
		rsCrossProduct WHERE rsCrossProduct.COD_PRE + rsCrossProduct.COD_SUB + rsCrossProduct.cod_eve_b ;
		NOT IN (SELECT DISTINCT COD_PRE + COD_SUB + COD_EVE FROM rsAllEventsRecordCount) ;
		INTO CURSOR rsNotNotifiedCases READWRITE

	*Paso 4: agrega al recordset rsNotNotifiedCases recien creado, las UPGDs que se encuentran activas en el Sistema y ;
	que no han tenido notificación alguna para el período considerado. Esto obedece al hecho de que, hasta este punto, ;
	se ha consultado los datos de notificaciones existentes en las tablas PACIENTE y BROTES ;
	sin embargo, cabe la posibilidad de que existan UPGDs activas que no hayan notificado nada;
	y deben aparecer, en consecuencia, silenciosas para los eventos con notificación negativa
	INSERT INTO rsNotNotifiedCases SELECT COD_PRE, COD_SUB, rsTargetEvents.COD_EVE ;
		 FROM UPGD, rsTargetEvents WHERE ACT_SIV=1 AND COD_PRE + COD_SUB ;
		 NOT IN(SELECT rsAllEventsRecordCount.COD_PRE+rsAllEventsRecordCount.COD_SUB FROM rsAllEventsRecordCount)

	*Paso 5: Complementa los registros de conteos con la información descriptiva de UPGDs, MUNICIPIOS y DEPARTAMENTOS 
	SELECT rsNotNotifiedCases.COD_PRE,rsNotNotifiedCases.COD_SUB,rsNotNotifiedCases.COD_EVE, 0 AS CASOS, ;
		ALLTRIM(UPGD.RAZ_SOC) AS RAZ_SOC,ALLTRIM(UPGD.RES_NOT) + ', Tel: ' + ALLTRIM(UPGD.TEL) AS RES_NOTI, ;
		IIF(UPGD.ACT_SIV=1,'SÍ           ',IIF(UPGD.ACT_SIV=2,'NO           ','INDETERMINADO')) AS ACTIVA_, ; 
		UPGD.FEC_CAR, MUNICIPIOS.COD_MUN, MUNICIPIOS.NOM_MUN, DEPTOS.COD_DEP, DEPTOS.NOM_DEP FROM ;
		rsNotNotifiedCases LEFT OUTER JOIN UPGD ON ;
		rsNotNotifiedCases.COD_PRE=UPGD.COD_PRE AND rsNotNotifiedCases.COD_SUB=UPGD.COD_SUB ;
		LEFT OUTER JOIN MUNICIPIOS ON LEFT(rsNotNotifiedCases.COD_PRE,5) = MUNICIPIOS.COD_MUN ;
		LEFT OUTER JOIN DEPTOS ON LEFT(rsNotNotifiedCases.COD_PRE,2) = DEPTOS.COD_DEP ;
		INTO CURSOR rsNotNotifiedCasesSpec READWRITE

	*Paso 6: Consolida los registros de ocurrencia y no ocurrencia de eventos de notificación negativa
	SELECT * FROM rsNotifiedCases UNION SELECT * FROM rsNotNotifiedCasesSpec INTO CURSOR rsNegativeNotificationReport NOFILTER 
		
	*Exporta el recordset de notificaciones consolidadas hacia un archivo excel
	*oDataExporter = NEWOBJECT("DataExporter","DataExporter.Prg",.null.,'rsSourceReport')
	*oDataExporter.sExportationPath = This.sXlsFilePath
	*oDataExporter.sExportedFileName = sPrefixFileName + "Eventos_Notificacion_Negativa_" + sTargetYear + "_Semana_" + This.sInitWeek
	*oDataExporter.exportToXLS()

	*IF oDataExporter.nInitRecs>0 THEN
	*	oDataExporter.ShowXLSExportationResults()
	*ELSE
	*	WAIT  "No se encontraron registros para exportar" WINDOW NOWAIT TIMEOUT 5
	*ENDIF

	*RELEASE oDataExporter

	ON KEY LABEL ESC &sOldOnEsc

ENDPROC

PROCEDURE notificationFromQueryToXLS(sEventCode as string, sFilter as String, sDBSourceTbName as string, sDCSourceTbName as string, ;
									 bSelectDistinct as boolean, bVerbose AS Boolean, sXlsFileName as string, sXlsFileNameSuffix AS String,;
									 bProcessAdjustments as Boolean)

	*Elabora un reporte excel de notificación de casos del evento sEventCode que cumplan la condición sFilter a partir de la especificación;
	de exportación a excel que para él se encuentre en la tabla ExportQueries.

	*Si se suministra sDBSourceTbName, en el query de exportación extraído de ExportQueries, se sustituirá por PACIENTE; en forma similar, ;
	si se suministra sDCSourceTbName, en el query se sustituirá por el nombre de la tabla de datos complementarios del evento.
	*Si bSelectDistinct es .T., se utilizará SELECT DISTINCT sobre la fuente de datos en vez del SELECT simple que normalmente viene en las ;
	especificaciones de exportación.

	*Si bVerbose  es .T., se mostrarán mensajes al usuario indicándole del avance del proceso
	
	*El archivo excel resultante quedará guardado en la carpeta This.sXlsFilePath con el nombre sXlsFileName; en caso de que no se suministre este parámetro,;
	se asignará el nombre EVENTO '+ sEventCode + xlsFileNameSuffix y, en caso de que no se suministre el parámetro sXlsFileNameSuffix, éste se asignará ;
	como "DATOS BASICOS Y COMPLEMENTARIOS"
	 
	
	*Establece el valor de la variable pública gsXLSFilter con el objeto de que los pre-procesamientos que dependan de ella -según se;
	encuentren establecidos en tables.ExportQueries-, se puedan ejecutar
	PUBLIC gsXLSFilter as String
	gsXLSFilter = sFilter

	LOCAL oCurrentEvent as Object
	oCurrentEvent = NEWOBJECT("Event","Event.Prg",.NULL.,sEventCode)
	
	LOCAL oDataExporter AS Object
	oDataExporter = NEWOBJECT("DataExporter","DataExporter.Prg")

	DIMENSION aSivigilaVersion(1)
		
	bReplaceBasicDataSource = (VARTYPE(sDBSourceTbName)='C' AND !EMPTY(sDBSourceTbName)) 
	bReplaceComplementaryDataSource = (VARTYPE(sDCSourceTbName )='C' AND !EMPTY(sDCSourceTbName)) 
	
	IF VARTYPE(sXlsFileNameSuffix)!='C' OR EMPTY(sXlsFileNameSuffix) THEN 
		sXlsFileNameSuffix =  ' DATOS BASICOS Y COMPLEMENTARIOS'
	ENDIF
	
	SET PROCEDURE TO PlainsLib ADDITIVE
	
	IF bVerbose THEN 		
		WAIT WINDOW 'Procesando datos...' NOWAIT
	ENDIF

	*Garantiza que la exportación siempre se lleve a cabo de acuerdo con la última versión del sistema definida;
	por sus identificadores nMajor.nMinor.nBuild según se encuentran definidos en SIVIGILADownloader.ini
	oIniMgr = NEWOBJECT('IniMgr', 'IniMgr.fxp',.NULL.,'SIVIGILADownloader.ini',.T.)
	sSivigilaVersion = oIniMgr.GetValue("Main", "Version", 'SIVIGILADownloader.ini')
	= ALINES(aSivigilaVersion, sSivigilaVersion, 13, '.')
	RELEASE oIniMgr
	
	*Obtiene el query de exportación para el evento
	sLinkerExpr = ''
	sDCTable = ''
	XlsQuery = GetQryString("E" + oCurrentEvent.sFORMULARIO, @sDCTable, VAL(aSivigilaVersion[1]), VAL(aSivigilaVersion[2]), VAL(aSivigilaVersion[3]), .T., .T., @sLinkerExpr)
	
	IF !EMPTY(XlsQuery) THEN 
		*Reemplaza las fuentes de datos báicos y/o complementarios, si es necesario
		IF bReplaceBasicDataSource THEN 
			XlsQuery = STRTRAN(XlsQuery, 'PACIENTE', sDBSourceTbName,-1,-1,1)
		ENDIF
		IF bReplaceComplementaryDataSource THEN 
			XlsQuery = STRTRAN(XlsQuery, oCurrentEvent.sDC_TABLA, sDCSourceTbName,-1,-1,1)
		ENDIF
		
	ELSE
		*Usa un query de exportación por defecto	
		
		IF oCurrentEvent.hasComplementaryData() THEN 
			*Usa el query de exportación para DB + DC reemplazando las fuentes de datos básicos complementarios, si es necesario
			XlsQuery = GetQryString("DBDC", @sDCTable, VAL(aSivigilaVersion[1]), VAL(aSivigilaVersion[2]), VAL(aSivigilaVersion[3]), .T., .T., @sLinkerExpr)
			IF bReplaceComplementaryDataSource THEN 
				XlsQuery = STRTRAN(XlsQuery, 'sComplementaryDataTableName', sDCSourceTbName,-1,-1,1)
			ELSE
				XlsQuery = STRTRAN(XlsQuery, 'sComplementaryDataTableName', oCurrentEvent.sDC_TABLA,-1,-1,1)
			ENDIF
		ELSE
			*Usa el query de exportación para eventos que solo capturan DB 
			XlsQuery = GetQryString("DB", @sDCTable, VAL(aSivigilaVersion[1]), VAL(aSivigilaVersion[2]), VAL(aSivigilaVersion[3]), .T., .T., @sLinkerExpr)
		ENDIF 
		
		*Reemplaza las fuentes de datos báicos si es necesario
		IF bReplaceBasicDataSource THEN 
			XlsQuery = STRTRAN(XlsQuery, 'PACIENTE', sDBSourceTbName,-1,-1,1)
		ENDIF
		
	ENDIF

	IF bSelectDistinct THEN 
		XlsQuery = STRTRAN(XlsQuery, "SELECT", "SELECT DISTINCT",1,1,1)
	ENDIF
	
	*Establece las condiciones de filtrado para la extracción de registros
	sResultsTbName = ADDBS(SYS(2023)) + "tbTmp" + SYS(2015)
	sWHERE_SQL =' A.COD_EVE ="' + IIF(!EMPTY(ALLTRIM(sEventCode)),sEventCode,ALLTRIM(sEventCode)) +  '" AND ' + ;
				sFilter 

	IF OCCURS('#',XlsQuery) > 0  THEN 
		*El query de exportación tiene placeholder, por tanto es reemplazado por la condición WHERE 
		XlsQuery = STRTRAN(XlsQuery, '#', sWHERE_SQL )
	ELSE 
		XlsQuery =  XlsQuery + ' WHERE ' + sWHERE_SQL
	ENDIF 
	XlsQuery =  XlsQuery + ' INTO TABLE ' + sResultsTbName + ' DATABASE SIVIGILATemp'

	sOldDB = SET("Database")
	OPEN DATABASE SIVIGILATemp
	SET  DATABASE TO SIVIGILATemp
	TRY
		REMOVE TABLE tbTmpQryToXLS DELETE
	CATCH TO oException
	ENDTRY
	&XlsQuery 
	
	*Exporta el recordset de notificaciones consolidadas hacia un archivo excel
	oDataExporter.sSourceTableName = JUSTSTEM(sResultsTbName)
	oDataExporter.sExportationPath = This.sXlsFilePath
	IF VARTYPE(sXlsFileName)='C' AND !EMPTY(sXlsFileName) THEN 
		oDataExporter.sExportedFileName = sXlsFileName
	ELSE
		oDataExporter.sExportedFileName = 'EVENTO '+ oCurrentEvent.sCOD_EVE + sXlsFileNameSuffix 
	ENDIF
	oDataExporter.exportToXLS()

	IF bVerbose THEN 
		IF oDataExporter.nInitRecs>0 THEN
			oDataExporter.ShowXLSExportationResults()
		ELSE
			WAIT  "No se encontraron registros para exportar" WINDOW NOWAIT TIMEOUT 5
		ENDIF
	ENDIF

	TRY
		REMOVE TABLE tbTmpQryToXLS DELETE
	CATCH TO oException
	ENDTRY
		
	WAIT CLEAR

	RELEASE gsXLSFilter	
	RELEASE oDataExporter
	RELEASE oCurrentEvent 
	
ENDPROC


PROCEDURE launchMakeContactsReport(sEventCode as string, sFilter as string, nMajor as Number, nMinor as Number, nBuild as Number, ;
									sExcelQueryKey as String, sXlsFileNameSuffix AS String)

	SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		
	DO showProgressMessage WITH 'Generando reporte de contactos y seguimientos' IN SIVIGILAMessenger
	This.makeContactsReport(sEventCode, sFilter, nMajor, nMinor, nBuild, 'CNTA', sXlsFileNameSuffix) 
	
	IF !EMPTY(This.sErrorMsg) THEN
		=showErrorMessage(ERR_CREATING_CONTACTS_REPORT_MESSAGE,0)
	ENDIF

	*Genera un reporte especial con la información de contactos y seguimientos a contactos que no tienen coincidente en datos básicos
	This.makeContactsReport(sEventCode, 'COD_EVEPOS="' + sEventCode + '" AND A.COD_EVE IS NULL', nMajor, nMinor, nBuild, 'CNTH', ;
							'Sin_Datos_Basicos' + IIF(VARTYPE(sXlsFileNameSuffix)='C',sXlsFileNameSuffix,'') ) 

	DO hideProgressMessage IN SIVIGILAMessenger
	
	IF !EMPTY(This.sErrorMsg) THEN
		=showErrorMessage(ERR_CREATING_CONTACTS_REPORT_MESSAGE,0)
	ENDIF

ENDPROC 			


PROCEDURE makeContactsReport(sEventCode as string, sFilter as string, nMajor as Number, nMinor as Number, nBuild as Number, ;
							sExcelQueryKey as String, sXlsFileNameSuffix AS String)
							
	LOCAL sPrimaryTableName AS String,  sLinkerExpr AS String, sPostCmds AS String 
	STORE '' TO sPrimaryTableName
	STORE '' TO sLinkerExpr
	STORE '' TO sPostCmds
	
	sTmpTableName = This.sTmpPath + SYS(2015)
	
	XlsQuery = GetQryString(sExcelQueryKey, @sPrimaryTableName, nMajor, nMinor, nBuild, .T., .T., @sLinkerExpr, , , @sPostCmds)
	
	*Establece las condiciones de filtrado para la extracción de registros

	sWHERE_SQL = ' WHERE ' + sFilter + ' INTO TABLE ' + (sTmpTableName) + " DATABASE SIVIGILATemp"
	XlsQuery =  XlsQuery + sWHERE_SQL 
*SET STEP ON 
	* Se ejecuta el Query que extrae los datos para exportar a Excel
	sOldDB = SET("Database")
	OPEN DATABASE SIVIGILATemp
	SET  DATABASE TO SIVIGILATemp
	TRY
		REMOVE TABLE (tmpTable) DELETE
	CATCH TO oException
	ENDTRY
	&XlsQuery 

	*?'Inicio: ' + TIME()
	IF _TALLY > 0
		* se encontraron registros. 

		xlsFileName = '\EVENTO ' + ALLTRIM(sEventCode) + '_Contactos y seguimientos' 
		IF VARTYPE(sXlsFileNameSuffix) = 'C' THEN 
			IF !EMPTY(sXlsFileNameSuffix) THEN 
				xlsFileName = xlsFileName + '_' + sXlsFileNameSuffix
			ENDIF 
		ELSE
			sXlsFileNameSuffix = ''
		ENDIF

		oDataExporter=NEWOBJECT("DataExporter","DataExporter.fxp",.null.,JUSTSTEM(sTmpTableName))
		oDataExporter.sExportationPath = This.sXlsFilePath
		oDataExporter.sExportedFileName = xlsFileName 
		oDataExporter.exportToXLS(.T.)
		RELEASE oDataExporter					

		TRY
			REMOVE TABLE (sTmpTableName) DELETE
		CATCH TO oException
		ENDTRY
		SET DATABASE TO &sOldDB
	
		&&?'Final: ' + TIME()
		*Ejecuta post comandos de exportación si es que existen
        IF !EMPTY(sPostCmds) THEN
			&sPostCmds
		ENDIF

		RELEASE oDataExporter
		
		DO DeleteTable WITH JUSTSTEM(sTmpTableName), .T. IN TransferDataHandler			
	ENDIF

	TRY
		REMOVE TABLE (sTmpTableName) DELETE
	CATCH TO oException
	ENDTRY
	SET DATABASE TO &sOldDB

	&&?'Final: ' + TIME()

ENDPROC 

PROCEDURE entityFromQueryToXLS(sEntityKey as string, sFilter as String, sDBSourceTbName as string, sDCSourceTbName as string, ;
							 bSelectDistinct as boolean, bVerbose AS Boolean, sXlsFileName as string, sXlsFileNameSuffix AS String,;
							 bProcessAdjustments as Boolean)

	*Elabora un reporte excel de notificación de registros de la entidad sEntityKey  que cumplan la condición sFilter a partir de la especificación;
	de exportación a excel que para tal entidad se encuentre en la tabla ExportQueries (.Shortname).

	*Si se suministra sDBSourceTbName, en el query de exportación extraído de ExportQueries, se sustituirá por ExportQueries.Tablename; en forma similar, ;
	si se suministra sDCSourceTbName, en el query se sustituirá por el nombre de la tabla de datos complementarios asociada a la entidad.
	*Si bSelectDistinct es .T., se utilizará SELECT DISTINCT sobre la fuente de datos en vez del SELECT simple que normalmente viene en las ;
	especificaciones de exportación.

	*Si bVerbose  es .T., se mostrarán mensajes al usuario indicándole del avance del proceso
	
	*El archivo excel resultante quedará guardado en la carpeta This.sXlsFilePath con el nombre sXlsFileName; en caso de que no se suministre este parámetro,;
	se asignará el nombre '' + xlsFileNameSuffix y, en caso de que no se suministre el parámetro sXlsFileNameSuffix, éste se asignará ;
	como "CONTACTOS Y SEGUIMIENTOS"
	 
	

	LOCAL oDataExporter AS Object
	oDataExporter = NEWOBJECT("DataExporter","DataExporter.Prg")

	DIMENSION aSivigilaVersion(1)
		
	bReplaceBasicDataSource = (VARTYPE(sDBSourceTbName)='C' AND !EMPTY(sDBSourceTbName)) 
	bReplaceComplementaryDataSource = (VARTYPE(sDCSourceTbName )='C' AND !EMPTY(sDCSourceTbName)) 
	
	IF VARTYPE(sXlsFileNameSuffix)!='C' OR EMPTY(sXlsFileNameSuffix) THEN 
		sXlsFileNameSuffix =  ' CONTACTOS Y SEGUIMIENTOS'
	ENDIF
	
	SET PROCEDURE TO PlainsLib ADDITIVE
	
	IF bVerbose THEN 		
		WAIT WINDOW 'Procesando datos...' NOWAIT
	ENDIF

	*Garantiza que la exportación siempre se lleve a cabo de acuerdo con la última versión del sistema definida;
	por sus identificadores nMajor.nMinor.nBuild según se encuentran definidos en SIVIGILADownloader.ini
	oIniMgr = NEWOBJECT('IniMgr', 'IniMgr.fxp',.NULL.,'SIVIGILADownloader.ini',.T.)
	sSivigilaVersion = oIniMgr.GetValue("Main", "Version", 'SIVIGILADownloader.ini')
	= ALINES(aSivigilaVersion, sSivigilaVersion, 13, '.')
	RELEASE oIniMgr
	
	*Obtiene el query de exportación para la entidad
	sLinkerExpr = ''
	sDCTable = ''
	XlsQuery = GetQryString(sEntityKey, @sDCTable, VAL(aSivigilaVersion[1]), VAL(aSivigilaVersion[2]), VAL(aSivigilaVersion[3]), .T., .T., @sLinkerExpr)
	
	IF !EMPTY(XlsQuery) THEN 
		*Reemplaza las fuentes de datos báicos y/o complementarios, si es necesario
		IF bReplaceBasicDataSource THEN 
			XlsQuery = STRTRAN(XlsQuery, sDCTable, sDBSourceTbName,-1,-1,1)
		ENDIF
		IF bReplaceComplementaryDataSource THEN 
			XlsQuery = STRTRAN(XlsQuery, 'SEGUIMIENTOCONTACTOS', sDCSourceTbName,-1,-1,1)
		ENDIF

		IF bSelectDistinct THEN 
			XlsQuery = STRTRAN(XlsQuery, "SELECT", "SELECT DISTINCT",1,1,1)
		ENDIF
		
		*Establece las condiciones de filtrado para la extracción de registros
		sResultsTbName = ADDBS(SYS(2023)) + "tbTmp" + SYS(2015)
		sWHERE_SQL =' WHERE ' + sFilter + ' INTO TABLE ' + sResultsTbName + " DATABASE SIVIGILATemp"

		XlsQuery =  XlsQuery + sWHERE_SQL 
		sOldDB = SET("Database")
		OPEN DATABASE SIVIGILATemp
		SET  DATABASE TO SIVIGILATemp
		TRY
			REMOVE TABLE tbTmpQryToXLS DELETE
		CATCH TO oException
		ENDTRY
		&XlsQuery 
		
		*Exporta el recordset de notificaciones consolidadas hacia un archivo excel
		oDataExporter.sSourceTableName = JUSTSTEM(sResultsTbName)
		oDataExporter.sExportationPath = This.sXlsFilePath
		IF VARTYPE(sXlsFileName)='C' AND !EMPTY(sXlsFileName) THEN 
			oDataExporter.sExportedFileName = sXlsFileName
		ELSE
			oDataExporter.sExportedFileName = '' + sXlsFileNameSuffix 
		ENDIF
		oDataExporter.exportToXLS()

		IF bVerbose THEN 
			IF oDataExporter.nInitRecs>0 THEN
				oDataExporter.ShowXLSExportationResults()
			ELSE
				WAIT  "No se encontraron registros para exportar" WINDOW NOWAIT TIMEOUT 5
			ENDIF
		ENDIF

		TRY
			REMOVE TABLE tbTmpQryToXLS DELETE
		CATCH TO oException
		ENDTRY
	ENDIF 
			
	WAIT CLEAR

	RELEASE oDataExporter
	
ENDPROC


HIDDEN FUNCTION EventIsPartitionable(sFilter as String) as Boolean 
	*Retorna .T., si el campo AÑO se encuentra en sFilter; en caso contrario, retorna .F.
	
	RETURN (OCCURS('AÑO', sFilter) = 1) 
ENDFUNC 


HIDDEN PROCEDURE setOrphansLabs()

	*Crea un cursor de nombre rsLaboratoriosHuerfanos que contiene los registros de la tabla LABORATORIOS que no tienen coincidente en la tabla PACIENTE

	WAIT 'Estableciendo laboratorios sin caso asociado ' WINDOW NOWAIT
	
	WAIT 'Asignando Id a laboratorios' WINDOW NOWAIT
	Select LABORATORIOS.*,RECNO() as ID FROM Laboratorios INTO CURSOR rsLabsWithID NOFILTER
	*USE Laboratorios IN 0 AGAIN ALIAS rsLabsWithID 
	
	WAIT 'Estableciendo laboratorios coincidentes' WINDOW NOWAIT
	SELECT DISTINCT rsLabsWithID.ID FROM rsLabsWithID INNER JOIN PACIENTE C ON  ;
		rsLabsWithID.AÑO+rsLabsWithID.SEMANA+rsLabsWithID.COD_EVE+rsLabsWithID.TIP_IDE+rsLabsWithID.NUM_IDE+rsLabsWithID.COD_PRE+rsLabsWithID.COD_SUB == ;
		C.AÑO+C.SEMANA+C.COD_EVE+C.TIP_IDE+C.NUM_IDE+C.COD_PRE+C.COD_SUB ;
		INTO CURSOR rsLaboratoriosNOHuerfanosLB NOFILTER 
	
	WAIT 'Estableciendo laboratorios huerfanos' WINDOW NOWAIT
	SELECT rsLabsWithID.* FROM rsLabsWithID LEFT OUTER JOIN rsLaboratoriosNOHuerfanosLB ON rsLabsWithID.ID = rsLaboratoriosNOHuerfanosLB.ID ;
		WHERE rsLaboratoriosNOHuerfanosLB.ID IS NULL ;
		INTO CURSOR rsLaboratoriosHuerfanosLB NOFILTER

	WAIT 'Asignando valores descriptivos a laboratorios huerfanos' WINDOW NOWAIT
	sLabDataSQL = ' SELECT A.*, B.raz_soc AS nom_upgd, I.nom_mun AS nmun_notif, J.nom_dep AS ndep_notif '

	sFROM_SQL =	' FROM rsLaboratoriosHuerfanosLB A LEFT OUTER JOIN UPGD B ON  A.cod_pre = B.cod_pre AND  A.cod_sub = B.cod_sub ' +;
				' LEFT OUTER JOIN MUNICIPIOS I  ON  LEFT(B.cod_pre,5) = I.cod_mun ' +;
				' LEFT OUTER JOIN DEPTOS J  ON  LEFT(B.cod_pre,2) = J.cod_dep ' 
				
	sLabDataSQL = sLabDataSQL + sFROM_SQL + ' INTO CURSOR rsLaboratoriosHuerfanos READWRITE' 
	&sLabDataSQL
	
	SELECT DISTINCT COUNT(*) as n FROM EVENTOS INTO CURSOR rsDummy

ENDPROC 

PROCEDURE LaunchNotificationToXls(EventCode AS String, sFilter as String,	nDiscriminateEvents as Byte, bBeSilent as Boolean, ;
							bUseAuxInformation as Boolean, bConsolidateBasicdata as Boolean, sOutputType as String, ;
							bUseDefaultQuery as Boolean, sFieldsInFilter as String, sReportsList as string)
	
	*Lanza el método NotificationToXls con exactamente los mismos parámetros que se suministran a este método

	PUBLIC gsXLSFilter as String
	gsXLSFilter = sFilter
	*Variable pública usada en los pre-procesamientos que dependan de ella -según se encuentren establecidos en tables.ExportQueries
	
	LOCAL sSQLCmd as String
	sSQLCmd = ''
	
	LOCAL oCurrentEvent as Object
	oCurrentEvent = NEWOBJECT("Event","Event.fxp",.NULL.)
	
	LOCAL sOldTalk
	sOldTalk = SET("Talk")
	SET TALK ON 
	
	LOCAL sOldExclusive
	sOldExclusive = SET("Exclusive")
	SET EXCLUSIVE ON 

	oIniMgr = NEWOBJECT('IniMgr', 'IniMgr.fxp',.NULL.,'SIVIGILAReporter.ini',.T.)
	nMAX_BATCH_SIZE = VAL(oIniMgr.GetValue("BatchSize", "MAX_BATCH_SIZE", 'SIVIGILAReporter.ini'))
	RELEASE oIniMgr

	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\QueriesHandler') ADDITIVE
	SET PROCEDURE TO SivigilaUtilities ADDITIVE
	SET PROCEDURE TO SivigilaMessenger ADDITIVE


	OPEN DATABASE BDSIVIGILA
					
	*Garantiza que la exportación siempre se lleve a cabo de acuerdo con la última versión del sistema definida;
	por sus identificadores nMajor.nMinor.nBuild según se encuentran definidos en main
	DO resetSivigilaVersion	IN main

	* Establece el conjunto de eventos que se deben procesar. En este conjunto se introduce un registro artificial vacío con el ;
	propósito de generar un conjunto de archivos excel en donde se encuentre la relación de todos los registros de datos básicos;
	de todos los eventos: evento  datos basicos.xls, evento  laboratorios y datos basicos.xls y evento  upgd.xls. El conjunto ;
	de archivos generado así generado es necesario para soportar el procesamiento de disponibilidades.
	IF EMPTY(EventCode) THEN
		IF VARTYPE(nDiscriminateEvents) = 'N' THEN
			IF nDiscriminateEvents = 1 THEN
				sSQLCmd  = " SELECT COD_EVE, FORMULARIO, PRIORIDAD AS NREG FROM EVENTOS WHERE COD_EVE!='000 ' "
				IF bConsolidateBasicdata THEN
					sSQLCmd  = sSQLCmd  + " UNION SELECT DISTINCT '' AS COD_EVE, '' AS FORMULARIO, 10000 AS NREG FROM EVENTOS "
				ENDIF
				sSQLCmd  = sSQLCmd  + " ORDER BY NREG "
			ELSE
				sSQLCmd  = "SELECT DISTINCT '' AS COD_EVE, '' AS FORMULARIO, 10000 AS NREG FROM EVENTOS "
			ENDIF
		ENDIF
	ELSE
		sSQLCmd = "SELECT COD_EVE, FORMULARIO FROM EVENTOS WHERE COD_EVE='" + EventCode + "'"
	ENDIF
	sSQLCmd = sSQLCmd +  " INTO CURSOR rsMasterTargetEvents"
	&sSQLCmd 
	
	sFormerFilter = IIF(AT("#",sFilter)=0,sFilter,STRTRAN(sFilter,"#"))

	*Con miras a mejorar el rendimiento cuando hay que discriminar por evento, construye un recordset con los registros de laboratorios ;
	que no tienen un caso básico asociado
	IF VARTYPE(nDiscriminateEvents) = 'N' THEN
		IF nDiscriminateEvents = 1 THEN
			This.setOrphansLabs()
		ENDIF 
	ENDIF 
*SET STEP ON 
	SELECT rsMasterTargetEvents
	SCAN
		oCurrentEvent.Refresh(rsMasterTargetEvents.COD_EVE)
		
		IF oCurrentEvent.bNotBDPlusCDEvent THEN
			*El evento tiene tabla de datos independiente y no requiere tratamiento especial
			This.NotificationToXls(oCurrentEvent.sCOD_EVE, sFormerFilter ,	nDiscriminateEvents, bBeSilent, bUseAuxInformation, ;
										bConsolidateBasicdata, sOutputType, bUseDefaultQuery, sFieldsInFilter)
		ELSE 	
			IF oCurrentEvent.bIsIndividual THEN 
				*El evento es individual
				
				*Establece qué tantos registros estarían involucrados en la producción del archivo excel
				WAIT 'Estableciendo base de agrupamientos' WINDOW NOWAIT
				sSQLCmd = 'SELECT COUNT(*) AS N FROM PACIENTE WHERE '
				IF !EMPTY(oCurrentEvent.sCOD_EVE) THEN 
					sSQLCmd = sSQLCmd +	' PACIENTE.COD_EVE="' + oCurrentEvent.sCOD_EVE  + '" AND '
				ENDIF 
				sSQLCmd = sSQLCmd + STRTRAN(sFormerFilter,'RecordSource','PACIENTE') + ' INTO CURSOR rsTotalRecs'
				&sSQLCmd

				IF (rsTotalRecs.n * MULTIPLICATION_FACTOR) > MAX_RECORDS_TO_XLS THEN

					IF This.EventIsPartitionable(sFilter) THEN 
						*Se debe producir el excel en lotes del tamaño máximo permisible

						IF IsMemberOf('LABS_HUERFANOS', sReportsList) THEN
							This.setOrphansLabs()
						ENDIF 
						
						WAIT 'Estableciendo grupos' WINDOW NOWAIT
						*Establece cuántos registros hay por grupo
						sSQLCmd = 'SELECT año,VAL(semana) AS nSEMANA, COUNT(*) as n from PACIENTE WHERE '
						IF !EMPTY(oCurrentEvent.sCOD_EVE) THEN 
							sSQLCmd = sSQLCmd + ' cod_eve="' + oCurrentEvent.sCOD_EVE + '" AND ' 
						ENDIF
						sSQLCmd = sSQLCmd + STRTRAN(sFormerFilter, 'RecordSource.') + ' GROUP BY año,nSEMANA INTO CURSOR rsEpidemWeekCounts'
						&sSQLCmd
						WAIT CLEAR 
						
						This.reset_aOutputFiles()
						
						*Recorre el recordset de grupos recién creado estableciendo valores mínimo y máximo de SEMANA para producir un XLS ;
						correspondiente a esos valores y que además cumpla con el filtro orignal sFormerFilter

						*Recorre por cada año, el recordset de grupos recién creado estableciendo valores mínimo y máximo de SEMANA para producir un XLS ;
						correspondiente a esos valores y que además cumpla con el filtro orignal sFormerFilter
 				
						DIMENSION aEpidemYears(1)
						SELECT DISTINCT AÑO FROM rsEpidemWeekCounts INTO ARRAY aEpidemYears
						nEpidemYears = _tally
						
						nBatchId = 0
						FOR iYear=1 to nEpidemYears
							SELECT rsEpidemWeekCounts
							IF nEpidemYears>1 THEN 
								sCmd = 'SET FILTER TO AÑO="' + aEpidemYears[iYear] + '"'
								&sCmd
							ENDIF
							GO TOP 
							
							nRecs = 0
							bSetInitWeek = .T.
							nCounter = 0
							DO WHILE !EOF()
								IF bSetInitWeek THEN 
									sInitWeek = ALLTRIM(STR(rsEpidemWeekCounts.nSEMANA))
									sLastWeek = sInitWeek 
								ENDIF 
								
								nRecs = nRecs +  rsEpidemWeekCounts.n
								IF nRecs > nMAX_BATCH_SIZE  THEN
									IF nCounter >= 1 THEN  
										SKIP -1		
									ENDIF 						
									sLastWeek = ALLTRIM(STR(rsEpidemWeekCounts.nSEMANA))
									
									nBatchId = nBatchId + 1
									IF sInitWeek != sLastWeek  THEN
										sFilter = sFormerFilter + " AND VAL(RecordSource.SEMANA)>= " + sInitWeek + " AND VAL(RecordSource.SEMANA) <= " +;
													sLastWeek + " AND RecordSource.AÑO = '" + aEpidemYears[iYear,1] + "'"
									ELSE
										sFilter = sFormerFilter + " AND VAL(RecordSource.SEMANA)= " + sInitWeek +  " AND RecordSource.AÑO = '" + aEpidemYears[iYear,1] + "'"
									ENDIF
									This.NotificationToXls(oCurrentEvent.sCOD_EVE, sFilter,	1, .T., bUseAuxInformation, bConsolidateBasicdata, 'CSV', ;
															bUseDefaultQuery, sFieldsInFilter, '_' + ALLTRIM(STR(nBatchId)), sReportsList)
									SELECT DISTINCT COUNT(*) as n FROM EVENTOS INTO CURSOR rsDummy
									
									IF oCurrentEvent.ManageContacts() AND IsMemberOf('CONTACTOS', sReportsList) THEN
										*Genera un reporte especial para el evento con la información de contactos y seguimientos a contactos

										This.launchMakeContactsReport(oCurrentEvent.sCOD_EVE, '.T.', nMajor, nMinor, nBuild, 'CNTA', '_' + ALLTRIM(STR(nBatchId)) ) 
									ENDIF

									nRecs = 0
									bSetInitWeek = .T.
									nCounter = 0
								ELSE
									bSetInitWeek = .F.
									nCounter = nCounter +1
								ENDIF
								
								SELECT rsEpidemWeekCounts
								IF !EOF() THEN 
									SKIP 
									sLastWeek = ALLTRIM(STR(rsEpidemWeekCounts.nSEMANA))
									 
									IF EOF() AND nRecs <= nMAX_BATCH_SIZE  THEN 
										nBatchId = nBatchId + 1
										sFilter = sFormerFilter + " AND VAL(RecordSource.SEMANA)>= " + sInitWeek + " AND RecordSource.AÑO = '" + aEpidemYears[iYear,1] + "'"
										This.NotificationToXls(oCurrentEvent.sCOD_EVE, sFilter,	1, bBeSilent, bUseAuxInformation, bConsolidateBasicdata, 'CSV', ;
																bUseDefaultQuery, sFieldsInFilter, '_' + ALLTRIM(STR(nBatchId)), sReportsList)
										SELECT DISTINCT COUNT(*) as n FROM EVENTOS INTO CURSOR rsDummy
										
										IF oCurrentEvent.ManageContacts() AND IsMemberOf('CONTACTOS', sReportsList) THEN
											*Genera un reporte especial para el evento con la información de contactos y seguimientos a contactos

											This.launchMakeContactsReport(oCurrentEvent.sCOD_EVE, '.T.', nMajor, nMinor, nBuild, 'CNTA', '_' + ALLTRIM(STR(nBatchId))) 
										ENDIF

										SELECT rsEpidemWeekCounts
									ENDIF 
								ENDIF 
								SELECT rsEpidemWeekCounts
							ENDDO 
						NEXT iYear

						*Crea archivos únicos con los reportes consolidados
						=ASORT(This.aOutputFiles, AELEMENT(This.aOutputFiles,1,3))
						sPreviousRootName = 'NoName'
						FOR i=1 TO  This.nOutputFiles
							sRootName = ALLTRIM(SUBSTR(This.aOutputFiles[i,1],1,AT('_',This.aOutputFiles[i,1])-1))
							
							IF sRootName != sPreviousRootName THEN 
								sBatFileContent = 'Set oldDir=%CD%' +  CrLf +;
													'CD /D "' + This.aOutputFiles[This.nOutputFiles,2] + '"' +  CrLf +;
													'MD Tmp785q3er' +  CrLf +;
													'findstr /i "cod_eve" "' + This.aOutputFiles[i,1] + '.CSV" > Tmp785q3er\Header.txt' +  CrLf +;
													'FOR %%F in ("' + sRootName + '*.CSV") DO (' + CrLf +;
													'findstr /v /i "cod_eve" "%%F" > Tmp785q3er\"%%F"' + CrLf +;
													')' + CrLf  +;
													'Copy Tmp785q3er\Header.txt + Tmp785q3er\*.csv "Consolidado_' + sRootName + '.csv"' + CrLf +;
													'RD Tmp785q3er /q /s' + CrLf +;
													'CD /D "%oldDir%"'
								DO runBatFile WITH sBatFileContent IN Utilities
							ENDIF 

							sPreviousRootName = sRootName 
						NEXT i

					ELSE 
						This.NotificationToXls(oCurrentEvent.sCOD_EVE, sFormerFilter, nDiscriminateEvents, bBeSilent, bUseAuxInformation, bConsolidateBasicdata, sOutputType, ;
													bUseDefaultQuery, sFieldsInFilter, sReportsList)
						SELECT DISTINCT COUNT(*) as n FROM EVENTOS INTO CURSOR rsDummy
						
						IF oCurrentEvent.ManageContacts() AND IsMemberOf('CONTACTOS', sReportsList) THEN
							*Genera un reporte especial para el evento con la información de contactos y seguimientos a contactos

							This.launchMakeContactsReport(oCurrentEvent.sCOD_EVE, '.T.', nMajor, nMinor, nBuild, 'CNTA') 
						ENDIF
					ENDIF 
				ELSE
					This.NotificationToXls(oCurrentEvent.sCOD_EVE, sFormerFilter, nDiscriminateEvents, bBeSilent, bUseAuxInformation, bConsolidateBasicdata, sOutputType, ;
												bUseDefaultQuery, sFieldsInFilter, , sReportsList)
					SELECT DISTINCT COUNT(*) as n FROM EVENTOS INTO CURSOR rsDummy
					
					IF oCurrentEvent.ManageContacts() AND IsMemberOf('CONTACTOS', sReportsList) THEN
						*Genera un reporte especial para el evento con la información de contactos y seguimientos a contactos

						This.launchMakeContactsReport(oCurrentEvent.sCOD_EVE, '.T.', nMajor, nMinor, nBuild, 'CNTA') 
					ENDIF
				ENDIF
			ELSE
				*El evento es colectivo
				This.NotificationToXls(oCurrentEvent.sCOD_EVE, sFormerFilter, nDiscriminateEvents, bBeSilent, bUseAuxInformation, ;
											bConsolidateBasicdata, sOutputType, bUseDefaultQuery, sFieldsInFilter)
			ENDIF 
		ENDIF 
		SELECT rsMasterTargetEvents
	ENDSCAN
	
	RELEASE gsXLSFilter,oCurrentEvent 
	
	SET TALK &sOldTalk 
	SET EXCLUSIVE &sOldExclusive 
	WAIT CLEAR 
ENDPROC 


PROCEDURE NotificationToXls(EventCode AS String, sFilter as String,	nDiscriminateEvents as Byte, bBeSilent as Boolean, ;
							bUseAuxInformation as Boolean, bConsolidateBasicdata as Boolean, sOutputType as String, ;
							bUseDefaultQuery as Boolean, sFieldsInFilter as String, sSuffix as string, sReportsList as String)

*Produce archivos de notificación en formato sOutputType -normalmente XLS- con los casos del evento EventCode que cumplan ;
la condición sFilter partiendo de la especificación de exportación que para él se encuentre en la tabla ExportQueries.

* EventCode: Código del evento cuyas notificaciones se exportan. Si EventCode es vacío, se exportarán todos los eventos de ;
			 la tabla Eventos siguiendo las indicciones del parámetro nDiscriminateEvents
			 
* nDiscriminateEvents : indica si deben producirse conjuntos de archivos excel discriminados por cada ;
						evento de la tabla eventos. Aplica cuando EventCode es vacío; en caso contrario, ;
						es ignorado
						
* bBeSilent: si es .T. los mensajes del procesamiento se dirigen a la interfaz de usuario (valor por defecto);
			 en caso contrario, se dirigen a un archivo plano de resultados	
			 
* bUseAuxInformation: indica si debe utilizarse la "inteligencia" auxiliar en la consolidación de ajustes,;
						esto es, utilizar otro criterio para procesar los ajustes que el habitual.
						
* bConsolidateBasicdata: indica si debe generarse el archivo que consolida los datos básicos en un único archivo.

*sOutputType (opcional): tipo de archivos de salida que debe producirse: XLS (Valor por defecto) o DBF			

*bUseDefaultQuery (opcional): si es .T,, en vez de usar el query de exportación que se encuentre en la tabla ExportQueries;
							utiliza un query de exportación que se encuentra definido en la implementación de este método


*sFieldsInFilter (opcional): lista separada por comas de campos presentes en sfilter

	
	LOCAL XlsQuery AS String, EventTable AS String

	LOCAL nExportQueries as Byte 
	nExportQueries = 1 &&Default value
	*Número de queries de exportación XLS que se encuentran en la tabla ExportQueries para un evento

	LOCAL oCurrentEvent  as object
	oCurrentEvent = NEWOBJECT("Event","Event.fxp",.NULL.)	
	
	LOCAL oDataExporter AS Object 
	oDataExporter = NEWOBJECT("DataExporter","DataExporter.fxp",.null.)

	DIMENSION aXLSQueries(1)
	*Almacenará los queries de exportación XLS para un evento, en caso de que se encuentren dos o más en la tabla ExportQueries 

	SET PROCEDURE TO Utilities ADDITIVE 
	SET PROCEDURE TO (PATH_TO_DEVELOPMENT_ENVIRONMENT + '\SIVIGILAMessenger') ADDITIVE
	
	IF !(VARTYPE(sOutputType )='C' AND !EMPTY(sOutputType )) THEN 
		sOutputType = 'XL5'
	ENDIF 
	oDataExporter.sExportedFileType = IIF(sOutputType = 'XLS', 'XL5', sOutputType )
	
	IF !(VARTYPE(sReportsList)='C' AND !EMPTY(sReportsList)) THEN 
		sReportsList = ''
	ENDIF 
	
	IF !(VARTYPE(sSuffix)='C' AND !EMPTY(sSuffix)) THEN 
		sSuffix = ''
	ENDIF 

	STORE '' TO XlsQuery, EventTable

	STORE 0 TO  nIndividualNotifications, nOrphansLabsNotifications
	STORE .F. TO HasComplementaryData
	STORE .T. TO bDoNotIgnoreEmptyFiles

	SET TALK ON
	SET PROCEDURE TO PlainsLib ADDITIVE

	*DEFINE WINDOW msgWindow AT 1,40 SIZE 5,100 FONT "Arial" TITLE "Visor de resultados" SYSTEM
	*ACTIVATE WINDOW msgWindow 
	*SET TALK WINDOW msgWindow

	sInitTime=TIME()

	IF VARTYPE(sSuffix)!='C' THEN 
		sSuffix = ''
	ENDIF 

	sFilter=STRTRAN(sFilter,"RecordSource","A")
	
	IF bBeSilent THEN
		sResultsFileName="ResultadosRetroalimentacionExcel_" + ALLTRIM(CHRTRAN(DTOC(DATE()),"/","")) + ".txt"
		nResultsFileHandler=FCREATE(This.sXlsFilePath + '\' + sResultsFileName)
	ENDIF

	oCurrentEvent.Refresh(EventCode)

	IF !oCurrentEvent.hasComplementaryData() THEN 
		tmpTable = 'rs' + 'PACIENTE'
	ELSE
		tmpTable = 'rs' + oCurrentEvent.sDC_TABLA
	ENDIF

	sResultMsg = ""
	sPostCmds = ""

	WAIT 'Procesando el evento con código ' + EventCode WINDOW NOWAIT TIMEOUT 10

	IF oCurrentEvent.bNotBDPlusCDEvent THEN
		*El evento no obedece al esquema datos básicos + datos complementarios y requiere tratamiento especial
	
		EventTable = ''

		*Establece los campos que se deben extraer. 
		XlsQuery = GetQryString("E" + PADL(oCurrentEvent.nFORMULARIO, 2, '0'), @EventTable, nMajor, nMinor, nBuild, .T., .T.,,,@nExportQueries, ;
								@sPostCmds, !oCurrentEvent.isActive())
		IF EMPTY(XlsQuery) THEN
			*No se encontró un query definido de exportación para el evento, por tanto, se usa el query por default	

			EventTable = 'Eventos_' + PADL(oCurrentEvent.nFORMULARIO, 2, '0')
			
			sComplementaryDataSQL = getQueryFields('E' + PADL(oCurrentEvent.nFORMULARIO, 2, '0'), 1, 'A', nMajor, nMinor, nBuild, !oCurrentEvent.isActive()) 
			
			sAdditionalDataSQL =', B.nit_upgd, E.nom_eve, B.raz_soc AS nom_upgd, C.nom_dep AS ndep_notif, ' +;
								' D.nom_mun AS nmun_notif, A.Version' 

			sFROM_SQL =	' FROM ' + EventTable + ' A  LEFT OUTER JOIN UPGD B ON  A.cod_pre = B.cod_pre AND  A.cod_sub = B.cod_sub ' + ;
						' LEFT OUTER JOIN DEPTOS C  ON  LEFT(B.cod_pre,2) = C.cod_dep ' +;
						' LEFT OUTER JOIN MUNICIPIOS D  ON  LEFT(B.cod_pre,5) = D.cod_mun ' +;
						' LEFT OUTER JOIN Eventos E ON  A.cod_eve = E.cod_eve '
		ELSE
			= ALINES(aXLSQueries,XlsQuery,15,'|') 
		ENDIF

		*Establece las condiciones de filtrado para la extracción de registros y el recordset de destino
		*tmpTable = 'tb' + SYS(2015)
		*bTmp = oCurrentEvent.ComplementaryDataHasFields(sFieldsInFilter)
		IF oCurrentEvent.ComplementaryDataHasFields(sFieldsInFilter) THEN 
			sWHERE_SQL =' WHERE A.COD_EVE ="' + EventCode + '" AND ' + IIF(AT("#",sFilter)=0,sFilter,STRTRAN(sFilter,"#")) + ' INTO TABLE ' + (tmpTable) +;
						' DATABASE SIVIGILATemp' 
						*' AND B.act_siv = 1 ' +	strCondition1 + strCondition2 + ;
						' ORDER BY nreg INTO TABLE ' + (tmpTable) ;
						Esta parte fue modificada por solicitud del INS (Ver error con IdError=350)
						
			FOR iQuery = 1 TO nExportQueries
				IF EMPTY(XlsQuery) THEN			
					XlsQuery = "SELECT " + sComplementaryDataSQL + sAdditionalDataSQL + sFROM_SQL + sWHERE_SQL 
				ELSE
					XlsQuery = aXLSQueries[iQuery] + sWHERE_SQL 
				ENDIF

				* Se ejecuta el Query que extrae los datos del evento para exportar a Excel
				*=STRTOFILE(XlsQuery, ALLTRIM(EventCode) + "_ToXls.qpr")
				sOldDB = SET("Database")
				OPEN DATABASE SIVIGILATemp
				SET  DATABASE TO SIVIGILATemp
				TRY
					REMOVE TABLE (tmpTable) DELETE
				CATCH TO oException
				ENDTRY
				&XlsQuery 
				IF _TALLY>0 THEN
					*Procesa los ajustes de la fuente de datos tmpTable recien creada
					SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
					DO fastProcessAdjustments WITH (tmpTable),getKeyFields(UPPER(EventTable)), .T. IN SIVIGILAUtilities			
					SELECT rsAdjustedCases
	*SET STEP ON 					
					*Ejecuta los post comandos asociados al i-ésimo query de exportación, si es que existen
					IF !EMPTY(sPostCmds) THEN
						s_iPostCmds = sPostCmds + ',' + ALLTRIM(STR(iQuery))
						&s_iPostCmds 
					ENDIF
										
			        xlsFileName = 'EVENTO '+ALLTRIM(EventCode) + IIF(nExportQueries>1,'_'+ALLTRIM(STR(iQuery)),'')
			        IF EMPTY(sPostCmds) THEN
			        	oDataExporter.sSourceTableName = 'rsAdjustedCases'
					ELSE
						oDataExporter.sSourceTableName =  ALIAS()
					ENDIF
					oDataExporter.sExportationPath = This.sXlsFilePath
					oDataExporter.sExportedFileName = xlsFileName 
					oDataExporter.exportToXLS(bDoNotIgnoreEmptyFiles)		
					sResultMsg = 'SE HAN GENERADO LOS SIGUIENTES ARCHIVOS'+CHR(13)+CHR(13)+;
								xlsFileName + '.' + CHRTRAN(oDataExporter.sExportedFileType,'5','S') + CHR(13)+;
								CHR(13)+'EN LA CARPETA: ' + This.sXlsFilePath
					IF !bBeSilent THEN
				        oDataExporter.ShowXLSExportationResults()
					ELSE
						sResultMsg= 'Evento ' + EventCode + CR_LF + sResultMsg + CR_LF + CR_LF
						=FPUTS(nResultsFileHandler,sResultMsg)
					ENDIF
					This.LogOutputFiles(oDataExporter, 'DB+DC')
				ELSE
					sResultMsg = 'No se encontraron registros que satisfagan los criterios'
					IF !bBeSilent THEN
						SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
				      	=showErrorMessage(sResultMsg,0)
					ELSE
						sResultMsg= 'Evento ' + EventCode + CR_LF + sResultMsg + CR_LF + CR_LF
						=FPUTS(nResultsFileHandler,sResultMsg)
					ENDIF					
				ENDIF
			ENDFOR
		
			TRY
				REMOVE TABLE (tmpTable) DELETE
			CATCH TO oException
			ENDTRY
			SET DATABASE TO &sOldDB
		ENDIF 
	ELSE
		IF !oCurrentEvent.bIsIndividual THEN
			* Es un evento colectivo
*SET STEP ON 
			tmpTable = 'tmpXLS'
	
			*Procesa los ajustes de los registros objetivo de la tabla brotes
			SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
			*DO ProcessAdjustments WITH "BROTES", getKeyFields("BROTES"), .T., "COD_EVE ='" + EventCode + "' AND " + STRTRAN(sFilter,"A.",""), ;
										"rsBrotesAjustados" IN SIVIGILAUtilities			
			DO fastProcessAdjustments WITH "BROTES", getKeyFields("BROTES"), .T., "COD_EVE ='" + EventCode + "' AND " + ;
											STRTRAN(IIF(AT("#",sFilter)=0,sFilter,STRTRAN(sFilter,"#")),"A.",""), ;
											"rsBrotesAjustados", .F. IN SIVIGILAUtilities			
			

			*Construye un query que toma la información de las tablas necesarias para exportar los datos del evento	


			*Establece el conjunto de campos que deben exportarse de la tabla BROTES incluyendo la decodificación;
			en donde sea necesario
			sCollectiveDataSQL= 'SELECT A.cod_eve, A.fec_not, A.semana, A.año, A.cod_pre, A.cod_sub, ' +;
								' A.cod_mun AS cod_mun_, A.num_con, A.gru_1, A.gru_2, A.GRUPO_3, A.GRUPO_4, ' +;
								' A.GRUPO_5, A.GRUPO_6,	A.GRUPO_7, A.GRUPO_8, A.GRUPO_9, A.GRUPO_10, A.GRUPO_11, ' +;
								' A.GRUPO_12, A.GRUPO_13, A.GRUPO_14, A.GRUPO_15, A.GRUPO_16, A.GRUPO_17, A.GRUPO_18, ' +;
								' A.cas_sos, A.cas_pro, A.cas_conl, A.cas_conc, A.cas_conn, ' +;
								' A.hombres, A.mujeres, A.vivos, A.muertos, A.pte_hos, A.pte_amb, LEFT(mun_pro,2) AS cod_dpto_p, ' +;
								' RIGHT(mun_pro,3) AS cod_mun_pr, A.bar_sector, A.ajuste, A.fec_aju, A.Version,' +;
								' B.nit_upgd, B.raz_soc AS nom_upgd, C.nom_dep AS ndep_notif, ' +;
			  					' D.nom_mun AS nmun_notif, F.nom_dep AS ndep_proce, G.nom_mun AS nmun_proce '
			  					
			sAdditionalCollectiveDataSQL = ', A.Version' 

			sFROM_SQL =	' FROM rsBrotesAjustados A LEFT OUTER JOIN UPGD B ON  A.cod_pre = B.cod_pre AND  A.cod_sub = B.cod_sub ' + ;
						' LEFT OUTER JOIN DEPTOS C ON  LEFT(A.cod_mun,2) = C.cod_dep ' +;
						' LEFT OUTER JOIN MUNICIPIOS D ON  A.cod_mun = D.cod_mun ' +;
						' LEFT OUTER JOIN DEPTOS F ON  LEFT(A.mun_pro,2) = F.cod_dep ' +;
						' LEFT OUTER JOIN MUNICIPIOS G ON  A.mun_pro = G.cod_mun ' 

			*Establece las condiciones de filtrado para la extracción de registros
			sWHERE_SQL =' WHERE A.COD_EVE ="' + EventCode + '" AND ' + IIF(AT("#",sFilter)=0,sFilter,STRTRAN(sFilter,"#")) + ;
						' INTO TABLE ' + (tmpTable) 
						*' AND B.act_siv = 1  INTO TABLE ' + (tmpTable) ;
						Esta parte fue modificada por solicitud del INS (Ver error con IdError=350)
			
			
			XlsQuery = sCollectiveDataSQL + sAdditionalCollectiveDataSQL + sFROM_SQL + sWHERE_SQL 


			*=STRTOFILE(XlsQuery, ALLTRIM(EventCode) + "_ToXls.qpr")
		    &XlsQuery
			IF _TALLY>0 THEN
				oDataExporter.sSourceTableName = tmpTable
				oDataExporter.sExportationPath = This.sXlsFilePath
				oDataExporter.sExportedFileName = 'EVENTO '+ALLTRIM(EventCode)
				oDataExporter.exportToXLS(bDoNotIgnoreEmptyFiles)

				IF !bBeSilent THEN
					oDataExporter.ShowXLSExportationResults()
				ELSE
					sResultMsg = 'SE HAN GENERADO LOS SIGUIENTES ARCHIVOS'+CHR(13)+CHR(13)+;
								oDataExporter.sExportedFileName + '.' + CHRTRAN(oDataExporter.sExportedFileType,'5','S') + CHR(13)+;
								CHR(13)+'EN LA CARPETA: ' + This.sXlsFilePath
					sResultMsg = 'Evento ' + EventCode + CR_LF + sResultMsg + CR_LF + CR_LF
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
				
				This.LogOutputFiles(oDataExporter, 'DB+DC')
			ELSE
				sResultMsg = 'No se encontraron registros que satisfagan los criterios'
				IF !bBeSilent THEN
					SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
			      	=showErrorMessage(sResultMsg,0)
				ELSE
					sResultMsg = 'Evento ' + EventCode + CR_LF + sResultMsg + CR_LF + CR_LF
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF					
			ENDIF
		ELSE
			* El Evento es individual (incluye el caso Empty(EventCode).)
	
			* Se determina si el evento tiene datos complementarios
			HasComplementaryData = (oCurrentEvent.bHasComplementaryData) 

			IF HasComplementaryData THEN
				xlsFileNameSuffix =  ' DATOS BASICOS Y COMPLEMENTARIOS'
			ENDIF
 
			sLinkerExpr = ''
			sPostCmds = ''
			IF !bUseDefaultQuery THEN 
				XlsQuery = GetQryString("E" + PADL(oCurrentEvent.nFORMULARIO, 2, '0'), @EventTable, nMajor, nMinor, nBuild, .T., .T., ;
										@sLinkerExpr, , , @sPostCmds, !oCurrentEvent.isActive())
			ENDIF 
			IF EMPTY(XlsQuery) THEN
				*No se encontró un query definido de exportación para el evento, por tanto, se usa el query por default	

				*Establece los campos de datos básicos que se deben extraer. Los campos son renombrados en donde quiera que sea neceario para evitar;
				posibles duplicidades de nombre con campos que se deben extraer de otras tablas distintas a PACIENTE
				sIndividualDataSQL='SELECT A.cod_eve, A.fec_not, A.semana, A.año, A.cod_pre, A.cod_sub,' + ;
									' A.pri_nom AS pri_nom_, A.seg_nom AS seg_nom_, A.pri_ape AS pri_ape_, A.seg_ape AS seg_ape_,' + ;
									' A.tip_ide AS tip_ide_, A.num_ide AS num_ide_, A.edad AS edad_, A.uni_med AS uni_med_,' + ;
									' A.NACIONALID AS NACIONALI_, P.PAIS AS NOMBRE_NACIONALIDAD, A.sexo AS sexo_, K.cod_pais AS cod_pais_o, ' + ;
									' LEFT(A.cod_mun,2) AS cod_dpto_o, RIGHT(A.cod_mun,3) AS cod_mun_o, A.area AS area_,' + ;
									' A.localidad AS localidad_, A.cen_poblad AS cen_pobla_, PADR(A.vereda,106) AS vereda_,' + ;
									' PADR(A.bar_ver,40) AS bar_ver_, PADR(A.dir_res,50) AS dir_res_, A.ocupacion AS ocupacion_,' + ;
									' A.tip_ss AS tip_ss_, A.cod_ase AS cod_ase_, A.per_etn AS per_etn_, O.nom_grupo as nom_grupo_, A.estrato AS estrato_,' + ;
									' A.GP_DISCAPA, A.GP_DESPLAZ, A.GP_MIGRANT, A.GP_CARCELA,' + ;
									' A.GP_GESTAN, A.SEM_GES as SEM_GES_, A.GP_INDIGEN, A.GP_POBICFB AS GP_POBICBF, A.GP_MAD_COM,' + ;
									' A.GP_DESMOVI, A.GP_PSIQUIA, A.GP_VIC_VIO, A.GP_OTROS, A.FUENTE AS FUENTE_,' + ;
									' P2.CodNumPais as cod_pais_r, LEFT(A.mun_pro,2) AS cod_dpto_r,' + ;
									' RIGHT(A.mun_pro,3) AS cod_mun_r, A.fec_con AS fec_con_, A.ini_sin AS ini_sin_,' + ;
									' A.tip_cas AS tip_cas_, A.pac_hos AS pac_hos_,' + ;
									' A.fec_hos AS fec_hos_, A.con_fin AS con_fin_, A.fec_def AS fec_def_, A.ajuste AS ajuste_,' + ;
									' A.telefono AS telefono_, A.fecha_nto AS fecha_nto_,' + ;
									' A.cer_def AS cer_def_, A.cbmte AS cbmte_, A.UNI_MODIF, L.raz_soc AS nuni_modif, DATE() AS fec_arc_xl,' + ;
									' PADR(A.nom_dil_fi,100) AS nom_dil_f_,' + ;
									' A.tel_dil_fi AS tel_dil_f_, A.fec_aju AS fec_aju_, B.nit_upgd, A.FM_FUERZA, A.FM_UNIDAD, A.FM_GRADO, A.Version'
									
				sAdditionalIndividualDataSQL = ', E.nom_eve, B.raz_soc AS nom_upgd,' + ; 
												' K.pais as npais_proce, C.nom_dep AS ndep_proce, D.nom_mun AS nmun_proce,' + ; 
												' P2.Pais as npai_resi, F.nom_dep AS ndep_resi, G.nom_mun AS nmun_resi,' +;
												' J.nom_dep AS ndep_notif, I.nom_mun AS nmun_notif'

				sFROM_SQL =	' FROM Paciente A  LEFT OUTER JOIN UPGD B ON  A.cod_pre = B.cod_pre AND  A.cod_sub = B.cod_sub ' + ;
							' LEFT OUTER JOIN MUNICIPIOS I  ON  LEFT(B.cod_pre,5) = I.cod_mun ' +;
							' LEFT OUTER JOIN DEPTOS J  ON  LEFT(B.cod_pre,2) = J.cod_dep ' +;
							' LEFT OUTER JOIN DEPTOS C ON  LEFT(A.cod_mun,2) = C.cod_dep ' +;
							' LEFT OUTER JOIN MUNICIPIOS D ON  A.cod_mun = D.cod_mun ' +;
							' LEFT OUTER JOIN Eventos E ON  A.cod_eve = E.cod_eve ' +;   						   					
							' LEFT OUTER JOIN DEPTOS F ON  LEFT(A.mun_pro,2) = F.cod_dep ' +;
							' LEFT OUTER JOIN MUNICIPIOS G ON  A.mun_pro = G.cod_mun ' +;
							' LEFT OUTER JOIN LOOKUPS!VMUNICIPIOS K ON  A.cod_mun = K.cod_mun ' +;
							' LEFT OUTER JOIN UPGD L ON  A.UNI_MODIF == L.cod_pre + L.cod_sub' +;
							' LEFT OUTER JOIN GruposEtnicos O ON A.GRUPO_ETNI = O.COD_GRUPO' +;
							' LEFT OUTER JOIN PAISES P ON  A.NACIONALID = P.CODNUMPAIS' +;
							' LEFT OUTER JOIN PAISES P2 ON G.Cod_pais = P2.Codnumpais'
							
				* Si el evento tiene datos complementarios, agrega las columnas de la tabla EventTable a la consulta de exportación
				IF HasComplementaryData THEN
					EventTable = 'Eventos_' + PADL(oCurrentEvent.nFORMULARIO, 2, '0')
					xlsFileNameSuffix =  ' DATOS BASICOS Y COMPLEMENTARIOS'
					IF bUseAuxInformation THEN
						*Efectúa un procesamiento de ajustes previo directamente sobre la tabla de datos complementarios, de tal forma que ;
						se le de prioridad a la especificación de procesamiento de ajustes que se encuentre en la tabla SIVIGILAAdjustmentsSetup.
						*Luego de este procesamiento, la tabla de datos complementarios será el recordset recién ajustado rsAdjustedCases
						
						SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
						sTagExpr = fieldValue("TAG_EXPR", "SIVIGILAAdjustmentsSetup", "TABLE_NAME == '" + UPPER(EventTable) + "' AND ACTIVO")
						IF VARTYPE(sTagExpr) == 'C' THEN
							* Hay una configuración para el evento en la tabla de configuración de ajustes
							sTagExpr = ALLTRIM(sTagExpr)
							sOrdenProc = ALLTRIM(fieldValue("ORDEN_PROC", "SIVIGILAAdjustmentsSetup", "TABLE_NAME == '" + UPPER(EventTable) + "'"))
							oAdjustmentsProcessor = NEWOBJECT("AdjustmentsProcessor","AdjustmentsProcessor.PRG")
							oAdjustmentsProcessor.sSourceTablename = EventTable
							IF oCurrentEvent.ComplementaryDataHasFields(sFieldsInFilter) THEN 
								oAdjustmentsProcessor.processAdjustments(sTagExpr, STRTRAN(IIF(AT("#",sFilter)=0,sFilter,SUBSTR(sFilter,1,AT("#",sFilter)-1)),"A.", "") ,sOrdenProc,,,,.T.)
							ELSE
								*La tabla de datos complementarios no tiene todos los campos que se usan en le filtro, por tanto, el procesamiento de ajustes;
								se hace sobre todolos registros de la tabla
								oAdjustmentsProcessor.processAdjustments(sTagExpr, ,sOrdenProc,,,,.T.)
							ENDIF 
							RELEASE oAdjustmentsProcessor
							
							EventTable = 'rsAdjustedCases'
						ENDIF
					ENDIF
					*Establece, según el evento, los campos de datos complementarios que se deben extraer 
					StartPosition = 8
					IF RIGHT(EventTable, 2) $ '31 32 43'
						StartPosition = 9
				 	ENDIF
 				 	
					sComplementaryDataSQL = getQueryFields('E' + PADL(oCurrentEvent.nFORMULARIO, 2, '0'), StartPosition, 'H', nMajor, nMinor, nBuild, !oCurrentEvent.isActive())
				  					
			   		sFROM_SQL =	sFROM_SQL + ' LEFT OUTER JOIN (SELECT * FROM ' + EventTable + ' ORDER BY AJUSTE,FEC_AJU) AS H ON A.cod_eve = H.cod_eve AND' +;
								' A.año = H.año AND A.semana = H.semana AND ' +;
								' A.tip_ide = H.tip_ide AND A.num_ide = H.num_ide  AND ' + ;
								' A.cod_pre = H.cod_pre AND A.cod_sub = H.cod_sub '
								
					XlsQuery =  sIndividualDataSQL + "," + sComplementaryDataSQL + sAdditionalIndividualDataSQL + sFROM_SQL 
									
				ELSE
					xlsFileNameSuffix =  ' DATOS BASICOS '
					XlsQuery = sIndividualDataSQL + sAdditionalIndividualDataSQL + sFROM_SQL 
				ENDIF
			ENDIF

			*Establece las condiciones de filtrado para la extracción de registros
			sWHERE_SQL = ' '
			IF !EMPTY(ALLTRIM(EventCode)) THEN 
				sWHERE_SQL = sWHERE_SQL + ' A.COD_EVE ="' + EventCode + '" AND '
			ENDIF 
			
			sWHERE_SQL = sWHERE_SQL + IIF(AT("#",sFilter)=0,sFilter,STRTRAN(sFilter,"#")) 
						*' AND B.act_siv = 1 ' +	strCondition1 + strCondition2 + ;
						' ORDER BY nreg INTO TABLE ' + (tmpTable) ;
						Esta parte fue modificada por solicitud del INS (Ver error con IdError=350)

			IF OCCURS('#',XlsQuery) > 0  THEN 
				*El query de exportación tiene placeholder, por tanto es reemplazado por la condición WHERE 
				XlsQuery = STRTRAN(XlsQuery, '#', sWHERE_SQL )
			ELSE 
				XlsQuery =  XlsQuery + ' WHERE ' + sWHERE_SQL
			ENDIF 
			XlsQuery =  XlsQuery + ' INTO TABLE ' + (tmpTable) + ' DATABASE SIVIGILATemp'

			* Se ejecuta el Query que extrae los datos de paciente y complementarios para exportar a Excel
			*=STRTOFILE(XlsQuery, ALLTRIM(EventCode) + "_ToXls.qpr")

			sOldDB = SET("Database")
			OPEN DATABASE SIVIGILATemp
			SET  DATABASE TO SIVIGILATemp
			TRY
				REMOVE TABLE (tmpTable) DELETE
			CATCH TO oException
			ENDTRY
			&XlsQuery 
			nRecordsToTraverse = RECCOUNT(tmpTable)

			WAIT 'Procesando ' + STR(nRecordsToTraverse) + ' registros del evento ' + EventCode WINDOW NOWAIT TIMEOUT 10
			*?'Inicio: ' + TIME()
*SET STEP ON 
			IF nRecordsToTraverse > 0
				* se encontraron registros. Se procesan  entonces los ajustes que quedarán en un cursor de nombre rsAdjustedCases;
				a partir de éste, se realiza la exportación

				nIndividualNotifications = _TALLY 
				
				sTagExpr = 'AÑO+SEMANA+COD_EVE+TIP_IDE_+NUM_IDE_+COD_PRE+COD_SUB'
				oAdjustmentsProcessor = NEWOBJECT("AdjustmentsProcessor","AdjustmentsProcessor.PRG")
				oAdjustmentsProcessor.sSourceTablename = tmpTable
				oAdjustmentsProcessor.sAdjustmentFieldName = 'AJUSTE_'
				oAdjustmentsProcessor.sAdjustmentDateFieldName = 'FEC_AJU_'
				IF EMPTY(sLinkerExpr) THEN
					oAdjustmentsProcessor.processAdjustments(sTagExpr)
				ELSE
					oAdjustmentsProcessor.processAdjustments(sLinkerExpr)
				ENDIF
				RELEASE oAdjustmentsProcessor
				
				SELECT rsAdjustedCases

				xlsFileNameSuffix = xlsFileNameSuffix + sSuffix 
				xlsFileName = This.sXlsFilePath + '\EVENTO ' + ALLTRIM(EventCode) + xlsFileNameSuffix 
				WITH oDataExporter 
					.sSourceTableName = 'rsAdjustedCases'
					.sExportationPath = This.sXlsFilePath
					.sExportedFileName = 'EVENTO '+ALLTRIM(EventCode) + xlsFileNameSuffix 
					.sExcludeFields = 'AJUSTE, FEC_AJU, AJUSTE_AUX'

					IF sOutputType = 'XLS' OR sOutputType = 'CSV' THEN 
						.exportToXLS(bDoNotIgnoreEmptyFiles)
					ELSE
						.exportToDBF(bDoNotIgnoreEmptyFiles)
					ENDIF 
				  	sResultMsg = '\EVENTO ' + ALLTRIM(EventCode) + xlsFileNameSuffix + '.' + CHRTRAN(.sExportedFileType,'5','S')
				ENDWITH 
				
				This.LogOutputFiles(oDataExporter, 'DB+DC')

				TRY
					REMOVE TABLE (tmpTable) DELETE
				CATCH TO oException
				ENDTRY
				SET DATABASE TO &sOldDB
*SET STEP ON 
				IF IsMemberOf('LABS', sReportsList) THEN 
					* Exporta la información de resultados de laboratorio con base en los registros que fueron ;
					consolidados en rsAdjustedCases. A este recordset se le agregan los resultados de laboratorio, si los hay.;
					El proceso se realiza en dos pasos.
					*?'Inicio: ' + TIME()
					*P1: Ajusta por separado los datos de laboratorio correspondientes a los casos consolidados en rsAdjustedCases
					sLabDataSQL = ' SELECT A.*,DTOC(A.FEC_EXA,1) AS Fecha_Examen, DTOC(A.FEC_REC,1) AS Fecha_Recepcion FROM Laboratorios A INNER JOIN rsAdjustedCases B ON ' +;
									' B.cod_eve = A.cod_eve AND B.año = A.año AND B.semana = A.semana AND B.tip_ide_ = A.tip_ide ' +;
									' AND B.num_ide_ = A.num_ide AND B.cod_pre = A.cod_pre AND B.cod_sub = A.cod_sub INTO CURSOR rsTargetLabs READWRITE' 
					&sLabDataSQL 
					sTagExpr = 'AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB+MUESTRA+PRUEBA+AGENTE+Fecha_Examen+Fecha_Recepcion'
					DO fastProcessAdjustments WITH "rsTargetLabs", sTagExpr, .T., ,	"rsAdjustedLabs", .F., .T. IN SIVIGILAUtilities			
					
					*P2: Encadena los datos ajustados de laboratorio con los casos consolidados en rsAdjustedCases
					XlsLabQuery = GetQryString("E" + PADL(oCurrentEvent.nFORMULARIO, 2, '0'), @EventTable, nMajor, nMinor, nBuild, , , , .T.)
					IF EMPTY(XlsLabQuery ) THEN
						*No se encontró un query definido de exportación para los datos de laboratorios del evento, por tanto, ;
						se usa el query por default	

						sLabDataSQL = ' SELECT A.control, A.fec_exa, A.fec_rec, A.muestra, ' +;
										' A.prueba, A.agente, A.resultado, A.fec_exp, A.valor, A.ajuste as lab_ajuste, B.*'

						sFROM_SQL =	' FROM rsAdjustedCases B LEFT OUTER JOIN rsAdjustedLabs A ON ' +;
										' B.cod_eve = A.cod_eve AND B.año = A.año AND B.semana = A.semana AND ' +;
						   					' B.tip_ide_ = A.tip_ide AND B.num_ide_ = A.num_ide AND ' +;
						   					' B.cod_pre = A.cod_pre AND B.cod_sub = A.cod_sub ' 

						XlsLabQuery =  sLabDataSQL + sFROM_SQL 
					ENDIF 
					XlsLabQuery = XlsLabQuery + ' INTO TABLE ' + (tmpTable) 
					&XlsLabQuery
					SELECT (tmpTable)
					WAIT 'Procesando ' + STR(RECCOUNT(tmpTable)) + ' registros de LABORATORIOS'  WINDOW	NOWAIT TIMEOUT 10
					
					xlsFileNameSuffix = ' LABORATORIOS Y DATOS BASICOS' + sSuffix  
					xlsFileName = This.sXlsFilePath + '\EVENTO ' + ALLTRIM(EventCode) + xlsFileNameSuffix 
					
					WITH oDataExporter
						.sSourceTableName = tmpTable
						.sExportationPath = This.sXlsFilePath
						.sExportedFileName = 'EVENTO '+ALLTRIM(EventCode) + xlsFileNameSuffix 
						.sExcludeFields = 'AJUSTE, FEC_AJU, AJUSTE_AUX'
						.exportToXLS(bDoNotIgnoreEmptyFiles)
						*?'Final: ' + TIME()
						sResultMsg = sResultMsg + CHR(13) + '\EVENTO '+ALLTRIM(EventCode) + xlsFileNameSuffix  + '.' +;
									CHRTRAN(.sExportedFileType,'5','S')
					ENDWITH				
					This.LogOutputFiles(oDataExporter, 'LAB+DB')
				ENDIF 
*SET STEP ON
				IF IsMemberOf('UPGD', sReportsList) THEN 
					*Exporta la información de caracterización de UPGDs desde donde se tomaron los datos
					*?'Inicio: ' + TIME()
					sTableLongName=''
					tmpTableUPGD = 'tmpXlsUPGD'
					XlsQuery = GetQryString('UPGD',sTableLongName, nMajor, nMinor, nBuild)
					sWHERE_SQL = ' WHERE COD_PRE + COD_SUB IN (SELECT DISTINCT COD_PRE + COD_SUB FROM rsAdjustedCases) '
					XlsQuery = XlsQuery + sWHERE_SQL + ' INTO TABLE ' + (tmpTableUPGD) 
					&XlsQuery
					WAIT 'Procesando ' + STR(RECCOUNT(tmpTableUPGD)) + ' registros de UPGDs'  WINDOW	NOWAIT TIMEOUT 10
							
					SELECT (tmpTableUPGD)

					xlsFileNameSuffix = ' UPGD' + sSuffix  
					xlsFileName = This.sXlsFilePath + '\EVENTO '+ALLTRIM(EventCode) + xlsFileNameSuffix 
					oDataExporter.sSourceTableName = tmpTableUPGD
					oDataExporter.sExportationPath = This.sXlsFilePath
					oDataExporter.sExportedFileName = 'EVENTO '+ALLTRIM(EventCode) + xlsFileNameSuffix 
					oDataExporter.exportToXLS(bDoNotIgnoreEmptyFiles)
					This.LogOutputFiles(oDataExporter, 'UPGD')

					IF IsMemberOf('UPGD_UCI', sReportsList) THEN 
						sWHERE_SQL = ' WHERE COD_PRE + COD_SUB IN (SELECT DISTINCT COD_PRE + COD_SUB FROM ' + tmpTableUPGD + ') '

						XlsQuery = 'SELECT B.raz_soc AS nom_upgd, C.nom_dep AS ndep_notif, D.nom_mun AS nmun_notif,A.* FROM ' +;
									'(SELECT cod_pre,cod_sub,id_uci,tipo_uci,tot_uci,comp_espac,n_cam_inte,sub_tipuci,activa,ajuste,fec_aju FROM ' +;
									'UPGD_UCIS ' + sWHERE_SQL + ') AS A LEFT OUTER JOIN UPGD B ON A.COD_PRE=B.COD_PRE AND A.COD_SUB=B.COD_SUB ' +;
									'LEFT OUTER JOIN DEPTOS C ON LEFT(B.cod_pre,2) = C.cod_dep LEFT OUTER JOIN MUNICIPIOS D ON LEFT(B.cod_pre,5) = D.cod_mun ' +;
									' INTO TABLE ' + (tmpTable) 
						&XlsQuery
						SELECT (tmpTable)
						xlsFileNameSuffix = ' UPGD_UCI' + sSuffix  
						oDataExporter.sSourceTableName = tmpTable
						oDataExporter.sExportedFileName = 'EVENTO '+ALLTRIM(EventCode) + xlsFileNameSuffix 
						oDataExporter.exportToXLS(bDoNotIgnoreEmptyFiles)
						
						This.LogOutputFiles(oDataExporter, 'UPGD_UCI')
					ENDIF 

					IF IsMemberOf('UPGD_TALENTO_HUMANO', sReportsList) THEN 
						sWHERE_SQL = ' WHERE COD_PRE + COD_SUB IN (SELECT DISTINCT COD_PRE + COD_SUB FROM ' + tmpTableUPGD + ') '
						
						XlsQuery = 'SELECT B.raz_soc AS nom_upgd, C.nom_dep AS ndep_notif, D.nom_mun AS nmun_notif, A.* FROM ' +;
									'(SELECT DISTINCT cod_pre,cod_sub,cod_art,LTRIM(valor,1,"0") as valor FROM ' +;
									'TAL_HUM ' + sWHERE_SQL + ') AS A LEFT OUTER JOIN UPGD B ON A.COD_PRE=B.COD_PRE AND A.COD_SUB=B.COD_SUB  ' +;
									'LEFT OUTER JOIN DEPTOS C ON LEFT(B.cod_pre,2) = C.cod_dep LEFT OUTER JOIN MUNICIPIOS D ON LEFT(B.cod_pre,5) = D.cod_mun ' +;
									' INTO TABLE ' + (tmpTable) 
						&XlsQuery
						
						SELECT (tmpTable)
						xlsFileNameSuffix = ' UPGD_TALENTO_HUMANO' + sSuffix  
						oDataExporter.sSourceTableName = tmpTable
						oDataExporter.sExportedFileName = 'EVENTO '+ALLTRIM(EventCode) + xlsFileNameSuffix 
						oDataExporter.exportToXLS(bDoNotIgnoreEmptyFiles)
						
						This.LogOutputFiles(oDataExporter, 'TAL_HUM')
					ENDIF 
					*?'Final: ' + TIME()
					sResultMsg = sResultMsg + CHR(13) + '\EVENTO '+ALLTRIM(EventCode)+' UPGD' + '.' + CHRTRAN(oDataExporter.sExportedFileType,'5','S')
				ENDIF 
*SET STEP ON 						
				*Ejecuta post comandos de exportación si es que existen
		        IF !EMPTY(sPostCmds) THEN
					&sPostCmds
				ENDIF

				DO DeleteTable WITH (tmpTable), .T. IN TransferDataHandler			
				*USE
				*DELETE FILE (tmpTable)
			ENDIF

			TRY
				REMOVE TABLE (tmpTable) DELETE
			CATCH TO oException
			ENDTRY
			SET DATABASE TO &sOldDB

			*?'Final: ' + TIME()
*SET STEP ON 	 
			* Exporta la información de resultados de laboratorio que no tienen datos básicos asociados
			*?'Inicio: ' + TIME()
			IF 'LABS_HUERFANOS' $ sReportsList THEN 
				tmpTable = 'tmpXLS'
				WAIT 'Procesando LABORATORIOS sin datos básicos'  WINDOW NOWAIT TIMEOUT 10

				IF VARTYPE(nDiscriminateEvents) = 'N' THEN
					IF nDiscriminateEvents = 1 THEN
						*Aprovecha la existencia del cursor rsLaboratoriosHuerfanos que ya contiene todos los registros huerfanos de todos los eventos
						sLabDataSQL = 'SELECT A.* FROM rsLaboratoriosHuerfanos A  '

						*Establece las condiciones de filtrado para la extracción de registros
						IF !EMPTY(EventCode) THEN 
							sWHERE_SQL = ' WHERE A.COD_EVE ="' + IIF(!EMPTY(ALLTRIM(EventCode)),EventCode,ALLTRIM(EventCode)) +  '" AND ' + ;
															IIF(AT("#",sFilter)=0,sFilter,SUBSTR(sFilter,1,AT("#",sFilter)-1)) + ' INTO TABLE ' + (tmpTable) 
						ELSE 
							sWHERE_SQL = ' WHERE .T. ' + ' INTO TABLE ' + (tmpTable) 
						ENDIF
						XlsQuery =  sLabDataSQL + sWHERE_SQL 
					ELSE
						*Construye un query que selecciona solo los registros que sean estrictamente necesarios. El conunto de registros ;
						estará determinado por sFilter
						sLabDataSQL = ' SELECT A.cod_eve, A.semana, A.año, A.cod_pre, A.cod_sub, A.tip_ide, A.num_ide,' +;
										' A.pri_nom, A.seg_nom, A.pri_ape, A.seg_ape, ' +;
										' A.control, A.telefono, A.Direccion, A.tip_reg_sa, A.cod_ase, A.fec_exa, A.fec_rec, A.muestra, ' +;
										' A.prueba, A.agente, A.resultado, A.fec_exp, A.valor, B.raz_soc AS nom_upgd, ' +;
										' I.nom_mun AS nmun_notif, J.nom_dep AS ndep_notif '

						sFROM_SQL =	' FROM Laboratorios A LEFT OUTER JOIN UPGD B ON  A.cod_pre = B.cod_pre AND  A.cod_sub = B.cod_sub ' +;
									' LEFT OUTER JOIN MUNICIPIOS I  ON  LEFT(B.cod_pre,5) = I.cod_mun ' +;
									' LEFT OUTER JOIN DEPTOS J  ON  LEFT(B.cod_pre,2) = J.cod_dep ' +;
									' LEFT OUTER JOIN PACIENTE C ON ' +;
									' C.cod_eve = A.cod_eve AND C.año = A.año AND C.semana = A.semana AND ' +;
				   					' C.tip_ide = A.tip_ide AND C.num_ide = A.num_ide AND ' +;
				   					' C.cod_pre = A.cod_pre AND C.cod_sub = A.cod_sub '

						*Establece las condiciones de filtrado para la extracción de registros
						sWHERE_SQL = ' WHERE A.COD_EVE ="' + IIF(!EMPTY(ALLTRIM(EventCode)),EventCode,ALLTRIM(EventCode)) +  '" AND ' + ;
														IIF(AT("#",sFilter)=0,sFilter,SUBSTR(sFilter,1,AT("#",sFilter)-1)) + ' AND C.COD_PRE IS NULL INTO TABLE ' + (tmpTable) 

						XlsQuery =  sLabDataSQL + sFROM_SQL + sWHERE_SQL 
					ENDIF
				ENDIF 
				*=STRTOFILE(XlsQuery, ALLTRIM(EventCode) + "_UnidadesLSP_ToXls.qpr")
	*SET STEP ON 				
				TRY
					&XlsQuery
					nOrphansLabsNotifications = _TALLY 
				CATCH TO oException
					nOrphansLabsNotifications = 0					
				ENDTRY
				
				
				IF nOrphansLabsNotifications > 0 THEN
					SELECT (tmpTable)
					WAIT 'Procesando ' + STR(RECCOUNT(tmpTable)) + ' registros de LABORATORIOS'  WINDOW	NOWAIT TIMEOUT 10
					
					xlsFileNameSuffix = ' NOTIFICADOS POR LABORATORIOS SIN DATOS BASICOS' + sSuffix  
					xlsFileName = This.sXlsFilePath + '\EVENTO '+ALLTRIM(EventCode) + xlsFileNameSuffix 
					oDataExporter.sSourceTableName = tmpTable
					oDataExporter.sExportationPath = This.sXlsFilePath
					oDataExporter.sExportedFileName = 'EVENTO '+ALLTRIM(EventCode) + xlsFileNameSuffix 
					oDataExporter.exportToXLS(bDoNotIgnoreEmptyFiles)
					
					This.LogOutputFiles(oDataExporter, 'LABS')
					*?'Final: ' + TIME()
					sResultMsg = sResultMsg + CHR(13) + '\EVENTO '+ALLTRIM(EventCode) + xlsFileNameSuffix + '.' +;
								CHRTRAN(oDataExporter.sExportedFileType,'5','S')
				ENDIF
			ENDIF
			
			IF (nIndividualNotifications + nOrphansLabsNotifications) > 0 
				sResultMsg = 'Se han generado los siguientes archivos'+CHR(13)+;
							sResultMsg + CHR(13) + CHR(13)+'En la carpeta: ' + This.sXlsFilePath
				IF !bBeSilent THEN
					MESSAGEBOX(sResultMsg,0+48+256,'SIVIGILA')
				ELSE
					sResultMsg= 'Evento ' + EventCode + CR_LF + sResultMsg + CR_LF + CR_LF
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF
			ELSE
				sResultMsg = 'No se encontraron registros que satisfagan los criterios'
				IF !bBeSilent THEN
					SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
			      	=showErrorMessage(sResultMsg,0)
				ELSE
					sResultMsg= 'Evento ' + EventCode + CR_LF + sResultMsg + CR_LF + CR_LF
					=FPUTS(nResultsFileHandler,sResultMsg)
				ENDIF					
			ENDIF
			*?'Final: ' + TIME()

		ENDIF
	ENDIF
	DO DeleteTable WITH (tmpTable), .T. IN TransferDataHandler

	sEndTime=TIME()

	sNotificatioMsg='Se han generado los archivos excel de retroalimentación para análisis.' + CHR(13) + + CHR(13) +;
					'Los archivos han sido ubicados en la carpeta: ' + This.sXlsFilePath
	IF !bBeSilent THEN
		SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		=showErrorMessage(sNotificatioMsg, 0)
	ELSE 
		=FPUTS(nResultsFileHandler,sNotificatioMsg)
		=FPUTS(nResultsFileHandler,'Hora de inicio: ' + sInitTime)
		=FPUTS(nResultsFileHandler,'Hora de finalización: ' + sEndTime)			
		=FCLOSE(nResultsFileHandler)
	ENDIF

	SET TALK OFF
	*RELEASE WINDOWS msgWindow 
	RELEASE oCurrentEvent, oDataExporter 

ENDPROC


PROCEDURE printContactsReport( sFilter as string)

	LOCAL nOldSelected as Number
	nOldSelected = SELECT()
	
	IF VARTYPE(sFilter)='C' AND !EMPTY(sFilter) THEN 
	ELSE 
		sFilter = '.T.'
	ENDIF 
	SELECT * FROM BDSivigila!vEspecContactos WHERE &sFilter INTO CURSOR rsContactos_a_Imprimir
	
	SELECT rsContactos_a_Imprimir
	REPORT FORM REPORTES\rptContacts.frx TO PRINTER PROMPT PREVIEW

	USE IN rsContactos_a_Imprimir
	
	SELECT (nOldSelected)
ENDPROC 

FUNCTION EventClasificationToWords(sClasificationId as string) as String

	LOCAL sClasificationName as String 
	sClasificationName = ''
	
	DO CASE 
		CASE sClasificationId = '1'
			sClasificationName = 'Sospechoso'
		CASE sClasificationId = '2'
			sClasificationName = 'Probable'
		CASE sClasificationId = '3'
			sClasificationName = 'Confirmado por Laboratorio'
		CASE sClasificationId = '4'
			sClasificationName = 'Confirmado por Clínica'
		CASE sClasificationId = '5'
			sClasificationName = 'Confirmado por Nexo epidemiológio'
		CASE sClasificationId = '6'
			sClasificationName = 'Descartado'
		CASE sClasificationId = '7'
			sClasificationName = 'Otro ajuste'
		CASE sClasificationId = 'D'
			sClasificationName = 'Descarte por error de digitacion'
		OTHERWISE
			sClasificationName = 'Desconocido'
	ENDCASE
	RETURN sClasificationName 

ENDFUNC


HIDDEN PROCEDURE LogOutputFiles(oDataExporter as Object, sSivigilaFileType)

	WITH This
		.nOutputFiles = .nOutputFiles + 1
		DIMENSION This.aOutputFiles(.nOutputFiles,4)
		
		.aOutputFiles[.nOutputFiles,1] = oDataExporter.sExportedFileName
		.aOutputFiles[.nOutputFiles,2] = oDataExporter.sExportationPath 
		.aOutputFiles[.nOutputFiles,3] = sSivigilaFileType
		.aOutputFiles[.nOutputFiles,4] = oDataExporter.sExportedFileType
	ENDWITH 
ENDPROC 

HIDDEN PROCEDURE reset_aOutputFiles()

	WITH This
		.nOutputFiles = 0
		DIMENSION .aOutputFiles(1,4)
		
		.aOutputFiles[1,1] = .F.
		.aOutputFiles[1,2] = .F.
		.aOutputFiles[1,3] = .F.
		.aOutputFiles[1,4] = .F.
	ENDWITH 
ENDPROC 

ENDDEFINE && SIVIGILAReporter 

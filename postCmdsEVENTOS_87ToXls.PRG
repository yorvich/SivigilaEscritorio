*Ejecuta post comandos para los eventos que se almacenan en la tabla EVENTOS_87

#INCLUDE SIVIGILA.H

LPARAMETERS sExportationPath AS String 
*Ruta hacia la cual deben exportarse los archivos excel resultantes del post-procesamiento

LOCAL oDataExporter as Object
oDataExporter=NEWOBJECT("DataExporter","DataExporter.Prg",.null.)

*Selecciona notificaciones mensuales del evento 357 (individual) que no tengan una notificación colectiva asociada
SELECT rsIndividualCasesByMonth.* FROM ;
	(SELECT AÑO, COD_PRE, COD_SUB, ALLTRIM(STR(MONTH(FEC_DIAG))) AS MES, COUNT(*) AS N_CASOS ;
	FROM rsAdjustedCases ;
	WHERE AÑO =ALLTRIM(STR(YEAR(DATE()))) AND AJUSTE_ != CLINICAL_DISCARD_ADJUSTMENT AND AJUSTE_ != CAPTURE_DISCARD_ADJUSTMENT ;
	GROUP BY AÑO, MES, COD_PRE, COD_SUB ) rsIndividualCasesByMonth ;
	 LEFT OUTER JOIN EVENTOS_88 ON ;
	 rsIndividualCasesByMonth.AÑO=EVENTOS_88.AÑO ;
	 AND rsIndividualCasesByMonth.COD_PRE=EVENTOS_88.COD_PRE ;
	 AND rsIndividualCasesByMonth.COD_SUB=EVENTOS_88.COD_SUB ;
	 AND rsIndividualCasesByMonth.MES=EVENTOS_88.MES ;
	 WHERE EVENTOS_88.MES IS NULL ;
	 INTO CURSOR rsMissingNotificationsByMonth

IF _TALLY>0 THEN
	WITH oDataExporter
	    xlsFileName = 'EVENTO 357_NOTIFICACIONES FALTANTES EN 359'
	    .sSourceTableName = 'rsMissingNotificationsByMonth'
		.sExportationPath = sExportationPath
		.sExportedFileName = xlsFileName 
		.exportToXLS(.F.)			
	ENDWITH 
ENDIF

SELECT rsMissingNotificationsByMonth
USE

RELEASE oDataExporter
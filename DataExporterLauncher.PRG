#INCLUDE SIVIGILA.h

LPARAMETERS sTableNameToExport as String, sExportedFileName AS String, sExportationPath as String

LOCAL oDataExporter as Object 

sSetProc = 'SET PROCEDURE TO ' + PATH_TO_COMMON_LIB + '\TablesHandler ADDITIVE'
&sSetProc

=SelectTable(sTableNameToExport,,.F.)
oDataExporter = NEWOBJECT("DataExporter", PATH_TO_COMMON_LIB + "DataExporter.Prg",.null.,sTableNameToExport)
IF VARTYPE(sExportationPath) = 'C'  THEN
	oDataExporter.sExportationPath = sExportationPath
ENDIF

WITH oDataExporter
	.sExportedFileName = sExportedFileName 
	.exportToXLS()
	.ShowXLSExportationResults()
ENDWITH

RELEASE oDataExporter 
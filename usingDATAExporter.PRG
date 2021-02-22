*SET STEP ON 

oDataExporter=NEWOBJECT("DataExporter","DataExporter.Prg",.null.,'PACIENTE')
oDataExporter.sExportationPath = 'C:\Users\wilson.aguilar\Documents\SIVIGILA 2012\ARCHIVOS XLS'
oDataExporter.sExportedFileName="Resultado"
oDataExporter.exportToXLS()


IF oDataExporter.nInitRecs>0 THEN
	oDataExporter.ShowXLSExportationResults()
ELSE
	WAIT  "No se encontraron registros para exportar" WINDOW NOWAIT TIMEOUT 5
ENDIF

RELEASE oDataExporter


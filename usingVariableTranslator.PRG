SET STEP ON 

*oVariableTranslator = NEWOBJECT("VariableTranslator","AnomaliesSolver.Prg",.null.,'EVENTOS_07')

*oVariableTranslator.sXlsFilePath = 'C:\Users\wilson.aguilar\Documents\SIVIGILA 2012\ARCHIVOS XLS'
*oVariableTranslator.translateVariables('850',.T.)

*IF oVariableTranslator.nUpdatedRecs>0 THEN
*	oVariableTranslator.ShowResults()
*ELSE
*	WAIT  "No se encontraron registros susceptibles de ser homologados" WINDOW NOWAIT TIMEOUT 5
*ENDIF

oVariableTranslator = NEWOBJECT("VariableTranslator","AnomaliesSolver.Prg",.null.,'PACIENTE')

oVariableTranslator.sXlsFilePath = 'C:\Users\wilson.aguilar\Documents\SIVIGILA 2012\ARCHIVOS XLS'
oVariableTranslator.translateVariablesEventFree

IF oVariableTranslator.nUpdatedRecs>0 THEN
	oVariableTranslator.ShowResults()
ELSE
	WAIT  "No se encontraron registros susceptibles de ser homologados" WINDOW NOWAIT TIMEOUT 5
ENDIF

RELEASE oVariableTranslator 


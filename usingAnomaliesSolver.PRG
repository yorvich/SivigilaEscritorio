SET STEP ON 

*oForm=NEWOBJECT("AnomaliesSolverUI","AnomaliesSolverUI.VCX")
*oForm.Visible=.t.

*oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,'C:\INS\SIVIGILA\PACIENTE')
oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,'PACIENTE')

*This next to exemplify how to initialize the object for dealing with a cursor
*SELECT * FROM paciente INTO CURSOR rsDuplicatesTst READWRITE 
*oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,'rsDuplicatesTst',.T.)

oAnomaliesSolver.sXlsFilePath = 'C:\Users\wilso\Documents\SIVIGILA - 2018\ARCHIVOS XLS'
*oAnomaliesSolver.sExcludedFields = " NUM_REG SUBE "
oAnomaliesSolver.deleteDuplicates()
*oAnomaliesSolver.computeEpidemiologicalWeek


IF oAnomaliesSolver.nDeletedRecs>0 THEN
	oAnomaliesSolver.ShowResults()
ELSE
	WAIT  "No se encontraron registros duplicados" WINDOW NOWAIT TIMEOUT 5
ENDIF

RELEASE oAnomaliesSolver


PROCEDURE myDuplicatesSolverProc

	LPARAMETERS sSourceTableName as String, sRecordsToDeleteRSName as String, sDuplicatesCountingRSName as String
	
	RETURN
ENDPROC


PROCEDURE variableTranslatorTst

SET STEP ON 

	oVariableTranslator = NEWOBJECT("VariableTranslator","AnomaliesSolver.Prg",.NULL.,'EVENTOS_09')
	oVariableTranslator.sXlsFilePath="c:\temp"
	*oVariableTranslator.sFilterExpresion = "FechaCarga=DATE()"
	oVariableTranslator.translateVariables('550' ,.T.)

	IF oVariableTranslator.nUpdatedRecs>0 THEN
		oVariableTranslator.ShowResults()
	ENDIF
	RELEASE oVariableTranslator 

ENDPROC

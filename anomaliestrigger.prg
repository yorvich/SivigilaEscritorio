LPARAMETERS sCleaningMethod as String, xlsFilesPath as String, sPreCommands as String, sPostCommands as String

IF VARTYPE(sPreCommands)='C' THEN
	&sPreCommands
ENDIF

oTriggerForm=NEWOBJECT("AnomaliesSolverUI","AnomaliesSolverUI.VCX")

oTriggerForm.xlsFilesPath=xlsFilesPath
oTriggerForm.SolverMethod=sCleaningMethod 

oTriggerForm.Show

IF VARTYPE(sPostCommands)='C' THEN
	&sPostCommands 
ENDIF

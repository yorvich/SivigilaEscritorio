LPARAMETERS sFilter as String, bBeSilent as String, sPreCommands as String, sPostCommands as String

IF VARTYPE(sPreCommands)='C' THEN
	&sPreCommands
ENDIF

SET PROCEDURE TO SivigilaMessenger ADDITIVE
SET PROCEDURE TO AnomaliesHandler ADDITIVE

* Resolver duplicidades antes de hacer el análisis sobre las tablas
showProgressMessage('Resolviendo duplicados en la Tabla UPGDs')
solveDuplicates('UPGD', _RTA_XLS)

showProgressMessage('Verificando Notificaciones')

oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg")
oAnomaliesSolver.sXlsFilePath = _RTA_XLS
* Se invoca al procedimiento que analiza las notificaciones
oAnomaliesSolver.verifyUpgdsNotification("diagnóstico_upgds",sFilter, bBeSilent )
RELEASE oAnomaliesSolver

IF VARTYPE(sPostCommands)='C' THEN
	&sPostCommands 
ENDIF



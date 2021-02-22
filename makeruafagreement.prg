#include DevEnvironment.h

sOldCurrentDir = SET("DIRECTORY")
IF FILE('SivigilaETL.app') THEN
	sCmd = 'DO SivigilaETL.app'
ELSE
	SET DEFAULT TO PATH_TO_SIVIGILAAgreements
	sCmd = 'DO FORM ' + PATH_TO_SIVIGILAAgreements + 'frmETLOrchestrator'
ENDIF
&sCmd

SET DEFAULT TO '&sOldCurrentDir'
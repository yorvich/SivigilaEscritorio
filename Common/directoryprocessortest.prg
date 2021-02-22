LPARAMETERS sTargetDir as String

LOCAL oDirProcessor AS Object

SET STEP ON 
oDirProcessor =NEWOBJECT("DirectoryProcessor","DirectoryProcessor.PRG")
oDirProcessor.SetupDPInitData('.\DataDirectoryProcessor')
oDirProcessor.dirListing(sTargetDir)

nOldDataSession = SET("Datasession")
SET DATASESSION TO oDirProcessor.nDataSessionId 
IF RECCOUNT(oDirProcessor.sDirListingTableName)> 0 THEN
	SELECT (oDirProcessor.sDirListingTableName)
	BROWSE
ENDIF
SET DATASESSION TO (nOldDataSession) 
RELEASE oDirProcessor


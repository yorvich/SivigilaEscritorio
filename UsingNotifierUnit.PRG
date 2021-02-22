LPARAMETERS bAddToSCREEN AS Boolean

SET STEP ON 

IF bAddToSCREEN THEN
	SET PROCEDURE TO NotifierUnit ADDITIVE
	_SCREEN.AddObject('oNotifierUnit',"NotifierUnit")

	*oNotifierUnit=NEWOBJECT("NotifierUnit","NotifierUnit.Prg")
	WITH _SCREEN.oNotifierUnit
		?.sID
		?.sName
		?.sType
		?.bIsLaboratory
		?.bIsMilitary
		?.bIsBloodBank
		?.sDrive
		?.sBackupDrive
		?.nBackupPeriod
		?.sCurrentYear
		?.sCurrentWeek
	ENDWITH
	_SCREEN.RemoveObject('oNotifierUnit')
ELSE
	oNotifierUnit=NEWOBJECT("NotifierUnit","NotifierUnit.Prg")
	WITH oNotifierUnit
		?.sID
		?.sName
		?.sType
		?.bIsLaboratory
		?.bIsMilitary
		?.bIsBloodBank
		?.sDrive
		?.sBackupDrive
		?.nBackupPeriod
		?.sCurrentYear
		?.sCurrentWeek
	ENDWITH
	RELEASE oNotifierUnit
ENDIF



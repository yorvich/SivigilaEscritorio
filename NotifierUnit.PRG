DEFINE CLASS NotifierUnit as Custom

sSourceTableName = 'GENERAL'
sDefaultKey = 'UNI_NOT'

sID = .NULL.
sName = .NULL.
sType = .NULL.
bIsLaboratory = .NULL.
bIsMilitary = .NULL.
bIsBloodBank = .NULL.
bISEAPB = .NULL.
sDrive = .NULL.
sBackupDrive = .NULL.
nBackupPeriod = .NULL.
sCurrentYear = .NULL.
sCurrentWeek = .NULL.
sStateCode = .NULL.
bIsUND = .NULL.
bIsUPGD = .NULL.

sMacAdresses = ''
sIPAdresses = ''

FUNCTION Init(bSetMacAddress as Boolean, bSetIPAddress as Boolean )
	
	LOCAL sSQLCmd as String
	
	SET PROCEDURE TO SivigilaSystem ADDITIVE 
	
	sSQLCmd = 'SELECT * FROM ' + This.sSourceTableName + ' INTO CURSOR rsNotifierUnitDescription'
	&sSQLCmd
	IF _TALLY > 0 THEN
		WITH THIS
			.sID = rsNotifierUnitDescription.UNI_NOT
			.sName = rsNotifierUnitDescription.RAZ_SOC
			.sType = rsNotifierUnitDescription.TIP_UNI
			.bIsLaboratory = (rsNotifierUnitDescription.LSP = 1)
			.bIsMilitary = (rsNotifierUnitDescription.FM = 1)
			.bIsBloodBank = (rsNotifierUnitDescription.BAN_SANGRE = 1)
			.bISEAPB = (rsNotifierUnitDescription.EAPB = 1)
			.sDrive = rsNotifierUnitDescription.UNIDAD
			.sBackupDrive = rsNotifierUnitDescription.UND_BCK
			.nBackupPeriod = rsNotifierUnitDescription.DIA_BCK
			.sCurrentYear = rsNotifierUnitDescription.VIGENCIA
			.sCurrentWeek = rsNotifierUnitDescription.SEMANA
			.sStateCode = LEFT(rsNotifierUnitDescription.UNI_NOT,2)
			.bIsUND= IIF(rsNotifierUnitDescription.TIP_UNI='3',.T.,.F.)
			.bIsUPGD = IIF(rsNotifierUnitDescription.TIP_UNI='1',.T.,.F.)
			
			IF bSetMacAddress THEN 
				.sMacAdresses = getMacAddress()
			ENDIF 
			IF bSetIPAddress THEN 
				.sIPAdresses = getIPAddress()
			ENDIF 
			
		ENDWITH
	ENDIF
	
ENDFUNC


FUNCTION Destroy
ENDFUNC

PROCEDURE Refresh()
	This.Init(.T.)
ENDPROC

ENDDEFINE


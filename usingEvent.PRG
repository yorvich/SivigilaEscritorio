LPARAMETERS sEventCode as String, seed as String
*SET STEP ON 
CLEAR
oEvent=NEWOBJECT("Event","Event.Prg",.NULL.,sEventCode)
WITH oEvent
	?.sCOD_EVE
	?.sNOM_EVE
	?.sINMEDIATA
	?.nTIP_NOT
	?.sFORMULARIO
	?.nVERSION
	?.sCLA_PER
	sClasificationsInWords = .clasificationsToWords()
	?sClasificationsInWords
	?.sGRU_PER
	?.nEXA_LAB
	?.nCON_FINAL
	?.sVAL_SEM
	?.nFLU_NOT
	?.nEVE_NAL
	?.dFEC_OBLIGA
	?.bINICIO_SIN
	?.sESTADO_EVE
	?.bACTIVO
	?.bADMITE_NI
	?.nLATENCIA_DC
	?.sAJUSTES_NA
	?.sEDAD_MAX
	?.sUNIDAD_MED
	?.bLAB_INCRUS
	?.sDC_FORM
	?.sDC_TABLA
	?.bSUPER_FAST
	?.sCONDICION_SUPER_FAST
	?.nDIAS_AJUSTE
	?.sCLASIF_INICIAL
	?.sACTUALIZABLES_POR_CONFIRMACION

	?.bHasComplementaryData
ENDWITH

nPriority = 0
IF oEvent.checkEpidemiologicalWeek(seed,@nPriority) THEN 
	?'Semana determinada por ' + seed + ' de prioridad ' + ALLTRIM(STR(nPriority))
ELSE
	?'Semana no determinada por ' + seed + ' El valor de prioridad permaneció en ' + ALLTRIM(STR(nPriority))
ENDIF

?'Maneja contactos: ' + TRANSFORM(oEvent.ManageContacts(),'Y')

oDependencies = oEvent.GetRetainigFields('3')
IF !ISNULL(oDependencies ) THEN 
	?'El número de campos dependientes del ajuste 3 es ' + ALLTRIM(STR(oDependencies.nSize )) + ' ' + oDependencies.get(1) + ' ...'
ELSE
	?'No hay dependencias del ajuste 3'
ENDIF

oDependencies = oEvent.GetRetainigFields('D')
IF !ISNULL(oDependencies ) THEN 
	?'El número de campos dependientes del ajuste D es ' + ALLTRIM(STR(oDependencies.nSize )) + ' ' + oDependencies.get(1) + ' ...'
ELSE
	?'No hay dependencias del ajuste D'
ENDIF

oDependencies = oEvent.GetRetainigFields('0')
IF !ISNULL(oDependencies ) THEN 
	?'El número de campos dependientes del ajuste 0 es ' + ALLTRIM(STR(oDependencies.nSize )) + ' ' + oDependencies.get(1) + ' ...'
ELSE
	?'No hay dependencias del ajuste 0'
ENDIF

RELEASE oEvent



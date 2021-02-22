LPARAMETERS sEventCode as string, sBDKey as String
SET STEP ON 

CLEAR
oComplementaryData = NEWOBJECT("IndividualComplementaryData","cls_IndividualComplementaryData.fxp",.NULL.,sEventCode,sBDKey)
*oComplementaryData.ToMem()
WITH oComplementaryData 
	?.sCOD_EVE
	?.sAJUSTE
	?.dFEC_AJU
ENDWITH
RELEASE oComplementaryData 



LPARAMETERS sBDKey as String
*SET STEP ON 

CLEAR
oBD=NEWOBJECT("BasicData","cls_BasicData.fxp",.NULL.,sBDKey)
WITH oBD
	?.sCOD_EVE
ENDWITH

?oBD.sSystemUpdates 

*?oBD.unCodify('C003C0') &&Corresponde a nombre completo + cod_se + tip_ss
*?oBD.unCodify('C00FC0') &&Corresponde a nombre completo + cod_se + tip_ss + documento identidad
?oBD.unCodify('C80')

RELEASE oBD



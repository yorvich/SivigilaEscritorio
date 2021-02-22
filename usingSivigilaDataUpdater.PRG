LOCAL oSIVIGILADataUpdater AS Object 


SET PROCEDURE TO SIVIGILAMessenger ADDITIVE

oSIVIGILADataUpdater = NEWOBJECT("SIVIGILADataUpdater","SIVIGILADataUpdater.Prg")

SET DELETED ON 
SET TALK ON

*SET STEP ON 

SET PROCEDURE TO SIVIGILAExternalDataLoader ADDITIVE
sPostCmds = "DELETE FOR TIP_IDE='AS' OR TIP_IDE='MS' OR COD_ASE='NULL' OR EMPTY(COD_ASE)"
IF loadTxtFile('','Seleccione el archivo de cambios suministrados por aseguradoras','tbBDUA','TAB', .T., ;
				'PRI_NOM,SEG_NOM,PRI_APE,SEG_APE,TIP_SS,COD_ASE,FEC_CON,FECHA_NTO', sPostCmds) THEN
	*oSIVIGILADataUpdater.updateBasicDataFieldFrom('PACIENTE', 'tbBDUA', 'COD_ASE') 
	oSIVIGILADataUpdater.updateBasicDataFieldFrom('PACIENTE', 'tbBDUA', 'COD_ASE,TIP_SS') 
ENDIF

SET TALK OFF 
SET DELETED OFF 
IF  EMPTY(oSIVIGILADataUpdater.sErrMsg) THEN
	sErrorMsg = "Actualización terminada con éxito"
ELSE
	sErrorMsg = oSIVIGILADataUpdater.sErrMsg 
ENDIF
=showErrorMessage(sErrorMsg, 0)

RELEASE oSIVIGILADataUpdater

LPARAMETERS CD_TABLE_NAME as String, sFilter as String

*Efectua un procesamiento de ajustes en los registros de la tabla CD_TABLE_NAME que cumplan la condición sFilter. El procesamiento efectuado obedece;
los criterios de configuración establecidos para CD_TABLE_NAME en Tables.SIVIGILAAdjustmentsSetup El recordset resultante que contiene los ;
registros ajustados queda con nombre rsAdjustedCases

LOCAL sTagExpr AS String, sOrdenProc AS String 
LOCAL oAdjustmentsProcessor AS Object
LOCAL bAdjustmentsDone as Boolean 

SET PROCEDURE TO SIVIGILAUtilities ADDITIVE

* Recupera la configuración de ajustes para el evento en la tabla de configuración
sTagExpr = fieldValue("TAG_EXPR", "SIVIGILAAdjustmentsSetup", "TABLE_NAME == '" + CD_TABLE_NAME + "'")
sTagExpr = ALLTRIM(sTagExpr)
sOrdenProc = ALLTRIM(fieldValue("ORDEN_PROC", "SIVIGILAAdjustmentsSetup", "TABLE_NAME == '" + CD_TABLE_NAME + "'"))

* Procesa los ajustes para la tabla CD_TABLE_NAME 
oAdjustmentsProcessor = NEWOBJECT("AdjustmentsProcessor","AdjustmentsProcessor.PRG")
oAdjustmentsProcessor.sSourceTablename = CD_TABLE_NAME
TRY 
	oAdjustmentsProcessor.processAdjustments(sTagExpr, STRTRAN(IIF(AT("#",sFilter)=0,sFilter,SUBSTR(sFilter,1,AT("#",sFilter)-1)),"A.", "") ,sOrdenProc,,,,.T.)
	bAdjustmentsDone = .T.
CATCH TO oException
	bAdjustmentsDone = .F.
ENDTRY 
IF 	!bAdjustmentsDone THEN 
	*La tabla de datos complementarios no tiene todos los campos que se usan en el filtro, por tanto, el procesamiento de ajustes;
	se hace sobre todolos registros de la tabla
	oAdjustmentsProcessor.processAdjustments(sTagExpr, ,sOrdenProc,,,,.T.)									
ENDIF

RELEASE oAdjustmentsProcessor

PROCEDURE setUPGDSetupDate

	LPARAMETERS sUPGDId as String, sUPGDSub as String 
	
	*Establece la fecha inicial de caracterización de la UPGD identificada por el par (sUPGDId,sUPGDSub) teniendo en cuenta ;
	la primera notificación que dicha UPGD tenga en el Sistema; si no se encuentra ninguna notificación, la fecha se establece;
	igual a UPGD.FEC_CAR

	LOCAL nDataRecordsFound as Number 
	
	*Crea el cursor que contendrá los resultados de notificaciones encontradas
	SELECT COD_PRE,COD_SUB,FEC_NOT FROM PACIENTE WHERE .F. INTO CURSOR rsNotificationDates READWRITE
	
	
	*Determina en qué fechas existen eventos notificados para la UPGD
	
	sWhereClause = " COD_PRE + COD_SUB = '" + LEFT(sUPGDId,10) + LEFT(sUPGDSub,2) + "'" + " AND !EMPTY(FEC_NOT) "
	nDataRecordsFound  = 0
	
	*Considera los registros de datos básicos de eventos individuales
	sSQLSelectCmd = "INSERT INTO rsNotificationDates SELECT TOP 1 COD_PRE,COD_SUB,FEC_NOT FROM PACIENTE WHERE " + sWhereClause + ;
					" ORDER BY FEC_NOT"
	&sSQLSelectCmd 
	nDataRecordsFound  = nDataRecordsFound  + _tally
	
	*Agrega los registros de Brotes	
	DO addIndexToTable WITH 'BROTES','COD_PRE + COD_SUB','UPGD_IDX' IN SIVIGILAIndexesHandler	
	sSQLSelectCmd = "INSERT INTO rsNotificationDates SELECT TOP 1 COD_PRE,COD_SUB,FEC_NOT FROM BROTES WHERE " + ;
					sWhereClause + " ORDER BY FEC_NOT "
	&sSQLSelectCmd 
	nDataRecordsFound  = nDataRecordsFound  + _tally
	
	*Agrega los registros de los eventos con tabla de datos independiente (eventos tipo seguimiento, encuesta,etc.)
	DO EventosConTablaDatosIndependiente.QPR
	SELECT DISTINCT "EVENTOS_" + PADL(FORMULARIO,2,'00') AS NombreTablaEventoSeguimiento FROM rsEventosConTablaDatosIndependiente ;
		INTO CURSOR rsFollowupEvents
	SELECT rsFollowupEvents
	SCAN
		DO addIndexToTable WITH rsFollowupEvents.NombreTablaEventoSeguimiento,'COD_PRE + COD_SUB','UPGD_IDX' IN SIVIGILAIndexesHandler	
		sSQLSelectCmd = "INSERT INTO rsNotificationDates SELECT TOP 1 COD_PRE,COD_SUB,FEC_NOT FROM " + rsFollowupEvents.NombreTablaEventoSeguimiento +;
						" WHERE  " + sWhereClause + " ORDER BY FEC_NOT "
		&sSQLSelectCmd 
		nDataRecordsFound  = nDataRecordsFound  + _tally
	ENDSCAN
	
	SELECT * FROM rsNotificationDates INTO ARRAY aNotificationDates
	IF nDataRecordsFound > 0 THEN
		=ASORT(aNotificationDates,AELEMENT(aNotificationDates,1,3))
		sUpdateSQLCMD = 'UPDATE UPGD SET FEC_INICAR = aNotificationDates(1,3) WHERE COD_PRE+COD_SUB="' + LEFT(sUPGDId,10) + LEFT(sUPGDSub,2) + '"'
	ELSE
		sUpdateSQLCMD = 'UPDATE UPGD SET FEC_INICAR = FEC_CAR WHERE COD_PRE+COD_SUB="' + LEFT(sUPGDId,10) + LEFT(sUPGDSub,2) + '"'
	ENDIF
	&sUpdateSQLCMD	
ENDPROC


PROCEDURE setUPGDsSetupDate

	LPARAMETERS sFilter as String, sPreCmd AS String, sPostCmd as string
	 
	*Establece la fecha inicial de caracterización de todas las UPGDs que se encuentren en la tabla UPGD y que cumplan la condición sFilter

	LOCAL nUPGDsRecords as Number, iUPGD as Number 
	LOCAL sOldDeleted as String, nUPGDsRecords  AS Number, iUPGD  AS Number 

	*?TIME()
	
	IF VARTYPE(sPreCmd)='C' THEN
		&sPreCmd
	ENDIF
	
	sOldDeleted = SET("Deleted")
	SET DELETED ON

	IF VARTYPE(sFilter)='L' THEN
		sFilter = '.T.'
	ENDIF
	SELECT DISTINCT COD_PRE,COD_SUB FROM UPGD WHERE &sFilter INTO CURSOR rsUPGDs
	SELECT rsUPGDs
	COUNT TO nUPGDsRecords 
	GO TOP
	iUPGD  = 1
	DO WHILE !EOF()
		WAIT "Asignando fecha inicial de caracterización: UPGD " + ALLTRIM(STR(iUPGD)) + " de un total de " + ALLTRIM(STR(nUPGDsRecords )) WINDOW NOWAIT
		DO setUPGDSetupDate WITH rsUPGDs.COD_PRE, rsUPGDs.COD_SUB
		iUPGD = iUPGD  + 1
		SELECT rsUPGDs
		SKIP
	ENDDO
	SELECT rsUPGDs
	USE
	SET DELETED &sOldDeleted
	
	IF VARTYPE(sPostCmd)='C' THEN
		&sPostCmd
	ENDIF

	WAIT CLEAR
	*?TIME()
ENDPROC


FUNCTION countUPGDs(sMunicipalityCode as String, bIncludeUIs as Boolean) as Number 

	LOCAL sSQLCmd as String

	*Cuenta cuántas UPGDs activas se encuentran en la tabla de UPGDs pertenecientes al municipio identificado por sMunicipalityCode. ;
	Si bIncludeUIs = .F., en el conteo NO se incluyen unidades informadoras según lo establezca getUPGDCondition 
	
	sSQLCmd = "SELECT COUNT(*) AS N FROM UPGD WHERE LEFT(UPGD.COD_PRE,5)=sMunicipalityCode AND UPGD.ACT_SIV=1"
	
	IF !bIncludeUIs THEN
		sSQLCmd = sSQLCmd + " AND " + getUPGDCondition() 
	ENDIF
	sSQLCmd = sSQLCmd + " INTO ARRAY aActiveUPGDs"
	&sSQLCmd
	
	RETURN aActiveUPGDs

ENDFUNC

FUNCTION getUPGDCondition as String

	*Retorna una claúsula WHERE de SQL que indica cuando una UPGD en la tabla de UPGDs NO es Unidad Informadora. ;
	En la actual implementación una UPGD se considera unidad informadora si cumple una o más de las siguientes condiciones: ;
	1. El sexto dígito del código de prestador es '6';
	2. cod_pre+cod_sub se encuentra en la tabla UnidadesInformadoras ;
	3. El campo Tables.UPGD.ES_UNI_NOT='1', es decir, explícitamente existe un registro en la tabla de UPGDs que indica que ;
	la unidad es informadora que notifica
	
	RETURN "SUBSTR(UPGD.COD_PRE,6,1)!='9' AND cod_pre+cod_sub NOT IN (SELECT cod_pre+cod_sub FROM UnidadesInformadoras) " + ;
			" AND cod_pre+cod_sub NOT IN (SELECT cod_pre+cod_sub FROM UPGD WHERE UPGD.ES_UNI_NOT='1') "
			
ENDFUNC

FUNCTION getUICondition as String

	*Retorna una claúsula WHERE de SQL que indica cuando una UPGD en la tabla de UPGDs es Unidad Informadora. ;
	En la actual implementación una UPGD se considera unidad informadora si cumple una o más de las siguientes condiciones: ;
	1. El sexto dígito del código de prestador es '6';
	2. cod_pre+cod_sub se encuentra en la tabla UnidadesInformadoras ;
	3. El campo Tables.UPGD.ES_UNI_NOT='1', es decir, explícitamente existe un registro en la tabla de UPGDs que indica que ;
	la unidad es informadora que notifica
	
	RETURN "SUBSTR(UPGD.COD_PRE,6,1)='9' OR cod_pre+cod_sub IN (SELECT cod_pre+cod_sub FROM UnidadesInformadoras) " + ;
			" OR cod_pre+cod_sub IN (SELECT cod_pre+cod_sub FROM UPGD WHERE UPGD.ES_UNI_NOT='1') "
			
ENDFUNC


PROCEDURE bulkDeleteUPGDs(sFilter as String)

	*Borra de la tabla UPGD los registros que cumplan con la condición sFilter, si es que se pasa este parámetro opcional;
	en caso contrario, borra todos los registros de la tabla
	
	IF VARTYPE(sFilter)!='C' THEN
		sFilter = '.T.'
	ENDIF
	
	DELETE FROM UPGD WHERE &sFilter 
ENDPROC

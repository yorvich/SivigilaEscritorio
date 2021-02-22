#INCLUDE SIVIGILA.H

PARAMETERS P_area_evto, VER_MSG_PA

LOCAL nRecsWithoutAccess as Number 
nRecsWithoutAccess = 0

*SET STEP ON 
SELECT(P_area_evto)
sTargetTableName = (ALIAS())
sSQLCmd = "DELETE " + sTargetTableName + " FROM " + sTargetTableName + " INNER JOIN ENTRA_03 ON " +;
			"'EVE'+" + sTargetTableName + ".cod_eve = ENTRA_03.clave " +;
			" WHERE ENTRA_03.cod_usu = '" +_cod_usu + "' AND ENTRA_03.acceso!='S'"
&sSQLCmd
nRecsWithoutAccess = _TALLY

IF !EMPTY(VER_MSG_PA)
   =showProgressMessage('EXISTEN ' + NC(nRecsWithoutAccess) + ' FICHAS A LAS QUE NO SE TIENE ACCESO DEBIDO A LAS' + CHR(13) +;
					   'RESTRICCIONES ASIGNADAS POR UN ADMINISTRADOR DEL SISTEMA')
ENDIF
VER_MSG_PA=0

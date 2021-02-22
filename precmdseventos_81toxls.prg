LPARAMETERS sFilter as String
*Calcula para el evento Cod. 354 el promedio de camas UCIA Y SHA agrupadas por COD_PRE, ;
*adicional se calcula el total de camas por entidad territorial utilizando el promedio ya calculado

*Los promedios y totales se calculan con base en un conjunto de registros ajustados  que cumplan la condición sFilter.

LOCAL bAdjustmentsDone as Boolean 

*SET STEP ON 
IF !(VARTYPE(sFilter)='C' AND !EMPTY(sFilter)) THEN 
	sFilter = '.T.'
ENDIF

*Esta parte debe modificarse si existe una configuración en Tables.SIVIGILAAdjustmentsSetup para EVENTOS_81
* Procesa los ajustes para la tabla EVENTOS_81
TRY 
	DO fastProcessAdjustments WITH 'EVENTOS_81',getKeyFields('EVENTOS_81'), .T., STRTRAN(STRTRAN(sFilter,"#"),"A.") IN SIVIGILAUtilities
		bAdjustmentsDone = .T.
CATCH TO oException
	bAdjustmentsDone = .F.
ENDTRY 
IF 	!bAdjustmentsDone THEN 
	*La tabla de datos complementarios no tiene todos los campos que se usan en el filtro, por tanto, el procesamiento de ajustes;
	se hace sobre todolos registros de la tabla
	DO fastProcessAdjustments WITH 'EVENTOS_81',getKeyFields('EVENTOS_81'), .T. IN SIVIGILAUtilities
ENDIF 

SET DECIMALS TO 3

*Calcula los promedios en varios pasos con el objeto de no tener en cuenta datos vacíos
SELECT cod_pre, cod_sub, AVG(VAL(t_cam_ucia)) as pmd_cam_ucia FROM rsAdjustedCases ;
	WHERE  !EMPTY(t_cam_ucia);
	GROUP BY cod_pre, cod_sub INTO CURSOR rsPromdCamas_ucia NOFILTER 

SELECT cod_pre, cod_sub, AVG(VAL(t_cam_sha)) as pmd_cam_sha FROM rsAdjustedCases ;
	WHERE  !EMPTY(t_cam_sha);
	GROUP BY cod_pre, cod_sub INTO CURSOR rsPromdCamas_sha NOFILTER 

SELECT rsPromdCamas_ucia.COD_PRE, rsPromdCamas_ucia.COD_SUB, pmd_cam_ucia, pmd_cam_sha FROM rsPromdCamas_ucia LEFT OUTER JOIN rsPromdCamas_sha  ;
	ON rsPromdCamas_ucia.COD_PRE + rsPromdCamas_ucia.COD_SUB = rsPromdCamas_sha.COD_PRE + rsPromdCamas_sha.COD_SUB INTO CURSOR rsFromUCIAToSHA NOFILTER 

SELECT rsPromdCamas_sha.COD_PRE, rsPromdCamas_sha.COD_SUB, pmd_cam_ucia, pmd_cam_sha FROM rsPromdCamas_sha LEFT OUTER JOIN rsPromdCamas_ucia ;
	ON rsPromdCamas_ucia.COD_PRE + rsPromdCamas_ucia.COD_SUB = rsPromdCamas_sha.COD_PRE + rsPromdCamas_sha.COD_SUB INTO CURSOR rsFromSHAToUCIA NOFILTER 

SELECT * FROM rsFromUCIAToSHA UNION select * FROM rsFromSHAToUCIA INTO CURSOR rsPromdCamas


*Calcula los promedios de total de camas
SELECT LEFT(cod_pre,2) as cod_et, AVG(VAL(t_cam_ucia)) as tt_cam_ucia, AVG(VAL(t_cam_sha)) as tt_cam_sha FROM rsAdjustedCases ;
	GROUP BY cod_et INTO CURSOR rsTtCamas NOFILTER

* Produce un único recordset con todos los promedios dispuestos en form horizontal, es decir, en donde cada promedio;
aparece como una columna
SELECT rsPromdCamas.*, rsTtCamas.tt_cam_ucia, rsTtCamas.tt_cam_sha ;
	FROM rsPromdCamas INNER JOIN rsTtCamas ;
	ON LEFT(rsPromdCamas.cod_pre,2) = rsTtCamas.cod_et ;
	INTO CURSOR rsPromedioTotalCamas
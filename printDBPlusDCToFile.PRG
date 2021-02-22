#INCLUDE SIVIGILA.h

PARAMETERS sExportPathAndFileName as String

*Imprime hacia un archivo ASCII, el reporte de notificación de datos individuales y complementarios rptDBPlusDC tomando como fuente de datos ;
el registro actual que se encuentre en las tablas PACIENTE y la tabla de datos complementarios (EVENTOS_XX) asociada. 
*El archivo ASCII quedará con el nombre y en la carpeta especificada por sExportPathAndFileName. En esa misma carpeta, producirá un archivo excel ;
con los datos básicos y complementarios

*Como efecto colateral construye dos recordsets de nombre rsPaciente y rsDC con los datos de los registros de datos básicos y complementarios;
utilizados

LOCAL bCDExist as Boolean 
bCDExist = .T.
*Indica si los datos complementarios asociados al registro actual de la tabla de datos básicos han sido diligenciados

LOCAL oReporter AS Object
oReporter=NEWOBJECT("SIVIGILAReporter","SIVIGILAReporter.fxp")

LOCAL oEvent as Object
oEvent = NEWOBJECT("Event","Event.fxp",.NULL.)

LOCAL nCurrentCDRecId  as Number 
nCurrentCDRecId = 0

sOldProc = SET("Procedure")

SET PROCEDURE TO Utilities ADDITIVE
SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
SET PROCEDURE TO (PATH_TO_COMMON_LIB + 'TablesHandler') ADDITIVE

oEvent.Refresh(PACIENTE.COD_EVE)
sEventTableName = oEvent.sDC_TABLA

IF  !ISNULL(sEventTableName) THEN

	*Crea un cursor temporal para almacenar los datos registrados decodificados de datos complementarios
	SELECT SPACE(17) AS NUM_IDE, SPACE(20) AS NOM_VAR, SPACE(200) AS DAT_REG, SPACE(254) AS DESC_VAR FROM EVENTOS ;
		WHERE .F. INTO CURSOR rsComplementaryData READWRITE

	SELECT PACIENTE
	nCurrentBDRecId = RECNO()
	SELECT * FROM PACIENTE WHERE RECNO()= nCurrentBDRecId INTO CURSOR rsPaciente NOFILTER 

	IF oEvent.hasComplementaryData() THEN 
		*La definición del evento indica que el registro debería tener datos complementarios en la tabla sEventTableName
	
		=SelectTable(sEventTableName)
		IF EOF()
			sInsertSQLCmd="INSERT INTO rsComplementaryData (num_ide,desc_var) VALUES (PACIENTE.NUM_IDE,'No se encontraron datos complementarios')"
			&sInsertSQLCmd
			bCDExist = .F.
		ELSE
			*El registro tiene datos complementarios en la tabla sEventTableName
			SCATTER MEMVAR

			*Establece qué variables fueron diligenciadas en los datos complementarios, cuáles fueron los valores diligenciados para ;
			cada variable y cuál es su interpretación en términos de la especificación del diccionario de datos del Sistema SIVIGILA
			sSQLCmd="SELECT * FROM DD WHERE LOWER(Nombre)='" + LOWER(sEventTableName) + "' INTO CURSOR rsEventVars"
			&sSQLCmd
*SET STEP ON 
			SELECT (sEventTableName)
			nCurrentCDRecId = RECNO()
			DO WHILE NOT EOF("rsEventVars")
				sVarName=ALLTRIM(rsEventVars.ExpNombreC)
				sVarDescription=ALLTRIM(rsEventVars.ddNombreLo)
				sStatVarType=ALLTRIM(rsEventVars.IdTipoPreg)
				TRY
					sVarStoredValue=&sVarName
				CATCH TO oException
					sVarStoredValue=""
				ENDTRY
				sVarValue=sVarStoredValue
				
				DO CASE 
					CASE VARTYPE(sVarStoredValue)='C'
						IF !EMPTY(sVarValue)  THEN
							IF sStatVarType='C' THEN

								*La variable correspondiente al campo sVarName es CATEGÓRICA
								IF !('tabla de referencia' $ rsEventVars.DDValoresP) THEN
									DIMENSION aOptions(1)
									DO splitOptions WITH ALLTRIM(rsEventVars.DDValoresP), aOptions
									iVarSpecification=ASCAN(aOptions,sVarStoredValue,-1,-1,2,15)
									IF iVarSpecification>0 THEN
										sVarValue=aOptions(iVarSpecification,2) + " - " + aOptions(iVarSpecification,3)
									ELSE
										sVarValue=sVarStoredValue + " - El valor registrado no se encuentra dentro de los valores permitidos para la versión actual del Sistema SIVIGILA"
									ENDIF
								ELSE
									sVarValue=sVarStoredValue + " - El valor registrado procede de una tabla de referencia asociada"
								ENDIF
							ENDIF
						ELSE
							sVarValue="No registró ningún valor"			
						ENDIF

					CASE VARTYPE(sVarStoredValue)='N'
						sVarValue = STR(sVarValue, MAX_SIVIGILA_DECIMAL_WIDTH, MAX_SIVIGILA_DECIMAL_PLACES)

					CASE VARTYPE(sVarStoredValue)='D'
						sVarValue = DTOC(sVarValue)
				ENDCASE

		        INSERT INTO rsComplementaryData (num_ide, nom_var, dat_reg, desc_var);
		            VALUES (&sEventTableName..NUM_IDE, sVarName, sVarValue, sVarDescription)
				SKIP IN rsEventVars
			ENDDO
		ENDIF
	ELSE
		bCDExist = .F.
		INSERT INTO rsComplementaryData (num_ide,desc_var) VALUES (PACIENTE.NUM_IDE, "PARA ESTE EVENTO NO APLICAN DATOS COMPLEMENTARIOS")
	ENDIF

	=SelectTable('EVENTOS',,.F.)
	sTargetEventCode = oEvent.sCOD_EVE
	LOCATE FOR COD_EVE=(sTargetEventCode)
	SELECT rsComplementaryData 
	*_ASCIICOLS = 120
	sCmd = 'REPORT FORM REPORTES\rptDBPlusDC TO FILE [' + sExportPathAndFileName + '] ASCII'
	&sCmd

	
	*Genera un archivo excel con la salida usual que produce el sistema durante la generación de archivos xls pero teniendo en cuenta únicamente ;
	los registros actuales de las tablas de datos básicos y complementarios
	WITH oReporter
		.sXlsFilePath = ADDBS(JUSTPATH(sExportPathAndFileName))
		IF bCDExist THEN 
			SELECT * FROM (sEventTableName) WHERE RECNO()=nCurrentCDRecId INTO CURSOR rsDC NOFILTER 
			.notificationFromQueryToXLS(oEvent.sCOD_EVE, "A.TIP_IDE='" + m.TIP_IDE + "' AND A.NUM_IDE='" + m.NUM_IDE + "'", ;
										"rsPaciente", "rsDC", .T., , JUSTFNAME(sExportPathAndFileName))
		ELSE
			.notificationFromQueryToXLS(oEvent.sCOD_EVE, "A.TIP_IDE='" + m.TIP_IDE + "' AND A.NUM_IDE='" + m.NUM_IDE + "'", ;
										"rsPaciente", , .T., , JUSTFNAME(sExportPathAndFileName))
		ENDIF 
	ENDWITH 
ELSE
	=showErrorMessage("NO HAY NADA QUE IMPRIMIR", 0)
ENDIF

RELEASE oEvent 
RELEASE oReporter

SET PROCEDURE TO 
SET PROCEDURE TO &sOldProc

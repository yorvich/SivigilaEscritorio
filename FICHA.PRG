#INCLUDE SIVIGILA.h

SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
SET PROCEDURE TO Utilities ADDITIVE
SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE

*SET STEP ON 

LOCAL oIndividualReporter AS Object
oIndividualReporter = NEWOBJECT("SIVIGILAReporter","SIVIGILAReporter.fxp")

sEventTableName = GetWarehouseTableName(PACIENTE.COD_EVE)
IF  VARTYPE(sEventTableName)!='L' THEN

	*Crea un cursor temporal para almacenar los datos registrados decodificados de datos complementarios
	SELECT SPACE(20) AS NOM_VAR, SPACE(200) AS DAT_REG, SPACE(254) AS DESC_VAR FROM EVENTOS WHERE .F. INTO CURSOR rsComplementaryData READWRITE

	SELECT PACIENTE
	REP_NOM_PTE=ALLTRIM(PRI_APE)+' '+ALLTRIM(SEG_APE)+' '+ALLTRIM(PRI_NOM)+' '+ALLTRIM(SEG_NOM)
	
	=SelectTable('EVENTOS','COD',.F.)
	SEEK PACIENTE.COD_EVE
	REP_NOM_EVE = ALLTRIM(EVENTOS.NOM_EVE)

	IF !EMPTY(EVENTOS.FORMULARIO)
		*La definición del evento indica que el registro debería tener datos complementarios en la tabla sEventTableName

		*Busca en sEventTableName los datos complementarios correspondientes al registro actual
		=SelectTable(sEventTableName)
		SEEK PACIENTE.AÑO + PACIENTE.SEMANA + PACIENTE.COD_EVE + PACIENTE.TIP_IDE + PACIENTE.NUM_IDE + PACIENTE.COD_PRE + PACIENTE.COD_SUB ORDER TAG LINKER_IDX

		IF !FOUND()
			sMessage="NO SE ENCONTRARON LOS DATOS COMPLEMENTARIOS DE ESTA NOTIFICACIÓN"
			=showErrorMessage(sMessage,0)
			sInsertSQLCmd="INSERT INTO rsComplementaryData (desc_var) VALUES ('" + sMessage + "')"
			&sInsertSQLCmd
		ELSE
			*El registro tiene datos complementarios en la tabla sEventTableName
			SCATTER MEMVAR

			*Establece qué variables fueron diligenciadas en los datos complementarios, cuáles fueron los valores diligenciados para ;
			cada variable y cuál es su interpretación en términos de la especificación del diccionario de datos del Sistema SIVIGILA
			sSQLCmd = "SELECT * FROM DD WHERE LOWER(Nombre)='" + LOWER(sEventTableName) + "' AND ExpNombreC!='AJUSTE' AND ExpNombreC!='FEC_AJU' INTO CURSOR rsEventVars"
			&sSQLCmd
*SET STEP ON 
			SELECT (sEventTableName)
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

		        INSERT INTO rsComplementaryData (nom_var, dat_reg, desc_var);
		            VALUES (sVarName, sVarValue, sVarDescription)
				SKIP IN rsEventVars
			ENDDO
		ENDIF
	ELSE
		INSERT INTO rsComplementaryData (desc_var) VALUES ("PARA ESTE EVENTO NO APLICAN DATOS COMPLEMENTARIOS")
	ENDIF

	SELECT rsComplementaryData 
	REPORT FORM REPORTES\FICHA TO PRINTER PROMPT PREVIEW 
ELSE
	=showErrorMessage("NO HAY NADA QUE IMPRIMIR", 0)
ENDIF

RELEASE oIndividualReporter


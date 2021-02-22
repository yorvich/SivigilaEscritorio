#INCLUDE SIVIGILA.H

#INCLUDE GlobalConst.H


PROCEDURE CountRecords 

	LPARAMETERS nEventsToCount as Long, pathToSourceFiles as String


	CLOSE ALL
	recCounter=0

	IF !USED('conteoRegistros.DBF') THEN
		USE conteoRegistros IN 0
	ELSE
		SELECT conteoRegistros
	ENDIF

	DELETE ALL
	PACK

	nParameters=PARAMETERS()
	IF nParameters=2 THEN
		currentDir=CURDIR()
		SET DEFAULT TO (pathToSourceFiles)
	ENDIF

	DO pCountTableRecords WITH 'GENERAL', 'UNIDAD'

	DO pCountTableRecords WITH 'UPGD', 'UPGD'

	DO pCountTableRecords WITH 'LABORATORIOS','ResultadoLaboratorio'

	DO pCountTableRecords WITH 'BROTES', 'Evento'

	DO pCountTableRecords WITH 'PACIENTE', 'Paciente'                                         

	DO pCountTableRecords WITH 'TAL_HUM', 'TalentoHumano'

	DO pCountTableRecords WITH 'NOTIFICADOS', 'RegistroNotificados'

	DO pCountTableRecords WITH 'AJUSTES', 'Ajuste'
	
	FOR event=1 TO nEventsToCount
		DO pCountTableRecords WITH 'EVENTOS_' + PADL(event,2,'0'), 'Evento'
	NEXT

	SELECT * FROM ConteoRegistros WHERE Regs<>0 INTO CURSOR vConteoRegistros 

	IF nParameters=2 THEN
		SET DEFAULT TO (currentDir)
	ENDIF
ENDPROC

PROCEDURE pCountTableRecords(sourceTableName as String, entityName as String)

	STORE sourceTableName + '.DBF' TO sSourceFileName

	IF FILE("&sSourceFileName") THEN 	
		SELECT COUNT(*) AS n FROM (sourceTableName) INTO ARRAY arrayTableRecords 
		INSERT INTO conteoRegistros (entidad, nombre, regs, campos) VALUES ('&entityName', '&sourceTableName' ,arrayTableRecords (1,1), FCOUNT(sourceTableName))
		SELECT (sourceTableName)
		USE
	ENDIF 	
ENDPROC


FUNCTION GetQuerySQLClause AS String

	LPARAMETERS sQprFileName AS String
	
	sSQLClause=''
	FileHandler = FOPEN(sQprFileName )
	IF FileHandler > 0
		DO WHILE .T.
			sFileLine = fgets(FileHandler)
			IF !EMPTY(sFileLine )
				sSQLClause= sSQLClause+ STRTRAN(sFileLine ,';','')
			ELSE
				EXIT
			ENDIF
		ENDDO
		FCLOSE(FileHandler)
	ENDIF
	RETURN	sSQLClause	
ENDFUNC


PROCEDURE setDataEntryButtons(oTargetForm as Form, nEditState as byte, sCommandButtonShowSlave as String)
	*Fija la propiedad Enabled y Caption de los botones de control del formulario oTargetForm. Se supone que los botones del formulario tienen;
	 los siguientes nombres: ;
	 -nue: Nuevo registro;
	 -sal: Salir;
	 -bus: buscar registro;
	 -gua: guardar registro;
	 -act: actualizar registro;
	 -SivigilaCmdButtonDelete: borrar registro;
	 -SivigilaCmdButtonAdjust: ajustar registro
		
	*nEditState: 0-->None	1-->New		2-->Saved	3-->Updated	4-->Adjusted
	
	*sCommandButtonShowSlave (opcional): nombre de un botón que se supone permite abrir un formulario esclavo de oTargetForm 
	
	DO CASE
		CASE nEditState =0
			oTargetForm.nue.Enabled=.T.
			oTargetForm.sal.Enabled=.T.
			oTargetForm.sal.caption = "\<Terminar"
			oTargetForm.bus.Enabled=.T.
			oTargetForm.gua.Enabled=.F.
			oTargetForm.act.Enabled=.F.
			oTargetForm.SivigilaCmdButtonDelete.Enabled=.F.
			oTargetForm.SivigilaCmdButtonAdjust.Enabled = .F.
			IF VARTYPE(sCommandButtonShowSlave)='C' AND !EMPTY(sCommandButtonShowSlave) THEN 
				sCmd = 'oTargetForm.' + sCommandButtonShowSlave + '.Enabled=.F.'
				&sCmd
			ENDIF 
		CASE nEditState =1
			oTargetForm.nue.Enabled=.F.
			oTargetForm.sal.Enabled=.T.
			oTargetForm.sal.caption = "\<Cancelar"
			oTargetForm.bus.Enabled=.F.
			oTargetForm.gua.Enabled=.F.
			oTargetForm.act.Enabled=.F.
			oTargetForm.SivigilaCmdButtonDelete.Enabled=.F.
			oTargetForm.SivigilaCmdButtonAdjust.Enabled = .F.		
			IF VARTYPE(sCommandButtonShowSlave)='C' AND !EMPTY(sCommandButtonShowSlave) THEN 
				sCmd = 'oTargetForm.' + sCommandButtonShowSlave + '.Enabled=.F.'
				&sCmd
			ENDIF 
		CASE nEditState =2
			oTargetForm.nue.Enabled=.T.
			oTargetForm.sal.Enabled=.T.
			oTargetForm.sal.caption = "\<Terminar"
			oTargetForm.bus.Enabled=.T.
			oTargetForm.gua.Enabled=.F.
			oTargetForm.act.Enabled=.T.
			oTargetForm.SivigilaCmdButtonDelete.Enabled=.T.
			oTargetForm.SivigilaCmdButtonAdjust.Enabled = .F.
			IF VARTYPE(sCommandButtonShowSlave)='C' AND !EMPTY(sCommandButtonShowSlave) THEN 
				sCmd = 'oTargetForm.' + sCommandButtonShowSlave + '.Enabled=.T.'
				&sCmd
			ENDIF 
		CASE nEditState =3
			oTargetForm.nue.Enabled=.F.
			oTargetForm.sal.Enabled=.T.
			oTargetForm.sal.caption = "\<Cancelar"			
			oTargetForm.bus.Enabled=.F.
			oTargetForm.gua.Enabled=.T.
			oTargetForm.act.Enabled=.F.
			oTargetForm.SivigilaCmdButtonDelete.Enabled=.F.			
			oTargetForm.SivigilaCmdButtonAdjust.Enabled = .F.			
			IF VARTYPE(sCommandButtonShowSlave)='C' AND !EMPTY(sCommandButtonShowSlave) THEN 
				sCmd = 'oTargetForm.' + sCommandButtonShowSlave + '.Enabled=.F.'
				&sCmd
			ENDIF 
		CASE nEditState =4
			oTargetForm.nue.Enabled=.T.
			oTargetForm.sal.Enabled=.T.
			oTargetForm.sal.caption = "\<Terminar"			
			oTargetForm.bus.Enabled=.T.
			oTargetForm.gua.Enabled=.F.
			oTargetForm.act.Enabled=.F.
			oTargetForm.SivigilaCmdButtonDelete.Enabled=.F.			
			oTargetForm.SivigilaCmdButtonAdjust.Enabled = .T.
			IF VARTYPE(sCommandButtonShowSlave)='C' AND !EMPTY(sCommandButtonShowSlave) THEN 
				sCmd = 'oTargetForm.' + sCommandButtonShowSlave + '.Enabled=.T.'
				&sCmd
			ENDIF 
		
		OTHERWISE
	ENDCASE

ENDPROC


PROCEDURE getFormControlsState

	LPARAMETERS oTargetForm as Object, aControlsState, bRetrieveAllControls as Boolean, bIncludeInvisibles as Boolean

	*Retorna en aControlsState algunas propiedades de los controles que se encuentran en el formulario oTargetForm. ;
	Específicamente las características retornadas por cada control son: Name, Enabled, Index, Value y TabIndex ;;
	aControlsState será una matriz de dimensiones nFormControls x 6 con nFormControls el número de controles en oTargetForm ;
	o el número de controles pertenecientes a las clases mencionadas más adelante y en donde la fila i contendrá las ;
	propìedades del i-ésimo control así: ;
		aControlsState[i,1] <-- .Name;
		aControlsState[i,2] <-- "Enabled";
		aControlsState[i,3] <-- .Enabled;
		aControlsState[i,4] <-- .Index;
		aControlsState[i,5] <-- .Value (si aplica);
		aControlsState[i,6] <-- .TabIndex;
		Modificado : JLGB 15/08/2019;
		aControlsState[i,7] <-- .Class;
		Modificado : JLGB 09/09/2019;
		Nuevo componente Sivigilatxtmunicipios
	* bRetrieveAllControls: Si es .T. se devolverán las propiedades de todos los controles de oTargetForm; en caso contrario, ;
	solamente se devolveran los de los controles de las clases Textbox, Combobox, Sivigilatxtboxes, Optiongroup, Checkbox, ;
	Sivigilatxtboxsearcher y SivigilaTxtDateBox

	* bIncludeInvisibles (opcional): Si es .T. se tendrán en cuenta los controles invisibles de oTargetForm; en caso contrario, ;
	solamente se tendrán en cuenta los visibles
	*Modificado 
	
	
	
	LOCAL nFormControls, nTargetControls
	
	nFormControls=oTargetForm.ControlCount
	
	nTargetControls=0
	FOR nControl = 1 TO nFormControls
		oTargetCtrl = oTargetForm.Controls(nControl )

		IF ! bRetrieveAllControls		
			IF oTargetCtrl.Class='Textbox' OR oTargetCtrl.Class='Combobox' OR oTargetCtrl.Class='Sivigilatxtdatebox' OR;
				oTargetCtrl.Class='Sivigilatxtboxes' OR oTargetCtrl.Class='Optiongroup' OR oTargetCtrl.Class='Checkbox' OR;
				oTargetCtrl.Class='Sivigilatxtboxsearcher' OR oTargetCtrl.Class='Sivigilatxtdocumentoidentidad' OR;
				oTargetCtrl.Class='Sivigilatxtmunicipios' THEN
				
				IF !bIncludeInvisibles THEN
					IF oTargetCtrl.Visible
						nTargetControls=nTargetControls+1
						DIMENSION aControlsState(nTargetControls,7)
						aControlsState(nTargetControls,1)=oTargetCtrl.Name
						aControlsState(nTargetControls,2)="Enabled"
						aControlsState(nTargetControls,3)=oTargetCtrl.Enabled
						aControlsState(nTargetControls,4)=nControl
						aControlsState(nTargetControls,6)=oTargetCtrl.TabIndex
						aControlsState(nTargetControls,7)=oTargetCtrl.Class				
						TRY
							aControlsState(nTargetControls,5)=oTargetCtrl.Value
						CATCH TO oException
						ENDTRY
					ENDIF
				ELSE
					nTargetControls=nTargetControls+1
					DIMENSION aControlsState(nTargetControls,7)
					aControlsState(nTargetControls,1)=oTargetCtrl.Name
					aControlsState(nTargetControls,2)="Enabled"
					aControlsState(nTargetControls,3)=oTargetCtrl.Enabled
					aControlsState(nTargetControls,4)=nControl
					aControlsState(nTargetControls,6)=oTargetCtrl.TabIndex
					aControlsState(nTargetControls,7)=oTargetCtrl.Class				
					TRY
						aControlsState(nTargetControls,5)=oTargetCtrl.Value
					CATCH TO oException
					ENDTRY
				ENDIF
			ENDIF
		ELSE
			IF oTargetCtrl.Class!='Label' AND oTargetCtrl.Class!='Hyperlink'  AND oTargetCtrl.Class!='Image' THEN
				nTargetControls=nTargetControls+1
				DIMENSION aControlsState(nTargetControls,7)
				aControlsState(nTargetControls,1)=oTargetCtrl.Name
				aControlsState(nTargetControls,2)="Enabled"
				aControlsState(nTargetControls,3)=oTargetCtrl.Enabled
				aControlsState(nTargetControls,4)=nControl
				aControlsState(nTargetControls,7)=oTargetCtrl.Class
				TRY
					aControlsState(nTargetControls,6)=oTargetCtrl.TabIndex
					aControlsState(nTargetControls,5)=oTargetCtrl.Value
				CATCH TO oException
				ENDTRY
			ENDIF
		ENDIF
	NEXT nControl
	
ENDPROC


PROCEDURE setFormControlsState

	LPARAMETERS oTargetForm as Object, aControlsState, bSetCtrlValues as Boolean
	
	LOCAL nFormControls, nTargetControls
	
	nTargetControls=ALEN(aControlsState,1)
	
	FOR nControl = 1 TO nTargetControls
		sCtrlName=aControlsState(nControl,1) 
		sCtrlProperty=aControlsState(nControl,2)
		bCtrlPropertyValue=aControlsState(nControl,3)
		nCtrlIndex=aControlsState(nControl,4)
		vCtrlValue=aControlsState(nControl,5)
		oTargetForm.Controls(nCtrlIndex).Enabled=bCtrlPropertyValue
		IF bSetCtrlValues THEN
			TRY
				oTargetForm.Controls(nCtrlIndex).Value=vCtrlValue
			CATCH TO oException
			ENDTRY
		ENDIF
		*Modificado : JLGB 15/08/2019 nuevo componente Sivigilatxtdocumentoidentidad
		*Modificado : JLGB 03/09/2019 nuevo componente Sivigilatxtmunicipios
		IF aControlsState(nControl,7) = 'Sivigilatxtdocumentoidentidad' OR aControlsState(nControl,7) = 'Sivigilatxtmunicipios' THEN
			nSubControl = oTargetForm.Controls(nCtrlIndex).ControlCount
			FOR mControl=1 TO nSubControl 
				IF oTargetForm.Controls(nCtrlIndex).Controls(mControl).Class = 'Sivigilatxtboxes' OR oTargetForm.Controls(nCtrlIndex).Controls(mControl).Class = 'Sivigilatxtboxsearcher' THEN
					oTargetForm.Controls(nCtrlIndex).Controls(mControl).Enabled = bCtrlPropertyValue
				ENDIF
			NEXT			
		ENDIF
	NEXT nControl
	
ENDPROC


PROCEDURE setEnableFormControlsTo(oTargetForm as Object, bNewEnableValue as boolean)
	
	*Modificado : JLGB 15/08/2019 nuevo componente Sivigilatxtdocumentoidentidad
	*Modificado : JLGB 03/09/2019 nuevo componente Sivigilatxtmunicipios
	
	oTargetForm.SetAll('Enabled',bNewEnableValue ,'Textbox')
	oTargetForm.SetAll('Enabled',bNewEnableValue,'SIVIGILATxtBoxes')
	oTargetForm.SetAll('Enabled',bNewEnableValue,'SIVIGILATxtBoxSearcher')
	oTargetForm.SetAll('Enabled',bNewEnableValue,'SivigilaTxtDateBox')
	oTargetForm.SetAll('Enabled',bNewEnableValue,'Combobox')
	oTargetForm.SetAll('Enabled',bNewEnableValue,'Sivigilatxtdocumentoidentidad')
	oTargetForm.SetAll('Enabled',bNewEnableValue,'SIVIGILATxtMunicipios')
ENDPROC



PROCEDURE getBoundedControlsIndex

	LPARAMETERS oTargetForm as Object, aControlsState, rsTargetControls
	
	LOCAL nFormControls, nTargetControls
	
	nFormControls=oTargetForm.ControlCount
	
	nTargetControls=0
	SELECT (rsTargetControls)
	FOR nControl = 1 TO nFormControls
		oTargetCtrl = oTargetForm.Controls(nControl)

		LOCATE FOR UPPER(nomb_ctrl) = UPPER(oTargetCtrl.Name)
		IF !EOF() THEN
			nTargetControls=nTargetControls+1
			DIMENSION aControlsState(nTargetControls,2)
			aControlsState(nTargetControls,1)=nControl
			aControlsState(nTargetControls,2)=oTargetCtrl.Name
		ENDIF
	NEXT nControl
	
ENDPROC


FUNCTION storeSIVIGILARecord AS Boolean

	LPARAMETERS sTargetTableName as string, bNotIsNew AS Byte, sEventCode as String, sAdjustValue as String, ;
				bUseRecnoAsIdNumber as Boolean, sIdType as String, sResultMsg as String
	
	SET PROCEDURE TO SIVIGILAMESSENGER ADDITIVE
	nReturnedValue=.F.
	sResultMsg=''
	nCurrentWorkArea=SELECT(0)
	IF UseTable(sTargetTableName ) THEN
		
		sSQLCmd = 'SELECT COD_EVE,FLU_NOT FROM EVENTOS WHERE COD_EVE="' + sEventCode + '" INTO ARRAY aEventSpec'
		&sSQLCmd
		IF _TALLY=0 THEN
			sResultMsg="No se pudo grabar el registro. No se encuentra el evento " +  sEventCode 
		ELSE
			m.flu_not=aEventSpec(1,2)
			
			SELECT (sTargetTableName )
			IF bNotIsNew = 0 THEN
				TRY
					*Libera la variable que da cuenta del estado de notificación del registro para que el valor del campo correspondiente sea asignado por Default
					RELEASE m.EST_NOTIF
				
					INSERT INTO &sTargetTableName FROM MEMVAR 
					IF bUseRecnoAsIdNumber THEN
						REPLACE NUM_IDE WITH ALLTRIM(STR(RECNO()))
					ENDIF

					IF VARTYPE(sIdType)='C' THEN
						REPLACE TIP_IDE WITH (sIdType)
					ENDIF
					nReturnedValue=.T.
					showProgressMessage('SE HA ALMACENADO UNA NUEVA FICHA')
				CATCH TO oException
					sResultMsg="No se pudo grabar el registro. Se produjo el siguiente error: " +  CHR(13) +  CHR(10) +;
								"Código interno de error: " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
								"Mensaje de error: "
					IF VARTYPE(gsErrorMsg)='C' AND !EMPTY(gsErrorMsg) THEN
						sResultMsg = sResultMsg + gsErrorMsg
					ELSE
						sResultMsg = sResultMsg + (oException.Message) + CHR(13)
					ENDIF
				FINALLY
					SELECT (nCurrentWorkArea)
				ENDTRY
			ELSE
				TRY
					GATHER MEMVAR
					nReturnedValue=.T.
					showProgressMessage('LA FICHA SE HA ACTUALIZADO')
				CATCH TO oException
					sResultMsg="No se pudo grabar el registro. Se produjo el siguiente error: " +  CHR(13) +  CHR(10) +;
								"Código interno de error: " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
								"Mensaje de error: "  
					IF VARTYPE(gsErrorMsg)='C'  AND !EMPTY(gsErrorMsg) THEN
						sResultMsg = sResultMsg + gsErrorMsg
					ELSE
						sResultMsg = sResultMsg + (oException.Message) + CHR(13)
					ENDIF
				FINALLY
					SELECT (nCurrentWorkArea)
				ENDTRY
			ENDIF

			IF nReturnedValue THEN
				SELECT (sTargetTableName )
				REPLACE NOTIFICA WITH 0
				
				IF EMPTY(sAdjustValue) THEN
					sAdjustValue='0'
				ENDIF
				REPLACE AJUSTE WITH (sAdjustValue)
				
			ENDIF
		ENDIF
	ENDIF
	RETURN nReturnedValue
ENDFUNC


PROCEDURE storeSIVIGILARecordTest
	?storeSIVIGILARecord ("eventos_24",0,'860','0',.f.,'NA')
ENDPROC


FUNCTION AddRecordFromMem(sTargetTableName as string, bIsNew AS Boolean, sPreCommands as String, sPostCommands as String, sIDField as string, sResultMsg AS String) AS Boolean

	DIMENSION aMaxIdValue(1)
	
	SET PROCEDURE TO SIVIGILAMESSENGER ADDITIVE
	bReturnedValue=.F.
	sResultMsg=''
	nCurrentWorkArea=SELECT(0)

	IF UseTable(sTargetTableName ) THEN
		IF VARTYPE(sPreCommands)='L' THEN
			sPreCommands = ''
		ENDIF
		IF VARTYPE(sPostCommands)='L' THEN
			sPostCommands = ''
		ENDIF
			
		SELECT (sTargetTableName )
		IF bIsNew THEN
			IF VARTYPE(sIDField)='C' AND !EMPTY(sIDField) THEN
				IF VARTYPE(&sTargetTableName..&sIDField) = 'N' THEN 
					sOldDeleted = SET("Deleted")
					SET DELETED OFF
					sSQLCmd = "SELECT MAX(" + sIDField + ") FROM " + sTargetTableName + " INTO ARRAY aMaxIdValue"
					&sSQLCmd
					IF _TALLY > 0 THEN 
						sIDMemVar = 'm.' + sIDField + '=' + ALLTRIM(STR(aMaxIdValue(1)+1))
					ELSE
						sIDMemVar = 'm.' + sIDField + '=1'
					ENDIF
					&sIDMemVar
					SET DELETED &sOldDeleted 
				ELSE 
					*Asigna un GUID como identificador unívoco al registro que se va a agregar (suministrado en https://microsoft.public.fox.vfp.language-menus.narkive.com/T4RgXy9V/how-to-create-guid-using-vfp)
					oGUID = CreateObject("scriptlet.typelib")
					sGUID = STREXTRACT(oGUID.GUID, "{", "}" )
					sIDMemVar = 'm.' + sIDField + '="' + sGUID + '"'
					&sIDMemVar
					RELEASE oGUID 
				ENDIF 
			ENDIF
			TRY
				&sPreCommands
				INSERT INTO &sTargetTableName FROM MEMVAR 
				IF !(goErrorlog.nErrNum = DATA_VALIDATION_ERROR OR goErrorlog.nErrNum = RECORD_VALIDATION_ERROR)
					&sPostCommands
					bReturnedValue=.T.
					showProgressMessage('Se ha almacenado un nuevo registro')
				ENDIF
			CATCH TO oException
				sResultMsg="No se pudo grabar el registro. Se produjo el siguiente error: " +  CHR(13) +  CHR(10) +;
							"Código interno de error: " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
							"Mensaje de error: "
				IF VARTYPE(gsErrorMsg)='C' AND !EMPTY(gsErrorMsg) THEN
					sResultMsg = sResultMsg + gsErrorMsg
				ELSE
					IF oException.ErrorNo = PK_VALIDATION_ERROR THEN
						sResultMsg = sResultMsg + 'YA EXISTE una ficha en el sistema correspondiente a estos datos.' + CHR(13)
					ELSE 
						sResultMsg = sResultMsg + (oException.Message) + CHR(13)
					ENDIF 
				ENDIF
			FINALLY
				SELECT (nCurrentWorkArea)
			ENDTRY
		ELSE
			TRY
				&sPreCommands
				GATHER MEMVAR
				IF !(goErrorlog.nErrNum = DATA_VALIDATION_ERROR OR goErrorlog.nErrNum = RECORD_VALIDATION_ERROR)
					&sPostCommands
					bReturnedValue=.T.
					showProgressMessage('El registro se ha actualizado')
				ENDIF
			CATCH TO oException
				sResultMsg="No se pudo grabar el registro. Se produjo el siguiente error: " +  CHR(13) +  CHR(10) +;
							"Código interno de error: " + ALLTRIM(STR(oException.ErrorNo)) + CHR(13) +;
							"Mensaje de error: "  
				IF VARTYPE(gsErrorMsg)='C'  AND !EMPTY(gsErrorMsg) THEN
					sResultMsg = sResultMsg + gsErrorMsg
				ELSE
					sResultMsg = sResultMsg + (oException.Message) + CHR(13)
				ENDIF
			FINALLY
				SELECT (nCurrentWorkArea)
			ENDTRY
		ENDIF

	ENDIF
	RETURN bReturnedValue
ENDFUNC


PROCEDURE checkRecExists

	LPARAMETERS sSourceTable as String, bRecIsUpdatable as Byte, commaSeparatedKey as String, bNotVerbose as Boolean, ;
				sLastAdjustment as strig, bCheckCaseDiscarded as Boolean 
	
	*Establece si en la tabla sSourceTable ya existe un registro con clave commaSeparatedKey (lista de campos separada por comas). Los valores ;
	a buscar de los campos de la lista commaSeparatedKey, se toman de las variables de memoria (m.) asociadas a ellos. En caso de que exista un ;
	registro, copia sus datos a un conjunto de variables DE memoria asociadas, es decir, hace un SCATTER MEMVAR. ;
	En caso de que el parámetro opcional commaSeparatedKey no se pase, la lista se toma de lo que resulte de la aplicación del método ;
	setDefaultKey() de la clase AnomaliesSolver
	
	*Devuelve en bRecIsUpdatable uno de los siguientes valores: ;
		0:	Existe un registro y ya fue notificado ;
		1:	Existe un registro y aún no ha sido notificado, por tanto, puede ser actualizado ;
		2:	No existe un registro
	
	LOCAL oAnomaliesSolver AS Object
	
	DIMENSION aExistentRecords(1)

	IF VARTYPE(commaSeparatedKey)='L' THEN
		oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.PRG")
		oAnomaliesSolver.sSourceTableName = sSourceTable
		= oAnomaliesSolver.setDefaultKey()
		commaSeparatedKey = oAnomaliesSolver.sDefaultKey
		RELEASE oAnomaliesSolver
	ENDIF
	plusSeparatedKey  = STRTRAN(commaSeparatedKey,',','+')

	plusSeparatedValues  = ''
	nSourceTableFields=AFIELDS(aSourceTableFields,sSourceTable )
	nFieldNamesToLookFor=ALINES(aFieldNamesToLookFor,commaSeparatedKey ,1,',')
	FOR nField=1 TO nFieldNamesToLookFor
		nFieldRow=ASCAN(aSourceTableFields,aFieldNamesToLookFor(nField),-1,-1,1,15)
		nFieldWidth=aSourceTableFields(nFieldRow,3)
		plusSeparatedValues  = plusSeparatedValues  + " PADR(m." + aFieldNamesToLookFor(nField) + "," + ALLTRIM(STR(nFieldWidth)) + ",' ') +"
	NEXT nField
	plusSeparatedValues=LEFT(plusSeparatedValues,LEN(plusSeparatedValues)-1)

	SQLClause="SELECT RECNO() AS nRec, AJUSTE, NOTIFICA FROM " + sSourceTable + " WHERE " + plusSeparatedKey + "=" + ;
				+ plusSeparatedValues + " ORDER BY " + ADJUSTMENT_DATE_FIELDNAME + " DESC, nRec DESC" + " INTO ARRAY aExistentRecords"
	&SQLClause	
	IF _TALLY>0 THEN
		nExistentRecords = _TALLY
		
		SELECT (sSourceTable)
		GO (aExistentRecords(1,1))
		SCATTER MEMVAR
		IF NOTIFICA = 0
			IF !bNotVerbose THEN
		        showErrorMessage ('La ficha ya fué ingresada al sistema' + CHR(13) + 'Datos disponibles para actualización.', 0,0)
			ENDIF
        	bRecIsUpdatable=1
		ELSE
			IF !bNotVerbose THEN
				showErrorMessage('La ficha ya fue notificada! No podrá hacer actualizaciones!',0,0)
			ENDIF
        	bRecIsUpdatable=0
		ENDIF
		sLastAdjustment = aExistentRecords(1,2)
		
		*Establece si en el conjunto de registros encontrados hay alguno descartado con ajuste CAPTURE_DISCARD_ADJUSTMENT que ya ha sido notificado
		IF bCheckCaseDiscarded THEN
			FOR i=1 TO nExistentRecords
				IF aExistentRecords(i,2)=CAPTURE_DISCARD_ADJUSTMENT AND aExistentRecords(i,3)>0 AND bCheckCaseDiscarded 
					sLastAdjustment = CAPTURE_DISCARD_ADJUSTMENT 
					bCheckCaseDiscarded = .F.
				ENDIF
			NEXT i
		ENDIF
	ELSE
		bRecIsUpdatable = 2
	ENDIF
	
ENDPROC

PROCEDURE checkRecExistsTst(sRecordSourceName as String, nRecID as number)

	LOCAL bRecIsUpdatable as Byte, sLastAdjustment as string, bCheckCaseDiscarded as Boolean 
	
	SET STEP ON 
	*sKeySearched = STRTRAN(STRTRAN(ALLTRIM(fieldValue('TAG_EXPR','SIVIGILAPKs',"TABLE_NAME='" + sRecordSourceName + "'")),'+NUM_CON'),'+',',')
	TRY
		USE (sRecordSourceName)
	CATCH TO oException
	ENDTRY
	SELECT (sRecordSourceName)
	GO nRecID 
	SCATTER MEMVAR FIELDS EXCEPT SEMANA, AÑO, COD_PRE, COD_SUB
	
	DO checkRecExists WITH sRecordSourceName, bRecIsUpdatable, 'COD_EVE,TIP_IDE,NUM_IDE' , .T. ,  sLastAdjustment, .T.
	?sLastAdjustment
ENDPROC


FUNCTION RecExists

	LPARAMETERS sSourceTable as String, sKeyExpressionToLookFor as String

	*Busca en sSourceTable un registro que cumpla la condición sKeyExpressionToLookFor. En caso de que tal registro exista, ;
	retorna el número del registro según lo determina la función RECNO(); en caso contrario, retorna 0.
	* Si sSourceTable no se encuentra abierta al momento de la llamada, luego de la ejecución quedará abierta en la primera ;
	area de trabajo disponible.
	
	bReturnedValue=0
	IF !USED(sSourceTable) THEN
		IF FILE(sSourceTable + ".DBF" ) THEN
			USE (sSourceTable) IN 0
		ENDIF
	ENDIF

	IF USED(sSourceTable) THEN
		SELECT RECNO() as nReg FROM &sSourceTable WHERE &sKeyExpressionToLookFor INTO ARRAY AExistentRecords
		IF _TALLY>0 THEN
			GO AExistentRecords(1)
			bReturnedValue=ALEN(AExistentRecords,1)
		ENDIF
	ENDIF
			
	RETURN bReturnedValue

ENDFUNC



FUNCTION SetExistentRecords

	LPARAMETERS sCandidatesRSName as String, sMasterTableName as String, sKeyExpressionToLookFor as String, ;
				sFilterCondition as String, sMasterKeyExpression as String
	
	
	*Retorna el número de registros de la tabla sCandidatesRSName que cumplan la condición sFilterCondition y que existen en ;
	la tabla sMasterTableName. Como efecto colateral crea un cursor llamado rsExistentRecords con tales registros
	*Se considera que un registro de sCandidatesRSName existe en la tabla sMasterTableName si sKeyExpressionToLookFor  ;
	ya se encuentra en la tabla maestra. sKeyExpressionToLookFor debe ser una expresion de la forma FieldName1 + ... + FiledNameN.
	*Si se pasa el parámetro opcional sMasterKeyExpression, sKeyExpressionToLookFor se compara contra  sMasterKeyExpression.
	*La función determina los registros existentes lanzando una sentencia SQL de la forma ;
		SELECT * FROM sCandidatesRSName WHERE sFilterCondition AND sKeyExpressionToLookFor ;
			IN (SELECT sKeyExpressionToLookFor FROM sMasterTableName ...) ;
	o de la forma ;
		SELECT * FROM sCandidatesRSName WHERE sFilterCondition AND sKeyExpressionToLookFor ;
			IN (SELECT sMasterKeyExpression FROM sMasterTableName ...) ;
	

	setExistentsRecsClause="SELECT * FROM " + sCandidatesRSName + " WHERE " + sFilterCondition + "  AND " ;
							+ qualifyFields(sKeyExpressionToLookFor,"+",sCandidatesRSName) + " IN (SELECT "
	IF VARTYPE(sMasterKeyExpression)='C' THEN
		IF !EMPTY(sMasterKeyExpression) THEN
			setExistentsRecsClause = setExistentsRecsClause + qualifyFields(sMasterKeyExpression,"+",sMasterTableName)
		ENDIF
	ELSE
		setExistentsRecsClause = setExistentsRecsClause + qualifyFields(sKeyExpressionToLookFor,"+",sMasterTableName)
	ENDIF
	setExistentsRecsClause = setExistentsRecsClause + " FROM " + sMasterTableName + ") INTO CURSOR rsExistentRecords NOFILTER"
	&setExistentsRecsClause
	RETURN _TALLY
ENDFUNC


FUNCTION qualifyFields

	LPARAMETERS sFieldsList as String, sSeparatorChar as String, sQualifier AS String

	DIMENSION aSourceFields(1,1)
	nSourceFields=ALINES(aSourceFields,sFieldsList,1,sSeparatorChar )
	DIMENSION aSourceFields(nSourceFields,1)

	STORE '' TO sQualifiesList
	
	FOR nField=1 TO nSourceFields
		IF !(sQualifier $ aSourceFields(nField,1)) THEN
			sQualifiesList=sQualifiesList + sSeparatorChar + sQualifier + "." + aSourceFields(nField,1)
		ELSE
			sQualifiesList=sQualifiesList + sSeparatorChar + aSourceFields(nField,1)
		ENDIF
	NEXT nField
	
	RETURN SUBSTR(sQualifiesList,2)
	  
ENDFUNC


FUNCTION UpdateFieldsTable

	LPARAMETERS sTargetTableName as String, sUpdateExpression as String, sSourceTableName as String, ;
				sMatchingFields as String

	*Actualiza los registros de la tabla sTargetTableName que existan en la tabla sSourceTableName fijando los valores de sus campos ;
	de acuerdo con sUpdateExpression. sUpdateExpression debe ser una expresion de la forma FieldName1 = Value1, ... , FieldNameN = ValueN
	*Se considera que un registro de sSourceTableName existe en la tabla sTargetTableName si los campos de sMatchingFields son iguales. ;
	sMatchingFields debe ser una expresion de la forma FieldName1, ... , FiledNameN

	sUpdateSQLClause = "UPDATE " + sTargetTableName + " SET " + sUpdateExpression + ;
						" FROM " + sSourceTableName + " WHERE "

	DIMENSION aMatchingFields (1,1)
	nMatchingFields=ALINES(aMatchingFields, sMatchingFields, 1, ',')
	DIMENSION aMatchingFields(nMatchingFields,1)
	sJoinCondition = '.T.'
	FOR nField=1 TO nMatchingFields
		sJoinCondition = sJoinCondition + " AND " + sTargetTableName + "." + aMatchingFields(nField,1) + "=" + ;
						sSourceTableName + "."  + aMatchingFields(nField,1)
	NEXT nField
	sUpdateSQLClause = sUpdateSQLClause + sJoinCondition 
	&sUpdateSQLClause
	
	RETURN _TALLY
ENDFUNC


FUNCTION DeleteSIVIGILAComplementaryData

	LPARAMETERS sSourceTableName as String, sMatchingFields as String, sFilterCondition as String, ;
				bMakeXLSBackupFile as Boolean, sXlsFilePath as String, sXLSFileNameSuffix as String, ;
				sSlaveFilterCondition as String)

	*Elimina de las tablas EVENTOS_xx y LABORATORIOS, los registros de la tabla sSourceTableName que cumplan la condición;
	sFilterCondition (si es que se pasa este parámetro opcional), que existan en ellas de acuerdo con sMatchingFields y ;
	que cumplan lA condición sSlaveFilterCondition (si es que se pasa este parámetro opcional). 
	*Se considera que un registro de sSourceTableName existe en las tablas EVENTOS_xx y LABORATORIOS si los campos de ;
	sMatchingFields son iguales. sMatchingFields debe ser una expresion de la forma FieldName1 + ... + FiledNameN.
	
	*Si bMakeXLSBackupFile = .T. los registros eliminados serán exportados a un archivo XLS o CSV que tendrá nombre ;
	EVENTOS_xx + sXLSFileNameSuffix y quedará ubicado en sXlsFilePath. 


	&&#DEFINE nMaxRowsForXLS 65000
	&&#DEFINE sExportingMsg "Exportando a excel..."

	LOCAL sExportedFileType as String, xlsFileName as String
	
	*Determina qué eventos se encuentran en sSourceTableName 
	sSelectSQLClause = "SELECT DISTINCT 'EVENTOS_'" + "+PADL(ALLTRIM(EVENTOS.FORMULARIO),2,'0') " +;
						" AS EventTableName FROM " + sSourceTableName + " INNER JOIN EVENTOS ON " + ;
						sSourceTableName + ".COD_EVE = EVENTOS.COD_EVE INTO CURSOR rsTargetTables"
	&sSelectSQLClause 
	
	nDeletedRecs = 0
	IF _TALLY>0 THEN
		*Elimina registros coincidentes de cada una de las tablas de eventos que se encontraron en sSourceTableName 
		SELECT rsTargetTables
		SCAN
			IF FILE(rsTargetTables.EventTableName + ".DBF") THEN
				sDeleteSQLClause = "DELETE FROM " + rsTargetTables.EventTableName + " WHERE " +;
									qualifyFields(sMatchingFields ,"+",rsTargetTables.EventTableName) + " IN (SELECT " +;
									qualifyFields(sMatchingFields ,"+",sSourceTableName) + " FROM " + sSourceTableName
				IF VARTYPE(sFilterCondition)='C' THEN
					IF !EMPTY(sFilterCondition) THEN
						sDeleteSQLClause = sDeleteSQLClause + " WHERE " + sFilterCondition
					ENDIF
				ENDIF
				sDeleteSQLClause = sDeleteSQLClause + ")"
				
				IF VARTYPE(sSlaveFilterCondition)='C' THEN
					IF !EMPTY(sSlaveFilterCondition) THEN
						sDeleteSQLClause = sDeleteSQLClause + " AND " + sSlaveFilterCondition
					ENDIF
				ENDIF

				&sDeleteSQLClause
				nDeletedRecs = nDeletedRecs + _TALLY
				
				IF bMakeXLSBackupFile THEN
					DO exportDeletedRecsToXLS WITH rsTargetTables.EventTableName, sXLSFileNameSuffix, sXlsFilePath  
				ENDIF					
			ENDIF
		ENDSCAN

		*Elimina registros coincidentes de la tabla LABORATORIOS 
		sDeleteSQLClause = "DELETE FROM LABORATORIOS WHERE " + qualifyFields(sMatchingFields ,"+","LABORATORIOS") +;
							" IN (SELECT " + qualifyFields(sMatchingFields ,"+",sSourceTableName) + ;
							" FROM " + sSourceTableName
		IF VARTYPE(sFilterCondition)='C' THEN
			IF !EMPTY(sFilterCondition) THEN
				sDeleteSQLClause = sDeleteSQLClause + " WHERE " + sFilterCondition
			ENDIF
		ENDIF	
		sDeleteSQLClause = sDeleteSQLClause + ")"

		IF VARTYPE(sSlaveFilterCondition)='C' THEN
			IF !EMPTY(sSlaveFilterCondition) THEN
				sDeleteSQLClause = sDeleteSQLClause + " AND " + sSlaveFilterCondition
			ENDIF
		ENDIF

		&sDeleteSQLClause
		nDeletedRecs = nDeletedRecs + _TALLY
		IF bMakeXLSBackupFile THEN
			DO exportDeletedRecsToXLS WITH "LABORATORIOS", sXLSFileNameSuffix, sXlsFilePath  
		ENDIF					
	ENDIF
	
	RETURN nDeletedRecs
ENDFUNC


FUNCTION UpdateSIVIGILAComplementaryData_V01

	LPARAMETERS sMasterTableName as String, sKeyExpressionToLookFor as String, sFilterCondition as String, ;
				sMasterKeyExpression as String, sUpdateExpression as String, sUserMsg as String

	*Actualiza en las tablas EVENTOS_xx y LABORATORIOS, los registros de la tabla sMasterTableName que cumplan la condición;
	sFilterCondition (si es que se pasa este parámetro opcional) y que tengan un registro coincidente en ellas ;
	fijando los valores de sus campos de acuerdo con sUpdateExpression. ;
	sUpdateExpression debe ser una expresion de la forma FieldName1 = Value1, ... , FieldNameN = ValueN.
	 
	*Se considera que un registro de sMasterTableName tiene un coincidente en las tablas EVENTOS_xx y LABORATORIOS ;
	si sKeyExpressionToLookFor se encuentra en ellas. ;
	sKeyExpressionToLookFor debe ser una expresion de la forma FieldName1 + ... + FiledNameN. ;
	*Si se pasa el parámetro opcional sMasterKeyExpression, sKeyExpressionToLookFor se compara contra sMasterKeyExpression.
	
	*El parámetro opcional sUserMsg permite especificar un mensaje que se mostrará al usuario mientras se ejecuta ;
	la actualización de registros. En tal caso, al mensaje suministrado se le agrega el postfijo "EVENTOS_xx"


	&&#DEFINE nMaxRowsForXLS 65000
	&&#DEFINE sExportingMsg "Exportando a excel..."

	LOCAL sExportedFileType as String, xlsFileName as String
	
	*Determina qué eventos se encuentran en sMasterTableName  
	sSelectSQLClause = "SELECT DISTINCT 'EVENTOS_'" + "+PADL(ALLTRIM(EVENTOS.FORMULARIO),2,'0') " +;
						" AS EventTableName FROM " + sMasterTableName  + " INNER JOIN EVENTOS ON " + ;
						sMasterTableName + ".COD_EVE = EVENTOS.COD_EVE INTO CURSOR rsTargetTables"
	IF VARTYPE(sFilterCondition)='C' THEN
		IF !EMPTY(sFilterCondition) THEN
			sSelectSQLClause = sSelectSQLClause + " WHERE " + sFilterCondition
		ENDIF
	ENDIF	
	&sSelectSQLClause 
	
	nUpdatedRecs = 0
	IF _TALLY>0 THEN
		*Actualiza registros coincidentes de cada una de las tablas de eventos que se encontraron en sMasterTableName  
		SELECT rsTargetTables
		SCAN
			IF FILE(rsTargetTables.EventTableName + ".DBF") THEN
				sUpdateSQLClause = "UPDATE " + rsTargetTables.EventTableName + "  SET " + sUpdateExpression + ;
									" FROM " + sMasterTableName + " WHERE " + ;
									qualifyFields(sKeyExpressionToLookFor,"+",rsTargetTables.EventTableName) + " = "
				IF VARTYPE(sMasterKeyExpression)='C' THEN
					IF !EMPTY(sMasterKeyExpression) THEN
						sUpdateSQLClause = sUpdateSQLClause + qualifyFields(sMasterKeyExpression,"+",sMasterTableName ) 
					ENDIF
				ELSE
					sUpdateSQLClause = sUpdateSQLClause + qualifyFields(sKeyExpressionToLookFor,"+",sMasterTableName ) 
				ENDIF
													
				IF VARTYPE(sFilterCondition)='C' THEN
					IF !EMPTY(sFilterCondition) THEN
						sUpdateSQLClause = sUpdateSQLClause + " AND " + sFilterCondition
					ENDIF
				ENDIF	

				IF VARTYPE(sUserMsg)='C' THEN
					IF !EMPTY(sUserMsg) THEN
						WAIT sUserMsg + " " + rsTargetTables.EventTableName  WINDOW NOWAIT
					ENDIF
				ENDIF	
				
				TRY
					&sUpdateSQLClause 
					nUpdatedRecs = nUpdatedRecs + _TALLY
				CATCH TO oException
				FINALLY
					WAIT CLEAR
				ENDTRY

				
			ENDIF
		ENDSCAN

		*Actualiza registros coincidentes de la tabla LABORATORIOS 
		sUpdateSQLClause = "UPDATE LABORATORIOS " + "  SET " + sUpdateExpression + ;
							" FROM " + sMasterTableName + " WHERE " + ;
							qualifyFields(sKeyExpressionToLookFor,"+","LABORATORIOS") + " = "
		IF VARTYPE(sMasterKeyExpression)='C' THEN
			IF !EMPTY(sMasterKeyExpression) THEN
				sUpdateSQLClause = sUpdateSQLClause + qualifyFields(sMasterKeyExpression,"+",sMasterTableName ) 
			ENDIF
		ELSE
			sUpdateSQLClause = sUpdateSQLClause + qualifyFields(sKeyExpressionToLookFor,"+",sMasterTableName ) 
		ENDIF
											
		IF VARTYPE(sFilterCondition)='C' THEN
			IF !EMPTY(sFilterCondition) THEN
				sUpdateSQLClause = sUpdateSQLClause + " AND " + sFilterCondition
			ENDIF
		ENDIF
		
		bContinue = .T.
		TRY
			&sUpdateSQLClause 
		CATCH TO oException
			bContinue = .F.
		ENDTRY
		IF bContinue THEN
			nUpdatedRecs = nUpdatedRecs + _TALLY
		ENDIF
	ENDIF
	
	RETURN nUpdatedRecs
ENDFUNC


FUNCTION UpdateSIVIGILAComplementaryData

	LPARAMETERS sMasterTableName as String, sKeyExpressionToLookFor as String, sFilterCondition as String, ;
				sMasterKeyExpression as String, sUpdateExpression as String, sUserMsg as String

	*Actualiza en las tablas EVENTOS_xx y LABORATORIOS, los registros de la tabla sMasterTableName que cumplan la condición;
	sFilterCondition (si es que se pasa este parámetro opcional) y que tengan un registro coincidente en ellas ;
	fijando los valores de sus campos de acuerdo con sUpdateExpression. ;
	sUpdateExpression debe ser una expresion de la forma FieldName1 = Value1, ... , FieldNameN = ValueN.
	 
	*Se considera que un registro de sMasterTableName tiene un coincidente en las tablas EVENTOS_xx y LABORATORIOS ;
	si sKeyExpressionToLookFor se encuentra en ellas. ;
	sKeyExpressionToLookFor debe ser una expresion de la forma FieldName1 + ... + FiledNameN. ;
	*Si se pasa el parámetro opcional sMasterKeyExpression, sKeyExpressionToLookFor se compara contra sMasterKeyExpression.
	
	*El parámetro opcional sUserMsg permite especificar un mensaje que se mostrará al usuario mientras se ejecuta ;
	la actualización de registros. En tal caso, al mensaje suministrado se le agrega el postfijo "EVENTOS_xx"


	&&#DEFINE nMaxRowsForXLS 65000
	&&#DEFINE sExportingMsg "Exportando a excel..."

	LOCAL sExportedFileType as String, xlsFileName as String
	
	*Determina qué eventos se encuentran en sMasterTableName  
	sSelectSQLClause = "SELECT DISTINCT 'EVENTOS_'" + "+PADL(ALLTRIM(EVENTOS.FORMULARIO),2,'0') " +;
						" AS EventTableName FROM " + sMasterTableName  + " INNER JOIN EVENTOS ON " + ;
						sMasterTableName + ".COD_EVE = EVENTOS.COD_EVE INTO CURSOR rsTargetTables"
	IF VARTYPE(sFilterCondition)='C' THEN
		IF !EMPTY(sFilterCondition) THEN
			sSelectSQLClause = sSelectSQLClause + " WHERE " + qualifyFields(sFilterCondition,"+",sMasterTableName)
		ENDIF
	ENDIF	
	&sSelectSQLClause 
	
	nUpdatedRecs = 0
	IF _TALLY>0 THEN
		*Actualiza registros coincidentes de cada una de las tablas de eventos que se encontraron en sMasterTableName  
		SELECT rsTargetTables
		SCAN
			IF FILE(rsTargetTables.EventTableName + ".DBF") THEN
				sUpdateSQLClause = "UPDATE " + rsTargetTables.EventTableName + "  SET " + sUpdateExpression + ;
									" FROM " + rsTargetTables.EventTableName + " INNER JOIN " + sMasterTableName + " ON " + ;
									qualifyFields(sKeyExpressionToLookFor,"+",rsTargetTables.EventTableName) + " = "
				IF VARTYPE(sMasterKeyExpression)='C' THEN
					IF !EMPTY(sMasterKeyExpression) THEN
						sUpdateSQLClause = sUpdateSQLClause + qualifyFields(sMasterKeyExpression,"+",sMasterTableName ) 
					ENDIF
				ELSE
					sUpdateSQLClause = sUpdateSQLClause + qualifyFields(sKeyExpressionToLookFor,"+",sMasterTableName ) 
				ENDIF
													
				IF VARTYPE(sFilterCondition)='C' THEN
					IF !EMPTY(sFilterCondition) THEN
						sUpdateSQLClause = sUpdateSQLClause + " AND " + sFilterCondition
					ENDIF
				ENDIF	

				IF VARTYPE(sUserMsg)='C' THEN
					IF !EMPTY(sUserMsg) THEN
						WAIT sUserMsg + " " + rsTargetTables.EventTableName  WINDOW NOWAIT
					ENDIF
				ENDIF	
				
				TRY
					&sUpdateSQLClause 
					nUpdatedRecs = nUpdatedRecs + _TALLY
				CATCH TO oException
				FINALLY
					WAIT CLEAR
				ENDTRY

				
			ENDIF
		ENDSCAN

		*Actualiza registros coincidentes de la tabla LABORATORIOS 
		sUpdateSQLClause = "UPDATE LABORATORIOS " + "  SET " + sUpdateExpression + ;
							" FROM " + sMasterTableName + " WHERE " + ;
							qualifyFields(sKeyExpressionToLookFor,"+","LABORATORIOS") + " = "
		IF VARTYPE(sMasterKeyExpression)='C' THEN
			IF !EMPTY(sMasterKeyExpression) THEN
				sUpdateSQLClause = sUpdateSQLClause + qualifyFields(sMasterKeyExpression,"+",sMasterTableName ) 
			ENDIF
		ELSE
			sUpdateSQLClause = sUpdateSQLClause + qualifyFields(sKeyExpressionToLookFor,"+",sMasterTableName ) 
		ENDIF
											
		IF VARTYPE(sFilterCondition)='C' THEN
			IF !EMPTY(sFilterCondition) THEN
				sUpdateSQLClause = sUpdateSQLClause + " AND " + sFilterCondition
			ENDIF
		ENDIF
		
		bContinue = .T.
		TRY
			&sUpdateSQLClause 
		CATCH TO oException
			bContinue = .F.
		ENDTRY
		IF bContinue THEN
			nUpdatedRecs = nUpdatedRecs + _TALLY
		ENDIF
	ENDIF
	
	RETURN nUpdatedRecs
ENDFUNC


FUNCTION UseTable

	LPARAMETERS sTableNameToUse as String
	
	*Opens sTableNameToUse in first available workarea without selecting it. ;
	Returns .T. if it was posible to open sTableNameToUse or it was already opened, otherwise returns .F.
	
	LOCAL bReturnedValue AS Boolean 
		
	bReturnedValue=.T.
	IF !USED(RIGHT(sTableNameToUse,LEN(sTableNameToUse)-RAT("\",sTableNameToUse))) THEN
		IF FILE(sTableNameToUse + ".DBF") THEN
			USE (sTableNameToUse) IN 0
		ELSE
			bReturnedValue=.F.
		ENDIF
	ENDIF
	RETURN bReturnedValue
ENDFUNC


FUNCTION IsField

	LPARAMETERS sTableName as String, sFieldName as String
	
	LOCAL bReturnedValue AS Boolean 
	
	*Returns .T. if sFieldName is a sTableName field, otherwise returns .F.
	
	bReturnedValue=.F.
	IF UseTable(sTableName) THEN
		nSourceFields=AFIELDS(aSourceFields,SUBSTR(sTableName,RAT('\',sTableName)+1))
		IF ASCAN(aSourceFields,sFieldName ,-1,-1,-1,15) > 0 THEN
			bReturnedValue=.T.
		ENDIF
	ENDIF
	RETURN bReturnedValue

ENDFUNC


PROCEDURE exportDeletedRecsToXLS

	LPARAMETERS rsToExportName AS String, sXLSFileNameSuffix as String, sXlsFilePath as String
	
	*Exporta los registros borrados de rsToExportName a un archivo con formato XLS o CSV, dependiendo de si ;
	el número de registros en rsToExportName es menor o mayor que nMaxXLSRows.
	
	*El archivo exportado tendrá nombre rsToExportName + sXLSFileNameSuffix y quedará ubicado en sXlsFilePath.
	

	&&#DEFINE nMaxRowsForXLS 65000
	
	LOCAL sExportedFileType as String, xlsFileName as String, sExportingMessage as String 
	LOCAL nSourceRecords
		
	sOldSetDeleted = SET("Deleted")
	IF !USED(rsToExportName) THEN
		USE (rsToExportName)
	ELSE
		SELECT (rsToExportName)
	ENDIF
	
	SET DELETED OFF
	sExportingMessage = "Exportando a XLS... " + rsToExportName 
	WAIT sExportingMessage WINDOW NOWAIT

	COUNT FOR DELETED() TO nSourceRecords
	IF nSourceRecords>nMaxRowsForXLS THEN
		*El número de registros a exportar es de tal magnitud que se hace la exportacióon hacia un archivo CSV;
		que es leido en forma nativa por Excel 2007 o superior
		sExportedFileType='CSV'
	ELSE
		sExportedFileType='XL5'
	ENDIF

	xlsFileName = rsToExportName
	IF VARTYPE(sXLSFileNameSuffix)='C' THEN
		xlsFileName = xlsFileName + sXLSFileNameSuffix
	ENDIF
	
	IF VARTYPE(sXlsFilePath)='C' THEN
		IF EMPTY(sXlsFilePath) THEN
			sXlsFilePath =  "."
		ENDIF
	ENDIF		
	sCopyToCmd="COPY TO [" + sXlsFilePath + "\" + xlsFileName + "] FOR DELETED() TYPE " + sExportedFileType
	&sCopyToCmd

	SET DELETED &sOldSetDeleted		
	WAIT CLEAR

ENDPROC


FUNCTION InsertDataIntoTable

	LPARAMETERS sSourceTableName as String, sTargetTableName as String, ;
				sConditioningTableName as String, sMatchingFields as String, ;
				sFilterCondition as String, sUserMsg as String

	*Inserta en la tabla sTargetTableName, los registros de la tabla sSourceTableName que cumplan la condición;
	sFilterCondition (si es que se pasa este parámetro opcional) y que existan en la tabla sConditioningTableName ;
	de acuerdo con sMatchingFields. 
	*Se considera que un registro de sSourceTableName existe en la tabla sConditioningTableName si los campos de ;
	sMatchingFields son iguales. sMatchingFields debe ser una expresion de la forma FieldName1 + ... + FiledNameN.
	*El parámetro opcional sUserMsg permite especificar un mensaje que se mostrará al usuiaro mientras se ejecuta ;
	la inserción de registros
	

	nInssertedRecs = 0
	sSourceTablePath = ''
	IF FILE(sTargetTableName + ".DBF") THEN
		sInsertSQLClause = "INSERT INTO [" + sTargetTableName + "] SELECT * FROM "
		IF AT('\',sSourceTableName)=0 THEN
			sSourceTablePath='.\'
		ENDIF
		sInsertSQLClause = sInsertSQLClause + "[" + sSourceTablePath + sSourceTableName + "] AS rsSourceTable WHERE " +;
							qualifyFields(sMatchingFields ,"+", "rsSourceTable") + " IN (SELECT " +;
							qualifyFields(sMatchingFields ,"+","rsConditioningTable") + " FROM [" + sConditioningTableName + ;
							"] AS rsConditioningTable)"
		IF VARTYPE(sFilterCondition)='C' THEN
			IF !EMPTY(sFilterCondition) THEN
				sInsertSQLClause = sInsertSQLClause + " AND " + sFilterCondition
			ENDIF
		ENDIF
		IF VARTYPE(sUserMsg)='C' THEN
			IF !EMPTY(sUserMsg) THEN
				WAIT sUserMsg WINDOW NOWAIT
			ENDIF
		ENDIF	
		&sInsertSQLClause 
		WAIT CLEAR
		nInssertedRecs = nInssertedRecs + _TALLY
		
		*Elimina de sTargetTableName posibles registros duplicados
		oAnomaliesSolver=NEWOBJECT("AnomaliesSolver","AnomaliesSolver.Prg",.null.,SUBSTR(sTargetTableName ,RAT('\',sTargetTableName)+1))
		oAnomaliesSolver.deleteDuplicates()
		RELEASE oAnomaliesSolver

	ENDIF
	
	RETURN nInssertedRecs 
ENDFUNC


*Procedimientos de prueba diseñados para algunos de los componentes de esta biblioteca

PROCEDURE InsertDataIntoTableTst
	
	*Interfaz de InsertDataIntoTable
	*LPARAMETERS sSourceTableName as String, sTargetTableName as String, ;
				sConditioningTableName as String, sMatchingFields as String, ;
				sFilterCondition as String

*SET STEP ON 	
	?InsertDataIntoTable(sSourceTableName, sTargetTableName, sConditioningTableName, ;
						 sMatchingFields, sFilterCondition, "Insertando registros en " + sTargetTableName)

ENDPROC


PROCEDURE UpdateSIVIGILAComplementaryDataTst

	*Interfaz de InsertDataIntoTable UpdateSIVIGILAComplementaryData
	*LPARAMETERS sMasterTableName as String, sKeyExpressionToLookFor as String, sFilterCondition as String, ;
				sMasterKeyExpression as String, sUpdateExpression as String

SET STEP ON				
	*?UpdateSIVIGILAComplementaryData("RecalculoDeSemanasEpidemiologicas","AÑO + SEMANA + COD_EVE+ TIP_IDE+ NUM_IDE+ COD_PRE+ COD_SUB", ;
									 "RecalculoDeSemanasEpidemiologicas.Tablafuente='PACIENTE'", ;
									 "ClaveEncadenante","AÑO=RecalculoDeSemanasEpidemiologicas.AÑO,SEMANA=RecalculoDeSemanasEpidemiologicas.SEMANA",;
									 "Actualizando datos de la tabla")

	*?UpdateSIVIGILAComplementaryData("RecalculoDeSemanasEpidemiologicas","COD_EVE + AÑO + SEMANA + COD_MUN + NUM_CON + COD_PRE + COD_SUB", ;
									 "RecalculoDeSemanasEpidemiologicas.Tablafuente='BROTES'", ;
									 "ClaveEncadenante","AÑO=RecalculoDeSemanasEpidemiologicas.AÑO,SEMANA=RecalculoDeSemanasEpidemiologicas.SEMANA",;
									 "Actualizando datos de la tabla")

	?UpdateSIVIGILAComplementaryData("PACIENTE","AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB", ;
									 "PACIENTE.AÑO+PACIENTE.SEMANA+PACIENTE.COD_EVE+PACIENTE.TIP_IDE+PACIENTE.NUM_IDE+PACIENTE.COD_PRE+PACIENTE.COD_SUB='" + ;
									 "201316580 TI2123123          760010000001'", , "NUM_IDE = '79123456'")

ENDPROC



FUNCTION CountTableRecords

	LPARAMETERS sourceTableName,  sCountFilter AS String

	nSelectedWorkArea=SELECT()
	bSourceTableIsOpen= USED(sourceTableName) 

	sSQLClause='SELECT COUNT(*) AS n FROM ' + (sourceTableName)
	IF VARTYPE(sCountFilter )='C' THEN
		sSQLClause = sSQLClause + ' WHERE ' + sCountFilter 
	ENDIF
	sSQLClause = sSQLClause + ' INTO ARRAY arrayTableRecords '
	&sSQLClause
	IF !bSourceTableIsOpen THEN
		SELECT (sourceTableName)
		USE
	ENDIF
	SELECT (nSelectedWorkArea)
	RETURN arrayTableRecords (1)
ENDPROC



PROCEDURE CloseTables(tablesNameList as String)
	*Closes the tables in tablesNameList -a comma separated list of tables.
	 
	LOCAL i as Byte
	
	nTablesToClose = ALINES(aTablesToClose,tablesNameList,1,",")
	FOR i=1 TO nTablesToClose 
		IF USED(aTablesToClose(i)) THEN
			SELECT (aTablesToClose(i))
			USE
		ENDIF
	ENDFOR
ENDPROC



FUNCTION vecSum

	LPARAMETERS vVector as Variant

	*Retorma la suma de los componentes de vVector expecificado como una lista de valores separados por comas
	
	DIMENSION aVector(1)

	nReturnedValue=0
	nComponentes=ALINES(aVector,vVector,15,',')
	FOR nComponent=1 TO nComponentes
		sComponentType=VARTYPE(aVector(nComponent))
		DO CASE
			CASE sComponentType='C'
				nReturnedValue=nReturnedValue + VAL(aVector(nComponent))
			CASE sSonControlSourceType='N'
				nReturnedValue=nReturnedValue + aVector(nComponent)
			OTHERWISE
		ENDCASE
	NEXT 	
	RETURN nReturnedValue
ENDFUNC


PROCEDURE ResynchView
	
	lparameters oContainer, toControl
	
	#DEFINE BUFFERSPACE -20
	
	Local lnXa, lnXb, lnYa, lnYb, lnCtrlXb, lnCtrlYb
	
	lnXa = oContainer.ViewportLeft
	lnXb = lnXa + oContainer.ViewPortWidth
	lnYa = oContainer.ViewportTop
	lnYb = lnYa + oContainer.ViewPortHeight
	lnCtrlXb = toControl.Left + toControl.Width
	lnCtrlYb = toControl.Top + toControl.Height

	if toControl.left < lnXa or lnCtrlXb > lnXb;
	   OR toControl.Top < lnYa or lnCtrlYb > lnYb

	   oContainer.setviewport(toControl.left + BUFFERSPACE, toControl.top + BUFFERSPACE)

	ENDIF
ENDPROC


PROCEDURE tableFields

	LPARAMETERS sTargetTableName as string, sFieldsList as String
	
	LOCAL nReturnedValue as Boolean, nSelectedWorkArea as Byte, bSourceTableIsOpen as Boolean 
	LOCAL iField as Byte, nFields as Byte  
	
	DIMENSION aTargetFields(1)
	
	
	nReturnedValue=.F.

	nSelectedWorkArea=SELECT()
	bSourceTableIsOpen= USED(sTargetTableName ) 

	IF useTable(sTargetTableName)
		SELECT (sTargetTableName ) 
		nFields = AFIELDS(aTargetFields,sTargetTableName)
		FOR iField=1 TO nFields 
			sFieldsList = sFieldsList + aTargetFields(iField,1) + ","
		NEXT 
		sFieldsList = LEFT(sFieldsList,LEN(sFieldsList)-1)
	ENDIF
	
	IF !bSourceTableIsOpen THEN
		SELECT (sTargetTableName)
		USE
	ENDIF
	SELECT (nSelectedWorkArea)
	RETURN nReturnedValue

ENDFUNC


FUNCTION compareTimePeriod
	LPARAMETERS sTime1 as String, sTime2 as String
	
	*Returns: ;
		-1 if sTime1 "is less than " sTime2 ;
		0  if sTime1 "is equal to" sTime2 ;
		1  if sTime1 "is gfrater than" sTime2 ;
		2  otherwise
		
	*sTime1 and sTime are time specifications in the format mm:hh:dd

	LOCAL ARRAY aLocalVersion(1,3)
	LOCAL ARRAY aForeignVersion(1,3)
	LOCAL iNMajorLocalVersion as Integer, iNMinorLocalVersion as integer, iNBuildLocalVersion as integer
	LOCAL iNMajorForeignVersion as Integer, iNMinorForeignVersion as integer, iNBuildForeignVersion as integer

	iReturnedValueForLess = -1
	iReturnedValueForEqual = 0
	iReturnedValueForBig = 1
	iReturnedValueForOtherwise = -2
	iReturnedValue = iReturnedValueForOtherwise
	
	* Validar que las expresiones que se pasan están bien formadas, esto es, de la forma ##:##:##
	IF AT(":", sTime1, 2) > 0 AND AT(":", sTime2, 2) > 0
		* Las cadenas no traen los caracteres ':'

		ALINES(aLocalVersion, sTime2 , 1, ':')
		ALINES(aForeignVersion, sTime1 , 1, ':')	
		
		iNMajorForeignVersion = INT(VAL(aForeignVersion(3)))
		iNMinorForeignVersion = INT(VAL(aForeignVersion(2)))
		iNBuildForeignVersion = INT(VAL(aForeignVersion(1)))
		
		iNMajorLocalVersion = INT(VAL(aLocalVersion(3)))
		iNMinorLocalVersion = INT(VAL(aLocalVersion(2)))
		iNBuildLocalVersion = INT(VAL(aLocalVersion(1)))
		
		*/ Validación para NMajor
		IF VARTYPE(iNMajorLocalVersion) = 'N' AND VARTYPE(iNMajorForeignVersion) = 'N'
			IF iNMajorForeignVersion >= iNMajorLocalVersion
				iReturnedValue = IIF(iNMajorForeignVersion > iNMajorLocalVersion, iReturnedValueForBig, iReturnedValueForEqual)
			ELSE
				iReturnedValue = iReturnedValueForLess
			ENDIF
		ELSE
			iReturnedValue = iReturnedValueForOtherwise
		ENDIF
			
		*/ Validación para NMinor
		IF iReturnedValue = iReturnedValueForEqual
			IF VARTYPE(iNMinorLocalVersion) = 'N' AND VARTYPE(iNMinorForeignVersion) = 'N'
				IF iNMinorForeignVersion >= iNMinorLocalVersion
					iReturnedValue = IIF(iNMinorForeignVersion > iNMinorLocalVersion, iReturnedValueForBig, iReturnedValueForEqual)
				ELSE
					iReturnedValue = iReturnedValueForLess
				ENDIF
			ELSE
				iReturnedValue = iReturnedValueForOtherwise
			ENDIF
		ENDIF
		
		*/ Validación para nBuild
		IF iReturnedValue = iReturnedValueForEqual
			IF VARTYPE(iNBuildLocalVersion) = 'N' AND VARTYPE(iNBuildForeignVersion) = 'N'
				IF iNBuildForeignVersion >= iNBuildLocalVersion
					iReturnedValue = IIF(iNBuildForeignVersion > iNBuildLocalVersion, iReturnedValueForBig, iReturnedValueForEqual)
				ELSE
					iReturnedValue = iReturnedValueForLess
				ENDIF
			ELSE
				iReturnedValue = iReturnedValueForOtherwise
			ENDIF
		ENDIF
	ENDIF
	RETURN iReturnedValue 
ENDFUNC

FUNCTION deleteSivigilaSingleData as Boolean
	LPARAMETERS sTableName, sFilterCondition,  sResultMsg
	
	* Funcion que realiza borrado de registros en la tabla sTableName
	* que cumpla la condición de filtrado sFilterCondition
	* Retorna .t.si la operacion es exitosa. .F. en caso contrario y
	* lo que produjo la situación de no-borrado se almacena en sResultMsg

	bReturnedValue = .F.
	bContinue = .T.
	sResultMsg = ""
	iOldSelect = SELECT(sTableName)
	bAffectedTableIsUsed = .F.
	lcFilterCondition = " .F. "
	
	IF VARTYPE(sFilterCondition) = "C"
		IF 	!EMPTY(sFilterCondition)
			lcFilterCondition = sFilterCondition
		ENDIF
	ENDIF
	
	IF !FILE (ALLTRIM(sTableName) + ".dbf")
		bContinue = .F.
		sResultMsg = "La tabla " + sTableName + " no existe"
	ENDIF
	
	IF bContinue
		
		TRY		
		
			IF USED(sTableName)	
				bAffectedTableIsUsed = .T.
			ENDIF
	
			sDeleteSQLClause = "DELETE FROM " + sTableName + "  WHERE " + lcFilterCondition 

			&sDeleteSQLClause
			IF _TALLY  < 1
				sResultMsg = "No se borraron registros. No hay registros que cumplan con el criterio."
			ELSE
				sResultMsg = "Se eliminaron " + TRANSFORM(_TALLY) + " Registros"
				bReturnedValue = .T.
			ENDIF

			IF !bAffectedTableIsUsed
				SELECT (sTableName)
				USE
			ENDIF

		CATCH TO oException
			sResultMsg="No se pudo borrar el registro. Se produjo el siguiente error: " +  CHR(13) +  CHR(10) +;
						"Código interno de error: " + ALLTRIM(STR(oException.ErrorNo)) + " : " + (oException.Message)

		ENDTRY
		
		
		SELECT (iOldSelect)
	ENDIF
	
	RETURN bReturnedValue
	
ENDFUNC


FUNCTION disableFormControls
	
	LPARAMETERS oContainer as object, sControlsList as String
	* Desactiva los controles cuyos nombres figuren en sControlsList  (lista separada por comas) y que pertenezcan al formulario oContainer 
	
	bReturnedvalue = .F.
	iCounter = 0
	
	IF ALINES(aControlsList,sControlsList,1,",") > 0
		FOR iCounter = 1 TO ALEN(aControlsList)
			TRY
				sDisableControl = "oContainer" + "." + aControlsList(iCounter) + ".Enabled = .F."
				&sDisableControl
			CATCH TO oException
			ENDTRY
			
		NEXT
		bReturnedValue = .T.
	ENDIF
	
	RETURN bReturnedValue
ENDFUNC

FUNCTION setVisibleFormControlsTo(oContainer as object, sControlsList as String, sVisible as String)

	* Establece a sVisible  el valor de la propiedad Visible de los controles cuyos nombres figuren en sControlsList (lista separada por comas);
	 y que pertenezcan al formulario oContainer 
	
	bReturnedvalue = .F.
	iCounter = 0
	
	IF ALINES(aControlsList,sControlsList,1,",") > 0
		FOR iCounter = 1 TO ALEN(aControlsList)
			TRY
				sSetVisibleControl = "oContainer" + "." + aControlsList(iCounter) + ".Visible = " + sVisible 
				&sSetVisibleControl 
			CATCH TO oException
			ENDTRY
			
		NEXT
		bReturnedValue = .T.
	ENDIF
	
	RETURN bReturnedValue
ENDFUNC

FUNCTION getKeyFields(sTableName as string) as String 
	
	* Retorna la clave primaria de la tabla sTableName según se encuentre definida en Tables.SivigilaPKS 
	* Si sTableName no se encuentra en  Tables.SivigilaPKS retorna una cadena de longitud cero.
	
	DIMENSION aKeyExpression (1)
	
	STORE "" TO sReturnedValue 
	
	sSelectSQLCmd  = "SELECT tag_expr FROM SivigilaPKS WHERE table_name = (sTableName) AND EXTRAS = 'PK' " + ;
						" INTO ARRAY aKeyExpression "
	&sSelectSQLCmd						
	IF _TALLY > 0
		sReturnedValue = aKeyExpression	
	ENDIF
	
	RETURN ALLTRIM(sReturnedValue)
ENDFUNC


FUNCTION caseIsAdjustable(sTargetTableName as String, sFilterCondition as String, lRecordId as long, sResultMsg as String, bCheckDiscardedCases as Boolean)
			
	* Retorna .T. si el caso determinado por sFilterCondition en la tabla sTargetTableName es ajustable, en caso contrario retorna .F. 
	* Como efecto colateral, retorna en lRecordId el número del registro ajustable y en sResultMsg, el mensaje resultado de la operación.;
	Si el caso no es ajustable, sResultmsg contiene la razón
	
	*Un caso es NO ajustable si se cumple una de las siguientes condiciones: ;
		1. Hay más de un registro que cumple con la condición sFilterCondition ;
		2. No hay registros que cumplan con la condición sFilterCondition ;
		3. El caso tiene más de un registro índice, es decir más de un registro con AJUSTE='0' ;
		4. Al menos un elemento de la clave del registro determinado por sFilterCondition está vacío ;
		5. Se cumple la condición establecida en la definición del parámetro bCheckDiscardedCases 
	
	*bCheckDiscardedCases (opcional): si es .T. declarará que un caso en no ajustable si en el conjunto de registros asociados al caso hay al menos uno ;
	con ajuste ='D'

	
	#define ANCIENT_DATE DATE(1800,01,01)

	LOCAL vKeyValue AS VARIANT, iOldSelect as Integer, lInitalRecordId as Long

	bReturnedValue = .T.
	lCandidateRecno = 0 
	lRecordId = 0
	bKeyIsNotValid = .F.
	DIMENSION aLastRecordLoadedToTable(2)

	iOriginalRecordsCounter = 0
	bNonNotifiedExists = .F.
	iOldSelect = SELECT()

	sSelectSqlCmd = "SELECT RECNO() AS nReg , * FROM (sTargetTableName)  WHERE " + sFilterCondition + " INTO CURSOR  rsResult"		
	&sSelectSqlCmd

	IF _TALLY = 1
		* Se extraen todos los registros que conforman el caso
		lInitalRecordId = rsResult.NREG
		sSelectSqlCmd = BuildSqlForGivenCase(sTargetTableName, rsResult.nReg)

		IF !EMPTY(sSelectSqlCmd)
			&sSelectSqlCmd
		ELSE
			bKeyIsNotValid = .T.
		ENDIF
	ELSE
		IF _TALLY > 1
			sResultMsg = "Hay más de un registro que cumple con la condición de filtrado"
		ELSE
			sResultMsg = "No hay registros que cumplan con la condición de filtrado"
		ENDIF		
		bReturnedValue = .F.
		bKeyIsNotValid = .T.
	ENDIF
	
	IF NOT bKeyIsNotValid
		* Si la clave es válida, entonces se procede a encontrar el registro candidato	
	
		IF _TALLY  >= 1
			IF _TALLY = 1
				lCandidateRecno = 1
				lRecordId = rsResult.nreg
				IF rsResult.notifica = 0
					sResultMsg = "Hay registros sin notificar"
					bNonNotifiedExists = .T.
				ENDIF			
				IF bCheckDiscardedCases THEN
					bReturnedValue = (rsResult.AJUSTE!=CAPTURE_DISCARD_ADJUSTMENT)
				ENDIF
			ELSE

				* Si en el caso hay varios registros, valida que no haya más de un registro con ajuse cero (0) e identifica cuál es el regitro ;
				con fecha más reciente de ajuste
				
				SELECT rsResult
				GO TOP
				dMostRecentDate = ANCIENT_DATE
				aLastRecordLoadedToTable(1)= RECNO()
				aLastRecordLoadedToTable(2) = rsResult.nReg

				SCAN 
					IF bCheckDiscardedCases AND bReturnedValue THEN
						IF rsResult.AJUSTE = CAPTURE_DISCARD_ADJUSTMENT AND rsResult.NOTIFICA>0 THEN
							bReturnedValue = .F.
						ENDIF
						IF !bReturnedValue THEN
							sResultMsg = "El caso fue descartado" + IIF(!EMPTY(rsResult.FEC_AJU),' el ' + DTOC(rsResult.FEC_AJU),'')
						ENDIF
					ENDIF
					
					iOriginalRecordsCounter	= IIF(rsResult.ajuste = '0' OR EMPTY(rsResult.ajuste), iOriginalRecordsCounter + 1, iOriginalRecordsCounter)		
					IF rsResult.notifica = 0
						ResultMsg = "Hay registros sin notificar"
						bNonNotifiedExists = .T.
					ENDIF
					IF rsResult.nReg > aLastRecordLoadedToTable(2)
						 aLastRecordLoadedToTable(1)= RECNO()
						 aLastRecordLoadedToTable(2) = rsResult.nReg
					ENDIF
					
					IF !EMPTY(rsResult.fec_aju )
						IF rsResult.fec_aju > dMostRecentDate
							dMostRecentDate = rsResult.fec_aju
							lCandidateRecno = RECNO()
							lRecordId = rsResult.nreg
						ELSE
							IF rsResult.fec_aju = dMostRecentDate
								lCandidateRecno = aLastRecordLoadedToTable(1)
								lRecordId = aLastRecordLoadedToTable(2)						
							ENDIF
						ENDIF
					ELSE
						IF !EMPTY(rsResult.fec_not)
							IF rsResult.fec_not > dMostRecentDate
								lCandidateRecno = RECNO()
								lRecordId = rsResult.nreg
								dMostRecentDate = rsResult.fec_not
							ELSE
								IF rsResult.fec_not = dMostRecentDate
									lCandidateRecno = aLastRecordLoadedToTable(1)
									lRecordId = aLastRecordLoadedToTable(2)						
								ENDIF
							ENDIF
						ELSE
							lCandidateRecno = aLastRecordLoadedToTable(1)
							lRecordId = aLastRecordLoadedToTable(2)
						ENDIF
					ENDIF
				ENDSCAN
			ENDIF		
		ELSE
			sResultMsg = "No hay registros que cumplan con el criterio"
			bReturnedValue = .F.
		ENDIF
	ENDIF

	IF bReturnedValue		
		* Establece el valor de la variable sResultMsg, según la naturaleza del resultado encontrado.
		IF iOriginalRecordsCounter > 1
			lRecordId  = 0
			IF EMPTY(sResultMsg)
				sResultMsg = "El caso tiene más de un registro índice asociado"
			ENDIF
			bReturnedValue = .F.
		ENDIF
		IF bKeyIsNotvalid 
			IF EMPTY(sResultMsg)
				sResultMsg = "Al menos un elemento de la clave está vacío."
			ENDIF
			lRecordId  = 0
			bReturnedValue = .F.
		ENDIF
		
		IF lInitalRecordId != lRecordId AND bReturnedValue
			IF bNonNotifiedExists THEN
				sResultMsg = ADJUST_ALREADY_EXISTS_MESSAGE
			ELSE
				sResultMsg = BD_REC_ALREADY_EXISTS_MESSAGE
			ENDIF 
		ENDIF
	ENDIF
	SELECT (iOldSelect)
		
	RETURN bReturnedValue

ENDFUNC

PROCEDURE caseIsAdjustableTst(nRecID AS Number )

	LOCAL nAdjustableRecordId as long, sResultMsg as String 
	
	SET STEP ON 
	*?caseIsAdjustable('PACIENTE','recno()=' + ALLTRIM(STR(nRecID)) , @nAdjustableRecordId, @sResultMsg, .T. )
	?caseIsAdjustable('EVENTOS_94','recno()=' + ALLTRIM(STR(nRecID)) , @nAdjustableRecordId, @sResultMsg, .F. )
ENDPROC


FUNCTION BuildSqlForGivenCase 

	PARAMETERS sTableName as String, lRecordNumber as Long, sFilterCondition
	
	* Construye una sentencia SELECT-SQL que permite recuperar de la tabla sTableName
	* todos los registros asociados con el caso al que pertenece el registro identificado
	* como lRecordNumber. Los registros asociados con un caso se establecen teniendo en cuenta
	* que tengan, los mismos valores en los campos clave de la tabla sTableName, según se encuentran
	* definidos en Tables.SivigilaPKS.
	* Si la clave correspondiente al registro, no puede ser construida
	* debido a que hay campos vacíos en el registro identificado por lrecordNumber, 
	* retornará una cadena vacía, en caso contrario retornará una sentencia SELECT-SQL lista para ser ejecutada.
	* La sentencia SELECT-SQL  construida, es de la forma
	* SELECT RECNO() AS nReg, * FROM (sTableName) WHERE ... INTO CURSOR rsResult
	
	LOCAL vKeyValue as Variant, sReturnedValue as String, lsFilterCondition as String
	sReturnedValue = ""
	bKeyIsNotvalid = .F.
	bDataTableIsUsed = USED(sTableName)
	iOldSelect = SELECT()
	
	IF VARTYPE(sFilterCondition) = "C"
		lsFilterCondition = " AND " + sFilterCondition
	ELSE
		lsFilterCondition  = ""
	ENDIF
		
	sTableKeyExpression = getKeyFields(sTableName)	
	
	* Se valida que la clave esté completa	
	IF !EMPTY(sTableKeyExpression)
		IF !bDataTableIsUsed
			SELECT 0
			USE (sTableName)
		ELSE
			SELECT (sTableName)
		ENDIF			
		GO (lRecordNumber)

		iKeyMembersCounter = ALINES(aKeyMembers, sTableKeyExpression ,1,"+")
		FOR iCounter = 1 TO iKeyMembersCounter
			sGetKeyColumnValue = " vKeyValue = " + sTableName + "." + aKeyMembers(iCounter) 
			&sGetKeyColumnValue  
			IF EMPTY(vKeyValue)
				bKeyIsNotvalid = .T.
				IF UPPER(sTableName + "." + aKeyMembers(iCounter))  $  FIELDS_ADMITTED_AS_EMPTY_VALUES
					bKeyIsNotvalid = .F.
				ELSE
					EXIT
				ENDIF
			ENDIF
		NEXT	
		IF NOT bDataTableIsUsed 	
			USE
		ENDIF

		IF NOT bKeyIsNotvalid
			sFilterCondition = sTableKeyExpression + " = '" + &sTableKeyExpression + "'"
			sReturnedValue = "SELECT RECNO() AS nReg , * FROM " + (sTableName) + " WHERE " + ;
				(sFilterCondition) + (lsFilterCondition) + " INTO CURSOR  rsResult"		
		ENDIF
	ENDIF

	SELECT (iOldSelect)
	RETURN sReturnedValue
ENDFUNC


PROCEDURE processAdjustments
		
	LPARAMETERS sSourceTablename as String, sMatchingFields  as String,  bChangeAnyValueAdjustmentCare as Boolean, ;
				sFilter as String, sResultsRSName as string
	
	* Consolida en un solo registro cada uno de los casos que se encuentren en sSourceTablename y que cumplan la condición sFilter (opcional). ;
	Los casos consolidados quedarán en un cursor de nombre sResultsRSName o 'rsAdjustedCases', si es que no se pasa este parámeto opcional.
	
	* Se considera que un caso está conformado por uno o más registros que están vinculados por tener los campos de sMatchingFields idénticos. ;
	La consolidación de un caso procede entonces conformando un registro con la información consolidada de todos los ;
	registros que lo conforman. En el registro consolidado, el campo TIP_CAS contendrá la clasificación inicial del caso en tanto que ;
	la clasificación final del mismo quedará almacenada en el campo AJUSTE.
	
	*sMatchingFields  es un conjunto de nombres de campos de sSourceTablename separados por '+'; por ejemplo;
	FieldName1 + FieldName2 + ... + FielName_n

	*Si bChangeAnyValueAdjustmentCare es .T. en la consolidación de cada caso se procesará o tendrá en cuenta los ;
	registros de ajustes que implican cambios en cualquier variable distinta a TIP_CAS; en caso contrario, solamente se ;
	tendrán en cuenta registros que cambien la clasificación inicial del caso.
	

	STORE '' TO strTip_cas
	IF VARTYPE(sResultsRSName)!='C' THEN
		sResultsRSName = 'rsAdjustedCases'
	ENDIF
	
	nSelectedWorkArea=SELECT()
	bSourceTableIsOpen= USED(sSourceTablename) 
	=UseTable(sSourceTablename)
	SELECT (sSourceTablename)
	
	
	*Crea un índice en sSourceTablename de tal forma que la operación sea óptima
	sOldSafety = SET("Safety")
	SET SAFETY OFF
	TRY
		sIndexCmd = "INDEX ON " + sMatchingFields + " TAG Search1"
		&sIndexCmd 
	CATCH TO oException
	ENDTRY
	SET SAFETY &sOldSafety 

	* se crea el cursor que contendrá los datos definitivos
	sSQLCmd = "SELECT * FROM " + sSourceTablename + " WHERE .F. INTO CURSOR " +  sResultsRSName + " READWRITE"
	&sSQLCmd 

	* Crea un cursor con los registros candidatos a ser consolidados tomando como base los campos de sMatchingFields  
	sSelectSQLCmd = "SELECT DISTINCT " + STRTRAN(sMatchingFields, "+", ",") + " FROM " + sSourceTablename
	IF VARTYPE(sFilter)!='C' THEN
		sFilter = '.T.'
	ENDIF
	sSelectSQLCmd = sSelectSQLCmd + " WHERE " + sFilter + " INTO CURSOR rsCandidates"
	&sSelectSQLCmd 
	
	SELECT rsCandidates
	nCandidates = RECCOUNT()
	IF nCandidates > 0 THEN
		nCandidate=0
		SCAN
			nMatchingFields = ALINES(aMatchingFields, sMatchingFields, 1 + 4 + 8, "+")
			FOR iField = 1 TO nMatchingFields 
				sGetFiedlValue = "rsCandidates." + aMatchingFields(iField)
				sDefine_i_ValueFieldVar = "Value" + ALLTRIM(STR(iField)) + " = &sGetFiedlValue"
				&sDefine_i_ValueFieldVar 
			ENDFOR
			
			strTip_cas = ''
			bFormerCase=.F.
			bPseudoFormerCase=.F.

			SELECT (sSourceTablename )
			dAdjustmentDate = CTOD('')
			dLastSignificantAdjustmentDate = CTOD('')

			sScanCondition = "&sMatchingFields" + " = '"
			FOR iField = 1 TO nMatchingFields 
				iFieldValue = "Value" + ALLTRIM(STR(iField))
				sScanCondition = sScanCondition + &iFieldValue
			ENDFOR
			sScanCondition = sScanCondition + "'" + " AND (" + sFilter + ")"
			
			SCAN FOR &sScanCondition
				
				IF VAL(&sSourceTablename..AJUSTE) = 0 AND !bFormerCase THEN
					*Se trata del registro o reporte inicial del paciente
					bFormerCase=.T.
					SCATTER MEMVAR
				ENDIF
				
				IF 	VAL(&sSourceTablename..AJUSTE) > 6 THEN
					*Se trata de un registro que contiene datos modificados del registro inicial del caso excepto en el tipo de caso
					IF bChangeAnyValueAdjustmentCare THEN
						IF &sSourceTablename..FEC_AJU >= dAdjustmentDate THEN
							bFormerCase = .T.
							dAdjustmentDate = &sSourceTablename..FEC_AJU
							SCATTER MEMVAR
						ENDIF
					ENDIF
				ENDIF
				
				IF 	(VAL(&sSourceTablename..AJUSTE) < 7) AND (&sSourceTablename..AJUSTE <> '0' AND !EMPTY(&sSourceTablename..AJUSTE)) THEN
					*Se trata de un registro que contiene una modificación de la clasificación del caso originalmente registrada
					
					IF &sSourceTablename..FEC_AJU >= dLastSignificantAdjustmentDate THEN
						strTip_cas = &sSourceTablename..AJUSTE
						dLastSignificantAdjustmentDate = &sSourceTablename..FEC_AJU
					ENDIF
					
					IF !bFormerCase AND !bPseudoFormerCase THEN
						*Aún no se han capturado los datos de las variables asociadas al paciente, por tanto, se capturan pero el caso;
						no puede considerarse como el registro inicial del paciente
						bPseudoFormerCase=.T.
						SCATTER MEMVAR
					ENDIF	
				ENDIF
				nCandidate=nCandidate+1				
				WAIT "Registro " + ALLTRIM(STR(nCandidate)) + " de " +  ALLTRIM(STR(nCandidates)) WINDOW NOWAIT TIMEOUT 2
			ENDSCAN					
			
			* Ingresa el registro consolidado en la tabla de resultados
			IF !EMPTY(strTip_cas) THEN
				*Se ha detectado una modificación en la clasificación inicial del caso
				m.ajuste = strTip_cas
			ENDIF
			IF !EMPTY(dLastSignificantAdjustmentDate) THEN
				*Se ha detectado una fecha de ajuste que dió lugar a que se modificara la clasificación inicial del caso
				m.fec_aju = dLastSignificantAdjustmentDate
			ENDIF
			
			sSQLCmd = 'INSERT INTO ' + sResultsRSName + ' FROM MEMVAR'
			&sSQLCmd
			
			SELECT rsCandidates
		ENDSCAN
	ENDIF

	IF !bSourceTableIsOpen THEN
		SELECT (sSourceTablename)
		USE
	ENDIF
	SELECT (nSelectedWorkArea)

ENDPROC

PROCEDURE fastProcessAdjustments
		
	LPARAMETERS sSourceTablename as String, sMatchingFields  as String,  bChangeAnyValueAdjustmentCare as Boolean, ;
				sFilter as String, sResultsRSName as string, bDiscardRepeatedIndexRecords  AS Boolean, bDirectIndex as Boolean
	
	* Consolida en un solo registro cada uno de los casos que se encuentren en sSourceTablename y que cumplan la condición sFilter (opcional). ;
	Los casos consolidados quedarán en un cursor de nombre sResultsRSName o 'rsAdjustedCases', si es que no se pasa este parámeto opcional.
	
	* Se considera que un caso está conformado por uno o más registros que están vinculados por tener los campos de sMatchingFields idénticos. ;
	La consolidación de un caso procede entonces conformando un registro con la información consolidada de todos los ;
	registros que lo conforman. En el registro consolidado, el campo TIP_CAS contendrá la clasificación inicial del caso en tanto que ;
	la clasificación final del mismo quedará almacenada en el campo AJUSTE.
	
	*sMatchingFields  es un conjunto de nombres de campos de sSourceTablename separados por '+'; por ejemplo;
	FieldName1 + FieldName2 + ... + FielName_n

	*Si bChangeAnyValueAdjustmentCare es .T. en la consolidación de cada caso se procesará o tendrá en cuenta los ;
	registros de ajustes que implican cambios en cualquier variable distinta a TIP_CAS; en caso contrario, solamente se ;
	tendrán en cuenta registros que cambien la clasificación inicial del caso.
	
	STORE '' TO strTip_cas
	IF VARTYPE(sResultsRSName)!='C' THEN
		sResultsRSName = 'rsAdjustedCases'
	ENDIF
	
	nSelectedWorkArea=SELECT()
	bSourceTableIsOpen= USED(sSourceTablename) 
	=UseTable(sSourceTablename)
	SELECT (sSourceTablename)
	
	*Crea un índice en sSourceTablename de tal forma que la operación sea óptima
	IF !bDirectIndex THEN
		DO addIndexToTable WITH sSourceTablename, sMatchingFields, "Search1" IN SIVIGILAIndexesHandler
	ELSE
		sIndexCmd = "INDEX ON " + sMatchingFields + " TAG adjustProc"
		&sIndexCmd 
	ENDIF

	* se crea el cursor que contendrá los datos definitivos
	sSQLCmd = "SELECT * FROM " + sSourceTablename + " WHERE .F. INTO CURSOR " +  sResultsRSName  + " READWRITE"
	&sSQLCmd 

	* Crea dos cursores con los registros candidatos a ser consolidados tomando como base los campos de sMatchingFields:;
	Uno de los cursores contendrá los casos conformados por un único registro, el otro, los casos conformados por más de;
	un registro
	sSelectSQLCmd = "SELECT COUNT(*) as n," + STRTRAN(sMatchingFields, "+", ",") + " FROM " + sSourceTablename
	IF VARTYPE(sFilter)!='C' THEN
		sFilter = '.T.'
	ENDIF
	sSelectSQLCmd = sSelectSQLCmd + " WHERE " + sFilter + " GROUP BY " + STRTRAN(sMatchingFields, '+', ',') +;
					" HAVING n=1 INTO CURSOR rsUniqueCandidates"
	&sSelectSQLCmd 
	
	sSelectSQLCmd = "SELECT COUNT(*) as n," + STRTRAN(sMatchingFields, "+", ",") + " FROM " + sSourceTablename
	IF VARTYPE(sFilter)!='C' THEN
		sFilter = '.T.'
	ENDIF
	sSelectSQLCmd = sSelectSQLCmd + " WHERE " + sFilter + " GROUP BY " + STRTRAN(sMatchingFields, '+', ',') +;
					" HAVING n>1 INTO CURSOR rsCandidates"
	&sSelectSQLCmd 

	*Procesa secuencialmente los ajustes de casos conformados por más de un registro
	SELECT rsCandidates
	nCandidates = RECCOUNT()
	nCandidate=0
	SCAN
		nMatchingFields = ALINES(aMatchingFields, sMatchingFields, 1 + 4 + 8, "+")
		FOR iField = 1 TO nMatchingFields 
			sGetFiedlValue = "rsCandidates." + aMatchingFields(iField)
			sDefine_i_ValueFieldVar = "Value" + ALLTRIM(STR(iField)) + " = &sGetFiedlValue"
			&sDefine_i_ValueFieldVar 
		ENDFOR
		
		strTip_cas = ''
		bFormerCase=.F.
		bPseudoFormerCase=.F.
		nIndexCases = 0
		nCaseAdjustments = 0

		SELECT (sSourceTablename )
		dAdjustmentDate = CTOD('')
		dLastSignificantAdjustmentDate = CTOD('')

		sScanCondition = "&sMatchingFields" + " = '"
		FOR iField = 1 TO nMatchingFields 
			iFieldValue = "Value" + ALLTRIM(STR(iField))
			sScanCondition = sScanCondition + &iFieldValue
		ENDFOR
		sScanCondition = sScanCondition + "'" + " AND (" + sFilter + ")"
		
		SCAN FOR &sScanCondition
			
			IF (&sSourceTablename..AJUSTE = '0' OR EMPTY(&sSourceTablename..AJUSTE)) THEN
				nIndexCases = nIndexCases + 1
				IF !bFormerCase THEN
					*Se trata del registro o reporte inicial del paciente
					bFormerCase=.T.
					SCATTER MEMVAR
				ENDIF
			ENDIF
			
			IF 	VAL(&sSourceTablename..AJUSTE) > 6 THEN
				*Se trata de un registro que contiene datos modificados del registro inicial del caso excepto en el tipo de caso
				IF bChangeAnyValueAdjustmentCare THEN
					IF &sSourceTablename..FEC_AJU >= dAdjustmentDate THEN
						bFormerCase = .T.
						dAdjustmentDate = &sSourceTablename..FEC_AJU
						SCATTER MEMVAR
						nCaseAdjustments = nCaseAdjustments + 1
					ENDIF
				ENDIF
			ENDIF
			
			IF 	(VAL(&sSourceTablename..AJUSTE) < 7) AND (&sSourceTablename..AJUSTE <> '0' AND !EMPTY(&sSourceTablename..AJUSTE)) THEN
				*Se trata de un registro que contiene una modificación de la clasificación del caso originalmente registrada
				
				IF &sSourceTablename..FEC_AJU >= dLastSignificantAdjustmentDate THEN
					strTip_cas = &sSourceTablename..AJUSTE
					dLastSignificantAdjustmentDate = &sSourceTablename..FEC_AJU
					nCaseAdjustments = nCaseAdjustments + 1
				ENDIF
				
				IF !bFormerCase AND !bPseudoFormerCase THEN
					*Aún no se han capturado los datos de las variables asociadas al paciente, por tanto, se capturan pero el caso;
					no puede considerarse como el registro inicial del paciente
					bPseudoFormerCase=.T.
					SCATTER MEMVAR
				ENDIF	
			ENDIF
			nCandidate=nCandidate+1				
			WAIT "Registro " + ALLTRIM(STR(nCandidate)) + " de " +  ALLTRIM(STR(nCandidates)) WINDOW NOWAIT TIMEOUT 2
		ENDSCAN					
		
		* Ingresa el registro consolidado en la tabla de resultados
		IF !EMPTY(strTip_cas) THEN
			*Se ha detectado una modificación en la clasificación inicial del caso
			m.ajuste = strTip_cas
		ENDIF
		IF !EMPTY(dLastSignificantAdjustmentDate) THEN
			*Se ha detectado una fecha de ajuste que dió lugar a que se modificara la clasificación inicial del caso
			m.fec_aju = dLastSignificantAdjustmentDate
		ENDIF

		IF nIndexCases > 1 THEN
			IF !bDiscardRepeatedIndexRecords THEN
				sSQLCmd = 'INSERT INTO ' + sResultsRSName + ' SELECT * FROM ' + sSourceTablename + ' WHERE ' + sScanCondition
				&sSQLCmd
			ENDIF
		ELSE
			sSQLCmd = 'INSERT INTO ' + sResultsRSName + ' FROM MEMVAR'
			&sSQLCmd
		ENDIF
						
		SELECT rsCandidates
	ENDSCAN
	
	*Agrega al recordset resultante los casos conformados por un único registro
	sSetProcCmd = "SET PROCEDURE TO '" + PATH_TO_COMMON_LIB + "QueriesHandler' ADDITIVE"
	&sSetProcCmd 
	
	sSQLCmd = 'INSERT INTO ' + sResultsRSName + ' SELECT ' + sSourceTablename + '.* FROM ' + sSourceTablename +;
				' INNER JOIN rsUniqueCandidates ON ' +; 
				matchFields(sMatchingFields, sSourceTablename , sMatchingFields, "rsUniqueCandidates", "+")
	&sSQLCmd

	IF !bSourceTableIsOpen THEN
		SELECT (sSourceTablename)
		USE
	ENDIF
	SELECT (nSelectedWorkArea)

ENDPROC

FUNCTION getControlToGoTo

	LPARAMETERS aControlsList AS Variant, sResultMessage
	* Del array de controles aControlsList, retorna el nombre del primer control 
	* cuyo estado de activación sea verdadero. En caso de no haber ninguno, 
	* retornará una cadena vacía.  La cadena sResultmessage, pasada por referencia,
	* contiene el mensaje de error correspondiente.
	
	LOCAL sReturnedValue  as String, iCounter as Integer, iQuantityOfControls as Integer
	LOCAL ARRAY aControlCandidate(2)
	sReturnedValue = ''

	TRY
		iQuantityOfControls = ALEN(aControlsList,1)
		aControlCandidate(1) = iQuantityOfControls
		aControlCandidate(2) = ''

		FOR iCounter = 1 TO iQuantityOfControls 
			IF aControlsList(iCounter, 3) = .T.
				IF aControlsList(iCounter,6) < aControlCandidate(1)
					aControlCandidate(1) = aControlsList(iCounter,6)
					aControlCandidate(2) = aControlsList(iCounter,1)
				ENDIF
			ENDIF
		NEXT
		sReturnedValue = aControlCandidate(2)
		
	CATCH TO oException
		sResultMessage = "El objeto no es un array"
	ENDTRY
	RETURN sReturnedValue
ENDFUNC


FUNCTION fieldValue(sTargetFieldName AS String, sTargetTableName as String, sFilter as String, sOrderBy as String, ;
					nLength as Number, nDecimalPlaces as Number, nRecords as Number)
	
	*Retorna el valor almacenado en el(los) campo(s) sTargetFieldName del registro de la tabla sTargetTableName que cumpla la condición sFilter. ;
	Si hay más de un registro en sTargetTableName  que cumpla la condición, retorna el valor del primero encontrado.
	
	*sTargetFieldName puede ser una lista de campos separada por comas; en este caso, se retornará una cadena separada por comas con los valores ;
	de cada campo convertido a un string; si sTargetFieldName es un solo campo, retornará su valor sin conversión alguna
	
	*El parámetro opcional sOrderBy permite especificar un criterio de ordenación para los registros que cumplan la condición sFilter ;
	de tal forma que, si existen varios registros que la cumplan, el valor retornado sea el primero de acuerdo con ese orden.
	
	*El parámetro opcional nRecords contendrá el número de registros encontrados
	
	LOCAL vReturnedValue  AS VARIANT
	LOCAL nSelectedWorkArea as Integer, bSourceTableIsOpen as Boolean
		
	vReturnedValue = NULL
	
	nSelectedWorkArea=SELECT()
	bSourceTableIsOpen= USED(sTargetTableName) 

	IF FILE('&sTargetTableName..DBF') THEN
		lcsSelectSqlCmd = 'SELECT ' + sTargetFieldName + ' FROM ' + sTargetTableName
		IF !EMPTY(sFilter)
			lcsSelectSqlCmd  = lcsSelectSqlCmd + ' WHERE ' + sFilter
		ENDIF
		IF !EMPTY(sOrderBy )
			lcsSelectSqlCmd  = lcsSelectSqlCmd + ' ORDER BY ' + sOrderBy 
		ENDIF

		lcsSelectSqlCmd  = lcsSelectSqlCmd  + ' INTO ARRAY aResult'
		&lcsSelectSqlCmd
		nRecords = _TALLY
		IF _TALLY != 0
			IF ALEN(aResult,2) = 1 THEN
				vReturnedValue = aResult
			ELSE
				IF VARTYPE(nLength)!='N' THEN
					nLength = 10
				ENDIF
				IF VARTYPE(nDecimalPlaces)!='N' THEN
					nDecimalPlaces = 0
				ENDIF
				
				sValuesList = ''
				FOR nValue=1 TO ALEN(aResult,2)
					DO CASE 
						CASE VARTYPE(aResult(1,nValue))='C'
							sValuesList  = sValuesList  + aResult(1,nValue)
						CASE VARTYPE(aResult(1,nValue))='D'
							sValuesList  = sValuesList  + DTOC(aResult(1,nValue))
						CASE VARTYPE(aResult(1,nValue))='N'
							sValuesList  = sValuesList  + ALLTRIM(STR(aResult(1,nValue),nLength,nDecimalPlaces))
						CASE VARTYPE(aResult(1,nValue))='L'
							sValuesList  = sValuesList  + TRANSFORM(aResult(1,nValue),'Y')
					ENDCASE
					sValuesList  = sValuesList  + ","
				NEXT nValue
				sValuesList  = LEFT(sValuesList ,LEN(sValuesList )-1)
				vReturnedValue = sValuesList 
			ENDIF
		ENDIF
		
		IF !bSourceTableIsOpen THEN
			sTableNameToClose = JUSTFNAME(sTargetTableName)
			SELECT (sTableNameToClose)
			USE
		ENDIF
	ENDIF
	SELECT (nSelectedWorkArea)
	
	RETURN vReturnedValue
ENDFUNC


PROCEDURE fieldValueTst
	SET STEP ON 
	vValueToLookFor = '01239'
	?fieldValue("CODNUMPAIS","vListaTodosLosMunicipios",'COD_MUN="' + vValueToLookFor + '"')
	*?fieldValue('NOM_MUN','MUNICIPIOS','COD_MUN="' + vValueToLookFor + '"')
	*?fieldValue('TIP_UNI,UNI_NOT','GENERAL','.T.')
	*?fieldValue('RECNO() as nReg','BROTES','COD_EVE+AÑO+ SEMANA+ COD_MUN + COD_PRE+ COD_SUB="999 20126 27450274500028413"',"FEC_AJU DESC, FEC_NOT DESC, nReg DESC")
	*?fieldValue('semana','paciente','ajuste="0"','num_ide')
	*?fieldValue('fec_not','paciente','ajuste="0"','num_ide')
	*?fieldValue('ingr_x_ni','paciente','ajuste="0"','num_ide')
	*?fieldValue('semana,año,cod_pre,ingr_x_ni,fec_not','paciente','ajuste="0"','num_ide')
ENDPROC


FUNCTION extractSivigilaVersion as String 
	
	LPARAMETERS sSIVIGILAInfo as String
	
	*Extrae de sSIVIGILAInfo la información de versión del aplicativo SIVIGILA Y la retorna en un string. Se supone que la información ;
	de versión se extrae de una línea en sSIVIGILAInfo que contien la expresión "SIVIGILA" y, posiblemente, una que contiene la ;
	expresión 'VERSIÓN'. En saso de que no se pueda extraer la información de versión específica, retorna una cadena vacía
	
	LOCAL sReturnedValue as String, nSIVIGILAInfo  AS Number, nLines  AS Number 
	
	DIMENSION aSIVIGILAInfo(1)
	
	sReturnedValue  = ""

	*Convierte sSIVIGILAInfo  en un array
	nLines = ALINES(aSIVIGILAInfo,sSIVIGILAInfo ,1+4)
	
	*Captura la información correspondiente a nombre de la aplicación
	nSIVIGILAInfo = ASCAN(aSIVIGILAInfo,'SIVIGILA',-1,-1,-1,13)
	sReturnedValue = sReturnedValue  + aSIVIGILAInfo(nSIVIGILAInfo)
	
	*Captura la información correspondiente a versión específica
	nSIVIGILAInfo = ASCAN(aSIVIGILAInfo,'VERSIÓN',-1,-1,-1,13)
	IF nSIVIGILAInfo > 0 THEN
		sReturnedValue = sReturnedValue  + " " + aSIVIGILAInfo(nSIVIGILAInfo)
	ENDIF
	
	RETURN sReturnedValue 
	
	
ENDPROC

FUNCTION enableFormControls
	
	LPARAMETERS oContainer as object, sControlsList as String
	* Activa los controles del contenedor oContainer cuyos nombres aparezcan en la lista separada por comas sControlsList 
	
	bReturnedvalue = .F.
	iCounter = 0
	
	IF ALINES(aControlsList,sControlsList,1,",") > 0
		FOR iCounter = 1 TO ALEN(aControlsList)
			TRY
				sDisableControl = "oContainer" + "." + aControlsList(iCounter) + ".Enabled = .T."
				&sDisableControl
			CATCH TO oException
			ENDTRY
			
		NEXT
		bReturnedValue = .T.
	ENDIF
	
	RETURN bReturnedValue
ENDFUNC

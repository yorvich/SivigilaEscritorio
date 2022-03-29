#INCLUDE SIVIGILA.h

DEFINE CLASS AdjustmentsProcessor as Custom 

sSourceTablename = ''
*Nombre de la tabla fuente que contiene los registros sobre los que se realizará el procesamiento de ajustes

sAdjustmentFieldName = 'AJUSTE'
*Nombre del campo en sSourceTablename que contiene el dato de ajuste 

sAdjustmentDateFieldName = 'FEC_AJU'
*Nombre del campo en sSourceTablename que contiene la fecha de realización o grabación del ajuste 


sSocialSecurityTypeFieldName = 'TIP_SS'
*Nombre del campo en sSourceTablename que contiene el tip de seguridad social

sSocialSecurityEntityFieldName = 'COD_ASE'
*Nombre del campo en sSourceTablename que contiene el nombre de la EAPB que cubre la seguridad social

sNotifierUnitIdFieldName = 'UNI_MODIF'
sNotifierUnitNameFieldName = 'NUNI_MODIF'
*Nombre de los campos en sSourceTablename que contienen la identificación de la unidad notificadora que realiza ;
el ajuste (Id de la unidad, Nombre de la unidad)


HIDDEN sOldPath 


FUNCTION Init

	This.sOldPath = SET("Path")
		
	sSetPathCmd = "SET PATH TO '" + PATH_TO_DEVELOPMENT_ENVIRONMENT + "' ADDITIVE"
	&sSetPathCmd
	
	sSetPathCmd = "SET PATH TO '" + PATH_TO_COMMON_LIB + "' ADDITIVE"
	&sSetPathCmd

ENDFUNC


FUNCTION Destroy

	sSetPathCmd = "SET PATH TO '" + This.sOldPath + "'"
	&sSetPathCmd

ENDFUNC


PROCEDURE processAdjustments(sMatchingFields  as String,  sFilter  as String, sOrder  as String, bOnTable as Boolean, bTrace  AS Boolean, ;
							sTraceDir as String, bUseAuxData as Boolean, sResultsRSName as string, sPostfix as String)
	
	* Consolida en un solo registro cada uno de los casos de datos complementarios que se encuentren en This.sSourceTablename ;
	y que satisfagan la condición sFilter, si es que se pasa este parámetro opcional. ;
	Los casos consolidados quedarán en un cursor de nombre sResultsRSName que contendrá un registro consolidado para cada;
	caso en This.sSourceTablename que tenga un número de registros >= 1. Los casos en This.sSourceTablename que ya tengan un único registro;
	se consideran no procesables por cuanto no hay registros de ajuste a aplicar y, por tanto, se incluyen directamente y sin ;
	procesamiento alguno en sResultsRSName. Si no se pasa el parámetro sResultsRSName, el nombre por defecto del cursor ;
	de casos consolidados será rsAdjustedCases.
	
	* Se considera que un caso está conformado por uno o más registros que están vinculados por tener los campos de sMatchingFields idénticos. ;
	La consolidación de un caso procede entonces conformando un registro con la información consolidada de todos los ;
	registros que lo conforman. El procesamiento de los registros de un caso es secuencial y se hará de acuerdo al orden determinado ;
	por sOrder, si es que se pasa este parámetro opcional; en caso contrario, el procesamiento será secuencial de acuerdo con el;
	orden de entrada de los registros en la tabla This.sSourceTablename pero tendrá en cuenta los valores de ajuste y fechas de realización ;
	de los ajustes a la hora de consolidar un único registro para el caso. 
	
	*sMatchingFields  es un conjunto de nombres de campos de This.sSourceTablename separados por '+'; por ejemplo;
	FieldName1 + FieldName2 + ... + FielName_n

	*Si bOnTable es .T., en la consolidación de cada caso se eliminará lógicamente de This.sSourceTablename los registros ;
	de ajuste de tal manera que en This.sSourceTablename solamente quedará el registro que consolida toda la información del caso.

	*Si bTrace es .T., se mantiene rastro de los casos que efectivamente fueron ajustados produciendo un archivo XLS de nombre;
	This.sSourceTablename en la carpeta de Backup sTraceDir en donde se indentifican los registros y cuantía de los mismos que conformaron cada ;
	uno de los casos que se establecieron para hacer el procesamiento de ajustes. En caso contrario, no se deja rastro de la operación.
	
	*Si bUseAuxData es .F. el procesamiento de ajustes procederá en forma secuencial teniendo en cuenta los campos de ajuste y fecha de realización del ajuste ;
	 -según se definan en This.sAdjustmentFieldName y This.sAdjustmentDateFieldName- al momento de consolidar un único registro; sin embargo, ;
	 si bUseAuxData es .T., en la consolidación solamente se tendrá en cuenta el orden establecido por sOrder
	 
	*sPostfix: (opcional, default='_') utilizado solamente cuando al procesar ajustes de un caso, se encuentren registros que han sido actualizados ;
	por el sistema Sivigila en uno o más de sus campos; en este caso, el sistema habrá dejado una huella de su acción en el campo ;
	NOM_DIL_F_ y el procesamiento de ajustes asignará al caso final consolidado, los valores de los campos que el Sistema haya modificado;
	directamente independientemente que en algunos registros del caso, se encuentre que a esos campos un usuario les ha asignado valores diferentes.
	
	
	LOCAL sSourceTablename as String
	*Almacena el valor actual de This.sSourceTablename
	
	DIMENSION aUnmovableFields(1)
	DIMENSION aUnmovableAdjustmentFields(1)
	
	LOCAL nUnmovableFields as Byte 
	LOCAL bUnmovableFieldsFound as Boolean, bUnmovableAdjustmentFieldsFound as Boolean 
	LOCAL sUnmovableFields as String 
		
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\QueriesHandler') ADDITIVE
	SET PROCEDURE TO SivigilaSystem ADDITIVE 

	sOldDeleted = SET("Deleted")
	SET DELETED ON

	IF VARTYPE(sPostfix)!='C' THEN 
		sPostfix = '_'
	ENDIF 
	
	sSourceTablename = This.sSourceTablename

	IF VARTYPE(sResultsRSName)='C' AND !EMPTY(sResultsRSName) THEN
	ELSE
		sResultsRSName = 'rsAdjustedCases'
	ENDIF
	
	
	nSelectedWorkArea=SELECT()
	bSourceTableIsOpen= USED(JUSTFNAME(sSourceTablename)) 
	=UseTable(sSourceTablename)
	SELECT JUSTFNAME((sSourceTablename))

	*Establece los registros objetivo en función de sFilter	
	sSourceTablename = JUSTFNAME(sSourceTablename)
	sFormerSourceTableName = sSourceTablename
	sSelectSQLCmd = "SELECT *,RECNO() AS nReg FROM " + sSourceTablename
	IF VARTYPE(sFilter)='C' THEN
		sSelectSQLCmd = sSelectSQLCmd + " WHERE " + sFilter
	ENDIF
	sSelectSQLCmd = sSelectSQLCmd +	" INTO CURSOR rsTargetRecords ORDER BY " + STRTRAN(sMatchingFields,'+',',') + " READWRITE"
	&sSelectSQLCmd 
	sSourceTablename  = "rsTargetRecords"
	SELECT (sSourceTablename)
	*Crea un índice en sSourceTablename de tal forma que la operación obedezca el orden sOrder
	IF bUseAuxData THEN
		IF VARTYPE(sOrder)!='C' THEN
			sOrder=''
		ENDIF
		sOldSafety = SET("Safety")
		SET SAFETY OFF
		IF !EMPTY(sOrder) THEN
			sIndexCmd = "INDEX ON " + sMatchingFields + "+" + sOrder + " TAG adjustProc"
		ELSE
			sIndexCmd = "INDEX ON " + sMatchingFields + " TAG adjustProc"
		ENDIF
		&sIndexCmd 
		SET SAFETY &sOldSafety
	ELSE
		sIndexCmd = "INDEX ON " + sMatchingFields + " TAG adjustProc"
		&sIndexCmd 
	ENDIF

	* se crea el cursor que contendrá los datos definitivos
	SELECT * FROM (sSourceTablename) WHERE .F. INTO CURSOR &sResultsRSName READWRITE
*SET STEP ON 	
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
	
	sSelectSQLCmd = "SELECT COUNT(*) as n, " + sMatchingFields + " as Clave," + STRTRAN(sMatchingFields, "+", ",") + " FROM " + sSourceTablename
	IF VARTYPE(sFilter)!='C' THEN
		sFilter = '.T.'
	ENDIF
	sSelectSQLCmd = sSelectSQLCmd + " WHERE " + sFilter + " GROUP BY " + STRTRAN(sMatchingFields, '+', ',') +;
					" HAVING n>1 INTO CURSOR rsCandidates"
	&sSelectSQLCmd 

	*Procesa secuencialmente los ajustes de casos conformados por más de un registro
	SELECT rsCandidates
	nCasesToAdjust = _TALLY
	IF nCasesToAdjust>0 THEN
		IF isField('rsCandidates','Cod_Eve_') THEN 
			oEvent = NEWOBJECT("Event","Event.fxp",.NULL.,rsCandidates.Cod_Eve_)
		ELSE
			oEvent = NEWOBJECT("Event","Event.fxp",.NULL.,rsCandidates.Cod_Eve)
		ENDIF 
		
		*Establece si el recordset de datos a justar contiene los campos sNotifierUnitIdFieldName y sNotifierUnitNameFieldName
		bExistsNotifierUnitIdField =  isField(sSourceTablename,This.sNotifierUnitIdFieldName)
		bExistsNotifierUnitNameField = isField(sSourceTablename,This.sNotifierUnitNameFieldName)

		oBD = NEWOBJECT("BasicData","cls_BasicData.fxp",.NULL.,'')
				
		IF bTrace THEN
			oDataExporter=NEWOBJECT("DataExporter","DataExporter.Prg",.null.)
			oDataExporter.sExportationPath = sTraceDir 
			oDataExporter.sSourceTableName = "rsCandidates"
			oDataExporter.sExportedFileName = sFormerSourceTableName 
			oDataExporter.exportToXLS()
		ENDIF

		SELECT rsCandidates
		nCandidates = RECCOUNT()
		nCandidate=0
		sAdjustmentFieldName = This.sAdjustmentFieldName 
		sCDAdjustmentFieldName = 'AJUSTE_AUX'
		sAdjustmentDateFieldName = This.sAdjustmentDateFieldName
		sNotifierUnitIdFieldName = This.sNotifierUnitIdFieldName
		sNotifierUnitNameFieldName = This.sNotifierUnitNameFieldName 
		
		sSocialSecurityTypeFieldName = This.sSocialSecurityTypeFieldName + sPostfix
		sSocialSecurityEntityFieldName = This.sSocialSecurityEntityFieldName + sPostfix

		SCAN
			IF isField('rsCandidates','Cod_Eve_') THEN 
				oEvent.Refresh(rsCandidates.Cod_Eve_)
			ELSE
				oEvent.Refresh(rsCandidates.Cod_Eve)
			ENDIF 

			nRecordsByCase = rsCandidates.n
			sCurrentCaseClasification = ''
			sNotifierUnitId = ''
			sNotifierUnitName = ''
			sSocialSecurityType = .NULL.
			sCurrentEAPB = .NULL.
			bFormerCase = .F.
			bPseudoFormerCase = .F.
			bNotifierUnitFixed = .F.
			bUnmovableFieldsFound = .F.
			bUnmovableAdjustmentFieldsFound = .F.
			sUnmovableFields = ''
			nUnmovableFields = 0
			
			SELECT (sSourceTablename )
			dAdjustmentDate = CTOD('')
			dLastSignificantAdjustmentDate = CTOD('')
			
			DIMENSION aUnmovableAdjustmentFields(1)
*SET STEP ON 
			SEEK rsCandidates.Clave
			FOR iRecord=1 TO nRecordsByCase 

				IF bExistsNotifierUnitIdField THEN 
					IF IsEAPB(&sSourceTablename..&sNotifierUnitIdFieldName) ;
						OR &sSourceTablename..&sSocialSecurityTypeFieldName = UNDEFINED_SOCIAL_SECURITY THEN 
						*Se ha encontrado un registro que ha sido modificado por una EAPB y por tanto, los valores;
						de los campos modificados por ella -TIP_SS Y COD_ASE-, deben conservarse en el caso consolidado final a no ser que, posteriormente, ;
						se encuentre alguna modificación derivada del sistema BDUA
						
						sSocialSecurityType = &sSourceTablename..&sSocialSecurityTypeFieldName
						sCurrentEAPB = &sSourceTablename..&sSocialSecurityEntityFieldName 
					ENDIF 
				ENDIF 
				
				IF isField(sSourceTablename,'NOM_DIL_F_') THEN 
					IF !bUnmovableFieldsFound AND (SIVIGILA_MARK $ &sSourceTablename..NOM_DIL_F_) THEN 
						*Se ha encontrado un registro que ha sido modificado directamente por el sistema Sivigila y, por tanto, los valores;
						de los campos modificados por él, deben conservarse en el caso consolidado final
						bUnmovableFieldsFound = .T.
						
						*Anula modificaciones a alguno de los campos TIP_SS o COD_ASE hechos por una EAPB por cuanto las modificaciones del ;
						sistema tiene precedencia
						IF !ISNULL(sSocialSecurityType) THEN 
							sSocialSecurityType = .NULL.
						ENDIF 
						IF !ISNULL(sCurrentEAPB) THEN 
							sCurrentEAPB = .NULL.
						ENDIF 
						
						sUnmovableFields = SUBSTR(&sSourceTablename..NOM_DIL_F_,AT('[',&sSourceTablename..NOM_DIL_F_)+1, ;
													AT(']',&sSourceTablename..NOM_DIL_F_)-AT('[',&sSourceTablename..NOM_DIL_F_)-1)
						nUnmovableFields = ALINES(aUnmovableFields, oBD.unCodify(sUnmovableFields), 15, '|')
						IF nUnmovableFields != 0 THEN 
							DIMENSION aUnmovableFields(3,nUnmovableFields)
							
							*Captura los valores de los campos modificados directamente por el sistema
							FOR iUnmovableField=1 TO nUnmovableFields 
								sUnmovableFieldName = aUnmovableFields[1,iUnmovableField]
								
								IF !EMPTY(sPostfix) THEN 
									*Los campos modificados por el sistema utilizan el sufijo sPostfix
									IF LEN(sUnmovableFieldName) <= 9 THEN 
										*El campo modificado tiene nombre corto
										sUnmovableFieldName = sUnmovableFieldName + sPostfix
									ELSE 
										*El campo modificado tiene nombre largo, por tanto, el nombre real del campo usa los primeros 9 caracteres del;
										nombre original más el sufijo sPostfix
										sUnmovableFieldName = SUBSTR(sUnmovableFieldName,1,9) + sPostfix
									ENDIF 
								ENDIF 
								
								aUnmovableFields[2,iUnmovableField] = &sSourceTablename..&sUnmovableFieldName
								aUnmovableFields[3,iUnmovableField] = sUnmovableFieldName 
							NEXT iUnmovableField
						ENDIF
					ENDIF
				ENDIF
				
				IF !bUseAuxData THEN
					
					IF VAL(&sSourceTablename..&sAdjustmentFieldName) = 0 AND &sSourceTablename..&sAdjustmentFieldName!=CAPTURE_DISCARD_ADJUSTMENT ;
						AND !bFormerCase THEN
						*Se trata del registro o reporte inicial del paciente
						bFormerCase=.T.
						SCATTER MEMO MEMVAR
					ENDIF
					
					IF 	VAL(&sSourceTablename..&sAdjustmentFieldName) > VAL(CLINICAL_DISCARD_ADJUSTMENT) THEN
						*Se trata de un registro que contiene datos modificados del registro inicial del caso excepto en el tipo de caso
						IF &sSourceTablename..&sAdjustmentDateFieldName >= dAdjustmentDate THEN
							bFormerCase = .T.
							dAdjustmentDate = &sSourceTablename..&sAdjustmentDateFieldName
							IF bExistsNotifierUnitIdField AND !bNotifierUnitFixed THEN 
								sNotifierUnitId = &sSourceTablename..&sNotifierUnitIdFieldName
							ENDIF 
							IF bExistsNotifierUnitNameField AND !bNotifierUnitFixed THEN 
								sNotifierUnitName = &sSourceTablename..&sNotifierUnitNameFieldName
							ENDIF 
							SCATTER MEMO MEMVAR
						ENDIF
					ENDIF
					
					IF 	(VAL(&sSourceTablename..&sAdjustmentFieldName) < VAL(CHANGE_ANY_VALUE_ADJUSTMENT)) AND (&sSourceTablename..&sAdjustmentFieldName <> '0' ;
						AND !EMPTY(&sSourceTablename..&sAdjustmentFieldName)) THEN
						*Se trata de un registro que contiene una modificación de la clasificación del caso originalmente registrada
						
						IF &sSourceTablename..&sAdjustmentDateFieldName >= dLastSignificantAdjustmentDate THEN
							sCurrentCaseClasification = &sSourceTablename..&sAdjustmentFieldName
							IF bExistsNotifierUnitIdField THEN 
								sNotifierUnitId = &sSourceTablename..&sNotifierUnitIdFieldName
								bNotifierUnitFixed = .T.
							ENDIF 
							IF bExistsNotifierUnitNameField THEN 
								sNotifierUnitName = &sSourceTablename..&sNotifierUnitNameFieldName
							ENDIF 
							dLastSignificantAdjustmentDate = &sSourceTablename..&sAdjustmentDateFieldName

							* Establece  si deben retenerse algunos campos diferentes a la clasificación incial del caso que hayan sido modificados ;
							debido al ajuste&sSourceTablename..&sAdjustmentFieldName
							IF oEvent.bIsDefined THEN 
							
								IF isField(sSourceTablename,sCDAdjustmentFieldName) AND ;
									&sSourceTablename..&sAdjustmentFieldName = &sSourceTablename..&sCDAdjustmentFieldName THEN 
									*La comparación de ajustes garantiza que no se tengan mezclas indebidas; es decir, que el ajuste en DB coincida con;
									el de DC; cuando ello sea así, se tomarán los valores de los campos que se deben retener
												
									oDependencies = oEvent.GetRetainigFields(&sSourceTablename..&sAdjustmentFieldName)
									IF !ISNULL(oDependencies ) THEN 
										
										*Captura los valores de los campos modificados como consecuencia del ajuste y que deben retenerse en el registro;
										 consolidado final siempre y cuando sean no vacíos
										FOR iUnmovableField=1  TO oDependencies.nSize 
					
											sUnmovableFieldName = oDependencies.get(iUnmovableField)
											IF isField(sSourceTablename,sUnmovableFieldName) THEN 
											
												IF !EMPTY(&sSourceTablename..&sUnmovableFieldName) THEN 
													*Agrega dos posiciones al arreglo que mantiene el rastro de los campos y valores a retener en el registro final
													nCurrentArraySize = ALEN(aUnmovableAdjustmentFields)
													IF nCurrentArraySize = 1 THEN 
														DIMENSION aUnmovableAdjustmentFields(2)
														nInsertPos = 1
													ELSE
														DIMENSION aUnmovableAdjustmentFields(nCurrentArraySize + 2)
														nInsertPos = nCurrentArraySize + 1
													ENDIF 
													
													*Captura valores y nombre de campos a retener en el registro final debidos al ajuste
													aUnmovableAdjustmentFields[nInsertPos] = &sSourceTablename..&sUnmovableFieldName
													aUnmovableAdjustmentFields[nInsertPos + 1] = sUnmovableFieldName 
													
													bUnmovableAdjustmentFieldsFound = .T.
												ENDIF 
											ENDIF 
										NEXT iUnmovableField
									ENDIF 
								ENDIF 
							ENDIF &&oEvent.bIsDefined 

						ENDIF
						
						IF !bFormerCase AND !bPseudoFormerCase THEN
							*Aún no se han capturado los datos de las variables asociadas al paciente, por tanto, se capturan pero el caso;
							no puede considerarse como el registro inicial del paciente
							bPseudoFormerCase=.T.
							SCATTER MEMO MEMVAR
						ENDIF	
					ENDIF
				ELSE
					IF !bFormerCase THEN
						*El primer registro de un caso se considera como el reporte inicial y a ese registro se le harán los ajustes que;
						se encuentren en registros posteriores del mismo caso
						bFormerCase=.T.
						SCATTER MEMO MEMVAR
						bGetNextRecord = .F.
					ELSE
						bGetNextRecord = .T.
					ENDIF
					
					IF 	bGetNextRecord THEN
						*Se trata de un registro que contiene datos modificados del registro inicial del caso
						bFormerCase = .T.
						SCATTER MEMO MEMVAR
					ENDIF
				
				ENDIF
									
				nCandidate=nCandidate+1				
				WAIT "Registro " + ALLTRIM(STR(nCandidate)) + " de " +  ALLTRIM(STR(nCandidates)) + " casos" WINDOW NOWAIT TIMEOUT 2
				SKIP 1
			NEXT
			
			* Ingresa el registro consolidado en la tabla de casos ajustados
			IF !bUseAuxData THEN
				IF !EMPTY(sCurrentCaseClasification) THEN
					*Se ha detectado una modificación en la clasificación inicial del caso
					sAssignmentInstruction = 'm.' + This.sAdjustmentFieldName + ' = sCurrentCaseClasification'
					&sAssignmentInstruction
					
				ENDIF
				IF !EMPTY(dLastSignificantAdjustmentDate) THEN
					*Se ha detectado una fecha de ajuste que dió lugar a que se modificara la clasificación inicial del caso
					sAssignmentInstruction = 'm.' + This.sAdjustmentDateFieldName + ' = dLastSignificantAdjustmentDate'
					&sAssignmentInstruction 
				ENDIF

				IF !ISNULL(sSocialSecurityType) THEN
					*Se ha detectado  un valor de tipo de seguridad social que debe retenerse
					sAssignmentInstruction = 'm.' + sSocialSecurityTypeFieldName + ' = sSocialSecurityType'
					&sAssignmentInstruction 
				ENDIF
				
				IF !ISNULL(sCurrentEAPB) THEN
					*Se ha detectado  un valor de Entidad aseguradora que debe retenerse
					sAssignmentInstruction = 'm.' + sSocialSecurityEntityFieldName + ' = sCurrentEAPB'
					&sAssignmentInstruction 
				ENDIF

				IF bUnmovableFieldsFound THEN 
					*Se han detectado campos modificados por el sistema Sivigila que deben mantener el valor asignado por él
					FOR iUnmovableField=1 TO nUnmovableFields
						*sAssignmentInstruction = 'm.' + aUnmovableFields[3,iUnmovableField] + ' = "' + aUnmovableFields[2,iUnmovableField] + '"'
						IF VARTYPE(aUnmovableFields[2,iUnmovableField]) = 'D' THEN 
							sAssignmentInstruction = 'm.' + aUnmovableFields[3,iUnmovableField] + ' = CTOD("' + DTOC(aUnmovableFields[2,iUnmovableField]) + '")'
						ELSE
							sAssignmentInstruction = 'm.' + aUnmovableFields[3,iUnmovableField] + ' = "' + aUnmovableFields[2,iUnmovableField] + '"'
						ENDIF
						
						&sAssignmentInstruction
					NEXT iUnmovableField

					IF !(SIVIGILA_MARK $ m.NOM_DIL_F_) THEN 
						*Garantiza que el nombre de quien diligencia la ficha contenga el string que identifica actualizaciones realizadas por el sistema
						m.NOM_DIL_F_ = ALLTRIM(m.NOM_DIL_F_) + ' ' + SIVIGILA_MARK + '[' + sUnmovableFields + ']'
					ENDIF 
					
				ENDIF 

				IF bUnmovableAdjustmentFieldsFound THEN 

					* Asigna al registro final consolidado, los valores de campos modificados vía ajustes confirmatorios o de descarte
					FOR iUnmovableField=1 TO ALEN(aUnmovableAdjustmentFields) STEP 2
					
						IF VARTYPE(aUnmovableAdjustmentFields[iUnmovableField]) != 'L' THEN
							*Existe efectivamente un valor a retener
							
							sAssignmentInstruction = 'm.' + aUnmovableAdjustmentFields[iUnmovableField + 1]
							vValToAssign = aUnmovableAdjustmentFields[iUnmovableField]
							IF NOT ISNULL(vValToAssign) THEN 
								DO CASE 
									CASE VARTYPE(vValToAssign ) = 'C'
										sAssignmentInstruction = sAssignmentInstruction + ' = "' + vValToAssign  + '"'
									CASE VARTYPE(vValToAssign ) = 'D'
										sAssignmentInstruction = sAssignmentInstruction + ' = CTOD("' + DTOC(vValToAssign )  + '")'
									CASE VARTYPE(vValToAssign ) = 'N'
										sAssignmentInstruction = sAssignmentInstruction + ' = ' + ALLTRIM(STR(vValToAssign))  
								ENDCASE 
								&sAssignmentInstruction
							ENDIF 
						ENDIF
					NEXT iUnmovableField
					
					RELEASE aUnmovableAdjustmentFields
				ENDIF
				
				*Establece cuál fue finalmente la unidad que ajustó el caso
				sAssignmentInstruction = 'm.' + This.sNotifierUnitIdFieldName + ' = sNotifierUnitId'
				&sAssignmentInstruction
				sAssignmentInstruction = 'm.' + This.sNotifierUnitNameFieldName + ' = sNotifierUnitName'
				&sAssignmentInstruction
			ENDIF
			
			INSERT INTO &sResultsRSName FROM MEMVAR
			
			SELECT rsCandidates
		ENDSCAN


		IF bOnTable THEN
			sSQLCmd = "SELECT A.* FROM(SELECT rsTargetRecords.nReg, .T. AS BORRAR FROM rsTargetRecords INNER JOIN rsCandidates ON " +;
						matchFields(STRTRAN(sMatchingFields, "+", ","), "rsTargetRecords", STRTRAN(sMatchingFields, "+", ","), "rsCandidates ", ",") +;
						") AS A LEFT OUTER JOIN " + sResultsRSName + " ON A.nReg = " + sResultsRSName + ".nReg WHERE ISNULL(" + sResultsRSName + ".nReg) " +;
						" INTO CURSOR rsRecordsToDelete READWRITE"
			&sSQLCmd	
			SELECT rsRecordsToDelete 
			INDEX ON nReg TAG nRegIDX

			SELECT (sFormerSourceTablename)
			SET RELATION TO RECNO() INTO rsRecordsToDelete 
			DELETE ALL FOR rsRecordsToDelete.BORRAR
			SET RELATION TO
		ENDIF
		RELEASE oBD
		RELEASE oEvent
	ENDIF

	*Agrega al recordset resultante los casos conformados por un único registro
	sSetProcCmd = "SET PROCEDURE TO '" + PATH_TO_COMMON_LIB + "QueriesHandler' ADDITIVE"
	&sSetProcCmd 
	
	sSQLCmd = 'INSERT INTO ' + sResultsRSName + ' SELECT ' + sSourceTablename + '.* FROM ' + sSourceTablename +;
				' INNER JOIN rsUniqueCandidates ON ' +; 
				matchFields(sMatchingFields, sSourceTablename , sMatchingFields, "rsUniqueCandidates", "+")
	&sSQLCmd

	IF !bSourceTableIsOpen THEN
		SELECT (sFormerSourceTablename)
		USE
	ENDIF
	SELECT (nSelectedWorkArea)

	RELEASE oDataExporter
	*DO CloseTables WITH "rsTargetRecords" IN PATH_TO_DEVELOPMENT_ENVIRONMENT + "\SIVIGILAUtilities.Prg"

	SET DELETED &sOldDeleted
ENDPROC

ENDDEFINE &&AdjustmentsProcessor

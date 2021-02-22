#INCLUDE SIVIGILA.h

#DEFINE MIN_ID_NUMBER_LENGTH 6
*Longitud mínima para un número de documento de identidad


PROCEDURE simpleDependencyRule
	LPARAMETERS oContainer as Object, sFatherControlValue as Variant, sSonControlName as string, ;
				vEnableValuesList as String, aControlsState, vDefaultValue
	
	* Establece el valor de la propiedad Enabled de los controles sSonControlName en función del valor ;
	actual sFatherControlValue de un control del cual cada uno de ellos es dependiente; 
	en consecuencia permite ejecutar reglas de negocio de la forma: ;
		Si Control_x.Value in {vEnableValuesList} entonces ;
			Control_y.Enabled=.T. ;
		si no ;
			Control_y.Enabled=.F. ;
			Control_y.Value=vDefaultValue;
	con y = 1,...,n y n = el número de controles nombrados en la lista separada por comas sSonControlName;
	en este caso, el control_y es hijo o depende del Control_x y vEnableValuesList es la lista de valores;
	separada por comas que habilitan el o los controles Control_y de sSonControlName. vDefaultValue es un ;
	parámetro opcional; si no se pasa, se asume vDefaultValue=Vacío, esto es, al valor del control ;
	dependiente se le asignará vacío
	
	*aControlsState es un arreglo de los controles contenidos en oContainer. La estructura del arreglo ;
	debe ser: ;
		aControlsState(.,1)= Nombre del control ;
		aControlsState(.,2)= ... ;
		aControlsState(.,3)= ... ;
		aControlsState(.,4)= índice del control dentro del contenedor

		
	LOCAL nFatherControlIndex, nSonControlIndex, sSonControlSourceType

	DIMENSION aSonControls(1,1)	
	nSonControls = ALINES(aSonControls,sSonControlName,1,',')
	DIMENSION aSonControls(nSonControls,1)

	FOR nSonControl=1 TO nSonControls 
		nSonControlRow=ASCAN(aControlsState,aSonControls(nSonControl,1),-1,-1,1,15) 
		nSonControlIndex=aControlsState(nSonControlRow,4)
		
		IF oContainer.Controls(nSonControlIndex).BaseClass = 'Container' THEN
			sValueToValidate = sFatherControlValue 
			IF VARTYPE(sFatherControlValue)='N' THEN
				sValueToValidate =ALLTRIM(STR(sFatherControlValue ))
			ENDIF
			IF ',' + sValueToValidate + ',' $ ',' + vEnableValuesList + ',' THEN
				oContainer.Controls(nSonControlIndex).enablectl(.T.)
			ELSE
				oContainer.Controls(nSonControlIndex).enablectl(.F.)
			ENDIF
			
			*Establece el valor de los controles que se encuentren dentro del contenedor. Para ello, hace una llamada recursiva a este PROCEDURE, ;
			pasando como parámetro oContainer el propio contenedor que se acaba de encontrar
				* sSubControlsList =''
				* nSubCtlTt = oContainer.Controls(nSonControlIndex).ControlCount
				* FOR nSubCtrInd=1 TO nSubCtlTt 
				* 	sSubControlsList = sSubControlsList + oContainer.Controls(nSonControlIndex).Controls(nSubCtrInd).Name + ','
				* NEXT nSubCtrInd
				* sSubControlsList = LEFT(sSubControlsList,LEN(sSubControlsList)-1)
				
				* DIMENSION aSubcontrolsControlsState(1,1)
				* DO getFormControlsState WITH oContainer.Controls(nSonControlIndex), aSubcontrolsControlsState, , .T. IN SivigilaUtilities
				* DO simpleDependencyRule WITH oContainer.Controls(nSonControlIndex), sFatherControlValue , sSubControlsList, vEnableValuesList ,aSubcontrolsControlsState, vDefaultValue

		ELSE
			
			sValueToValidate = sFatherControlValue 
			IF VARTYPE(sFatherControlValue)='N' THEN
				sValueToValidate =ALLTRIM(STR(sFatherControlValue ))
			ENDIF
			IF ',' + sValueToValidate + ',' $ ',' + vEnableValuesList + ',' THEN
				oContainer.Controls(nSonControlIndex).Enabled=.T.
				oContainer.Controls(nSonControlIndex).Visible=.T.
			ELSE
				oContainer.Controls(nSonControlIndex).Enabled=.F.
				
				sSonControlSourceName = oContainer.Controls(nSonControlIndex).ControlSource
				IF !EMPTY(sSonControlSourceName) THEN 
					getSonControlSourceType = "sSonControlSourceType=VARTYPE(&sSonControlSourceName)"
					&getSonControlSourceType
				ELSE
					sSonControlSourceType = 'C'
				ENDIF 
				
				IF VARTYPE(vDefaultValue)='L' THEN
					DO CASE
						CASE sSonControlSourceType='C'
							vlcDefaultValue=''	
						CASE sSonControlSourceType='D'
							vlcDefaultValue=CTOD('')
						CASE sSonControlSourceType='N'
							vlcDefaultValue=0
					ENDCASE
				ELSE
					vlcDefaultValue=vDefaultValue
				ENDIF
				
				oContainer.Controls(nSonControlIndex).Value=vlcDefaultValue
			ENDIF
		ENDIF 
	NEXT nSonControl

ENDPROC



PROCEDURE setControlValue

	LPARAMETERS oContainer as Object, sFatherControlValue as Variant, sSonControlName as string, ;
				vEnableValuesList as String, aControlsState, vDefaultValue, bClearSonControlValue as Boolean, bNotEnableSonControlValue as Boolean 
				
	
	* Establece el valor de los controles sSonControlName en función del valor ;
	actual sFatherControlValue de un control; en consecuencia permite ejecutar reglas de negocio de la forma: ;
		Si Control_x.Value in {vEnableValuesList} entonces ;
			Control_y.Value=vDefaultValue;
	con y = 1,...,n y n = el número de controles nombrados en la lista separada por comas sSonControlName;
	en este caso, el valor del control_y es establecido de acuerdo al valor del Control_x y vEnableValuesList ;
	es la lista de valores separada por comas que determinan los valores de los controles Control_y de sSonControlName.
	
	*Si se pasa el parámetro opcional bClearSonControlValue, la aplicación de la regla será de la forma: ;
		Si Control_x.Value in {vEnableValuesList} entonces ;
			Control_y.Value=vDefaultValue;
		sino;
			Control_y.Value=Vacío.
	 
	*Si se pasa el parámetro opcional bNotEnableSonControlValue, la aplicación de la regla será de la forma: ;
		Si Control_x.Value in {vEnableValuesList} entonces ;
			Control_y.Value=vDefaultValue;
			Control_y.Enable=.F.;
	
	*aControlsState es un arreglo de los controles contenidos en oContainer. La estructura del arreglo ;
	debe ser: ;
		aControlsState(.,1)= Nombre del control ;
		aControlsState(.,2)= ... ;
		aControlsState(.,3)= ... ;
		aControlsState(.,4)= índice del control dentro del contenedor

		
	LOCAL nFatherControlIndex, nSonControlIndex, sSonControlSourceType


	DIMENSION aSonControls(1,1)	
	nSonControls = ALINES(aSonControls,sSonControlName,1,',')
	DIMENSION aSonControls(nSonControls,1)
	FOR nSonControl=1 TO nSonControls 
		nSonControlRow=ASCAN(aControlsState,aSonControls(nSonControl,1),-1,-1,1,15) 
		nSonControlIndex=aControlsState(nSonControlRow,4)
		*JLGB 11/01/2020
		IF oContainer.Controls(nSonControlIndex).BaseClass = 'Container' THEN
	 
	 		IF ',' + sFatherControlValue + ',' $ ',' + vEnableValuesList + ',' THEN
	 			oContainer.Controls(nSonControlIndex).enablectl(.T.)
	 		ELSE
	 			oContainer.Controls(nSonControlIndex).enablectl(.F.)
	 		ENDIF
		ELSE
			sSonControlSourceName = oContainer.Controls(nSonControlIndex).ControlSource
			getSonControlSourceType = "sSonControlSourceType=VARTYPE(&sSonControlSourceName)"
			&getSonControlSourceType
			
			IF ',' + sFatherControlValue + ',' $ ',' + vEnableValuesList + ',' THEN
				oContainer.Controls(nSonControlIndex).Value=vDefaultValue
				IF bNotEnableSonControlValue THEN
					oContainer.Controls(nSonControlIndex).Enabled = .F.
				ENDIF
			ELSE
				IF bClearSonControlValue THEN
					DO CASE
						CASE sSonControlSourceType='C'
							oContainer.Controls(nSonControlIndex).Value=''	
						CASE sSonControlSourceType='D'
							oContainer.Controls(nSonControlIndex).Value=CTOD('')
						CASE sSonControlSourceType='N'
							oContainer.Controls(nSonControlIndex).Value=0
					ENDCASE
				ENDIF
			ENDIF
			
		ENDIF
	NEXT nSonControl
ENDPROC


FUNCTION checkBusinessRule

	LPARAMETERS vValueToValidate as Variant, sLogicalOperator as String, vRefValue as Variant, ;
				sErrorMsg as string, bAllowEmptyValues as Boolean

	*Check if vValueToValidate is =,<=,>=,< or > than vRefValue. If not, shows sErrorMsg and returns 0; otherwise returns .T.
	*If the optional paameter bAllowEmptyValues is .T., check if vValueToValidate is =,<=,>=,< or > than vRefValue or vValueToValidate is empty


	IF !bAllowEmptyValues THEN
		sRuleIsViolated="!(vValueToValidate" + sLogicalOperator + "vRefValue)"
	ELSE
		sRuleIsViolated="!EMPTY(vValueToValidate ) AND !(vValueToValidate" + sLogicalOperator + "vRefValue)"
	ENDIF
	
	IF  &sRuleIsViolated
		SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		=showErrorMessage(sErrorMsg, 1)
		RETURN 0
	ELSE
		RETURN
	ENDIF
ENDFUNC

FUNCTION checkBusinessRule2

	LPARAMETERS bRuleToValidate as Boolean, sErrorMsg as string

	IF  !bRuleToValidate 
		SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		=showErrorMessage(sErrorMsg, 1)
		RETURN 0
	ELSE
		RETURN
	ENDIF
ENDFUNC


FUNCTION checkIdentityNumber(sIdKind as String, sId as String, sErrorMsg as string, nRuleID as Number)
	
	#DEFINE nCC_MAX_ID_NUMBER_LENGTH  16
		
	LOCAL bRuleIsViolated  as Boolean 

	SET PROCEDURE TO Utilities ADDITIVE

	IF VARTYPE(nRuleID)='L' THEN
		nRuleID = 1
	ENDIF
	
	nMaxIdNumberLength = 8
	
	bRuleIsViolated = .F.
	DO CASE
		CASE nRuleID = 1
			sIdKind = UPPER(sIdKind)
			DO CASE
			   CASE sIdKind ='MS'
			      nMaxIdNumberLength = 12
			   CASE sIdKind ='RC'
			      nMaxIdNumberLength = 11
			   CASE sIdKind ='TI'
			      nMaxIdNumberLength = 11
			   CASE sIdKind ='CC'
			      nMaxIdNumberLength = nCC_MAX_ID_NUMBER_LENGTH
			   CASE sIdKind ='CE'
			      nMaxIdNumberLength = 8
			   CASE sIdKind ='PA'
			      nMaxIdNumberLength = 16
			   CASE sIdKind ='AS'
			      nMaxIdNumberLength = 10
			   CASE sIdKind ='PE'
			      nMaxIdNumberLength = 15
			   OTHERWISE
			   	nMaxIdNumberLength = 0
 			ENDCASE
					
			IF LEN(ALLTRIM(ALLTRIM(sId))) > nMaxIdNumberLength THEN
				*El número de documento estaría errado si, a partir de la máxima longitud permitida, los caracteres restantes ;
				no obedecen el patrón _?? o el patrón -??
				IF !(SUBSTR(sId,nMaxIdNumberLength+1,1) $  ' - _') THEN
					bRuleIsViolated = .T.
				ELSE
					IF LEN(SUBSTR(sId,nMaxIdNumberLength+2)) > 2 THEN
						bRuleIsViolated = .T.
					ENDIF
				ENDIF
			ENDIF
		CASE nRuleID = 2
			bRuleIsViolated = (LEN(ALLTRIM(ALLTRIM(sId))) < MIN_ID_NUMBER_LENGTH)
		CASE nRuleID = 3
			bRuleIsViolated = (PADR(ALLTRIM(sId),25,'0')='0000000000000000000000000')
		CASE nRuleID = 4
			IF sIdKind ='CC' THEN
				IF (OCCURS('_',sId) + OCCURS('-',sId))>1 THEN
					bRuleIsViolated = .T.
				ELSE
					IF '-' $ sId OR '_' $ sId THEN
						sId = SUBSTR(sId,1,AT('-',sId)-1)
						bRuleIsViolated = (!IsNumeric(ALLTRIM(sId)))
					ENDIF
				ENDIF
			ENDIF
	ENDCASE

	IF  bRuleIsViolated 
		SET PROCEDURE TO SIVIGILAMessenger ADDITIVE
		=showErrorMessage(sErrorMsg, 1)
		RETURN 0
	ELSE
		RETURN
	ENDIF

ENDFUNC


FUNCTION ageIsLessThan

	LPARAMETERS sAge1 as String, sAge2 as String

	*Retorna TRUE si sAge1 < sAge2. ;
	sAge(i) i=1,2 es una lista separada por comas en donde el primer item es el número de la edad ;
	y el segundo la unidad de medida (1-Años, 2-Meses, 3-Días, 4-Horas, 5-Minutos)
	
	DIMENSION aAge1(1)
	DIMENSION aAge2(1)
	
	=ALINES(aAge1,sAge1,15,',')  
	=ALINES(aAge2,sAge2,15,',')
	
	bReturnedValue=.T.
	IF aAge1(2)=aAge2(2)
		RETURN (VAL(aAge1(1))<VAL(aAge2(1)))
	ELSE
		RETURN reduceToMinutes(@aAge1)<reduceToMinutes(@aAge2)
	ENDIF
	
ENDFUNC

FUNCTION ageIsGreaterThan

	LPARAMETERS sAge1 as String, sAge2 as String

	*Retorna TRUE si sAge1 > sAge2. ;
	sAge(i) i=1,2 es una lista separada por comas en donde el primer item es el número de la edad ;
	y el segundo la unidad de medida (1-Años, 2-Meses, 3-Días, 4-Horas, 5-Minutos)
	
	RETURN ageIsLessThan(sAge2, sAge1)
	
ENDFUNC

FUNCTION reduceToMinutes

	LPARAMETERS aAge

	*Retorna el equivalente en minutos de la edad aAge = (a1, a2 ) con a1 un caracter numérico y ;
	a2 en {1,2,3,4,5} con 1-Años, 2-Meses, 3-Días, 4-Horas, 5-Minutos
	
	DO CASE 
		CASE aAge(2)='1'
			RETURN ((VAL(aAge(1))*365)*24)*60
		CASE aAge(2)='2'
			RETURN ((VAL(aAge(1))*30)*24)*60
		CASE aAge(2)='3'
			RETURN (VAL(aAge(1))*24)*60
		CASE aAge(2)='4'
			RETURN VAL(aAge(1))*60
		OTHERWISE
			RETURN VAL(aAge(1))
	ENDCASE
	
ENDFUNC


PROCEDURE conditioningDependencyRule

	LPARAMETERS oContainer as Object, sLogicalCondition as String, sSonControlName as string, ;
				aControlsState, vDefaultValue
	
	* Establece el valor de la propiedad Enabled de los controles sSonControlName en función de la condición ;
	lógica sLogicalCondition; en consecuencia permite ejecutar reglas de negocio de la forma: ;
		Si sLogicalCondition entonces ;
			Control_y.Enabled=.T. ;
		si no ;
			Control_y.Enabled=.F. ;
			Control_y.Value=vDefaultValue;
	con y = 1,...,n y n = el número de controles nombrados en la lista separada por comas sSonControlName.;
	vDefaultValue es un parámetro opcional; si no se pasa, se asume vDefaultValue=Vacío, ;
	esto es, al valor del control_y se le asignará vacío.
	
	*aControlsState es un arreglo de los controles contenidos en oContainer. La estructura del arreglo ;
	debe ser: ;
		aControlsState(.,1)= Nombre del control ;
		aControlsState(.,2)= ... ;
		aControlsState(.,3)= ... ;
		aControlsState(.,4)= índice del control dentro del contenedor

		
	LOCAL nFatherControlIndex, nSonControlIndex, sSonControlSourceType


	DIMENSION aSonControls(1,1)	
	nSonControls = ALINES(aSonControls,sSonControlName,1,',')
	DIMENSION aSonControls(nSonControls,1)
	FOR nSonControl=1 TO nSonControls 
		nSonControlRow=ASCAN(aControlsState,aSonControls(nSonControl,1),-1,-1,1,15) 
		nSonControlIndex=aControlsState(nSonControlRow,4)
		
		*Modificado : JLGB 11/01/2020
		IF oContainer.Controls(nSonControls).BaseClass = 'Container' THEN
			IF &sLogicalCondition  THEN
				oContainer.Controls(nSonControlIndex).enablectl(.T.)
			ELSE
				oContainer.Controls(nSonControlIndex).enablectl(.F.)
			ENDIF
		ELSE
			IF &sLogicalCondition  THEN
				oContainer.Controls(nSonControlIndex).Enabled=.T.
			ELSE
				oContainer.Controls(nSonControlIndex).Enabled=.F.
				
				sSonControlSourceName = oContainer.Controls(nSonControlIndex).ControlSource
				getSonControlSourceType = "sSonControlSourceType=VARTYPE(&sSonControlSourceName)"
				&getSonControlSourceType
				
				IF VARTYPE(vDefaultValue)='L' THEN
					DO CASE
						CASE sSonControlSourceType='C'
							vlcDefaultValue=''	
						CASE sSonControlSourceType='D'
							vlcDefaultValue=CTOD('')
						CASE sSonControlSourceType='N'
							vlcDefaultValue=0
					ENDCASE
				ELSE
					vlcDefaultValue=vDefaultValue
				ENDIF
				
				oContainer.Controls(nSonControlIndex).Value=vlcDefaultValue
			ENDIF
		ENDIF
	NEXT nSonControl
ENDPROC


PROCEDURE conditioningDependencyRule2

	LPARAMETERS oContainer as Object, bLogicalCondition as Boolean, sSonControlName as string, ;
				aControlsState, vDefaultValue
	
	* Establece el valor de la propiedad Enabled de los controles sSonControlName en función de la condición ;
	lógica bLogicalCondition; en consecuencia permite ejecutar reglas de negocio de la forma: ;
		Si bLogicalCondition entonces ;
			Control_y.Enabled=.T. ;
		si no ;
			Control_y.Enabled=.F. ;
			Control_y.Value=vDefaultValue;
	con y = 1,...,n y n = el número de controles nombrados en la lista separada por comas sSonControlName.;
	vDefaultValue es un parámetro opcional; si no se pasa, se asume vDefaultValue=Vacío, ;
	esto es, al valor del control_y se le asignará vacío.
	
	*aControlsState es un arreglo de los controles contenidos en oContainer. La estructura del arreglo ;
	debe ser: ;
		aControlsState(.,1)= Nombre del control ;
		aControlsState(.,2)= ... ;
		aControlsState(.,3)= ... ;
		aControlsState(.,4)= índice del control dentro del contenedor

		
	LOCAL nFatherControlIndex, nSonControlIndex, sSonControlSourceType


	
	DIMENSION aSonControls(1,1)	
	nSonControls = ALINES(aSonControls,sSonControlName,1,',')
	DIMENSION aSonControls(nSonControls,1)
	FOR nSonControl=1 TO nSonControls 
		nSonControlRow=ASCAN(aControlsState,aSonControls(nSonControl,1),-1,-1,1,15) 
		nSonControlIndex=aControlsState(nSonControlRow,4)
		*Modificado : JLGB 11/01/2020
		IF oContainer.Controls(nSonControlIndex).BaseClass = 'Container' THEN  
			oContainer.Controls(nSonControlIndex).enablectl(bLogicalCondition)
		ELSE
			IF bLogicalCondition THEN
				oContainer.Controls(nSonControlIndex).Enabled=.T.
			ELSE
				oContainer.Controls(nSonControlIndex).Enabled=.F.
				
				sSonControlSourceName = oContainer.Controls(nSonControlIndex).ControlSource
				getSonControlSourceType = "sSonControlSourceType=VARTYPE(&sSonControlSourceName)"
				&getSonControlSourceType
				
				IF VARTYPE(vDefaultValue)='L' THEN
					DO CASE
						CASE sSonControlSourceType='C'
							vlcDefaultValue=''	
						CASE sSonControlSourceType='D'
							vlcDefaultValue=CTOD('')
						CASE sSonControlSourceType='N'
							vlcDefaultValue=0
					ENDCASE
				ELSE
					vlcDefaultValue=vDefaultValue
				ENDIF
				
				oContainer.Controls(nSonControlIndex).Value=vlcDefaultValue
			ENDIF
		ENDIF
	NEXT nSonControl
ENDPROC


PROCEDURE conditioningDependencyRule3

	LPARAMETERS oContainer as Object, bLogicalCondition as Boolean, sSonControlName as string, ;
				aControlsState, vDefaultValue
	
	* Establece el valor de la propiedad Enabled de los controles sSonControlName en función de la condición ;
	lógica bLogicalCondition; en consecuencia permite ejecutar reglas de negocio de la forma: ;
		Si bLogicalCondition entonces ;
			Control_y.Enabled=.F. ;
			Control_y.Value=vDefaultValue;
		si no ;
			Control_y.Enabled=.T. ;
	con y = 1,...,n y n = el número de controles nombrados en la lista separada por comas sSonControlName.;
	vDefaultValue es un parámetro opcional; si no se pasa, se asume vDefaultValue=Vacío, ;
	esto es, al valor del control_y se le asignará vacío.
	
	*aControlsState es un arreglo de los controles contenidos en oContainer. La estructura del arreglo ;
	debe ser: ;
		aControlsState(.,1)= Nombre del control ;
		aControlsState(.,2)= ... ;
		aControlsState(.,3)= ... ;
		aControlsState(.,4)= índice del control dentro del contenedor

		
	LOCAL nFatherControlIndex, nSonControlIndex, sSonControlSourceType


	DIMENSION aSonControls(1,1)	
	nSonControls = ALINES(aSonControls,sSonControlName,1,',')
	DIMENSION aSonControls(nSonControls,1)
	FOR nSonControl=1 TO nSonControls 
		nSonControlRow=ASCAN(aControlsState,aSonControls(nSonControl,1),-1,-1,1,15) 
		nSonControlIndex=aControlsState(nSonControlRow,4)
		
		*Modificado : JLGB 11/01/2020
		IF oContainer.Controls(nSonControlIndex).BaseClass = 'Container' THEN  
			oContainer.Controls(nSonControlIndex).enablectl(bLogicalCondition  )
		ELSE
			IF bLogicalCondition THEN
				oContainer.Controls(nSonControlIndex).Enabled=.F.
				
				sSonControlSourceName = oContainer.Controls(nSonControlIndex).ControlSource
				getSonControlSourceType = "sSonControlSourceType=VARTYPE(&sSonControlSourceName)"
				&getSonControlSourceType
				
				IF VARTYPE(vDefaultValue)='L' THEN
					DO CASE
						CASE sSonControlSourceType='C'
							vlcDefaultValue=''	
						CASE sSonControlSourceType='D'
							vlcDefaultValue=CTOD('')
						CASE sSonControlSourceType='N'
							vlcDefaultValue=0
					ENDCASE
				ELSE
					vlcDefaultValue=vDefaultValue
				ENDIF
				
				oContainer.Controls(nSonControlIndex).Value=vlcDefaultValue
			ELSE
				oContainer.Controls(nSonControlIndex).Enabled=.T.
			ENDIF
		ENDIF
	NEXT nSonControl
ENDPROC


PROCEDURE conditioningDependencyRule4

	LPARAMETERS oContainer as Object, bLogicalCondition as Boolean, sSonControlNameToUnenable as string, ;
				sSonControlNameToEnable, aControlsState, vDefaultValue
	
	* Establece el valor de la propiedad Enabled de los controles sSonControlNameToUnenable en función de la condición ;
	lógica bLogicalCondition; en consecuencia permite ejecutar reglas de negocio de la forma: ;
		Si bLogicalCondition entonces ;
			Control_y.Enabled=.F. ;
			Control_y.Value=vDefaultValue;
		si no ;
			Control_z.Enabled=.T. ;
	con y = 1,...,n y n = el número de controles nombrados en la lista separada por comas sSonControlNameToUnenable.;
	y 	z = 1,...,m y m = el número de controles nombrados en la lista separada por comas sSonControlNameToEnable.;
	vDefaultValue es un parámetro opcional; si no se pasa, se asume vDefaultValue=Vacío, ;
	esto es, al valor del control_y se le asignará vacío.
	
	*aControlsState es un arreglo de los controles contenidos en oContainer. La estructura del arreglo ;
	debe ser: ;
		aControlsState(.,1)= Nombre del control ;
		aControlsState(.,2)= ... ;
		aControlsState(.,3)= ... ;
		aControlsState(.,4)= índice del control dentro del contenedor

		
	LOCAL nFatherControlIndex, nSonControlIndex, sSonControlSourceType

	DIMENSION aSonControls(1,1)	
	nSonControls = ALINES(aSonControls,sSonControlNameToEnable,1,',')
	DIMENSION aSonControls(nSonControls,1)
	FOR nSonControl=1 TO nSonControls 
		nSonControlRow=ASCAN(aControlsState,aSonControls(nSonControl,1),-1,-1,1,15) 
		nSonControlIndex=aControlsState(nSonControlRow,4)
		
		IF !bLogicalCondition THEN
			oContainer.Controls(nSonControlIndex).Enabled=.T.
		ENDIF
	NEXT nSonControl

	DIMENSION aSonControls(1,1)	
	nSonControls = ALINES(aSonControls,sSonControlNameToUnenable,1,',')
	DIMENSION aSonControls(nSonControls,1)
	FOR nSonControl=1 TO nSonControls 
		nSonControlRow=ASCAN(aControlsState,aSonControls(nSonControl,1),-1,-1,1,15) 
		nSonControlIndex=aControlsState(nSonControlRow,4)
		
		*Modificado : JLGB 11/01/2020
		IF oContainer.Controls(nSonControlIndex).BaseClass = 'Container' THEN 
			oContainer.Controls(nSonControlIndex).enablectl(bLogicalCondition)
		ELSE
			IF bLogicalCondition THEN
				oContainer.Controls(nSonControlIndex).Enabled=.F.
				
				sSonControlSourceName = oContainer.Controls(nSonControlIndex).ControlSource
				getSonControlSourceType = "sSonControlSourceType=VARTYPE(&sSonControlSourceName)"
				&getSonControlSourceType
				
				IF VARTYPE(vDefaultValue)='L' THEN
					DO CASE
						CASE sSonControlSourceType='C'
							vlcDefaultValue=''	
						CASE sSonControlSourceType='D'
							vlcDefaultValue=CTOD('')
						CASE sSonControlSourceType='N'
							vlcDefaultValue=0
					ENDCASE
				ELSE
					vlcDefaultValue=vDefaultValue
				ENDIF
				
				oContainer.Controls(nSonControlIndex).Value=vlcDefaultValue
			ENDIF
		ENDIF
	NEXT nSonControl
ENDPROC

PROCEDURE conditioningDependencyRule5

	LPARAMETERS oContainer as Object, bLogicalCondition as Boolean, sSonControlName as string, ;
				aControlsState, vDefaultValue
	
	* Establece el valor de la propiedad Enabled de los controles sSonControlName en función de la condición ;
	lógica bLogicalCondition; en consecuencia permite ejecutar reglas de negocio de la forma: ;
		Si bLogicalCondition entonces ;
			Control_y.Enabled=.F. ;
			Control_y.Value=vDefaultValue;
		si no ;
			Control_y.Enabled=.T. ;
			Control_y.Value=('' or 0 or {//}';
	con y = 1,...,n y n = el número de controles nombrados en la lista separada por comas sSonControlName.;
	vDefaultValue es un parámetro opcional; si no se pasa, se asume vDefaultValue=Vacío, ;
	esto es, al valor del control_y se le asignará vacío.
	
	*aControlsState es un arreglo de los controles contenidos en oContainer. La estructura del arreglo ;
	debe ser: ;
		aControlsState(.,1)= Nombre del control ;
		aControlsState(.,2)= ... ;
		aControlsState(.,3)= ... ;
		aControlsState(.,4)= índice del control dentro del contenedor

		
	LOCAL nFatherControlIndex, nSonControlIndex, sSonControlSourceType


	DIMENSION aSonControls(1,1)	
	nSonControls = ALINES(aSonControls,sSonControlName,1,',')
	DIMENSION aSonControls(nSonControls,1)
	FOR nSonControl=1 TO nSonControls 
		nSonControlRow=ASCAN(aControlsState,aSonControls(nSonControl,1),-1,-1,1,15) 
		nSonControlIndex=aControlsState(nSonControlRow,4)
		
		*Modificado : JLGB 11/01/2020
		IF oContainer.Controls(nSonControlIndex).BaseClass = 'Container' THEN
			oContainer.Controls(nSonControlIndex).enablectl(bLogicalCondition)
		ELSE
			IF bLogicalCondition THEN
				oContainer.Controls(nSonControlIndex).Enabled=.F.
				
				sSonControlSourceName = oContainer.Controls(nSonControlIndex).ControlSource
				getSonControlSourceType = "sSonControlSourceType=VARTYPE(&sSonControlSourceName)"
				&getSonControlSourceType
				
				IF VARTYPE(vDefaultValue)='L' THEN
					DO CASE
						CASE sSonControlSourceType='C'
							vlcDefaultValue=''	
						CASE sSonControlSourceType='D'
							vlcDefaultValue=CTOD('')
						CASE sSonControlSourceType='N'
							vlcDefaultValue=0
					ENDCASE
				ELSE
					vlcDefaultValue=vDefaultValue
				ENDIF
				
				oContainer.Controls(nSonControlIndex).Value=vlcDefaultValue
			ELSE
				oContainer.Controls(nSonControlIndex).Enabled=.T.

				sSonControlSourceName = oContainer.Controls(nSonControlIndex).ControlSource
				getSonControlSourceType = "sSonControlSourceType=VARTYPE(&sSonControlSourceName)"
				&getSonControlSourceType
				
				vSonControlValue=''	
				DO CASE
					CASE sSonControlSourceType='D'
						vSonControlValue=CTOD('')
					CASE sSonControlSourceType='N'
						vSonControlValue=0
				ENDCASE
				
				oContainer.Controls(nSonControlIndex).Value=vSonControlValue
			ENDIF
		ENDIF
	NEXT nSonControl
ENDPROC


PROCEDURE calculateControlValue

	LPARAMETERS oContainer as Object, sTargetControlName as Variant, sSourceControlsList as string, ;
				aControlsState


	DIMENSION aSourceControls(1,1)	
	nSourceControls = ALINES(aSourceControls,sSourceControlsList,1,',')
	DIMENSION aSourceControls(nSourceControls,1)
	nCalculatedResult = 0
	FOR nSourceControl=1 TO nSourceControls
		nSourceControlRow=ASCAN(aControlsState,aSourceControls(nSourceControl,1),-1,-1,1,15) 
		nSourceControlIndex=aControlsState(nSourceControlRow,4)
		
		sSourceControlSourceName= oContainer.Controls(nSourceControlIndex).ControlSource
		getSourceControlSourceType = "sSourceControlType=VARTYPE(&sSourceControlSourceName)"
		&getSourceControlSourceType
		
		DO CASE
			CASE sSourceControlType='C'
				nCalculatedResult= 	nCalculatedResult +	VAL(oContainer.Controls(nSourceControlIndex).Value)
			CASE sSourceControlType='N'
				nCalculatedResult= 	nCalculatedResult +	oContainer.Controls(nSourceControlIndex).Value
			OTHERWISE
		ENDCASE
	NEXT

	nTargetControlRow=ASCAN(aControlsState,sTargetControlName,-1,-1,1,15) 
	nTargetControlIndex=aControlsState(nTargetControlRow,4)
	sTargetControlSourceName= oContainer.Controls(nTargetControlIndex).ControlSource
	getTargetControlSourceType = "sTargetControlSourceType=VARTYPE(&sTargetControlSourceName)"
	&getTargetControlSourceType 
	DO CASE
		CASE sTargetControlSourceType='C'
			oContainer.Controls(nTargetControlIndex).Value = ALLTRIM(STR(nCalculatedResult))
		CASE sTargetControlSourceType='N'
			oContainer.Controls(nTargetControlIndex).Value = nCalculatedResult
		OTHERWISE
	ENDCASE
	
ENDPROC


FUNCTION isValidAge

	LPARAMETERS dBirthDate AS DATE, cAge AS String, cUnitOfMeasure AS String

	* Valida la consistencia entre dBirthDate, cAge y cUnitOfMeasure.  Retorna .T. si es consistente y .F. en caso contrario.

	IF EMPTY(cUnitOfMeasure) OR EMPTY(cAge) THEN 
		*No se puede verificar la consistencia por falta de datos; por tanto, retorna .T.
		RETURN .T.
	ENDIF 
	
	SET PROCEDURE TO Age ADDITIVE	
	LOCAL bReturnedValue, lcAge, lcUnitOfMeasure
	
	IF EMPTY(dBirthDate)
		* Verifica si  (cAge, cUnitOfMeasure ) es consistente
		
		bReturnedValue = .F.
		DO CASE
		
			* Si la edad son minutos, se valida que esté dentro del rango permitido
			CASE cUnitOfMeasure = '5' 
				IF BETWEEN(INT(VAL(cAge)),1,59)
					bReturnedValue = .T.
				ENDIF
				
			* Si la edad son horas, se valida que esté dentro del rango permitido
			CASE cUnitOfMeasure = '4' 
				IF BETWEEN(INT(VAL(cAge)),1,23)
					bReturnedValue = .T.
				ENDIF
				
			* Si la edad son días, se valida que esté dentro del rango permitido
			CASE cUnitOfMeasure = '3' 
				IF BETWEEN(INT(VAL(cAge)),1,29)
					bReturnedValue = .T.
				ENDIF
				
			* Si la edad son meses, se valida que esté dentro del rango permitido
			CASE cUnitOfMeasure = '2' 
				IF BETWEEN(INT(VAL(cAge)),1,11)
					bReturnedValue = .T.
				ENDIF
			
			* Si la edad son años, se valida que esté dentro del rango permitido
			CASE cUnitOfMeasure = '1' 
				IF BETWEEN(INT(VAL(cAge)),1,132)
					bReturnedValue = .T.
				ENDIF
			
			* Si la edad, y la unidad de medida son 0
			CASE cUnitOfMeasure = '0' 
				IF INT(VAL(cAge)) = 0
					bReturnedValue = .T.
				ENDIF
		ENDCASE		
	ELSE
		*Calcula la edad y unidad de mdedia de la edad con base en dBirthDate  y la fecha actual (DATE())
		bReturnedValue = .T.
		oAgeCalculator = NEWOBJECT("SivigilaAgeCalculator", "SivigilaAgeCalculator.fxp")
		oAgeCalculator.SivigilaCalculateAge(dBirthDate, DATE())
		lcAge = oAgeCalculator.cAge
		lcUnitOfMeasure = oAgeCalculator.cUnitOfMeasure
		RELEASE oAgeCalculator 
*SET STEP ON 	
		* Verifica si la edad y su unidad de medida recien calculadas son consistentes con (cAge, cUnitOfMeasure )
		
		IF lcAge <> '0'
			*Edad y unidad de mdedia de la edad recien calculadas deben ser iguales a edad y unidad de medida suministradas como parámetros ;
			de esta función
			bReturnedValue = (lcUnitOfMeasure = cUnitOfMeasure AND lcAge = cAge)
		ELSE 
			*La unidad de medida solo puede ser 0 = No aplica 4 = Horas 5 = Minutos por cuanto lcAge ='0' (=0 días) 
			bReturnedValue = .F.
			DO CASE
				* Si la unidad de medida son minutos, se valida que la edad esté dentro del rango permitido
				CASE cUnitOfMeasure = '5' 
					IF BETWEEN(INT(VAL(cAge)),1,59)
						bReturnedValue = .T.
					ENDIF
					
				* Si la unidad de medida son horas, se valida que la edad esté dentro del rango permitido
				CASE cUnitOfMeasure = '4' 
					IF BETWEEN(INT(VAL(cAge)),1,23)
						bReturnedValue = .T.
					ENDIF
					
				* Si la unidad de medida es 0, edad debe ser 0
				CASE cUnitOfMeasure = '0' 
						bReturnedValue = (INT(VAL(cAge))=0)
			ENDCASE		
		ENDIF 
	ENDIF
	RETURN bReturnedValue
ENDFUNC


FUNCTION isValidIdentificationDoc

	LPARAMETERS sIdentificationKind as String, sUnitOfMeasure as string, sAge as String, sErrMsg as sgtring	
	
	*Establece la consistencia entre el tipo de identifcación sIdentificationKind y la edad determinada por el par (sUnitOfMeasure, sAge );
	Devuelve .T. si hay consistencia; en caso contrario decuelve .F. ;
	En el parámetro opcional sErrMsg, se retorna un aviso indicativo de la inconsistencia encontrada
	
	IF sUnitOfMeasure ='1' AND (VAL(sAge)<18 OR VAL(sAge)>132)
		*sUnitOfMeasure es 1 = Años
		IF sIdentificationKind ='CC' OR sIdentificationKind ='CE' OR sIdentificationKind ='AS'
			IF VARTYPE(sErrMsg)='C' THEN
				sErrMsg = "Edad inválida para el tipo de documento"
			ENDIF
	    	RETURN .F.
		ENDIF
	ENDIF
	
	IF sUnitOfMeasure !='1'
		*sUnitOfMeasure es una de las unidades 0 = No aplica 2 = Meses 3 = Días 4 = Horas 5 = Minutos
		IF sIdentificationKind ='CC' OR sIdentificationKind ='CE' OR sIdentificationKind ='AS'
			IF VARTYPE(sErrMsg)='C' THEN
				sErrMsg = "Edad inválida para el tipo de documento"
			ENDIF
	    	RETURN .F.
		ENDIF
	ENDIF
	
	IF sUnitOfMeasure ='1' AND VAL(sAge)<7
		*sUnitOfMeasure es 1 = Años
		IF sIdentificationKind!='MS' AND sIdentificationKind!='RC' AND sIdentificationKind!='PA'  AND sIdentificationKind!='PE'
			IF VARTYPE(sErrMsg)='C' THEN
				sErrMsg = "Edad inválida para el tipo de documento"
			ENDIF
	    	RETURN .F.
		ENDIF
	ENDIF

	IF INLIST(ALLTRIM(sIdentificationKind),'CC','TI','CE') AND sUnitOfMeasure  !='1' THEN
		IF VARTYPE(sErrMsg)='C' THEN
			sErrMsg = 'Si selecciona  CC, TI o CE  en documento de identidad, unidad de medida solo puede ser 1'
		ENDIF
		RETURN .F.
	ENDIF

	IF sUnitOfMeasure ='1' AND VAL(sAge)>17 
		IF sIdentificationKind ='MS' OR sIdentificationKind ='RC' OR sIdentificationKind ='TI'
			IF VARTYPE(sErrMsg)='C' THEN
				sErrMsg = "Edad inválida para el tipo de documento"
			ENDIF
	    	RETURN .F.
		ENDIF
	ENDIF
 
	IF ALLTRIM(sIdentificationKind) = 'CN' THEN
		sErrMsg = 'Si selecciona  CN en documento de identidad, la edad debe ser menor o igual a tres meses'
		IF EMPTY(sAge) OR EMPTY(sUnitOfMeasure) then
			RETURN .F.
		ELSE
			*RETURN (ageIsLessThan('&sAge,&sUnitOfMeasure','4,2'))
			RETURN (ageIsLessThan('&sAge,&sUnitOfMeasure','91,3'))
		ENDIF
	ENDIF
	
	RETURN .T.
ENDFUNC


FUNCTION maxNotificationDate(dTargetDate as Date) as Date

	*Retorna la fecha máxima permitida de notificación dado dTargetDate. Esta fecha corresponde a la del;
	sábado o viernes inmediatamente anterior a dTargetDate o a dTargetDate, si dTargetDate corresponde a ;
	un sábado o un viernes.

	LOCAL returnedVal as Date
	
	IF DOW(dTargetDate) = 7 OR DOW(dTargetDate) = 6 THEN
		*dTargetDate corresponde a un sábado o un viernes
		returnedVal = dTargetDate 
	ELSE
		returnedVal = dTargetDate - DOW(dTargetDate)
	ENDIF
	RETURN returnedVal 
ENDFUNC


FUNCTION beforeInsert(sEventCode as String, sMsg) as boolean
	
	*Ejecuta reglas de validación para el evento con código sEventCode antes de insertar/actualizar un registro. Devuelve .T., si los datos en memoria,;
	cumplen las reglas; en caso contrario, retorna .F. y sMsg contendrá el texto de error de la regla que se está incumpliendo. 
	
	*Esta función parte de la base de que existen en memoria variables asociadas a campos de datos básicos y datos complementarios que se han instanciado;
	gracias a la ejecución de un SCATTER MEMVAR
	
	LOCAL bSuccess  as Boolean
	
	DO CASE  
		CASE sEventCode = '710' OR sEventCode = '730'
			bSuccess = (!((m.TIP_CAS $ '3,4,5' OR m.AJUSTE $ '3,4,5') AND SEG_CONT='1')  OR (!EMPTY(m.FEC_SEGUIM)))
			sMsg = 'Si el caso es confirmado y hubo seguimiento a contactos, es obligatorio diligenciar la fecha final de seguimiento a contactos '
			
			IF bSuccess THEN 
				bSuccess = (!(m.AJUSTE $ '3,4,5') OR !EMPTY(m.FTE_INFECC))
				sMsg = 'Si el caso es confirmado, se debe digitar fuente de infección'
			ENDIF

			IF bSuccess THEN 
				bSuccess = (m.AJUSTE!='6' OR !EMPTY(m.CRI_DES))
				sMsg = 'Si se realiza ajuste 6 (Descartado), se debe digitar el criterio para descartar'
			ENDIF

			IF bSuccess THEN 
				bSuccess = (!(m.AJUSTE='3' AND m.FTE_INFECC='1') OR !EMPTY(m.IMP_PAIS))
				sMsg = 'Si se realiza ajuste 3(confirmado por laboratorio) y fuente de infección = 1 (Importado), debe diligenciar "de qué país"'
			ENDIF
			
			RETURN bSuccess 
			
		CASE sEventCode = '815'
			bSuccess = m.RES_PRUVIH!='1' OR (!EMPTY(m.TER_PREV_T) AND !EMPTY(m.TRAT_ANTIR))
			sMsg = 'Si "Resultado de la prueba para diagnostico de VIH" es 1, entonces debe diligenciar "Terapia preventiva  con trimetropin sulfa/cotrimoxazol" y "Recibe tratamiento antirretroviral"'
			
			RETURN bSuccess 

		CASE sEventCode = '359'
			bSuccess = VAL(m.N_D_P1_S_N) + VAL(m.N_D_P2_S_N) + VAL(m.N_D_P3_S_N) + VAL(m.N_D_P4_S_N) + VAL(m.N_D_P5_S_N) = VAL(m.DIA_PAC_N)
			sMsg = 'La suma de días paciente por grupos de edad debe ser igual al total de días paciente vigilados en UCI/N"'
			
			IF bSuccess THEN 
				bSuccess = VAL(m.N_D_P1_SIN) + VAL(m.N_D_P2_SIN) + VAL(m.N_D_P3_SIN) + VAL(m.N_D_P4_SIN) + VAL(m.N_D_P5_SIN) = VAL(m.DIA_PAC_IN)
				sMsg = 'La suma de días paciente por grupos de edad debe ser igual al total de días paciente vigilados en UCI/IN"'
			ENDIF
			
			RETURN bSuccess 
			
		CASE sEventCode = '205'
			*SET STEP ON 
			*bSuccess = (m.CLAS_CASO != '2' OR (ageIsLessThan(m.EDAD + "," + m.UNI_MED,'18,1') OR m.GP_GESTAN ='1' OR m.PER_ETN = '1') OR (m.FUENTE='5'))
			*sMsg = 'Los casos crónicos solo pueden ser de menores de 18 años, gestantes o indígenas'
			bSuccess = (m.CLAS_CASO != '2' OR (m.SEXO!='F' OR (ageIsGreaterThan(m.EDAD + "," + m.UNI_MED,'0,1') AND ageIsLessThan(m.EDAD + "," + m.UNI_MED,'55,1')) OR m.PER_ETN = '1' OR m.FUENTE='5'))
			sMsg = 'Los casos crónicos en mujeres solo pueden ser de 1 a 54 años o indígenas'
			
			IF bSuccess THEN 
				bSuccess = (m.CLAS_CASO != '2' OR (m.SEXO!='M' OR (ageIsLessThan(m.EDAD + "," + m.UNI_MED,'18,1') OR m.PER_ETN = '1' OR m.FUENTE='5')))
				sMsg = 'Los casos crónicos en hombres solo pueden ser de menores de 18 años o indígenas'
			ENDIF

			IF bSuccess THEN
				bSuccess = (m.PROB_VIA != '3' OR (ageIsLessThan(m.EDAD + "," + m.UNI_MED,'30,3') AND m.CLAS_CASO='1' ) )
				sMsg = 'La opción de posible vía de trasmisión congénita solo aplica para el recién nacido y se debe considerar como un caso agudo'
			ENDIF

			RETURN bSuccess 

		CASE sEventCode = '348'
			bSuccess = (m.PAC_HOS != '1' OR !EMPTY(m.SERV_HOSP))
			sMsg = 'Si  en hospitalizado se digitó 1, entonces debe diligenciar Servicio en el que se hospitalizó'
			
			RETURN bSuccess 
			
		CASE sEventCode = '850'
			bSuccess = (m.EST_CLI!='3' OR m.CON_FIN='2')
			sMsg = 'Si estado clínico es 3 = Muerto, entonces En la condición final de los datos básicos se debe registrar 2 = Muerto'
			
			RETURN bSuccess 

		CASE sEventCode = '352'

			bSuccess = (m.SEXO!='M' OR PROCEDIMIE!='1')
			sMsg = 'Si en  los datos básicos se ha seleccionado Hombre, el "Procedimiento quirúrgico realizado" no puede ser cesarea '
			
			IF bSuccess THEN 
				*Endometritis después de parto vaginal hasta por 30 días.
				bSuccess = (m.PROCEDIMIE!='3' OR ABS(m.INI_SIN - m.FEC_PROC) <= 30)

				IF bSuccess THEN
					*Infección de sitio quirúrgico superficial 30 días para todos los procedimientos
					bSuccess = (!(m.SUP_PRIMAR='1' OR m.SUP_SECUND='1')  OR (ABS(m.INI_SIN - m.FEC_PROC) <= 30))
				ENDIF 
			
				IF bSuccess THEN
					*Infección de sitio quirúrgico profunda primaria u órgano espacio después de cesarea hasta por 30 días
					bSuccess = (!((m.PROF_PRIMA = '1' OR m.ORG_ESPACI='1') AND m.PROCEDIMIE='1') OR (ABS(m.INI_SIN - m.FEC_PROC) <= 30))
				ENDIF 

				IF bSuccess THEN
					*Infección de sitio quirúrgico profunda primaria u órgano espacio después de colecistectomía hasta por 30 días
					bSuccess = (!((m.PROF_PRIMA = '1' OR m.ORG_ESPACI='1') AND m.PROCEDIMIE='5') OR (ABS(m.INI_SIN - m.FEC_PROC) <= 30))
				ENDIF 
				
				IF bSuccess THEN
					*Infección de sitio quirúrgico profunda primaria u órgano espacio después de herniorrafia hasta por 90 días
					bSuccess = (!((m.PROF_PRIMA = '1' OR m.ORG_ESPACI='1') AND m.PROCEDIMIE='2') OR (ABS(m.INI_SIN - m.FEC_PROC) <= 90))
				ENDIF 
				
				IF bSuccess THEN
					*Infección de sitio quirúrgico superficial secundaria y profunda secundaria después de revascularización miocárdica con incisión torácica y de sitio donante hasta por 90 días
					bSuccess = (!((m.SUP_SECUND = '1' OR m.PROF_SECUN='1') AND m.PROCEDIMIE='4') OR (ABS(m.INI_SIN - m.FEC_PROC) <= 90))
				ENDIF 
				
				IF bSuccess THEN
					*Infección de sitio quirúrgico profunda primaria u órgano espacio después de revascularización miocárdica con incisión torácica y de sitio donante hasta por 90 días
					bSuccess = (!((m.PROF_PRIMA = '1' OR m.ORG_ESPACI='1') AND m.PROCEDIMIE='4') OR (ABS(m.INI_SIN - m.FEC_PROC) <= 90))
				ENDIF
				
				sMsg = 'Se ha excedido el periodo de vigilancia para este procedimiento, verifique la fecha de inicio de síntomas y la fecha del procedimiento'
			ENDIF 						
			
			RETURN bSuccess 

		CASE sEventCode = '365'
			bSuccess = (m.TIP_EXP!='11' OR m.CON_FIN='2')
			sMsg = 'El tipo de exposición Suicidio solo aplica si la condición final del paciente es muerto'
			
			RETURN bSuccess 

		CASE sEventCode = '351'
			*bSuccess = (m.ATEN_PARTO!='1' OR (ABS(m.INI_SIN - m.FEC_PARTO) <= 30))
			bSuccess = (ABS(m.INI_SIN - m.FEC_PARTO) <= 30)
			sMsg = 'Se ha excedido el periodo de vigilancia para este procedimiento, verifique la fecha de inicio de síntomas y la fecha del procedimiento'
			
			RETURN bSuccess 

		CASE sEventCode = '452'
			bSuccess = !(m.ARTEF_LESI='1' AND ageIsLessThan(m.edad + "," + m.uni_med,'18,1')) OR !EMPTY(m.CON_ALC_AC)
			sMsg = 'Si "Tipo de artefacto que produjo la lesión" es 1, debe diligenciar "Si es menor de edad, hubo consumo de alcohol u otras sustancias psicoactivas previamente a la lesión por pólvora de los adultos acompañantes"'
			
			RETURN bSuccess 

		CASE sEventCode = '453'
			bSuccess = .T.
			
			IF VARTYPE(m.OCASIO_POR)!='U' THEN 
				*Las restanes variables de datos complementarios involucradas en la regla también se encuentran deefinidas con seguridad
				bSuccess = (!(m.PAC_HOS='1' AND m.OCASIO_POR='4') OR (HOSPITALIZ='1' OR UCI='1'))
				sMsg = 'Si el paciente está hospitalizado, al menos una de las variables "Hospitalización" o "UCI" debe estar diligenciada en 1"'
			ENDIF 
			
			IF VARTYPE(m.OCASIO_POR)!='U' THEN 
				*Las restanes variables de datos complementarios involucradas en la regla también se encuentran deefinidas con seguridad
				IF bSuccess THEN 
					bSuccess = (m.OCASIO_POR!='2' OR ageIsLessThan(m.EDAD + "," + m.UNI_MED,'18,1'))
					sMsg = 'Los casos de exposicion laboral en menores de 18 años solo aplican a personas con edades menores a 18 años'
				ENDIF
			ENDIF
			
			IF VARTYPE(m.CIGARR_ELE)!='U' THEN 
				*La variable de datos complementarios involucrada en la regla se encuentra deefinida con seguridad
				IF bSuccess THEN 
					bSuccess = (m.TIP_CAS!='1' OR m.CIGARR_ELE='1')
					sMsg = 'Solo se permite la clasificación inicial Sospechoso cuando cuando la notificación sea con mecanismo "cigarrillo electrónico"'
				ENDIF
			ENDIF 
				
			RETURN bSuccess 

		CASE sEventCode = '875'
			bSuccess = (!(',' + ALLTRIM(m.NAT_VIOSEX) + ',' $  ',6,7,10,14,' AND m.SEXO='F') OR (!EMPTY(m.AC_ANTICON) AND !EMPTY(m.AC_IVE)))
			sMsg = 'Las variables de atención en salud son de obligatorio diligenciamiento para las opciones 6, 7, 10, 14 de violencia sexual'
			
			IF bSuccess THEN 
				bSuccess = (m.FECHA_NTO <= m.FEC_HECHO)
				sMsg = 'La fecha del hecho no puede ser anterior a la fecha de nacimiento'
			ENDIF

			IF bSuccess THEN 
				bSuccess = (m.FEC_HECHO <= m.FEC_CON)
				sMsg = 'La fecha del hecho no puede ser posterior a la fecha de consulta'
			ENDIF

			RETURN bSuccess 

		CASE sEventCode = '652' OR sEventCode = '650'

			bSuccess = (m.fec_vac <= m.FEC_NOT)
			sMsg = 'La fecha de vacunación del animal no puede ser superior a la fecha de notificación'
			
			IF bSuccess THEN 
				bSuccess = (m.f_ini_sin <= m.FEC_NOT)
				sMsg = 'La fecha de Inicio de Síntomas no puede ser superior a la fecha de notificación'
			ENDIF

			IF bSuccess THEN
				bSuccess = (!(VAL(m.ajuste)>0) OR (m.fec_aju >= m.FEC_NOT))
				sMsg = 'La fecha del Ajuste, no puede ser inferior a la fecha de notificación'
			ENDIF
			
			RETURN bSuccess 
		*JLGB : 21/122019	
		CASE sEventCode = '740'
			bSuccess = !((m.con_fin = '1') AND (m.RES_GESTAC = '3'))
			sMsg = 'No puede digitar vivo en la "Condición Final" en datos básicos y mortinato en "Resultado de la Gestación"'
			
			RETURN bSuccess 

		CASE sEventCode = '465'
			bSuccess = (m.F_INI_TRAT >= m.ini_sin AND m.F_INI_TRAT >= m.fec_con)
			sMsg = 'La fecha de inicio de tratamiento no puede ser menor a la fecha de inicio de síntomas ni a la fecha de consulta'
			
			IF bSuccess THEN
				bSuccess = (m.CON_FIN!='2' OR m.COMPLICACI='1')
				sMsg = 'La variable condición final de datos básicos, solo permite el ingreso de casos con condición final 2=muerto, cuando se seleccione la opción 1=Si en complicaciones'
			ENDIF

			RETURN bSuccess 

		CASE sEventCode = '549'
			bSuccess = (EMPTY(m.FEC_EGRESO) or (m.FEC_EGRESO >= m.FEC_HOS))
			sMsg = 'La fecha de egreso no puede ser menor a la fecha de hospitalización'

			RETURN bSuccess 

		CASE UPPER(sEventCode) = 'GENERAL'
			bSuccess = (m.DIA_BCK=1)
			sMsg = 'El número de días de intervalo entre backups debe ser 1'

			RETURN bSuccess 

		CASE sEventCode = '346 '
			bSuccess = (m.PAC_HOS!='1' or !EMPTY(m.SERV_HOSP))
			sMsg = 'Si el paciente es hospitalizado, debe diligenciar Servicio en el que se hospitalizó'

			RETURN bSuccess 
			
		OTHERWISE
			RETURN .T.
	ENDCASE

ENDPROC


*JLGB : 23/01(2020
FUNCTION beforeInsertLab(sEventCode as String, sMsg, oCData, bIsUpdating as Boolean) as boolean
	
	*Ejecuta reglas de validación para los datos de laboratorio del evento con código sEventCode antes de insertar/actualizar un registro. ;
	Devuelve .T., si los datos en memoria, cumplen las reglas; en caso contrario, retorna .F. y sMsg contendrá el texto de error de la ;
	regla que se está incumpliendo. 
	*oCData es un objeto de la clase cls_IndividualComplementaryData que contiene los datos complementarios del evento
	
	*Esta función parte de la base de que existen en memoria variables asociadas a campos de datos básicos y datos complementarios que se han instanciado;
	gracias a la ejecución de un SCATTER MEMVAR
	
	LOCAL nCurrentCaseLabData as Byte 
	nCurrentCaseLabData = 0
	
	LOCAL bSuccess  as Boolean
	bSuccess = .T.

	DO CASE  
		CASE sEventCode = '357'
			
			bSuccess = !ISNULL(oCData)
			sMsg = 'Se requiere diligenciar Datos Complementarios antes de ingresar laboratorios'
			
			IF bSuccess THEN 
				*Establece cuántos registros activos de laboratorio existen actualmente para el evento.Un registro es activo si no se ha descartado.
				sCadKey = m.AÑO+ m.SEMANA+ m.COD_EVE+ m.TIP_IDE+ m.NUM_IDE+ m.COD_PRE+ m.COD_SUB
				sCadSQL = "Select *,DTOC(FEC_EXA,1) AS Fecha_Examen, DTOC(FEC_REC,1) AS Fecha_Recepcion FROM Laboratorios " +;
							" Where AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB = '" + sCadKey +;
						 	"' INTO CURSOR rsCaseLabs READWRITE"
	 			&sCadSQL
	 			SELECT rsCaseLabs 
	 			COUNT FOR AJUSTE != CLINICAL_DISCARD_ADJUSTMENT  TO nCurrentCaseLabData
	 			
	 			IF nCurrentCaseLabData <= 1 THEN 
	 				
	 			ELSE 
	 				*Se ha encontrado varios registros de laboratorio para el paciente; por tanto, procesa los ajustes de tal forma que se pueda;
	 				establecer el número de activos
		 			sTagExpr = 'AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB+MUESTRA+PRUEBA+AGENTE+Fecha_Examen+Fecha_Recepcion'
		 			DO fastProcessAdjustments WITH "rsCaseLabs", sTagExpr, .F., , "rsAdjustedLabs", .F., .T. IN SIVIGILAUtilities	
		 			SELECT rsCaseLabs		
					COUNT FOR AJUSTE != CLINICAL_DISCARD_ADJUSTMENT  TO nCurrentCaseLabData
				ENDIF 	 			
	 		ENDIF
	 		
	 		IF bSuccess THEN 
	 			IF !bIsUpdating THEN 
	 				*Se prevé agregar un registro nuevo
		 			bSuccess = (!(oCData.IAD_POLIM = '2') OR nCurrentCaseLabData = 0)
		 		ELSE
		 			*Se prevé la actualización de un registro existente
			 		bSuccess = (!(oCData.IAD_POLIM = '2') OR nCurrentCaseLabData <= 1)
		 		ENDIF 
				sMsg = 'Si IAD polimicrobiana es NO, solo se permite el ingreso de un microorganismo'
			ENDIF 

			IF bSuccess THEN 
	 			IF !bIsUpdating THEN 
	 				*Se prevé agregar un registro nuevo
					bSuccess = (!(oCData.TIPO_IAD = '2' AND oCData.IAD_POLIM = '1') OR nCurrentCaseLabData <= 1)
		 		ELSE
		 			*Se prevé la actualización de un registro existente
					bSuccess = (!(oCData.TIPO_IAD = '2' AND oCData.IAD_POLIM = '1') OR nCurrentCaseLabData <= 2)
				ENDIF 
				sMsg = 'Si tipo de IAD = 2 (ISTU-AC) e IAD polimicrobiana es SI, solo se permite el ingreso de dos microorganismos'
			ENDIF 
			
			IF bSuccess THEN 
	 			IF !bIsUpdating THEN 
	 				*Se prevé agregar un registro nuevo
					bSuccess = (!(oCData.TIPO_IAD = '3' AND oCData.IAD_POLIM = '1') OR nCurrentCaseLabData <= 2)
		 		ELSE
		 			*Se prevé la actualización de un registro existente
		 			bSuccess = (!(oCData.TIPO_IAD = '3' AND oCData.IAD_POLIM = '1') OR nCurrentCaseLabData <= 3)
				ENDIF 
				sMsg = 'Si tipo de IAD = 3 (ITS-AC) e IAD polimicrobiana es SI, solo se permite el ingreso de tres microorganismos'
			ENDIF
		
			IF bSuccess THEN 
	 			IF !bIsUpdating THEN 
	 				*Se prevé agregar un registro nuevo
					bSuccess = (!( oCData.TIPO_IAD = '1' AND oCData.CRIT_NAV > '1' AND oCData.IAD_POLIM = '1') OR nCurrentCaseLabData <= 2)
		 		ELSE
		 			*Se prevé la actualización de un registro existente
		 			bSuccess = (!( oCData.TIPO_IAD = '1' AND oCData.CRIT_NAV > '1' AND oCData.IAD_POLIM = '1') OR nCurrentCaseLabData <= 3)
		 		ENDIF
				sMsg = 'Si tipo de IAD = 1 (NAV) y Criterio NAV es NEU2 o NEU3 e IAD polimicrobiana es SI, solo se permite el ingreso de tres microorganismos'
			ENDIF
			
			IF bSuccess THEN
				*bSuccess = (!(oCData.TIPO_IAD = '1' AND oCData.CRIT_NAV>'1') OR (M.MUESTRA = '1 ' AND INLIST(M.PRUEBA,'92','G8','G9','G7','G6','3 ')))
				bSuccess = (!(oCData.TIPO_IAD = '1' AND oCData.CRIT_NAV>'1') OR ( ;
							(M.MUESTRA = '1 ' AND INLIST(M.PRUEBA,'92','G8','G9','G7','G6','3 ')) OR ;
							(M.MUESTRA = '4 ' AND (M.PRUEBA $ "G3,G4")) OR ;
							(M.MUESTRA = '11' AND INLIST(M.PRUEBA,'G1','G2','H2','H1')) OR ;
							(M.MUESTRA = '32' AND M.PRUEBA $ "G5,C7") ;
							))
				sMsg = 'Si Tipo IAD es NAV y Criterio NAV es NEU2 o NEU3 solo se admite ' + CrLf +;
						'MUESTRA=1 (sangre) y como prueba una de las siguientes: ' + CrLf +;
						'92=HEMOCULTIVO, G8=RADIOINMUNOANÁLISIS; G9=INMUNOFLUORESCENCIA (IFA) INDIRECTA, G7=MICROINMUNOFLUORESCENCIA, ' +;
						'G6=TEST DE MICRO INMUNOFLUORESCENCIA PARA CHLAMYDIA o 3=IgG' + CrLf  + CrLf +;
						'MUESTRA=4 (Tejido) y como prueba una de las siguientes:' + CrLf +;
						'(G3) biopsia o (G4) cultivo del parenquima pulmonar.' + CrLf  + CrLf +;
						'MUESTRA=11 (otros liquidos esteriles) y como prueba una de las siguientes:' + CrLf +;
						'G1=CULTIVO DE LÍQUIDO PLEURAL, G2=LAVADO BRONCOALVEOLAR, ' + CrLf +;
						'H2=CEPILLADO PROTEGIDO o H1=LAVADO BRONCO ALVEOLAR  (LBA) PROTEGIDO'+ CrLf  + CrLf +;
						'MUESTRA=32 (Secreciones) y como prueba una de las siguientes:' + CrLf +;
						'(G5) cultivo de secresiones respiratorias o (C7) RT/PCR' 
						
			ENDIF
			
			*IF bSuccess THEN 
			*	bSuccess = (!(oCData.TIPO_IAD = '1' AND oCData.CRIT_NAV>'1') OR  ((M.MUESTRA = '11' AND INLIST(M.PRUEBA,'G1','G2','H2','H1'))))
			*	sMsg = 'Si Tipo IAD es NAV y Criterio NAV es NEU2 o NEU3 solo se admite MUESTRA=11 (otros liquidos esteriles), ' +;
			*			'y como prueba una de las siguientes: G1=CULTIVO DE LÍQUIDO PLEURAL, G2=LAVADO BRONCOALVEOLAR, ' +;
			*			'H2=CEPILLADO PROTEGIDO o H1=LAVADO BRONCO ALVEOLAR  (LBA) PROTEGIDO'
			*ENDIF

			*IF bSuccess THEN 
			*	bSuccess = (!((oCData.TIPO_IAD = '1' AND oCData.CRIT_NAV>'1'))  OR (M.MUESTRA = '4 ' AND (M.PRUEBA $ "G3,G4")))
			*	sMsg = 'Si Tipo IAD es NAV y Criterio NAV es NEU2 o NEU3 solo se admite MUESTRA=4 (Tejido) y como prueba una de las siguientes: (G3) biopsia o (G4) cultivo del parenquima pulmonar.'
			*ENDIF

			*IF bSuccess THEN 
			*	bSuccess = (!(oCData.TIPO_IAD = '1' AND oCData.CRIT_NAV>'1') OR (M.MUESTRA = '32' AND M.PRUEBA $ "G5,C7"))
			*	sMsg = 'Si Tipo IAD es NAV y Criterio NAV es NEU2 o NEU3 solo se admite MUESTRA=32 (Secreciones) y como prueba una de las siguientes: (G5) cultivo de secresiones respiratorias o (C7) RT/PCR'
			*ENDIF

			IF bSuccess THEN 
				bSuccess =  (oCData.TIPO_IAD != '2' OR (M.MUESTRA = '2 ' AND M.PRUEBA = 'G0'))
				sMsg = 'Si Tipo IAD es ISTU-AC solo se admite MUESTRA=2 (Orina) y como prueba (G0) urocultivo'
			ENDIF
			
			IF bSuccess THEN 
				bSuccess =  (oCData.TIPO_IAD != '3' OR (M.MUESTRA = '1 ' AND M.PRUEBA = '92'))
				sMsg = 'Si Tipo IAD es ITSAC solo se admite MUESTRA=1 (Sangre) y como prueba (92) hemocultivo'
			ENDIF
			
			RETURN bSuccess 
			
			IF bSuccess THEN 
				bSuccess =  (!((oCData.TIPO_IAD = '3' AND oCData.CRIT_NAV='2')) OR (M.MUESTRA = '1 ' AND M.PRUEBA = '92'))
				sMsg = 'Si Tipo IAD es ITSAC y el criterio es 2 solo se admite MUESTRA=1 (Sangre) y como prueba (92) hemocultivo y el microorganismo debe ser comensal'
			ENDIF
			
			RETURN bSuccess 
			
		OTHERWISE
			RETURN .T.
	ENDCASE
ENDPROC




PROCEDURE calculate_zScores(nWeight as number, nHeight as Number, sSex as String, nAgeInMonths as Number, aResults)
	*Retorna el aResults los valores de (imc,Zscore Peso/Talla,clasificación peso,Zscore Talla/Edad,Clasificación talla ) ;
	calculados a partir de nWeight, nHeight para el sexo sSex, según fórmulas de la OMS (who anthro)

	*SET STEP ON 
	LOCAL sOldProc as String
	sOldProc = SET("Procedure")
	
	SET PROCEDURE TO 
	SET PROCEDURE TO SIVIGILAUtilities
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE 
	
	DIMENSION aValues(1) as Variant 
	
	*Calcula el IMC
	IF nHeight!=0 THEN
		aResults(1) = nWeight/((nHeight/100)^2)
	ENDIF	
	
	*Calcula el Zscore Peso/Talla. En este caso, se tienen en cuenta el valor de una altura en cm y el valor (y) de un peso en kg ;
	zScore = {[y/M(t)]^L(t)}-1 / (S(t)*L(t)) en donde M(t), L(t) y S(t) están determinados por su valor en la tabla ;
	OMSZscore_PesoTalla correspondiente a la fila con Height = Adjusted(nHeight).  El ajuste a la altura, se hace;
	con base en los valores de alturas disponibles en la tabla
	IF nWeight !=0 THEN
		IF nHeight > 120 THEN
			nValueToSerchFor = 120
		ELSE
			IF (nHeight-FLOOR(nHeight))*10 >= 5 THEN
				nValueToSerchFor = FLOOR(nHeight) + 0.5
			ELSE
				nValueToSerchFor = FLOOR(nHeight) 
			ENDIF
		ENDIF
		zValues = fieldValue('L,M,S,sd3neg,sd2neg,sd1neg,sd0,sd1,sd2,sd3', 'OMSZscore_PesoTalla','SEXO="' + UPPER(sSex) +;
							 '" AND Height=' + ALLTRIM(STR(nValueToSerchFor,5,1)) +;
							 ' AND BETWEEN(' + ALLTRIM(STR(nAgeInMonths)) + ',EDAD_INICIAL,EDAD_FINAL)',,7,5)
		IF !ISNULL(zValues) THEN
			= ALINES(aValues, zValues, 1, ',')
			DIMENSION aResults(3)
			vResult = (((nWeight / VAL(aValues[2])) ^ VAL(aValues[1])) -1) / (VAL(aValues[1])*VAL(aValues[3]))
			aResults(2) = vResult 
			DO CASE
				CASE vResult < -3
					sClasification = '1'
				CASE vResult >= -3 AND vResult < -2
					sClasification = '2'
				CASE vResult >= -2 AND vResult < -1
					sClasification = '3'
				CASE vResult >= -1 AND vResult <= 1
					sClasification = '4'
				CASE vResult > 1 AND vResult <= 2
					sClasification = '5'
				CASE vResult > 2 AND vResult <= 3
					sClasification = '6'
				CASE vResult > 3 
					sClasification = '7'
			ENDCASE
			aResults(3) = sClasification 
		ENDIF
	ENDIF		

	*Calcula el Zscore Talla/Edad. En este caso, sNumerator corresponde al valor de una edad en meses ;
	nHeight al valor (y) de una talla en cm ;
	zScore = {[y/M(t)]^L(t)}-1 / (S(t)*L(t)) en donde M(t), L(t) y S(t) están determinados por su valor en la tabla ;
	OMSZscore_TallaEdad correspondiente a la fila con Month = Adjusted(nAgeInMonths).  El ajuste a la edad, se hace;
	con base en los valores de edades disponibles en la tabla
	IF nHeight !=0 THEN
		IF nAgeInMonths > 60 THEN
			nValueToSerchFor = 60
		ELSE
			nValueToSerchFor = FLOOR(nAgeInMonths)
		ENDIF
		zValues = fieldValue('L,M,S,sd,sd3neg,sd2neg,sd1neg,sd0,sd1,sd2,sd3', 'OMSZscore_TallaEdad','SEXO="' + ;
							UPPER(sSex) + '" AND Month=' + ALLTRIM(STR(nValueToSerchFor,5,1)),,7,5)
		= ALINES(aValues, zValues, 1, ',')
		DIMENSION aResults(5)
		vResult = (((nHeight / VAL(aValues[2])) ^ VAL(aValues[1])) -1) / (VAL(aValues[1])*VAL(aValues[3]))
		aResults(4) = vResult
		
		DO CASE
			CASE vResult < -2
				sClasification = '1'
			CASE vResult >= -2 AND vResult < -1
				sClasification = '2'
			CASE vResult >= -1
				sClasification = '3'
		ENDCASE
		aResults(5) = sClasification 
	ENDIF		

	SET PROCEDURE TO &sOldProc 
ENDPROC 


PROCEDURE calculate_zScores_V02(nWeight as number, nHeight as Number, sSex as String, nAgeInMonths as Number, aResults)
	*Retorna el aResults los valores de (imc,Zscore Peso/Talla,clasificación peso,Zscore Talla/Edad,Clasificación talla ) ;
	calculados a partir de nWeight, nHeight para el sexo sSex, según fórmulas de la OMS (who anthro)

	*SET STEP ON 
	LOCAL sOldProc as String
	sOldProc = SET("Procedure")
	
	SET PROCEDURE TO 
	SET PROCEDURE TO SIVIGILAUtilities
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE 
	
	DIMENSION aValues(1) as Variant 
	
	*Calcula el IMC
	IF nHeight!=0 THEN
		aResults(1) = nWeight/((nHeight/100)^2)
	ENDIF	
	
	*Calcula el Zscore Peso/Talla. En este caso, se tienen en cuenta el valor de una altura en cm y el valor (y) de un peso en kg ;
	zScore = {[y/M(t)]^L(t)}-1 / (S(t)*L(t)) en donde M(t), L(t) y S(t) están determinados por su valor en la tabla ;
	OMSZscore_PesoTalla correspondiente a la fila con Height = Adjusted(nHeight).  El ajuste a la altura, se hace;
	con base en los valores de alturas disponibles en la tabla
	IF nWeight !=0 THEN
		=SelectTable('OMSZscore_PesoTalla',,.F.)
		IF nHeight > 120 THEN
			nValueToSerchFor = 120
		ELSE
			IF (nHeight-FLOOR(nHeight))*10 >= 5 THEN
				nValueToSerchFor = FLOOR(nHeight) + 0.5
			ELSE
				nValueToSerchFor = FLOOR(nHeight) 
			ENDIF
		ENDIF
		*zValues = fieldValue('L,M,S,sd3neg,sd2neg,sd1neg,sd0,sd1,sd2,sd3', 'OMSZscore_PesoTalla','SEXO="' + UPPER(sSex) +;
							 '" AND Height=' + ALLTRIM(STR(nValueToSerchFor,5,1)) +;
							 ' AND BETWEEN(' + ALLTRIM(STR(nAgeInMonths)) + ',EDAD_INICIAL,EDAD_FINAL)',,7,5)
		sSearchedExpr = 'SEXO="' + UPPER(sSex) + '" AND Height=' + ALLTRIM(STR(nValueToSerchFor,5,1)) +;
							 ' AND BETWEEN(' + ALLTRIM(STR(nAgeInMonths)) + ',EDAD_INICIAL,EDAD_FINAL)'
		LOCATE FOR &sSearchedExpr 
		IF FOUND() THEN
			SCATTER FIELDS L,M,S,sd3neg,sd2neg,sd1neg,sd0,sd1,sd2,sd3 TO aValues 
			DIMENSION aResults(3)
			vResult = (((nWeight / aValues[2]) ^ aValues[1]) -1) / (aValues[1]*aValues[3])
			aResults(2) = vResult 
			DO CASE
				CASE vResult < -3
					sClasification = '1'
				CASE vResult >= -3 AND vResult < -2
					sClasification = '2'
				CASE vResult >= -2 AND vResult < -1
					sClasification = '3'
				CASE vResult >= -1 AND vResult <= 1
					sClasification = '4'
				CASE vResult > 1 AND vResult <= 2
					sClasification = '5'
				CASE vResult > 2 AND vResult <= 3
					sClasification = '6'
				CASE vResult > 3 
					sClasification = '7'
			ENDCASE
			aResults(3) = sClasification 
		ENDIF
	ENDIF		

	*Calcula el Zscore Talla/Edad. En este caso, sNumerator corresponde al valor de una edad en meses ;
	nHeight al valor (y) de una talla en cm ;
	zScore = {[y/M(t)]^L(t)}-1 / (S(t)*L(t)) en donde M(t), L(t) y S(t) están determinados por su valor en la tabla ;
	OMSZscore_TallaEdad correspondiente a la fila con Month = Adjusted(nAgeInMonths).  El ajuste a la edad, se hace;
	con base en los valores de edades disponibles en la tabla
	IF nHeight !=0 THEN
		=SelectTable('OMSZscore_TallaEdad',,.F.)

		IF nAgeInMonths > 60 THEN
			nValueToSerchFor = 60
		ELSE
			nValueToSerchFor = FLOOR(nAgeInMonths)
		ENDIF
		*zValues = fieldValue('L,M,S,sd,sd3neg,sd2neg,sd1neg,sd0,sd1,sd2,sd3', 'OMSZscore_TallaEdad','SEXO="' + ;
							UPPER(sSex) + '" AND Month=' + ALLTRIM(STR(nValueToSerchFor,5,1)),,7,5)
		*= ALINES(aValues, zValues, 1, ',')
		sSearchedExpr = 'SEXO="' + UPPER(sSex) + '" AND Month=' + ALLTRIM(STR(nValueToSerchFor,5,1))
		LOCATE FOR &sSearchedExpr 
		IF FOUND() THEN
			SCATTER FIELDS L,M,S,sd,sd3neg,sd2neg,sd1neg,sd0,sd1,sd2,sd3 TO aValues 
			DIMENSION aResults(5)
			vResult = (((nHeight / aValues[2]) ^ aValues[1]) -1) / (aValues[1]*aValues[3])
			aResults(4) = vResult
			
			DO CASE
				CASE vResult < -2
					sClasification = '1'
				CASE vResult >= -2 AND vResult < -1
					sClasification = '2'
				CASE vResult >= -1
					sClasification = '3'
			ENDCASE
			aResults(5) = sClasification 
		ENDIF 
	ENDIF		

	SET PROCEDURE TO &sOldProc 
ENDPROC 


PROCEDURE calculate_zScores_V03(nWeight as number, nHeight as Number, sSex as String, nAgeInMonths as Number, aResults, bSuccess as Boolean)
	*Retorna el aResults los valores de (imc,Zscore Peso/Talla,clasificación peso,Zscore Talla/Edad,Clasificación talla ) ;
	calculados a partir de nWeight, nHeight para el sexo sSex, según fórmulas de la OMS (who anthro).
	*Si no se puede caluclar alguno de los valores, bSuccess será .F.; en caso contrario, será .T.

	*SET STEP ON 
	LOCAL sOldProc as String
	sOldProc = SET("Procedure")
	
	SET PROCEDURE TO 
	SET PROCEDURE TO SIVIGILAUtilities
	SET PROCEDURE TO (PATH_TO_COMMON_LIB + '\TablesHandler') ADDITIVE 
	
	DIMENSION aValues(1) as Variant 
	
	bSuccess = .F. 
	
	*Calcula el IMC
	IF nHeight!=0 THEN
		aResults(1) = nWeight/((nHeight/100)^2)
	ENDIF	
	
	*Calcula el Zscore Peso/Talla. En este caso, se tienen en cuenta el valor de una altura en cm y el valor (y) de un peso en kg ;
	zScore = {[y/M(t)]^L(t)}-1 / (S(t)*L(t)) en donde M(t), L(t) y S(t) están determinados por su valor en la tabla ;
	OMSZscore_PesoTalla correspondiente a la fila con Height = Adjusted(nHeight).  El ajuste a la altura, se hace;
	con base en los valores de alturas disponibles en la tabla
	IF nWeight !=0 THEN
		SELECT rsOMSZscore_PesoTalla
		IF nHeight > 120 THEN
			nValueToSerchFor = 120
		ELSE
			IF (nHeight-FLOOR(nHeight))*10 >= 5 THEN
				nValueToSerchFor = FLOOR(nHeight) + 0.5
			ELSE
				nValueToSerchFor = FLOOR(nHeight) 
			ENDIF
		ENDIF
		sSearchedExpr = 'SEXO="' + UPPER(sSex) + '" AND Height=' + ALLTRIM(STR(nValueToSerchFor,5,1)) +;
							 ' AND BETWEEN(' + ALLTRIM(STR(nAgeInMonths)) + ',EDAD_INICIAL,EDAD_FINAL)'
		LOCATE FOR &sSearchedExpr 
		IF FOUND() THEN
			SCATTER FIELDS L,M,S,sd3neg,sd2neg,sd1neg,sd0,sd1,sd2,sd3 TO aValues 
			DIMENSION aResults(3)
			vResult = (((nWeight / aValues[2]) ^ aValues[1]) -1) / (aValues[1]*aValues[3])
			aResults(2) = vResult 
			DO CASE
				CASE vResult < -3
					sClasification = '1'
				CASE vResult >= -3 AND vResult < -2
					sClasification = '2'
				CASE vResult >= -2 AND vResult < -1
					sClasification = '3'
				CASE vResult >= -1 AND vResult <= 1
					sClasification = '4'
				CASE vResult > 1 AND vResult <= 2
					sClasification = '5'
				CASE vResult > 2 AND vResult <= 3
					sClasification = '6'
				CASE vResult > 3 
					sClasification = '7'
			ENDCASE
			aResults(3) = sClasification 
			
			bSuccess = .T. 
		ENDIF
	ENDIF		

	*Calcula el Zscore Talla/Edad. En este caso, sNumerator corresponde al valor de una edad en meses ;
	nHeight al valor (y) de una talla en cm ;
	zScore = {[y/M(t)]^L(t)}-1 / (S(t)*L(t)) en donde M(t), L(t) y S(t) están determinados por su valor en la tabla ;
	OMSZscore_TallaEdad correspondiente a la fila con Month = Adjusted(nAgeInMonths).  El ajuste a la edad, se hace;
	con base en los valores de edades disponibles en la tabla
	IF bSuccess THEN 
		IF nHeight !=0 THEN
			SELECT rsOMSZscore_TallaEdad

			IF nAgeInMonths > 60 THEN
				nValueToSerchFor = 60
			ELSE
				nValueToSerchFor = FLOOR(nAgeInMonths)
			ENDIF
			*zValues = fieldValue('L,M,S,sd,sd3neg,sd2neg,sd1neg,sd0,sd1,sd2,sd3', 'OMSZscore_TallaEdad','SEXO="' + ;
								UPPER(sSex) + '" AND Month=' + ALLTRIM(STR(nValueToSerchFor,5,1)),,7,5)
			*= ALINES(aValues, zValues, 1, ',')
			sSearchedExpr = 'SEXO="' + UPPER(sSex) + '" AND Month=' + ALLTRIM(STR(nValueToSerchFor,5,1))
			LOCATE FOR &sSearchedExpr 
			IF FOUND() THEN
				SCATTER FIELDS L,M,S,sd,sd3neg,sd2neg,sd1neg,sd0,sd1,sd2,sd3 TO aValues 
				DIMENSION aResults(5)
				vResult = (((nHeight / aValues[2]) ^ aValues[1]) -1) / (aValues[1]*aValues[3])
				aResults(4) = vResult
				
				DO CASE
					CASE vResult < -2
						sClasification = '1'
					CASE vResult >= -2 AND vResult < -1
						sClasification = '2'
					CASE vResult >= -1
						sClasification = '3'
				ENDCASE
				aResults(5) = sClasification 
				
				bSuccess = .T. 
			ELSE
				bSuccess = .F. 
			ENDIF 
		ELSE 
			bSuccess = .F. 
		ENDIF		
	ENDIF
	
	SET PROCEDURE TO &sOldProc 
ENDPROC 

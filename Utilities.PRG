*General purpose utilites

#DEFINE CrLf CHR(13) + CHR(10)


PROCEDURE splitOptions
	
	LPARAMETERS sStr as String, aStr

	*Divide sStr en sus opciones componentes y retorna cada una de ellas en aStr
	*sStr debe ser un string de la forma V1 = descripción1 V2 = descripción2 ... Vm = descripción_m ;
	por ejemplo, "1 = N-Metil Glucamina 2 = Estibogluconato de sodio 3 = Isotianato de pentamidina "
	
	LOCAL nOptions as Integer, nWords as Integer 
	 
	DIMENSION aStr(1,3)
	DIMENSION aStrWords(1)

	IF !EMPTY(sStr) THEN
		sStr=STRTRAN(sStr, " = ","=")
		nWords=ALINES(aStrwords,sStr," ")
		nOptions=0
		FOR iWord=1 TO nWords
			IF OCCURS("=",aStrwords(iWord))=0 THEN
				aStr(nOptions,1)=aStr(nOptions,1)+ " " + aStrwords(iWord)
			ELSE
				nOptions = nOptions + 1
				DIMENSION aStr(nOptions,3)
				aStr(nOptions,1) = aStrwords(iWord)
			ENDIF
		NEXT
		
		FOR iOption=1 TO nOptions
			nEqualSignPosition = AT("=",aStr(iOption,1))
			nOptionsLength = LEN(aStr(iOption,1))
			aStr(iOption,2)=LEFT(aStr(iOption,1),nEqualSignPosition-1)
			aStr(iOption,3)=RIGHT(aStr(iOption,1),nOptionsLength-nEqualSignPosition)
		NEXT
	ENDIF
ENDPROC


FUNCTION splitOptionsTst

	LPARAMETERS sStr as string
		
	DIMENSION aOptions(1)
	
	DO splitOptions WITH sStr , aOptions
	CLEAR
	DISPLAY MEMORY LIKE aOptions*
ENDFUNC


FUNCTION IsNumeric

	LPARAMETERS sExpression as String

	*Retorna True si sExpression corresponde a un número. El único separador de cifras decimales admitido es el '.'; por tanto, por ejemplo, ;
	'12.4' es un número pero '12,4', no lo es.
	
	
	LOCAL sExpressionWithoutLeftZeros as String
	*expresión obtenida al eliminar ceros no significativos a la izquierda de sExpression 
	
	LOCAL sAux as String
	*expresión obtenida al agregar a la derecha de sExpressionWithoutLeftZeros el número necesario de ceros hasta completar 10 decimales. ;
	La expresión obtenida sAux será de longitud <= a 21 caracteres
	 
	LOCAL  nPointPosition as Number, sLength as Number 
	
	
	IF VARTYPE(sExpression )='N' THEN
		RETURN .T.
	ELSE
		*Elimina de sExpression todos los ceros a la izquierda
		sExpressionWithoutLeftZeros=LTRIM(sExpression,1,'0')
		
		*Si sExpression contiene decimales, agrega un cero a la izquierda
		nPointPosition = AT('.',sExpressionWithoutLeftZeros)
		IF nPointPosition > 0 THEN
			IF LEFT(sExpressionWithoutLeftZeros,1) = '.' THEN
				sExpressionWithoutLeftZeros = '0' + sExpressionWithoutLeftZeros
			ENDIF
		ENDIF
		
		sLength=LEN(ALLTRIM(sExpressionWithoutLeftZeros))
		
		*Agrega el número necesario de ceros a la derecha de sExpressionWithoutLeftZeros hasta completar un número de decimales igual 10
		sAux=ALLTRIM(STR(VAL(sExpressionWithoutLeftZeros),21,10))
		
		*Retorna True si la expresión que se obtiene al eliminar a la derecha los ceros no significativos es idéntica a la expresión ;
		obtenida al eliminar ceros no significativos a la izquierda
		RETURN	STUFF(sAux,sLength+1,LEN(sAux)-sLength,'')=ALLTRIM(sExpressionWithoutLeftZeros)
	ENDIF
	
ENDFUNC

FUNCTION IsHexa(sExpression as String)

	*Retorna True si sExpression corresponde a un número hexadecimal. El único separador de cifras admitido es el '-'.

	IF VARTYPE(sExpression)!='C' THEN
		RETURN .F.
	ELSE
		IF EMPTY(sExpression) THEN 
			RETURN .F.
		ELSE
			sExpression = UPPER(ALLTRIM(sExpression ))
			nExpressionLength = LEN(sExpression)
			i=1
			bContinue = .T.
			nExpressionLength = LEN(sExpression)
			DO WHILE bContinue AND i<=nExpressionLength 
				bContinue = (SUBSTR(sExpression,i,1) $ '0123456789ABCDEF-' )
				i = i+1
			ENDDO 
			RETURN	bContinue
		ENDIF
	ENDIF 	
ENDFUNC


FUNCTION  isAlphabetical

	LPARAMETERS sTargetString as String 

	LOCAL bReturnedValue as Boolean 

	bReturnedValue = .F.  && Empty string will return .F. from this code

	FOR lnCharacter = 1 TO LEN(sTargetString)
	   lcChar = SUBSTRC(sTargetString,lnCharacter,1)
	   bReturnedValue = (ISALPHA(lcChar) OR lcChar= ' ')
	   IF !bReturnedValue
	      EXIT
	   ENDIF
	ENDFOR

	RETURN bReturnedValue
ENDFUNC

PROCEDURE ExportMemoFields(sSourceTable as String)
	*sSourceTable (opcional): nombre de la tabla a exportar
	 
	CLOSE ALL

	lcFieldString = ''
	lcMemo = ''

	IF EMPTY(sSourceTable) THEN  	
		USE GETFILE('dbf', 'Select DBF') && Prompts for table to be used.
	ELSE
		USE (sSourceTable)
	ENDIF 

	lnFieldCount = AFIELDS(laGetFields) && Builds array of fields from the
	                                    && selected table.

	*!* Prompt for Output file and use Low-Level functions
	*!* to create it.
	lcTextFile = FCREATE(GETFILE('txt', 'Select Text'))

	sHeaderFileName = (ALIAS()) + '_header'
	COPY TO "&sHeaderFileName" FOR .f. CSV
	
	*!* Starts scanning the table and converts the fields
	*!* values according to their types **
	SCAN
	   WAIT WINDOW STR(RECNO()) + ' Of ' + STR(RECCOUNT()) NOWAIT
	  
	   FOR lnCount = 1 TO lnFieldCount
	      lcType = laGetFields(lnCount, 2)

	      IF lcType # 'G' && Don't try to turn a general field into a string
	         lcString = EVALUATE(laGetFields(lnCount, 1))
	      EndIf
	      
	      DO CASE
	         CASE lcType = 'M' && Process the Memo Fields
	            lnMemoLines = MEMLINES(EVALUATE(laGetFields(lnCount,1)))
	            FOR lnLoop = 1 TO lnMemoLines
	               IF lnLoop < lnMemoLines
	                  lcMemo = lcMemo + ;
	                     ALLTRIM(MLINE(EVALUATE(laGetFields(lnCount, 1)), ;
	                                   lnLoop)) + ' '
	               ELSE
	                  lcMemo = lcMemo + ;
	                     ALLTRIM(MLINE(EVALUATE(laGetFields(lnCount, 1)), ;
	                                   lnLoop))
	               ENDif
	            ENDfor
	           
	            lcString = lcMemo
	            lcMemo = ''
	         CASE lcType = 'G' && Process the General Fields
	            lcString = 'Gen'
	         CASE lcType = 'D' && Process the Date Fields
	            lcString = DTOC(lcString)
	         CASE lcType = 'T' && Process the DateTime Fields
	            lcString = TTOC(lcString)
	         CASE lcType = 'N' && Process the Numeric Fields
	            lcString = STR(lcString, LEN(STR(lcString)), 2)
	         CASE lcType = 'I' && Process the Integer Fields
	            lcString = STR(lcString)
	         CASE lcType = 'L' && Process the Logical Fields
	            IF lcString = .T.
	               lcString = 'T'
	            ELSE
	               lcString = 'F'
	            ENDif
	      ENDcase
	     
	      IF lnCount < lnFieldCount && Determines if the last field was
	                                && processed and sets the closing quote.
	         lcFieldString = lcFieldString + '"' + lcString + '"' + ','
	      ELSE
	         lcFieldString = lcFieldString + '"' + lcString + '"'
	      ENDif
	   ENDfor

	   FPUTS(lcTextFile, lcFieldString) && Writes string to the text file.
	   lcFieldString = ''
	ENDscan

	FCLOSE(lcTextFile)

	CLOSE All
	CLEAR All
	WAIT WINDOW 'Text File Creation Completed' NOWAIT
ENDPROC


FUNCTION isLessThanOrEqual(a AS Number, b AS Number, c AS Number, d AS Number)
	
	*Retorna .T. si el par (a,b) es menor o igual que el par (c,d). (a,b) "<" (c,d) si d>b o si d=b y c>a; por otra parte;
	(a,b) = (c,d) en el sentido matemático usual
	
	bReturnedValue = .F.
	IF d>b THEN
		bReturnedValue=.T.
	ELSE
		IF d=b THEN
			bReturnedValue = (c>=a)
		ENDIF
	ENDIF
	RETURN bReturnedValue
	
ENDFUNC

PROCEDURE Transpose(aSource, aTarget)

	nOldRows = MAX(ALEN(aSource,1),1) 
	nOldColumns = MAX(ALEN(aSource,2),1) 
	
	DIMENSION aTarget(nOldColumns, nOldRows)
	
	FOR nRowCounter = 1 to nOldColumns 
        FOR nColumnCounter = 1 to nOldRows 
                aTarget[nRowCounter, nColumnCounter] = aSource[nColumnCounter, nRowCounter] 
		NEXT nColumnCounter 
	NEXT nRowCounter
   
ENDPROC

PROCEDURE TransposeTst

	DIMENSION aSource(1,5)
	DIMENSION aTarget(1)
	
	aSource(1,1) = '1'
	aSource(1,2) = '2'
	aSource(1,3) = '3'
	aSource(1,4) = '4'
	aSource(1,5) = '5'

	DO Transpose WITH aSource, aTarget
	FOR i=1 TO 5
		?aTarget(i,1)
	NEXT
	
ENDPROC


FUNCTION daysOfMonth(nYear as Number, nMonth as Number )
	x = DATE(nYear, nMonth, 1)
	y = GOMONTH(x,1)
	RETURN  y - x 
ENDFUNC


FUNCTION ageInMonths(cAge as String, cUnitOfMeasure as  string) as Number 
	
	* Retorna en meses la edad dada por cAge y cUnitOfMeasure

	LOCAL nAgeInMinutes as Double

	
	DO CASE
		CASE cUnitOfMeasure = '5'
			nAgeInMinutes = VAL(cAge)
			
		CASE cUnitOfMeasure = '4'
			nAgeInMinutes = VAL(cAge) * 60
			
		CASE cUnitOfMeasure = '3'
			nAgeInMinutes = (VAL(cAge) * 24) * 60

		CASE cUnitOfMeasure = '2'
			nAgeInMinutes = ((VAL(cAge) * 30) *24) * 60

		CASE cUnitOfMeasure = '1'
			nAgeInMinutes = (((VAL(cAge) * 12) * 30) *24) * 60

		OTHERWISE

	ENDCASE
	
	RETURN FLOOR(nAgeInMinutes / 43200)
ENDFUNC

PROCEDURE ageInMonthsTst(cAge as String, cUnitOfMeasure as  string)
	?ageInMonths(cAge, cUnitOfMeasure)
ENDPROC


FUNCTION ageInMonths2(dDateToCompareWith as Date, dBirthDate as Date) as Number 
	
	* Retorna en meses la edad dada por la diferencia entre la fecha dDateToCompareWith y la fecha dBirthDate 

	RETURN FLOOR((dDateToCompareWith - dBirthDate) / 30.42) &&30.42 = 365/12
ENDFUNC

FUNCTION ageInDays(dDateToCompareWith as Date, dBirthDate as Date) as Number 
	
	* Retorna en dias la edad dada por la diferencia entre la fecha dDateToCompareWith y la fecha dBirthDate 

	RETURN FLOOR(dDateToCompareWith - dBirthDate) 
ENDFUNC

PROCEDURE ageInMonths2Tst(dBirthDate as Date)
	*SET STEP ON 
	?ageInMonths2(dBirthDate)
ENDPROC


FUNCTION permutate(sStr as String) as String 
	
	LOCAL sResult as String, sNextChar as String
	LOCAL nStrLength as numeric, nNextCharToSelect as numeric
	
	sResult = ''
	nNextCharToSelect = RAND(-1)
	IF EMPTY(sStr) then
		sResult = sStr
	ELSE
		nStrLength = LEN(sStr)
		FOR i=1 TO nStrLength
			nNextCharToSelect = INT(nStrLength * RAND( ) + 1)
			sNextChar = SUBSTR(sStr,nNextCharToSelect,1)
			sResult = sResult + sNextChar
		NEXT i
	ENDIF
	
	RETURN sResult
ENDFUNC

PROCEDURE permutateFieldValues(sTableName as String, sFieldList as String)
	
	DIMENSION aFieldList(1)

	sCmd = ''
	TRY
		USE &sTableName
	CATCH TO oException
	ENDTRY
	
	sTableName = JUSTFNAME(sTableName)
	SELECT &sTableName
	nFields = ALINES(aFieldList,sFieldList,1+4+8,',')
	FOR i=1 TO nFields
		sCmd = sCmd + aFieldList(i) + ' WITH permutate(ALLTRIM(' + aFieldList(i) + ')), '
	NEXT i
	sCmd = SUBSTR(sCmd,1,LEN(sCmd)-2)
	sCmd = 'REPLACE ' + sCmd + ' ALL'
	glDisableRules = .T.
	&sCmd
	
ENDPROC

*..............................................................................
*   Function: bin2dec
*    Purpose: convert binary string to decimal number            
* Parameters: pbinnum - string to convert i.e.,    
*   '0' - '11111111111111111111111111111111111111111111111111111'
*   '0' - (53 1's)     
*    returns: integer data type      
*      Usage: nresult=bin2dec(cBinString)
*             STORE bin2dec('11111111') TO nMyNum 
*   nMyNum == 255
*		url: https://www.tek-tips.com/faqs.cfm?fid=4461 
*..............................................................................
FUNCTION bin2dec
	PARAMETERS pbinnum
	PRIVATE retval, bindex

	STORE 0 TO retval
	pbinnum = ALLTRIM(pbinnum)
	STORE LEN(pbinnum) TO nDigits
	FOR bindex = 0 TO nDigits
	   IF SUBSTR(pbinnum, nDigits - bindex, 1) = '1'
	      retval = retval + 2^bindex
	   ENDIF
	NEXT
	RETURN INT(retval)
ENDFUNC 

*..............................................................................
*   Function: DEC2BASX
*    Purpose:  Convert whole number 0-?, to base 2-16 
*
* Parameters: nTempNum - number to convert (0-9007199254740992)
*             base    - base to convert to i.e., 2 4 8 16...
*    returns: string
*      Usage:  cresult=Dec2BasX(nParm1, nParm2)
*              STORE Dec2BasX(255, 16) TO cMyString  &&... cMyString contains 'ff'
*		url: https://www.tek-tips.com/faqs.cfm?fid=4461 
*..............................................................................
FUNCTION dec2basx
	PARAMETERS nTempNum, nNewBase

	STORE 0 TO nWorkVal,;
	   remainder,;
	   dividend,;
	   nextnum,;
	   digit

	nWorkVal = nTempNum  
	ret_str = ''

	DO WHILE .T.
	   digit = MOD(nWorkVal, nNewBase)
	   dividend = nWorkVal / nNewBase
	   nWorkVal = INT(dividend)

	   DO CASE
	      CASE digit = 10
	         ret_str = 'a' + ret_str
	      CASE digit = 11
	         ret_str = 'b' + ret_str
	      CASE digit = 12
	         ret_str = 'c' + ret_str
	      CASE digit = 13
	         ret_str = 'd' + ret_str
	      CASE digit = 14
	         ret_str = 'e' + ret_str
	      CASE digit = 15
	         ret_str = 'f' + ret_str
	      OTHERWISE
	         ret_str = LTRIM(STR(digit)) + ret_str
	   ENDCASE

	   IF nWorkVal = 0
	      EXIT
	   ENDIF ( nWorkVal = 0 )
	ENDDO ( .T. )
	RETURN ret_str
ENDFUNC 

*..............................................................................
*   Function: bin2hex  
*    Purpose: convert binary representation of bcd number to hex 
* Parameters: string data type.    123.45 =        
*      '0100000001011110110111001100110011001100110011001100110011001101'      
*       = 0x '405EDD2F1A9FBE77'      
*     Takes each segment of four binary digits and converts to hex digit       
*     Example: 0100 0000 0101 1110.... = 4 0 5 E .....           
*    returns: string data type       
*      Usage: cresult=bin2hex(cBinNum)
*             STORE bin2hex('0100') TO nMyNum 
*   nMyNum == '4'
*		url: https://www.tek-tips.com/faqs.cfm?fid=4461 
*..............................................................................
FUNCTION bin2hex
	PARAMETERS cBinString

	STORE '' TO cHexString
	STORE Len(cBinString) TO nStrLen
	FOR zzz = 1 TO nStrLen STEP 4
	   cHexString = cHexString + ;
	      dec2basx(bin2dec(SUBSTR(cBinString, zzz, 4)), 16)
	NEXT
	RETURN cHexString
ENDFUNC 

*..............................................................................
*   Function: hex2bin
*    Purpose: convert hex bcd number to binary representation of bcd number    
* Parameters: string data type       
*     Takes each hex digit and converts to four binary digits    
*     Example: 4 = 0100, 0 = 0000, 5 = 0101 etc.   
*    returns: string data type       
*      Usage: cresult=hex2bin(cHexString)
*             STORE hex2bin('405EDD2F1A9FBE77') TO nMyNum 
*  nMyNum == '0100000001011110110111010010111100011010100111111011111001110111'
*		url: https://www.tek-tips.com/faqs.cfm?fid=4461 
*..............................................................................
FUNCTION hex2bin
	PARAMETERS cHexString

	STORE '' TO cBinString, cBinString1
	STORE Len(cHexString) TO nStrLen
	FOR zzz = 1 TO nStrLen
	   cBinString = cBinString + ; 
	      Padl(dec2basx(Int(Val('0x' + Substr(cHexString, zzz, 1))), 2), 4, '0')
	NEXT
	RETURN cBinString
ENDFUNC 



FUNCTION  getMyDocumentsDir()

	#DEFINE CSIDL_PERSONAL 0x0005  && My Documents
	
	LOCAL oCommonFolder AS Object, returnedValue

	oCommonFolder = NEWOBJECT("_commonfolder","C:\Program Files (x86)\Microsoft Visual FoxPro 9\Ffc\_system.vcx")

	returnedValue=oCommonFolder.getfolder(CSIDL_PERSONAL)
	RELEASE oCommonFolder
	
	RETURN returnedValue

ENDFUNC


PROCEDURE copyFiles_V01(sTargetPath AS String, sFilesToCopyFileName as String)
	
	*Copia los archivos del directorio actual especificados en el archivo texto sFilesToCopyFileName hacia la carpeta sTargetPath
	
	LOCAL sFileNameToCopy as String, sOldSetSafety as String

	sOldSetSafety = SET("Safety")
	SET SAFETY OFF 
*SET STEP ON 
	sTargetPath = ADDBS(sTargetPath)
	nFileHandler = FOPEN(sFilesToCopyFileName )
	IF nFileHandler > 0
		bContinue = .T.
		DO WHILE bContinue
			sFileNameToCopy = FGETS(nFileHandler)
			IF !EMPTY(sFileNameToCopy)
				sCmd = 'COPY FILE "' + sFileNameToCopy + '" TO "' + (sTargetPath + sFileNameToCopy ) + '"'
				&sCmd
			ELSE
				bContinue = .F.
			ENDIF
		ENDDO
	ENDIF
	FCLOSE(nFileHandler)
	
	SET SAFETY &sOldSetSafety 

ENDPROC

PROCEDURE copyFiles(sTargetPath AS String, sFilesToCopyFileName as String)
	
	*Copia los archivos del directorio actual especificados en el archivo texto sFilesToCopyFileName hacia la carpeta sTargetPath
	
	LOCAL sBatFilenameToRun as String, sBatFileContent as String, sRunCmd as String 

*SET STEP ON 
	*Construye dinámicamente un archivo .bat que pueda ser ejecutado con la instrucción RUN
	sBatFilenameToRun = ADDBS(GETENV("TEMP"))+SYS(2015)+'.BAT'

	* Agrega al archivo .bat, las instrucciones necesarias para copiar los archivos
	sBatFileContent = 'FOR /F %%G IN (' + sFilesToCopyFileName + ') DO (' + CrLf +;
						'copy /y %%G "' + sTargetPath + '")' + CrLf 

	*Para evitar problemas de código de página con el archivo .bat, lo convierte a CodePage 850-International MS-DOS
	sBatFileContent  = CPCONVERT(CPCURRENT(),850,sBatFileContent)
	=STRTOFILE(sBatFileContent, sBatFilenameToRun)
	sRunCmd = 'RUN  ' +  sBatFilenameToRun 
	&sRunCmd
	
	*Borra el archivo .bat recién construido
	DELETE FILE "&sBatFilenameToRun"

ENDPROC

FUNCTION MonthName(nMonth as Byte) as String 

	STORE '' TO sMonth
	
	DO CASE 
		CASE nMonth=1
			sMonth = 'enero'
		CASE nMonth=2
			sMonth = 'febrero'
		CASE nMonth=3
			sMonth = 'marzo'
		CASE nMonth=4
			sMonth = 'abril'
		CASE nMonth=5
			sMonth = 'mayo'
		CASE nMonth=6
			sMonth = 'junio'
		CASE nMonth=7
			sMonth = 'julio'
		CASE nMonth=8
			sMonth = 'agosto'
		CASE nMonth=9
			sMonth = 'septiembre'
		CASE nMonth=10
			sMonth = 'octubre'
		CASE nMonth=11
			sMonth = 'noviembre'
		CASE nMonth=12
			sMonth = 'diciembre'
		OTHERWISE 
	ENDCASE 
	RETURN sMonth
ENDFUNC 


FUNCTION Pull(sList as String, sItem as String ) as String
	
	sLocalList = ',' + sList + ',' 
*SET STEP ON 	
	sLocalList= STRTRAN(sLocalList, ',' + sItem + ',', ',')
	sLocalList= STRTRAN(sLocalList, ',,')
	IF  SUBSTR(sLocalList,1,1) = ',' THEN 
		sLocalList = SUBSTR(sLocalList,2,LEN(sLocalList)-1)
	ENDIF 
	IF  SUBSTR(sLocalList,LEN(sLocalList),1) = ',' THEN 
		sLocalList = SUBSTR(sLocalList,1,LEN(sLocalList)-1)
	ENDIF 
	
	RETURN sLocalList
ENDFUNC 


FUNCTION IsMemberOf( sItem as String, sList as String ) as Boolean
	RETURN (',' + sItem + ',' $ ',' + sList + ',')
ENDFUNC 


FUNCTION ListFromNumbers( sStream as String, sSeparator as Character, nPadLength as Byte) as String 
	*Retorna una lista separada por sSeparator en donde el i-ésimo ítem corresponde a la i-ésima secuencia de números que ;
	existan en sStream, que se supone es un flujo de caracteres que contiene secuencias de digitos decimales mezclados ;
	con otros caracteres. Si sStream es vacío o no contien caracteres distintos a digitios decimales, retorna una lista ;
	vacía.
	
	*nPadLength (in): nose usa en la actual implementación
	
	LOCAL sListItem as String
	sListItem = ''
	
	LOCAL sList as String
	sList = ''
	
	IF !EMPTY(sStream ) THEN 
		nStreamLength = LEN(sStream)
		iChar=1
		DO WHILE  iChar <=  nStreamLength 
			IF ISDIGIT(SUBSTR(sStream,iChar,1)) THEN 
				sListItem = sListItem + SUBSTR(sStream,iChar,1)
			ELSE
				sList = sList + IIF(EMPTY(sListItem ),'',PADR(sListItem,nPadLength,' ') + sSeparator )
				sListItem = ''
				 
			ENDIF 
			iChar = iChar + 1
		ENDDO 
	ENDIF 
	
	RETURN SUBSTR(sList,1,LEN(sList)-1)
ENDFUNC 


PROCEDURE Push(sList as String)
ENDPROC 


FUNCTION ListLength(sList as String)
	RETURN (OCCURS(',',sList)+1)
ENDPROC 

PROCEDURE runBatFile(sCommads as String)

	*Construye dinámicamente un archivo .bat con los comandos sCommads y lo ejecuta
	
	LOCAL sBatFilenameToRun as String, sBatFileContent as String, sRunCmd as String 
	
	sBatFilenameToRun = ADDBS(GETENV("TEMP"))+SYS(2015)+'.BAT'

	* Agrega al archivo .bat, las instrucciones necesarias para copiar los archivos
	sBatFileContent = sCommads 
	
	*Para evitar problemas de código de página con el archivo .bat, lo convierte a CodePage 850-International MS-DOS
	sBatFileContent  = CPCONVERT(CPCURRENT(),850,sBatFileContent)
	=STRTOFILE(sBatFileContent, sBatFilenameToRun)
	sRunCmd = 'RUN  ' +  sBatFilenameToRun 
	&sRunCmd
	
	*Borra el archivo .bat recién construido
	DELETE FILE "&sBatFilenameToRun"

ENDPROC 


PROCEDURE buildLongCalendar()

	TRY
		USE CALENDARIO IN 0
	CATCH TO oException
	ENDTRY 
	SELECT CALENDARIO 
	
	SCAN 
		nDatesToBuild = CALENDARIO.hasta - CALENDARIO.desde 
		FOR iDate=0 TO nDatesToBuild 
			INSERT INTO CALENDARIO2 VALUES (CALENDARIO.vigencia,CALENDARIO.periodo,CALENDARIO.semana,CALENDARIO.desde + iDate)
		ENDFOR 
	ENDSCAN
		
ENDPROC 


FUNCTION getListItem(sList as String, nIndex as numbre ) as String
	
	LOCAL sLocalList as String
	sLocalList  = ',' + sList + ','
	
	RETURN STREXTRACT(sLocalList, ',', ',', nIndex)
ENDFUNC 


FUNCTION weekDistance(sWeek1 as String, sWeek2 as String) as Number 

	*Retorna el número de semanas epidemiológicas que dista entre sWeek1 y  sWeek2. ;
	sWeek(i) i=1,2 es una lista separada por comas en donde el primer item es el número de la semana ;
	y el segundo el número del año. ;
	Se supone que sWeek1_2 <= sWeek1_2, en caso contrario retorna 65535
	
*SET STEP ON 
	sWeek1_1 = VAL(getListItem(sWeek1, 1 ))
	sWeek2_1 = VAL(getListItem(sWeek2, 1 ))
	sWeek1_2 = VAL(getListItem(sWeek1, 2 ))
	sWeek2_2 = VAL(getListItem(sWeek2, 2 ))
	
	IF sWeek1_2 = sWeek2_2
		RETURN ABS(sWeek1_1 - sWeek2_1)
	ELSE
		IF sWeek1_2 <= sWeek2_2
			RETURN (ABS(sWeek1_1 - 53) + ABS(1 - sWeek2_1))
		ELSE
			RETURN 65535
		ENDIF 
	ENDIF
	
ENDFUNC
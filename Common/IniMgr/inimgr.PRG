**********************************************************************
* Program....: IniMgr.prg
* Compiler...: Visual FoxPro 06.00.8492.00 for Windows
* Abstract...: Class Definition for an INI File Manager
**********************************************************************

*** CALLING PROTOTYPE
*** Enables this class to be set up using DO command
LPARAMETERS tcIniFile, tlNoCreate
RELEASE goIniMgr
PUBLIC goIniMgr
goIniMgr = CREATEOBJECT( 'IniMgr', tcIniFile, tlNoCreate )

DEFINE CLASS IniMgr AS RELATION

	*** Property Definitions
	PROTECTED ARRAY aFiles[1]
	PROTECTED cCurIniFile
	PROTECTED lNoCreate

	PROCEDURE Init( tcFileName, tlNoCreate )
		LOCAL lnRetVal
		*** Declare WIN32API Functions (Both Read and Write)
		DECLARE integer GetPrivateProfileString IN Win32API ;
			AS GetIniTxt string,string,string,string,integer,string
		DECLARE integer WritePrivateProfileString IN Win32API ;
			AS SetIniTxt string,string,string,string
		*** Create Internal Collection & Set Properties
		DIMENSION This.aFiles[1]
		This.cCurIniFile = ""
		This.lNoCreate = tlNoCreate
		IF TYPE( "tcFileName" ) = "C" AND ! EMPTY( tcFileName )
			*** If a FileName is passed, Register It and make it Current
			This.RegIniFile( UPPER( ALLTRIM( tcFileName ) ) )
		ELSE
			*** Otherwise Initialise the Array to ""
			This.aFiles[1] = ""
		ENDIF			
	ENDPROC

	********************************************************************
	*** Set a specified Section/Item
	*** Return Values Are Numeric:
	*** 	-1 := Error in Input Params
	***		 0 := Unable to Write to File
	*** 	 1 := Write Succeeded
	********************************************************************
	FUNCTION SetValue( tcSection, tcItem, tcValue, tcIniFile )
		LOCAL lcIniFile, lcItem, lcValue, lcSection, lnRetVal
		*** Check Parameters
		STORE "" TO lcIniFile, lcItem, lcValue, lcSection
		lcIniFile = This.ChkParams( 'FILE', tcIniFile )
		IF EMPTY( lcIniFile )
			*** Invalid File Name specified
			RETURN -1
		ENDIF
		*** Check the Item,Section and Value Names are sensible		
		lcItem = This.ChkParams( 'ITEM', tcItem )
		lcSection = This.ChkParams( 'ITEM', tcSection )
		lcValue = This.ChkParams( 'VALUE', tcValue )
		*** If any are nonsense, Return an Empty String
		IF EMPTY( lcItem ) OR EMPTY( lcSection ) OR EMPTY( tcValue )
			lnRetVal = -1
		ELSE
			*** Write the Data to the specified file
			lnRetVal = SetIniTxt( lcSection, lcItem, lcValue, lcIniFile )
		ENDIF
		RETURN lnRetVal
	ENDFUNC
	
	********************************************************************
	*** Read the Setting for a specified Section/Item
	*** Return Values Are Always Character Strings:
	***		Either the Value or an Empty String is Returned
	********************************************************************
	FUNCTION GetValue( tcSection, tcItem, tcIniFile )
		LOCAL lcIniFile, lcItem, lcSection, lcBuff, lnFound, lcRetVal
		*** Check Parameters
		STORE "" TO lcIniFile, lcTxt, lcItem, lcSection, lcBuff, lcRetVal
		lcIniFile = This.ChkParams( 'FILE', tcIniFile )
		IF EMPTY( lcIniFile )
			*** Invalid File Name specified
			RETURN ""
		ENDIF
		*** Check the Item Name and Section Names are sensible		
		lcItem = This.ChkParams( 'ITEM', tcItem )
		lcSection = This.ChkParams( 'ITEM', tcSection )
		*** If either is nonsense, Return an Empty String
		IF EMPTY( lcItem ) OR EMPTY( lcSection )
			lcRetVal = ""
		ELSE
			*** Set up a Read Buffer and get the Value
			lcBuff = SPACE(2000)
			lnFound = GetIniTxt( lcSection, lcItem, '', @lcBuff, 2000, lcIniFile )
			IF lnFound > 0
				lcRetVal = LEFT( lcBuff, lnFound)
			ELSE
				lcRetVal = ""
			ENDIF
		ENDIF
		RETURN lcRetVal
	ENDFUNC

	********************************************************************
	*** Set the Default Ini File Property to a Specified File
	*** Return Values Are Numeric:
	*** 	-1 := Error in Input Params
	***		 0 := Unable to Register the  File
	*** 	 1 := File is Set as Default
	********************************************************************
	PROCEDURE SetIniFile( tcFileName )
		LOCAL lcTxt, lcFName, lnIdx, lnRetVal
		IF TYPE( "tcFileName" ) = "C" AND ! EMPTY( tcFileName )
			lcFName = This.ChkFileName( tcFileName )
			*** Is the File Registered
			lnIdx = This.IniIdx( lcFName )
			IF lnIdx > 0
				*** Already Registered, so make it Current
				This.cCurIniFile = This.aFiles[lnIdx]
				lnRetVal = 1
			ELSE
				*** Register and make it current
				lnRetVal = IIF( ! This.RegIniFile( lcFname ), 0, 1 )
			ENDIF
		ELSE
			lnRetVal = -1
		ENDIF
		RETURN lnRetVal
	ENDPROC	
	
	********************************************************************
	*** Return the current Default INI File Name
	*** Returns the Fully Qualified File Name of the Current Default File
	********************************************************************
	FUNCTION GetIniFile
		RETURN This.cCurIniFile	
	ENDFUNC	

	********************************************************************
	*** Read the INI file and Populate an array passed by reference 
	*** Return Values Are Numeric:
	*** 	-1 := Error in Input Params 
	***		 0 := Unable to open the file
	*** 	 n := Number of lines read
	********************************************************************
	FUNCTION ReadIniFile( taResArray, tcIniFile )
		LOCAL lcIniFile, lcStr, lcHdr, lcItm, lnHand, lnALen 
		*** Check Parameters
		STORE "" TO lcIniFile, lcStr, lcHdr, lcItm
		STORE 0 TO lnHand, lnALen 
		lcIniFile = This.ChkParams( 'FILE', tcIniFile )
		IF EMPTY( lcIniFile )
			*** Invalid File Name specified
			RETURN -1
		ENDIF	
		IF TYPE( 'taResArray[1]' ) = "U"
			*** Not An Array
			RETURN -1
		ELSE
			*** Clear it
			DIMENSION taResArray[1]
			STORE "" TO taResArray
		ENDIF
		*** Open the file low level
		lnHand = FOPEN( lcIniFile)
		IF lnHand < 0
			RETURN 0
		ENDIF
		*** Read the INI file
		DO WHILE ! FEOF( lnHand )
			lcStr = fgets(lnHand)
			DO CASE
			CASE "[" $ lcStr
				*** This is a Section Header
				lcHdr = ALLTRIM( lcStr )
				lcItm = "[]"
			CASE AT( '=', lcStr ) > 0
				*** This is an entry
				lcHdr = ALLTRIM( LEFT( lcStr, AT('=', lcStr)-1 ) )
				lcItm = ALLTRIM( SUBSTR( lcStr, AT('=', lcStr)+1 ) )
			OTHERWISE
				*** Just ignore it
				STORE "" TO lcHdr, lcItm
				LOOP
			ENDCASE					
			*** Write details to array
			lnALen = ALEN( taResArray, 1)
			IF lnALen = 1 AND EMPTY( taResArray[1,1])
				lnAlen = 1
			ELSE
				lnALen = lnALen + 1
			ENDIF
			DIMENSION taResArray[lnAlen,2]
			taResArray[lnAlen,1] = lcHdr
			taResArray[lnAlen,2] = lcItm
		ENDDO
		llOk = IIF( FCLOSE(lnHand), lnAlen, 0 )
		RETURN llOk
	ENDFUNC

	********************************************************************
	*** Read the array passed by reference Write contents to an INI File
	*** Return Values Are Numeric:
	***		 0 := Error in Input Params 
	*** 	 n := Number of lines written
	********************************************************************
	FUNCTION WriteIniFile( taResArray, tcIniFile )
		LOCAL lcIniFile, lnALen, lnCnt, lcSec, lcItm, lcVal 
		*** Check Parameters
		STORE "" TO lcIniFile, lcSec, lcItm, lcVal
		STORE 0 TO lnALen, lnCnt
		lcIniFile = This.ChkParams( 'FILE', tcIniFile )
		IF EMPTY( lcIniFile )
			*** Invalid File Name specified
			RETURN 0
		ENDIF	
		IF TYPE( 'taResArray[1]' ) = "U"
			*** Not An Array
			RETURN 0
		ELSE
			*** How Many Rows
			lnALen = ALEN( taResArray, 1 )
		ENDIF
		*** Loop Through the Array
		FOR	lnCnt = 1 TO lnAlen
			IF taResArray[lnCnt,2] = "[]"
				*** This is a Header
				lcSec = ALLTRIM(taResArray[lnCnt,1])
				lcItm = ""
				lcVal = ""
			ELSE
				lcItm = ALLTRIM(taResArray[lnCnt,1])
				lcVal = ALLTRIM(taResArray[lnCnt,2])
				lnRetVal=SetIniTxt( lcSec, lcItm, lcVal, lcIniFile )
			ENDIF
		NEXT
		RETURN lnAlen
	ENDFUNC
		
	********************************************************************
	**                                                                **
	***                                                              ***
	**** All Functions below this point are Protected and Internal  ****
	***                                                              ***
	**                                                                **
	********************************************************************
		
	********************************************************************
	*** Register the specified file with the collection and make current
	********************************************************************
	PROTECTED FUNCTION RegIniFile( tcFileName )
		LOCAL llOk, lcFileName
		*** Tidy the File Name
		lcFileName = This.ChkFileName( tcFileName)
		*** Check File exists
		IF FILE( lcFileName )
			*** Add it to the Collection
			This.IniAdd( lcFileName )
			llOk = .T.
		ELSE
			*** Create the Ini File if necessary
			IF This.lNoCreate
				*** No Creation Required
				llOk = .F.
			ELSE
				llOk = This.CreIni( lcFileName )
			ENDIF
		ENDIF
		*** Make the file the Default File if all is OK
		IF llOk
			This.cCurIniFile = lcFileName
		ENDIF
		RETURN llOk
	ENDFUNC

	********************************************************************
	*** Return Index of the specifed File from the Files Collection
	********************************************************************
	PROTECTED FUNCTION IniIdx( tcFileName )
	*** Scan the Files Collection for the FileName
	*** Returns the ROW number if found, 0 if not found
		LOCAL lnElem, lnIdx
		lnIdx = 0
		SET EXACT ON
		*** Scan the array
		lnElem = ASCAN( This.aFiles, tcFileName )
		*** Calculate the Row Number if found
		IF lnElem > 0
			lnIdx = ASUBSCRIPT( This.aFiles, lnElem, 1)
		ENDIF
		SET EXACT OFF
		RETURN lnIdx
	ENDFUNC

	********************************************************************
	*** Add a specified file to the Collection and make it current
	********************************************************************
	PROTECTED PROCEDURE IniAdd( tcFileName )
		LOCAL lnALen
		lnAlen = ALEN( This.aFiles, 1 )
		IF lnALen = 1 AND EMPTY( This.aFiles[1] )
			This.aFiles[1] = tcFileName
		ELSE
			lnALen = lnALen + 1
			DIMENSION This.aFiles[lnALen]
			This.AFiles[lnAlen] = tcFileName
		ENDIF	
		This.cCurIniFile = This.aFiles[lnAlen]
	ENDPROC
	
	********************************************************************
	*** Create a blank INI File with 1 Section named same as the file
	********************************************************************
	PROTECTED FUNCTION CreIni( tcFileName )
		LOCAL llOk, lcFileName, lnHand, lcStr
		*** Trim up file name
		lcFileName = ALLTRIM( tcFileName )
		*** Create and Open the file
		lnHand = FCREATE( lcFileName )
		IF lnHand < 0
			llOk = .F.
		ELSE
			lcStr = "[" + JUSTSTEM( lcFileName ) +"]"
			=FPUTS( lnHand, lcStr )
			llOK = FCLOSE( lnHand )	
		ENDIF
		RETURN llOk
	ENDFUNC

	********************************************************************
	*** Returns Qualified File Name
	*** To run this under VFP prior to V6.0 needs FoxTools,
	*** This method should check for it being loaded, 
	*** and load it if necessary!
	********************************************************************
	PROTECTED FUNCTION ChkFileName( tcFileName )
		LOCAL lcFileName
		*** Tidy up the File Name
		lcFileName = UPPER( ALLTRIM( tcFileName) )
		*** Check that an Extension is specified
		IF EMPTY( JUSTEXT( lcFileName ) )
			*** Nope, so default it to INI
			lcFileName = FORCEEXT( lcFileName, 'INI' )
		ENDIF
		*** Check that path has been specified 
		IF EMPTY( JUSTPATH( lcFileName) )
			*** Nope, so default it to current drive/directory
			lcFileName = FORCEPATH( lcFileName, SYS(5) + CURDIR() )
		ENDIF
		*** Return a Qualified Path Name
		RETURN lcFileName
	ENDFUNC

	********************************************************************
	*** Check Parameters
	********************************************************************
	PROTECTED FUNCTION ChkParams( tcType, tcParam )
		LOCAL lcFName, lcRetVal
		DO CASE
		CASE tcType = "ITEM"
			*** Must be of type Character, Force to Upper Case
			lcRetVal = IIF( TYPE("tcParam")="C" AND !EMPTY(tcParam), UPPER( ALLTRIM( tcParam )), "" )
		CASE tcType = "VALUE"
			*** Must be of type Character, Returned trimmed, 'as is'
			lcRetVal = IIF( TYPE("tcParam")="C" AND !EMPTY(tcParam), ALLTRIM( tcParam ), "" )
		CASE tcType = "FILE"
			IF TYPE( "tcParam" ) = "C" AND ! EMPTY( tcParam )
				*** We've been passed a specific INI File - check it exists
				lcFName = This.ChkFileName( tcParam )
				lcRetVal = IIF( FILE( lcFName ), lcFName, "" )
			ELSE
			 	*** Otherwise use default setting
			 	lcRetVal = This.cCurIniFile
			ENDIF
		OTHERWISE
			lcRetVal = ""
		ENDCASE
		RETURN lcRetVal	
	ENDFUNC
ENDDEFINE

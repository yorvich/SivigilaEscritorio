DEFINE CLASS DirectoryProcessor AS Session 

sDataPath='.\DataDirectoryProcessor'

sDirListingTableName = "DirListing"
nDataSessionId = .null.


PROCEDURE processFile(sFileNameAndPath)
  * Abstract Method to be overridden in subclasses that actually do something
ENDPROC


PROCEDURE SetupDPInitData

	LPARAMETERS sTargetDir as String

	IF VARTYPE(sTargetDir)='C' THEN
		This.sDataPath = sTargetDir 
	ENDIF
	
	This.UseTable(ADDBS(This.sDataPath) + This.sDirListingTableName)
	SELECT (This.sDirListingTableName)
	ZAP
	USE
	USE (This.sDirListingTableName)
	This.nDataSessionId = This.DataSessionId 

ENDPROC


FUNCTION processDirectoryFiles

	LPARAMETERS sTargetDir, sFileSkeleton as String
	
	LOCAL lnPtr, nFileCount, aFileList, sDir, lcFile, sCurDir

	sCurDir = FULLPATH(CURDIR())
	CHDIR (sTargetDir)


	DIMENSION aFileList[1]

	*--- Read the chosen directory.
	IF VARTYPE(sFileSkeleton)!='C' THEN
		sFileSkeleton = '*.*'
	ENDIF
	
	nFileCount = ADIR(aFileList, '*.*', 'D')
	FOR lnPtr = 1 TO nFileCount

	  IF 'D' $ aFileList[lnPtr, 5]
	    *--- Get directory name.
	    sDir = aFileList[lnPtr, 1]

	    *--- Ignore current and parent directory pointers.
	    IF sDir != '.' AND sDir != '..'
	      *--- Call this routine again.
	      THIS.processDirectoryFiles(addbs(sTargetDir)+sDir, sFileSkeleton)
	    ENDIF

	  ELSE
	    *--- Get the Long file name and process it:
	    IF LIKE(sFileSkeleton, aFileList[lnPtr, 1]) THEN
		    THIS.processFile( addbs(sTargetDir)+aFileList[lnPtr, 1])
		  ENDIF
	  ENDIF
	ENDFOR

	*--- Move back to parent directory.
	CHDIR (sCurDir)
	RETURN
ENDFUNC


FUNCTION dirListing

	LPARAMETERS sTargetDir

	*Almacena en This.sDirListingTableName el árbol de directorios NO vacíos que se encuentren a partir de sTargetDir
	
	
	LOCAL lnPtr, nFileCount, aFileList, sDir, lcFile, sCurDir
	LOCAL sInsertSQLCmd AS String 

	sCurDir = FULLPATH(CURDIR())
	CHDIR (sTargetDir)


	DIMENSION aFileList[1]

	*--- Read the chosen directory.
	nFileCount = ADIR(aFileList, '*.*', 'D')
	FOR lnPtr = 1 TO nFileCount
		IF 'D' $ aFileList[lnPtr, 5]
			*--- Get directory name.
		    sDir = aFileList[lnPtr, 1]

		    *--- Ignore current and parent directory pointers.
			IF sDir != '.' AND sDir != '..'
				*--- Call this routine again.
				THIS.dirListing(addbs(sTargetDir)+sDir)
			ENDIF
		ELSE
			SELECT (This.sDirListingTableName )
			LOCATE FOR  PATHNAME == '&sTargetDir'
			IF EOF() THEN
				sInsertSQLCmd = "INSERT INTO " + (This.sDirListingTableName ) + " VALUES ('" + sTargetDir + "', DATE())"
				&sInsertSQLCmd 
			ENDIF
	    ENDIF
	ENDFOR
	
	*--- Move back to parent directory.
	CHDIR (sCurDir)
	RETURN
ENDFUNC


HIDDEN FUNCTION UseTable

	LPARAMETERS sTableNameToUse as String
	
	*Opens sTableNameToUse exclusively in first available workarea without selecting it. ;
	Returns .T. if it was posible to open sTableNameToUse or it was already opened, otherwise returns .F.;
	sTableNameToUse may be specified with or without path.
	
	LOCAL bReturnedValue AS Boolean 
		
	bReturnedValue=.T.
	IF !USED(JUSTFNAME(sTableNameToUse)) THEN
		IF FILE(sTableNameToUse + ".DBF") THEN
			USE (sTableNameToUse) IN 0 EXCLUSIVE 
		ELSE
			bReturnedValue=.F.
		ENDIF
	ENDIF
	RETURN bReturnedValue
ENDFUNC

ENDDEFINE


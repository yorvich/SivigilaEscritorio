FUNCTION IsAppRunning( tcUniqueID AS String )

	LOCAL bRetVal, sUniqueID
	
	*** MUST pass an application ID to this function!
	IF EMPTY(tcUniqueID) OR VARTYPE( tcUniqueID ) # "C"
		RETURN .T.
	ELSE
		*** Strip out any spaces
		sUniqueID = STRTRAN( tcUniqueID, " " )
	ENDIF
	
	*** First check for the existence of the Semaphore window
	IF WEXIST("_Semaphore_")
		RETURN .T.
	ENDIF

	*** Look for an occurrence of this ID as a Window Name
	DECLARE INTEGER FindWindow IN Win32Api AS FindApp String, String
	IF FindApp( NULL, sUniqueID ) > 0
		*** We found one! Set Return Value
		bRetVal = .T.
	ELSE
		*** Create a new window with this ID
		DEFINE WINDOW _Semaphore_ IN DESKTOP FROM 1,1 TO 2,2 TITLE sUniqueID
	ENDIF
	*** Return Status Flag
	RETURN bRetVal	
ENDFUNC

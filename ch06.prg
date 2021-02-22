**********************************************************************
* Custom Header Class
* Compiler...: Visual FoxPro 06.00.8492.00 for Windows
* Abstract...: Allows the header to display tooltip text using the grid's
* ...........: ToolTipText property and allows sorting of the grid by the
* ...........: tag on the current column when it is clicked
**********************************************************************
DEFINE CLASS HdrBase AS Header
	*** Display custom tool tip text
	FUNCTION MouseMove
	LPARAMETERS nButton, nShift, nXCoord, nYCoord
		WITH This
			.Parent.Parent.ToolTipText = .Tag
		ENDWITH			
	ENDFUNC
	*** Re-order the grid by tag (if it exists on this column
	FUNCTION Click
		WITH This.Parent
			IF PEMSTATUS( .Parent, 'SetOrder', 5 )
				.Parent.SetOrder( JUSTEXT( .ControlSource ) )
			ENDIF
		ENDWITH		
	ENDFUNC
ENDDEFINE

**********************************************************************
* Program....: IsTag
* Compiler...: Visual FoxPro 06.00.8492.00 for Windows
* Abstract...: Passed the name of an index tag returns true if it is a
* ...........: tag for the specified table. Uses table in the current
* ...........: work area if no table name is passed. 
**********************************************************************
FUNCTION IsTag( tcTagName, tcTable )
LOCAL lnCnt, llRetVal, lnSelect

IF TYPE( 'tcTagName' ) # 'C'
  *** Error - must pass a Tag Name
  ERROR '9000: Must Pass a Tag Name when calling ISTAG()'
  RETURN .F.
ENDIF

*** Save Work Area Number
lnSelect = SELECT()
IF TYPE('tcTable') = 'C' AND ! EMPTY( tcTable )
  *** If a table specified, select it
  SELECT (tcTable)
ENDIF
*** Check Tags    
FOR lnCnt = 1 TO TAGCOUNT()
  IF UPPER(ALLTRIM( tcTagName ) ) == UPPER( ALLTRIM( TAG( lnCnt ) ) )
    llRetVal = .T.
    EXIT
  ENDIF
NEXT
*** Restore Work Area
SELECT (lnSelect)
*** Return Whether Tag Found
RETURN llRetVal

**********************************************************************
* Program....: GetPKFieldName
* Compiler...: Visual FoxPro 06.00.8492.00 for Windows
* Abstract...: Passed the alias of a data source, it returns the name of the
* ...........: field used as the primary key or candidate key or an empty string
* ...........: if none exists. Assumes the use of surrogate integer PKs for tables
* ...........: in the dbc and surrogate integer CKs for free tables
**********************************************************************
FUNCTION GetPKFieldName( tcAlias )
LOCAL lnCnt, lcFieldName, lnSelect

ASSERT VARTYPE( tcAlias ) = 'C' AND !EMPTY( tcAlias ) ;
	MESSAGE 'Alias name must be passed to GetPKFieldNAme!'

lcFieldName = ''
lnSelect = SELECT()
SELECT ( tcAlias )

*** Find out if we are looking at a view or at a table
IF CURSORGETPROP( 'SourceType', tcAlias ) = 3
	*** We are either looking at a table or a cursor 
	*** Make sure that it at least has a cdx associated with it
	*** Then find the key expression for either the primary or candidate index tag
	IF TAGCOUNT() > 0
		FOR lnCnt = 1 TO TAGCOUNT()
			*** See if we can at least find a candidate index
			IF PRIMARY( lnCnt ) OR CANDIDATE( lnCnt ) 
				*** If we have one, get the key field
				lcFieldName = KEY( lnCnt )
				EXIT
			ENDIF
		ENDFOR			
	ENDIF
ELSE
	*** We have a view
	*** If the KeyFieldList for the view contains several fields,
	*** code must be added to handle this!
	lcFieldName = CURSORGETPROP( 'KeyFieldList', tcAlias )
ENDIF

SELECT ( lnSelect )			

RETURN lcFieldName

**********************************************************************
* Program....: DoSets
* Compiler...: Visual FoxPro 06.00.8492.00 for Windows
* Abstract...: Quickie function to set up the environment
* ...........: Called from BeforeOPenTables method of DE so forms with private
* ...........: datasession have everything set up properly before opening views
**********************************************************************
FUNCTION DoSets

SET SYSFORMATS ON
SET SAFETY OFF
SET MEMOWIDTH TO 120
SET TALK OFF
SET CENTURY ON
SET CENTURY TO 19 ROLLOVER 30
SET MULTILOCKS ON               && For table buffering
SET DELETED ON
SET EXCLUSIVE OFF
SET NOTIFY OFF
SET BELL OFF
SET NEAR OFF
SET EXACT OFF
SET INTENSITY OFF
SET CONFIRM ON
SET STATUS BAR OFF
SET NULLDISPLAY TO 'None'
ENDFUNC

**********************************************************************
* Program....: GetKey
* Compiler...: Visual FoxPro 06.00.8492.00 for Windows
* Abstract...: Get a unique PK value for tables or updateable cursors not in the DBC
* ...........: (therefore can't use Stored Procedure NewID for this)
**********************************************************************
FUNCTION GetKey
LPARAMETER lcTableName
LOCAL lnRetVal, lnOldRepr

lnRetVal = 0

IF NOT USED( "Setup" )
   USE SetUp IN 0
ENDIF
SELECT SetUp
SET ORDER TO TableName
CURSORSETPROP( "Buffering", 1 )		&& set buffering off
SEEK UPPER(lcTableName)
IF NOT FOUND()
   INSERT INTO SetUp ( TableName, Value) VALUES ( lcTableName, 1 )
   lnRetVal = 1
ELSE        
   lnOldRepr = SET( "REPR" )
   SET REPROCESS TO 5 SECONDS
   IF RLOCK()
     ** Check for valid integer value
      IF TYPE( 'SetUp.Value' ) = "N"
         IF SetUp.Value < 2147483646
            lnRetVal = SetUp.Value + 1            
            REPLACE Value WITH lnRetVal
         ENDIF   
      ENDIF 
   ENDIF
   UNLOCK
   SET REPROCESS TO ( lnOldRepr )
ENDIF
USE

RETURN lnRetVal
ENDFUNC

**********************************************************************
* Program....: Str2Exp
* Compiler...: Visual FoxPro 06.00.8492.00 for Windows
* Abstract...: Passed a string and a data type, return the expression
* ...........: after conversion to the specified data type
**********************************************************************
FUNCTION Str2Exp( tcExp, tcType )
*** Convert the passed string to the passed data type
LOCAL luRetVal, lcType

*** Remove double quotes (if any) 
tcExp = STRTRAN( ALLTRIM( tcExp ), CHR( 34 ), "" ) 
*** If no type passed -- display error message
*** the procedure is not clairvoyant
IF TYPE( 'tcType' ) = 'C'
	lcType = UPPER( ALLTRIM( tcType ) )
ELSE
	*** Type is a required parameter. Let the developer know
	ERROR 'Missing Parameter: Expression type is a required parameter to Str2Exp'
ENDIF
*** Convert from Character to type
DO CASE
  CASE INLIST( lcType, 'I', 'N' ) AND INT( VAL( tcExp ) ) == VAL( tcExp ) && Integer
    luRetVal = INT( VAL( tcExp ) )
  CASE INLIST( lcType, 'N', 'Y')                      && Numeric or Currency
    luRetVal = VAL( tcExp )
  CASE INLIST( lcType, 'C', 'M' )                     && Character or memo
    luRetVal = tcExp
  CASE lcType = 'L'                                   && Logical
    luRetVal = IIF( !EMPTY( tcExp ), .T., .F. )
  CASE lcType = 'D'                                   && Date 
    luRetVal = CTOD( tcExp )
  CASE lcType = 'T'                                   && DateTime 
    luRetVal = CTOT( tcExp )
  OTHERWISE
    *** There is no otherwise unless, of course, Visual FoxPro adds
    *** a new data type. In this case, the function must be modified 
ENDCASE
*** Return value as Data Type
RETURN luRetVal


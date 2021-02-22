**********************************************************************
* Program....: ErrorLog.prg
* Compiler...: Visual FoxPro 06.00.8492.00 for Windows
* Abstract...: Simple Error Logging Class
* ...........: ON ERROR GoErrLog.LogError( SET("DATASESSION"), PROGRAM(), LINENO() )
**********************************************************************
#INCLUDE ErrorLog.H

DEFINE CLASS xErrLog AS FormSet

	&&#DEFINE ERROR_AND_SHUTDOWN_SIVIGILA  "shutDownSivigila()"  
	&&#DEFINE MSG_NON_TRAPPED_ERROR  "Se ha producido un error inesperado. El sistema se cerrará."

    *** Set Protected Properties
    PROTECTED FormCount, Visible, DataSession, cAppName, nDSId
    PROTECTED cOldError, cErrText, cErrSce, cErrProg, nErrLine
    PROTECTED cErrCat, cErrTyp, cErrAct
    FormCount   = 0        && Don't allow any forms
    Visible     = .F.      && Keep the Formset invisible
    DataSession = 2        && In a Private Datasession
    nDSId       = 1        && Default to 1
    cAppName    = "SIVIGILA"		&& Application Name
    cLogFile    = ""                && File Name for Err Text File
    cOldError   = ""				&& Error Setting on Entry
    cErrText    = ""				&& Error Message Text
    cErrSce     = ""				&& Source Code 
    cErrProg    = ""				&& Program/Method
    nErrLine    = ""				&& Line Number
    cErrCat     = 'Undetermined'	&& Category
    cErrTyp     = 'UnIdentified'	&& Type
    cErrAct     = 'ShutDown'		&& Action
    *** Exposed Properties
    cNextAction = ""                && Next Action Required
    cUserMsg    = ""                && User Mesage Text
    nErrNum		= 0 				&& Error Number
    cLogTo      = ".\"				&& Path for Err Text File
	cDataPath='.\'					&& DataPath




	****************************************************************
	*** xErrLog::Init()
	*** Get Application Name And error logging path
	*** Open ErrorLog and Message Tables,  
	****************************************************************
    PROCEDURE Init
		SET TALK OFF 
        WITH This
            *** Set Application Name
            IF TYPE("gcAppName") = "C" AND ! EMPTY( gcAppName )
                .cAppName = gcAppName
            ELSE
                .cAppName = "SIVIGILA"
            ENDIF

            *** Set Error Log Path
            IF TYPE("gcErrPath") = "C" AND ! EMPTY( gcErrPath )
                .cLogTo = gcErrPath
            ELSE
                .cLogTo =  FULLPATH( CURDIR() )
            ENDIF

            *** Open Error Category Table in This DS
            IF ! USED( 'ErrMsg' )
                USE ErrMsg AGAIN SHARED IN 0 ALIAS errmsg
            ENDIF
            *** Disable Buffering
            CURSORSETPROP("Buffering", 1, 'errmsg')

            *** Open Error Log Table in This DS
            IF ! USED( 'ErrorLog' )
                USE ErrorLog AGAIN SHARED IN 0 ALIAS errorlog
            ENDIF
            *** Disable Buffering
            CURSORSETPROP("Buffering", 1, 'errorlog')

            *** Save Error Handler's DataSession
            .nDSId = .DataSessionID
            
            *** Set Data Paht
           	setDefaultCmd="SET DEFAULT TO [" + .cDataPath + "]"
			&setDefaultCmd

        ENDWITH    
    ENDPROC
    
	****************************************************************
	*** xErrLog::LogError(tcProgram, tnLineNo) - Controlling Method for Logging Errors
	*** Expects DataSessionID, Program Name and Line Number as parameters
	****************************************************************
    PROCEDURE LogError( tnDSID, tcProgram, tnLineNo )
    	WITH This
            *** Switch to Required DataSession
            SET DATASESSION TO (tnDSId)
    		*** Switch-off error handler to stop recursive calls
	        .SetErrorHandling( "OFF" )
            *** Get Details of the Error
            .GetErrorDetails( tcProgram, tnLineNo )
            *** Create the Error Log Summary Record
			.WriteSummaryRecord()
			*** Write the Error Details to Text File
			.WriteErrorReport()
            *** Now handle the error
            */.HandleError()
            */ No se requiere si se va a tomar la acción desde una tabla
			*** Restore Error Handling on Exit    
    	    .SetErrorHandling( "ON ")
            *** Restore Correct DataSession on Exit
            SET DATASESSION TO (tnDSID)
        ENDWITH
	ENDPROC

	****************************************************************
	*** xErrLog::HandleError 
	*** Method to actually handle the error, depending on Action specified
	****************************************************************
    PROTECTED PROCEDURE HandleError
    LOCAL lcMsgTxt, lcNextAction
	    &&#DEFINE CRLF CHR(13)+CHR(10)
	    WITH This
            DO CASE
                CASE INLIST( .nErrNum, 125, 1643, 1644 )
                    *** 125  Printer is not ready.
                    *** 1643 Printer driver is corrupted.
                    *** 1644 Printer driver is not found.
                    lcMsgTxt = "Windows has reported an error with the specified printer!" ; 
                             + CRLF ;
                             + "Please check that the printer is On-Line, and is ready to print" ;
                             + CRLF ;
                             + "If the problem persists, you may need to contact Technical Support" ;
                             + CRLF ;
                             + "to determine the cause of the problem"
                    lcNextAction = "RETRY"
                CASE .nErrNum = 1589
                    *** 1589  Table or row buffering requires that SET MULTILOCKS is set to ON.
                    SET MULTILOCKS ON
                    *** No message required
                    lcMsgTxt = ""
                    lcNextAction = "RETRY"
                    
                CASE .nErrNum = 1582
                    *** Error de validacion de datos
                    lcMsgTxt = .cUserMsg    
                    lcNextAction = "RETURN TO MASTER"
                    
            	CASE .nErrNum = 1545
            	    *** 1545 Table buffer for alias "name" contains uncommitted changes.
               		*** Get table alias for bad table
            	    LOCAL lnStt, lnEnd, lcTable
                    lnStt   = AT( '"', lcMessage )+1
                    lnEnd   = AT( '"', lcMessage, 2 )-1
                    lcTable = SUBSTR( lcMessage, lnStt, lnEnd - lnStt + 1)
                    *** Revert the table
                    TABLEREVERT(.T., lcTable)
                    *** No message required
                    lcMsgTxt = ""
                    lcNextAction = "RETRY"
                OTHERWISE
                    lcMsgTxt = "An " + .cErrTyp + " Error has occurred in the application and has been recorded"
                    lcNextAction = .cErrAct
            ENDCASE
            *** Populate the Exposed Properties
            .cUserMsg    = lcMsgTxt
            .cNextAction = lcNextAction 
        ENDWITH
	ENDPROC

	****************************************************************
	*** xErrLog::WriteDetails()
	*** Generates Error Text
	****************************************************************
    PROTECTED PROCEDURE WriteDetails()
        WITH This
         	? '********* NEW ERROR: ' + TTOC( DATETIME() ) + ' *******************'
        	? 'Error : ' + PADL( .nErrNum, 4) + ': ' + .cErrText
        	? '        at line ' + ALLTRIM( PADL( .nErrLine, 10 ) ) + ' in ' + .cErrProg
        	? 'Source Code: ' + .cErrSce
        	?
        	? 'User  : ' + SYS(0)
        	? '================================================================'
            ? 'Category: ' + .cErrCat
            ? 'Type:     ' + .cErrTyp
            ? 'Action:   ' + .cErrAct 
        	? '****************************************************************'
            ?
        	? '[1] *** Memory dump: LIST MEMORY'
        	LIST MEMORY LIKE *
        	? '================================================================'
        	?
        	? '[2] *** Data Dump'
        	?
        	LOCAL ARRAY laUsed[1]
            LOCAL lnCnt, lnRec, lnFld, lcFVal, lcStat, lcProgPath, lnSelect
        	lnSelect = SELECT()
        	FOR lnCnt = 1 TO AUSED(laUsed)
        		SELECT (laUsed[lnCnt,2])  && work area no.
        		? 'Table: ' + DBF() + ': Alias: ' + ALIAS()
        		? REPLICATE('-',69)
                DO CASE
                    CASE RECCOUNT() < 1
                        *** Empty Table
                        lcStat = "Table has no records"
                    CASE BOF()
                        *** At BOF()
                        lcStat = "At BOF()"
                    CASE EOF()
                        *** At EOF()
                        lcStat = "At EOF()"
                    OTHERWISE
                        *** Has Valid Records
                		lnRec = RECNO()
        		        lcStat = SPACE(5) + 'Record #: ' + ALLTRIM( PADL( lnRec, 10 ) )
                        *** What is the record state
                		IF CURSORGETPROP( 'Buffering' ) > 1 ;
                		        AND ( '3' $ GETFLDSTATE(-1) OR '4' $ GETFLDSTATE(-1) )
        		        	lcStat = lcStat + SPACE(5) + '  ** APPENDED **'
        		        ENDIF
		                IF CURSORGETPROP('Buffering')>1 AND GETFLDSTATE(0)%2 = 0
        		        	IF DELETED()
        		        		lcStat = lcStat + SPACE(5) + '  ** DELETED **'
        		        	ELSE
        		        		lcStat = lcStat + SPACE(5) + '  ** UNDELETED **'
        		        	ENDIF
        		        ENDIF
			    ENDCASE
			    ? lcStat
			    ?
		        FOR lnFld = 1 TO FCOUNT()
		        	lcFVal = .ConvToStr( EVAL( FIELD(lnFld) ))
                    lcStat = SPACE(7)
        			IF CURSORGETPROP('Buffering')>1
		        		IF INLIST( GETFLDSTATE( lnFld ), 2, 4)
		        		    *** User Edited this Field
		                    lcStat = SPACE(5) + "* "
                       ENDIF
                    ENDIF
                    *** Field Name
                    lcStat = lcStat + PADR( FIELD(lnFld) + ' ', 25, '.' )
                    *** Field Type
		        	lcStat = lcStat + "(" + TYPE( FIELD(lnFld) ) + ")"
                    *** Field Value
                    lcStat = lcStat + " " + lcFVal
                    ? lcStat
		        NEXT
		        ?
        	NEXT
        	? '================================================================'
        	?
        	? '[3] *** Calling Chain'
            ?
        	lnCnt = 1
        	lcProgPath =""
        	DO WHILE !EMPTY( PROGRAM( lnCnt ))
        		lcProgPath = lcProgPath + PROGRAM(lnCnt) + CHR(13)+CHR(10)
        		lnCnt = lnCnt + 1
        	ENDDO
        	? lcProgPath
        	? '================================================================'
            ?
        	? '[4] *** Clipboard contents'
        	? _CLIPTEXT
        	? '================================================================'
        	?
        	? '[5] *** Object dump: LIST OBJECTS'
        	LIST OBJECTS
        	? '================================================================'
        	?
        	? '[6] *** Data Session status: LIST STATUS'
        	LIST STATUS
        	? '================================================================'
        	? CHR(13) + CHR(10)

 		ENDWITH
	ENDPROC	

	****************************************************************
	*** xErrLog::WriteErrorReport()
	*** Creates the Error Details Text File
	****************************************************************
    PROTECTED PROCEDURE WriteErrorReport()
        WITH This
            *** Direct Output to logfile
            lcLogName = .cLogFile
        	SET ALTERNATE TO &lcLogName ADDITIVE
        	SET ALTERNATE ON
        	SET CONSOLE OFF
            *** Write Details
            .WriteDetails()
            *** Turn Logging Off
            SET CONSOLE ON
        	SET ALTERNATE OFF
        	SET ALTERNATE TO
		ENDWITH
	ENDPROC	

	****************************************************************
	*** xErrLog::WriteSummaryRecord() 
	*** Writes a record to the ErrorLog Table
	****************************************************************
    PROTECTED PROCEDURE WriteSummaryRecord()
        
        WITH This
            LOCAL lnDSId
            *** Save Working DataSession
            lnDSId = SET("DATASESSION")
            *** Switch to Error Handler's Datasession
            SET DATASESSION TO (.nDSId)
            *** Add Record to Error log and Get Generated PK
            APPEND BLANK IN errorlog
            lnErrId = errorlog.logsid
			*** Account for embedded spaces in the path!
	        *.cLogFile =  '[' + ADDBS( .cLogTo ) + 'E' + PADL( lnErrId, 7, "0" ) + '.TXT]'
	        .cLogFile = '[' + ADDBS( .cLogTo ) + .getErrFileName() + ']'
            *** Insert the Error Summary Record
            REPLACE logdtime   WITH DATETIME(), ; 
                    logerrnum  WITH .nErrNum, ;
                    logerrprog WITH .cErrProg, ;
                    logerrline WITH .nErrLine, ;
                    logerruser WITH SYS(0), ; 
                    logerrtxt  WITH .cLogFile ;
                    IN errorlog
            *** Restore Working DataSession
            SET DATASESSION TO (lnDSId)
		ENDWITH
	ENDPROC	

	****************************************************************
	*** xErrLog::GetErrorDetails(tcProgram, tnLineNo) 
	*** Populate Object Properties with details of the last error
	*** Expects Program Name and Line Number as parameters
	****************************************************************
    PROTECTED PROCEDURE GetErrorDetails( tcProgram, tnLineNo )
        WITH This
            LOCAL lnDSId
            *** Retrieve Details of the last Error
            .nErrNum   = ERROR()
            .cErrText  = MESSAGE()
            .cErrSce   = MESSAGE(1)
            .cErrProg  = IIF( !EMPTY(tcProgram),  tcProgram, '<unknown>' )
            .nErrLine  = IIF( !EMPTY(tnLineno),   tnLineno,  0 )

            *** Save Working DataSession
            lnDSId = SET("DATASESSION")
            *** Switch to Error Handler's Datasession
            SET DATASESSION TO (.nDSId)
            *** Look up the error in the message table
            IF SEEK( .nErrNum, 'ErrMsg', 'ErrNum' )
                .cErrCat = ALLTRIM( ErrMsg.ErrCat )
                .cErrTyp = ALLTRIM( ErrMsg.ErrType )
                .cNextAction = ALLTRIM(EVALUATE(ErrMsg.ErrAction))
                .cUserMsg = ALLTRIM( ErrMsg.ErrText )
            ELSE
                .cErrCat = 'Undetermined'
                .cErrTyp = 'UnIdentified'
                .cErrAct = 'ShutDown'
                .cNextAction = ERROR_AND_SHUTDOWN_SIVIGILA
                .cUserMsg = MSG_NON_TRAPPED_ERROR
            ENDIF
            *** Restore Working DataSession
            SET DATASESSION TO (lnDSId)
        ENDWITH
    ENDPROC
    
	****************************************************************
	*** xErrLog::SetErrorHandling( tcHow ) 
	*** Save and restore Error Handling
	*** Expects ON or OFF as parameters
	****************************************************************
    PROTECTED PROCEDURE SetErrorHandling( tcHow )
	LOCAL lcSetTo
		WITH This
			IF tcHow = "OFF"
			    *** Save Error Handler
				.cOldError = ON("ERROR")
				*** Disable further handling
				ON ERROR *
			ELSE
			    *** Retrieve original setting
				lcSetTo = .cOldError
				*** Restore Error Handling
				ON ERROR &lcSetTo
			ENDIF
		ENDWITH
	ENDPRO

	****************************************************************
	*** xErrLog::resetErrorNo
	*** Resets modified values default values
	*** 
	****************************************************************
    PROCEDURE resetErrorLog()
	    
	    WITH This
		    .cErrCat     = ''	&& Category
		    .cErrTyp     = ''	&& Type
	    	.cErrAct     = ''	&& Action
	        .cNextAction = ''
	        .cUserMsg    = ''
            .nErrNum   = 0
            .cErrText  = '' 
            .cErrSce   = '' 
            .cErrProg  = ''
            .nErrLine  = 0
		ENDWITH
	ENDPRO


    PROTECTED FUNCTION ConvToStr( tuExp, tcType )
		LOCAL lcRetVal, lcType
		*** If no type passed -- map to expression type
		lcType = IIF( VARTYPE( tcType )='C', UPPER(ALLTRIM(tcType)), VARTYPE( tuExp ) )
		*** Convert from type to char
		DO CASE
			CASE ISNULL( tuExp )
				lcRetVal = "NULL"
	 		CASE INLIST(lcType, 'I', 'N') AND INT( tuExp ) = tuExp && Integer
				lcRetVal = ALLTRIM(STR(tuExp,16,0))
			CASE INLIST(lcType, 'N', 'Y')	&& Numeric or Currency
				lcRetVal = ALLTRIM(PADL(tuExp,32))
			CASE lcType = 'C'  && Character
				lcRetVal = ALLTRIM( tuExp )
			CASE lcType = 'L'  && Logical
				lcRetVal = IIF(!EMPTY( tuExp ), '.T.', '.F.')
			CASE lcType = 'D'  && Date 
				lcRetVal = ALLTRIM( DTOC( tuExp ) )
			CASE lcType = 'T'  && DateTime 
				lcRetVal = ALLTRIM( TTOC( tuExp ) )
		ENDCASE
		*** Return value as character
		RETURN lcRetVal
	ENDFUNC
	
	PROTECTED FUNCTION getErrFileName
		RETURN 'ERROR_' + STRTRAN(TTOC(DATETIME(),3),":","_") + '.TXT'
	ENDFUNC
	
ENDDEFINE
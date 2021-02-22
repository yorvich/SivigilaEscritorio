#INCLUDE DevEnvironment.h

DEFINE CLASS AutomationServer as Custom 

cServerName = .NULL.
oServer = .NULL.
sErrorMsg = ''

HIDDEN oCurrentWorksheet as Object 

FUNCTION Init

	LPARAMETERS cServerName, lCreateAtInit

	LOCAL oRegistry

	IF VarType(cServerName) = "C"
		* Check to see if this is a valid server name
		oRegistry = NewObject("Registry",PATH_TO_VFP_FOUNDATION_CLASSES + "Registry")
		IF oRegistry.IsKey(cServerName)
			This.cServerName = cServerName
		ELSE
			RETURN .F.
		ENDIF
		RELEASE oRegistry
	ELSE
		* No point in creating this object
		RETURN .F.
	ENDIF

	IF VarType(lCreateAtInit) = "L" AND lCreateAtInit
		This.OpenServer()
	ENDIF
	RETURN
ENDFUNC

FUNCTION Destroy
	This.oServer.DisplayAlerts = 0
	This.oServer.Quit
ENDFUNC


PROCEDURE OpenServer()

	* Open the server specified by This.cServerName
	* Store the new instance to This.oServer

	IF VARTYPE(This.cServerName) = "C"
		* Should check here that the server name is valid on this machine
		This.oServer = CreateObject(This.cServerName)
	ENDIF

	RETURN
ENDPROC 


PROCEDURE OpenXLSFile(sXLSFileNameAndPath as string)
	
	TRY
		This.oServer.Workbooks.Open(sXLSFileNameAndPath)
		This.oCurrentWorksheet = This.oServer.Workbooks(1).Worksheets(1)
	CATCH TO oException
		This.sErrorMsg = oException.Message
	ENDTRY
ENDPROC


PROCEDURE CloseXLSFile(vSourceWorkbook as Variant, bSave as Boolean, sSaveAs as String)

	This.oServer.DisplayAlerts = 0
	IF bSave THEN
		This.oServer.Workbooks(vSourceWorkbook).Close(bSave, sSaveAs)
	ELSE
		This.oServer.Workbooks(vSourceWorkbook).Close(bSave)
	ENDIF
	This.oServer.DisplayAlerts = -1
	
ENDPROC


PROCEDURE CleanWorksheet(vSourceWorkbook as Variant, vWorksheet as Variant)

	LOCAL oRange as Object 

	IF VARTYPE(vSourceWorkbook)='C' OR VARTYPE(vSourceWorkbook)='N' AND ;
		VARTYPE(vWorksheet)='C' OR VARTYPE(vWorksheet)='N' THEN
		
		oRange = This.oServer.Workbooks(vSourceWorkbook).Worksheets(vWorksheet).Cells
		oRange.ClearContents
	ENDIF
	
ENDPROC


PROCEDURE NameRange(vSourceWorkbook as Variant, vWorksheet as Variant, sRangeName as String)

	LOCAL oRange as Object 

	IF VARTYPE(vSourceWorkbook)='C' OR VARTYPE(vSourceWorkbook)='N' AND ;
		VARTYPE(vWorksheet)='C' OR VARTYPE(vWorksheet)='N' THEN
		
		This.oServer.Workbooks(vSourceWorkbook).Activate
		This.oServer.ActiveWorkbook.Sheets(vWorksheet).Select
		This.oServer.Range("A1").Select
		oRange = This.oServer.Range(This.oServer.Selection, This.oServer.ActiveCell.SpecialCells(11)).Cells
		sRangeSpecification = "=" + vWorksheet + "!R" + ALLTRIM(STR(oRange.Row)) + "C" + ALLTRIM(STR(oRange.Column)) + ":R" + ALLTRIM(STR(oRange.Rows.Count)) + "C" + ALLTRIM(STR(oRange.Columns.Count))
		This.oServer.ActiveWorkbook.Names.Add(sRangeName,"A1:A1")
		This.oServer.ActiveWorkbook.Names.Item(sRangeName).RefersToR1C1 = sRangeSpecification 
	ENDIF
	RELEASE oRange	
		
ENDPROC


PROCEDURE CopyCells(vSourceWorkbook as Variant, vSourceWorksheet as Variant, vTargetWorkbook as Variant, vTargetWorksheet as Variant)

	LOCAL oSourceRange as Object 

	IF (VARTYPE(vSourceWorkbook)='C' OR VARTYPE(vSourceWorkbook)='N') AND ;
		(VARTYPE(vSourceWorksheet)='C' OR VARTYPE(vSourceWorksheet)='N') AND ;
		(VARTYPE(vTargetWorksheet)='C' OR VARTYPE(vTargetWorksheet)='N') AND ;
		(VARTYPE(vTargetWorkbook)='C' OR VARTYPE(vTargetWorkbook)='N') THEN
		
		This.oServer.Workbooks(vSourceWorkbook).Activate
		This.oServer.ActiveWorkbook.Sheets(vSourceWorksheet).Select
		This.oServer.Range("A1").Select
		This.oServer.Range(This.oServer.Selection, This.oServer.ActiveCell.SpecialCells(11)).Select		&& 11 = xlLastCell
		This.oServer.Selection.Copy
		
		This.oServer.Workbooks(vTargetWorkbook).Activate
		This.oServer.ActiveWorkbook.Worksheets(vTargetWorksheet).Select
		This.oServer.Range("A1").Select
		This.oServer.ActiveSheet.Paste
	ENDIF
	
ENDPROC


PROCEDURE UpdatePivotTable(vSourceWorkbook as Variant, vSourceWorksheet as Variant, sPivotTableName as String)

	IF (VARTYPE(vSourceWorkbook)='C' OR VARTYPE(vSourceWorkbook)='N') AND ;
		(VARTYPE(vSourceWorksheet)='C' OR VARTYPE(vSourceWorksheet)='N') THEN
		
		This.oServer.Workbooks(vSourceWorkbook).Activate
		This.oServer.ActiveWorkbook.Sheets(vSourceWorksheet).Select
		
		This.oServer.ActiveWorkbook.ActiveSheet.PivotTables(sPivotTableName).PivotCache.Refresh
	ENDIF

ENDPROC


PROCEDURE makeMeVisible()
	This.oServer.Visible=.T.
ENDPROC

PROCEDURE makeMeInvisible()
	This.oServer.Visible=.F.
ENDPROC


PROCEDURE VFP2Excel(tcCursorName, toRange, tcHeaders, tnPrefferredWidthForMemo)
	* Author: Cetin Basoz
    
    * tcCursorName: name of the cursor to be exported. If ommitted, the cursor to export will be the current selected workarea
    * toRange: an excel range from wich to render data
    * tcHeaders: Optional. Defaults for field headers if you don't want to use the recordset headers
    * tnPrefferredWidthForMemo: Optional. Default 80
    
    tcCursorName = Evl(m.tcCursorName,Alias())
    tnPrefferredWidthForMemo = Evl(m.tnPrefferredWidthForMemo,80)
    
    Local loConn As AdoDB.Connection, loRS As AdoDB.Recordset,;
        lcTemp,lcTempDb, oExcel,ix, lcFieldName, lcHeaders

    lnSelect = Select()
    lcTemp   = Forcepath(Sys(2015)+'.dbf',Sys(2023))
    lcTempDb = Forcepath(Sys(2015)+'.dbc',Sys(2023))

    Create Database (m.lcTempDb)
    Select * From (m.tcCursorName) Into Table (m.lcTemp) Database (m.lcTempDb)

    Local Array aMemo[1]
    Local nMemoCount
    nMemoCount = 0
    lcHeaders = ''
    For ix = 1 To Fcount()
        lcFieldName = Field(m.ix)
        If Type(Field(m.ix))='M'
            nMemoCount = m.nMemoCount + 1
            Dimension aMemo[m.nMemoCount]
            aMemo[m.nMemoCount] = m.ix
            Replace All &lcFieldName With Chrtran(&lcFieldName,Chr(13)+Chr(10),Chr(10))
        Endif
        lcHeaders = m.lcHeaders + Iif(Empty(m.lcHeaders),'',',')+Proper(m.lcFieldName)
    Endfor
    tcHeaders = Evl(m.tcHeaders,m.lcHeaders)

    Use In (Juststem(m.lcTemp))
    Close Databases
    Set Database To

    loStream = Createobject('AdoDb.Stream')
    loConn = Createobject('ADODB.Connection')
    loRS = Createobject("ADODB.Recordset")
    loConn.ConnectionString = "Provider=VFPOLEDB;Data Source="+m.lcTempDb
    loConn.Open()
    loRS = loConn.Execute("select * from "+m.lcTemp)
    loRS.Save( loStream )
    loRS.Close
    loConn.Close
    Erase (m.lcTemp)

    * Use first row for headers
    Local Array aHeader[1]

    loRS.Open( loStream )
    toRange.Offset(1,0).CopyFromRecordSet( loRS )  && Copy data starting from headerrow + 1

    Set Safety Off
    Delete Database (m.lcTempDb) Deletetables

    Select (m.lnSelect)

    For ix=1 To Iif( !Empty(m.tcHeaders), ALINES(aHeader, m.tcHeaders,1,','), loRS.Fields.Count )
        toRange.Offset(0,m.ix-1).Value = Iif( !Empty(m.tcHeaders), aHeader[m.ix], Proper(loRS.Fields(m.ix-1).Name) )
        toRange.Offset(0,m.ix-1).Font.Bold = .T.
    Endfor

    #Define xlJustify                                         -4130
    #Define xlTop                                             -4160
    * This part is cosmetic
    toRange.WorkSheet.Activate
    With toRange.WorkSheet.UsedRange
        .VerticalAlignment = xlTop && set all to top
        For ix=1 To m.nMemoCount
            With .Columns(aMemo[m.ix])
                .ColumnWidth = m.tnPrefferredWidthForMemo && 80 chars width
                .WrapText = .T.
            Endwith
        Endfor
        .Columns.AutoFit
        .Rows.AutoFit
    Endwith
ENDPROC


HIDDEN FUNCTION _GetChar
	* Returns A, AA, BC etc notation for nth column
	
    Lparameters tnColumn && Convert tnvalue to Excel alpha notation
    
    If m.tnColumn = 0
        Return ""
    Endif
    If m.tnColumn <= 26
        Return Chr(Asc("A")-1+m.tnColumn)
    Else
        Return  _GetChar(Int(Iif(m.tnColumn % 26 = 0,m.tnColumn - 1, m.tnColumn) / 26)) + ;
            _GetChar((m.tnColumn-1)%26+1)
    Endif
ENDFUNC


ENDDEFINE
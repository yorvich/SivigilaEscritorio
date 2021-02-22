Attribute VB_Name = "AuditTrailer"
Option Compare Database
Option Explicit


Const sAUDIT_TABLE_NAME As String = "tbHistorialCambios"

Dim sSQLCmd As String

Sub RecordChange(ByVal sSourceTableName As String, ByVal sSourceFieldName As String, ByVal vRecordId, _
                  ByVal vOldValue, ByVal vNewValue, Optional ByVal sCurrentUser As String)

On Error GoTo ErrorHandler
   
   If sCurrentUser = "" Then
      sCurrentUser = CurrentUser()
   End If
   
   sSQLCmd = "INSERT INTO " + sAUDIT_TABLE_NAME + " (Usuario, IdRegistro, TablaFuente, CampoFuente," _
            + "ValorAnterior, ValorNuevo,FechaCambio) VALUES ('" + sCurrentUser + "',"
   Select Case VarType(vRecordId)
      Case vbInteger, vbLong, vbSingle, vbDouble, vbCurrency, vbDecimal, vbByte
         sSQLCmd = sSQLCmd + "'" + Trim(str(vRecordId)) + "'"
      Case vbDate
         sSQLCmd = sSQLCmd + "'" + Trim(CStr(vRecordId)) + "'"
      Case vbString
         sSQLCmd = sSQLCmd + "'" + vRecordId + "'"
   End Select
   sSQLCmd = sSQLCmd + ",'" + sSourceTableName + "','" + sSourceFieldName + "',"
   
   Select Case VarType(vOldValue)
      Case vbInteger, vbLong, vbSingle, vbDouble, vbCurrency, vbDecimal, vbByte
         sSQLCmd = sSQLCmd + "'" + Trim(str(vOldValue)) + "'"
      Case vbDate
         sSQLCmd = sSQLCmd + "'" + Trim(CStr(vOldValue)) + "'"
      Case vbString
         sSQLCmd = sSQLCmd + "'" + vOldValue + "'"
      Case vbNull
         sSQLCmd = sSQLCmd + "''"
   End Select
   sSQLCmd = sSQLCmd + ","
   
   Select Case VarType(vNewValue)
      Case vbInteger, vbLong, vbSingle, vbDouble, vbCurrency, vbDecimal, vbByte
         sSQLCmd = sSQLCmd + "'" + Trim(str(vNewValue)) + "'"
      Case vbDate
         sSQLCmd = sSQLCmd + "'" + Trim(CStr(vNewValue)) + "'"
      Case vbString
         sSQLCmd = sSQLCmd + "'" + vNewValue + "'"
      Case vbNull
         sSQLCmd = sSQLCmd + "''"
   End Select
   sSQLCmd = sSQLCmd + ", Date())"
   DoCmd.SetWarnings False
   DoCmd.RunSQL sSQLCmd
   DoCmd.SetWarnings True

ExitSub:
   Exit Sub
   
ErrorHandler:
   MsgBox Err.Description, , "Error:" & Err.Number
   Resume ExitSub
End Sub


Sub RecordFrmRecordChange(oSourceForm As Form, oRecorIdControl As control)

On Error GoTo ErrorHandler
   
   Dim oControl As control
   Dim sSourceFieldName As String, sSourceTableName As String
   Dim oRSForm As DAO.Recordset
   Dim vCurrentValue, vOldValue
   
   Set oRSForm = oSourceForm.Recordset
   
   For Each oControl In oSourceForm.Controls
      With oControl
         'Avoid labels and other controls with Value property.
         If .ControlType = acTextBox Then
            vCurrentValue = IIf(IsNull(.Value), "", .Value)
            vOldValue = IIf(IsNull(.OldValue), "", .OldValue)
            If vCurrentValue <> vOldValue Then
               sSourceFieldName = .ControlSource
               sSourceTableName = oRSForm.Fields(sSourceFieldName).SourceTable
               
               RecordChange sSourceTableName, sSourceFieldName, oRecorIdControl.Value, .OldValue, .Value, CurrentUser()
            End If
         End If
      End With
   Next
   Set oControl = Nothing
   Set oRSForm = Nothing
ExitSub:
   Exit Sub
   
ErrorHandler:
   MsgBox Err.Description, , "Error:" & Err.Number
   Resume ExitSub

End Sub

Function auditTrailTst()
 RecordFrmRecordChange Forms(0), Forms(0).Controls("cod_eve")
End Function


Sub ReverseChange(ByVal nTrailId, ByVal sRecordIdFieldName As String)

   'Reversa el cambio identificado con nTrailId  en la tabla de control de cambios.

   'sRecordIdFieldName es el nombre del campo que identifica unívocamente registros en la tabla _
   a la que pertenecía el campo que fue objeto de modificación.
   
   'Teniendo en cuenta que pueden existir varios cambios aplicados a un mismo campo a través de la _
   historia, solamente se reversará el cambio más reciente; en caso de que el cambio identificado por _
   nTrailId no coincida con el último cronológicamente registrado, se informará mediante un mensaje.
   
On Error GoTo ErrorHandler
   
   Dim oRSTrailToReverse As Recordset, oRSTargetTable As Recordset, oRSTrails As Recordset
   Dim oTargetDB As Database
   Dim sSourceFieldName As String, sSourceTableName As String
   Dim vOldValue
   Dim vRecToRestoreId
   
   Set oTargetDB = CurrentDb()
   
   'Recupera la información detallada del cambio a reversar
   sSQLCmd = "SELECT * FROM " + sAUDIT_TABLE_NAME + " WHERE ID=" + Trim(str(IIf(IsNull(nTrailId), 0, nTrailId)))
   Set oRSTrailToReverse = oTargetDB.OpenRecordset(sSQLCmd, dbOpenSnapshot)
   
   If oRSTrailToReverse.RecordCount > 0 Then
   
      'Determina si es posible reversar el cambio identificado con nTrailId en la tabla de control de cambios. _
      Solamente se puede reversar el último cambio registrado en la tabla de control de cambios
      sSQLCmd = "SELECT * FROM " + sAUDIT_TABLE_NAME + " WHERE TablaFuente='" + oRSTrailToReverse!TablaFuente _
               + "' AND CampoFuente='" + oRSTrailToReverse!CampoFuente + "' ORDER BY FechaCambio DESC"
      Set oRSTrails = oTargetDB.OpenRecordset(sSQLCmd, dbOpenSnapshot)
      
      If oRSTrails!Id = oRSTrailToReverse!Id Then
         'El cambio se puede reversar
         
         'Determina cuál fue la tabla y campo al que se le aplicó el cambio que se pretende reversar
         sSourceTableName = oRSTrailToReverse!TablaFuente
         Set oRSTargetTable = oTargetDB.OpenRecordset(sSourceTableName, dbOpenSnapshot)
         
         sSourceFieldName = oRSTrailToReverse!CampoFuente
         vOldValue = oRSTrailToReverse!ValorAnterior
         vRecToRestoreId = oRSTrailToReverse!IDregistro
         
         'Construye una consulta de actualización que reversará el cambio teniendo en cuenta el tipo de datos _
         del campo sSourceFieldName al que se le efectuó y el tipo de datos del campo sRecordIdFieldName _
         que identifica unívocamente registros en la tabla a la que pertenece dicho campo
         sSQLCmd = "UPDATE " + sSourceTableName + " SET " + sSourceFieldName + "="
         Select Case VarType(oRSTargetTable.Fields(sSourceFieldName))
            Case vbInteger, vbLong, vbSingle, vbDouble, vbCurrency, vbDecimal, vbByte
               sSQLCmd = sSQLCmd + "Val(" + vOldValue + ")"
            Case vbDate
               sSQLCmd = sSQLCmd + "CDate('" + vOldValue + "')"
            Case vbString
               sSQLCmd = sSQLCmd + "'" + vOldValue + "'"
            Case vbNull
               sSQLCmd = sSQLCmd + "''"
         End Select
         sSQLCmd = sSQLCmd + " WHERE " + sRecordIdFieldName + " = "
         
         Select Case VarType(oRSTargetTable.Fields(sRecordIdFieldName))
            Case vbInteger, vbLong, vbSingle, vbDouble, vbCurrency, vbDecimal, vbByte
               sSQLCmd = sSQLCmd + vRecToRestoreId
            Case vbDate
               sSQLCmd = sSQLCmd + "CDate(vRecToRestoreId)"
            Case vbString
               sSQLCmd = sSQLCmd + "'" + vRecToRestoreId + "'"
            Case vbNull
               sSQLCmd = sSQLCmd + "''"
         End Select
         oTargetDB.Execute sSQLCmd, dbFailOnError
         
         sSQLCmd = "DELETE * FROM " + sAUDIT_TABLE_NAME + " WHERE ID=" + Trim(str(nTrailId))
         oTargetDB.Execute sSQLCmd, dbFailOnError
      Else
         MsgBox "Solamente se puede reversar el último cambio cronológico registrado.", , ""
      End If
   End If
   Set oRSTrailToReverse = Nothing
   Set oRSTrails = Nothing
   Set oTargetDB = Nothing
   Set oRSTargetTable = Nothing
   
ExitSub:
   Exit Sub
   
ErrorHandler:
   MsgBox Err.Description, , "Error:" & Err.Number
   Resume ExitSub
End Sub


Function reverseChangetst()
   ReverseChange 2, "ID"
End Function

LOCAL oGetServ

SET STEP ON 

oGetServ = NewObject("AutomationServer", "AutomationServer.prg", "",  "Excel.Application", .T.)
IF VarType(oGetServ) = "O"
	oExcel = oGetServ.oServer
	IF VarType(oExcel)<>"O"
		MessageBox("Can't create Excel automation object")
	ELSE
		oGetServ.OpenXLSFile('C:\Users\wilson.aguilar\Documents\SIVIGILA - 2015\ARCHIVOS XLS\352Notificaciones.xls')
		
		oGetServ.OpenXLSFile('C:\Users\wilson.aguilar\Documents\ProyectoSivigila\Desarrollo\evento 352 InformeNHINSS.xlsx')
		oGetServ.CleanWorksheet('evento 352 InformeNHINSS.xlsx', 1)
		*oGetServ.CleanWorksheet('erradicacion_control_internal_2')

		oGetServ.CopyCells('352Notificaciones.xls',1,'evento 352 InformeNHINSS.xlsx','DatosNotificacion')
		oGetServ.CloseXLSFile('352Notificaciones.xls')
		
		*oGetServ.makeMeVisible()

		oGetServ.NameRange('evento 352 InformeNHINSS.xlsx','DatosNotificacion','Notificaciones')
		oGetServ.UpdatePivotTable('evento 352 InformeNHINSS.xlsx','Estadisticas','InformeNHSN_NNISS')

		oGetServ.CloseXLSFile('evento 352 InformeNHINSS.xlsx',.T.)
	ENDIF
ENDIF

RELEASE oGetServ
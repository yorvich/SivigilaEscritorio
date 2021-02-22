DEFINE CLASS FileAndPathHandler as Custom

sSelectedPath = ''
*Ruta seleccionada incluyendo el último "\"

sSelectedFile = ''
*Archivo seleccionado incluyendo toda la ruta

sCurrentSelection = ''
*Si es F, se ha seleccionado un archivo, si es P, se ha seleccionado una carpeta; si es empty, no se ha seleccionado nada


PROCEDURE selectPath(sMessage as String)

	this.sSelectedPath = GETDIR('.',sMessage,'',16+32+64)
	IF !EMPTY(this.sSelectedPath) THEN
		this.sCurrentSelection = 'P'
	ENDIF

ENDPROC

PROCEDURE selectFile(sMessage as String, sFileExtensions as String)

	this.sSelectedFile = GETFILE(sFileExtensions,'','',0,sMessage)
	IF !EMPTY(this.sSelectedFile) THEN
		this.sCurrentSelection = 'F'
	ENDIF

ENDPROC

ENDDEFINE

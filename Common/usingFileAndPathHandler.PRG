oFileAndPathHandler = NEWOBJECT("FileAndPathHandler","FileAndPathHandler.Prg",.null.)

oFileAndPathHandler.selectPath('Selecciones la ruta en donde se encuentran los archivos objetivo')
MESSAGEBOX("Ruta seleccionada: " + oFileAndPathHandler.sSelectedPath,0+64)

oFileAndPathHandler.selectFile('Selecciones un archivo objetivo','prg')
MESSAGEBOX("Archivo seleccionado: " + oFileAndPathHandler.sSelectedFile,0+64)

RELEASE oFileAndPathHandler 





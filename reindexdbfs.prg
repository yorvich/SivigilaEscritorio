LPARAMETERS pathToDBFFiles as String

currentDir=CURDIR()
IF !EMPTY(pathToDBFFiles) THEN
	SET DEFAULT TO (pathToDBFFiles)
ENDIF

nDBFFiles=ADIR(DBFFiles,'DBF')
FOR nFile=1 TO nDBFFiles
	IF USED(DBFFiles(nFile,1)) THEN
		SELECT DBFFiles(nFile,1)
	ELSE
		USE DBFFiles(nFile,1)
	ENDIF
	REINDEX
NEXT
SET DEFAULT TO (currentDir)
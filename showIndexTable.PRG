LPARAMETERS nTables

SET DEVICE TO FILE tableIndexes
CLOSE ALL

currentLine=1
FOR event=1 TO nTables
	targetTableName='EVENTOS_' + PADL(event,2,'0')
	USE (targetTableName)
	@currentLine,0 SAY 'TableName :' + (targetTableName)
	currentLine=currentLine+1
	indexTags=ATAGINFO(tableIndexesInfo)
	FOR j=1 TO indexTags
		@currentLine,0 SAY tableIndexesInfo(j,1) + ':	' + tableIndexesInfo(j,3)
		currentLine=currentLine+1
	ENDFOR
	USE
	currentLine=currentLine+2
NEXT

SET DEVICE TO SCREEN

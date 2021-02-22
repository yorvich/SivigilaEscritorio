FUNCTION getPosibleEventClassificationTst

	LPARAMETERS eventCode as string
	
	SET PROCEDURE TO BusinessRules
	WAIT getPosibleEventClassification(eventCode) WINDOW 
	SET PROCEDURE TO
	
ENDFUNC
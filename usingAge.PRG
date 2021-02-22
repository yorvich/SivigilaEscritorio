LPARAMETERS dBirthDate as Date

LOCAL sMeasureUnit as String, sAge as String 

oAge= NEWOBJECT( "AgeCalculator", "age.fxp")
oAge.CalculateAge(dBirthDate, DATE())
IF oAge.nYears>0 THEN
	sMeasureUnit='Años'
	sAge =ALLTRIM(STR(oAge.nYears))
ELSE
	IF oAge.nMonths>0 THEN
		sMeasureUnit='Meses'
		sAge =ALLTRIM(STR(oAge.nMonths))
	ELSE
		sAge =ALLTRIM(STR(oAge.nDays))
		IF oAge.nDays>0 THEN
			sMeasureUnit='Días'
		ELSE
			sMeasureUnit=''
		ENDIF
	ENDIF		
ENDIF
MESSAGEBOX("La edad es " + sAge  + " medida en " + sMeasureUnit,0+48)
RELEASE oAge

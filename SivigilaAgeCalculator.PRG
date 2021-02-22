DEFINE CLASS SivigilaAgeCalculator as ageCalculator

	cAge = ""
	cUnitOfMeasure = ""
	
	FUNCTION SivigilaCalculateAge
		LPARAMETERS dDate1 AS Date, dDate2 AS Date
		* Calcula la edad en años, meses o días entre dDate2 y dDate1
		
		THIS.cUnitOfMeasure=''
		THIS.cAge = ''
		This.CalculateAge(dDate1, dDate2)
				
		IF This.nYears > 0 THEN
			THIS.cUnitOfMeasure = '1'
			THIS.cAge = ALLTRIM(STR(This.nYears))
		ELSE
			IF This.nMonths > 0 THEN
				THIS.cUnitOfMeasure = '2'
				THIS.cAge=ALLTRIM(STR(This.nMonths))
			ELSE
				IF NOT ISNULL(This.nDays)
					THIS.cAge=ALLTRIM(STR(This.nDays))
					IF This.nDays>0 THEN
						THIS.cUnitOfMeasure = '3'
					ENDIF
				ENDIF
			ENDIF		
		ENDIF	
	ENDFUNC	

ENDDEFINE

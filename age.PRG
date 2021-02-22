*==============================================================================
* Program:        AGE.PRG
* Author:         Rick Borup
* Date Written:   10/29/2004
* Copyright:      (c) 2004 Information Technology Associates
*                     All rights reserved.
* Compiler:       Visual FoxPro 08.00.0000.3117 for Windows
* Abstract:       A class to calculate age in years, months, and days		
* Comments:       Use as follows:
*                    ox = NEWOBJECT( "AgeCalculator", "age.vcx")
*                    ox.CalculateAge( dDate1, dDate2)
*                 Results are stored in ox.nYears, ox.nMonths, and ox.nDays.
*
*                 See the accompanying 'readme' file for more information.
*
* Limitations:    This class uses VFP's GOMONTH() function, which works
*                 for dates in the range 01-Jan-1753 through 31-Dec-9999.
*                 This class returns NULL for dates earlier than 01-Jan-1753,
*                 or if the calculation could not be completed for any other
*                 reason. 
*
*                 The maximum allowable difference between the two dates 
*                 is 999 years, 11 months.
*
* Terms of Use:   This software is distributed "AS IS". There is NO WARRANTY of 
*                 any kind. The author will not be liable for damages of any kind,
*                 including but not limited to indirect or consequential damages, 
*                 resulting from the use of this software. Use at your own risk. 
*                 Check your own results.
*
* Revisions:      04-Jan-2005 - This release features a complete replacement of the
*                 core logic using custom methods to handle the calculation of 'next 
*                 month' and 'next year' and gives more accurate results when the 
*                 date range starts on day 29, 30, or 31 and spans several months 
*                 or years.
*==============================================================================

*---------------------------------------
DEFINE CLASS AgeCalculator AS Custom
*---------------------------------------
nYears = NULL
nMonths = NULL
nDays = NULL

*--------------------------------------------------------------------
*   AgeCalculator :: CalculateAge
*------------------------------------
*   Function: Calculate the age between two dates in years, months,
*             and days.
*       Pass: tdDate1 - the starting date
*             tdDate2 - the ending date (defaults to DATE())
*     Return: .T. if successful, otherwise NULL
*   Comments: Results are stored in this.nYears, this.nMonths, and 
*             this.nDays.
*--------------------------------------------------------------------
FUNCTION CalculateAge( tdDate1, tdDate2)
*	Initialize result properties.
WITH this
	.nYears = NULL		
	.nMonths = NULL
	.nDays = NULL
ENDWITH

*	Validate parameters.
IF PCOUNT() = 0                 	&& If we didn't get a date,
	RETURN NULL              		&& return NULL.
ENDIF
IF VARTYPE( tdDate1) <> "D"		&& If 1st parameter is not a date
	RETURN NULL              		&& return NULL.
ENDIF
IF EMPTY( tdDate1)					&& If it's a bad date
	RETURN NULL              		&& return NULL.
ENDIF
IF PCOUNT() = 1						&& If we only got one date,
	tdDate2 = DATE()					&& then use today's date for date2.
ENDIF
IF VARTYPE( tdDate2) <> "D"		&& If 2nd parameter is not a date
	RETURN NULL              		&& return NULL.
ENDIF
IF tdDate1 > tdDate2					&& If date1 > date 2,
	RETURN NULL              		&& return NULL.
ENDIF
IF tdDate1 < {^1753/01/01} OR tdDate2 < {^1753/01/01}	&& If either date is before 01-Jan-1753
	RETURN NULL              		&& return NULL.
ENDIF

*	Declare and initialize local memvars.
LOCAL ldWorkingDate
ldWorkingDate = tdDate1                 && A work field to increment the date.

WITH this
	.nYears = 0
	.nMonths = 0
	.nDays = 0

	*	Go as far as we can by year
	LOCAL ldNextYear
	DO WHILE .T.
		ldNextYear = .NextYear( ldWorkingDate, tdDate1)
		IF ldNextYear <= tdDate2
			.nYears = .nYears + 1
			ldWorkingDate = ldNextYear
		ELSE
			EXIT
		ENDIF
	ENDDO

	*	Then go as far as we can by month
	LOCAL ldNextMonth
	DO WHILE .T.
		ldNextMonth = .NextMonth( ldWorkingDate, tdDate1)
		IF ldNextMonth <= tdDate2
			.nMonths = .nMonths + 1
			ldWorkingDate = ldNextMonth
		ELSE
			EXIT
		ENDIF
	ENDDO

	*	Finish up with days
	.nDays = tdDate2 - ldWorkingDate

ENDWITH

RETURN .T.
ENDFUNC && CalculateAge

*--------------------------------------------------------------------
*   AgeCalculator :: NextYear
*------------------------------------
*   Function: Calculate same date next year
*       Pass: tdDate         - the starting date
*             tdOriginalDate - the original starting date, if any
*     Return: Date
*   Comments: If original starting date is Feb. 29, this function
*             returns Feb. 29 for every leap year in the series.
*--------------------------------------------------------------------
PROTECTED FUNCTION NextYear( tdDate, tdOriginalDate)
IF EMPTY( tdOriginalDate)
	tdOriginalDate = tdDate
ENDIF
LOCAL ldNextYear
ldNextYear = GOMONTH( tdDate, 12)
IF MONTH( ldNextYear) = 02 AND DAY( ldNextYear) = 28 AND ;
	DAY( tdOriginalDate) = 29 AND this.IsLeapYear( YEAR( ldNextYear))
	ldNextYear = ldNextYear + 1
ENDIF
RETURN ldNextYear
ENDFUNC && NextYear

*--------------------------------------------------------------------
*   AgeCalculator :: NextMonth
*------------------------------------
*   Function: Calculate the same date next month
*       Pass: tdDate         - the starting date
*             tdOriginalDate - the original starting date, if any
*     Return: Date
*   Comments: If day of month of original starting month is 29, 30,
*             or 31, this function returns the original starting day
*             or as close to it as possible for the new month.
*--------------------------------------------------------------------
PROTECTED FUNCTION NextMonth( tdDate, tdOriginalDate)
IF EMPTY( tdOriginalDate)
	tdOriginalDate = tdDate
ENDIF
LOCAL ldNextMonth, lnDay
ldNextMonth = GOMONTH( tdDate, 1)
IF DAY( tdOriginalDate) >= 29
	lnDay = MIN( DAY( tdOriginalDate), this.DaysInMonth( ldNextMonth))
	ldNextMonth = DATE( YEAR( ldNextMonth), MONTH( ldNextMonth), lnDay)
ENDIF
RETURN ldNextMonth
ENDFUNC && NextMonth

*--------------------------------------------------------------------
*   AgeCalculator :: DaysInMonth
*------------------------------------
*   Function: Calculate the number of days in a given month
*       Pass: tdDate - the date
*     Return: Numeric
*   Comments: Because the parameter is a complete date, this function
*             handles leap years correctly, too.
*--------------------------------------------------------------------
PROTECTED FUNCTION DaysInMonth( tdDate)
RETURN DAY( GOMONTH( tdDate - DAY( tdDate) + 1, 1) - 1)
ENDFUNC && DaysInMonth

*--------------------------------------------------------------------
*   AgeCalculator :: IsLeapYear
*------------------------------------
*   Function: Determine is a year is a leap year
*       Pass: tnYear - the year
*     Return: Logical
*   Comments: In VFP, DATE( yyyy, 02, 29) returns the empty date if
*             the year 'yyyy' is not a leap year.
*--------------------------------------------------------------------
PROTECTED FUNCTION IsLeapYear( tnYear)
RETURN NOT EMPTY( DATE( tnYear, 02, 29))
ENDFUNC && IsLeapYear

ENDDEFINE && AgeCalculator

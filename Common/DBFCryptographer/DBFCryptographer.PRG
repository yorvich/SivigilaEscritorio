#DEFINE N_BYTES_TO_FIDDLE 32

DEFINE CLASS DBFCryptographer AS Session 

*Date: 29/07/2013
*Version: 1.0
*Author: Wilson E. Aguilar
*Company: Instituto Nacional de Salud

* This class provides methods to "encrypt" and "unencrypt" the DBF header of a table

* The vfp table header record structure is described in ;
http://www.foxite.com/archives/encryption-of-dbf-header-0000098256.htm

* Table Header Record Structure
* Byte offset Description
* 0 File type:
* 0x02 FoxBASE
* 0x03 FoxBASE+/Dbase III plus, no memo
* 0x30 Visual FoxPro
* 0x31 Visual FoxPro, autoincrement enabled
* 0x43 dBASE IV SQL table files, no memo
* 0x63 dBASE IV SQL system files, no memo
* 0x83 FoxBASE+/dBASE III PLUS, with memo
* 0x8B dBASE IV with memo
* 0xCB dBASE IV SQL table files, with memo
* 0xF5 FoxPro 2.x (or earlier) with memo
* 0xFB FoxBASE
* 1 - 3 Last update (YYMMDD)
* 4 - 7 Number of records in file
* 8 - 9 Position of first data record
* 10 - 11 Length of one data record, including delete flag
* 12 - 27 Reserved
* 28 Table flags:
* 0x01 file has a structural .cdx
* 0x02 file has a Memo field
* 0x04 file is a database (.dbc)
* This byte can contain the sum of any of the above values. For example, the value 0x03 indicates 
* the table has a structural .cdx and a Memo field.
* 29 Code page mark
* 30 - 31 Reserved, contains 0x00
* 32 - n Field subrecords, see Field Subrecords Structure below
* The number of fields determines the number of field subrecords. One field subrecord exists for 
* each field in the table.
* n+1 Header record terminator (0x0D)
* The following is only true for Visual Foxpro tables.
* n+2 to n+264 A 263-byte range that contains the backlink, which is the relative path of an associated
* database (.dbc) file, information. If the first *byte is 0x00, the file is not associated
* with a database. Therefore, database files always contain 0x00.
******************************************************************************************
* Field Subrecords Structure
* Byte offset Description
* 0 - 10 Field name with a maximum of 10 characters. If less than 10, 
* it is padded with null characters (0x00).
* 11 Field type: 
* C – Character
* Y – Currency Visual Foxpro
* N – Numeric Visual Foxpro
* F – Float Visual Foxpro
* D – Date
* T – DateTime Visual Foxpro
* B – Double Visual Foxpro
* I – Integer Visual Foxpro
* L – Logical
* M – Memo
* G – General Visual Foxpro
* C – Character (binary) Visual Foxpro
* M – Memo (binary) Visual Foxpro
* P – Picture Visual Foxpro
* 12 - 15 Displacement of field in record
* 16 Length of field (in bytes)
* 17 Number of decimal places
* 18 Field flags: Visual Foxpro
* 0x01 System Column (not visible to user)
* 0x02 Column can store null values
* 0x04 Binary column (for CHAR and MEMO only) 
* 0x06 (0x02+0x04) When a field is NULL and binary (Integer, Currency, and Character/Memo fields)
* 0x0C Column is autoincrementing
* 19 - 22 Value of autoincrement Next value Visual Foxpro 8 and higher
* 23 Value of autoincrement Step value Visual Foxpro 8 and higher
* 24 - 31 Reserved
*
************************************************************************************** 


sTargetFileNameAndPath = .NULL.
*FileNameAndPath of the table to "encrypt" or "unencrypt"


PROCEDURE encryptFile

	*Table´s dbf header encryption is based on the following function: ;
		f(x) = x + 10 si 0<=x<=245 ;
		f(x) = -x + 255 si 245<x<=255 ;
	where x is the ascii code of a character belonging to the header
	
	*It can be shown that this function has inverse defined as ;
		f(x) = x - 10 si 10<=x<=255 ;
		f(x) = -x + 255 si 0<=x<10
	
	
	LOCAL nHndl as Number
	LOCAL sDBFHeaderString as String, sFiddledDBFHeaderString as String 
	LOCAL sCharToFiddle as String, nCharToFiddle as Integer 
	 
	IF FILE(This.sTargetFileNameAndPath) THEN
		nHndl = FOPEN(This.sTargetFileNameAndPath ,12)
		IF nHndl!=-1 THEN
			*Read the first record
			sDBFHeaderString = FREAD(nHndl,N_BYTES_TO_FIDDLE)
		 
			*'Fiddle the first record adding 10 to the ASC() of each byte
			sFiddledDBFHeaderString = ""
			FOR i = 1 to LEN(sDBFHeaderString)
				sCharToFiddle = SUBSTR(sDBFHeaderString,i,1)
				nCharToFiddle = ASC(sCharToFiddle )
				IF  BETWEEN(nCharToFiddle,0,245) THEN
					sFiddledDBFHeaderString = sFiddledDBFHeaderString + CHR(nCharToFiddle  + 10)
				ELSE
					sFiddledDBFHeaderString = sFiddledDBFHeaderString + CHR(255 - nCharToFiddle )
				ENDIF
			ENDFOR

			*go back to the top of the file with and replace the first record with the codified string.
			FSEEK(nHndl,0,0)
			FWRITE(nHndl,sFiddledDBFHeaderString)

			*Close the file
			FCLOSE(nHndl)
		ENDIF
	ENDIF
ENDPROC


PROCEDURE decryptFile

	*Table´s dbf header decryption is based on the following function: ;
		f(x) = x + 10 si 0<=x<=245 ;
		f(x) = -x + 255 si 245<x<=255 ;
	where x is the ascii code of a character belonging to the header
	
	*It can be shown that this function has inverse defined as ;
		f(x) = x - 10 si 10<=x<=255 ;
		f(x) = -x + 255 si 0<=x<10

	*The file to decrypt must be encripted; if not, this method does nothing
	
	LOCAL nHndl as Number
	LOCAL sDBFHeaderString as String, sUnfiddledDBFHeaderString as String 
	LOCAL sCharToFiddle as String, nCharToFiddle as Integer 

	IF FILE(This.sTargetFileNameAndPath) THEN
		nHndl = FOPEN(This.sTargetFileNameAndPath ,12)

		IF nHndl!=-1 THEN

			*Read the first record
			sDBFHeaderString = FREAD(nHndl,N_BYTES_TO_FIDDLE)

			IF ASC(SUBSTR(sDBFHeaderString,1,1))!=0x30 AND ASC(SUBSTR(sDBFHeaderString,1,1))!=0x31 AND ASC(SUBSTR(sDBFHeaderString,1,1))!=0x32 THEN
				*The targete file was Fiddled, then unfiddle its first bytes
				sUnfiddledDBFHeaderString = ""
				FOR i = 1 to LEN(sDBFHeaderString)
					sCharToFiddle = SUBSTR(sDBFHeaderString,i,1)
					nCharToFiddle = ASC(sCharToFiddle )
					IF BETWEEN(nCharToFiddle,10,255) THEN
						sUnfiddledDBFHeaderString = sUnfiddledDBFHeaderString + CHR(nCharToFiddle  - 10)
					ELSE
						sUnfiddledDBFHeaderString = sUnfiddledDBFHeaderString + CHR(255 - nCharToFiddle)
					ENDIF
				ENDFOR

				*go back to the top of the file and replace the first record with decodified string
				FSEEK(nHndl,0,0)
				FWRITE(nHndl,sUnfiddledDBFHeaderString)
			ENDIF
			
			*Close the file
			FCLOSE(nHndl)
		ENDIF
	ENDIF
ENDPROC

ENDDEFINE

LPARAMETERS sEncryptOrDecrypt as String 
*Si es 'E', ejecuta el proceso de "encripción"; si es 'D', el de "desencripción"

LOCAL oDBFHeaderCryptographer as Object 

IF !EMPTY(sEncryptOrDecrypt) THEN
	oDBFHeaderCryptographer = NEWOBJECT("DBFCryptographer","DBFCryptographer.PRG")
	oDBFHeaderCryptographer.sTargetFileNameAndPath = "c:\temp\pruebas\paciente.dbf"

	DO CASE sEncryptOrDecrypt
		CASE UPPER(sEncryptOrDecrypt)='E'
			oDBFHeaderCryptographer.encryptFile
		CASE UPPER(sEncryptOrDecrypt)='D'
			oDBFHeaderCryptographer.decryptFile
	ENDCASE

	RELEASE oDBFHeaderCryptographer 
ELSE
	MESSAGEBOX("Uso: sEncryptOrDecrypt 'E' - para encriptar o " + CHR(13) + CHR(10) + "sEncryptOrDecrypt 'D' - para desencriptar",16)
ENDIF

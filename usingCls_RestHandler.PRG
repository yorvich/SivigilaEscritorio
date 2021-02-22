LPARAMETERS sPath as string, sTipIde as String, sNumIde as string

*SET STEP ON 

CLEAR
DO CASE 
	CASE sPath = '1'
		oRestHandler = NEWOBJECT("RestHandler","cls_RestHandler.fxp",.NULL.)

		*Gets an Authorization token
		oRestHandler.sRestURL = "https://segcovid19.sispro.gov.co/WSConsultaAfiliados/api/login/authenticate"
		oRestHandler.set_sAuthorizationToken("POST",'UserINSConsultaAfiliacion','A13902C2-097D-4CE2-A563-3EC3A7A4F59E')

		*Gets data
		oRestHandler.sRestURL = "https://segcovid19.sispro.gov.co/WSConsultaAfiliados/api/AfiliadoInfo1/" + sTipIde + "/" + sNumIde 
		oRestHandler.ResetRequestBody()
		oRestHandler.SendGETRequest()


		?oRestHandler.sResponseText 

		RELEASE oRestHandler
	
	CASE sPath = '2'
		sObsoloteToken = '' &&Obliga a obtener un nuevo token
		*sObsoloteToken = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1bmlxdWVfbmFtZSI6IlVzZXJJTlNDb25zdWx0YUFmaWxpYWNpb24' +;
						'iLCJuYmYiOjE1OTIwNjgwOTAsImV4cCI6MTU5MjA2OTg5MCwiaWF0IjoxNTkyMDY4MDkwLCJpc3MiOiJodHRwOi8vMTkyLjE2OC45Ny4yMTEvV1NDb25zdWx0YUFmaWxpYWRvcyIsImF1ZCI6Imh0dHA6Ly8xOTIuMTY4Ljk3LjIxMS9XU0NvbnN1bHRhQWZpbGlhZG9zIn0.OmH0Nuyp3zezktLuw-4BlDwSsYvd-o6GNWRz0t0PIZA'
		oBDUARestHandler = NEWOBJECT("BDUARestHandler","cls_RestHandler.fxp",.NULL.,sObsoloteToken )

		IF !oBDUARestHandler.HasError() THEN 

			*Gets unique person data
			oBDUARestHandler.GetPersonData(sTipIde ,sNumIde )
			
			IF !oBDUARestHandler.HasError() THEN 
				IF oBDUARestHandler.IsValidPerson() THEN 
					?oBDUARestHandler.oPersonDataBDUA.get('TipoDocumento'), oBDUARestHandler.oPersonDataBDUA.get('PrimerApellido'),  ;
					oBDUARestHandler.oPersonDataBDUA.get('SegundoApellido')  
				ELSE
					?'La persona no se encontró en el BDUA'
				ENDIF 
			ELSE 
				?'Se presentó el error:' + oBDUARestHandler.sErrMsg + ' (Cod.' + ALLTRIM(STR(oBDUARestHandler.nErrCode)) + ')'
			ENDIF 
		ELSE
			?'Se presentó el error:' + oBDUARestHandler.sErrMsg + ' (Cod.' + ALLTRIM(STR(oBDUARestHandler.nErrCode)) + ')'
		ENDIF 
		
		RELEASE oBDUARestHandler

ENDCASE 





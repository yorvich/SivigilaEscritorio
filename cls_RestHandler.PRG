#INCLUDE SIVIGILA.H
#INCLUDE cls_RestHandler.h


DEFINE CLASS RestHandler as Custom

*Provee métodos para usar servicios Restful. La clase extiende un ejemplo publicado en https://stackoverflow.com/questions/14228596/how-can-i-consume-restful-services-from-visual-fox-pro
*Otra documentación relevante se puede consultar en: ;
https://west-wind.com/wconnect/weblog/ShowEntry.blog?id=920    ;                                                                                                                                                                 
https://www.example-code.com/foxpro/rest_simple.asp	;
https://www.example-code.com/foxpro/rest.asp


*La clase hace uso intensivo del objeto MSXML2.ServerXMLHTTP según se encuentra documentado en ;
https://docs.microsoft.com/en-us/previous-versions/windows/desktop/ms763809(v=vs.85)

#DEFINE TIME_OUT 10

HIDDEN xmlHttp as Object 
*Objeto MSXML2.ServerXMLHTTP

HIDDEN sAuthorizationToken as String 
sAuthorizationToken = ''
*Mantiene un token de autorización de uso que se utilizará en todas las peticiones que se hagan: POST, GET, PUT, DELETE

sRestURL = ''
*URL en donde se encuentra publicado el servicio

sRequestBody = ''
sResponseText  = ''

sErrMsg = ''
*Mensaje de error resultante de la utilización del servicio, si es que se ha producido uno

nErrCode = 0
*Código de error resultante de la utilización del servicio: ;
- 401 Unauthorized - el token ya venció;
- 500 Internal server error;
- 502 No hay red/internet;
- 200 OK (internamente hace que nErrCode sea 0);
- 50 código asignado por la clase utilizado en forma adicional cuando 'La petición fue cancelada por el usuario'


FUNCTION Init()
	This.xmlHttp = CREATEOBJECT("MSXML2.ServerXMLHTTP")
ENDFUNC 

FUNCTION Destroy
ENDFUNC

PROCEDURE OpenService(sMethod as String)
	This.xmlHttp.open(sMethod , This.sRestURL, 1)
ENDPROC


PROCEDURE SendJsonRequest(sMsgWhileWaiting as String)

	*Envía una petición al servicio usando application/json como valor del campo Content-Type. Utiliza el token de autorización ;
	si se ha obtenido uno -vía set_sAuthorizationToken- y el requestBody establecido en This.sRequestBody. La respuesta obtenida ;
	se almacena en This.sResponseText
	
	LOCAL nUserReply as integer 
	nUserReply = -2 &&Este valor inicial, de mantenerse en el transcurso de la ejecución, indica que la petición obtuvo respuesta
	
	IF !(VARTYPE(sMsgWhileWaiting )='C' AND !EMPTY(sMsgWhileWaiting )) THEN 
		sMsgWhileWaiting = 'Por favor espere, consultando...'
	ENDIF 
	
	This.xmlHttp.setRequestHeader("Content-Type", "application/json")
	
	IF !EMPTY(This.sAuthorizationToken) THEN 
		This.xmlHttp.setRequestHeader("Authorization", This.sAuthorizationToken)
	ENDIF 
	
	bContinue = .T.
	This.sErrMsg = ''
	TRY 
		This.xmlHttp.send(This.sRequestBody)
	CATCH TO oException
		bContinue = .F.
		IF oException.ErrorNo = 1429 THEN 
			This.nErrCode = 502
			This.sErrMsg = 'No hay disponibildad de red/internet'
		ELSE
			This.nErrCode = oException.ErrNo
			This.sErrMsg = oException.Message
		ENDIF 
	ENDTRY 
	
	IF bContinue THEN 
		nInitTime = DATETIME()
		
		DO WHILE This.xmlHttp.readyState <> 4 &&COMPLETED
			
			WAIT sMsgWhileWaiting WINDOW NOWAIT NOCLEAR 
			
			DOEVENTS &&FORCE Ver https://www.tek-tips.com/viewthread.cfm?qid=1772216
			
			IF DATETIME() - nInitTime > TIME_OUT THEN 
				*Se ha cumplido el itempo máximo para obtener respuesta
	
				WAIT CLEAR 
				nUserReply = -1
				This.sErrMsg = 'El servicio está tardando demasiado en responder o no hay disponibildad de red/internet'
				This.nErrCode = 502
				EXIT 
			ENDIF 
			
		ENDDO 
		
		IF nUserReply = -2 THEN 
			*Verifica cuál fue el resultado de la petición
			
			WAIT CLEAR 
			
			This.nErrCode = This.xmlHttp.status
			
			DO CASE
				CASE This.xmlHttp.status = 200
					This.nErrCode = 0
					
				CASE This.xmlHttp.status = 401 &&Unauthorized
					This.sErrMsg = 'El token ya venció'

				CASE This.xmlHttp.status = 500 &&Internal server error
					This.sErrMsg = 'El servidor consultado presenta fallas'

				CASE This.xmlHttp.status = 502
					This.sErrMsg = 'No hay disponibildad de red/internet'

			ENDCASE
		ENDIF 
	
		IF EMPTY(This.sErrMsg) THEN 
			This.sResponseText = This.xmlHttp.responseText
		ELSE
			This.sResponseText = ''
		ENDIF 
	ENDIF 		

ENDPROC 


FUNCTION set_sAuthorizationToken(sMethod as String, sUsername as String, sPwd as String, sToken as string)

	*Obtiene un token de autorización de uso del servicio usando para ello las credenciales sUsername y sPwd. sMethod es el método que;
	debe usarse para obtener el token.
	* Si se suministra el parámetro opcional sToken, se asigna el valor de la propiedad This.sAuthorizationToken a ese valor y se;
	ignoran los restantres parámetros.
	
	IF VARTYPE(sToken)='C' AND !EMPTY(sToken) THEN 
		This.sAuthorizationToken = sToken
	ELSE 
		WITH this 
			.sRequestBody = '' +;
							'{'  +;
							'"username":"' + sUsername  + '",' +;
							'"password":"' + sPwd  + '"' +;
							'}'
			.OpenService(sMethod)
			
			.SendJsonRequest()
			IF This.nErrCode  = 401 &&Unauthorized 
				This.sErrMsg = 'No se pudo obtener un token por falta de autorización'
			ENDIF 
			
			.sAuthorizationToken = SUBSTR(.sResponseText,2,LEN(.sResponseText)-2) 
		ENDWITH 
	ENDIF 
ENDFUNC 


FUNCTION ResetRequestBody()
	This.sRequestBody = ''
ENDFUNC 


PROCEDURE SendGETRequest(sMsgWhileWaiting as String)
	*Envía una petición GET al servicio usando el valor actual de This.sRequestBody; si este valr es vacío, la petición estará totalmente;
	contenida en la URL fijada en This.sRestURL
	
	This.OpenService("GET")
	
	This.SendJsonRequest(sMsgWhileWaiting)
	IF This.nErrCode = 0 THEN 
		This.sResponseText = This.xmlHttp.responseText
	ELSE
		This.sResponseText = ''
	ENDIF 
ENDPROC 

ENDDEFINE &&CLASS RestHandler


******************************************************************************************************************************************

DEFINE CLASS BDUARestHandler as RestHandler 
*Provee métodos para usar el servicio Restful suministrado por el Ministerio de salud y protección social que permite la consulta ;
de información de afiliación de una persona identificada por el par {Tipo de Documento Identificación C(2), Número Documento Identificación C(17)}

*La respuesta obtenida a partir de la consulta al servicio se almacena en un objeto json que puede ser accesado usando la sintaxis ;
oPersonDataBDUA('TipoDocumento'), oPersonDataBDUA('NumeroDocumento'), oPersonDataBDUA('PrimerApellido'), etc. según los campos que se definen;
a continuación: ;
TipoDocumento	varchar	2;	
NumeroDocumento	varchar	17;	
	CC: Cédula de Ciudadanía;
	TI: Tarjeta de Identidad;
	RC: Registro Civil;
	CN: Certificado de Nacido Vivo;
	AS: Adulto sin identificar;
	MS: Menor sin identificar;
	CE: Cedula de extranjería;
	PE: Permiso Especial de Permanencia;
PrimerApellido	varchar	60;
SegundoApellido	varchar	60;
PrimerNombre	varchar	60;
SegundoNombre	varchar	60;
EPSAfiliacion	varchar	6;
NombreEPS	varchar	60;
EstadoAfiliacion	varchar	2;
	AC - Activo;
	SU – Suspendido;
	AF - Fallecido;
	SA – Sin afiliación;
FechaConsulta	varchar	10;

oPersonDataBDUA = .null.
*El objeto json con los datos de identificación y afiliación obtenidos

FUNCTION Init(sToken as string)

	DODEFAULT()
	
	*Gets an Authorization token
	This.sRestURL = "https://segcovid19.sispro.gov.co/WSConsultaAfiliados/api/login/authenticate"
	IF VARTYPE(sToken)='C' AND !EMPTY(sToken ) THEN 
		This.set_sAuthorizationToken(,,,sToken)
	ELSE 
		This.set_sAuthorizationToken("POST",'UserINSConsultaAfiliacion','A13902C2-097D-4CE2-A563-3EC3A7A4F59E')
	ENDIF 
ENDFUNC 

PROCEDURE refreshToken()
	This.set_sAuthorizationToken("POST",'UserINSConsultaAfiliacion','A13902C2-097D-4CE2-A563-3EC3A7A4F59E')		
ENDFUNC 


FUNCTION HasError()
	Return (This.nErrCode != 0)
ENDFUNC 

FUNCTION IsValidPerson() as Boolean
	Return (!ISNULL(This.oPersonDataBDUA))
ENDFUNC 

PROCEDURE GetPersonData(sIdType as String, sIdentificationNumber as String)

	*Consulta el servicio Restful suministrado por el MSPS suministrando como parámetros en la URl, sIdType y sIdentificationNumber.;
	La respuesta obtenida queda en This.oPersonDataBDUA. En caso de que no se obtenga una respuesta del servicio o sea vacía, ;
	This.oPersonDataBDUA será .NULL.

	SET PROCEDURE TO (PATH_TO_VFPJson_CLASS + '\json') ADDITIVE

	This.sRestURL = "https://segcovid19.sispro.gov.co/WSConsultaAfiliados/api/AfiliadoInfo1/" + sIdType + "/" + sIdentificationNumber 
	This.ResetRequestBody()
	This.SendGETRequest(MSG_WHILE_CONSULTING)
	This.sResponseText = SUBSTR(This.sResponseText,2,LEN(This.sResponseText)-2)

	IF !EMPTY(This.sResponseText) THEN
		This.oPersonDataBDUA = json_decode(This.sResponseText)
		IF !EMPTY(json_getErrorMsg()) THEN 
			This.sErrMsg = 'Error al decodificar:' + json_getErrorMsg()
		ENDIF
	ELSE
		This.oPersonDataBDUA = .NULL.
	ENDIF
ENDPROC 

ENDDEFINE &&CLASS BDUARestHandler

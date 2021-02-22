#INCLUDE GlobalConst.h


#DEFINE MOTHER_DATA_REQUIRED ;
	"Recuerde que para este evento los datos de identificación que se deben diligenciar son los de identificación de la Madre"

#DEFINE ERR_CAN_NOT_BE_TOSFERINA ;
	"Si usted marca NO en esta variable, el caso no corresponde a una Tos ferina. Desea corregir?" + CrLf + CrLf


#DEFINE ERR_SEVERAL_CONTROL_FILES ;
	"ERROR: varios de los archivos encontrados podrían interpretarse como archivos de control" + CrLf + CrLf


#DEFINE ERR_CONTROL_FILE_NOT_PICKED ;
	"ERROR: No se seleccionó un archivo de control" + CrLf + CrLf


#DEFINE ERR_CONTROL_FILE_NOT_FOUND ;
	"ERROR: No se encontró un archivo de control en la carpeta" + CrLf + CrLf


#DEFINE ERR_INVALID_CONTROL_FILE_NAME ;
	'ERROR: El nombre del archivo de control NO es válido' + CrLf + CrLf


#DEFINE ERR_INVALID_CONTROL_FILE_EXT ;
	'ERROR: La extensión del archivo de control NO es válida' + CrLf + CrLf


#DEFINE ERR_VERSION_CONFLICT ;
	'La versión de los archivos planos es superior a la de este sistema. ' + CrLf +;
	'No se pueden cargar. Debe actualizar el Sistema a la última versión disponible'


#DEFINE ERR_VERSION_FILE_ERROR ;
	'No se pudieron leer los archivos planos: El archivo de información de versiónes (SIVIGILA.TXT)' + CrLf +;
	'con las que se generaron tiene errores que impiden determinar el número de versión.' + CrLf + CrLf


#DEFINE ERR_VERSION_FILE_NOT_FOUND ;
	'ERROR: No se encontró el archivo SIVIGILA.TXT' + CrLf + CrLf


#DEFINE ERR_CAN_NOT_OPEN_VERSION_FILE ;
	'ERROR: No se pudo abrir el archivo SIVIGILA.TXT' + CrLf + CrLf


#DEFINE ERR_FEEDBACK_NOT_AVAILABLE ;
	'ERROR: No se pueden cargar datos desde archivos de retroalimentación' + CrLf + CrLf


#DEFINE ERR_CONSOLIDATION_FOLDER_NOT_FOUND ;
	'ERROR: No se encuentra la carpeta de consolidación' + CrLf + CrLf


#DEFINE ERR_CONTROL_FILE_WITHOUT_DATA ;
	'ERROR: No existen datos en el archivo de Control' + CrLf + CrLf


#DEFINE ERR_CONTROL_FILE_STRUCTURE_ERROR ;
	'ERROR: El nombre del archivo de Control NO es concordante con el nombre del archivo texto (U, B, D, etc.) que se está importando' + CrLf + CrLf


#DEFINE WARNING_UPGD_PLAIN_FILE_WITH_DUPLICATES ;
	'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
	'Existen registros repetidos o duplicados en el archivo plano de UPGDs.' + CHR(13) + ;
	'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
	'repetidos o duplicados en la misma carpeta en donde están los archivos planos  '  + CHR(13) + ;
	'para los fines que se considere pertinentes.'  + CHR(13)


#DEFINE WARNING_TH_PLAIN_FILE_WITH_DUPLICATES ;
	'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
	'Existen registros repetidos o duplicados en el archivo plano de Talento Humano.' + CHR(13) + ;
	'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
	'repetidos o duplicados en la misma carpeta en donde están los archivos planos  '  + CHR(13) + ;
	'para los fines que se considere pertinentes.'  + CHR(13)

#DEFINE WARNING_UCI_PLAIN_FILE_WITH_DUPLICATES ;
	'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
	'Existen registros repetidos o duplicados en el archivo plano de UCI.' + CHR(13) + ;
	'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
	'repetidos o duplicados en la misma carpeta en donde están los archivos planos  '  + CHR(13) + ;
	'para los fines que se considere pertinentes.'  + CHR(13)

#DEFINE WARNING_INDIVIDUAL_PLAIN_FILE_WITHOUT_CD ;
	'Existen registros sin Datos complementarios en el archivo plano de Notificación Individual.' + CHR(13) + ;
	'Estos registros NO se cargarán en la base de datos Maestra.'  + CHR(13) + CHR(13) + ;
	'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
	'huerfanos en la misma carpeta en donde están los archivos planos  '  + CHR(13) + ;
	'para los fines que se considere pertinentes.'  + CHR(13)


#DEFINE WARNING_INDIVIDUAL_PLAIN_FILE_WITH_DUPLICATES ;
	'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
	'Existen registros repetidos o duplicados en el archivo plano de Notificación Individual.' + CHR(13) + ;
	'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
	'repetidos o duplicados en la misma carpeta en donde están los archivos planos  '  + CHR(13) + ;
	'para los fines que se considere pertinentes.'  + CHR(13)


#DEFINE WARNING_OUTBREAK_PLAIN_FILE_WITHOUT_CD ;
	'Existen registros sin Datos complementarios en el archivo plano de Notificación Colectiva.' + CHR(13) + ;
	'Estos registros NO se cargarán en la base de datos Maestra.'  + CHR(13) + CHR(13) + ;
	'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
	'huerfanos en la misma carpeta en donde están los archivos planos  '  + CHR(13) + ;
	'para los fines que se considere pertinentes.'  + CHR(13)


#DEFINE WARNING_OUTBREAK_PLAIN_FILE_WITH_DUPLICATES ;
	'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
	'Existen registros repetidos o duplicados en el archivo plano de Notificación Colectiva.' + CHR(13) + ;
	'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
	'repetidos o duplicados en la misma carpeta en donde están los archivos planos  '  + CHR(13) + ;
	'para los fines que se considere pertinentes.'  + CHR(13)


#DEFINE WARNING_COMPLEMENTARY_PLAIN_FILE_WITH_DUPLICATES_PART1 ;
	'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
	'Existen registros repetidos o duplicados en el archivo plano de Datos Complementarios ' + CHR(13) + ;
	'para el evento '

#DEFINE WARNING_COMPLEMENTARY_PLAIN_FILE_WITH_DUPLICATES_PART2 ;
	'.' + CHR(13) + ;
	'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
	'repetidos o duplicados en la misma carpeta en donde están los archivos planos  '  + CHR(13) + ;
	'para los fines que se considere pertinentes.'  + CHR(13)


#DEFINE WARNING_IRA_PLAIN_FILE_WITH_DUPLICATES ;
	'ESTO NO ES UN ERROR DEL SISTEMA SINO SOLO UN AVISO DE PRECAUCIÓN.' + CHR(13) + CHR(13) +;
	'Existen registros repetidos o duplicados en el archivo plano de Morbilidad por IRA.' + CHR(13) + ;
	'El proceso continuará y se guardará un archivo excel con los registros '  + CHR(13) + ;
	'repetidos o duplicados en la misma carpeta en donde están los archivos planos  '  + CHR(13) + ;
	'para los fines que se considere pertinentes.'  + CHR(13)


#DEFINE ERR_PLAINS_NOT_LOADED ;
	"No se han leído datos desde archivos planos. No hay nada que consolidar."


#DEFINE ERR_PLAINS_DATA_WITH_ANOMALIES ;
	"Se encontraron errores durante la validación de campos." + CrLf +;
	"Debe revisar los archivos planos que pretende cargar para garantizar " +;
	"su consistencia con las reglas de validación establecidas para las " +;
	"variables de los eventos del Sistema SIVIGILA" + CrLf + CrLf


#DEFINE ERR_OPERATION_DENIED ;
	'No se le ha asignado acceso para operar este módulo' + CrLf + 'Por favor solicite autorización al Administrador del SIVIGILA'


#DEFINE WARNING_LOADED_RECS_ALREADY_IN_DB ;
	'Se detectaron algunos registros existentes en la base de datos maestra' + CrLf + CrLf


#DEFINE ERR_VALUE_NOT_FOUNDED ;
	'El valor no existe. Desea Consultar'

#DEFINE ERR_MISSING_BASIC_DATA ;
	'Primero se deben almacenar los datos del paciente'

#DEFINE ERR_NOT_VALID_EVENT_ADJUSTMENT ;
	"El ajuste no es válido para el evento" + CrLf + CrLf

#DEFINE ADJUST_ALREADY_EXISTS_MESSAGE ;
	"Ya existe un ajuste que está pendiente por notificar."  + CrLf + CrLf + ;
	"Actualice el registro existente que se le mostrará a continuación."

#DEFINE WARNING_ADJUSTMENT_AUTOCALCS ;
	"Recuerde que si ajusta la fecha de nacimiento, el Sistema NO "  + CrLf + ;
	"recalculará la edad automáticamente por cuanto esa variable" + CrLf + ;
	"corresponde al momento en que el paciente presentó el evento" + CrLf + ;
	"y no al momento en que se está haciendo un ajuste." + CrLf + ;
	"Modifique la edad manualmente en caso de ser pertinente."

#DEFINE WARNING_RENOTIFICATION_IN_PROGRESS ;
	"Recuerde que esta opción solamente aplica si usted ya notificó previamente o generó archivos planos de la semana que va a renotificar." + CrLf + CrLf +;
	"Desea continuar"

#DEFINE DENGUE_NOTIFICATION_NOT_ALLOWED_1 "Municipio seleccionado con altura sobre el nivel del mar superior a 2200 mts. Quiere corregir el dato"

#DEFINE DENGUE_NOTIFICATION_NOT_ALLOWED "En el municipio seleccionado no se presenta el vector. Quiere corregir el dato"

#DEFINE ERR_UPGD_AND_NOTIFIER_ARE_INCONSISTENT "El código de UPGD no es consistente con el código de unidad que reporta datos definido en la configuración del sistema"

#DEFINE BD_REC_ALREADY_EXISTS_MESSAGE ;
	'YA EXISTE una ficha en el sistema correspondiente a estos datos.' + CrLf + CrLf +;
	'A continuación se le mostrará la ficha que ya se encuentra grabada.'

#DEFINE ERR_UPGD_INACTIVE_MESSSAGE ;
	'En la caracterización de esta UPGD se tiene registrado que NO' + Cr +;
	'está activa.' + Cr +;
	'Por favor actualice esta información desde la ventana de UPGDs'

#DEFINE ERR_NONEXISTENT_UPGD_MESSSAGE ;
	'El código de la UPGD no existe'

#DEFINE FEEDBACK_RESULT_MESSAGE ;
	'Base de datos maestra actualizada.' + CrLf + CrLf +;
	'En la misma carpeta en donde están los archivos planos, se guardó un archivo excel con los registros de datos básicos agregados a la BD'

#DEFINE ERR_FEC_ASESOR_MESSSAGE ;
	'La fecha de  asesoría y canalización no debe ser anterior a la fecha de reactividad'

#DEFINE ERR_CREATING_EXCEL_AUTOMATION_SERVER_MESSSAGE ;
	'Error al crear el objeto de automatización Excel'

#DEFINE ERR_CREATING_NHSN_REPORT_MESSAGE ;
	'No se pudo crear el archivo reporte basado en el índice de riesgo NHSN'

#DEFINE EVENT_359_BEDROOMS_MESSAGE ;
	"El valor de la variable 'Camas vigiladas' no puede ser mayor al número de camas de este tipo que se hayan caracterizado en el sistema"

#DEFINE EVENT_359_SERVICES_MESSAGE ;
	"El valor de la variable '# de UCI vigiladas' no puede ser mayor al número de UCI de este tipo que se hayan caracterizado en el sistema"

#DEFINE ILOGICAL_UCI_BEDROOMS_MESSAGE ;
	'El total de camas por cada UCI, no puede ser inferior o igual a las camas de intermedios'

#DEFINE ERR_WITHOUT_SYMPTONS_BEGINNING_MESSSAGE ;
	"La fecha de inicio de síntomas es obligatoria para este evento"

#DEFINE ERR_SYMPTONS_BEGINNING_GREATER_THAN_CONSULTATION_MESSSAGE ;
	"La fecha de inicio de síntomas no debe ser mayor a la fecha de consulta"

#DEFINE ERR_HAPPENING_GREATER_THAN_CONSULTATION_MESSSAGE ;
	"La fecha del hecho debe ser anterior o igual a la de consulta"

#DEFINE WARNING_340_IS_THIS_A_CASE ;
	"Está seguro que cumple con definición de caso: paciente entre "  + CrLf + ;
	"9 y < 24 meses de edad, con resultado serológico para detección "  + CrLf + ;
	"de HBsAg positivo, nacido de una madre con una prueba "  + CrLf + ;
	"de detección para HBsAg positiva."

#DEFINE WARNING_340_HBsAG_POSITIVE ;
	"Esta seguro que la madre es positiva para HBsAg (+)"

#DEFINE ERR_INVALID_NEGATIVE_NOTIFICATION_DATE ;
	'La fecha máxima de notificación negativa es la del sábado inmediatamente anterior'

#DEFINE EVENT_815_VIF_DIAGNOSTIC_MESSAGE ;
	"Si usted ingresa 1, es por que se encuentra seguro que el "  + CrLf + ;
	"caso tiene resultado positivo para VIH. Recuerde que este "  + CrLf + ;
	"caso debe ser ingresado con el còdigo 850, bajo estadio "  + CrLf + ;
	"clínico igual a SIDA (2)"

#DEFINE EVENT_607_FIBER_MESSAGE ;
	"Si esta notificando un caso sospechoso de Ébola el paciente"  + CrLf + ;
	"debe presentar fiebre, de lo contrario no cumple con"  + CrLf + ;
	"definición de caso "

#DEFINE ERR_INVALID_YEAR_MESSSAGE ;
	'Año Invalido. Debe corresponder a la vigencia o al año inmediatamente anterior'

#DEFINE ERR_INVALID_EPIDEMIOLOGICAL_DATE ;
	'La fecha, semana o año ingresado no se encuentra en el calendario epidemiológico'

#DEFINE WARNING_NON_UPDATABLE_NOTIFICATION ;
	'La ficha ya fue notificada! No podrá hacer actualizaciones!'

#DEFINE WARNING_NON_UPDATABLE_FIELDS ;
	'El sistema ha establecido automáticamente varios valores' + CrLf +;
	'de la ficha! No podrá hacer actualizaciones de esos valores!' + CrLf + CrLf +;
	'Está seguro de querer Guardar'

#DEFINE WARNING_EXISTENT_NOTIFICATION ;
	'La ficha ya fué ingresada al sistema' + CrLf + ;
	'Datos disponibles para actualización.'

#DEFINE ERR_WHITOUT_PERMISSION_FOR_DATA_ENTRY ;
	'No se le ha asignado permiso para registrar este evento'

#DEFINE ERR_ATTEMPT_TO_UPDA_NATIONAL_EVENT ;
	'Este Evento es de notificación nacional' + CrLf + ;
	'Solo podrá cambiar las variables que indican si está bajo' + CrLf +;
	' control internacional, si maneja contactos o si admite'  + CrLf +;
	'notificación negativa semanal.' + CrLf +;
	'Quiere hacerlo'

#DEFINE ERR_INVALID_NOTIFICATION_YEAR ;
	'Año Invalido. Debe corresponder a'   + CrLf + ;
	'la vigencia o al año inmediatamente anterior'

#DEFINE WARNING_300_WRONG_TREATMENT_1 ;
   'Error en el tratamiento,' + CrLf +;
   'Favor avise a epidemiología o a la UPGD notificadora.' + CrLf +;
   'Recuerde que en las No Exposiciones y en  las Exposiciones Leves' + CrLf +;
   'No se debe aplicar suero antirrábico'

#DEFINE WARNING_300_WRONG_TREATMENT_2 ;
   'Error en el tratamiento,' + CrLf +;
   'Favor avise a epidemiología o a la UPGD notificadora.' + CrLf +;
   'Toda Exposición Grave debe ser tratada con Suero y Vacuna'

#DEFINE WARNING_300_WRONG_CLASIFICATION ;
   'Error en la clasificación,' + CrLf +;
   'Favor avise a epidemiología o a la UPGD notificadora.' + CrLf +;
   'Toda agresión causada por un animal silvestre o salvaje' + CrLf +;
   ' debe recibir tratamiento con suero'

#DEFINE QUESTION_UPDATE_OR_NEW ;
   'Desea actualizar el registro?,' + CrLf +;
   'Si responde <SI> se actualizara el último registro.' + CrLf +;
   'Si responde <NO> se generara un nuevo registro.' + CrLf + CrLf +;
   '¿Desea actualizar el registro'

#DEFINE ERR_NO_RECORDS_TO_NOTIFY ;
	'No se encontraron registros para generar los archivos planos en ninguna de las ' + CrLf +;
	'siguientes categorías: datos básicos, colectivos ni morbilidad por IRA'

#DEFINE QUESTION_NOTIFICATION_OR_NEW ;
	'Desea crear un nuevo registro?' + CrLf + CrLf +;
	'¡Hay fichas notificadas!' + CrLf +;
	'Si responde <SI> se generará un nuevo registro.' + CrLf +;
	'Si responde <NO> podra ajustar el último registro notificado.' + CrLf + CrLf +;
	'¿Desea crear un nuevo registro'

#DEFINE EPIDEMILOGICAL_WEEK_CHANGED ;
	'Se cambiará la semana epidemiológica en función de la fecha' + CHR(13) +;
	'que acaba de digitar, por cuanto la definición de este evento' + CHR(13) +;
	'de salud así lo establece.'

#DEFINE ERR_INCORRECT_EPIDEMILOGICAL_MONTH ;
	'El mes debe ser anterior o igual al de la fecha de notificación.'

#DEFINE ERR_INVALID_AGE ;
	'La fecha de nacimiento, edad y unidad de medida no son consistentes.'

#DEFINE ERR_INVALID_PHONE_NUMBER ;
	'Alfanumérico - Mínimo 7 dígitos y máximo 12 en caso de tener extensión'


#DEFINE WARNING_298_WRONG_VAR_VALUE ;
	'Recuerde que las vacunas administradas por vía oral no causan celulitis o absceso. Desea corregir'


#DEFINE ERR_BACKUP_DRIVE_NOT_FOUND ;
	'No existe la unidad de disco configurada para hacer backup'


*/ incluidas en SIVIGILAMESSENGER.PRG
#DEFINE MSG_PROCESSING_ERROR_TITLE 'Problema de procesamiento SIVIGILA'
#DEFINE MSG_VALIDATION_ERROR_TITLE 'Violación de validación SIVIGILA'
#DEFINE MSG_NOTIFICATION_TITLE 'Notificación SIVIGILA'
#DEFINE MSG_UNEXPECTED_ERROR_TITLE 'Error inesperado SIVIGILA'


#DEFINE WARNING_549_BASIC_DATA_DATES ;
	"Para morbilidad materna extrema la fecha de consulta y de hospitalización hace referencia al momento del diagnóstico de acuerdo con los criterios establecidos para morbilidad materna extrema."


#DEFINE EVENT_357_FEC_DIAG_MESSAGE ;
	"Recuerde que la fecha de diagnóstico debe ser igual a la "  + CrLf + ;
	"fecha de toma del cultivo o a la fecha de inicio de "  + CrLf + ;
	"síntomas para las NAV tipo I"


#DEFINE ERR_INVALID_EMAIL ;
	'No es una dirección válida de correo electrónico'

#DEFINE ERR_CREATING_CONTACTS_REPORT_MESSAGE ;
	'No se pudo crear el archivo reporte de contactos y seguimientos'


#DEFINE ERR_BDUA_SERVICE_NOT_AVAILABLE ;
	"En este momento no es posible acceder al BDUA."  + CrLf + CrLf + ;
	"Se presentó el error: "


#DEFINE WARNING_346_ONSET_REQUIRED ;
	"Si se trata de un paciente asintomático la fecha de inicio de síntomas debe ser la misma fecha de consulta."

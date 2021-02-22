*Ejecuta post comandos para los eventos que se almacenan en la tabla EVENTOS_88, de tal forma que se puedan hacer adecuaciones especiales para;
su salida en archivos excel

LPARAMETERS sSourceRSName as String, nQuery as Byte
*sSourceRSName (in): nombre del recordset que contiene los registros del evento objetivo. se supone que este recordset ya tiene los registros casi;
					listos para ser exportados hacia un archivo excel y que lo único que hay que hacer con ellos son modificaciones de forma o ;
					agregar expresiones calculadas

*nQuery (in): indica el número de query que se utilizó para producir el recordset sSourceRSName de acuerdo con las características generales de exportación;
				que se encuentran en la tabla ExportQueries

LOCAL oAdjustmentsProcessor AS Object 
LOCAL sSQLCmd AS String, sUCIFields  AS String
LOCAL nFirstUCIField AS Integer, nSourceTableFields AS Integer 

DIMENSION aSourceTableFields(1)
*SET STEP ON 

* Ejecuta un procesamiento de ajustes teniendo en cuenta un agrupamiento distinto al agrupamiento por defecto. Este proceso, como efecto colateral, ;
eliminará registros repetidos según la clave sTagExpr y dejará solo uno incluso en aquellos casos en donde exista más de un registro índice para el caso
sTagExpr = 'MES+AÑO+COD_PRE+COD_SUB+COD_EVE'
sOrdenProc = 'DTOC(FEC_AJU,1)' 
oAdjustmentsProcessor = NEWOBJECT("AdjustmentsProcessor","AdjustmentsProcessor.PRG")
oAdjustmentsProcessor.sSourceTablename = sSourceRSName 
oAdjustmentsProcessor.processAdjustments(sTagExpr, ,sOrdenProc,,,,.T.,'rsToXLS_Source')
RELEASE oAdjustmentsProcessor

sSourceRSName = 'rsToXLS_Source'

*Establece la lista de nombres de campos de la forma '_*'

nSourceTableFields = AFIELDS(aSourceTableFields,sSourceRSName)
*Localiza el primer nombre de campo que empiece con el caracter '_'
nFirstUCIField = ASCAN(aSourceTableFields,'_',1,-1,1,1+4+8)
sUCIFields = ''
FOR nField = nFirstUCIField TO nSourceTableFields 
	sUCIFields  = sUCIFields + aSourceTableFields(nField,1) + ","
NEXT nField
sUCIFields = LEFT(sUCIFields ,LEN(sUCIFields)-1)

DO CASE 
	CASE nQuery = 1 
		*Extrae los datos de UCI/A
		sSQLCmd = 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/A" as tipo_uci, ser_uci_a as serv_vig, cam_uci_a as camas_encuesta,' +;
				'dia_pac_a as dias_paciente, n_itsac_a as cas_itsac, d_itsac_a as dias_cc, tasa_itsac_uci_a as tasa_itsac, porc_itsac_uci_a as por_cc,' +;
				'n_istua_a as cas_istuac, d_istua_a as dias_cu, tasa_istuac_uci_a as tasa_istuac, porc_istua_uci_a as por_cu, n_nav_a as cas_nav, ' +;
				'd_nav_a as dias_vm, tasa_nav_uci_a as tasa_nav, porc_nav_uci_a as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION '
					
		*Extrae los datos de UCI/IA
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/IA" as tipo_uci, ser_uci_ia as serv_vig, cam_uci_ia as camas_encuesta,' +;
				'dia_pac_ia as dias_paciente, n_itsac_ia as cas_itsac, d_itsac_ia as dias_cc, tasa_itsac_uci_ia as tasa_itsac, porc_itsac_uci_ia as por_cc,' +;
				'n_istua_ia as cas_istuac, d_istua_ia as dias_cu, tasa_istuac_uci_ia as tasa_istuac, porc_istua_uci_ia as por_cu, n_nav_ia as cas_nav, ' +;
				'd_nav_ia as dias_vm, tasa_nav_uci_ia as tasa_nav, porc_nav_uci_ia as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION '
					
		*Extrae los datos de UCI/P
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/P" as tipo_uci, ser_uci_p as serv_vig, cam_uci_p as camas_encuesta,' +;
				'dia_pac_p as dias_paciente, n_itsac_p as cas_itsac, d_itsac_p as dias_cc, tasa_itsac_p as tasa_itsac, porc_itsac_uci_p as por_cc,' +;
				'n_istua_p as cas_istuac, d_istua_p as dias_cu, tasa_istua_p as tasa_istuac, porc_istuac_uci_p as por_cu, n_nav_p as cas_nav, ' +;
				'd_nav_p as dias_vm, tasa_nav_p as tasa_nav, porc_nav_uci_p as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION '

		*Extrae los datos de UCI/IP
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/IP" as tipo_uci, ser_uci_ip as serv_vig, cam_uci_ip as camas_encuesta,' +;
				'dia_pac_ip as dias_paciente, n_itsac_ip as cas_itsac, d_itsac_ip as dias_cc, tasa_itsac_ip as tasa_itsac, porc_itsac_uci_ip as por_cc,' +;
				'n_istua_ip as cas_istuac, d_istua_ip as dias_cu, tasa_istua_ip as tasa_istuac, porc_istuac_uci_ip as por_cu, n_nav_ip as cas_nav, ' +;
				'd_nav_ip as dias_vm, tasa_nav_ip as tasa_nav, porc_nav_uci_ip as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION '

		sSQLCmd = LEFT(sSQLCmd,LEN(sSQLCmd)-6) + ' INTO CURSOR rs359ToXls'
		&sSQLCmd

	CASE nQuery = 2
		*Extrae los datos de UCI/N
		*Categoría Menos_de_751
		sSQLCmd = 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/N" as tipo_uci, "Menor_o_igual_a_750" as categoria_peso,'  +;
				'ser_uci_n as serv_vig, cam_uci_n as camas_encuesta, n_d_p1_s_n as dias_paciente,  n_c_n1_s_n as cas_itsac, n_d_c1_s_n AS dias_cc,'  +;
				'tasa_itsac_n_750 as tasa_itsac, porc_itsac_uci_n_750 as por_cc, n_c_n1_v_n as cas_nav, n_d_c1_v_n as dias_vm, '  +;
				'tasa_nav_n_750 as tasa_nav, porc_nav_uci_n_750 as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION ' 
		*Categoría Entre_751_1000
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/N" as tipo_uci, "Entre_751_1000" as categoria_peso,'  +;
				'ser_uci_n as serv_vig, cam_uci_n as camas_encuesta, n_d_p2_s_n as dias_paciente,  n_c_n2_s_n as cas_itsac, n_d_c2_s_n AS dias_cc,'  +;
				'tasa_itsac_n_1000 as tasa_itsac, porc_itsac_uci_n_1000 as por_cc, n_c_n2_v_n as cas_nav, n_d_c2_v_n as dias_vm, '  +;
				'tasa_nav_n_1000 as tasa_nav, porc_nav_uci_n_1000 as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION ' 

		*Categoría Entre_1001_1500
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/N" as tipo_uci, "Entre_1001_1500" as categoria_peso,'  +;
				'ser_uci_n as serv_vig, cam_uci_n as camas_encuesta, n_d_p3_s_n as dias_paciente,  n_c_n3_s_n as cas_itsac, n_d_c3_s_n AS dias_cc,'  +;
				'tasa_itsac_n_1500 as tasa_itsac, porc_itsac_uci_n_1500 as por_cc, n_c_n3_v_n as cas_nav, n_d_c3_v_n as dias_vm, '  +;
				'tasa_nav_n_1500 as tasa_nav, porc_nav_uci_n_1500 as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION ' 

		*Categoría Entre_1501_2500
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/N" as tipo_uci, "Entre_1501_2500" as categoria_peso,'  +;
				'ser_uci_n as serv_vig, cam_uci_n as camas_encuesta, n_d_p4_s_n as dias_paciente,  n_c_n4_s_n as cas_itsac, n_d_c4_s_n AS dias_cc,'  +;
				'tasa_itsac_n_2500 as tasa_itsac, porc_itsac_uci_n_2500 as por_cc, n_c_n4_v_n as cas_nav, n_d_c4_v_n as dias_vm, '  +;
				'tasa_nav_n_2500 as tasa_nav, porc_nav_uci_n_2500 as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION ' 

		*Categoría Mas_de_2500
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/N" as tipo_uci, "Mas_de_2500" as categoria_peso,'  +;
				'ser_uci_n as serv_vig, cam_uci_n as camas_encuesta, n_d_p5_s_n as dias_paciente,  n_c_n5_s_n as cas_itsac, n_d_c5_s_n AS dias_cc,'  +;
				'tasa_itsac_n_Mayor2500 as tasa_itsac, porc_itsac_uci_n_Mayor2500 as por_cc, n_c_n5_v_n as cas_nav, n_d_c5_v_n as dias_vm, '  +;
				'tasa_nav_n_Mayor2500 as tasa_nav, porc_nav_uci_n_Mayor2500 as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' INTO CURSOR rs359ToXls_1'
		&sSQLCmd
		
		*Extrae los datos de UCI/IN
		*Categoría Menos_de_751
		sSQLCmd = 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/IN" as tipo_uci, "Menor_o_igual_a_750" as categoria_peso,'  +;
				'ser_uci_in as serv_vig, cam_uci_in as camas_encuesta, n_d_p1_sin as dias_paciente,  n_c_n1_sin as cas_itsac, n_d_c1_sin AS dias_cc,'  +;
				'tasa_itsac_in_750 as tasa_itsac, porc_itsac_uci_in_750 as por_cc, n_c_n1_vin as cas_nav, n_d_c1_vin as dias_vm, '  +;
				'tasa_nav_in_750 as tasa_nav, porc_nav_uci_in_750 as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION ' 
				
		*Categoría Entre_751_1000
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/IN" as tipo_uci, "Entre_751_1000" as categoria_peso,'  +;
				'ser_uci_in as serv_vig, cam_uci_in as camas_encuesta, n_d_p2_sin as dias_paciente,  n_c_n2_sin as cas_itsac, n_d_c2_sin AS dias_cc,'  +;
				'tasa_itsac_in_1000 as tasa_itsac, porc_itsac_uci_in_1000 as por_cc, n_c_n2_vin as cas_nav, n_d_c2_vin as dias_vm, '  +;
				'tasa_nav_in_1000 as tasa_nav, porc_nav_uci_in_1000 as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION ' 

		*Categoría Entre_1001_1500
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/IN" as tipo_uci, "Entre_1001_1500" as categoria_peso,'  +;
				'ser_uci_in as serv_vig, cam_uci_in as camas_encuesta, n_d_p3_sin as dias_paciente,  n_c_n3_sin as cas_itsac, n_d_c3_sin AS dias_cc,'  +;
				'tasa_itsac_in_1500 as tasa_itsac, porc_itsac_uci_in_1500 as por_cc, n_c_n3_vin as cas_nav, n_d_c3_vin as dias_vm, '  +;
				'tasa_nav_in_1500 as tasa_nav, porc_nav_uci_in_1500 as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION ' 

		*Categoría Entre_1501_2500
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/IN" as tipo_uci, "Entre_1501_2500" as categoria_peso,'  +;
				'ser_uci_in as serv_vig, cam_uci_in as camas_encuesta, n_d_p4_sin as dias_paciente,  n_c_n4_sin as cas_itsac, n_d_c4_sin AS dias_cc,'  +;
				'tasa_itsac_in_2500 as tasa_itsac, porc_itsac_uci_in_2500 as por_cc, n_c_n4_vin as cas_nav, n_d_c4_vin as dias_vm, '  +;
				'tasa_nav_in_2500 as tasa_nav, porc_nav_uci_in_2500 as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' UNION ' 

		*Categoría Mas_de_2500
		sSQLCmd = sSQLCmd + 'SELECT mes, año, cod_pre, cod_sub, cod_eve, num_con, fec_not, "UCI/IN" as tipo_uci, "Mas_de_2500" as categoria_peso,'  +;
				'ser_uci_in as serv_vig, cam_uci_in as camas_encuesta, n_d_p5_sin as dias_paciente,  n_c_n5_sin as cas_itsac, n_d_c5_sin AS dias_cc,'  +;
				'tasa_itsac_in_Mayor2500 as tasa_itsac, porc_itsac_uci_in_Mayor2500 as por_cc, n_c_n5_vin as cas_nav, n_d_c5_vin as dias_vm, '  +;
				'tasa_nav_in_Mayor2500 as tasa_nav, porc_nav_uci_in_Mayor2500 as por_vm, ajuste, fec_aju, nit_upgd, nom_eve, nom_upgd, ndep_notif, nmun_notif, Version, ' +;
				sUCIFields + ' FROM ' + sSourceRSName + ' INTO CURSOR rs359ToXls_2' 
		&sSQLCmd
		
		SELECT rs359ToXls_1.* FROM rs359ToXls_1 UNION SELECT rs359ToXls_2.* FROM rs359ToXls_2 INTO CURSOR rs359ToXls
		*sSQLCmd = LEFT(sSQLCmd,LEN(sSQLCmd)-6) + ' INTO CURSOR rs359ToXls'

ENDCASE

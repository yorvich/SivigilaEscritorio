LPARAMETERS sEventCode as String

nMajor = 14
nMinor = 1
nBuild = 0

SET STEP ON 
sIndividualDataSQL='SELECT A.cod_eve, A.fec_not, A.semana, A.año, A.cod_pre, A.cod_sub,' + ;
					' A.pri_nom AS pri_nom_, A.seg_nom AS seg_nom_, A.pri_ape AS pri_ape_, A.seg_ape AS seg_ape_,' + ;
					' A.tip_ide AS tip_ide_, A.num_ide AS num_ide_, A.edad AS edad_, A.uni_med AS uni_med_,' + ;
					' A.sexo AS sexo_, K.codnumpais AS cod_pais_o, ' + ;
					' LEFT(A.cod_mun,2) AS cod_dpto_o, RIGHT(A.cod_mun,3) AS cod_mun_o, A.area AS area_,' + ;
					' A.localidad AS localidad_, A.cen_poblad AS cen_pobla_, A.vereda AS vereda_,' + ;
					' A.bar_ver AS bar_ver_, A.dir_res AS dir_res_, A.ocupacion AS ocupacion_,' + ;
					' A.tip_ss AS tip_ss_, A.cod_ase AS cod_ase_, A.per_etn AS per_etn_,' + ;
					' A.GP_DISCAPA, A.GP_DESPLAZ, A.GP_MIGRANT, A.GP_CARCELA,' + ;
					' A.GP_GESTAN, A.GP_INDIGEN, A.GP_POBICFB AS GP_POBICBF, A.GP_MAD_COM,' + ;
					' A.GP_DESMOVI, A.GP_PSIQUIA, A.GP_VIC_VIO, A.GP_OTROS,' + ;
					' LEFT(A.mun_pro,2) AS cod_dpto_r,' + ;
					' RIGHT(A.mun_pro,3) AS cod_mun_r, A.fec_con AS fec_con_, A.ini_sin AS ini_sin_,' + ;
					' A.tip_cas AS tip_cas_, A.pac_hos AS pac_hos_,' + ;
					' A.fec_hos AS fec_hos_, A.con_fin AS con_fin_, A.fec_def AS fec_def_, A.ajuste AS ajuste_,' + ;
					' A.telefono AS telefono_, A.fecha_nto AS fecha_nto_,' + ;
					' A.cer_def AS cer_def_, A.cbmte AS cbmte_, DATE() AS fec_arc_xl,' + ;
					' A.nom_dil_fi AS nom_dil_f_,' + ;
					' A.tel_dil_fi AS tel_dil_f_, A.fec_aju AS fec_aju_, B.nit_upgd, A.FM_FUERZA, A.FM_UNIDAD, A.FM_GRADO, A.Version'
					
sAdditionalIndividualDataSQL = ', E.nom_eve, B.raz_soc AS nom_upgd,' + ; 
								' C.nom_dep AS ndep_proce, ' +;
			  					' D.nom_mun AS nmun_proce, F.nom_dep AS ndep_resi,' + ; 
			  					' G.nom_mun AS nmun_resi, I.nom_mun AS nmun_notif,' + ; 
			  					' J.nom_dep AS ndep_notif '

sFROM_SQL =	' FROM Paciente A  LEFT OUTER JOIN UPGD B ON  A.cod_pre = B.cod_pre AND  A.cod_sub = B.cod_sub ' + ;
			' LEFT OUTER JOIN MUNICIPIOS I  ON  LEFT(B.cod_pre,5) = I.cod_mun ' +;
			' LEFT OUTER JOIN DEPTOS J  ON  LEFT(B.cod_pre,2) = J.cod_dep ' +;
			' LEFT OUTER JOIN DEPTOS C ON  LEFT(A.cod_mun,2) = C.cod_dep ' +;
			' LEFT OUTER JOIN MUNICIPIOS D ON  A.cod_mun = D.cod_mun ' +;
			' LEFT OUTER JOIN Eventos E ON  A.cod_eve = E.cod_eve ' +;   						   					
			' LEFT OUTER JOIN DEPTOS F ON  LEFT(A.mun_pro,2) = F.cod_dep ' +;
			' LEFT OUTER JOIN MUNICIPIOS G ON  A.mun_pro = G.cod_mun ' +;
			' LEFT OUTER JOIN PAISES K ON  A.Pais = K.Pais '
   					
*Establece las condiciones de filtrado para la extracción de registros
sWHERE_SQL =' WHERE A.COD_EVE ="' + IIF(!EMPTY(ALLTRIM(sEventCode)),sEventCode,ALLTRIM(sEventCode)) + '"' + ;
			' INTO CURSOR rsPACIENTE NOFILTER'
sSQLCmd = sIndividualDataSQL + sAdditionalIndividualDataSQL + sFROM_SQL + sWHERE_SQL 

* Si el evento tiene datos complementarios, agrega las columnas de la tabla EventTable a ;
la consulta de exportación
SET PROCEDURE TO SIVIGILAUtilities ADDITIVE
HasComplementaryData = fieldValue('FORMULARIO','EVENTOS','COD_EVE="' + ALLTRIM(sEventCode ) + '"')
IF NOT ISNULL(HasComplementaryData) THEN
	EventTableCode = ALLTRIM(HasComplementaryData)
	IF !EMPTY(HasComplementaryData) THEN
		EventTable = 'Eventos_' + PADL(EventTableCode, 2, '0')
		xlsFileNameSuffix =  ' DATOS BASICOS Y COMPLEMENTARIOS '

		*Establece, según el evento, los campos de datos complementarios que se deben extraer 
		StartPosition = 8
		IF RIGHT(EventTable, 2) $ '31 32 43'
			StartPosition = 9
	 	ENDIF
	 	SET PROCEDURE TO PlainsLib ADDITIVE
		sComplementaryDataSQL = getQueryFields('E' + PADL(EventTableCode, 2, '0'), StartPosition, 'H', ;
												nMajor, nMinor, nBuild)
	  					
   		sFROM_SQL =	sFROM_SQL + ' LEFT OUTER JOIN ' + EventTable + ' H ON A.cod_eve = H.cod_eve AND' +;
				' A.año = H.año AND A.semana = H.semana AND ' +;
				' A.tip_ide = H.tip_ide AND A.num_ide = H.num_ide  AND ' + ;
				' A.cod_pre = H.cod_pre AND A.cod_sub = H.cod_sub '
					
		sSQLCmd =  sIndividualDataSQL + "," + sComplementaryDataSQL + sAdditionalIndividualDataSQL + ;
					sFROM_SQL + sWHERE_SQL 
						
	ELSE
		xlsFileNameSuffix =  ' DATOS BASICOS '
		sSQLCmd = sIndividualDataSQL + sAdditionalIndividualDataSQL + sFROM_SQL + sWHERE_SQL 
	ENDIF
ENDIF

* Se ejecuta el Query que extrae los datos de paciente y complementarios para exportar a Excel
&sSQLCmd

SET STEP ON 

oAdjustmentsProcessor = NEWOBJECT("AdjustmentsProcessor","AdjustmentsProcessor.prg")

*Ejemplo 1: procesamiento secuencial por defecto, es decir, teniendo en cuenta los campos de ajuste y fecha de ajuste
oAdjustmentsProcessor.sSourceTablename = "rsPACIENTE"
oAdjustmentsProcessor.sAdjustmentFieldName = 'AJUSTE_'
oAdjustmentsProcessor.sAdjustmentDateFieldName = 'FEC_AJU_'
oAdjustmentsProcessor.processAdjustments2("AÑO+SEMANA+COD_EVE+TIP_IDE_+NUM_IDE_+COD_PRE+COD_SUB")

*Ejemplo 2: procesamiento teniendo en cuenta un orden prestablecido e ignorando los campos de ajuste y fecha de ajuste. En este ejemplo;
el orden prestablecido está dado por DTOC(FCH_INVCMP,1) + IIF(INVESCAMPO='2','0',INVESCAMPO) + AJUSTE
*oAdjustmentsProcessor.sSourceTablename = "EVENTOS_13"
*oAdjustmentsProcessor.processAdjustments("AÑO+SEMANA+COD_EVE+TIP_IDE+NUM_IDE+COD_PRE+COD_SUB", "AÑO = '2013'", ;
										"DTOC(FCH_INVCMP,1) + IIF(INVESCAMPO='2','0',INVESCAMPO) + AJUSTE", , , , .T.)

RELEASE oAdjustmentsProcessor


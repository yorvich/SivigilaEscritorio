#INCLUDE SIVIGILA.h

LPARAMETERS sFormName as String, bForTests as Boolean, sParams as String  

SET PROCEDURE TO PATH_TO_COMMON_LIB + 'TablesHandler' ADDITIVE

sFormName = UPPER(sFormName)

DO CASE 
	CASE sFormName = 'UPGD' OR sFormName = 'FRMUPGD_CRUD'
		STORE '' TO COD_PRE,COD_SUB,RAZ_SOC,DIR,REP_LEG,COR_ELE,RES_NOT,TEL,NAT_JUR,NIV,NOT_ACT,UNI_ANA,COV,TAL_HUM,TEC_DIS,;
					COM,FAX_MOD,TIE_COR,INT,TEL_FAX,RAD_TEL,ACT_SIV,NIT_UPGD,ES_UNI_NOT,ESTADOUPGD,LOC_O_ZONA,;
					HOSP_UNIVE,REG_EXCEPC,TOT_CAMAS,COMITE_INF,BI_PROFESI,IAAS_ULTIM,INF_TENDEN,SOCIAL_TEN,;
					LAB_MICROB,LAB_PROPIO,LABS_CONTR,IDENT_GYE,PRUE_SUCEP,LAB_AUTOMA,VITEK,MICROSCAN,PHOENIX,LAB_CCI,;
					LAB_CCE,MICR_CDI,WHONET,INFORM_PAT,LAB_CON_PE,LAB_REM_CE,LAB_REPORT,NOTIF_IAD,NOTIF_ISO,NOTIF_CAB,QUIEN_VCAB,;
					SER_CESARE,SER_PAR_VA,SER_COLECI,SER_HERNIO,SER_REVASC
					
		STORE CTOD('') TO FEC_CAR,FEC_CON,FEC_INICAR
		
		IF bForTests THEN 
			tecla_pres='LASTKEY()=5 OR LASTKEY()=19 OR LASTKEY()=127 OR LASTKEY()=31 OR LASTKEY()=27'
			_cod_acc='SIVIGILA  '
			_nomb_usua=''
			_rta=0
			specialChars = "+.\/:*?',{}<>_&;^()=#$%[]" + '"' 
			dSivigila2010LaunchDate = {^2018-02-05}
			_ver=''
			_car_esp = "This.value=chrtran(This.value,specialChars,'')"
			_dir = SYS(2023)
			RemoveSpecialChars = "This.value=chrtran(This.value,specialChars,'')"
			
			DATA_VALIDATION_ERROR = 1582
			RECORD_VALIDATION_ERROR = 1583

			goErrorlog = NEWOBJECT( 'xErrLog', 'errorlog.prg', NULL )
			gsErrorMsg = ""
			
			OPEN DATABASE BDSivigila
		ENDIF

		DO FORM (sFormName)

		IF bForTests THEN 
			RELEASE tecla_pres, _cod_acc, _rta, specialChars, dSivigila2010LaunchDate, _nomb_usua, _ver, _car_esp, _dir, RemoveSpecialChars, ;
					goErrorlog, gsErrorMsg 
			CLOSE DATABASES 
		ENDIF
		
	CASE sFormName = 'EVENTOS_24'
		
		IF bForTests THEN 
			tecla_pres='LASTKEY()=5 OR LASTKEY()=19 OR LASTKEY()=127 OR LASTKEY()=31 OR LASTKEY()=27'
			_rta=0
			_car_esp = "This.value=chrtran(This.value,specialChars,'')"
			specialChars = "+.\/:*?',{}<>_&;^()=#$%[]" + '"' 
			RemoveSpecialChars = "This.value=chrtran(This.value,specialChars,'')"
						
			goErrorlog = NEWOBJECT( 'xErrLog', 'errorlog.prg', NULL )
			gsErrorMsg = ""
			
			OPEN DATABASE BDSivigila
		ENDIF

		DO FORM (sFormName) WITH sParams

		IF bForTests THEN 
			RELEASE tecla_pres, _cod_acc, _rta, specialChars, dSivigila2010LaunchDate, _nomb_usua, _ver, _car_esp, _dir, RemoveSpecialChars, ;
					goErrorlog, gsErrorMsg 
			CLOSE DATABASES 
		ENDIF

	CASE sFormName = 'EVENTOS_43'
		DO FORM EVENTOS_43
		
	CASE sFormName = 'FRMEVENTOS_88'
		=SelectTable('EVENTOS_88',,.F.)
		
		STORE '' TO MES,AÑO,COD_PRE,COD_SUB,COD_EVE,NUM_CON,FEC_NOT,SER_UCI_A,CAM_UCI_A,DIA_PAC_A,N_ITSAC_A,D_ITSAC_A,;
			N_ISTUA_A,D_ISTUA_A,N_NAV_A,D_NAV_A,SER_UCI_IA,CAM_UCI_IA,DIA_PAC_IA,N_ITSAC_IA,D_ITSAC_IA,N_ISTUA_IA,D_ISTUA_IA,N_NAV_IA,D_NAV_IA,;
			SER_UCI_P,CAM_UCI_P,DIA_PAC_P,N_ITSAC_P,D_ITSAC_P,N_ISTUA_P,D_ISTUA_P,N_NAV_P,D_NAV_P,SER_UCI_IP,CAM_UCI_IP,DIA_PAC_IP,N_ITSAC_IP,;
			D_ITSAC_IP,N_ISTUA_IP,D_ISTUA_IP,N_NAV_IP,D_NAV_IP,SER_UCI_N,CAM_UCI_N,DIA_PAC_N,N_C_N1_S_N,N_D_C1_S_N,N_D_P1_S_N,N_C_N2_S_N,;
			N_D_C2_S_N,N_D_P2_S_N,N_C_N3_S_N,N_D_C3_S_N,N_D_P3_S_N,N_C_N4_S_N,N_D_C4_S_N,N_D_P4_S_N,N_C_N5_S_N,N_D_C5_S_N,N_D_P5_S_N,;
			N_C_N1_V_N,N_D_C1_V_N,N_D_P1_V_N,N_C_N2_V_N,N_D_C2_V_N,N_D_P2_V_N,N_C_N3_V_N,N_D_C3_V_N,N_D_P3_V_N,N_C_N4_V_N,N_D_C4_V_N,;
			N_D_P4_V_N,N_C_N5_V_N,N_D_C5_V_N,N_D_P5_V_N,SER_UCI_IN,CAM_UCI_IN,DIA_PAC_IN,N_C_N1_SIN,N_D_C1_SIN,N_D_P1_SIN,N_C_N2_SIN,;
			N_D_C2_SIN,N_D_P2_SIN,N_C_N3_SIN,N_D_C3_SIN,N_D_P3_SIN,N_C_N4_SIN,N_D_C4_SIN,N_D_P4_SIN,N_C_N5_SIN,N_D_C5_SIN,N_D_P5_SIN,;
			N_C_N1_VIN,N_D_C1_VIN,N_D_P1_VIN,N_C_N2_VIN,N_D_C2_VIN,N_D_P2_VIN,N_C_N3_VIN,N_D_C3_VIN,N_D_P3_VIN,N_C_N4_VIN,N_D_C4_VIN,;
			N_D_P4_VIN,N_C_N5_VIN,N_D_C5_VIN,N_D_P5_VIN,AJUSTE
		
		STORE CTOD('') TO FEC_NOT, FEC_AJU
		
		IF bForTests THEN 
			DO FORM (sFormName) WITH 'EVENTOS_88'
		ELSE 
			DO FORM (sFormName) WITH 'EVENTOS_88', '"' + _ver + 'Evento: ' + m.cod_eve + ' ' + m.nom_eve 
		ENDIF 
	CASE sFormName = 'FRMEVENTOS_81'
		SET PROCEDURE TO PATH_TO_COMMON_LIB + 'TablesHandler' ADDITIVE
		=SelectTable('EVENTOS_81',,.F.)

		STORE '354' TO COD_EVE
		
		IF bForTests THEN 
			DO FORM (sFormName) WITH 'EVENTOS_81'
		ELSE
			DO FORM (sFormName) WITH 'EVENTOS_81', 'Evento: 354 Consumo de antibióticos en el ámbito hospitalario' 
		ENDIF 
				
	CASE sFormName = 'FRMEVENTOS_90'
		=SelectTable('EVENTOS_90',,.F.)
		
		IF bForTests THEN 
			DO FORM (sFormName) WITH 'EVENTOS_90'
		ELSE 
			DO FORM (sFormName) WITH 'EVENTOS_90', '"' + 'Evento: 362 Infecciones de sitio quirúrgico asociadas a procedimiento médico quirúrgico - Colectivo' 
		ENDIF 

	CASE sFormName = 'FRMEVENTOS_91'
		SET PROCEDURE TO PATH_TO_COMMON_LIB + 'TablesHandler' ADDITIVE
		=SelectTable('EVENTOS_91',,.F.)
		
		STORE '' TO COD_EVE,SEMANA,AÑO,COD_PRE,COD_SUB,COD_MUN,NUM_CON,AGUA,ALIMENTOS,PERS_PERS,CONT_AMBIE,OTRO,DESCONOCID,;
			ESTADO_BRO,G1_ENFERMO,G1_EXPUEST,G1_HOMBRES,G1_MUJERES,G1_VIVOS,G1_MUERTOS,G2_ENFERMO,G2_EXPUEST,G2_HOMBRES,G2_MUJERES,;
			G2_VIVOS,G2_MUERTOS,G3_ENFERMO,G3_EXPUEST,G3_HOMBRES,G3_MUJERES,G3_VIVOS,G3_MUERTOS,G4_ENFERMO,G4_EXPUEST,G4_HOMBRES,;
			G4_MUJERES,G4_VIVOS,G4_MUERTOS,G5_ENFERMO,G5_EXPUEST,G5_HOMBRES,G5_MUJERES,G5_VIVOS,G5_MUERTOS,G6_ENFERMO,G6_EXPUEST,;
			G6_HOMBRES,G6_MUJERES,G6_VIVOS,G6_MUERTOS,G7_ENFERMO,G7_EXPUEST,G7_HOMBRES,G7_MUJERES,G7_VIVOS,G7_MUERTOS,;
			ALIMENTO1,ENF_ALIM1,EXP_ALIM1,ALIMENTO2,ENF_ALIM2,EXP_ALIM2,ALIMENTO3,ENF_ALIM3,EXP_ALIM3,ALIMENTO4,ENF_ALIM4,EXP_ALIM4,;
			ALIMENTO5,ENF_ALIM5,EXP_ALIM5,ALIMENTO6,ENF_ALIM6,EXP_ALIM6,ALIMENTO7,ENF_ALIM7,EXP_ALIM7,ALIMENTO8,ENF_ALIM8,EXP_ALIM8,;
			ALIMENTO9,ENF_ALIM9,EXP_ALIM9,ALIMENTO10,ENF_ALIM10,EXP_ALIM10,CAS_NINGUN,CAS_NAUSEA,CAS_VOMITO,CAS_DIARRE,CAS_FIEBRE,;
			CAS_DOLABD,CAS_CEFALE,CAS_DESHID,CAS_CIANOS,CAS_MIALGI,CAS_ALTRAL,CAS_MAREO,CAS_ESTRE,CAS_ESCALO,CAS_PAREST,CAS_ICTERI,;
			CAS_ACOLIA,CAS_COLURI,CAS_LESMAC,CAS_ANOREX,CAS_BRADIC,CAS_SIALOR,CAS_MIOSIS,CAS_OTROS,CUAL_SYS,LUGAR,DIR_LUG,AGENTE1,AGENTE2,;
			AGENTE3,AGENTE4,AGENTE5,TOTAL_ENF,TOTAL_EXP,TOTAL_HOM,TOTAL_MUJ,TOTAL_VIVO,TOTAL_MUER,AJUSTE
			
		STORE CTOD('') TO FEC_NOT, FEC_CASO1, FEC_AJU
		
		*For development
		*gsErrorMsg = ''
		*OPEN DATABASE BDSivigila
		*DO FORM (sFormName) WITH 'EVENTOS_91' &&, '"' + _ver + 'Evento: ' + m.cod_eve + ' ' + m.nom_eve 
		
		*For production
		DO FORM (sFormName) WITH 'EVENTOS_91', '"' + _ver + 'Evento: ' + m.cod_eve + ' ' + m.nom_eve 

	CASE sFormName = 'IMPORTAR'
		IF bForTests THEN 
			*PUBLIC _dir_act 
			SET SAFETY OFF 
			
			_cod_acc='SIVIGILA  '
			_nomb_usua=''
			_ver=''

			_dir_act=sys(5)+sys(2003)+'\'

			_mydocument=ADDBS(GETENV("HOMEPATH"))+'DOCUMENTS\SIVIGILA - 2018'
			_RTA_PLANOS=_mydocument+'\ARCHIVOS PLANOS'
			_RTA_CONSOLIDA = _mydocument + "\ARCHIVOS DE CONSOLIDACION"
			
			NMAJOR=18
			NMINOR=1
			NBUILD=5
			
		ENDIF 
		
		DO FORM (sFormName)
		
		IF bForTests THEN 
			RELEASE tecla_pres, _cod_acc, _rta, specialChars, dSivigila2010LaunchDate, _nomb_usua, _ver, _car_esp, _dir, RemoveSpecialChars, ;
					goErrorlog, gsErrorMsg 
			SET SAFETY ON 
		ENDIF
	
	CASE sFormName = 'FRMEXPORTTOFILE' OR sFormName = 'EXPORTAR'
		IF bForTests THEN 
			SET SAFETY OFF 
			
			tecla_pres='LASTKEY()=5 OR LASTKEY()=19 OR LASTKEY()=127 OR LASTKEY()=31 OR LASTKEY()=27'
			_cod_acc='SIVIGILA  '
			_mydocument=ADDBS(GETENV("HOMEDRIVE")+GETENV("HOMEPATH"))+'DOCUMENTS\SIVIGILA - 2018'
			_RTA_PLANOS=_mydocument+'\ARCHIVOS PLANOS'
			_RTA_XLS=_mydocument+'\ARCHIVOS XLS'
			_nomb_usua=''
			*_rta=0
			*specialChars = "+.\/:*?',{}<>_&;^()=#$%[]" + '"' 
			*dSivigila2010LaunchDate = {^2018-02-05}
			_ver=''
			_vigencia='2019'
			sFollowupEvents=' 650 652 680 860 870 890 995 353 354 359 362 349 '
			*_car_esp = "This.value=chrtran(This.value,specialChars,'')"
			*_dir = SYS(2023)
			*RemoveSpecialChars = "This.value=chrtran(This.value,specialChars,'')"
			
			*goErrorlog = NEWOBJECT( 'xErrLog', 'errorlog.prg', NULL )
			*gsErrorMsg = ""

			NMAJOR=18
			NMINOR=1
			NBUILD=6
			
			*OPEN DATABASE BDSivigila
		ENDIF

		DO FORM (sFormName) WITH VAL(sParams)

		IF bForTests THEN 
			RELEASE tecla_pres, _cod_acc, _rta, specialChars, dSivigila2010LaunchDate, _nomb_usua, _ver, _car_esp, _dir, RemoveSpecialChars, ;
					goErrorlog, gsErrorMsg 
			CLOSE DATABASES 
		ENDIF

	CASE sFormName = 'FRMCONTACTOS'
		*Inicializa variables necesarias para el buen funcionamiento del formulario de captura exceptuando aquellas a las que se asignan ;
		valore por defecto
		STORE '' TO ID, PRI_NOM, SEG_NOM, PRI_APE, SEG_APE, TIP_IDE, NUM_IDE, TIPIDE_POS, NUMIDE_POS, COD_EVEPOS, EDAD, UNI_MED, SEXO, DIR_RES, ;
					OCUPACION, TIP_SS, COD_ASE, PER_ETN, GP_DISCAPA, GP_DESPLAZ, GP_MIGRANT, GP_CARCELA, GP_GESTAN, GP_INDIGEN, GP_POBICFB, ;
					GP_MAD_COM, GP_DESMOVI, GP_PSIQUIA, GP_VIC_VIO, GP_OTROS, COD_MUN_R, TELEFONO, VERSION, UNI_MODIF, ;
					EST_INGR, GRUPO_ETNI, NACIONALID, DESPLAZAMI, COD_MUN_D1, COD_MUN_D2, COD_MUN_D3, COD_MUN_D4, COD_MUN_D5, INST_LABOR, ;
					COR_ELE, ID_PADRE, TIPO_CONTA, ROL, TOMA_MUEST, RESULTADO, SINTOMATIC
		
		STORE {//} TO FEC_NOT, FECHA_NTO, FEC_POS_EX, FEC_TOM_MU, FEC_INI_SI
		STORE 0 TO NOTIFICA
		
		IF bForTests THEN 
			DATA_VALIDATION_ERROR = 1582
			RECORD_VALIDATION_ERROR = 1583
			goErrorlog = NEWOBJECT( 'xErrLog', 'errorlog.prg', NULL )
		
			sCmd = "DO FORM " + sFormName + " WITH  " + sParams
			&sCmd

			RELEASE goErrorLog
		ELSE 
			DO FORM (sFormName) WITH 'Contactos','',_RTA_XLS,0
		ENDIF 

	CASE sFormName = 'FRMSEGUIMIENTOSCONTACTOS'
		DATA_VALIDATION_ERROR = 1582
		RECORD_VALIDATION_ERROR = 1583
		goErrorlog = NEWOBJECT( 'xErrLog', 'errorlog.prg', NULL )

		sCmd = "DO FORM " + sFormName + " WITH  " + sParams
		&sCmd

		IF bForTests THEN 
			RELEASE goErrorLog
		ENDIF

	CASE sFormName = 'FRMEVENTOS_94'
		OPEN DATABASE BDSivigila
		DO FORM FRMEVENTOS_94 WITH 'EVENTOS_94'

ENDCASE


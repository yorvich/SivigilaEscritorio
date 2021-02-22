FUNCTION FieldRuleFor_Paciente_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_FEC_NOT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_NOT))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_PRI_NOM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PRI_NOM)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','PRI_NOM'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(PRI_NOM)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_PRI_APE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PRI_APE)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','PRI_APE'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(PRI_APE)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE))) OR COD_EVE='000'
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE)) OR COD_EVE='000'
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_EDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EDAD)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','EDAD'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(EDAD)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_UNI_MED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(UNI_MED)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','UNI_MED'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(UNI_MED,'0','1','2','3','4','5')) AND ( .NOT. EMPTY(UNI_MED))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_SEXO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SEXO)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','SEXO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SEXO,'M','F','I')) AND ( .NOT. EMPTY(SEXO))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_COD_MUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(COD_MUN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','COD_MUN'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(COD_MUN)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_AREA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AREA)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','AREA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AREA,'1','2','3')) AND ( .NOT. EMPTY(AREA))) OR INLIST(COD_EVE,'357','351','352') OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_DIR_RES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(DIR_RES)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','DIR_RES'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(DIR_RES)) OR INLIST(COD_EVE,'357','351','352') OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_OCUPACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(OCUPACION)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','OCUPACION'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(OCUPACION)) OR INLIST(COD_EVE,'357','351','352') OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_TIP_SS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(TIP_SS)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','TIP_SS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(TIP_SS,'C','S','P','E','N','I')) AND ( .NOT. EMPTY(TIP_SS))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_PER_ETN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PER_ETN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','PER_ETN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(PER_ETN,'1','2','3','4','5','6')) AND ( .NOT. EMPTY(PER_ETN))) OR INLIST(COD_EVE,'357','351','352') OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_DISCAPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_DISCAPA)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_DISCAPA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_DISCAPA,'1','2')) OR ( EMPTY(GP_DISCAPA))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_DESPLAZ
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_DESPLAZ)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_DESPLAZ'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_DESPLAZ,'1','2')) OR ( EMPTY(GP_DESPLAZ))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_MIGRANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_MIGRANT)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_MIGRANT'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_MIGRANT,'1','2')) OR ( EMPTY(GP_MIGRANT))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_CARCELA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_CARCELA)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_CARCELA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_CARCELA,'1','2')) OR ( EMPTY(GP_CARCELA))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_GESTAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_GESTAN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_GESTAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_GESTAN,'1','2')) OR ( EMPTY(GP_GESTAN))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_INDIGEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_INDIGEN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_INDIGEN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_INDIGEN,'1','2')) OR ( EMPTY(GP_INDIGEN))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_POBICFB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_POBICFB)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_POBICFB'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_POBICFB,'1','2')) OR ( EMPTY(GP_POBICFB))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_MAD_COM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_MAD_COM)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_MAD_COM'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_MAD_COM,'1','2')) OR ( EMPTY(GP_MAD_COM))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_DESMOVI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_DESMOVI)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_DESMOVI'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_DESMOVI,'1','2')) OR ( EMPTY(GP_DESMOVI))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_PSIQUIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_PSIQUIA)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_PSIQUIA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_PSIQUIA,'1','2')) OR ( EMPTY(GP_PSIQUIA))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_VIC_VIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_VIC_VIO)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_VIC_VIO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_VIC_VIO,'1','2')) OR ( EMPTY(GP_VIC_VIO))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_GP_OTROS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(GP_OTROS)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','GP_OTROS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(GP_OTROS,'1','2')) OR ( EMPTY(GP_OTROS))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_MUN_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(MUN_PRO)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','MUN_PRO'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(MUN_PRO)) OR INLIST(COD_EVE,'357','351','352') OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_TIP_CAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(TIP_CAS)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','TIP_CAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(TIP_CAS,'1','2','3','4','5')) AND ( .NOT. EMPTY(TIP_CAS))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_PAC_HOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PAC_HOS)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','PAC_HOS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(PAC_HOS,'1','2')) OR ( EMPTY(PAC_HOS))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_CON_FIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CON_FIN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','CON_FIN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CON_FIN,'0','1','2')) AND ( .NOT. EMPTY(CON_FIN))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AJUSTE)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','AJUSTE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_TELEFONO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(TELEFONO)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','TELEFONO'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(TELEFONO)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_NOM_DIL_FI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NOM_DIL_FI)) OR COD_EVE='000'
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_TEL_DIL_FI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(TEL_DIL_FI)) OR COD_EVE='000'
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_FEC_AJU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_AJU)) OR COD_EVE='000'
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ESTADOTRAN)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','ESTADOTRAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ESTADOTRAN,'3','5','2')) AND ( .NOT. EMPTY(ESTADOTRAN))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_FM_FUERZA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FM_FUERZA)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','FM_FUERZA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FM_FUERZA,'1','2','3','4','5','6','7','8')) OR ( EMPTY(FM_FUERZA))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_EST_INGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EST_INGR)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','EST_INGR'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EST_INGR,0,2,3)) OR (EST_INGR=0)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_FUENTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FUENTE)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','FUENTE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FUENTE,'1','2','3','4','5')) OR ( EMPTY(FUENTE))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_ESTRATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ESTRATO)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','ESTRATO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ESTRATO,'1','2','3','4','5','6')) OR ( EMPTY(ESTRATO))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_SEM_GES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SEM_GES)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','SEM_GES'))) THEN
			RETURN .T.
		ELSE
			RETURN (( VAL(SEM_GES)>=1  AND  VAL(SEM_GES) <=45 AND ISNUMERIC(SEM_GES)) OR ( EMPTY(SEM_GES))) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_LAT_DIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LAT_DIR)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','LAT_DIR'))) THEN
			RETURN .T.
		ELSE
			RETURN ((LAT_DIR>=-90  AND LAT_DIR <= 90 AND ISNUMERIC(LAT_DIR)) OR (LAT_DIR=0)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_LONG_DIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LONG_DIR)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','LONG_DIR'))) THEN
			RETURN .T.
		ELSE
			RETURN ((LONG_DIR>=-180  AND LONG_DIR <= 180 AND ISNUMERIC(LONG_DIR)) OR (LONG_DIR=0)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_Paciente_CONFGEODIR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CONFGEODIR)) OR (bIsInmediateNotification AND !isAvoidable(Paciente.COD_EVE,'Paciente','CONFGEODIR'))) THEN
			RETURN .T.
		ELSE
			RETURN ((CONFGEODIR>=0  AND CONFGEODIR <= 100 AND ISNUMERIC(CONFGEODIR)) OR (CONFGEODIR=0)) OR COD_EVE='000'
		ENDIF
	ENDIF
ENDFUNC



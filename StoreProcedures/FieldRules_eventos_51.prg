FUNCTION FieldRuleFor_eventos_51_EVI_MLEGAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EVI_MLEGAL)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','EVI_MLEGAL'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EVI_MLEGAL,'1','2')) OR ( EMPTY(EVI_MLEGAL)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ESTADOTRAN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ESTADOTRAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_NATURALEZA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(NATURALEZA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','NATURALEZA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(NATURALEZA,'1','2','3')) OR ( EMPTY(NATURALEZA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ACTIVIDAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ACTIVIDAD)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ACTIVIDAD'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ACTIVIDAD,'13','24','26','28','29','30','31','32','33')) AND ( .NOT. EMPTY(ACTIVIDAD)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_CONSUM_SPA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CONSUM_SPA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','CONSUM_SPA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CONSUM_SPA,'1','2')) AND ( .NOT. EMPTY(CONSUM_SPA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_MUJER_CABF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(MUJER_CABF)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','MUJER_CABF'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(MUJER_CABF,'1','2')) AND ( .NOT. EMPTY(MUJER_CABF)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ANTEC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ANTEC)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ANTEC'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ANTEC,'1','2')) AND ( .NOT. EMPTY(ANTEC)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_SEXO_AGRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SEXO_AGRE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','SEXO_AGRE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SEXO_AGRE,'M','F','I')) OR ( EMPTY(SEXO_AGRE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_R_FAM_VIC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(R_FAM_VIC)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','R_FAM_VIC'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(R_FAM_VIC,'9','10','22','23','24','25')) OR ( EMPTY(R_FAM_VIC)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_CONV_AGRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CONV_AGRE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','CONV_AGRE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CONV_AGRE,'1','2')) AND ( .NOT. EMPTY(CONV_AGRE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_R_NOFILIAR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(R_NOFILIAR)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','R_NOFILIAR'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(R_NOFILIAR,'1','2','3','4','6','7','8','10','11','12','13')) OR ( EMPTY(R_NOFILIAR)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ARMAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ARMAS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ARMAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ARMAS,'1','2','3','4','11','12','13','14','15','16')) OR ( EMPTY(ARMAS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_CARA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_CARA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_CARA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_CARA,'1','2')) OR ( EMPTY(QUE_CARA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_CUELLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_CUELLO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_CUELLO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_CUELLO,'1','2')) OR ( EMPTY(QUE_CUELLO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_MANO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MANO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_MANO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MANO,'1','2')) OR ( EMPTY(QUE_MANO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_PIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_PIE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_PIE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_PIE,'1','2')) OR ( EMPTY(QUE_PIE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_PLIEGU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_PLIEGU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_PLIEGU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_PLIEGU,'1','2')) OR ( EMPTY(QUE_PLIEGU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_GENITA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_GENITA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_GENITA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_GENITA,'1','2')) OR ( EMPTY(QUE_GENITA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_TRONCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_TRONCO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_TRONCO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_TRONCO,'1','2')) OR ( EMPTY(QUE_TRONCO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_MIESUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MIESUP)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_MIESUP'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MIESUP,'1','2')) OR ( EMPTY(QUE_MIESUP)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_QUE_MIEINF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MIEINF)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','QUE_MIEINF'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MIEINF,'1','2')) OR ( EMPTY(QUE_MIEINF)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_CLA_GRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CLA_GRA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','CLA_GRA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CLA_GRA,'1','2','3')) OR ( EMPTY(CLA_GRA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_EXT_QUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EXT_QUE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','EXT_QUE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EXT_QUE,'1','2','3')) OR ( EMPTY(EXT_QUE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_SUST_VICT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SUST_VICT)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','SUST_VICT'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SUST_VICT,'1','2')) AND ( .NOT. EMPTY(SUST_VICT)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_FEC_HECHO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FEC_HECHO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','FEC_HECHO'))) THEN
			RETURN .T.
		ELSE
			RETURN ( .NOT. EMPTY(FEC_HECHO))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ESCENARIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ESCENARIO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ESCENARIO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ESCENARIO,'1','2','3','4','7','8','9','10','11','12')) AND ( .NOT. EMPTY(ESCENARIO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ZONA_CONF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ZONA_CONF)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ZONA_CONF'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ZONA_CONF,'1','2')) AND ( .NOT. EMPTY(ZONA_CONF)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AC_MENTAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AC_MENTAL)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','AC_MENTAL'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AC_MENTAL,'1','2')) AND ( .NOT. EMPTY(AC_MENTAL)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_SP_ITS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SP_ITS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','SP_ITS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SP_ITS,'1','2')) OR ( EMPTY(SP_ITS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_PROF_HEP_B
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PROF_HEP_B)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','PROF_HEP_B'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(PROF_HEP_B,'1','2')) OR ( EMPTY(PROF_HEP_B)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_PROF_OTRAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(PROF_OTRAS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','PROF_OTRAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(PROF_OTRAS,'1','2')) OR ( EMPTY(PROF_OTRAS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AC_ANTICON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AC_ANTICON)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','AC_ANTICON'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AC_ANTICON,'1','2')) OR ( EMPTY(AC_ANTICON)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AC_IVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AC_IVE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','AC_IVE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AC_IVE,'1','2')) OR ( EMPTY(AC_IVE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_INF_AUT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(INF_AUT)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','INF_AUT'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(INF_AUT,'1','2')) AND ( .NOT. EMPTY(INF_AUT)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_REMIT_PROT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(REMIT_PROT)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','REMIT_PROT'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(REMIT_PROT,'1','2')) AND ( .NOT. EMPTY(REMIT_PROT)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AJUSTE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','AJUSTE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_NAT_VIOSEX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(NAT_VIOSEX)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','NAT_VIOSEX'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(NAT_VIOSEX,'5','6','7','10','12','14','15')) OR ( EMPTY(NAT_VIOSEX)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_ORIENT_SEX
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ORIENT_SEX)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','ORIENT_SEX'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ORIENT_SEX,'1','2','5','6')) AND ( .NOT. EMPTY(ORIENT_SEX)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_IDENT_GENE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(IDENT_GENE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','IDENT_GENE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(IDENT_GENE,'1','2','3')) AND ( .NOT. EMPTY(IDENT_GENE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_AMBITO_LUG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMBITO_LUG)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','AMBITO_LUG'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMBITO_LUG,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(AMBITO_LUG)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_51_TIPO_GRUPO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(TIPO_GRUPO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_51.COD_EVE,'eventos_51','TIPO_GRUPO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(TIPO_GRUPO,'1','2')) OR ( EMPTY(TIPO_GRUPO)))
		ENDIF
	ENDIF
ENDFUNC



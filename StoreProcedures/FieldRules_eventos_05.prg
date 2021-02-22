FUNCTION FieldRuleFor_eventos_05_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_A�O
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(A�O))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_TIP_AGR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_AGR,'1','2','3','6','7','8')) AND ( .NOT. EMPTY(TIP_AGR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_AREA_MORDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AREA_MORDE,'1','2')) OR ( EMPTY(AREA_MORDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_AGR_PRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AGR_PRO,'1','2')) AND ( .NOT. EMPTY(AGR_PRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_TIP_LES
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_LES,'1','2')) AND ( .NOT. EMPTY(TIP_LES)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_PROFUN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PROFUN,'1','2')) AND ( .NOT. EMPTY(PROFUN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_CCC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CCC,'1','2')) AND ( .NOT. EMPTY(CCC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_MAN_DED
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MAN_DED,'1','2')) AND ( .NOT. EMPTY(MAN_DED)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_TRONCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRONCO,'1','2')) AND ( .NOT. EMPTY(TRONCO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_MIE_SUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIE_SUP,'1','2')) AND ( .NOT. EMPTY(MIE_SUP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_MIE_INF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIE_INF,'1','2')) AND ( .NOT. EMPTY(MIE_INF)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_PIES_DEDOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PIES_DEDOS,'1','2')) AND ( .NOT. EMPTY(PIES_DEDOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_GENIT_EXT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(GENIT_EXT,'1','2')) AND ( .NOT. EMPTY(GENIT_EXT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_FEC_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_EXP))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_ESP_ANI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_ANI,'1','2','3','4','5','7','8','9','10','12','13','14')) AND ( .NOT. EMPTY(ESP_ANI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_ANT_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_VAC,'1','2','3')) OR ( EMPTY(ANT_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_CAR_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CAR_VAC,'1','2')) OR ( EMPTY(CAR_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_EST_MA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EST_MA,'1','2','3')) OR ( EMPTY(EST_MA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_ESTADO_ANI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADO_ANI,'1','2','3')) OR ( EMPTY(ESTADO_ANI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_UBICACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(UBICACION,'1','2')) OR ( EMPTY(UBICACION)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_TIP_EXP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_EXP,'0','1','2')) AND ( .NOT. EMPTY(TIP_EXP)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_SUE_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUE_ANT,'1','2','3')) AND ( .NOT. EMPTY(SUE_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_VAC_ANT
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VAC_ANT,'1','2','3')) AND ( .NOT. EMPTY(VAC_ANT)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_LE_AGU_JAB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(LE_AGU_JAB,'1','2')) AND ( .NOT. EMPTY(LE_AGU_JAB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_SUT_HER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUT_HER,'1','2')) AND ( .NOT. EMPTY(SUT_HER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_APL_SA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_SA,'1','2')) AND ( .NOT. EMPTY(APL_SA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_APL_VAC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(APL_VAC,'1','2')) AND ( .NOT. EMPTY(APL_VAC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_05_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC



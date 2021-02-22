FUNCTION FieldRuleFor_eventos_67_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_A�O
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(A�O))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_COD_ENFERM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_ENFERM))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_NIVEL_EDUC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NIVEL_EDUC,'1','2','3','4','5','6','7','8','9','10','11','12','13')) AND ( .NOT. EMPTY(NIVEL_EDUC)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_TRAB_URBAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAB_URBAN,'1','2')) AND ( .NOT. EMPTY(TRAB_URBAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_TRAB_RURAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TRAB_RURAL,'1','2')) AND ( .NOT. EMPTY(TRAB_RURAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_JOV_VUL_RU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JOV_VUL_RU,'1','2')) AND ( .NOT. EMPTY(JOV_VUL_RU)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_JOV_VUL_UR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(JOV_VUL_UR,'1','2')) AND ( .NOT. EMPTY(JOV_VUL_UR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_SISNER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_SISNER,'1','2')) AND ( .NOT. EMPTY(DIS_SISNER)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_OJOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_OJOS,'1','2')) AND ( .NOT. EMPTY(DIS_OJOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_OIDOS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_OIDOS,'1','2')) AND ( .NOT. EMPTY(DIS_OIDOS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_OTRSEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_OTRSEN,'1','2')) AND ( .NOT. EMPTY(DIS_OTRSEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_VOZHAB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_VOZHAB,'1','2')) AND ( .NOT. EMPTY(DIS_VOZHAB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_CARDIO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_CARDIO,'1','2')) AND ( .NOT. EMPTY(DIS_CARDIO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_DIGEST
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_DIGEST,'1','2')) AND ( .NOT. EMPTY(DIS_DIGEST)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_SISGEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_SISGEN,'1','2')) AND ( .NOT. EMPTY(DIS_SISGEN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_MOVIMI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_MOVIMI,'1','2')) AND ( .NOT. EMPTY(DIS_MOVIMI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_PIEL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_PIEL,'1','2')) AND ( .NOT. EMPTY(DIS_PIEL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_DIS_OTRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(DIS_OTRA,'1','2')) AND ( .NOT. EMPTY(DIS_OTRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_NO_DEFINID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NO_DEFINID,'1','2')) AND ( .NOT. EMPTY(NO_DEFINID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_FEC_DIAGNO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(FEC_DIAGNO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_67_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC



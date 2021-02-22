FUNCTION FieldRuleFor_eventos_60_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_LACERACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LACERACION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','LACERACION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(LACERACION,'1','2')) AND ( .NOT. EMPTY(LACERACION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_CONTUSION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CONTUSION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','CONTUSION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CONTUSION,'1','2')) AND ( .NOT. EMPTY(CONTUSION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUEMADURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUEMADURA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUEMADURA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUEMADURA,'1','2')) AND ( .NOT. EMPTY(QUEMADURA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_CARA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_CARA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_CARA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_CARA,'1','2')) OR ( EMPTY(QUE_CARA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_CUELLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_CUELLO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_CUELLO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_CUELLO,'1','2')) OR ( EMPTY(QUE_CUELLO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_MANO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MANO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_MANO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MANO,'1','2')) OR ( EMPTY(QUE_MANO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_PIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_PIE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_PIE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_PIE,'1','2')) OR ( EMPTY(QUE_PIE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_PLIEGU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_PLIEGU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_PLIEGU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_PLIEGU,'1','2')) OR ( EMPTY(QUE_PLIEGU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_GENITA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_GENITA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_GENITA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_GENITA,'1','2')) OR ( EMPTY(QUE_GENITA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_TRONCO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_TRONCO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_TRONCO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_TRONCO,'1','2')) OR ( EMPTY(QUE_TRONCO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_MIESUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MIESUP)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_MIESUP'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MIESUP,'1','2')) OR ( EMPTY(QUE_MIESUP)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_QUE_MIEINF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUE_MIEINF)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','QUE_MIEINF'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUE_MIEINF,'1','2')) OR ( EMPTY(QUE_MIEINF)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_CLA_GRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CLA_GRA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','CLA_GRA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CLA_GRA,'1','2','3')) OR ( EMPTY(CLA_GRA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_EXT_QUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EXT_QUE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','EXT_QUE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EXT_QUE,'1','2','3')) OR ( EMPTY(EXT_QUE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMPUTACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMPUTACION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMPUTACION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMPUTACION,'1','2')) AND ( .NOT. EMPTY(AMPUTACION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_DEDMAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_DEDMAN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_DEDMAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_DEDMAN,'1','2')) OR ( EMPTY(AMP_DEDMAN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_MANO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_MANO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_MANO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_MANO,'1','2')) OR ( EMPTY(AMP_MANO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_ANTEBR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_ANTEBR)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_ANTEBR'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_ANTEBR,'1','2')) OR ( EMPTY(AMP_ANTEBR)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_BRAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_BRAZO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_BRAZO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_BRAZO,'1','2')) OR ( EMPTY(AMP_BRAZO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_MUSLO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_MUSLO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_MUSLO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_MUSLO,'1','2')) OR ( EMPTY(AMP_MUSLO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_PIERNA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_PIERNA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_PIERNA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_PIERNA,'1','2')) OR ( EMPTY(AMP_PIERNA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_PIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_PIE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_PIE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_PIE,'1','2')) OR ( EMPTY(AMP_PIE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AMP_DEDPIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMP_DEDPIE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AMP_DEDPIE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMP_DEDPIE,'1','2')) OR ( EMPTY(AMP_DEDPIE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_DAÑ_OCU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(DAÑ_OCU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','DAÑ_OCU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(DAÑ_OCU,'1','2')) AND ( .NOT. EMPTY(DAÑ_OCU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_DAÑ_AUD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(DAÑ_AUD)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','DAÑ_AUD'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(DAÑ_AUD,'1','2')) AND ( .NOT. EMPTY(DAÑ_AUD)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRACTURAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRACTURAS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRACTURAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRACTURAS,'1','2')) AND ( .NOT. EMPTY(FRACTURAS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_CRANEO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_CRANEO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_CRANEO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_CRANEO,'1','2')) OR ( EMPTY(FRA_CRANEO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_HUEMAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_HUEMAN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_HUEMAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_HUEMAN,'1','2')) OR ( EMPTY(FRA_HUEMAN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_HUEPIE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_HUEPIE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_HUEPIE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_HUEPIE,'1','2')) OR ( EMPTY(FRA_HUEPIE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_REJA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_REJA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_REJA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_REJA,'1','2')) OR ( EMPTY(FRA_REJA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_VIA_AER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(VIA_AER)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','VIA_AER'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(VIA_AER,'1','2')) AND ( .NOT. EMPTY(VIA_AER)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_ABDOMEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ABDOMEN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','ABDOMEN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ABDOMEN,'1','2')) AND ( .NOT. EMPTY(ABDOMEN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_OTRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(OTRO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','OTRO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(OTRO,'1','2')) AND ( .NOT. EMPTY(OTRO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_ARTEF_LESI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ARTEF_LESI)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','ARTEF_LESI'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ARTEF_LESI,'1','2','3')) AND ( .NOT. EMPTY(ARTEF_LESI)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_ARTEFACTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ARTEFACTO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','ARTEFACTO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ARTEFACTO,'1','2','3','4','5','6','7','8','9','10')) OR ( EMPTY(ARTEFACTO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_CON_ALC_LE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CON_ALC_LE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','CON_ALC_LE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CON_ALC_LE,'1','2')) OR ( EMPTY(CON_ALC_LE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_CON_ALC_AC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CON_ALC_AC)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','CON_ALC_AC'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CON_ALC_AC,'1','2')) OR ( EMPTY(CON_ALC_AC)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_LUG_OCU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LUG_OCU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','LUG_OCU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(LUG_OCU,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(LUG_OCU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_ACT_POLVOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ACT_POLVOR)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','ACT_POLVOR'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ACT_POLVOR,'1','2','3','4','5','6','7')) OR ( EMPTY(ACT_POLVOR)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_ACT_MINAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ACT_MINAS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','ACT_MINAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ACT_MINAS,'1','2','3','4','5')) OR ( EMPTY(ACT_MINAS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AJUSTE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','AJUSTE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_MIESUP
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_MIESUP)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_MIESUP'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_MIESUP,'1','2')) OR ( EMPTY(FRA_MIESUP)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_COLUMN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_COLUMN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_COLUMN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_COLUMN,'1','2')) OR ( EMPTY(FRA_COLUMN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_CADERA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_CADERA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_CADERA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_CADERA,'1','2')) OR ( EMPTY(FRA_CADERA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_60_FRA_MIEINF
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRA_MIEINF)) OR (bIsInmediateNotification AND !isAvoidable(eventos_60.COD_EVE,'eventos_60','FRA_MIEINF'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRA_MIEINF,'1','2')) OR ( EMPTY(FRA_MIEINF)))
		ENDIF
	ENDIF
ENDFUNC



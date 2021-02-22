FUNCTION FieldRuleFor_eventos_26_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_EST_SOC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EST_SOC)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','EST_SOC'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EST_SOC,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(EST_SOC)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_ESCOLARIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ESCOLARIDA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','ESCOLARIDA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ESCOLARIDA,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(ESCOLARIDA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_AMPUTACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AMPUTACION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','AMPUTACION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AMPUTACION,'0','3','4','5','6','7','8','9','10','11')) AND ( .NOT. EMPTY(AMPUTACION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_LACERACION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LACERACION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','LACERACION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(LACERACION,'1','2')) AND ( .NOT. EMPTY(LACERACION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CONTUSION
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CONTUSION)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CONTUSION'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CONTUSION,'1','2')) AND ( .NOT. EMPTY(CONTUSION)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_HERIDA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(HERIDA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','HERIDA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(HERIDA,'1','2')) AND ( .NOT. EMPTY(HERIDA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_QUEMADURA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(QUEMADURA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','QUEMADURA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(QUEMADURA,'1','2')) AND ( .NOT. EMPTY(QUEMADURA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_SIT_QUEMAD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SIT_QUEMAD)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','SIT_QUEMAD'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SIT_QUEMAD,'1','2','3','4','5','6')) OR ( EMPTY(SIT_QUEMAD)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CLA_GRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CLA_GRA)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CLA_GRA'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CLA_GRA,'1','2','3','4','5')) OR ( EMPTY(CLA_GRA)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_EXT_QUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(EXT_QUE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','EXT_QUE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(EXT_QUE,'1','2','3','4')) OR ( EMPTY(EXT_QUE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_DAÑ_OCU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(DAÑ_OCU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','DAÑ_OCU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(DAÑ_OCU,'0','3','4','5')) AND ( .NOT. EMPTY(DAÑ_OCU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_DAÑ_AUD
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(DAÑ_AUD)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','DAÑ_AUD'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(DAÑ_AUD,'0','3','4','5')) AND ( .NOT. EMPTY(DAÑ_AUD)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_FRACTURAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(FRACTURAS)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','FRACTURAS'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(FRACTURAS,'0','1','2','3','4','5','6','7','8','9')) AND ( .NOT. EMPTY(FRACTURAS)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_SIN_DATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SIN_DATO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','SIN_DATO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SIN_DATO,'1','2')) AND ( .NOT. EMPTY(SIN_DATO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_VIA_AER
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(VIA_AER)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','VIA_AER'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(VIA_AER,'1','2')) AND ( .NOT. EMPTY(VIA_AER)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_ABDOMEN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ABDOMEN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','ABDOMEN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ABDOMEN,'1','2')) AND ( .NOT. EMPTY(ABDOMEN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_SA_SINDATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SA_SINDATO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','SA_SINDATO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SA_SINDATO,'1','2')) AND ( .NOT. EMPTY(SA_SINDATO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_SA_OTRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(SA_OTRO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','SA_OTRO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(SA_OTRO,'1','2')) AND ( .NOT. EMPTY(SA_OTRO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CIRCUNSTAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CIRCUNSTAN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CIRCUNSTAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CIRCUNSTAN,'1','2','3','4','5','6','7','8','9','10')) AND ( .NOT. EMPTY(CIRCUNSTAN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_INTENC_MAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(INTENC_MAN)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','INTENC_MAN'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(INTENC_MAN,'1','2','3','4','5','6','7')) OR ( EMPTY(INTENC_MAN)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_LUG_OCU
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(LUG_OCU)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','LUG_OCU'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(LUG_OCU,'1','2','3','4','5','7','6')) AND ( .NOT. EMPTY(LUG_OCU)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CIR_SOCIAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CIR_SOCIAL)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CIR_SOCIAL'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CIR_SOCIAL,'1','2','3','4','5','6','7')) AND ( .NOT. EMPTY(CIR_SOCIAL)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CON_ALC_LE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CON_ALC_LE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CON_ALC_LE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CON_ALC_LE,'1','2')) OR ( EMPTY(CON_ALC_LE)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_CON_ALC_AC
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(CON_ALC_AC)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','CON_ALC_AC'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(CON_ALC_AC,'1','2')) OR ( EMPTY(CON_ALC_AC)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_ARTEF_LESI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ARTEF_LESI)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','ARTEF_LESI'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ARTEF_LESI,'1','2','3','4','5')) AND ( .NOT. EMPTY(ARTEF_LESI)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_ARTEFACTO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(ARTEFACTO)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','ARTEFACTO'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(ARTEFACTO,'1','2','3','4','5','6','7','8','10','11')) OR ( EMPTY(ARTEFACTO)))
		ENDIF
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_26_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		bIsInmediateNotification = IsInmediateNotification()
		IF  !((!bIsInmediateNotification OR !EMPTY(AJUSTE)) OR (bIsInmediateNotification AND !isAvoidable(eventos_26.COD_EVE,'eventos_26','AJUSTE'))) THEN
			RETURN .T.
		ELSE
			RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
		ENDIF
	ENDIF
ENDFUNC



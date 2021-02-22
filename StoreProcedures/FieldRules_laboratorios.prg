FUNCTION FieldRuleFor_laboratorios_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_AÑO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AÑO))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_CONTROL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(CONTROL))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_PRI_NOM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PRI_NOM))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_PRI_APE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PRI_APE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_MUESTRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MUESTRA,'0','1','2','3','4','6','7','8','10','11','12','13','14','15','16','17','18','19','20','21','22','31','32','33','34')) AND ( .NOT. EMPTY(MUESTRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_PRUEBA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(PRUEBA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_AGENTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(AGENTE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_RESULTADO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESULTADO,'1','2','3','4','5','6','7','9','10','11','12','13','14','15','16','17','18','19')) AND ( .NOT. EMPTY(RESULTADO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_TIP_REG_SA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_REG_SA,'C','S','P','E','N')) OR ( EMPTY(TIP_REG_SA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_laboratorios_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'3','5','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC



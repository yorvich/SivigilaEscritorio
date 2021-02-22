FUNCTION FieldRuleFor_eventos_18_SEMANA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(SEMANA))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_A�O
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(A�O))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COD_PRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_PRE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COD_SUB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_SUB))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COD_EVE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(COD_EVE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_TIP_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(TIP_IDE,'RC','TI','CC','CE','PA','MS','AS','PE','CN')) AND ( .NOT. EMPTY(TIP_IDE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_NUM_IDE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN ( .NOT. EMPTY(NUM_IDE))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_VIAJO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(VIAJO,'1','2','3')) AND ( .NOT. EMPTY(VIAJO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_PAD_MAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PAD_MAL,'1','2','3')) AND ( .NOT. EMPTY(PAD_MAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_AUTOMEDICA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AUTOMEDICA,'1','2','3')) AND ( .NOT. EMPTY(AUTOMEDICA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ANT_TRA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ANT_TRA,'1','2','3')) AND ( .NOT. EMPTY(ANT_TRA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_CEREBR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_CEREBR,'1','2')) AND ( .NOT. EMPTY(COM_CEREBR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_RENAL,'1','2')) AND ( .NOT. EMPTY(COM_RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_HEPATI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_HEPATI,'1','2')) AND ( .NOT. EMPTY(COM_HEPATI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_PULMON
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_PULMON,'1','2')) AND ( .NOT. EMPTY(COM_PULMON)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_HEMATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_HEMATO,'1','2')) AND ( .NOT. EMPTY(COM_HEMATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COM_OTRAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COM_OTRAS,'1','2')) AND ( .NOT. EMPTY(COM_OTRAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_FIEBRE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(FIEBRE,'1','2')) AND ( .NOT. EMPTY(FIEBRE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CEFALEA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CEFALEA,'1','2')) AND ( .NOT. EMPTY(CEFALEA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ESCALOFR�O
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESCALOFR�O,'1','2')) AND ( .NOT. EMPTY(ESCALOFR�O)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_SUDORACI�N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SUDORACI�N,'1','2')) AND ( .NOT. EMPTY(SUDORACI�N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_MIALGIAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MIALGIAS,'1','2')) AND ( .NOT. EMPTY(MIALGIAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HIPEREMESI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPEREMESI,'1','2')) AND ( .NOT. EMPTY(HIPEREMESI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_NAUSEAS
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(NAUSEAS,'1','2')) AND ( .NOT. EMPTY(NAUSEAS)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ASTENIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ASTENIA,'1','2')) AND ( .NOT. EMPTY(ASTENIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ADINAMIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ADINAMIA,'1','2')) AND ( .NOT. EMPTY(ADINAMIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HEMOGLOB
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMOGLOB,'1','2')) AND ( .NOT. EMPTY(HEMOGLOB)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_PLAQUETA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PLAQUETA,'1','2')) AND ( .NOT. EMPTY(PLAQUETA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HEMORRAGIA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEMORRAGIA,'1','2')) AND ( .NOT. EMPTY(HEMORRAGIA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CID
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CID,'1','2')) AND ( .NOT. EMPTY(CID)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HEPATOMEGA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEPATOMEGA,'1','2')) AND ( .NOT. EMPTY(HEPATOMEGA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ESPLENOMEG
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESPLENOMEG,'1','2')) AND ( .NOT. EMPTY(ESPLENOMEG)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_EDE_PUL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EDE_PUL,'1','2')) AND ( .NOT. EMPTY(EDE_PUL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HIPOTENSI�
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HIPOTENSI�,'1','2')) AND ( .NOT. EMPTY(HIPOTENSI�)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_RENAL
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RENAL,'1','2')) AND ( .NOT. EMPTY(RENAL)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_RESPIRATOR
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(RESPIRATOR,'1','2')) AND ( .NOT. EMPTY(RESPIRATOR)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_HEP�TICA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(HEP�TICA,'1','2')) AND ( .NOT. EMPTY(HEP�TICA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CONFUSI�N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONFUSI�N,'1','2')) AND ( .NOT. EMPTY(CONFUSI�N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_SOMNOLENCI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SOMNOLENCI,'1','2')) AND ( .NOT. EMPTY(SOMNOLENCI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CONVULSI�N
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CONVULSI�N,'1','2')) AND ( .NOT. EMPTY(CONVULSI�N)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COMA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COMA,'1','2')) AND ( .NOT. EMPTY(COMA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CHOQUE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CHOQUE,'1','2')) AND ( .NOT. EMPTY(CHOQUE)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ESP_PLA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESP_PLA,'1','2','3','4')) AND ( .NOT. EMPTY(ESP_PLA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_EMBARAZO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(EMBARAZO,'1','2','3')) AND ( .NOT. EMPTY(EMBARAZO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CLOROQUINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLOROQUINA,'1','2','3')) AND ( .NOT. EMPTY(CLOROQUINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_AMODIAQUIN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AMODIAQUIN,'1','2','3')) AND ( .NOT. EMPTY(AMODIAQUIN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_SULFAPIRIM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(SULFAPIRIM,'1','2','3')) AND ( .NOT. EMPTY(SULFAPIRIM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_PRIMAQUINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(PRIMAQUINA,'1','2','3')) AND ( .NOT. EMPTY(PRIMAQUINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_QUININA_O
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUININA_O,'1','2','3')) AND ( .NOT. EMPTY(QUININA_O)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_QUININA_I
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(QUININA_I,'1','2','3')) AND ( .NOT. EMPTY(QUININA_I)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_MEFLOQUINA
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(MEFLOQUINA,'1','2','3')) AND ( .NOT. EMPTY(MEFLOQUINA)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ARTESUNATO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ARTESUNATO,'1','2','3')) AND ( .NOT. EMPTY(ARTESUNATO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_COARTEM
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(COARTEM,'1','2','3')) AND ( .NOT. EMPTY(COARTEM)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_CLINDAMICI
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(CLINDAMICI,'1','2','3')) AND ( .NOT. EMPTY(CLINDAMICI)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_OTRO
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(OTRO,'1','2','3')) AND ( .NOT. EMPTY(OTRO)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_ESTADOTRAN
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(ESTADOTRAN,'1','2')) AND ( .NOT. EMPTY(ESTADOTRAN)))
	ENDIF
ENDFUNC


FUNCTION FieldRuleFor_eventos_18_AJUSTE
	IF DoNotApplyRules() THEN
		RETURN .T.
	ELSE
		RETURN (( INLIST(AJUSTE,'0','3','4','5','6','7','D')) OR ( EMPTY(AJUSTE)))
	ENDIF
ENDFUNC



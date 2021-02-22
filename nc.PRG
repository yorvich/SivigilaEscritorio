PARAMETERS dat_ent_fun, nLength as Integer, nDecimals as Integer

IF VARTYPE(nLength)='N' AND VARTYPE(nLength)='N' THEN
	dat_ent_fun=ALLTRIM(STR(dat_ent_fun,nLength,nDecimals))
ELSE
	dat_ent_fun=ALLTRIM(STR(dat_ent_fun))
ENDIF
RETURN dat_ent_fun

PARAMETER m.tit,txt_adi, _ti_sn

IF VARTYPE(_nomb_usua)!='C' 
	_nomb_usua=''
ENDIF
IF VARTYPE(_VER)!='C' 
	_VER=''
ENDIF
	
_rta=MESSAGEBOX(_nomb_usua+', ¿'+ALLT(M.TIT)+'?' , 4 + 32 + iif(!empty(_ti_sn),0,256), _VER+iif(empty(txt_adi),'',txt_adi) )
_ti_sn=0

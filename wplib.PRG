*------------------------------------------------
* WPLIB.PRG
*
* Funciones wrapper para la libreria
* ENUMPROC.DLL
*
* Autor: Victor Espina (vespinas@cantv.net)
* Fecha: Noviembre 2003
*
* Actualización Febrero 2005:
* - Nuevas funciones:
*   WPGetProcessIDByHWnd()
*   WPGetMainHWnd()
*   WPGetCurrentProcessID()
*   WPKillProcess()
*   WPKillCurrentProcess()
*
* - Se añadieron nuevas declaraciones en WPLoadAPI()
*-------------------------------------------------


*-- WPLoadAPI
*   Define las funciones API necesarias
*
PROC WPLoadAPI
 *
 DECLARE INTEGER OpenProcList IN enumproc.dll
 DECLARE INTEGER GetFirstProc  IN enumproc.dll ;
         INTEGER hhandle, INTEGER@ lpdwprocid, SHORT@ lpwprocid16, STRING@ lpzprocname
 DECLARE INTEGER GetNextProc  IN enumproc.dll ;
         INTEGER hhandle, INTEGER@ lpdwprocid, SHORT@ lpwprocid16, STRING@ lpzprocname
 DECLARE INTEGER CloseProcList IN enumproc.dll ;
         INTEGER hHandle
 DECLARE INTEGER OpenProcess IN kernel32 ;
         INTEGER dwDesiredAccess, INTEGER blnheritHandle, INTEGER dwAppProcessId
 DECLARE INTEGER TerminateProcess IN kernel32 ;
         INTEGER nPID, INTEGER exitcode
 DECLARE INTEGER CloseHandle IN kernel32 ;
         INTEGER hObject
 DECLARE INTEGER GetWindowThreadProcessId IN user32 ;
         INTEGER hWnd, INTEGER@ nProcID
 DECLARE INTEGER GetActiveWindow IN user32         
 *
ENDPROC



*-- WPEnumProcs
*   Llena un array pasado por referencia con la lista de procesos activos. La función
*   devuelve el nro. de elementos en el array o -1 se ocurrió algun error. El array
*   es llenado con dos columnas: la primera contiene el ID del proceso y la 2da 
*   el nombre en mayúscula del archivo asociado al proceso.
*
PROC WPEnumProcs(aProcs)
 *
 *-- Se cargan las funciones API necesarias
 *
 WPLoadAPI()
 
 *-- Se preparan las variables necesarias
 *
 local cprocname, nprocid, nprocid16, nhandle, nCount
 cprocname = Replicate(chr(0), 260)
 nprocid = 0
 nprocid16 = 0
 
 *-- Se intenta abrir la lista de procesos
 *
 nHandle = OpenProcList() 
 If nHandle = 0
  return -1
 Endif

 *-- Se obtiene la lista de procesos activos
 *
 If GetFirstProc(nHandle, @nprocid, @nprocid16, @cprocname) != 0
  dimen aProcs[1,2]
  aProcs[1,1]=iif(nProcID<>0,nProcID,nProcID16)
  aProcs[1,2]=upper(left(cprocname, at(chr(0), cprocname)-1))
  nCount=1
  Do while GetNextProc(nHandle, @nprocid, @nprocid16, @cprocname) != 0
   nCount=nCount + 1
   dimen aProcs[nCount,2]
   aProcs[nCount,1]=iif(nProcID<>0,nProcID,nProcID16)
   aProcs[nCount,2]=upper(left(cprocname, at(chr(0), cprocname)-1))
  EndDo
 Else
  return -1
 Endif


 *-- Se cierra la lista de procesos
 *
 CloseProcList(nHandle)


 *-- Se devuelve la cantidad de procesos en el array
 * 
 return nCount
 *
ENDPROC



*-- WPGetProcessID
*   Devuelve el nro. de ID de proceso para un programa dado, o -1 si ocurre algun error o 
*   no se encontró el programa indicado.
*
PROC WPGetProcessID(pcProcName)
 *
 *-- Se cargan las funciones API necesarias
 *
 WPLoadAPI()
 
 *-- Se preparan las variables necesarias
 *
 local cprocname, nprocid, nprocid16, nhandle, lFound
 cprocname = Replicate(chr(0), 260)
 nprocid = 0
 nprocid16 = 0
 
 *-- Se intenta abrir la lista de procesos
 *
 nHandle = OpenProcList() 
 If nHandle = 0
  return -1
 Endif

 *-- Se obtiene la lista de procesos activos
 *
 If GetFirstProc(nHandle, @nprocid, @nprocid16, @cprocname) != 0
  if upper(pcProcName) $ upper(left(cprocname, at(chr(0), cprocname)-1))
   lFound=.T.
  else
   Do while GetNextProc(nHandle, @nprocid, @nprocid16, @cprocname) != 0
    if upper(pcProcName) $ upper(left(cprocname, at(chr(0), cprocname)-1))
     lFound=.T.
     exit
    endif 
   EndDo
  endif
 Else
  return -1
 Endif


 *-- Se cierra la lista de procesos
 *
 CloseProcList(nHandle)


 *-- Se devuelve el ID del proceso
 * 
 if lFound
  return iif(nProcID<>0,nProcID,nProcID16)
 else
  return -1
 endif
 *
ENDPROC


*-- WPGetProcessIDByHWnd
*   Devuelve el ProcessID para una ventana dada.
*
PROC WPGetProcessIDByHWnd(pnHWnd)
 *
 *-- Se cargan las funciones API necesarias
 *
 WPLoadAPI()


 *-- Se obtiene el ProcessID para la ventana indicada 
 LOCAL nProcID
 nProcID=0
 GetWindowThreadProcessId(pnHWnd, @nProcID)

 
 RETURN nProcID
 *
ENDPROC



*-- WPGetMainHWnd
*   Devuelve el hWnd para la ventana principal de la aplicacion actual
*
PROC WPGetMainHWnd
 *
 *-- Se cargan las funciones API necesarias
 *
 WPLoadAPI()


 *-- Se obtiene el hWnd para la ventana principal
 *
 LOCAL nHWnd
 IF "06." $ VERSION()
  nHWnd=GetActiveWindow()
 ELSE
  nHWnd=_SCREEN.hWnd
 ENDIF
 
 RETURN nHWnd
 *
ENDPROC



*-- WPGetCurrentProcessID
*   Devuelve el ProcessID del programa actual
*
PROC WPGetCurrentProcessID
 *
 RETURN WPGetProcessIdByHWnd( WPGetMainHWnd() ) 
 *
ENDPROC



*-- WPKillProcess
*   Termina un proceso mediante su ProcessID
*
PROC WPKillProcess(pnProcID)
 *
 #DEFINE PROCESS_TERMINATE	1
 
 *-- Se cargan las funciones API necesarias
 *
 WPLoadAPI()
 

 *-- Se obtiene un hProcess para el ProcessID indicada
 *
 LOCAL nHProc
 STORE 0 TO nHProc
 nHProc=OpenProcess(PROCESS_TERMINATE, 0, pnProcID)


 *-- Si se obtuvo un hProcess, se termina el proceso
 *
 IF nHProc > 0
  TerminateProcess(nHProc,0)
  CloseHandle(phnd)
 ENDIF 


 RETURN .F. 
 *
ENDPROC



*-- WPKillCurrentProcess
*   Termina el proceso actual
*
PROC WPKillCurrentProcess
 *
 *-- Se cargan las funciones API necesarias
 *
 WPLoadAPI()


 *-- Se obtiene el hWnd para la ventana principal
 *
 LOCAL nHWnd
 IF "06." $ VERSION()
  nHWnd=GetActiveWindow()
 ELSE
  nHWnd=_SCREEN.hWnd
 ENDIF
 
 
 *-- Se obtiene el ProcessID
 *
 LOCAL nProcID
 nProcID=WPGetProcessIDByHWnd(nHWnd)
 
 
 *-- Se termina el proceso
 *
 RETURN WPKillProcess(nProcID)
 *
ENDPROC
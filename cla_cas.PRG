PARAMETERS cc
ncc=cc
DO case
   CASE cc='1'
      ncc='Sospechoso'
   CASE cc='2'
      ncc='Probable'
   CASE cc='3'
      ncc='Confirmado por Laboratorio'
   CASE cc='4'
      ncc='Confirmado por Clínica'
   CASE cc='5'
      ncc='Confirmado por Nexo epidemiológio'
   CASE cc='6'
      ncc='Caso Nuevo'
   CASE cc='7'
      ncc='Caso Retratado'
   CASE cc='8'
      ncc='Embarazada Nueva'
   CASE cc='9'
      ncc='Embarazada Retratada'
ENDCASE
RETURN ncc

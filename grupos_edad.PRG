PARAMETERS tmp_edad,tmp_uni
DO case
   CASE VAL(tmp_uni)>1
      tmp_gru=1
   CASE tmp_uni='1' AND BETWEEN(VAL(edad),1,4)
      tmp_gru=2
   CASE tmp_uni='1' AND BETWEEN(VAL(edad),5,14)
      tmp_gru=3
   CASE tmp_uni='1' AND BETWEEN(VAL(edad),15,44)
      tmp_gru=4
   CASE tmp_uni='1' AND BETWEEN(VAL(edad),45,64)
      tmp_gru=5
   CASE tmp_uni='1' AND VAL(edad)>=65
      tmp_gru=6
ENDCASE
RETURN tmp_gru


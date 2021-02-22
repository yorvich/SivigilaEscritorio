nc=1
num=''
c=1
do while !empty(substr(contra,nc,1))
   if substr(contra,nc,1)=','
      nc=nc+1
      if empty(substr(contra,nc,1))
         exit
      endif
      m.seña=m.seña+chr(val(num)/c)
      num=''
      c=c+1
   endif
   num=num+substr(contra,nc,1)
   nc=nc+1
enddo
if !empty(num)
   m.seña=m.seña+chr(val(num)/c)
endif

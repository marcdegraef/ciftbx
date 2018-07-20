        program testrle
        character*1024 astr,bstr,cstr
        integer tbxxrld, lastch, mlen, ii
        integer lastnb
 1      read (*,'(a)',end=900) astr
        bstr = ' '
        call tbxxrle(astr,bstr,mlen)
        cstr = ' '
        lastch= tbxxrld(cstr,bstr,.false.)
        if (astr.ne.cstr(1:lastch)) then
          print *,' mismatch ','astr=',astr(1:lastnb(astr)),
     *      'cstr=',cstr(1:lastch)
          print *,(ichar(bstr(ii:ii)),ii=1,mlen)
        endif
        go to 1
 900    continue
        stop
        end

        include 'ciftbx.f'
        include 'hash_funcs.f'

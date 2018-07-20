      subroutine clearfp
C
C     subroutine to clear IEEE floating point exceptions
C     for inexact and underflow under SUN OS 4 f77
C
      character*1 out
      ii = ieee_flags('clear','exception','underflow',out)
      ii = ieee_flags('clear','execption','inexact',out)
      return
      end

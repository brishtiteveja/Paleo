! Frequently used subroutines and function
! ----------------------------------------
!
! Command Line: - integer function nargs()
!               - function getarg(idx)
!
!
! written: 10.08.98
! modifications:
!
! Author: Michael Schulz, Institute for Geosciences, Univ. Kiel,
! ------- Olshausenstr. 40, D-24118 Kiel, FRG
!         mschulz@email.uni-kiel.de
!
!-------------------------------------------------------------------------
  module mutil
  contains
!
!-------------------------------------------------------------------------
     integer function nargs()
!-------------------------------------------------------------------------
!    Return the number of command line arguments (NB: the program name
!    is not counted).
!-------------------------------------------------------------------------
     implicit  none
     integer             ::  pos1, pos2, argcnt, nmax
     character(len = 80) ::  cmdlin
!
     call getcl(cmdlin)
     nmax = len(cmdlin)
     pos1 = index(cmdlin, " ")
!
!    no arguments ?
!    --------------
     if (pos1 .eq. 1) then
        nargs = 0
        return
     else
        argcnt = 1
        do while (pos1 .lt. nmax)
!
!          search start index of next argument
!          -----------------------------------
           do while (pos1 .lt. nmax)
              if (cmdlin(pos1:pos1) .ne. " ") then
                 exit
              else
                 pos1 = pos1 + 1
              end if
           end do
           argcnt = argcnt + 1
           pos2 = pos1
!
!          search end index of next argument
!          ---------------------------------
           do while (pos2 .le. nmax)
              if (cmdlin(pos2:pos2) .eq. " ") then
                 exit
              else
                 pos2 = pos2 + 1
              end if
           end do
           pos1 = pos2
        end do
     end if
     nargs = argcnt - 1
     end function nargs
!
!
!-------------------------------------------------------------------------
     function getarg(idx)
!-------------------------------------------------------------------------
!    Return the idx'th command line argument. Use in combination with
!    function NARGS.
!-------------------------------------------------------------------------
     implicit  none
     character (len = 40) :: getarg
     integer              :: idx
     integer              :: pos1, pos2, argcnt, nmax
     character (len = 80) :: cmdlin
!
     call getcl(cmdlin)
     nmax = len(cmdlin)
     pos1 = index(cmdlin, " ")
     if (idx .eq. 1) then
        getarg = cmdlin(:pos1-1)
        return
     else
        argcnt = 1
        do while (pos1 .lt. nmax)
           do while (pos1 .lt. nmax)
              if (cmdlin(pos1:pos1) .ne. " ") then
                 exit
              else
                 pos1 = pos1 + 1
              end if
           end do
           argcnt = argcnt + 1
           pos2 = pos1
           do while (pos2 .le. nmax)
              if (cmdlin(pos2:pos2) .eq. " ") then
                 exit
              else
                 pos2 = pos2 + 1
              end if
           end do
           if (argcnt .eq. idx) exit
           pos1 = pos2
        end do
     end if
     getarg = cmdlin(pos1:pos2-1)
     end function getarg
!
  end module

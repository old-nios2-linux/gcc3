        program test
        implicit none
! check all types of data statements
! pr 17541
        real r(2)
        double precision s(2)
        integer*1 ib(2)
        integer*2 ih(2)
        integer*4 iw(2)
        integer*8 id(3)
        logical*1 lb(2)
        logical*2 lh(2)
        logical*4 lw(2)
        logical*8 ld(2)
        character*1 a(2)
        character*5 b(2)
        complex c1(2)
        complex*8 c2(2)
        data r / 1.0,2.0 /
        data s / 2.d0,1.d0/
        data ib / 1,-1 /
        data ih / 2,100/
        data iw / 4,3560000 /
        data id / 8,Z'ABCDEF01',Z'5555AAAA' /
        data a / 'a', 'z' /
        data b / 'xyz','abc'/
        data c1 /(1.0,2.0),(-1.0,-2.0)/
        data c2 /(1.d0,2.d0),(-1.d0,-2.d0)/
        data lb / .TRUE.,.FALSE. /
        data lh / .TRUE.,.FALSE. /
        data lw / .TRUE.,.FALSE. /
        data ld / .TRUE.,.FALSE. /
        logical dbug
        data dbug /.FALSE./
! check the reals first
        if (r(1).ne.1.0) then
           if (dbug) then
             print*,r(1), ' should be 1.0 '
           else
             call abort
           endif
        endif
        if (r(2).ne.2.0) then
           if (dbug) then
             print*,r(2), ' should be 2.0 '
           else
             call abort
           endif
        endif
        if (s(1).ne.2.d0) then
           if (dbug) then
             print*,s(1), ' xxshould be 2.d0 '
           else
             call abort
           endif
        endif
        if (s(2).ne.1.d0) then
           if (dbug) then
             print*,s(2), ' should be 1.d0 '
           else
             call abort
           endif
        endif
! now the integers
        if (ib(1).ne.1) then
           if (dbug) then
             print*,ib(1), ' should be 1 '
           else
             call abort
           endif
        endif
        if (ib(2).ne.-1) then
           if (dbug) then
             print*,ib(2), ' should be -1 '
           else
             call abort
           endif
        endif
        if (ih(1).ne.2) then
           if (dbug) then
             print*,ih(2), ' should be 2 '
           else
             call abort
           endif
        endif
        if (ih(2).ne.100) then
           if (dbug) then
             print*,ih(2), ' should be 100 '
           else
             call abort
           endif
        endif
        if (iw(1).ne.4) then
           if (dbug) then
             print*,iw(1), ' should be 4 '
           else
             call abort
           endif
        endif
        if (iw(2).ne.3560000) then
           if (dbug) then
             print*,iw(2), ' should be 3560000 '
           else
             call abort
           endif
        endif
        if (id(1).ne.8) then
           if (dbug) print*,id(1), ' should be 8 '
           call abort
        endif
        if (id(2).ne.Z'ABCDEF01') then
           if (dbug) print*,id(2), " should be Z'ABCDEF01' "
           call abort
        endif
        if (id(3).ne.Z'5555AAAA') then
           if (dbug) print*,id(2), " should be Z'5555AAAA' "
           call abort
        endif
! complex
        if (c1(1).ne.(1.0,2.0)) then
           if (dbug) then
             print*,c1(1), ' should be (1.0,2.0) '
           else
             call abort
           endif
        endif
        if (c1(2).ne.(-1.0,-2.0)) then
           if (dbug) then
             print*,c1(2), ' should be (-1.0,-2.0) '
           else
             call abort
           endif
        endif
        if (c2(1).ne.(1.d0,2.d0)) then
           if (dbug) then
             print*,c2(1), ' should be (1.0,2.0) '
           else
             call abort
           endif
        endif
        if (c2(2).ne.(-1.d0,-2.d0)) then
           if (dbug) then
             print*,c2(2), ' should be (-1.0,-2.0) '
           else
             call abort
           endif
        endif
! character
        if (a(1).ne.'a') then
           if (dbug) then
             print*,a(1), ' should be a '
           else
             call abort
           endif
        endif
        if (b(1).ne.'xyz') then
           if (dbug) then
             print*,b(1), ' should be xyz '
           else
             call abort
           endif
        endif
!logicals
        if (.NOT.lb(1)) then
           if (dbug) print*,lb(1), ' should be .T. '
           call abort
        endif
        if (lb(2)) then
           if (dbug) print*,lb(2), ' should be .F. '
           call abort
        endif
        if (.NOT.lh(1)) then
           if (dbug) print*,lh(1), ' should be .T. '
           call abort
        endif
        if (lh(2)) then
           if (dbug) print*,lh(2), ' should be .F. '
           call abort
        endif
        if (.NOT.lw(1)) then
           if (dbug) print*,lw(1), ' should be .T. '
           call abort
        endif
        if (lw(2)) then
           if (dbug) print*,lw(2), ' should be .F. '
           call abort
        endif
        if (.NOT.ld(1)) then
           if (dbug) then
             print*,ld(1), ' should be .T. '
           else
             call abort
           endif
        endif
        if (ld(2)) then
           if (dbug) then
             print*,ld(2), ' should be .F. '
           else
             call abort
           endif
        endif
        end

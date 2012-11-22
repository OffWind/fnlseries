!!$--------!---------!---------!---------!---------!---------!---------%
!!$
!!$     File Name: FNLSERIES_MOD.F90
!!$
!!$     Author:    Carlos Silva Santos, Megajoule Inovação Lda.
!!$
!!$     URL:       www.megajoule.pt
!!$
!!$     Version:   2.0
!!$
!!$     Date:      16.October.2012
!!$
!!$--------!---------!---------!---------!---------!---------!---------%
!!$    
!!$###############################
!!$###############################
!!$ 
module parameter_fnl

  implicit none
  save

  integer,parameter :: ni_fnl=360,nj_fnl=181

end module parameter_fnl
!!$    
!!$###############################
!!$###############################
!!$
module fnl

  use parameter_fnl

  implicit none
  save

  real, dimension(ni_fnl,nj_fnl) :: lat,lon,uuu,vvv,vh,land
  real, dimension(ni_fnl/2,nj_fnl) :: temp

end module fnl

!!$--------!---------!---------!---------!---------!---------!---------%
!!$
!!$     File Name: FNLSERIES.F90
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

program fnlseries

  use parameter_fnl
  use fnl

  implicit none

  integer, parameter :: long = selected_int_kind(12)

!!$ Local variables
  character (len=12)   :: gribfile
  character (len=20)   :: seriesfile
  character (len=26)   :: alpha 
  character (len=200)  :: wgrib_command,unix2doscom

  integer :: ni,nj,i1,i2,i3,n1,n2,n3,ifile,nfile,ierror,i,j,ni_tmp,nj_tmp,itecplot
  integer :: quadrante,iip,jjp,ip,np
  real :: dir,pi
  integer(kind=long) :: idate,initdate,enddate
  integer,dimension(1000) :: latp,lonp
  real,dimension(1000) :: latpr,lonpr


  pi = atan(1.0) * 4.0

!!$ Tecplot output control
  itecplot = 0

!!$ Check how many fnl* files are present in current directory

  alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  ierror = 0
  nfile = 0
  initdate = 0
  enddate = 0

  do i1 = 1, 26
     do i2 = 1, 26
        do i3 = 1, 26
           write(gribfile,10) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
10         format('GRIBFILE.',3(a1))

           open(1,file=gribfile,status='old',iostat=ierror)
           if (ierror.ne.0) then
              exit
           else
!!$              write(wgrib_command,60) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
!!$              call system(wgrib_command)
!!$              write(wgrib_command,70) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
!!$              call system(wgrib_command)
!!$              open(2,file='date.txt',form='formatted')
!!$              read(2,*) idate
!!$              close(2)
!!$              if (nfile.eq.0) then
!!$                 initdate = idate * 100
!!$              else
!!$                 enddate = idate * 100
!!$              end if
              nfile = nfile + 1
           end if
        end do
        if (ierror.ne.0) exit
     end do
     if (ierror.ne.0) exit
  end do
  n1 = i1 - 1
  n2 = i2 - 1
  n3 = i3 - 1

!!$ Get initial date
  i1 = 1
  i2 = 1
  i3 = 1
  write(wgrib_command,60) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
  call system(wgrib_command)
  write(wgrib_command,70) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
  call system(wgrib_command)
  open(2,file='date.txt',form='formatted')
  read(2,*) idate
  close(2)
  initdate = idate * 100

!!$ Get final date
  ifile = nfile
  i1 = int(ifile / 26**2)
  i2 = int((ifile - i1 * 26**2) / 26)
  i3 = (ifile - i1 * 26**2 - i2 * 26)
  i1 = i1 + 1
  i2 = i2 + 1
  i3 = i3 
  write(wgrib_command,60) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
  call system(wgrib_command)
  write(wgrib_command,70) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
  call system(wgrib_command)
  open(2,file='date.txt',form='formatted')
  read(2,*) idate
  close(2)
  enddate = idate * 100

  write(*,*) 'Found ',nfile,' grib files ...'
  write(*,*)

  if (nfile.eq.0) stop

!!$  Set latitude and longitude fields

  do j = 1, nj_fnl
     do i = 1, 180
        lon(i,j) = -180 + i
        lat(i,j) = 91 - j
     end do
     do i = 181,360
        lon(i,j) = -180 + i
        lat(i,j) = 91 - j
     end do
  end do

!!$ Ask user for point coordinates

  open(1,file='fnlseries.cfg',status='old',iostat=ierror)
  if (ierror.eq.0) then
     read(1,*) np
     do ip = 1, np
        read(1,*) latpr(ip),lonpr(ip)
        latp(ip) = nint(latpr(ip))
        lonp(ip) = nint(lonpr(ip))

        do i = 1, ni_fnl
           if (lon(i,1).eq.lonp(ip)) then
              lonp(ip) = i
              exit
           end if
        end do
        do j = 1, nj_fnl
           if (lat(1,j).eq.latp(ip)) then
              latp(ip) = j
              exit
           end if
        end do
     end do
     close(1)
  else
     write(*,*) 'Latitude of point (decimal degrees) :: '
     np = 1
     ip = 1
     read(*,*) latpr(ip)
     write(*,*) 'Longitude of point (decimal degrees) :: '
     read(*,*) lonpr(ip)
     latp(ip) = nint(latpr(ip))
     lonp(ip) = nint(lonpr(ip))

     do i = 1, ni_fnl
        if (lon(i,1).eq.lonp(ip)) then
           lonp(ip) = i
           exit
        end if
     end do
     do j = 1, nj_fnl
        if (lat(1,j).eq.latp(ip)) then
           latp(ip) = j
           exit
        end if
     end do
  end if

  do ip = 1, np
     if ((nint(latpr(ip)).lt.0).and.(nint(lonpr(ip)).lt.0)) write(seriesfile,11) abs(nint(lonpr(ip))),abs(nint(latpr(ip)))
     if ((nint(latpr(ip)).ge.0).and.(nint(lonpr(ip)).lt.0)) write(seriesfile,12) abs(nint(lonpr(ip))),abs(nint(latpr(ip)))
     if ((nint(latpr(ip)).lt.0).and.(nint(lonpr(ip)).ge.0)) write(seriesfile,13) abs(nint(lonpr(ip))),abs(nint(latpr(ip)))
     if ((nint(latpr(ip)).ge.0).and.(nint(lonpr(ip)).ge.0)) write(seriesfile,14) abs(nint(lonpr(ip))),abs(nint(latpr(ip)))

     open(ip,file=trim(seriesfile),access='append',form='formatted')
     write(ip,656) 
     write(ip,660) initdate,enddate
     write(ip,666) nint(latpr(ip)), nint(lonpr(ip))
     write(ip,670) 
  end do

11 format(i3,'W_',i2,'S.dat')
12 format(i3,'W_',i2,'N.dat')
13 format(i3,'E_',i2,'S.dat')
14 format(i3,'E_',i2,'N.dat')
656 format('MJ format: FNL data series')
660 format('MJ|Inov Start date ',i12.12,' End date ',i12.12)
666 format('Lat=',i4,' Long=',i4,' Pressure level= 1000 mb')
670 format('Est,dir,Vmed,raj,Vmin,desv.padrao,AAAAMMDDhhmm,')

!!$ ------------------------------------------------------------------------------ $!!
!!$ Cycle through each GRIB file
!!$ ------------------------------------------------------------------------------ $!!

  do ifile = 0, nfile-1
     i1 = int(ifile / 26**2)
     i2 = int((ifile - i1 * 26**2) / 26)
     i3 = (ifile - i1 * 26**2 - i2 * 26)
     i1 = i1 + 1
     i2 = i2 + 1
     i3 = i3 + 1

     write(gribfile,20) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
20   format('GRIBFILE.',3(a1))

     write(*,25) gribfile,int(ifile * 100.0 / nfile)
25   format('... processing file ',a12,' ... ', i3,' %')

!!$ Use wgrib to extract data UUUm, VVVm & LAND
     write(wgrib_command,30) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3),alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
30   format('wgrib -text GRIBFILE.',3(a1),' | grep ":UGRD:" | grep "1000 mb"', &
          ' | wgrib -i    -text GRIBFILE.',3(a1),' -o fnlseries.uuu > /dev/null')
     call system(wgrib_command)

     write(wgrib_command,40) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3),alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
40   format('wgrib -text GRIBFILE.',3(a1),' | grep ":VGRD:" | grep "1000 mb"', &
          ' | wgrib -i    -text GRIBFILE.',3(a1),' -o fnlseries.vvv > /dev/null')
     call system(wgrib_command)

     if (itecplot.eq.1) then
        write(wgrib_command,50) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3),alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
50      format('wgrib -text GRIBFILE.',3(a1),' | grep ":LAND:" ', &
             ' | wgrib -i    -text GRIBFILE.',3(a1),' -o fnlseries.land > /dev/null')
        call system(wgrib_command)
     end if

     write(wgrib_command,60) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
60   format('wgrib GRIBFILE.',3(a1),' | grep ":UGRD:" | grep "1000 mb" > tmp1')
     call system(wgrib_command)
     write(wgrib_command,70) alpha(i1:i1),alpha(i2:i2),alpha(i3:i3)
70   format('cat tmp1 | wgrib -i -V GRIBFILE.',3(a1),' | grep date | cut -d " " -f 3 > date.txt')
     call system(wgrib_command)

!!$ Process the ungribbed records

     open(11,file='date.txt',form='formatted')
     read(11,*) idate
     idate=idate*100
     close(11)

     open(11,file='fnlseries.uuu' ,status='old',form='formatted')
     open(12,file='fnlseries.vvv' ,status='old',form='formatted')
     if (itecplot.eq.1) open(3,file='fnlseries.land',status='old',form='formatted')
     read(11,*) ni_tmp,nj_tmp
     read(12,*) ni_tmp,nj_tmp
     if (itecplot.eq.1) read(3,*) ni_tmp,nj_tmp

     do j = 1, nj_fnl
        do i = 1, ni_fnl
           read(11,*) uuu(i,j)
           read(12,*) vvv(i,j)
           if (itecplot.eq.1) read(3,*) land(i,j)
           vh(i,j) = (uuu(i,j)**2.0 + vvv(i,j)**2.0)**0.5
        end do
     end do
66   format(2(i3,1x),4(f6.1,2x))

     close(11)
     close(12)
     if (itecplot.eq.1) close(3)

!!$ Order data

     temp(1:180,1:181) = uuu(181:360,:)
     uuu(181:360,:) = uuu(1:180,:)
     uuu(1:180,:) = temp(1:180,1:181)
     temp(1:180,1:181) = vvv(181:360,:)
     vvv(181:360,:) = vvv(1:180,:)
     vvv(1:180,:) = temp(1:180,1:181)
     if (itecplot.eq.1) then 
        temp = land(181:360,:)
        land(181:360,:) = land(1:180,:)
        land(1:180,:) = temp(1:180,1:181)
     end if
     temp = vh(181:360,:)
     vh(181:360,:) = vh(1:180,:)
     vh(1:180,:) = temp(1:180,1:181)

!!$ Write time series

     do ip = 1, np
        iip = lonp(ip)
        jjp = latp(ip)
        if(uuu(iip,jjp).le.0.and.vvv(iip,jjp).le.0) quadrante = 1
        if(uuu(iip,jjp).le.0.and.vvv(iip,jjp).ge.0) quadrante = 2
        if(uuu(iip,jjp).ge.0.and.vvv(iip,jjp).ge.0) quadrante = 3
        if(uuu(iip,jjp).ge.0.and.vvv(iip,jjp).le.0) quadrante = 4

        dir=atan(vvv(iip,jjp)/uuu(iip,jjp))*180./pi

        if(quadrante.eq.1.or.quadrante.eq.2) dir=90.-dir
        if(quadrante.eq.3.or.quadrante.eq.4) dir=90.-dir+180.

        write(ip,777) 999,dir,vh(iip,jjp),99.99,99.99,99.99,idate
     end do

777  format(i3,',',f5.1,',',f5.2,',',f5.2,',',f5.2,',',f5.2,',',i12,',')

!!$ Write tecplot file 

     if (itecplot.eq.1) then
        open(13,file=trim(gribfile)//'.plt')
        write(13,9000) gribfile
        write(13,9001) 
        write(13,9002) gribfile,ni_fnl,nj_fnl
        write(13,9003)
        do j=1,nj_fnl
           do i=1,ni_fnl
              write(13,9004) lon(i,j),lat(i,j),uuu(i,j),vvv(i,j),vh(i,j),land(i,j)
           enddo
        enddo
        close(13)  

        write(*,*) ' => wrote ',trim(gribfile)//'.plt'
        write(*,*) '... ... ... ... ... ... ... ...' 
     end if

  end do

  call system('rm -rf fnlseries.uuu fnlseries.vvv fnlseries.land date.txt tmp1 dump ; echo "rm -rf fnlseries.uuu fnlseries.vvv fnlseries.land date.txt tmp1 dump"')

9000 format('title="',a12,'"')
9001 format('variables="XLONG","XLAT","UUU","VVV","VH","LAND"')
9002 format('zone t="',a12,'", i=',i4," j=",i4,' k=1')
9003 format('c=red,f=point')
9004 format(f5.0,1x,f4.0,1x,2(f6.1,1x),f5.1,1x,f3.1)

!!$ Convert UNIX2DOS to get rid of CR characters

  do ip = 1, np
     if ((nint(latpr(ip)).lt.0).and.(nint(lonpr(ip)).lt.0)) write(seriesfile,11) abs(nint(lonpr(ip))),abs(nint(latpr(ip)))
     if ((nint(latpr(ip)).ge.0).and.(nint(lonpr(ip)).lt.0)) write(seriesfile,12) abs(nint(lonpr(ip))),abs(nint(latpr(ip)))
     if ((nint(latpr(ip)).lt.0).and.(nint(lonpr(ip)).ge.0)) write(seriesfile,13) abs(nint(lonpr(ip))),abs(nint(latpr(ip)))
     if ((nint(latpr(ip)).ge.0).and.(nint(lonpr(ip)).ge.0)) write(seriesfile,14) abs(nint(lonpr(ip))),abs(nint(latpr(ip)))

     write(unix2doscom,8888) seriesfile
8888 format('unix2dos ',a12)

     call system(unix2doscom)
  end do

end program fnlseries

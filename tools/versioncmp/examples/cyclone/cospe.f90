! (C) Copyright 1996-2016 ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
! In applying this licence, ECMWF does not waive the privileges and immunities 
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.
!

      PROGRAM cospe
      INTEGER, PARAMETER :: NENS = 51
      INTEGER            :: I,iretI,iretR,Nlines,KKHOUR,KYEAR1,KMON1,KDAY1,KHOUR1
      INTEGER, DIMENSION(:), ALLOCATABLE :: KPF,KDAT,KHOUR,KFFF,KMSL,IWRK
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ICSTORM
      REAL   , DIMENSION(:), ALLOCATABLE :: XLAT,XLON,X5PBOX,X5PBOX2
      REAL   , DIMENSION(:,:), ALLOCATABLE :: Z5PBOX,Z5PBOX2,ZMXFFF
      REAL               :: RADIUS,MAP(720,361),TMAP(720,361),xminlat,xminlon,xmaxlat,xmaxlon
      REAL               :: XLATPF(80,NENS+1),XLONPF(80,NENS+1),XMSLP(80,NENS+1),XFFF(80,NENS+1),ZRES(5)
      CHARACTER          :: CPORRA*4


      RADIUS=120. ! in km
      XLATPF=-999.
      XLONPF=-999.
      XMSLP =-999.
      XFFF  =-999.
      KYEAR=2011
      KMON =3
      KDAY =31
      KKHOUR=0
      KSTEP=144 
      KDAYINCR=KSTEP/24

! Compute the forecast lead datetime
      
      CALL DAYINCR (KYEAR, KMON, KDAY, KDAYINCR, INYEAR, INMON, INDAY, IRET)
      IF (IRET.NE.0) THEN
         STOP 'PROBLEMS WITH DAYINCR ROUTINE (ECLIB) '
      END IF
      KBASETIME =KYEAR*10000+KMON*100+KDAY     
      KFCDATE   =INYEAR*10000+INMON*100+INDAY      ! HARDCODDED
      PRINT*, INYEAR, INMON, INDAY,KFCDATE
!      STOP

      OPEN(10,file="msl_25U_25U_2011033100")
      I=1
      DO
        READ(10,*,END = 20) 
 
        I = I +1
 
      END DO
 20   CONTINUE
      NLINES = I-1

      !	allocate memory

      ALLOCATE(KPF  (Nlines))
      ALLOCATE(KDAT (Nlines))
      ALLOCATE(KHOUR(Nlines))
      ALLOCATE(KFFF (Nlines))
      ALLOCATE(KMSL (Nlines))
      ALLOCATE(XLAT (Nlines))
      ALLOCATE(XLON (Nlines))
      print*,i,iretI,iretR
!      CLOSE(10)
!      open(10,file="/home/mo/moo/FVitart/source/NewProducts/mc0/msl_25U_25U_2011033100")
      REWIND (10)
      DO I=1,Nlines
       READ(10,*,END = 40) XLAT(I),XLON(I),KPF(I),KDAT(I),KHOUR(I),KFFF(I),KMSL(I)
        
      END DO
 40   CONTINUE     
      ! Go through the ensemble members and HiRes (==52)
	  ! and compute the strike probability field from individual tracks; RADIUS is set to 120km
      DO I=1, NENS
         MAP=0.
         !K1=1
         DO K=1, NLINES
        
            KYEAR1=KDAT(K)/10000
            KMON1 =(KDAT(K)-KYEAR1*10000)/100
            KDAY1 =(KDAT(K)-KYEAR1*10000)-KMON1*100

            CALL HOURDIFF(KYEAR1,KMON1,KDAY1,KHOUR(K)/100,KYEAR,KMON,KDAY,KKHOUR,IHOURS,IRET)


            IF ( KPF(K) == I .and. (IHOURS.GE.0.AND.IHOURS.LE.KSTEP)) THEN!KDAT(K) <= KFCDATE) THEN     ! CHANGE !!!!!!!!!!!!!!!!!!!!

               K1=IHOURS/6 + 1                     ! HARDCODDED (EPS must be defined every 6 hours)

               ii=int(XLON(K))/0.5+1
               jj=(90-int(XLAT(K)))/0.5+1
               do ii0=-10,10
                 do jj0=-10,10
                   xlon0=((ii+ii0)-1)*0.5
                   xlat0=90-((jj+jj0)-1)*0.5

                   call GreatCircleDistance(XLON(K),XLAT(K),xlon0,xlat0,xdist)

                   if (xdist.le.RADIUS) then
                      map(ii+ii0,jj+jj0)=map(ii+ii0,jj+jj0)+1
                   endif
                 enddo
               enddo
               XLATPF(K1,I)=XLAT(K)
               XLONPF(K1,I)=XLON(K)
               XMSLP (K1,I)=FLOAT(KMSL(K)) 
               XFFF  (K1,I)=FLOAT(KFFF(K)) 
               !K1 = K1 + 1
            END IF
         END DO
         DO I1=1,720
            DO J=1,361
               IF (MAP(I1,J).GE.1) THEN
                  TMAP(I1,J)=TMAP(I1,J)+100/51.
               ENDIF
            ENDDO
         ENDDO
      END DO

! Include the HiRes track (==52) 
      !K1=1
      DO K=1, NLINES
      
        KYEAR1=KDAT(K)/10000
        KMON1 =(KDAT(K)-KYEAR1*10000)/100
        KDAY1 =(KDAT(K)-KYEAR1*10000)-KMON1*100
        CALL HOURDIFF(KYEAR1,KMON1,KDAY1,KHOUR(K)/100,KYEAR,KMON,KDAY,KKHOUR,IHOURS,IRET)

        IF ( KPF(K) == (NENS+1) .and. (IHOURS.GE.0.AND.IHOURS.LE.KSTEP)) THEN !KDAT(K) <= KFCDATE) THEN    ! CHANGE !!!!!!!!!!!!!!!!!
          K1=IHOURS/6 + 1                     ! HARDCODDED (EPS must be defined every 6 hours)
          XLATPF(K1,NENS+1)=XLAT(K)
          XLONPF(K1,NENS+1)=XLON(K)
          XMSLP (K1,NENS+1)=FLOAT(KMSL(K))
          XFFF  (K1,NENS+1)=FLOAT(KFFF(K))
          !K1 = K1 + 1
          print*,XMSLP(k1,52)
        END IF
      END DO 
      !stop
! define the map corners based on the tracks 
      xmaxlat=maxval(XLATPF,XLATPF.NE. -999)
      xminlat=minval(XLATPF,XLATPF.NE. -999)
      xmaxlon=maxval(XLONPF,XLONPF.NE. -999)
      xminlon=minval(XLONPF,XLONPF.NE. -999)
      call plot_area(xminlat,xminlon,xmaxlat,xmaxlon)

      !print*,"minlat,minlon,maxlat,maxlon  ",xminlat,xminlon,xmaxlat,xmaxlon

!
! compute the quantiles for each step 
!
      KSTEP6=(KSTEP/6)+1 

      ALLOCATE (Z5PBOX (KSTEP6,8))
      ALLOCATE (Z5PBOX2(KSTEP6,8))
      ALLOCATE (ICSTORM(KSTEP6,5))
      ICSTORM=0
      DO K=1, KSTEP6 

        NPF= count(XMSLP(K,1:NENS) /= -999.)
        ALLOCATE(X5PBOX (NPF))
        ALLOCATE(X5PBOX2(NPF))
        ALLOCATE(IWRK   (NPF))

        X5PBOX =PACK(XMSLP(K,1:NENS),XMSLP(K,1:NENS) /= -999.)
        X5PBOX2=PACK(XFFF (K,1:NENS),XFFF (K,1:NENS) /= -999.)
        IF (NPF >= 5 ) THEN
 
! MSLP 
          IFAIL=0
! call NAG lib to compute the min/Q25/Q50/Q75/max for each time step

          CALL G01ALF(NPF,X5PBOX,IWRK,ZRES,IFAIL)
       
          IF (IFAIL /= 0) THEN 
             Z5PBOX(K,1:6)=-999.
             print*,'fail to find the percentiles at ', k*6,' hours'
          ELSE
             Z5PBOX(K,1:5)=ZRES
             Z5PBOX(K,  6)=XMSLP(K,NENS)         ! Ctrl 
             Z5PBOX(K,  7)=XMSLP(K,NENS+1)       ! HRes
             Z5PBOX(K,  8)=FLOAT(NPF)
          END IF

         
          !DEALLOCATE(IWRK  )

! WIND SPEED
! call NAG lib to compute the min/Q25/Q50/Q75/max for each time step
          IFAIL=0
          IWRK=0
          ZRES=0.
          CALL G01ALF(NPF,X5PBOX2,IWRK,ZRES,IFAIL)
       
          IF (IFAIL /= 0) THEN 
             Z5PBOX2(K,1:6)=-999.
             print*,'fail to find the percentiles at ', k*6,' hours'
          ELSE
             Z5PBOX2(K,1:5)=ZRES
             Z5PBOX2(K,  6)=XFFF (K,NENS)
             Z5PBOX2(K,  7)=XFFF (K,NENS+1)
             Z5PBOX2(K,  8)=FLOAT(NPF)
          END IF

        ELSE
          Z5PBOX(K,1:8)=-999.     ! This is not completely true
          Z5PBOX2(K,1:8)=-999.
        END IF
         
        DEALLOCATE(X5PBOX)
        DEALLOCATE(X5PBOX2)
        DEALLOCATE(IWRK  )

! compute the number of members for each storm class at each time step

        DO I=1,NENS

          IF (XFFF(K,I) /= -999.) THEN

            IF (XFFF(K,I).LT.17) THEN ! tropical depression
               ICSTORM(K,1)=ICSTORM(K,1)+1
            END IF
            IF (XFFF(K,I).GE.17.AND.XFFF(K,I).LE.32) THEN   ! tropical storm
               ICSTORM(K,2)=ICSTORM(K,2)+1
            ENDIF
            IF (XFFF(K,I).GE.33.AND.XFFF(K,I).LE.42) THEN   ! hurricane cat1
               ICSTORM(K,3)=ICSTORM(K,3)+1
            END IF
            IF (XFFF(K,I).GE.43.AND.XFFF(K,I).LE.48) THEN   ! hurricane cat2
               ICSTORM(K,4)=ICSTORM(K,4)+1
            END IF
            IF (XFFF(K,I).GE.49.) THEN                      ! hurricane cat3
               ICSTORM(K,5)=ICSTORM(K,5)+1
            END IF 
          END IF
        END DO
        print*,ICSTORM(K,1:5)
      END DO
      ALLOCATE(ZMXFFF(KDAYINCR,NENS+1))
      K1=1
      DO K=5,KSTEP6,4
         ZMXFFF(K1,1:NENS+1)=XFFF (K,1:NENS+1)
         K1=K1+1
      END DO
      !DO IS=1,NENS
      !   write(*,'(I2,A2,5(F5.0))') IS," ",(ZMXFFF(Kk,is),Kk=1,KDAYINCR)
      !end do

 
      !do kk=1,KSTEP6
      !write(*,'(5(F6.0),F4.0)') (Z5PBOX2(Kk,i),i=1,8)
      !end do
         

! draw maps with the strike probability and individual tracks
     
      print*,kbasetime,kfcdate,kkhour,kstep,xminlat,xminlon,xmaxlat,xmaxlon
      call makeplot (tmap,kbasetime,kfcdate,kkhour,kstep,xminlat,xminlon,xmaxlat,xmaxlon,xlatpf,xlonpf,Z5PBOX,Z5PBOX2,ZMXFFF,ICSTORM,KSTEP6,KDAYINCR)


      END



      SUBROUTINE GreatCircleDistance(xlon0,xlat0,xlon1,xlat1,d_km)

!
!  Computes the distance in meters between two points
!

!
!
! Input:
!       xlon0,xlat0: longitude and latitude of the first point
!       xlon1,xlat1: longitude and latitude of the second point
!
! Output:
!       d_km: distance between the 2 pointsin kilometres
!

      REAL nm2km,rad2nm,deg2rad,d_nm,d_km,d_rad

      pi=3.1416
      rad2nm=180*60/pi
      nm2km=1.852
      nm2km=1.852
      deg2rad=pi/180.

      rlon0=xlon0
      rlat0=xlat0
      rlon1=xlon1
      rlat1=xlat1

      xdiff=rlon1-rlon0+180
      if (xdiff.ge.360) xdiff=xdiff-360
      if (xdiff.le.-360) xdiff=xdiff+360
      rlon1=(rlon0+xdiff-180)*deg2rad

      xdiff=rlat1-rlat0+180
      if (xdiff.ge.360) xdiff=xdiff-360
      if (xdiff.le.-360) xdiff=xdiff+360
      rlat1=(rlat0+xdiff-180)*deg2rad

      rlon0=xlon0*deg2rad
      rlat0=xlat0*deg2rad

!
! The modulo function is invoked to keep +- 180
!

!
! We compute the distance between the 2 points
!

      d_rad=acos(sin(rlat0)*sin(rlat1)+cos(rlat0)*cos(rlat1)*cos(rlon0-rlon1))
      d_nm=d_rad*rad2nm
      d_km=d_nm*nm2km

      END
      subroutine plot_area(minlat,minlon,maxlat,maxlon)
      IMPLICIT NONE
      INTEGER      :: nlons,nlats
      REAL         :: maxlon,maxlat,minlon,minlat,latrange,lonrange,scaling
      REAL         :: lonextra,latextra
      LOGICAl      :: flag
!      maxlon=maxval(xlons)+8
!      minlon=minval(xlons)-8
!      maxlat=maxval(xlats)+8
!      minlat=minval(xlats)-8
      maxlon=maxlon+8
      minlon=minlon-8
      maxlat=maxlat+8
      minlat=minlat-8
!      print*,"maxlon,maxlat,minlon,minlat ",maxlon,maxlat,minlon,minlat
      latrange=maxlat-minlat
      lonrange=maxlon-minlon
      if (latrange/lonrange >= 2./3.) then
        scaling=(3./2.)/(abs(lonrange/latrange))			! ideal proportions are landscape 3:2
        lonextra=(scaling*lonrange)-lonrange
        maxlon=maxlon+(lonextra/2.)
        minlon=minlon-(lonextra/2.)
!        print*,"if 1"
      else
        scaling=(3./2.)/(abs(lonrange/latrange))			! ideal proportions are landscape 3:2
        latextra=(latrange/scaling)-latrange
        maxlat=maxlat+(latextra/2.)
        minlat=minlat-(latextra/2.)
!        print*,"if 2"
      end if
      
      end



      PROGRAM RhoSigChi
C Read/write stuff (mama)
CJEM      COMMON/Sp1Dim/rSPEC(2,0:8191),MAXCH
CJEM      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
CJEM      COMMON/Sp2Dim/rMAT(2,0:4095,0:511),APP(512),XDIM,YDIM
      INTEGER XDIM,YDIM,MAXCH
CJEM      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60,comment*60
      CHARACTER outfile*20,APP*4
CJEM      COMMON/iter/jmin,jmax,igmin,nit,igmax(0:511),iu0

C Stuff for the rhosig iteration
      REAL Rho(0:100,0:511),Rhov(0:100,0:511)
      REAL Sig(0:100,0:511),Sigv(0:100,0:511)
      REAL SumFg(0:511),SumFgv(0:511)
      REAL Fg(0:511,0:511),FgTeo(0:511,0:511),FgN(0:511,0:511)
      REAL Fgv(0:511,0:511),sFg(0:511,0:511),sFgN(0:511,0:511)
      REAL sRho(0:511),sSig(0:511)
      REAL rRho(0:511),rSig(0:511),Sum,Sum2
      REAL Chi(0:100),a1,a0
      REAL Ex_min,Ex_max,Emr
      REAL Ex,dE_p,dE_g1MeV,dE_g8MeV,dE_g
      REAL egmax(0:511)
      INTEGER igmax
      INTEGER time,nunc,degrees,deg,jmax,ix,ig,iu,iemr,step,in0
      INTEGER ism,istart,istop,ll,i,j
      REAL*8 xfit(1:512),yfit(1:512),y0,y1,y2,x0,x1,x2,x3,x4,d0,d1,d2,d3 !REAL->REAL*8
      REAL*8 tmp(0:511),coef(3)                                          !Corrected 11 feb 2016
      DIMENSION Fi(0:511),Ff(0:511) 
      WRITE(6,*)' ________________________________________'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|         R H O S I G C H I  1.5.3       |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|  Program to calculate level density    |'
      WRITE(6,*)'| Rho, and gamma strength function Sig   |'
      WRITE(6,*)'| from first-generation spectra, using   |'
      WRITE(6,*)'|   FgNorm(Ex,Eg)=Rho(Ex-Eg)*Sig(Eg)     |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|      Oslo Cyclotron Laboratory         |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|        Created: 13/08 - 1999           |'
      WRITE(6,*)'|           Andreas Schiller             |'
      WRITE(6,*)'|  Lisbeth Bergholt, Magne Guttormsen    |'
      WRITE(6,*)'|                                        |'
      WRITE(6,*)'|        Improved: 01/05 - 2003          |'
      WRITE(6,*)'|           Alexander Voinov             |'
      WRITE(6,*)'|  Magne Guttormsen, Andreas Schiller    |'
      WRITE(6,*)'|        Dubna, Oslo, Livermore          |'
      WRITE(6,*)'|  Modified 28 Mar 2014: Ex < 0 MeV      |'
      WRITE(6,*)'|  Modified 13 Feb 2015: One ch shift,   |'
      WRITE(6,*)'|  ch ig < 0, cut igmax                  |'
      WRITE(6,*)'|  Modified 31 Aug 2015: No normalization|'
      WRITE(6,*)'|  Modified 15 Dec 2015: input > 10**-06 |'
      WRITE(6,*)'|  Modified 11 Feb 2016: new Ydim, Xdim  |'
      WRITE(6,*)'|  REAL->REAL*8 precision, smooth 300 keV|'
      WRITE(6,*)'|________________________________________|'
C Things that should be done in the future:
C Fitting with error weighting
C Finding a better parametrization of the strength function
C Initializing parameter values
      nit=50                  ! Number of iterations
      Ex_min=4000.0           ! Default lower Ex for 1.gen. spec.
      Ex_max=8000.0           ! Default higher Ex for 1.gen. spec.
      Eg_min=1000             ! Default lowest gamma energy
      nunc=100                ! Number of extractions
      dE_p = 150.             ! Particle resolution
      dE_g1MeV = 60.          ! NaI resolution at 1 MeV
      dE_g8MeV = 300.         ! NaI resolution at 8 MeV
      eps      = 0.000001     ! Lowest counts in fg matrix

C Initializing some vectors and matrices
      DO i=0,511
        sRho(i) = 0.
        sSig(i) = 0.
        rRho(i) = 0.
        rSig(i) = 0.
        DO j=0,100
           Rho(j,i)  = 0.
           Rhov(j,i) = 0.
           Sig(j,i)  = 0.
           Sigv(j,i) = 0.
        ENDDO
      ENDDO

C Reading first-generation mama-matrix
      IDEST=1
      ITYPE=3
C       WRITE(6,*)'Please, answer 1 and the name of your input first-'
C       WRITE(6,*)'generation matrix in the two next questions... '
      CALL READFILE("firstgen-jem-20161011-Nexbins196-Nstat500.m")
CJEM   added to load fg variance (fgv) spectrum into the other rMAT channel:
      IDEST = 2
      CALL READFILE("fgvar-jem-20161011-Nexbins196-Nstat500.m")
      IDEST = 1
CJEM  done loading fgv    
      IF(XDIM.GT.512)XDIM=512
      IF(YDIM.GT.512)YDIM=512
      bx=cal(1,IDEST,1,1)+cal(1,IDEST,1,2)+cal(1,IDEST,1,3)
      by=cal(1,IDEST,2,1)+cal(1,IDEST,2,2)+cal(1,IDEST,2,3)
      IF(bx+by.EQ.2.)THEN
        aEg0=11.               !Defaults in case of no calibration
        aEg1=20.
        aEx0=9660.
        aEx1=-120.
      ELSE
        aEg0=cal(1,IDEST,1,1)  !Picks the spectrum calibration
        aEg1=cal(1,IDEST,1,2)
        aEx0=cal(1,IDEST,2,1)
        aEx1=cal(1,IDEST,2,2)
      ENDIF
      a1=ABS(aEx1)
      multiple=INT((120./a1)+0.5)
      IF(a1.GT.150.)multiple=1
      a1=FLOAT(multiple)*a1
C An energy that was in the middle of a channel, shall still be in the
C middle of a channel after change of calibration
      Eold=aEx0+aEx1*0                                  !Choosing old channel 0
      a0=(Eold/a1-INT(Eold/a1))*a1

C Now calculating max energy for NaI to cover resolution. Max is 800 keV
      a0  = a0 - INT((800./a1) + 0.5)*a1
      iu0 = INT((ABS(a0/a1) + 0.5))
      DO j=0, 511
        Ex = a0 + j*a1
        dE_g = dE_g1MeV + ((dE_g8MeV-dE_g1MeV)/(8000.-1000.))*(Ex-1000.)
        egmax(j) = SQRT(dE_g*dE_g + dE_p*dE_p)
        IF(egmax(j).GT.600.)egmax(j)=600.
        IF(egmax(j).LT.200.)egmax(j)=200.
        igmax(j) = j + INT((egmax(j)/a1)+0.5)
        IF(igmax(j).GT.511)igmax(j)=511
        egmax(j) = Ex + egmax(j)
      ENDDO

C Compressing (or stretching) along X and Y - axis
      DO i=0,511
         Fi(i)=0.
         Ff(i)=0.
      ENDDO

      DO j=0,YDIM-1
         DO i=0,XDIM-1
            If(rMAT(IDEST,i,j).LT.eps) rMAT(IDEST,i,j) = eps
         ENDDO
      ENDDO
      DO j=0,YDIM-1
         Sum=0.
         DO i=0,XDIM-1
            Fi(i)=rMAT(IDEST,i,j)                       ! Fi(i) and Ff(i) real type
            Sum=Sum+Fi(i)
         ENDDO
         IF(Sum.NE.0.)THEN
            CALL ELASTIC(Fi,Ff,aEg0,aEg1,a0,a1,512,512) ! Modifies spectrum to give  
            DO i=0,XDIM-1                               ! calibration a0 and a1
               Fg(i,j)=Ff(i)
               Fi(i)=0.
            ENDDO
         ENDIF
      ENDDO

      DO i=0,511
         Fi(i)=0.
         Ff(i)=0.
      ENDDO

      DO i=0,XDIM-1                                     ! Y-axis
         Sum=0.
         DO j=0,YDIM-1
            Fi(j)=Fg(i,j)
            Sum=Sum+Fi(j)
         ENDDO
         IF(Sum.NE.0.)THEN
            CALL ELASTIC(Fi,Ff,aEx0,aEx1,a0,a1,512,512)
            DO j=0,YDIM-1
               Fg(i,j)=Ff(j)
               Fi(j)=0.
            ENDDO
         ENDIF
      ENDDO

C Replacing negative counts with 0 and finding dimension of Fg matrix
C      XDIM=INT(FLOAT(XDIM)*ABS(aEg1/a1)+0.5) + iu0
c      YDIM=INT(FLOAT(YDIM)*ABS(aEx1/a1)+0.5)
c      YDIM=INT((ABS((FLOAT(YDIM) * aEx1 + aEx0 - a0))/ABS(a1)) + 0.5)
c      XDIM=YDIM + iu0
      XDIM=INT((ABS((FLOAT(XDIM) * aEg1 + aEg0 - a0))/ABS(a1)) + 0.5) + iu0
      YDIM=INT((ABS((FLOAT(YDIM) * aEx1 + aEx0 - a0))/ABS(a1)) + 0.5)

c           write(6,*)xdim,ydim
      imax=10
      DO j=0,YDIM
         DO i=0,XDIM
            IF(Fg(i,j).GT.0..AND.i.GT.imax)imax=i
            IF(Fg(i,j).LE.0.)Fg(i,j)=0             !Delete negative numbers
         ENDDO
      ENDDO

      imax=MIN(imax,XDIM)
      Eg_limit = a0+a1*imax

CJEM  ==============================================
CJEM  WARNING -- SUPER HACKY: Replicating the above loops to fill sFg also 
CJEM
CJEM           START
CJEM
CJEM  ==============================================

CJEM   Changing IDEST to get fg variance spectrum
      IDEST = 2
C Compressing (or stretching) along X and Y - axis
      DO i=0,511
         Fi(i)=0.
         Ff(i)=0.
      ENDDO

      DO j=0,YDIM-1
         DO i=0,XDIM-1
            If(rMAT(IDEST,i,j).LT.eps) rMAT(IDEST,i,j) = eps
         ENDDO
      ENDDO
      DO j=0,YDIM-1
         Sum=0.
         DO i=0,XDIM-1
            Fi(i)=rMAT(IDEST,i,j)                       ! Fi(i) and Ff(i) real type
            Sum=Sum+Fi(i)
         ENDDO
         IF(Sum.NE.0.)THEN
            CALL ELASTIC(Fi,Ff,aEg0,aEg1,a0,a1,512,512) ! Modifies spectrum to give  
            DO i=0,XDIM-1                               ! calibration a0 and a1
               sFg(i,j)=Ff(i)
               Fi(i)=0.
            ENDDO
         ENDIF
      ENDDO

      DO i=0,511
         Fi(i)=0.
         Ff(i)=0.
      ENDDO

      DO i=0,XDIM-1                                     ! Y-axis
         Sum=0.
         DO j=0,YDIM-1
            Fi(j)=Fg(i,j)
            Sum=Sum+Fi(j)
         ENDDO
         IF(Sum.NE.0.)THEN
            CALL ELASTIC(Fi,Ff,aEx0,aEx1,a0,a1,512,512)
            DO j=0,YDIM-1
               sFg(i,j)=Ff(j)
               Fi(j)=0.
            ENDDO
         ENDIF
      ENDDO

C Replacing negative counts with 0 and finding dimension of Fg matrix
C      XDIM=INT(FLOAT(XDIM)*ABS(aEg1/a1)+0.5) + iu0
c      YDIM=INT(FLOAT(YDIM)*ABS(aEx1/a1)+0.5)
c      YDIM=INT((ABS((FLOAT(YDIM) * aEx1 + aEx0 - a0))/ABS(a1)) + 0.5)
c      XDIM=YDIM + iu0
      XDIM=INT((ABS((FLOAT(XDIM) * aEg1 + aEg0 - a0))/ABS(a1)) + 0.5) + iu0
      YDIM=INT((ABS((FLOAT(YDIM) * aEx1 + aEx0 - a0))/ABS(a1)) + 0.5)

c           write(6,*)xdim,ydim
      imax=10
      DO j=0,YDIM
         DO i=0,XDIM
            IF(Fg(i,j).GT.0..AND.i.GT.imax)imax=i
            IF(Fg(i,j).LE.0.)Fg(i,j)=0             !Delete negative numbers
         ENDDO
      ENDDO


      IDEST = 1
CJEM  ==============================================
CJEM              STOP filling sFg(i,j)
CJEM  ==============================================







c      write(6,*)'imax,XDIM, Eglimit',imax,XDIM, Eg_limit

      OPEN(23,FILE='input.rsg',STATUS='old',ERR=666)
      READ(23,*,END=666,ERR=666)Eg_min,Ex_min,Ex_max
      GO TO 777
 666  WRITE(6,*)'Warning: no input.rsg file found, defaults taken'
 777  CLOSE(23)

C Input some parameters from keyboard
C Input lower limit for gammas used in the extraction
      igmin=INT(((Eg_min-a0)/a1)+0.5)
      Eg_min=a0+a1*igmin
      WRITE(6,10)Eg_min
 10   FORMAT('Lower limit of gamma energy (keV)      <',F7.1,'>:',$)
      CALL READF(5,Eg_min)
      igmin=INT(((Eg_min-a0)/a1)+0.5)
      Eg_min=a0+a1*igmin
      jmin=INT(((Ex_min-a0)/a1)+0.5)
      jmin=MAX(jmin,igmin)
      Ex_min=a0+a1*jmin
      WRITE(6,11)Ex_min
 11   FORMAT('Lower limit of excitation energy (keV) <',F7.1,'>:',$)
      CALL READF(5,Ex_min)
      jmin=INT(((Ex_min-a0)/a1)+0.5)
      Ex_min=a0+a1*jmin
      IF(Ex_min.LT.Eg_min)THEN
         WRITE(6,'(A29)')'Warning, Ex_min<Eg_min not allowed, reset'
         Ex_min=Eg_min
      ENDIF

      jmax=INT(((Ex_max-a0)/a1)+0.5)
      jmax =MIN(jmax,YDIM)
      Ex_max=a0+a1*jmax
      WRITE(6,12)Ex_max
 12   FORMAT('Upper limit of excitation energy (keV) <',F7.1,'>:',$)
      CALL READF(5,Ex_max)
      jmax=INT(((Ex_max-a0)/a1)+0.5)
      jmax=MIN(jmax,YDIM)
      Ex_max=a0+a1*jmax
      IF(jmax.GT.511)THEN
         WRITE(6,13)Ex_max,a1
 13      FORMAT('Warning, Ex_max/a1 = ',F7.1,'/',F5.1,' > 511 channels, reset')
         jmax=511
      ENDIF
      Emr=Ex_max-Eg_min
      iemr=INT(((Emr-a0)/a1)+0.5)
      Emr=a0+a1*iemr


      imax = jmax + iu0


      Eg_max=egmax(imax)
      IF(Eg_max.GT.Eg_limit)Eg_max = Eg_limit
      DO ix=jmin,jmax
         IF(igmax(ix).GT.imax)igmax(ix)=imax
c            write(6,*)'ix,igmax(ix)',ix,igmax(ix)
      ENDDO


C Calculating number of points in the matrix to be fitted
      degrees = 0
      DO ix=jmin,jmax
        degrees = degrees + (igmax(ix) - igmin)
      ENDDO
      write(6,*)'Number of data points   ',degrees
C Minus number of data points in the fit functions (rho and sigma)
      degrees=degrees-(jmax-jmin+1) - (igmax(jmax)-igmin+1)
      write(6,*)'DOF, data points-rho-sig',degrees

      WRITE(6,14)a0,a1,imax+1,jmax+1,Ex_min,Ex_max,a0,Emr,Eg_min,Eg_max
 14   FORMAT('Common calibration is a0=',F7.2,'keV and a1=',F7.2,'keV/ch',
     + /,'Dimension is',I4,' x',I4, 
     + /,'excitation energy region is ',F6.0,'keV to ',F7.0,'keV',
     + /,'rho extracted from ',F6.0,'keV to ',F7.0,'keV',
     + /,'sig extracted from ',F6.0,'keV to ',F7.0,'keV')
C Finding number of counts in Fg(ig,ix) for each Ex 
      DO ix=jmin,jmax
         SumFg(ix)=0.
         DO ig=igmin,igmax(ix)
            SumFg(ix)=SumFg(ix)+Fg(ig,ix) 
         ENDDO
         DO ig=igmin,igmax(ix)
            IF(SumFg(ix).LE.0.)THEN
               FgN(ig,ix)=0.
            ELSE
               FgN(ig,ix)=Fg(ig,ix)/SumFg(ix)
            ENDIF
         ENDDO
      ENDDO
C Start value for Rho and Sig
      DO iu=0,jmax-igmin + iu0
         Rho(0,iu)=1.
      ENDDO
      DO ig=igmin,igmax(jmax)
         Sig(0,ig)=0.
         DO ix=MAX(jmin,2*ig-igmax(ig)),jmax  !We are adding for one ig along ix. But when ig>ix
            Sig(0,ig)=Sig(0,ig)+FgN(ig,ix)    !we start from 2*ig-igmax(ix)
         ENDDO
      ENDDO
 
C Statistical errors
C C Calculating statistical error of first generation spectra
      DO ix=jmin,jmax
C          Ex=a0+a1*ix
C          sFmax=0.
C          isFEg=0
C          TotM=MAX(1.,gM(Ex))            !Number of gammas for 1.gen. 
C          BckM=MAX(0.,gM(Ex)-1.)         !Number og gammas from 2.+3.+... gen.
C          DO ig=igmin,igmax(ix)
C C Fg=total-background NaI spectra, factor 2 due to unfolding
C C Total and background error are assumed to be independent from each 
C C other, therefore we use SQRT(tot+bck) instead of SQRT(tot)+SQRT(bck)
C             sFg(ig,ix)=2.*SQRT((TotM+BckM)*Fg(ig,ix))
C             IF(sFg(ig,ix).GT.sFmax)THEN
C                isFEg=ig
C                sFmax=sFg(ig,ix)
C             ENDIF
C          ENDDO
C C The factor 2 due to unfolding is very uncertain, it could be anything...
C C We treat sFg(ig,ix) from Eg=Eg_min up to gamma energy (isFEg) for maximum 
C C uncertainty (sFmax) in a special way (we have high sFg around Eg=0)
C C The Factor 1 is again very uncertain
C          IF(isFEg.GT.igmin)THEN
C             DO ig=igmin,isFEg
C                sFg(ig,ix)=sFmax*(1.+1.*(FLOAT(isFEg)-FLOAT(ig))/FLOAT(isFEg))
C             ENDDO
C          ENDIF
C As a further twist, we will smooth the errors (one-dimensionally) for each
C first generation spectrum. The smoothing is done over gamma energy intervals 
C of 3 MeV. This is new for version 1.2 of rhosigchi. Smoothing is performed 
C on the tmp() array.
         DO ig=igmin,igmax(ix)
            tmp(ig)=MAX(2.,sFg(ig,ix))  ! no errors less than 2 counts
            IF(Fg(ig,ix).GT.0.)THEN
                xx=ABS(tmp(ig)/Fg(ig,ix))
                IF(xx.LT.0.10)tmp(ig)=0.10*Fg(ig,ix) ! no errors less than 10 percent (25.01.2007)
            ENDIF
         ENDDO
         ism=NINT(300./a1)! +/-ism defines channel fit-region. Corrected 1500->300 11 feb 2016
C The smoothing procedure of Andreas starts, fasten seatbelts...
         DO ig=igmin,igmax(ix)
            istart=ig-ism
            istop=ig+ism
            ll=0
            IF(istart.LT.igmin)istart=igmin
            IF(istop.GT.igmax(ix))istop=igmax(ix) !corrected 11 feb 2016
            DO j=istart,istop
               ll=ll+1
               xfit(ll)=FLOAT(j)
               yfit(ll)=tmp(j)
            ENDDO
            IF(ll.GT.3)THEN
               y0=0.
               y1=0.
               y2=0.
               x0=0.
               x1=0.
               x2=0.
               x3=0.
               x4=0.
               DO j=1,ll
                  y0=y0+yfit(j)
                  y1=y1+yfit(j)*xfit(j)
                  y2=y2+yfit(j)*xfit(j)**2.
                  x0=x0+1.
                  x1=x1+xfit(j)
                  x2=x2+xfit(j)**2.
                  x3=x3+xfit(j)**3.
                  x4=x4+xfit(j)**4.
               ENDDO
               d0=x4*x2*x0+x3*x2*x1+x3*x2*x1-x4*x1*x1-x3*x3*x0-x2*x2*x2
               d1=y2*x2*x0+y1*x2*x1+y0*x3*x1-y2*x1*x1-y1*x3*x0-y0*x2*x2
               d2=y2*x2*x1+y1*x4*x0+y0*x3*x2-y2*x3*x0-y1*x2*x2-y0*x4*x1
               d3=y2*x3*x1+y1*x3*x2+y0*x4*x2-y2*x2*x2-y1*x4*x1-y0*x3*x3
               IF(d0.NE.0.)THEN
                  coef(1)=d1/d0
                  coef(2)=d2/d0
                  coef(3)=d3/d0
                  sFg(ig,ix)=coef(1)*FLOAT(ig)**2.+coef(2)*FLOAT(ig)+coef(3)
               ENDIF
            ENDIF
         ENDDO
C The first generation spectrum at high gamma energies is very uncertain
C due to unfolding. Usually we produce a lot of high energy gamma counts
C in the unfolding procedure. Some channel might nevertheless turn out to
C contain almost zero counts (a factor of 250 less counts than neighbouring 
C channels has been observed). Still these channels should have comparable
C errors to their neighbouring channels. This will be taken into account in 
C the following: From gamma energies greater than Ex_min, the error in the 
C first-generation spectrum must not change by more than +/-30% per increasing 
C gamma bin. No errors are less than 2 counts.
         DO ig=igmin+1,igmax(ix)
            sFg(ig,ix)=MAX(0.7*sFg(ig-1,ix),sFg(ig,ix)) !to avoid large decrease in error
            sFg(ig,ix)=MIN(1.3*sFg(ig-1,ix),sFg(ig,ix)) !to avoid large increase in error
            sFg(ig,ix)=MAX(2.,sFg(ig,ix))               !to avoid too small errors
         ENDDO
      ENDDO
C Normalizing sFg(Eg,Ex)
      DO ix=jmin,jmax
         DO ig=igmin,igmax(ix)
            IF(SumFg(ix).LE.0.)THEN
               sFgN(ig,ix)=0.
            ELSE
               sFgN(ig,ix)=sFg(ig,ix)/SumFg(ix)
            ENDIF
         ENDDO
      ENDDO


C Calculating number of degrees of freedom
C Number of data points fitted in first generation spectrum
c      degrees=(jmax-jmin+1)*(jmax+jmin-2*igmin+2)/2                !CHECK THIS
c           write(6,*)'Number of data points',degrees
C Minus number of data points in the fit functions (rho and sigma)
c      degrees=degrees-2*(jmax-igmin+1)
c           write(6,*)'DOF, data points - rho - sig',degrees


C Iteration starts here
      CALL Iteration(FgN,sFgN,Rho,Sig)
C Finding extracted average 1. gen. spectra FgTeo and Chi**2
      DO it=0,nit
         deg=degrees
         Chi(it)=0.
         DO ix=jmin,jmax
            SumFg(ix)=0.
            DO ig=igmin,igmax(ix)
               iu=ix-ig+iu0
               SumFg(ix)=SumFg(ix)+Sig(it,ig)*Rho(it,iu)
            ENDDO
            DO ig=igmin,igmax(ix)
               iu=ix-ig+iu0
               IF(SumFg(ix).GE.0.)THEN
                  FgTeo(ig,ix)=Sig(it,ig)*Rho(it,iu)/SumFg(ix)
               ELSE 
                  FgTeo(ig,ix)=0.
               ENDIF
               IF(sFgN(ig,ix).GT.0.)THEN
                  Chi(it)=Chi(it)+((FgTeo(ig,ix)-FgN(ig,ix))/sFgN(ig,ix))**2.
               ELSE
                  deg=deg-1
C Minus one data point in the calculation of Chi^2
               ENDIF
            ENDDO 
         ENDDO
         IF(deg.GT.0)THEN
            Chi(it)=Chi(it)/FLOAT(deg)
         ELSE
            Chi(it)=0.
         ENDIF
      ENDDO
C Showing indicators for how well the iteration converged
      i1MeV=jmax/4
      i2MeV=jmax/2
      i3MeV=3*jmax/4
      e1=a0+FLOAT(i1MeV)*a1
      e2=a0+FLOAT(i2MeV)*a1
      e3=a0+FLOAT(i3MeV)*a1
      step=nit/5
      WRITE(6,*)'_______________________________________________________'
      WRITE(6,15)
 15   FORMAT('        Convergence test using various indicators')
      WRITE(6,16)(it,it=step,nit,step)
 16   FORMAT('Indicator Iteration = 0',5I6)
      IF(Rho(0,i1MeV).NE.0.)WRITE(6,17)e1,(Rho(it,i1MeV)/Rho(0,i1MeV),it=0,nit,step)
      IF(Rho(0,i2MeV).NE.0.)WRITE(6,17)e2,(Rho(it,i2MeV)/Rho(0,i2MeV),it=0,nit,step)
      IF(Rho(0,i3MeV).NE.0.)WRITE(6,17)e3,(Rho(it,i3MeV)/Rho(0,i3MeV),it=0,nit,step)
 17   FORMAT('Rho/Rho0 at  U=',F5.0,6F6.2)
      IF(Sig(0,i1MeV).NE.0.)WRITE(6,18)e1,(Sig(it,i1MeV)/Sig(0,i1MeV),it=0,nit,step)
      IF(Sig(0,i2MeV).NE.0.)WRITE(6,18)e2,(Sig(it,i2MeV)/Sig(0,i2MeV),it=0,nit,step)
      IF(Sig(0,i3MeV).NE.0.)WRITE(6,18)e3,(Sig(it,i3MeV)/Sig(0,i3MeV),it=0,nit,step)
 18   FORMAT('Sig/Sig0 at Eg=',F5.0,6F6.2)
      WRITE(6,19)(Chi(it),it=0,nit,step)
 19   FORMAT('Chi^2 for 1.gen.sp. ',6F6.2)
      WRITE(6,*)'_______________________________________________________'
C Error calculation
C Initializing new Rhov = 1
      DO iu=0,jmax-igmin+iu0
         Rhov(0,iu)=Rho(0,iu)
      ENDDO
C New iteration were we test the influence of Fg -> Fg + sFg
C One by one channel is changed and (Sig-Sigv)**2
C and (Rho-Rhov)**2 is summed up, and SigmaRho and SigmaF is found
C Initializing arrays
      r=rand(time())               !seeding
      DO iu=0,jmax-igmin+iu0
         sRho(iu)=0.
      ENDDO
      DO ig=igmin,igmax(imax)
         sSig(ig)=0.
      ENDDO
      DO i=1,nunc
         DO ix=jmin,jmax
            DO ig=igmin,igmax(ix)
               r=rand(0)
               z=Finvert(r)
               Fgv(ig,ix)=Fg(ig,ix)+z*sFg(ig,ix) !Adding uncertainty
               IF(Fgv(ig,ix).LT.0.)Fgv(ig,ix)=0. !Zeroing negative numbers
            ENDDO
         ENDDO
         DO ix=jmin,jmax   !Finding number of counts in Fgv(ig,ix) for each Ex
            SumFgv(ix)=0.
            DO ig=igmin,igmax(ix)
               SumFgv(ix)=SumFgv(ix)+Fgv(ig,ix) 
            ENDDO
C Normalizing Fgv(Eg,Ex)
            DO ig=igmin,igmax(ix)
               IF(SumFgv(ix).LE.0.)THEN
                  Fgv(ig,ix)=0.
               ELSE
                  Fgv(ig,ix)=Fgv(ig,ix)/SumFgv(ix)
               ENDIF
            ENDDO
         ENDDO
C Initializing new Sigv
         DO ig=igmin,igmax(jmax)
            Sigv(0,ig)=0.
            DO ix=MAX(jmin,ig),jmax
               Sigv(0,ig)=Sigv(0,ig)+Fgv(ig,ix)
            ENDDO
         ENDDO
         CALL Iteration(Fgv,sFgN,Rhov,Sigv) !Iterating Fgv
         DO iu=0,jmax-igmin+iu0
            sRho(iu)=sRho(iu)+(Rhov(nit,iu)-Rho(nit,iu))**2.
         ENDDO
         DO ig=igmin,igmax(jmax)
            sSig(ig)=sSig(ig)+(Sigv(nit,ig)-Sig(nit,ig))**2.
         ENDDO
C Begin calculating relative errors
         Sum=0.
         Sum2=0.
         in0=0
         DO iu=0,jmax-igmin+iu0
            IF(Rho(nit,iu).GT.0.AND.sRho(iu).GT.0.)THEN
               rRho(iu)=SQRT(sRho(iu)/FLOAT(i))/Rho(nit,iu)
               in0=in0+1
            ELSE
               rRho(iu)=0.
            ENDIF
            Sum=Sum+rRho(iu)
         ENDDO
         DO ig=igmin,igmax(jmax)
            IF(Sig(nit,ig).GT.0.AND.sSig(ig).GT.0.)THEN
               rSig(ig)=SQRT(sSig(ig)/FLOAT(i))/Sig(nit,ig)
               in0=in0+1
            ELSE
               rSig(ig)=0.
            ENDIF
            Sum=Sum+rSig(ig)
         ENDDO  
         IF(in0.GT.0)THEN
            Sum=Sum/FLOAT(in0)
         ELSE
            Sum=0.
         ENDIF
         DO iu=0,jmax-igmin+iu0
            IF(Rho(nit,iu).GT.0.)Sum2=Sum2+(rRho(iu)-Sum)**2.
         ENDDO
         DO ig=igmin,igmax(jmax)
            IF(Sig(nit,ig).GT.0.)Sum2=Sum2+(rSig(ig)-Sum)**2.
         ENDDO
C End calculating relative errors
      ENDDO


      DO iu=0,jmax-igmin+iu0
         sRho(iu)=SQRT(sRho(iu)/FLOAT(nunc))
      ENDDO
      DO ig=igmin,igmax(jmax)
         sSig(ig)=SQRT(sSig(ig)/FLOAT(nunc))
      ENDDO
C Output of the results
      WRITE(6,*)
      WRITE(6,50)
50    FORMAT(' ch   Ex(keV)     Rho(1/MeV)  dRho(1/MeV) (UNNORMALIZED)')
      DO iu=0,jmax-igmin+iu0
        WRITE(6,51)iu, a0+a1*FLOAT(iu), Rho(nit,iu), sRho(iu)
51      FORMAT( I5,  F8.1,  E14.3, E12.3)
      ENDDO
      WRITE(6,*)
      WRITE(6,52)
52    FORMAT(' ch   Eg(keV)         Sig        dSig (UNNORMALIZED)')
      DO ig=igmin,igmax(jmax)
        WRITE(6,53)ig, a0+a1*FLOAT(ig), Sig(nit,ig), sSig(ig)
53      FORMAT( I5,  F8.1,  E14.3, E12.3)
      ENDDO


C Writing matrices  out for mama
C Writing matrices to files
      cal(1,1,1,1)=a0
      cal(1,1,1,2)=a1
      cal(1,1,1,3)=0.
      cal(1,1,2,1)=a0
      cal(1,1,2,2)=a1
      cal(1,1,2,3)=0.
      XDIM = imax+1
c      YDIM=dim
      YDIM = jmax+1
      DO ix=0,YDIM-1
         DO ig=0,XDIM-1
            rMAT(IDEST,ig,ix)=FgN(ig,ix)
         ENDDO
      ENDDO
      outfile='fg.rsg'
      comment='Observed first generation matrix'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,36)outfile
 36   FORMAT('Observed first generation matrix written to file:     ',A11)
      DO ix=0,YDIM-1
         DO ig=0,XDIM-1
            rMAT(IDEST,ig,ix)=sFgN(ig,ix)
         ENDDO
      ENDDO
      outfile='fgerr.rsg'
      comment='Estimated first generation error matrix'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,37)outfile
 37   FORMAT('Estimated first gen. err. matrix written to file:  ',A11)
      DO ix=0,YDIM-1
         DO ig=0,XDIM-1
            rMAT(IDEST,ig,ix)=FgTeo(ig,ix)
         ENDDO
      ENDDO
      outfile='fgteo.rsg'
      comment='Theoretical first generation matrix'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,38)outfile
 38   FORMAT('Theoret. first generation matrix written to file:  ',A11)

C Calculate relative level density and strength function
C Writing spectra out for PAW
      outfile='rhopaw.rsg'
      OPEN(UNIT=20,FILE=outfile)
      DO iu=0,jmax
         WRITE(20,*)Rho(nit,iu)
      ENDDO
      DO iu=0,jmax
         WRITE(20,*)sRho(iu)
      ENDDO
      CLOSE(20)
      WRITE(6,39)outfile
 39   FORMAT('Unnormalized Rho and dRho written to array file:  ',A11)

      outfile='sigpaw.rsg'
      OPEN(UNIT=20,FILE=outfile)
      DO ig=0,imax
         WRITE(20,*)Sig(nit,ig)
      ENDDO
      DO ig=0,imax
         WRITE(20,*)sSig(ig)
      ENDDO
      CLOSE(20)
      WRITE(6,40)outfile
 40   FORMAT('Unnormalized T and dT     written to array file:  ',A11)

C Writting spectra to mama matrices
      cal(1,1,2,1)=0.
      cal(1,1,2,2)=1.
      cal(1,1,2,3)=0.

      YDIM=2
      XDIM = jmax+1
      DO iu=0,XDIM-1
         rMAT(IDEST,iu,0)=Rho(nit,iu)
         rMAT(IDEST,iu,1)=sRho(iu)
      ENDDO
      outfile='rhosp.rsg'
      comment='Unnormalized Rho and dRho'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,41)outfile
 41   FORMAT('Unnormalized Rho and dRho written to mama file:    ',A11)

      YDIM=2
      XDIM=imax+1
      DO ig=0,XDIM-1
         rMAT(IDEST,ig,0)=Sig(nit,ig)
         rMAT(IDEST,ig,1)=sSig(ig)
      ENDDO
      outfile='sigsp.rsg'
      comment='Unnormalized T and dT'
      OPEN(20,FILE=outfile,ACCESS='SEQUENTIAL',ERR=99)
      CALL norw2dim(20,comment)
      CLOSE(20)
      WRITE(6,42)outfile
 42   FORMAT('Unnormalized T and dT     written to mama file:    ',A11)

      OPEN(23,FILE='input.rsg',ACCESS='SEQUENTIAL',ERR=888)
      WRITE(23,*)Eg_min,Ex_min,Ex_max ! deleted writing mass, corrected 11 feb 2016
      CLOSE(23)
888   CONTINUE

      STOP
 99   WRITE(6,*)'Could not open file for results and spectra'
      STOP
      END

C -----------------------------------------------------------------------------
      FUNCTION gM(Ex)
C Gamma-Multiplicity as function of Ex (in keV)
      a=42.12/100.
      b=0.0467/100.
      c=-0.128868E-05/100.
      gM=0.
      IF(Ex.GT.0..AND.Ex.LT.10000.)gM=a+b*Ex+c*Ex*Ex
      IF(Ex.GE.10000.)gM=a+b*10000.+c*10000.*10000.
      IF(gM.LT.0.)gM=0.
      RETURN
      END
C -----------------------------------------------------------------------------

      SUBROUTINE ELASTIC(Fi,Ff,A0i,A1i,A0f,A1f,Di,Df)
C The most magnificent stretch- and compress-routine
C ever created by a human being. It is complicated, but works!
C The routine stretches or compresses spectrum from initial
C calibration (A0i,A1i) to final (A0f,A1f). The dimensions
C of the real spectra are Di and Df. First channel is 0, so
C that the spectra occupy (0:D-1) channels.
C August 1994, Oslo Cyclotron Laboratory, Magne Guttormsen
      INTEGER Di,Df
      DIMENSION Fi(0:Di-1),Ff(0:Df-1)
C Testing
      IF(A1i.EQ.0..OR.A1f.EQ.0.)RETURN
C Zeroing final spectrum
      DO i=0,Df-1
         Ff(i)=0.  
      ENDDO
C Case where no action is taken
      IF(A0i.EQ.A0f.AND.A1i.EQ.A1f)THEN
         DO i=0,MIN0(Di-1,Df-1)
            Ff(i)=Fi(i)
         ENDDO
      RETURN
      ENDIF
C Taking counts in initial spectrum and find where
C to put it in final spectrum. Then it is distributed on
C the channel(s). The loop goes through all the
C channels i of the initial spectrum
      IF(ABS(A1i/A1f).LT.2)THEN
         DO i=0,Di-1
            IF(Fi(i).EQ.0.)GO TO 97
            EiL=A0i+A1i*(i-0.5)      !Step 1.0 chs left and right
            EiH=A0i+A1i*(i+0.5)
            CHf1=(EiL-A0f)/A1f       !CHf1 and CHf2 define limits where
            CHf2=(EiH-A0f)/A1f       !to put the counts in final spc.
            CHlength=ABS(CHf1-CHf2)  !Number of channels (float)
            CountCH=Fi(i)/CHlength   !Number of counts pr.ch unit
            CHfL=CHf1
            CHfH=CHf2
            IF(CHfL.GT.CHfH)THEN
               CHfL=CHf2
               CHfH=CHf1
            ENDIF
            j1=INT(CHfL+0.5)
            j2=INT(CHfH+0.5)                     !filling with CHwidth*CountCH
            IF(j1.GE.Df.OR.j2.LT.0)GO TO 97
            nch=j2-j1+1
            IF(nch.EQ.1)THEN                !One channel to fill
               IF(j1.GE.0)Ff(j1)=Ff(j1)+Fi(i)
               GO TO 97
            ENDIF
            IF(nch.GT.1)THEN                !Two or three channels to fill
               Counts=CountCH*(FLOAT(j1)+0.5-CHfL) !Fraction of left channel
               IF(j1.GE.0)Ff(j1)=Ff(j1)+Counts       
               Counts=CountCH*(CHfH+0.5-j2) !Fraction of right channel
               IF(j2.LE.Df-1)Ff(j2)=Ff(j2)+Counts
               DO j=j1+1,j2-1               !Filling in for whole chs.
                  IF(j.GE.0.AND.j.LE.Df-1)Ff(j)=Ff(j)+CountCH
               ENDDO
            ENDIF
 97         CONTINUE
         ENDDO
      ELSE
C The counts will be distributed in the streching procedure as a triangle,
C which has its left and right tail overlaping with the center of the 
C next triangle. The heighth and basis of the triangle is called h and b:
C               x         x          x
C             x   x     x   x      x   x
C           x       x x       x   x      x
C         x          0          0          x
C       x          x   x      x  x           x
C     x          x       x  x      x           x
C   x          x           0         x           x
         b=2.0*A1i/A1f   !basis of triangle
         h=2.0/b         !height of triangle in order to get area=1
         alpha=h/(b/2.)  !slope of triangle tails

         DO i=0,Di-1
            IF(Fi(i).EQ.0.)GO TO 98
            EiL=A0i+A1i*(i-1.)      !Step 1.0 chs left and right
            EiH=A0i+A1i*(i+1.)
            CHf1=(EiL-A0f)/A1f      !CHf1 and CHf2 define limits where
            CHf2=(EiH-A0f)/A1f      !to put the counts in final spc.
            CHfL=CHf1
            CHfH=CHf2
            IF(CHfL.GT.CHfH)THEN
               CHfL=CHf2
               CHfH=CHf1
            ENDIF
            j1=INT(CHfL)+1
            j2=INT(CHfH)
            IF(j1.GE.Df.OR.j2.LT.0)GO TO 98
            w=0.
            DO j=j1,j2
               IF(j.LT.CHfL+(b/2.))THEN
                  w=alpha*(j-CHfL)                !up going slope
               ELSE 
                  w=h-alpha*(j-(CHfL+(b/2.)))     !down going slope
               ENDIF
               IF(w.LT.-0.1)WRITE(6,*)'Warning, weight w < 0 : ',w
               IF(w.LT.0.)w=0.
               IF(j.GE.0.AND.j.LE.Df-1)Ff(j)=Ff(j)+w*Fi(i)
            ENDDO
 98         CONTINUE
         ENDDO
      ENDIF
      END

C -----------------------------------------------------------------------------
      SUBROUTINE Iteration(FgN,sFgN,Rho,Sig)
c      COMMON/iter/jmin,imax,igmin,nit,igmax(0:511),iu0
      COMMON/iter/jmin,jmax,igmin,nit,igmax(0:511),iu0

C Ansatz=FgN(E_x,E_gamma)=rho(E_x-E_gamma)*sigma(E_gamma)
      REAL Rho(0:100,0:511),Sig(0:100,0:511)
      REAL FgN(0:511,0:511),sFgN(0:511,0:511)
      REAL fun1(0:511),fun2(0:511),fun3(0:511)
      REAL nom(0:511,0:511),denom(0:511,0:511)
      REAL up,down,var
      INTEGER ii,it,ix,ig,iu,iu0,jmin,jmax,igmin,nit,igmax
C Start iteration
      DO it=1,nit
         IF(it.LE.5)THEN
            var=1.2
         ELSE IF(it.LE.12)THEN
            var=1.1
         ELSE IF(it.LE.21)THEN
            var=1.05
         ELSE IF(it.LE.30)THEN
            var=1.025
         ELSE
            var=1.01
         ENDIF
C Calculating two matrices 'nom' and 'denom'
         DO ix=jmin,jmax
            fun1(ix)=0.
            fun2(ix)=0.
            fun3(ix)=0.
            DO ig=igmin,igmax(ix)
               iu=ix-ig+iu0
                 IF(iu.LT.0)  write(6,*)'Warning iu is negative, ix, ig, iu = ',ix,ig,iu
               fun1(ix)=fun1(ix)+Sig(it-1,ig)*Rho(it-1,iu)
               IF(sFgN(ig,ix).GT.0.)THEN
                  fun2(ix)=fun2(ix)+(Sig(it-1,ig)*Rho(it-1,iu)/sFgN(ig,ix))**2.
                  fun3(ix)=fun3(ix)+Sig(it-1,ig) *Rho(it-1,iu)*FgN(ig,ix)/(sFgN(ig,ix)**2.)
               ENDIF
            ENDDO
            IF(fun1(ix).GT.0.)THEN
               fun2(ix)=fun2(ix)/(fun1(ix)**3.)
               fun3(ix)=fun3(ix)/(fun1(ix)**2.)
            ELSE
               fun2(ix)=0.
               fun3(ix)=0.
            ENDIF
            DO ig=igmin,igmax(ix)
               IF((fun1(ix)*sFgN(ig,ix)).GT.0.)THEN
                  nom(ig,ix)=fun2(ix)-fun3(ix)+FgN(ig,ix)/(fun1(ix)*(sFgN(ig,ix)**2.))
                  denom(ig,ix)=1./(fun1(ix)*sFgN(ig,ix))**2.
               ELSE
                  nom(ig,ix)=0.
                  denom(ig,ix)=0.
               ENDIF
            ENDDO
         ENDDO
C Varying a point of sigma
         DO ig=igmin,igmax(jmax)
            up=0.
            down=0.
            ii=MAX(jmin,ig)
            DO ix=ii,jmax
               iu=ix-ig+iu0
               up=up+Rho(it-1,iu)*nom(ig,ix)
               down=down+(Rho(it-1,iu)**2.)*denom(ig,ix)
                  if(iu.lt.0)write(6,*)'Warning negative iu'
            ENDDO
            IF(down.GT.0.)THEN
               IF((up/down).GT.(var*Sig(it-1,ig)))THEN
                  Sig(it,ig)=var*Sig(it-1,ig)
               ELSE IF((up/down).LT.(Sig(it-1,ig)/var))THEN
                  Sig(it,ig)=Sig(it-1,ig)/var
               ELSE
                  Sig(it,ig)=up/down
               ENDIF
            ELSE 
               Sig(it,ig)=0.
            ENDIF
         ENDDO
C Varying a point of rho
         DO iu=0,jmax-igmin+iu0
            up=0.
            down=0.
            ii=MAX(jmin,iu)
            DO ix=ii,jmax
               ig=ix-iu+iu0
               up=up+Sig(it-1,ig)*nom(ig,ix)
               down=down+(Sig(it-1,ig)**2.)*denom(ig,ix)
                 if(ig.lt.0)write(6,*)'Warning negative ig'
            ENDDO
            IF(down.GT.0.)THEN
               IF((up/down).GT.(var*Rho(it-1,iu)))THEN
                  Rho(it,iu)=var*Rho(it-1,iu)
               ELSE IF((up/down).LT.(Rho(it-1,iu)/var))THEN
                  Rho(it,iu)=Rho(it-1,iu)/var
               ELSE
                  Rho(it,iu)=up/down
               ENDIF
            ELSE
               Rho(it,iu)=0.
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END
C -----------------------------------------------------------------------------
      FUNCTION Finvert(y0)
C Inverting the monoton increasing function r=F(z) -> z=Finv(r) 
C This means to find the z-value giving the value y0 
C The function F is the cummulative Gauss function F=1/2(1+erf(z/sqrt(2)))
      REAL xl,xh,yl,yh,x,y,y0
      xl =-3.0
      xh = 3.0
      x  = 0.0
      yl = 0.0
      yh = 1.0
      y  = 0.5
      DO WHILE(ABS(y-y0).GT.0.001)
         x=xl+(xh-xl)*(y0-yl)/(yh-yl)
         y=0.5*(1.+erf(x/1.414213562))
         IF(y.GT.y0)THEN
            yl=y
            xl=x
         ELSE
            yh=y
            xh=x
         ENDIF
      ENDDO
      Finvert=x
      RETURN
      END

      SUBROUTINE READFILE2(filename,spectrum)
CJEM  An attempt to copy the relevant bits of the rewr.f READFILE
CJEM  subroutine to read MAMA 2D spectra in a simpler way. The variable
CJEM  spectrum is just the 2D array of counts.
      character filename*80
      double precision spectrum(0:4095,0:511)

CJEM  Original declarations:
      INTEGER XDIM,YDIM,dim
      CHARACTER fname*8,comm*60,xcomm*60
      CHARACTER FILNAM*80,FILNAMX*80,APP*4,text*110,tableline*5000
      REAL Calib(6), Spec(0:8191)
      CHARACTER ans*80


C Trying first to open file and extract parameters from
C the header file. After this we ask, with proper defaults
C       WRITE(6,2)FILNAM(1:4)
C   2   FORMAT('Filename          <',A,'>:',$)
C       CALL READA(5,FILNAM)
C       IF(Istatus.NE.0)RETURN
      OPEN(INF,FILE=filename,ACCESS='SEQUENTIAL',status='old',ERR=901)
      GO TO 3

C No file with this name. Checks if it is of type NA-0, NA-1, NA-2,..etc.
 901  CLOSE(INF)
      CALL LENGDE(FILNAM,LIN)
      DO i=1,512
        FILNAMX=FILNAM(1:LIN)//APP(i)
        OPEN(INF,FILE=FILNAMX,ACCESS='SEQUENTIAL',status='old',ERR=902)
        ITYP=2        ! The spectra are of type NA-0, NA-1, NA-2,..
        GO TO 3       ! out of loop and reads spectra of type 2
 902    CONTINUE
      ENDDO

      WRITE(6,4)FILNAM(1:LIN),FILNAM(1:LIN)
 4    FORMAT('File: ',A,' or ',A,'xx (xx=number) do not exist')
      CLOSE(INF)
      ITYPE=ITYPEold
      IDEST=IDESTold
      RETURN

 3    CONTINUE                       !OK, file exist, reads the header

      READ(INF,5,ERR=999)text
 5    FORMAT(A110)
      READ(text(1:1),6,ERR=903)
 6    FORMAT(A)

      IF(text(1:1).EQ.'!')THEN       ! It is a mama file
        REWIND(INF)
        DO i=1,10 
          READ(INF,5,ERR=903)text
          IF(text(1:10).EQ.'!DIMENSION')THEN
            READ(text(12:12),7)I1
 7          FORMAT(I1)
            IF(I1.EQ.1.AND.ITYP.EQ.0)THEN
              ITYPE=1 
            ENDIF
            IF(I1.EQ.1.AND.ITYP.EQ.2)THEN
              ITYPE=2 
            ENDIF
            IF(I1.EQ.2.AND.ITYP.EQ.0)THEN
              ITYPE=3 
            ENDIF
          ENDIF
        ENDDO
      
 903    REWIND(INF)

        IF(ITYPE.EQ.1)THEN             !singles spectrum
          DO i=0,8191
            Spec(i)=0
          ENDDO
          CALL norr1dim(INF, comm(2,IDEST), dim, Spec, Calib)
          DO i=1,3            !Adopting the calibration from file
            IF(Calib(1).NE.0.OR.Calib(2).NE.1)cal(2,IDEST,1,i)=Calib(i)
          ENDDO
          DO i=0,8191
            rSPEC(IDEST,i)=Spec(i)
          ENDDO
          MAXCH=dim-1
c          CALL SetMarker(1,2,0)
        ENDIF

        IF(ITYPE.EQ.2)THEN             ! spectra NA-0, NA-1,...
          CLOSE(INF)
          DO j=0,511
            DO i=0,4095
              Spec(i)=0
            ENDDO
            FILNAMX=FILNAM(1:LIN)//APP(j+1)
            OPEN(INF,FILE=FILNAMX,ACCESS='SEQUENTIAL',status='old',ERR=904)
            CALL norr1dim(INF, comm(1,IDEST), dim, Spec, Calib)
            CLOSE(INF)
            YDIM=j+1     !The last successful spectrum determine Y-dimension
904         CONTINUE
            DO i=0,4095
              rMAT(IDEST,i,j)=Spec(i)
            ENDDO
          ENDDO
          XDIM=dim
          DO i=1,3
            IF(Calib(1).NE.0.OR.Calib(2).NE.1)cal(1,IDEST,1,i)=Calib(i)
          ENDDO
          cal(1,IDEST,2,1)=0.
          cal(1,IDEST,2,2)=1.
          cal(1,IDEST,2,3)=0.
c          CALL SetMarker(1,1,1)
        ENDIF

        IF(ITYPE.EQ.3)THEN                 !matrix
          CALL norr2dim(INF)
c          CALL SetMarker(1,1,1)
        ENDIF

        CONTINUE
        CLOSE(INF)
        GO TO 3333
      ENDIF

C Stripping away path-name from spectrum name
 3333 CALL LENGDE(FILNAM,LIN)
      ii=0
      DO i=1,LIN
        IF(FILNAM(i:i).EQ.'/')ii=i  ! ii marks the position of last '/'
      ENDDO
      I1=1                !matrix
      IF(ITYPE.EQ.1)I1=2  !singles
      DO i=1,MIN0(LIN,8)
        j=i+ii
        IF(j.LE.80)fname(I1,IDEST)(i:i)=FILNAM(j:j)
      ENDDO
      DO i=MIN0(LIN,8)+1,8          !blanks out the rest
        fname(I1,IDEST)(i:i)=' '
      ENDDO
      xcomm='|RE:'//fname(I1,IDEST)(1:MIN0(LIN,8))
c      CALL AddComment(xcomm,12)
      RETURN 

907   IF(ITYPE.EQ.0)THEN
        WRITE(6,*)'Sorry, not a spectrum/matrix file'
        WRITE(6,*)'The spectrum is of unknown type (mama (1,2,3), MCA or PAW)'
        CLOSE(INF)
        ITYPE=ITYPEold
        IDEST=IDESTold
        RETURN
      ENDIF
999   WRITE(6,*)'Tullball with the header file, could not retrieve info, go home!'
      ITYPE=ITYPEold
      IDEST=IDESTold
      RETURN
      END
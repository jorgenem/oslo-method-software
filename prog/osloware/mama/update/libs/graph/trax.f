      SUBROUTINE TRAX(TX,XI,TY,YI,K)

C      draw axes in graphics window....
C           x-axis from xi to xi+tx....
C           y-axis from yi to yi+ty....
C           k = 1/2/3 for lin/sqrt/log y-axis on left hand side....
C           k = -1/-2/-3 for lin/sqrt/log y-axis on right hand side....
C           k = 0 for linear y-axis, no axes drawn....

      REAL        TX,XI,TY,YI
      INTEGER     K

      REAL               FDX,FX0,FDY,FY0
      INTEGER            IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON /MINIG_DAT/ FDX,FX0,FDY,FY0,IDX,IX0,IDY,IY0,IYFLAG,ITERM
      COMMON/State/Istatus,ITYPE,IDEST,cal(2,2,2,3),Idim(2,2,2),fname(2,2),comm(2,2)
      CHARACTER fname*8,comm*60
      COMMON/AXIS/iCE,itext,UNITx,UNITy
      CHARACTER UNITx*3,UNITy*3
      COMMON/DisType/Idistype,OLlow,OLhigh,OLlocnt,OLhicnt
      INTEGER                 OLlow,OLhigh,OLlocnt,OLhicnt

      REAL DEX(14)
      CHARACTER*8 BC
      UNITx=' ch'
      UNITy=' ch'
      xMeV=1.
      yMeV=1.

      FDX = TX
      FX0 = XI
      FY0 = YI
      IYFLAG = IABS(K)
      IF (K.EQ.0) IYFLAG=1

      IF (IYFLAG.EQ.1) THEN
         FDY = TY
      ELSEIF (IYFLAG.EQ.2) THEN
         FDY = SQRT(FY0+TY)-SQRT(FY0)
      ELSE
         FY0 = AMAX1(FY0,1.0)
         FDY = ALOG(FY0+TY)-ALOG(FY0)
      ENDIF

      IF (K.EQ.0) RETURN    

      X1 = FX0              !channel-region
      X2 = FX0+FDX
      Y1 = FY0
      Y2 = FY0+TY

      EX1= X1               !energy region
      EX2= X2
      EY1= Y1
      EY2= Y2

      CALL CVXY(X2,Y2,IXmax,IYmax,1)  !max pixelvalue for x- and y-axis

      IF(iCE.EQ.1)THEN
        UNITx='keV'
        UNITy='keV'
        IF(ITYPE.GT.1)THEN
          ax0=cal(1,IDEST,1,1)
          ax1=cal(1,IDEST,1,2)
          ax2=cal(1,IDEST,1,3)
          ay0=cal(1,IDEST,2,1)
          ay1=cal(1,IDEST,2,2)
          ay2=cal(1,IDEST,2,3)
        ELSE
          ax0=cal(2,IDEST,1,1)
          ax1=cal(2,IDEST,1,2)
          ax2=cal(2,IDEST,1,3)
          ay0=0
          ay1=1.
          ay2=0
        ENDIF

C For outlay
        IF(Idistype.EQ.2)THEN
          ay0=0.
          ay1=1.
          ay2=0.
        ENDIF

        EX1=ax0+ax1*(X1)+ax2*(X1)*(X1)
        EX2=ax0+ax1*(X2)+ax2*(X2)*(X2)
        IF(IYFLAG.EQ.1)THEN
          EY1=ay0+ay1*(Y1)+ay2*(Y1)*(Y1)
          EY2=ay0+ay1*(Y2)+ay2*(Y2)*(Y2)
        ENDIF

        IF(ABS(EX1-EX2).GT.2000)THEN
          EX1=EX1/1000.
          EX2=EX2/1000.
          UNITx='MeV'
          xMeV=1000.
        ENDIF
        IF(ITYPE.GT.1.AND.IYFLAG.EQ.1)THEN
          IF(ABS(EY1-EY2).GT.2000)THEN
            EY1=EY1/1000.
            EY2=EY2/1000.
            UNITy='MeV'
            yMeV=1000.
          ENDIF
        ENDIF
C Puts ch-unit if a0=0 and a1=1 (calibr. not defined)
        IF(ax0.EQ.0.AND.ax1.EQ.1)THEN
          UNITx=' ch'
          xMeV=1.
          EX1=X1
          EX2=X2
        ENDIF
        IF(ay0.EQ.0.AND.ay1.EQ.1)THEN
          UNITy=' ch'
          yMeV=1.
          EY1=Y1
          EY2=Y2
        ENDIF
      ENDIF

C For outlay
      IF(Idistype.EQ.2)THEN
        EY1=Y1
        EY2=Y2
      ENDIF


      ETX=ABS(EX2-EX1)
      ETY=ABS(EY2-EY1)

C       draw x-axis....
      CALL PSPOT(X1,Y1)
      CALL VECT(X2,Y1)
      CALL GRAX(EX1,EX2,DEX,NV,1)
C             write(6,*)'ex1,ex2,nv',ex1,ex2,nv
      IXold=10000    
      DO 20 N=1,NV
         V=DEX(N)
         ch=V
         IF(iCE.EQ.1)ch=CHAN(V*xMeV,ax0,ax1,ax2)+0.5
C                write(6,*)'N,V,ch',N,V,ch
         IF (ch.EQ.X1) GO TO 20
         CALL CVXY(ch,Y1,IX,IY,1)
         CALL MSPOT(IX,IY+3)
         CALL IVECT(IX,IY-0)
         CALL MSPOT(IX,IY-1)
         IF((2*(N/2).EQ.N).AND.(IABS(IXold-IX).GT.20).AND.IX.LT.(IXmax-30))THEN
C              write value in graphtext....
            IF (ETX.LE.15.0) THEN
               WRITE(BC,5,ERR=8) V
5              FORMAT(F7.2,' ')
            ELSE
               WRITE(BC,6,ERR=8) V
6              FORMAT(F8.0)
            ENDIF
8           DO 10 I=1,7
               IF (BC(I:I).NE.' ') GO TO 15
10          CONTINUE
15          NC=8-I
            CALL PUTG(BC(I:8),-NC,6,1)
            IXold=IX
         ENDIF
20    CONTINUE

C     draw y-axis....
      X=X1
      IF (K.LT.0) X=X2
      CALL PSPOT(X,Y1)
      CALL VECT(X,Y2)
      CALL GRAX(EY1,EY2,DEX,NV,IYFLAG)
      DO 40 N=1,NV
         V=DEX(N)
         ch=V
C        Require energy-display and 3-dimensional spectra
         IF(iCE.EQ.1.AND.ITYPE.GT.1)THEN
           IF(IYFLAG.EQ.1) ch=CHAN(V*yMeV,ay0,ay1,ay2)+0.5
         ENDIF
         IF (ch.LE.Y1.OR.ch.GE.Y2) GO TO 40
         CALL CVXY(X,ch,IX,IY,1)
         IF(K.GE.0)THEN             !Y-axis on left side
           CALL MSPOT(IX+3,IY)
           CALL IVECT(IX-0,IY)
         ELSE                       !Y-axis on right side
           CALL MSPOT(IX-0,IY)
           CALL IVECT(IX-3,IY)
         ENDIF
         IF ((IYFLAG.EQ.3.OR.(N+1)/4*4.EQ.N+1).AND.IY.LT.IYmax-25) THEN
C              write value in graphtext....
            IF (ETY.LE.15.0) THEN
               WRITE(BC,5,ERR=25) V
            ELSE
               WRITE(BC,6,ERR=25) V
            ENDIF
25          DO 30 I=1,7
               IF (BC(I:I).NE.' ') GO TO 35
30          CONTINUE
35          NC=8-I
            IF (K.LT.0) THEN
               CALL MSPOT(IX-7,IY-7)
               CALL PUTG(BC(I:8),-NC,8,1)
            ELSE
               CALL MSPOT(IX+6,IY-7)
               CALL PUTG(BC(I:8),-NC,2,1)
            ENDIF
         ENDIF
40    CONTINUE
      RETURN
      END

      
      FUNCTION CHAN(E,a0,a1,a2)
C Finds the most probable solution of the 2. degree polynom:
C E=a0+a1*ch+a2*ch*ch. We assume ch to be positive (channels),
C and that a2 only makes a small change in E

C Without a1 and/or a2 term
      chan=0
      IF(a1.EQ.0.AND.a2.EQ.0)RETURN
      IF(a1.NE.0)chan=(E-a0)/a1      
      IF(a2.EQ.0)THEN
        IF(chan.LT.0)chan=0
        RETURN
      ENDIF

      tch=chan                          !to test against

C Assuming full equation with a0,a1 and a2
      chan=0
      root=a1*a1-4.*a2*(a0-E)
      IF(root.LT.0)RETURN
      ch1=(-a1+sqrt(root))/(2.*a2)
      ch2=(-a1-sqrt(root))/(2.*a2)
      t1=ABS(ch1-tch)
      t2=ABS(ch2-tch)
      IF(t1.LE.t2.AND.ch1.GE.0)chan=ch1
      IF(t2.LE.t1.AND.ch2.GE.0)chan=ch2
    
      RETURN

      END

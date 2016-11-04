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

      NFILIN=0
      INF=20
      ITYP=0
      ITYPE=0


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
 904        CONTINUE
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
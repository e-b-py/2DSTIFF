C     ..
C     .. Parameters ..
C     ..
      PARAMETER        ( MAXND = 100,
     ;                   MAXEL = 100,
     ;                   MAXSC = 10,
     ;                   MAXMT = 5,
     ;                   MAXRT = 20,
     ;                   MAXLK = 5,
     ;                   MAXDF = 3*MAXND )
C     ..
C     .. Scalar Variables ..
C     ..
      INTEGER*4        LDCRD, LDCON, LDCSEC, LDCMAT, LDIDOF, LDELDS,
     ;                 LDLNK, LDRST, LDKG, NDOF, NELE, NSEC, NMAT,
     ;                 NRST, NLNK, NNDL, NELL
C     ..
C     .. Array Variables ..
C     ..
      INTEGER*4        CON(MAXEL, 2), ISEC(MAXEL), IMAT(MAXEL),
     ;                 IDOF(MAXND, 3), RST(MAXRT, 2), LNK(MAXLK, 3)
      REAL*8           CRD(MAXND, 2), CSEC(MAXSC, 4), CMAT(MAXMT, 4),
     ;                 ELDS(MAXEL, 5), NLDS(MAXND), DSPG(MAXDF),
     ;                 DSPL(6), KL(6, 6), KG(MAXDF, MAXDF), FL(6),
     ;                 FG(MAXDF)
      COMMON /CONFIG/ NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                NELL
C 
C     .. Executable Statements .. 
C
C     Initialize the leading dimensions
C
      LDCRD = MAXND   
      LDCON = MAXEL  
      LDCSEC= MAXSC  
      LDCMAT= MAXMT   
      LDIDOF= MAXND   
      LDELDS= MAXEL   
      LDNLDS= MAXND   
      LDRST = MAXRT   
      LDLNK = MAXLK   
      LDKG  = MAXDF
C
C     Initialize the arrays
C
      CON  = 0
      ISEC = 0
      IMAT = 0
      IDOF = 0
      RST  = 0
      LNK  = 0
      CRD  = 0.D0
      CSEC = 0.D0
      CMAT = 0.D0
      ELDS = 0.D0
      NLDS = 0.D0
      DSPG = 0.D0
      DSPL = 0.D0
      KL   = 0.D0
      KG   = 0.D0
      FL   = 0.D0
      FG   = 0.D0
C
C     Call the subroutines
C     
      CALL GEOMET(CRD, LDCRD, CON, LDCON, CSEC, LDCSEC, ISEC, CMAT, 
     ;           LDCMAT, IMAT, RST, LDRST, LNK, LDLNK)

      CALL SCODE(IDOF, LDIDOF, RST, LDRST, LNK, LDLNK)
      PRINT *, 'Number of DOFs', NDOF
      PRINT *, 'After SCODE, IDOF matrix'
      DO 30 I = 1, NNODE
         PRINT *,
         DO 40 J = 1, 3
            WRITE(*,'(I5)',ADVANCE='NO') IDOF(I, J)
   40    CONTINUE
   30 CONTINUE
      CALL LOADS(IDOF, LDIDOF, ELDS, LDELDS, NLDS)
      PRINT *,
      PRINT *, 'Nodal Loads'
      DO 50 I = 1, NDOF
         WRITE(*, *) NLDS(I)
   50 CONTINUE
      PRINT *,
      PRINT *, 'Element Loads'
      DO 60 I = 1, NELE
         PRINT *,
         DO 70 J = 1, 5
         WRITE(*, '(F8.2)', ADVANCE='NO') ELDS(I, J)
   70    CONTINUE
   60 CONTINUE
      PRINT *,
      CALL ASSEMB(CON, LDCON, CRD, LDCRD, IDOF, LDIDOF, CSEC, LDCSEC,
     ;            ISEC, CMAT, LDCMAT, IMAT, ELDS, LDELDS, NLDS, KL,
     ;            FL, KG, LDKG, FG)
      PRINT *,
      PRINT *, 'GLOBAL STIFFNESS MATRIX'
      DO 80 I = 1, NDOF
         WRITE(*,*)
         DO 90 J = 1, NDOF
            WRITE(*, '(D14.4)', ADVANCE='NO') KG(I, J)
   90    CONTINUE
   80    CONTINUE
      PRINT *,
      PRINT *, 'GLOBAL FORCE MATRIX'
      DO 100 I = 1, NDOF
         WRITE(*, '(F18.5)') FG(I)
  100 CONTINUE
      CALL GSOLVE(KG, LDKG, NDOF, NDOF, DSPG, FG)
      PRINT *,
      PRINT *, 'GLOBAL DISPLACEMENTS'
      DO 110 I = 1, NDOF
         WRITE(*, '(F18.10)') DSPG(I)
  110 CONTINUE
      CALL STRE(CON, LDCON, CRD, LDCRD, IDOF, LDIDOF, ELDS, LDELDS, 
     ;          DSPG)
      STOP
      END


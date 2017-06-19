C     ..
C     .. Parameters ..
C     ..
      PARAMETER        ( MAXND = 100,
     ;                   MAXEL = 100,
     ;                   MAXSC = 10,
     ;                   MAXMT = 5,
     ;                   MAXRT = 40,
     ;                   MAXLK = 20,
     ;                   MAXDF = 3*MAXND )
C     ..
C     .. Scalar Variables ..
C     ..
      INTEGER*4        LDCRD, LDCON, LDCSEC, LDCMAT, LDIDOF, LDELDS,
     ;                 LDEPRS, LDLNK, LDRST, LDLNKE, LDRSTE, LDKG, NDOF,
     ;                 NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE,
     ;                 NLNKE, NNDL, NELL, NPRES     
C     ..
C     .. Array Variables ..
C     ..
      INTEGER*4        CON(MAXEL, 2), ISEC(MAXEL), IMAT(MAXEL),
     ;                 IDOF(MAXND, 3), RST(MAXRT, 2), LNK(MAXLK, 3)
      REAL*8           CRD(MAXND, 2), CSEC(MAXSC, 4), CMAT(MAXMT, 4),
     ;                 ELDS(MAXEL, 5), NLDS(MAXND), DSPG(MAXDF),
     ;                 DSPL(6), KL(6, 6), KG(MAXDF, MAXDF), FL(6),
     ;                 FG(MAXDF), RSTE(MAXRT, 3), LNKE(MAXLK, 4),
     ;                 EPRS(MAXEL, 4)
      COMMON /CONFIG/ NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF, NNDL, NELL, NPRES
C 
C     .. Executable Statements .. 
C
C     Open files for I/O and debug
C
      OPEN(UNIT=11, FILE='/dev/stdin')
      OPEN(UNIT=12, FILE='../out/output.dat')
      OPEN(UNIT=15, FILE='../debug/debug.out')
      OPEN(UNIT=16, FILE='../out/elstiff.dat', FORM='UNFORMATTED')
      OPEN(UNIT=18, FILE='../out/axial.vtk') 
      OPEN(UNIT=20, FILE='../out/shear.vtk') 
      OPEN(UNIT=22, FILE='../out/moment.vtk') 
      OPEN(UNIT=24, FILE='../out/deformed.vtk') 
      OPEN(UNIT=26, FILE='../out/undeformed.vtk') 
C
C     Initialize the leading dimensions
C
      LDCRD = MAXND   
      LDCON = MAXEL  
      LDCSEC= MAXSC  
      LDCMAT= MAXMT   
      LDIDOF= MAXND   
      LDELDS= MAXEL   
      LDEPRS= MAXEL
      LDNLDS= MAXND   
      LDRST = MAXRT   
      LDLNK = MAXLK   
      LDRSTE= MAXRT   
      LDLNKE= MAXLK   
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
      RSTE= 0.D0
      LNKE= 0.D0
      CRD  = 0.D0
      CSEC = 0.D0
      CMAT = 0.D0
      ELDS = 0.D0
      EPRS = 0.D0
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
     ;           LDCMAT, IMAT, RST, LDRST, LNK, LDLNK, RSTE, LDRSTE,
     ;           LNKE, LDLNKE)
      CALL SCODE(IDOF, LDIDOF, RST, LDRST, LNK, LDLNK)
      CALL LOADS(IDOF, LDIDOF, ELDS, LDELDS, EPRS, LDEPRS, NLDS)
      CALL ASSEMB(CON, LDCON, CRD, LDCRD, IDOF, LDIDOF, CSEC, LDCSEC,
     ;            ISEC, CMAT, LDCMAT, IMAT, ELDS, LDELDS, EPRS, LDEPRS,
     ;            NLDS, KL, FL, KG, LDKG, FG)
      CALL JOINT(KG, LDKG, RSTE, LDRSTE, LNKE, LDLNKE, IDOF, LDIDOF)
      CALL GSOLVE(KG, LDKG, NDOF, NDOF, DSPG, FG)
      CALL STRE(CON, LDCON, CRD, LDCRD, IDOF, LDIDOF, ELDS, LDELDS, 
     ;          EPRS, LDEPRS, DSPG)
      STOP
      END


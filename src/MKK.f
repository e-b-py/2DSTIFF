C
C =====================================================================
      SUBROUTINE MKK(I, CON, LDCON, CRD, LDCRD, IDOF, LDIDOF, CSEC, 
     ;               LDCSEC, ISEC, CMAT, LDCMAT, IMAT, ELDS, LDELDS, 
     ;               NLDS, KL, FL, KG, LDKG, FG, ELDOF)
C
C     Computes the local stiffness and force matrix of the given element
C     in the global coordinates
C
C     .. Scalar Arguments ..
C     INTEGER*4        LDCON, LDCRD, LDIDOF, LDCSEC, LDCMAT, LDELDS, 
C                      LDKG, I
C                      
C     ..
C     .. Array Arguments ..
C     INTEGER*4        CON(LDCON, *)  : Connectivity matrix
C                      IDOF(LDIDOF, *): Matrix of the degrees of freedom
C                      ISEC(*)        : Section incidence vector
C                      IMAT(*)        : Material incidence vector
C
C     REAL*8           CRD(LDCRD, *)  : Matrix of coordinates
C                      CSEC(LDCSEC, *): Matrix of section properties
C                      CMAT(LDCMAT, *): Matrix of material properties
C                      NLDS(*)        : Vector of nodal loads
C                      ELDS(LDELDS, *): Matrix of element loads
C                      KL(6,6)        : Local stiffness matrix
C                      FL(6)          : Local force matrix
C                      KG(LDKG, *)    : Global stiffness matrix
C                      FG(*)          : Global force matrix
C                      ELDOF(6)       : Global DOFs of the element
C     ..
C     .. Local Scalars ..
C     INTEGER*4        SNOD: Start node of the element
C                      ENOD: End node of the element
C                      CDOF: Current DOF used when looping through DOFs
C                      
C     REAL*8           SCX : Start node's x-coordinate                      
C                      SCY : Start node's y-coordinate                      
C                      ECX : End node's x-coordinate                      
C                      ECY : End node's y-coordinate                      
C                      LX  : Difference between x-coordinates
C                      LY  : Difference between y-coordinates
C                      L   : Length of the element
C                      CSA : Cosine of the angle
C                      SNA : Sine of the angle
C                      E   : Young's modulus
C                      V   : Poisson's ratio
C                      ALPH: Thermal expansion coefficient
C                      GAMM: Unit weight
C                      G   : Shear modulus
C                      A   : Cross sectional area
C                      INE : Moment of inertia
C                      H   : Section depth
C                      X   : Shape factor
C                      PHI : Shear deformability coefficient
C                      PX  : Distributed axial load
C                      PY1 : Value of distr. vertical load at node 1
C                      PY2 : Value of distr. vertical load at node 2
C                      DTT : Temperature variation at the top face
C                      DTB : Temperature variation at the bottom face
C                      C   : Coefficient used in linearly varying load
C                      QY  : Difference between Y1 and Y2
C                      GX  : Self weight, axial component
C                      GY  : Self weight, vertical component
C                      DDT : Difference between temperature variations
C                      DTM : Mean temperature variation
C     ..
C     .. Local Arrays ..
C     REAL*8           T(6,6)   : Coordinate transformation matrix 
C                      FEF(6)   : Fixed end forces
C                      TMPK(6,6): Temporary array used in transformation
C                      TMPF(6)  : Temporary array used in transformation
C     ..
C     .. Common Scalars ..
C     INTEGER*4        NNODE: number of nodes
C                      NELE : number of elements
C                      NSEC : number of cross sections
C                      NMAT : number of materials
C                      NRST : number of restraints
C                      NLNK : number of links
C                      NRSTE: number of elastic restraints
C                      NLNKE: number of elastic links
C                      NDOF : number of degrees of freedom
C                      NNDL : number of nodal loads
C                      NELL : number of element loads
C                      NPRES: number of prestressed elements
C     ,,
C     .. Scalar Arguments ..
      INTEGER*4        LDCON, LDCRD, LDIDOF, LDCSEC, LDCMAT, LDELDS,
     ;                 LDKG, I
C     ..
C     .. Array Arguments ..
      INTEGER*4        CON(LDCON, *), IDOF(LDIDOF, *), ISEC(*), IMAT(*)
      REAL*8           CRD(LDCRD, *), CSEC(LDCSEC, *), CMAT(LDCMAT, *),
     ;                 NLDS(*), ELDS(LDELDS, *), KL(6, 6), FL(6),
     ;                 KG(LDKG, *), FG(*), ELDOF(6)
C     ..
C =====================================================================
C     .. Local Scalars ..
      INTEGER*4        SNOD, ENOD, M, N, J, Z, CDOF
      REAL*8           SCX, SCY, ECX, ECY, LX, LY, L, CSA, SNA, E, V,
     ;                 ALPH, GAMM, G, A, INE, H, X, PHI, PX, PY1, PY2,
     ;                 DTT, DTB, C, QY, GX, GY, DDT, DTM
C     ..
C     .. Local Arrays ..
      REAL*8           T(6, 6), FEF(6), TMPK(6, 6)
C     .. Common Scalars ..
      INTEGER*4       NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF, NNDL, NELL, NPRES
      COMMON /CONFIG/ NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF, NNDL, NELL, NPRES
C     ..
C     .. Executable statements ..
C
C     Get the start & end coordinates of the element, associate the
C     global degrees of freedom corresponding to the locals
C
      SNOD = CON(I, 1)
      ENOD = CON(I, 2)
      SCX  = CRD(SNOD, 1)
      SCY  = CRD(SNOD, 2)
      ECX  = CRD(ENOD, 1)   
      ECY  = CRD(ENOD, 2)   
      DO 10 J = 1, 3
         ELDOF(J) = IDOF(SNOD, J)
   10 CONTINUE
      DO 20 J = 1, 3
         ELDOF(J+3) = IDOF(ENOD, J)
   20 CONTINUE
C
C     Compute length and terms of the transformation matrix
C
      LX = ECX - SCX
      LY = ECY - SCY
      L  = DSQRT(LX**2 + LY**2)
      CSA= LX / L
      SNA= LY / L
C
C     Initialize the transformation matrix
C
      T = 0.D0
      T(1,1) = CSA
      T(2,1) = SNA
      T(1,2) = -SNA
      T(2,2) = CSA
      T(3,3) = 1
      T(4,4) = CSA
      T(5,4) = SNA
      T(4,5) = -SNA
      T(5,5) = CSA
      T(6,6) = 1
      WRITE(15, '(A, I3)') 'ELEMENT', I
      WRITE(15, '(A, I3)') 'SNOD', SNOD
      WRITE(15, '(A, I3)') 'ENOD', ENOD
      WRITE(15, '(A, F8.3)') 'SCX', SCX
      WRITE(15, '(A, F8.3)') 'SCY', SCY
      WRITE(15, '(A, F8.3)') 'ECX', ECX
      WRITE(15, '(A, F8.3)') 'ECY', ECY
      WRITE(15, '(A, F8.3)') 'LX', LX
      WRITE(15, '(A, F8.3)') 'LY', LY
      WRITE(15, '(A, F8.3)') 'L', L
      WRITE(15, '(A, F8.3)') 'COS', CSA
      WRITE(15, '(A, F8.3)') 'SIN', SNA
      WRITE(15, '(A)') 'Transformation Matrix'
      DO 11 M = 1, 6
         WRITE(15, *)
         DO 12 N = 1, 6
            WRITE(15, '(F8.3)',ADVANCE='NO') T(M, N)
   12    CONTINUE
   11    CONTINUE
      WRITE(15, *)
C
C     Write the transformation matrix to file
C
      WRITE(16) T
C
C     Get the section properties (A, I, h, X)
C
      A   = CSEC(ISEC(I), 1)
      INE = CSEC(ISEC(I), 2)
      H   = CSEC(ISEC(I), 3)
      X   = CSEC(ISEC(I), 4)
C
C     Get the material properties (E, V, alpha, gamma)
C
      E = CMAT(IMAT(I), 1)
      V = CMAT(IMAT(I), 2)
      ALPH = CMAT(IMAT(I), 3)
      GAMM = CMAT(IMAT(I), 4)
C
C     Compute G and PHI using these
C
      G = E / (2.D0*(1+V))
      PHI = X*12.D0*E*INE/(L**2.D0*G*A)
C
C     Initialize the stiffness matrix
C
      KL = 0.D0
      KL(1,1) = E*A/L
      KL(4,1) = -KL(1,1)
      KL(2,2) = 12.D0*E*INE / ( L**3*(1.D0+PHI) )
      KL(3,2) = 6.D0*E*INE / ( L**2*(1.D0+PHI) )
      KL(5,2) = -KL(2,2)
      KL(6,2) = KL(3,2)
      KL(3,3) = (4.D0+PHI)*E*INE / L*(1.D0+PHI)
      KL(5,3) = -KL(3,2)
      KL(6,3) = (2.D0-PHI)*E*INE / L*(1.D0+PHI)
      KL(4,4) = KL(1,1)
      KL(5,5) = KL(2,2)
      KL(6,5) = -KL(3,2)
      KL(6,6) = KL(3,3)
C     Symmetric
      DO 40 N = 2, 6
         DO 50 M = 1, N-1
            KL(M, N) = KL(N, M)
   50    CONTINUE
   40 CONTINUE                
      WRITE(15, '(A)') 'KL Before transformation'
      DO 13 M = 1, 6
         WRITE(15,*)
         DO 14 N = 1, 6
         WRITE(15, '(D14.4)', ADVANCE='NO') KL(M, N)
   14    CONTINUE
   13    CONTINUE
      WRITE(15,*)
C
C     Write local stiffness matrix to file before rotating
C
      WRITE(16) KL
C
C     Transform the stiffness matrix
C
      TMPK = 0.D0
      DO 60 M = 1, 6
         DO 70 N = 1, 6
            DO 80 J = 1, 6
               DO 90 Z = 1, 6
                  TMPK(M, N) = TMPK(M, N) + T(M, J) * KL(J, Z) * T(N, Z)
   90          CONTINUE
   80       CONTINUE
   70    CONTINUE
   60 CONTINUE
      WRITE(15, *)
      WRITE(15, '(A)') 'TMPK Matrix'
      DO 15 M= 1,6
         WRITE(15, *)
         DO 16 N=1,6
            WRITE(15, '(D14.4)', ADVANCE='NO') TMPK(M, N)
   16    CONTINUE
   15    CONTINUE
      DO 100 M = 1, 6
      DO 100 N = 1, 6
         KL(M, N) = TMPK(M, N)
  100 CONTINUE
      WRITE(15, '(A)') 'KL After transformation'
      DO 17 M = 1, 6
         WRITE(15,*)
         DO 18 N = 1, 6
         WRITE(15, '(D14.4)', ADVANCE='NO') KL(M, N)
   18    CONTINUE
   17    CONTINUE
      WRITE(15,*)

C
C     Compute the fixed end forces TODO burda kaldim FIXME 
C
      FEF = 0.D0
      FL  = 0.D0
C
C     Distributed loads (with self weight contributions)
C
      PX  = ELDS(I, 1) + (-GAMM*A*SNA)
      PY1 = ELDS(I, 2) + (-GAMM*A*CSA)
      PY2 = ELDS(I, 3) + (-GAMM*A*CSA)
      FEF(1) = -PX*L/2.D0
      FEF(4) = -PX*L/2.D0
      IF( DABS(PY2 - PY1).LT.1.D-14 ) THEN
         FEF(2) = -PY1*L/2.D0
         FEF(3) = -PY1*L**2/12.D0
         FEF(5) = -PY1*L/2.D0
         FEF(6) = PY1*L**2/12.D0
      ELSEIF( DABS(PY2).GT.DABS(PY1) ) THEN
         C = PHI / (1+PHI)
         QY= PY2 - PY1
         FEF(2) = -PY1*L/2.D0 - 9.D0/60.D0*QY*L*(1.D0 + C/9.D0)
         FEF(3) = -PY1*L**2/12.D0 - QY*L**2/30.D0*(1.D0 + C/9.D0)
         FEF(5) = -PY1*L/2.D0 - 21.D0/60.D0*QY*L*(1.D0 - C/21.D0)
         FEF(6) = PY1*L/12.D0 + QY*L**2/20.D0*(1.D0 - C/6.D0)
      ELSE
         C = PHI / (1+PHI)
         QY= PY1 - PY2
         FEF(2) = -PY1*L/2.D0 - 21.D0/60.D0*QY*L*(1.D0 - C/21.D0)
         FEF(3) = PY1*L/12.D0 + QY*L**2/20.D0*(1.D0 - C/6.D0)
         FEF(5) = -PY1*L/2.D0 - 9.D0/60.D0*QY*L*(1.D0 + C/9.D0)
         FEF(6) = -PY1*L**2/12.D0 - QY*L**2/30.D0*(1.D0 + C/9.D0)
      ENDIF
C
C     Temperature variation
C
      DTT = ELDS(I, 4)
      DTB = ELDS(I, 5)
      DDT = DTT - DTB
      IF( DABS(DTT).LT.1.D-14 ) THEN
         FEF(1) = FEF(1) + E*A*ALPH*DTT
         FEF(4) = FEF(4) - E*A*ALPH*DTT
      ELSE
         DTM = (DTT + DTB) / 2
         FEF(1) = FEF(1) + E*A*ALPH*DTM
         FEF(4) = FEF(4) - E*A*ALPH*DTM
         FEF(3) = FEF(3) - 2*ALPH*DTT*E*INE/H
         FEF(6) = FEF(6) + 2*ALPH*DTT*E*INE/H
      ENDIF
C
C     Write FEF to file before rotating 
C
      WRITE(16) FEF     
C
C      Transform the force matrix
C
      DO 120 M = 1, 6
         DO 130 N = 1, 6
            FL(M) = FL(M) + T(M, N) * (-FEF(N))
  130    CONTINUE
  120 CONTINUE
      RETURN
C
C     .. End of MKK ..
C
      END

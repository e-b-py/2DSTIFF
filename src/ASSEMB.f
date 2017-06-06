C
C =====================================================================
      SUBROUTINE ASSEMB(CON, LDCON, CRD, LDCRD, IDOF, LDIDOF, CSEC, 
     ;                  LDCSEC, ISEC, CMAT, LDCMAT, IMAT, ELDS, LDELDS, 
     ;                  NLDS, KL, FL, KG, LDKG, FG)
C
C     Assemble the global stiffness matrix and force vector.
C
C     .. Scalar Arguments ..
C     INTEGER*4        LDCON, LDCRD, LDIDOF, LDCSEC, LDCMAT, LDELDS, 
C                      LDKG
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
C     ..
C     .. Local Scalars ..
C     INTEGER*4        SNOD : Start node of the element
C                      ENOD : End node of the element
C                      RI   : Raw index  
C                      CI   : Column index  
C     ..
C     .. Local Arrays ..
C     REAL*8           ELDOF(6): Global DOFs of the element
C     ..
C     .. Common Scalars ..
C     INTEGER*4        NNODE: number of nodes
C                      NELE : number of elements
C                      NSEC : number of cross sections
C                      NMAT : number of materials
C                      NRST : number of restraints
C                      NLNK : number of links
C                      NDOF : number of degrees of freedom
C                      NNDL : number of nodal loads
C                      NELL : number of element loads
C     ,,
C     .. Scalar Arguments ..
      INTEGER*4        LDCON, LDCRD, LDIDOF, LDCSEC, LDCMAT, LDELDS,
     ;                 LDKG
C     ..
C     .. Array Arguments ..
      INTEGER*4        CON(LDCON, *), IDOF(LDIDOF, *), ISEC(*), IMAT(*)
      REAL*8           CRD(LDCRD, *), CSEC(LDCSEC, *), CMAT(LDCMAT, *),
     ;                 NLDS(*), ELDS(LDELDS, *), KL(6,6), FL(6),
     ;                 KG(LDKG, *), FG(*)
C     ..
C =====================================================================
C     .. Local Scalars ..
      INTEGER*4        SNOD, ENOD, RI, CI
C     ..
C     .. Local Arrays ..
      REAL*8           ELDOF(6)
C     .. Common Scalars ..
      INTEGER*4        NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                 NELL
      COMMON /CONFIG/  NNODE, NELE, NSEC, NMAT, NRST, NLNK, NDOF, NNDL,
     ;                 NELL
C     ..
C     .. Executable statements ..
C
C     Loop over elements
C
      ELDOF = 0.D0
      DO 10 I = 1, NELE
         CALL MKK(I, CON, LDCON, CRD, LDCRD, IDOF, LDIDOF, CSEC, 
     ;            LDCSEC, ISEC, CMAT, LDCMAT, IMAT, ELDS, LDELDS, 
     ;            NLDS, KL, FL, KG, LDKG, FG, ELDOF)
C
C        Assemble 
C
         DO 40 M = 1, 6
            RI = ELDOF(M)
            IF( RI.EQ.-1 ) GO TO 40
            FG(RI) = FG(RI) + FL(M)
            DO 50 N = 1, 6
               CI = ELDOF(N)
               IF ( CI.EQ.-1) GO TO 50
               KG(RI, CI) = KG(RI, CI) + KL(M, N)
   50       CONTINUE
   40    CONTINUE
   10 CONTINUE
C
C     Add nodal loads to FG
C
      DO 60 I = 1, NDOF
         FG(I) = FG(I) + NLDS(I)
   60 CONTINUE
      RETURN
C
C     .. End of ASSEMB ..
C
      END

         

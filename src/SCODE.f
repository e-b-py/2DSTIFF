C
C =====================================================================
      SUBROUTINE SCODE(IDOF, LDIDOF, RST, LDRST, LNK, LDLNK)
C
C     Numerates the degrees of freedom considering the restraints
C     and master-slave relations.
C
C     .. Scalar Arguments ..
C     INTEGER*4        LDIDOF, LDRST, LDLNK
C     ..
C     .. Array Arguments ..
C     INTEGER*4        IDOF(LDIDOF, *): Matrix of the degrees of freedom
C                      RST(LDRST, *)  : Matrix of restraints
C                      LNK(LDLNK, *)  : Matrix of internal links
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
C     ,,
C     .. Scalar Arguments ..
      INTEGER*4        LDIDOF, LDRST, LDLNK
C     ..
C     .. Array Arguments ..
      INTEGER*4        IDOF(LDIDOF, *), RST(LDRST, *), LNK(LDLNK, *)
C     ..
C =====================================================================
C     .. Common Scalars ..
      INTEGER*4       NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF
      COMMON /CONFIG/ NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF
C     ..
C     .. Executable statements ..
C
C     Loop through the restraints, assign -1 to corresponding DOFs
C
      DO 10 I = 1, NRST
         IDOF( RST(I, 1), RST(I, 2) ) = -1
   10 CONTINUE
C
C     Loop through the links, assign slave DOFs their masters' node
C     number
C
      DO 20 I = 1, NLNK
         IDOF( LNK(I, 2) , LNK(I, 3) ) = LNK(I, 1)
   20 CONTINUE
C
C     Numerate the DOFS
C
      NDOF = 0
      DO 30 I = 1, NNODE
         DO 40 J = 1, 3
            IF( IDOF(I, J).EQ.-1 ) GO TO 40
            IF( IDOF(I, J).EQ.0 ) THEN
               NDOF = NDOF + 1
               IDOF(I, J) = NDOF
            ELSE
               IDOF(I, J) = IDOF(IDOF(I, J), J)
            ENDIF
   40    CONTINUE
   30 CONTINUE
      WRITE(15, '(A/)') 'IDOF'
      DO 901 I = 1, NNODE
         WRITE(15, '(I3, I3, I3)') IDOF(I, 1), IDOF(I, 2), IDOF(I, 3)
  901 CONTINUE
      WRITE(15, '(/)')
      RETURN
C
C     .. End of SCODE ..
C
      END

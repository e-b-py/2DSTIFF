C
C =====================================================================
      SUBROUTINE JOINT(KG, LDKG, RSTE, LDRSTE, LNKE, LDLNKE, IDOF, 
     ;                 LDIDOF)
C
C     Apply the elastic restraints and links to the stiffness matrix
C
C     .. Scalar Arguments ..
C     INTEGER*4        LDKG, LDRSTE, LDLNKE, LDIDOF
C     ..
C     .. Array Arguments ..
C     INTEGER*4        IDOF(LDIDOF, *): Matrix of the degrees of freedom
C
C     REAL*8           RSTE(LDRSTE, *): Matrix of elastic restraints
C                      LNKE(LDLNKE, *): Matrix of elastic links
C                      KG(LDKG, *)    : Global stiffness matrix
C     ..
C     .. Local Scalars ..
C     INTEGER*4        CNODE1 : Node 1 of the elastic link (or restr.)
C                      CNODE2 : Node 2 of the elastic link
C                      CDIR   : Direction of the e.link or e.restr.
C                      CDOF1  : DOF corresponding to CNODE1 and CDIR
C                      CDOF2  : DOF corresponding to CNODE2 and CDIR
C
C     REAL*8           CK     : Spring stiffness
C                      
C     ..
C     .. Local Arrays ..
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
C     ,,
C     .. Scalar Arguments ..
      INTEGER*4        LDKG, LDRSTE, LDLNKE, LDIDOF
C      REAL*8           
C     ..
C     .. Array Arguments ..
      INTEGER*4        IDOF(LDIDOF, *)
      REAL*8           RSTE(LDRSTE, *), LNKE(LDLNKE, *), KG(LDKG, *)
C     ..
C =====================================================================
C     .. Local Scalars ..
      INTEGER*4       CNODE1, CNODE2, CDIR, CDOF1, CDOF2
      REAL*8          CK
C     ..
C     .. Local Arrays ..

C     .. Common Scalars ..
      INTEGER*4       NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF, NNDL, NELL 
      COMMON /CONFIG/ NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF, NNDL, NELL
C     ..
C     .. Executable statements ..
C
C     Elastic restraints
C      
      DO 10 I = 1, NRSTE
         CNODE1 = IDINT(RSTE(I, 1))
         CDIR   = IDINT(RSTE(I, 2))
         CK     = RSTE(I, 3)
         CDOF1  = IDOF(CNODE1, CDIR)
         IF( CDOF.EQ.-1 ) THEN
            WRITE(*, '(A, I3)') 
     ;'Cannot apply elastic restraint to restrained node. Node:', CNODE1
            GO TO 10
         ENDIF
         KG(CDOF1, CDOF1) = KG(CDOF1, CDOF1) + CK
   10 CONTINUE
C
C     Elastic links
C      
      DO 20 I = 1, NLNKE
         CNODE1 = IDINT(LNKE(I, 1))
         CNODE2 = IDINT(LNKE(I, 2))
         CDIR   = IDINT(LNKE(I, 3))
         CK     = LNKE(I, 4)
         CDOF1  = IDOF(CNODE1, CDIR)
         CDOF2  = IDOF(CNODE2, CDIR)
         IF( ( CDOF1.EQ.-1 ).OR.( CDOF2.EQ.-1 ) ) THEN
            WRITE(*, '(A, I3, I3)') 
     ;'Cannot apply elastic link to restrained node. Nodes:', 
     ; CNODE1, CNODE2
            GO TO 20
         ENDIF
         KG(CDOF1, CDOF1) = KG(CDOF1, CDOF1) + CK
         KG(CDOF2, CDOF2) = KG(CDOF2, CDOF2) + CK
         KG(CDOF1, CDOF2) = KG(CDOF1, CDOF2) - CK
         KG(CDOF2, CDOF1) = KG(CDOF2, CDOF1) - CK
   20 CONTINUE
C
C     .. End of JOINT ..
C
      END



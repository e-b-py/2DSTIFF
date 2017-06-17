C
C =====================================================================
      SUBROUTINE LOADS(IDOF, LDIDOF, ELDS, LDELDS, EPRS, LDEPRS, NLDS)
C
C     Reads the input data, processes the loads and assigns them to the
C     correct positions in the corresponding arrays.
C
C     .. Scalar Arguments ..
C     INTEGER*4        LDIDOF, LDELDS, LDEPRS
C     ..
C     .. Array Arguments ..
C     REAL*8           NLDS(*)        : Vector of nodal loads
C                      ELDS(LDELDS, *): Matrix of element loads
C                      EPRS(LDEPRS, *): Matrix of prestressing
C     INTEGER*4        IDOF(LDIDOF, *): Matrix of the degrees of freedom
C     ..
C     .. Local Scalars ..
C     REAL*8           CMAGN: Magnitude of the nodal load
C                      PX   : Distributed constant axial load
C                      PY1  : Value of the distributed vertical load at
C                             the first node
C                      PY2  : Value of the distributed vertical load at
C                             the second node
C                      DTTOP: Temperature variation at the top face
C                      DTBOT: Temperature variation at the bottom face
C   
C     INTEGER*4        CNODE: Node to which the nodal load is applied
C                      CDIR : Direction of the applied nodal load
C                      CDOF : DOF corresponding to the node and
C                             direction of a given nodal load
C                      CEL  : Element number of the current loaded element
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
      INTEGER*4        LDIDOF, LDELDS, LDEPRS
C     ..
C     .. Array Arguments ..
      INTEGER*4        IDOF(LDIDOF, *)
      REAL*8           NLDS(*), ELDS(LDELDS, *), EPRS(LDEPRS, *)
C     ..
C =====================================================================
C     .. Local Scalars ..
      REAL*8           CMAGN, PX, PY1, PY2, DTTOP, DTBOT
      INTEGER*4        CNODE, CDIR, CDOF, CEL
C     ..
C     .. Common Scalars ..
      INTEGER*4       NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF, NNDL, NELL
      COMMON /CONFIG/ NNODE, NELE, NSEC, NMAT, NRST, NLNK, NRSTE, NLNKE,
     ;                NDOF, NNDL, NELL
      
C     ..
C     .. Executable statements ..
C
C     Read the nodal loads, assign them to the nodal load vector.
C
      WRITE(12, '(/A)') '# NODAL LOADS'
      WRITE(12, '(A, T14, A, T32, A)') 'Node', 'Direction', 'Magnitude'
      READ(11, *)
      READ(11, *) NNDL
      DO 10 I = 1, NNDL
         READ(11, *) CNODE, CDIR, CMAGN
         WRITE(12, '(I3, T16, I3, T32, F6.2)') CNODE, CDIR, CMAGN
         CDOF = IDOF(CNODE, CDIR)
         NLDS(CDOF) = CMAGN
   10 CONTINUE
C
C     Read the element loads, assign them to the element load matrix.
C
      WRITE(12, '(/A)') '# ELEMENT LOADS'
      WRITE(12, '(A, T15, A, T26, A, T38, A, T49, A, T60, A)') 
     ;            'Element', 'Px', 'Py1', 'Py2', 'DTtop', 'DTbottom'
      READ(11, *)
      READ(11, *) NELL
      DO 20 I = 1, NELL
         READ(11, *) CEL, (ELDS(CEL, J), J=1,5)
         WRITE(12, '(I3, T12, F6.2, T24, F6.2, T36, F6.2, T48, F6.2,
     ;               T60, F6.2)') CEL, (ELDS(CEL,J), J=1,5)
   20 CONTINUE
C
C     Read the prestressing data, assign them to the prestressing matrix
C
      WRITE(12, '(/A)') '# PRESTRESSING'
      WRITE(12, '(A, T15, A, T27, A, T39, A, T51, A)')
     ;            'Element', 'e1', 'em', 'e2', 'P'
      READ(11, *)
      READ(11, *) NPRES
      DO 30 I = 1, NPRES
         READ(11, *) CEL, (EPRS(CEL, J), J=1,4)
         WRITE(12, '(I3, T12, F6.2, T24, F6.2, T36, F6.2, T48, F6.2)')
     ;              CEL, (EPRS(CEL, J), J=1,4)
  30  CONTINUE
      RETURN
C      
C     .. End of LOADS ..      
C      
      END

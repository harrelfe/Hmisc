      subroutine hlqest(X, N, LB, RB, Q, ran, result)
C
C    Subroutine hlqest
C
C    PURPOSE       COMPUTES THE HODGES-LEHMANN LOCATION ESTIMATOR:
C                  MEDIAN OF ( X(I) + X(J) ) / 2   FOR 1 LE I LE J LE N
C
C    USAGE         hlqest(X,N,LB,RB,Q,ran,result)
C
C    ARGUMENTS  X   REAL*8 ARRAY OF OBSERVATIONS  (INPUT)
C                 * VALUES OF X MUST BE IN NONDECREASING ORDER *
C
C               N   INTEGER NUMBER OF OBSERVATIONS  (INPUT)
C                 * N MUST NOT BE LESS THAN 1 *
C
C               LB  INTEGER ARRAY OF LENGTH N FOR WORKSPACE
C
C               RB  INTEGER ARRAY OF LENGTH N FOR WORKSPACE
C
C               Q   INTEGER ARRAY OF LENGTH N FOR WORKSPACE
C
C               ran  1000 real*8 uniform(0,1) random numbers
C
C               result REAL*8 result
C
C         NOTE ---  ONLY LB,RB, Q, result ARE CHANGED IN COMPUTATION
C
C   NOTES           HLQEST HAS AN EXPECTED TIME COMPLEXITY ON
C                   THE ORDER OF N * LG( N )
C
C  J F MONAHAN, APRIL 1982, DEPT OF STAT, N C S U, RALEIGH, N C 27650
C  FINAL VERSION  JUNE 1983
C 
C  Modified 2024-10-19 FE Harrell, Dept of Biostatistics, Vanderbilt University, Nashville TN
C
C  Changes: Changed integer variables other than N to long integers so that sample size can be
C           up to sqrt(2^63 - 1) instead of sqrt(2^31 - 1)
C           Removed random number generation and had the user pass random U(0,1) to the function.
C           Note: Need for more than 35 random numbers was not seen in any examples
C           Changed some intrinsic functions
C  Original Fortran code is at https://dl.acm.org/doi/suppl/10.1145/1271.319414/suppl_file/616.gz
C
C
      REAL*8 X(N), AMN, AMX, AM, result, ran(1000)
      INTEGER*8 LB(1), RB(1), Q(1), SM, SQ, I, J, K, K1, K2, L, NN,
     * MDLL, MDLU, LBI, RBI, MDLROW, IQ, ipiq
      INTEGER*4 N, nran
C
      nran = 0
C
C  TAKE CARE OF SPECIAL CASES: N=1 AND N=2
C
      IF (N.GT.2) GO TO 10
      result = X(1)
      IF (N.EQ.1) RETURN
      result = (X(1)+X(2))/2D0
      RETURN
C
C  FIND THE TOTAL NUMBER OF PAIRS (NN) AND THE MEDIAN(S) (K1,K2) NEEDED
C
   10 NN = N
      NN = (NN * (NN + 1)) / 2
      K1 = (NN + 1) / 2
      K2 = (NN + 2) / 2
C
C  INITIALIZE LEFT AND RIGHT BOUNDS
C
      DO 20 I=1,N
        LB(I) = I
        RB(I) = N
   20 CONTINUE
C  SM = NUMBER IN SET S AT STEP M
      SM = NN
C  L = NUMBER OF PAIRS LESS THAN THOSE IN SET S AT STEP M
      L = 0
C
C
C  USE THE MEDIAN OF X(I)'S TO PARTITION ON THE FIRST STEP
C
      MDLL = (N+1)/2
      MDLU = (N+2)/2
      AM = X(MDLL) + X(MDLU)
      GO TO 80
C
C  USE THE MIDRANGE OF SET S AS PARTITION ELEMENT WHEN TIES ARE LIKELY
C   -- OR GET THE AVERAGE OF THE LAST 2 ELEMENTS
C
   30 AMX = X(1) + X(1)
      AMN = X(N) + X(N)
      DO 40 I=1,N
C   SKIP THIS ROW IF NO ELEMENT IN IT IS IN SET S ON THIS STEP
        IF (LB(I).GT.RB(I)) GO TO 40
        LBI = LB(I)
C                             GET THE SMALLEST IN THIS ROW
        AMN = dmin1(AMN,X(LBI)+X(I))
        RBI = RB(I)
C                             GET THE LARGEST IN THIS ROW
        AMX = dmax1(AMX,X(RBI)+X(I))
   40 CONTINUE
      AM = (AMX+AMN)/2D0
C  BE CAREFUL TO CUT OFF SOMETHING -- ROUNDOFF CAN DO WIERD THINGS
      IF (AM.LE.AMN .OR. AM.GT.AMX) AM = AMX
C  UNLESS FINISHED, JUMP TO PARTITION STEP
      IF (AMN.NE.AMX .AND. SM.NE.2) GO TO 80
C  ALL DONE IF ALL OF S IS THE SAME -OR- IF ONLY 2 ELEMENTS ARE LEFT
      result = AM/2D0
      RETURN
C
C   *****   RESTART HERE UNLESS WORRIED ABOUT TIES   *****
C
   50 CONTINUE
C                        USE RANDOM ROW MEDIAN AS PARTITION ELEMENT
      nran = nran + 1
      if(nran .gt. 1000) nran = 1
      K = int(dfloat(SM) * ran(nran))
C                        K IS A RANDOM INTEGER FROM O TO SM-1
      DO 60 I=1,N
        J = I
        IF (K.LE.RB(I)-LB(I)) GO TO 70
        K = K - RB(I) + LB(I) - 1
   60 CONTINUE
C                        J IS A RANDOM ROW --- NOW GET ITS MEDIAN
   70 MDLROW = (LB(J)+RB(J))/2
      AM = X(J) + X(MDLROW)
C
C       *****   PARTITION STEP   *****
C
C  USE AM TO PARTITION S0 INTO 2 GROUPS: THOSE .LT. AM, THOSE .GE. AM
C  Q(I)= HOW MANY PAIRS (X(I)+X(J)) IN ROW I LESS THAN AM
   80 CONTINUE
      J = N
C                              START IN UPPER RIGHT CORNER
      SQ = 0
C                              I COUNTS ROWS
      DO 110 I=1,N
        Q(I) = 0
C                              HAVE WE HIT THE DIAGONAL ?
   90   IF (J.LT.I) GO TO 110
C                              SHALL WE MOVE LEFT ?
        IF (X(I)+X(J).LT.AM) GO TO 100
        J = J - 1
        GO TO 90
C                              WE'RE DONE IN THIS ROW
  100   Q(I) = J - I + 1
C  SQ = TOTAL NUMBER OF PAIRS LESS THAN AM
        SQ = SQ + Q(I)
  110 CONTINUE
C
C  ***  FINISHED PARTITION --- START BRANCHING  ***
C
C  IF CONSECUTIVE PARTITIONS ARE THE SAME WE PROBABLY HAVE TIES
      IF (SQ.EQ.L) GO TO 30
C
C  ARE WE NEARLY DONE, WITH THE VALUES WE WANT ON THE BORDER?
C  IF(WE NEED  MAX OF THOSE .LT. AM -OR- MIN OF THOSE .GE. AM) GO TO 90
C
      IF (SQ.EQ.K2-1) GO TO 180
C
C  THE SET S IS SPLIT, WHICH PIECE DO WE KEEP?
C  70  =  CUT OFF BOTTOM,   90  =  NEARLY DONE,   60  =  CUT OFF TOP
C
      IF (SQ-K1) 140, 180, 120
C
C  NEW S = (OLD S) .INTERSECT. (THOSE .LT. AM)
  120 CONTINUE
      DO 130 I=1,N
C                            RESET RIGHT BOUNDS FOR EACH ROW
        RB(I) = I + Q(I) - 1
  130 CONTINUE
      GO TO 160
C  NEW S = (OLD S) .INTERSECT. (THOSE .GE. AM)
  140 CONTINUE
      DO 150 I=1,N
C                            RESET LEFT BOUNDS FOR EACH ROW
        LB(I) = I + Q(I)
  150 CONTINUE
C
C  COUNT   SM = NUMBER OF PAIRS STILL IN NEW SET S
C           L = NUMBER OF PAIRS LESS THAN THOSE IN NEW SET S
  160 L = 0
      SM = 0
      DO 170 I=1,N
        L = L + LB(I) - I
        SM = SM + RB(I) - LB(I) + 1
  170 CONTINUE
C
C        *****   NORMAL RESTART JUMP   *****
C
      IF (SM.GT.2) GO TO 50
C  CAN ONLY GET TO 2 LEFT IF K1.NE.K2  -- GO GET THEIR AVERAGE
      GO TO 30
C
C  FIND:   MAX OF THOSE .LT. AM
C          MIN OF THOSE .GE. AM
  180 CONTINUE
      AMN = X(N) + X(N)
      AMX = X(1) + X(1)
      DO 190 I=1,N
        IQ = Q(I)
        IPIQ = I + IQ
        IF (IQ.GT.0) AMX = dmax1(AMX,X(I)+X(IPIQ-1))
        IPIQ = I + IQ
        IF (IQ.LT.N-I+1) AMN = dmin1(AMN,X(I)+X(IPIQ))
  190 CONTINUE
      result = (AMN+AMX)/4D0
C  WE ARE DONE, BUT WHICH SITUATION ARE WE IN?
      IF (K1.LT.K2) RETURN
      IF (SQ.EQ.K1) result = AMX/2D0
      IF (SQ.EQ.K1-1) result = AMN/2D0
      RETURN
      END

      * Licensed Materials - Property of IBM
      *
      * SAMPLE
      *
      * (c) Copyright IBM Corp. 2017 All Rights Reserved
      *
      * US Government Users Restricted Rights - Use, duplication or
      * disclosure restricted by GSA ADP Schedule Contract with IBM Corp
      *
      ******************************************************************
      * ASYNCPG1 - demonstrate the use of EXEC CICS FETCH commands
      *            with and without TIMEOUT, NOSUSPEND options.
      *
      *            Part of the application consists of
      *              ASYNCPG1, ASYNCCH1, ASYNCCH2, ASYNCCH3, ASYNCCH4
      *            ASYNCCH1 has a delay of 2 seconds.
      *            ASYNCCH2 has a delay of 5 seconds.
      *            ASYNCCH3 has a delay of 6 seconds.
      *            ASYNCCH4 has a delay of 1 second.
      *
      * ASYNCPG1 acts as a parent program which starts 4
      * asynchronous child tasks.
      * It then does 3 differnt flavours of fetch to get the response,
      * according to the business requirement as following:
      * - definitely need the response from the first child so
      *    use FETCH CHILD with no SUSPEND or TIEMOUT option.
      * - can wait for the response from the second child for a certain
      *   period so use FETCH CHILD TIMEOUT
      * - try to get response from either third or fouth child
      *   but cannot wait so use FETCH ANY NOSUSPEND
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASYNCPG1.
      ***** Main Procedure *****
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       01 W-RESP         PIC S9(8) USAGE BINARY.
       01 W-RESP2        PIC S9(8) USAGE BINARY.
       01 CHLDTOKN1      PIC X(16) VALUE SPACE.
       01 CHLDTOKN2      PIC X(16) VALUE SPACE.
       01 CHLDTOKN3      PIC X(16) VALUE SPACE.
       01 CHLDTOKN4      PIC X(16) VALUE SPACE.
       01 ANYTOKN        PIC X(16) VALUE SPACE.

       01 CVDA           PIC S9(8) COMP-5 SYNC.

       01 INCONTAINER    PIC X(16) value 'INCONT'.
       01 OUTCONTAINER   PIC X(16) VALUE 'OUTCONT'.
       01 CHLDCHNL1      PIC X(16).
       01 CHLDCHNL2      PIC X(16).
       01 ANYCHNL        PIC X(16).

       01 TIMEOUT1       PIC S9(8) USAGE BINARY VALUE 1000.

       1 PRINT-LINE.
         2 PARENT-PROGRAM   PIC X(8)  VALUE 'ASYNCPG1'.
         2 FILLER           PIC X(1)  VALUE ' '.
         2 CHILD-1          PIC X(4)  VALUE 'SUB1'.
         2 FILLER           PIC X(1)  VALUE '('.
         2 CHILD1-STATUS    PIC X(2)  VALUE SPACE.
         2 FILLER           PIC X(2)  VALUE ') '.
         2 CHILD-2          PIC X(4)  VALUE 'SUB2'.
         2 FILLER           PIC X(1)  VALUE '('.
         2 CHILD2-STATUS    PIC X(2)  VALUE SPACE.
         2 FILLER           PIC X(2)  VALUE ') '.
         2 CHILD-3          PIC X(4)  VALUE 'SUB3'.
         2 FILLER           PIC X(1)  VALUE '('.
         2 CHILD3-STATUS    PIC X(2)  VALUE ' '.
         2 FILLER           PIC X(2)  VALUE ') '.
         2 CHILD-4          PIC X(4)  VALUE 'SUB4'.
         2 FILLER           PIC X(1)  VALUE '('.
         2 CHILD4-STATUS    PIC X(2)  VALUE ' '.
         2 FILLER           PIC X(2)  VALUE ') '.

      *
       LINKAGE SECTION.
      *
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

      * run child 1
           EXEC CICS PUT CONTAINER(INCONTAINER)
           FROM('input to child 1')
           CHANNEL('PG1CHNL')
           END-EXEC.

           EXEC CICS RUN TRANSID('SUB1')
           CHANNEL('PG1CHNL') CHILD(CHLDTOKN1)
           END-EXEC.

      * run child 2
           EXEC CICS PUT CONTAINER(INCONTAINER)
           FROM('input to child 2')
           CHANNEL('PG1CHNL')
           END-EXEC.

           EXEC CICS RUN TRANSID('SUB2')
           CHANNEL('PG1CHNL') CHILD(CHLDTOKN2)
           END-EXEC.

      * run child 3
           EXEC CICS PUT CONTAINER(INCONTAINER)
           FROM('input to child 3')
           CHANNEL('PG1CHNL')
           END-EXEC.

           EXEC CICS RUN TRANSID('SUB3')
           CHANNEL('PG1CHNL') CHILD(CHLDTOKN3)
           END-EXEC.

      * run child 4
           EXEC CICS PUT CONTAINER(INCONTAINER)
           FROM('input to child 4')
           CHANNEL('PG1CHNL')
           END-EXEC.

           EXEC CICS RUN TRANSID('SUB4')
           CHANNEL('PG1CHNL') CHILD(CHLDTOKN4)
           END-EXEC.

      * fetch with suspend to get the result from the first child
           EXEC CICS FETCH CHILD(CHLDTOKN1) CHANNEL(CHLDCHNL1)
           COMPSTATUS(CVDA)
           RESP(W-RESP) RESP2(W-RESP2)
           END-EXEC.

           IF W-RESP = DFHRESP(NORMAL) THEN
              MOVE 'OK' TO CHILD1-STATUS
           END-IF

      * fetch the second child with timeout
           EXEC CICS FETCH CHILD(CHLDTOKN2) CHANNEL(CHLDCHNL2)
           TIMEOUT(TIMEOUT1)
           COMPSTATUS(CVDA)
           RESP(W-RESP) RESP2(W-RESP2)
           END-EXEC.

           IF W-RESP = DFHRESP(NORMAL) THEN
              MOVE 'OK' TO CHILD2-STATUS
           END-IF

           IF W-RESP = DFHRESP(NOTFINISHED) THEN
              IF W-RESP2 = 53 THEN
                MOVE 'NO' TO CHILD2-STATUS
              END-IF
           END-IF

      * fetch any of the third or fourth child without suspend
           EXEC CICS FETCH ANY(ANYTOKN) CHANNEL(ANYCHNL)
           NOSUSPEND COMPSTATUS(CVDA)
           RESP(W-RESP) RESP2(W-RESP2)
           END-EXEC.

           IF W-RESP = DFHRESP(NORMAL) THEN
              IF ANYTOKN = CHLDTOKN3 THEN
                MOVE 'OK' TO CHILD3-STATUS
              ELSE
                MOVE 'OK' TO CHILD4-STATUS
              END-IF
           END-IF

           IF W-RESP = DFHRESP(NOTFINISHED) THEN
              IF W-RESP2 = 52 THEN
                MOVE 'NO' TO CHILD3-STATUS
                MOVE 'NO' TO CHILD4-STATUS
              END-IF
           END-IF

           EXEC CICS SEND TEXT FROM(PRINT-LINE) FREEKB END-EXEC.

           EXEC CICS RETURN END-EXEC.

       END PROGRAM 'ASYNCPG1'.
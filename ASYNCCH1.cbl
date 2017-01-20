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
      * ASYNCCH1 - Implement the first child for asynch parent.
      *            Part of the application consists of
      *              ASYNCPG1, ASYNCCH1, ASYNCCH2, ASYNCCH3, ASYNCCH4
      *
      ******************************************************************
       IDENTIFICATION DIVISION.

       Program-id. ASYNCCH1.

       Environment division.

       Data division.

       Working-storage section.
       01 INCONTAINER    PIC X(16) value 'INCONT'.
       01 OUTCONTAINER   PIC X(16) VALUE 'OUTCONT'.

       01 MSG            PIC X(60) value spaces.

       Procedure division.

           EXEC CICS GET CONTAINER (INCONTAINER)
                     INTO    ( MSG )
                     NOHANDLE
           END-EXEC.

           EXEC CICS DELAY FOR SECONDS(2)
           END-EXEC

           EXEC CICS PUT CONTAINER ( OUTCONTAINER )
                     FROM   ( 'child 1 finished' )
                     NOHANDLE
           END-EXEC

           EXEC CICS RETURN END-EXEC.
           Goback.

       End program ASYNCCH1.
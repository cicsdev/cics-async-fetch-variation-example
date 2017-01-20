      * This program is the child program for asynch parent
       IDENTIFICATION DIVISION.
      *ID DIVISION.
       Program-id. ASYNCCH3.
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

           EXEC CICS DELAY FOR SECONDS(6)
           END-EXEC

           EXEC CICS PUT CONTAINER ( OUTCONTAINER )
                     FROM   ( 'child 3 finished' )
                     NOHANDLE
           END-EXEC

           EXEC CICS RETURN END-EXEC.
           Goback.

       End program ASYNCCH3.
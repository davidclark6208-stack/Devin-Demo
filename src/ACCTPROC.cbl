       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTPROC.
       AUTHOR. DEVIN.
       DATE-WRITTEN. 2026-01-31.
      *****************************************************************
      * PROGRAM: ACCTPROC                                             *
      * PURPOSE: PROCESS ACCOUNT FILE TO DETERMINE DEFINITION,        *
      *          RISK SEGMENT, AND ACTION CODE                        *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE  ASSIGN TO 'ACCTINP'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-INPUT-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO 'ACCTOUT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUTPUT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  INP-SCORE           PIC 9(3).
           05  INP-BALANCE         PIC 9(16).
           05  INP-STATUS-CODE     PIC X(2).
           05  INP-ACCOUNT-NUMBER  PIC X(20).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05  OUT-ACCOUNT-NUMBER  PIC X(20).
           05  OUT-SCORE           PIC 9(3).
           05  OUT-BALANCE         PIC 9(16).
           05  OUT-STATUS-CODE     PIC X(2).
           05  OUT-DEFINITION      PIC X(10).
           05  OUT-RISK-SEGMENT    PIC X(6).
           05  OUT-ACTION-CODE     PIC X(4).

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS.
           05  WS-INPUT-STATUS     PIC X(2).
           05  WS-OUTPUT-STATUS    PIC X(2).

       01  WS-FLAGS.
           05  WS-EOF-FLAG         PIC X(1) VALUE 'N'.
               88  END-OF-FILE               VALUE 'Y'.
               88  NOT-END-OF-FILE           VALUE 'N'.

       01  WS-COUNTERS.
           05  WS-RECORDS-READ     PIC 9(9) VALUE 0.
           05  WS-RECORDS-WRITTEN  PIC 9(9) VALUE 0.

       01  WS-WORK-FIELDS.
           05  WS-DEFINITION       PIC X(10).
           05  WS-RISK-SEGMENT     PIC X(6).
           05  WS-ACTION-CODE      PIC X(4).
           05  WS-ACCOUNT-ACTIVE   PIC X(1).
               88  ACCOUNT-IS-ACTIVE         VALUE 'Y'.
               88  ACCOUNT-NOT-ACTIVE        VALUE 'N'.

       01  WS-STATUS-CODES.
           05  WS-STATUS-OPEN      PIC X(2) VALUE '32'.
           05  WS-STATUS-BANKRUPT  PIC X(2) VALUE '41'.
           05  WS-STATUS-FRAUD     PIC X(2) VALUE '42'.

       01  WS-RISK-THRESHOLDS.
           05  WS-HIGH-RISK-MIN    PIC 9(3) VALUE 200.
           05  WS-MED-RISK-MIN     PIC 9(3) VALUE 100.

       01  WS-ACTION-CODES.
           05  WS-ACTION-DIAL      PIC X(4) VALUE 'DIAL'.
           05  WS-ACTION-NODIAL    PIC X(4) VALUE 'NODL'.

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-RECORDS
               UNTIL END-OF-FILE
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT  INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           IF WS-INPUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING INPUT FILE: ' WS-INPUT-STATUS
               STOP RUN
           END-IF
           IF WS-OUTPUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING OUTPUT FILE: ' WS-OUTPUT-STATUS
               STOP RUN
           END-IF
           PERFORM 1100-READ-INPUT-FILE.

       1100-READ-INPUT-FILE.
           READ INPUT-FILE
               AT END
                   SET END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-RECORDS-READ
           END-READ.

       2000-PROCESS-RECORDS.
           PERFORM 3000-DETERMINE-DEFINITION
           PERFORM 4000-DETERMINE-RISK-SEGMENT
           PERFORM 5000-DETERMINE-ACTION
           PERFORM 6000-WRITE-OUTPUT-RECORD
           PERFORM 1100-READ-INPUT-FILE.

       3000-DETERMINE-DEFINITION.
      *****************************************************************
      * DETERMINE ACCOUNT DEFINITION BASED ON STATUS CODE             *
      * 32 = OPEN (ACTIVE)                                            *
      * 41 = BANKRUPTCY                                               *
      * 42 = FRAUD                                                    *
      *****************************************************************
           SET ACCOUNT-NOT-ACTIVE TO TRUE
           EVALUATE INP-STATUS-CODE
               WHEN WS-STATUS-OPEN
                   MOVE 'OPEN'       TO WS-DEFINITION
                   SET ACCOUNT-IS-ACTIVE TO TRUE
               WHEN WS-STATUS-BANKRUPT
                   MOVE 'BANKRUPTCY' TO WS-DEFINITION
               WHEN WS-STATUS-FRAUD
                   MOVE 'FRAUD'      TO WS-DEFINITION
               WHEN OTHER
                   MOVE 'UNKNOWN'    TO WS-DEFINITION
           END-EVALUATE.

       4000-DETERMINE-RISK-SEGMENT.
      *****************************************************************
      * DETERMINE RISK SEGMENT BASED ON SCORE                         *
      * SCORE > 200 = HIGH RISK                                       *
      * SCORE > 100 = MEDIUM RISK                                     *
      * EVERYTHING ELSE = LOW RISK                                    *
      *****************************************************************
           EVALUATE TRUE
               WHEN INP-SCORE > WS-HIGH-RISK-MIN
                   MOVE 'HIGH'   TO WS-RISK-SEGMENT
               WHEN INP-SCORE > WS-MED-RISK-MIN
                   MOVE 'MEDIUM' TO WS-RISK-SEGMENT
               WHEN OTHER
                   MOVE 'LOW'    TO WS-RISK-SEGMENT
           END-EVALUATE.

       5000-DETERMINE-ACTION.
      *****************************************************************
      * DETERMINE ACTION CODE BASED ON DEFINITION AND RISK SEGMENT    *
      * IF ACCOUNT IS ACTIVE (OPEN) AND HIGH OR MEDIUM RISK = DIAL    *
      * EVERYTHING ELSE = NO DIAL (NODL)                              *
      *****************************************************************
           IF ACCOUNT-IS-ACTIVE
               AND (WS-RISK-SEGMENT = 'HIGH' 
                    OR WS-RISK-SEGMENT = 'MEDIUM')
               MOVE WS-ACTION-DIAL TO WS-ACTION-CODE
           ELSE
               MOVE WS-ACTION-NODIAL TO WS-ACTION-CODE
           END-IF.

       6000-WRITE-OUTPUT-RECORD.
           MOVE INP-ACCOUNT-NUMBER TO OUT-ACCOUNT-NUMBER
           MOVE INP-SCORE          TO OUT-SCORE
           MOVE INP-BALANCE        TO OUT-BALANCE
           MOVE INP-STATUS-CODE    TO OUT-STATUS-CODE
           MOVE WS-DEFINITION      TO OUT-DEFINITION
           MOVE WS-RISK-SEGMENT    TO OUT-RISK-SEGMENT
           MOVE WS-ACTION-CODE     TO OUT-ACTION-CODE
           WRITE OUTPUT-RECORD
           IF WS-OUTPUT-STATUS = '00'
               ADD 1 TO WS-RECORDS-WRITTEN
           ELSE
               DISPLAY 'ERROR WRITING OUTPUT: ' WS-OUTPUT-STATUS
           END-IF.

       9000-TERMINATE.
           DISPLAY 'RECORDS READ:    ' WS-RECORDS-READ
           DISPLAY 'RECORDS WRITTEN: ' WS-RECORDS-WRITTEN
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE.

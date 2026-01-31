       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRATENG.
       AUTHOR. DEVIN.
       DATE-WRITTEN. 2026-01-31.
      *****************************************************************
      * PROGRAM: STRATENG - STRATEGY DECISION ENGINE                  *
      * PURPOSE: PROCESS ACCOUNT AND SCORE FILES TO DETERMINE         *
      *          ACCOUNT DEFINITIONS, RISK SEGMENTS, AND DIAL         *
      *          ACTIVITY STRATEGIES                                  *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'ACCTINP'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT SCORE-FILE ASSIGN TO 'SCOREINP'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SCORE-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO 'STRATOUT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.

      *****************************************************************
      * ACCOUNT INPUT FILE RECORD LAYOUT                              *
      *****************************************************************
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCT-ACCOUNT-NUMBER    PIC X(20).
           05  ACCT-STATUS            PIC X(10).
           05  ACCT-BALANCE           PIC 9(13)V99.
           05  ACCT-PHONE-NUMBER      PIC X(15).
           05  ACCT-PHONE-CONSENT     PIC X(1).

      *****************************************************************
      * SCORE INPUT FILE RECORD LAYOUT (ORDERED BY ACCOUNT NUMBER)    *
      *****************************************************************
       FD  SCORE-FILE.
       01  SCORE-RECORD.
           05  SCORE-ACCOUNT-NUMBER   PIC X(20).
           05  SCORE-VALUE            PIC 9(3).

      *****************************************************************
      * OUTPUT FILE RECORD LAYOUT                                     *
      *****************************************************************
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05  OUT-ACCOUNT-NUMBER     PIC X(20).
           05  OUT-STATUS             PIC X(10).
           05  OUT-BALANCE            PIC 9(13)V99.
           05  OUT-PHONE-NUMBER       PIC X(15).
           05  OUT-PHONE-CONSENT      PIC X(1).
           05  OUT-SCORE              PIC 9(3).
           05  OUT-DEF-WORKABLE       PIC X(1).
           05  OUT-DEF-DIALABLE       PIC X(1).
           05  OUT-RISK-SEGMENT       PIC X(6).
           05  OUT-SMALL-BALANCE      PIC X(1).
           05  OUT-ACTIVITY           PIC X(10).
           05  OUT-DIAL-FREQUENCY     PIC 9(1).

       WORKING-STORAGE SECTION.

      *****************************************************************
      * FILE STATUS FIELDS                                            *
      *****************************************************************
       01  WS-FILE-STATUS.
           05  WS-ACCT-STATUS         PIC X(2).
           05  WS-SCORE-STATUS        PIC X(2).
           05  WS-OUT-STATUS          PIC X(2).

      *****************************************************************
      * END OF FILE FLAGS                                             *
      *****************************************************************
       01  WS-EOF-FLAGS.
           05  WS-ACCT-EOF            PIC X(1) VALUE 'N'.
               88  ACCT-END-OF-FILE             VALUE 'Y'.
               88  ACCT-NOT-EOF                 VALUE 'N'.
           05  WS-SCORE-EOF           PIC X(1) VALUE 'N'.
               88  SCORE-END-OF-FILE            VALUE 'Y'.
               88  SCORE-NOT-EOF                VALUE 'N'.

      *****************************************************************
      * RECORD COUNTERS                                               *
      *****************************************************************
       01  WS-COUNTERS.
           05  WS-ACCT-READ           PIC 9(9) VALUE 0.
           05  WS-SCORE-READ          PIC 9(9) VALUE 0.
           05  WS-RECORDS-WRITTEN     PIC 9(9) VALUE 0.
           05  WS-RECORDS-MATCHED     PIC 9(9) VALUE 0.
           05  WS-RECORDS-UNMATCHED   PIC 9(9) VALUE 0.

      *****************************************************************
      * WORKING FIELDS FOR DEFINITIONS AND STRATEGY                   *
      *****************************************************************
       01  WS-DEFINITIONS.
           05  WS-WORKABLE            PIC X(1).
               88  IS-WORKABLE                  VALUE 'Y'.
               88  NOT-WORKABLE                 VALUE 'N'.
           05  WS-DIALABLE            PIC X(1).
               88  IS-DIALABLE                  VALUE 'Y'.
               88  NOT-DIALABLE                 VALUE 'N'.

       01  WS-RISK-FIELDS.
           05  WS-RISK-SEGMENT        PIC X(6).
           05  WS-SMALL-BALANCE       PIC X(1).
               88  IS-SMALL-BALANCE             VALUE 'Y'.
               88  NOT-SMALL-BALANCE            VALUE 'N'.

       01  WS-STRATEGY-FIELDS.
           05  WS-ACTIVITY            PIC X(10).
           05  WS-DIAL-FREQUENCY      PIC 9(1).

      *****************************************************************
      * THRESHOLD CONSTANTS                                           *
      *****************************************************************
       01  WS-RISK-THRESHOLDS.
           05  WS-HIGH-RISK-MIN       PIC 9(3) VALUE 300.
           05  WS-MED-RISK-MIN        PIC 9(3) VALUE 200.
           05  WS-LOW-RISK-MIN        PIC 9(3) VALUE 100.

       01  WS-BALANCE-THRESHOLD.
           05  WS-SMALL-BAL-MAX       PIC 9(13)V99 VALUE 250.00.

      *****************************************************************
      * DIAL FREQUENCY CONSTANTS                                      *
      *****************************************************************
       01  WS-DIAL-FREQ-VALUES.
           05  WS-DIAL-HIGH           PIC 9(1) VALUE 4.
           05  WS-DIAL-MEDIUM         PIC 9(1) VALUE 3.
           05  WS-DIAL-LOW            PIC 9(1) VALUE 2.
           05  WS-DIAL-NONE           PIC 9(1) VALUE 0.

      *****************************************************************
      * ACCOUNT STATUS CONSTANTS                                      *
      *****************************************************************
       01  WS-STATUS-VALUES.
           05  WS-STATUS-OPEN         PIC X(10) VALUE 'OPEN'.
           05  WS-STATUS-CLOSE        PIC X(10) VALUE 'CLOSE'.
           05  WS-STATUS-BANKRUPT     PIC X(10) VALUE 'BANKRUPT'.
           05  WS-STATUS-DEFAULT      PIC X(10) VALUE 'DEFAULT'.
           05  WS-STATUS-FRAUD        PIC X(10) VALUE 'FRAUD'.

      *****************************************************************
      * MATCHED SCORE FIELD                                           *
      *****************************************************************
       01  WS-MATCHED-SCORE           PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.

      *****************************************************************
      * MAIN PROCESSING LOGIC                                         *
      *****************************************************************
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-ACCOUNTS
               UNTIL ACCT-END-OF-FILE
           PERFORM 9000-TERMINATE
           STOP RUN.

      *****************************************************************
      * INITIALIZATION - OPEN FILES AND READ FIRST RECORDS            *
      *****************************************************************
       1000-INITIALIZE.
           OPEN INPUT  ACCOUNT-FILE
           OPEN INPUT  SCORE-FILE
           OPEN OUTPUT OUTPUT-FILE

           IF WS-ACCT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING ACCOUNT FILE: ' WS-ACCT-STATUS
               STOP RUN
           END-IF

           IF WS-SCORE-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING SCORE FILE: ' WS-SCORE-STATUS
               STOP RUN
           END-IF

           IF WS-OUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING OUTPUT FILE: ' WS-OUT-STATUS
               STOP RUN
           END-IF

           PERFORM 1100-READ-ACCOUNT-FILE
           PERFORM 1200-READ-SCORE-FILE.

      *****************************************************************
      * READ ACCOUNT FILE                                             *
      *****************************************************************
       1100-READ-ACCOUNT-FILE.
           READ ACCOUNT-FILE
               AT END
                   SET ACCT-END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-ACCT-READ
           END-READ.

      *****************************************************************
      * READ SCORE FILE                                               *
      *****************************************************************
       1200-READ-SCORE-FILE.
           READ SCORE-FILE
               AT END
                   SET SCORE-END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-SCORE-READ
           END-READ.

      *****************************************************************
      * MAIN ACCOUNT PROCESSING LOOP                                  *
      *****************************************************************
       2000-PROCESS-ACCOUNTS.
           PERFORM 2100-MATCH-SCORE
           PERFORM 3000-DETERMINE-DEFINITIONS
           PERFORM 4000-DETERMINE-RISK-SEGMENT
           PERFORM 5000-DETERMINE-STRATEGY
           PERFORM 6000-WRITE-OUTPUT
           PERFORM 1100-READ-ACCOUNT-FILE.

      *****************************************************************
      * MATCH ACCOUNT WITH SCORE FILE                                 *
      * BOTH FILES ARE ORDERED BY ACCOUNT NUMBER                      *
      *****************************************************************
       2100-MATCH-SCORE.
           MOVE 0 TO WS-MATCHED-SCORE

           PERFORM UNTIL SCORE-END-OF-FILE
               OR SCORE-ACCOUNT-NUMBER >= ACCT-ACCOUNT-NUMBER

               PERFORM 1200-READ-SCORE-FILE
           END-PERFORM

           IF NOT SCORE-END-OF-FILE
               IF SCORE-ACCOUNT-NUMBER = ACCT-ACCOUNT-NUMBER
                   MOVE SCORE-VALUE TO WS-MATCHED-SCORE
                   ADD 1 TO WS-RECORDS-MATCHED
                   PERFORM 1200-READ-SCORE-FILE
               ELSE
                   ADD 1 TO WS-RECORDS-UNMATCHED
               END-IF
           ELSE
               ADD 1 TO WS-RECORDS-UNMATCHED
           END-IF.

      *****************************************************************
      * STEP 1: DETERMINE ACCOUNT DEFINITIONS                         *
      * WORKABLE: ACCOUNT STATUS IS OPEN                              *
      * DIALABLE: HAS VALID PHONE AND CONSENT TO DIAL                 *
      *****************************************************************
       3000-DETERMINE-DEFINITIONS.
           PERFORM 3100-DETERMINE-WORKABLE
           PERFORM 3200-DETERMINE-DIALABLE.

       3100-DETERMINE-WORKABLE.
           IF ACCT-STATUS = WS-STATUS-OPEN
               SET IS-WORKABLE TO TRUE
           ELSE
               SET NOT-WORKABLE TO TRUE
           END-IF.

       3200-DETERMINE-DIALABLE.
           IF ACCT-PHONE-NUMBER NOT = SPACES
               AND ACCT-PHONE-NUMBER NOT = LOW-VALUES
               AND ACCT-PHONE-CONSENT = 'Y'
               SET IS-DIALABLE TO TRUE
           ELSE
               SET NOT-DIALABLE TO TRUE
           END-IF.

      *****************************************************************
      * STEP 2: DETERMINE RISK SEGMENTATION                           *
      * SCORE > 300 = HIGH RISK                                       *
      * SCORE > 200 = MEDIUM RISK                                     *
      * SCORE > 100 = LOW RISK                                        *
      * ALSO CHECK FOR SMALL BALANCE (< 250)                          *
      *****************************************************************
       4000-DETERMINE-RISK-SEGMENT.
           PERFORM 4100-CALCULATE-RISK
           PERFORM 4200-CHECK-SMALL-BALANCE.

       4100-CALCULATE-RISK.
           EVALUATE TRUE
               WHEN WS-MATCHED-SCORE > WS-HIGH-RISK-MIN
                   MOVE 'HIGH'   TO WS-RISK-SEGMENT
               WHEN WS-MATCHED-SCORE > WS-MED-RISK-MIN
                   MOVE 'MEDIUM' TO WS-RISK-SEGMENT
               WHEN WS-MATCHED-SCORE > WS-LOW-RISK-MIN
                   MOVE 'LOW'    TO WS-RISK-SEGMENT
               WHEN OTHER
                   MOVE 'NONE'   TO WS-RISK-SEGMENT
           END-EVALUATE.

       4200-CHECK-SMALL-BALANCE.
           IF ACCT-BALANCE < WS-SMALL-BAL-MAX
               SET IS-SMALL-BALANCE TO TRUE
           ELSE
               SET NOT-SMALL-BALANCE TO TRUE
           END-IF.

      *****************************************************************
      * STEP 3: DETERMINE STRATEGIC ACTIVITY                          *
      * IF WORKABLE AND DIALABLE, SET ACTIVITY TO DIALABLE            *
      * DIAL FREQUENCY BASED ON RISK:                                 *
      *   HIGH = 4 TIMES/DAY                                          *
      *   MEDIUM = 3 TIMES/DAY                                        *
      *   LOW = 2 TIMES/DAY                                           *
      * SMALL BALANCE ACCOUNTS ARE NOT DIALED                         *
      *****************************************************************
       5000-DETERMINE-STRATEGY.
           MOVE SPACES TO WS-ACTIVITY
           MOVE WS-DIAL-NONE TO WS-DIAL-FREQUENCY

           IF IS-WORKABLE AND IS-DIALABLE
               IF IS-SMALL-BALANCE
                   MOVE 'NO DIAL'   TO WS-ACTIVITY
                   MOVE WS-DIAL-NONE TO WS-DIAL-FREQUENCY
               ELSE
                   MOVE 'DIALABLE'  TO WS-ACTIVITY
                   PERFORM 5100-SET-DIAL-FREQUENCY
               END-IF
           ELSE
               MOVE 'NO DIAL'   TO WS-ACTIVITY
               MOVE WS-DIAL-NONE TO WS-DIAL-FREQUENCY
           END-IF.

       5100-SET-DIAL-FREQUENCY.
           EVALUATE WS-RISK-SEGMENT
               WHEN 'HIGH'
                   MOVE WS-DIAL-HIGH TO WS-DIAL-FREQUENCY
               WHEN 'MEDIUM'
                   MOVE WS-DIAL-MEDIUM TO WS-DIAL-FREQUENCY
               WHEN 'LOW'
                   MOVE WS-DIAL-LOW TO WS-DIAL-FREQUENCY
               WHEN OTHER
                   MOVE WS-DIAL-NONE TO WS-DIAL-FREQUENCY
           END-EVALUATE.

      *****************************************************************
      * WRITE OUTPUT RECORD                                           *
      *****************************************************************
       6000-WRITE-OUTPUT.
           MOVE ACCT-ACCOUNT-NUMBER   TO OUT-ACCOUNT-NUMBER
           MOVE ACCT-STATUS           TO OUT-STATUS
           MOVE ACCT-BALANCE          TO OUT-BALANCE
           MOVE ACCT-PHONE-NUMBER     TO OUT-PHONE-NUMBER
           MOVE ACCT-PHONE-CONSENT    TO OUT-PHONE-CONSENT
           MOVE WS-MATCHED-SCORE      TO OUT-SCORE
           MOVE WS-WORKABLE           TO OUT-DEF-WORKABLE
           MOVE WS-DIALABLE           TO OUT-DEF-DIALABLE
           MOVE WS-RISK-SEGMENT       TO OUT-RISK-SEGMENT
           MOVE WS-SMALL-BALANCE      TO OUT-SMALL-BALANCE
           MOVE WS-ACTIVITY           TO OUT-ACTIVITY
           MOVE WS-DIAL-FREQUENCY     TO OUT-DIAL-FREQUENCY

           WRITE OUTPUT-RECORD

           IF WS-OUT-STATUS = '00'
               ADD 1 TO WS-RECORDS-WRITTEN
           ELSE
               DISPLAY 'ERROR WRITING OUTPUT: ' WS-OUT-STATUS
           END-IF.

      *****************************************************************
      * TERMINATION - DISPLAY STATS AND CLOSE FILES                   *
      *****************************************************************
       9000-TERMINATE.
           DISPLAY '**********************************************'
           DISPLAY 'STRATEGY ENGINE PROCESSING COMPLETE'
           DISPLAY '**********************************************'
           DISPLAY 'ACCOUNT RECORDS READ:    ' WS-ACCT-READ
           DISPLAY 'SCORE RECORDS READ:      ' WS-SCORE-READ
           DISPLAY 'RECORDS MATCHED:         ' WS-RECORDS-MATCHED
           DISPLAY 'RECORDS UNMATCHED:       ' WS-RECORDS-UNMATCHED
           DISPLAY 'OUTPUT RECORDS WRITTEN:  ' WS-RECORDS-WRITTEN
           DISPLAY '**********************************************'

           CLOSE ACCOUNT-FILE
           CLOSE SCORE-FILE
           CLOSE OUTPUT-FILE.

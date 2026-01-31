       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRATPY.
       AUTHOR. DEVIN.
       DATE-WRITTEN. 2026-01-31.
      *****************************************************************
      * PROGRAM: STRATPY - STRATEGY DECISION ENGINE WITH PYTHON RULES *
      * PURPOSE: PROCESS ACCOUNT AND SCORE FILES USING PYTHON FOR     *
      *          BUSINESS RULE EVALUATION                             *
      *                                                               *
      * ARCHITECTURE:                                                 *
      *   - COBOL handles file I/O and data processing               *
      *   - Python (strategy_rules.py) contains all business rules   *
      *   - COBOL calls Python via SYSTEM call for each record       *
      *   - Python returns rule results via output file              *
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
           SELECT PYTHON-INPUT ASSIGN TO 'PYINPUT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PYIN-STATUS.
           SELECT PYTHON-OUTPUT ASSIGN TO 'PYOUTPUT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PYOUT-STATUS.

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

      *****************************************************************
      * PYTHON INPUT FILE - DATA SENT TO PYTHON                       *
      *****************************************************************
       FD  PYTHON-INPUT.
       01  PYTHON-INPUT-RECORD        PIC X(200).

      *****************************************************************
      * PYTHON OUTPUT FILE - RESULTS FROM PYTHON                      *
      *****************************************************************
       FD  PYTHON-OUTPUT.
       01  PYTHON-OUTPUT-RECORD       PIC X(100).

       WORKING-STORAGE SECTION.

      *****************************************************************
      * FILE STATUS FIELDS                                            *
      *****************************************************************
       01  WS-FILE-STATUS.
           05  WS-ACCT-STATUS         PIC X(2).
           05  WS-SCORE-STATUS        PIC X(2).
           05  WS-OUT-STATUS          PIC X(2).
           05  WS-PYIN-STATUS         PIC X(2).
           05  WS-PYOUT-STATUS        PIC X(2).

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
           05  WS-PYTHON-CALLS        PIC 9(9) VALUE 0.
           05  WS-PYTHON-ERRORS       PIC 9(9) VALUE 0.

      *****************************************************************
      * PYTHON INTEGRATION FIELDS                                     *
      *****************************************************************
       01  WS-PYTHON-COMMAND          PIC X(500).
       01  WS-PYTHON-SCRIPT           PIC X(100) 
               VALUE 'python3 strategy_rules.py'.
       01  WS-SYSTEM-RC               PIC S9(4) COMP.

      *****************************************************************
      * PYTHON RESULT FIELDS                                          *
      *****************************************************************
       01  WS-PYTHON-RESULT.
           05  WS-PY-WORKABLE         PIC X(1).
           05  WS-PY-DELIM1           PIC X(1).
           05  WS-PY-DIALABLE         PIC X(1).
           05  WS-PY-DELIM2           PIC X(1).
           05  WS-PY-RISK-SEGMENT     PIC X(6).
           05  WS-PY-DELIM3           PIC X(1).
           05  WS-PY-SMALL-BALANCE    PIC X(1).
           05  WS-PY-DELIM4           PIC X(1).
           05  WS-PY-ACTIVITY         PIC X(10).
           05  WS-PY-DELIM5           PIC X(1).
           05  WS-PY-DIAL-FREQ        PIC X(1).

      *****************************************************************
      * WORKING FIELDS FOR DATA FORMATTING                            *
      *****************************************************************
       01  WS-WORK-FIELDS.
           05  WS-MATCHED-SCORE       PIC 9(3) VALUE 0.
           05  WS-BALANCE-DISPLAY     PIC Z(12)9.99.
           05  WS-SCORE-DISPLAY       PIC ZZ9.

      *****************************************************************
      * PARSED PYTHON OUTPUT FIELDS                                   *
      *****************************************************************
       01  WS-PARSED-OUTPUT.
           05  WS-WORKABLE            PIC X(1).
           05  WS-DIALABLE            PIC X(1).
           05  WS-RISK-SEGMENT        PIC X(6).
           05  WS-SMALL-BALANCE       PIC X(1).
           05  WS-ACTIVITY            PIC X(10).
           05  WS-DIAL-FREQUENCY      PIC 9(1).

      *****************************************************************
      * STRING PARSING FIELDS                                         *
      *****************************************************************
       01  WS-PARSE-FIELDS.
           05  WS-PARSE-PTR           PIC 9(3).
           05  WS-FIELD-NUM           PIC 9(1).
           05  WS-TEMP-FIELD          PIC X(20).
           05  WS-OUTPUT-LINE         PIC X(100).

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
           DISPLAY '**********************************************'
           DISPLAY 'STRATEGY ENGINE WITH PYTHON RULES - STARTING'
           DISPLAY '**********************************************'

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
           PERFORM 3000-CALL-PYTHON-RULES
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
      * CALL PYTHON SCRIPT TO EVALUATE BUSINESS RULES                 *
      * Python script: strategy_rules.py                              *
      * Arguments: account_number status balance phone consent score  *
      * Output: workable|dialable|risk|small_bal|activity|dial_freq   *
      *****************************************************************
       3000-CALL-PYTHON-RULES.
           ADD 1 TO WS-PYTHON-CALLS

           MOVE ACCT-BALANCE TO WS-BALANCE-DISPLAY
           MOVE WS-MATCHED-SCORE TO WS-SCORE-DISPLAY

           STRING WS-PYTHON-SCRIPT DELIMITED BY '  '
                  ' "' DELIMITED BY SIZE
                  ACCT-ACCOUNT-NUMBER DELIMITED BY '  '
                  '" "' DELIMITED BY SIZE
                  ACCT-STATUS DELIMITED BY '  '
                  '" ' DELIMITED BY SIZE
                  WS-BALANCE-DISPLAY DELIMITED BY '  '
                  ' "' DELIMITED BY SIZE
                  ACCT-PHONE-NUMBER DELIMITED BY '  '
                  '" "' DELIMITED BY SIZE
                  ACCT-PHONE-CONSENT DELIMITED BY '  '
                  '" ' DELIMITED BY SIZE
                  WS-SCORE-DISPLAY DELIMITED BY '  '
                  ' > PYOUTPUT' DELIMITED BY SIZE
               INTO WS-PYTHON-COMMAND
           END-STRING

           CALL 'SYSTEM' USING WS-PYTHON-COMMAND
               RETURNING WS-SYSTEM-RC
           END-CALL

           IF WS-SYSTEM-RC NOT = 0
               DISPLAY 'PYTHON ERROR FOR ACCOUNT: ' 
                       ACCT-ACCOUNT-NUMBER
               DISPLAY 'RETURN CODE: ' WS-SYSTEM-RC
               ADD 1 TO WS-PYTHON-ERRORS
               PERFORM 3100-SET-DEFAULT-VALUES
           ELSE
               PERFORM 3200-READ-PYTHON-OUTPUT
           END-IF.

      *****************************************************************
      * SET DEFAULT VALUES IF PYTHON CALL FAILS                       *
      *****************************************************************
       3100-SET-DEFAULT-VALUES.
           MOVE 'N'        TO WS-WORKABLE
           MOVE 'N'        TO WS-DIALABLE
           MOVE 'NONE'     TO WS-RISK-SEGMENT
           MOVE 'N'        TO WS-SMALL-BALANCE
           MOVE 'ERROR'    TO WS-ACTIVITY
           MOVE 0          TO WS-DIAL-FREQUENCY.

      *****************************************************************
      * READ AND PARSE PYTHON OUTPUT                                  *
      * Format: workable|dialable|risk_segment|small_bal|activity|freq*
      *****************************************************************
       3200-READ-PYTHON-OUTPUT.
           OPEN INPUT PYTHON-OUTPUT
           IF WS-PYOUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING PYTHON OUTPUT: ' WS-PYOUT-STATUS
               PERFORM 3100-SET-DEFAULT-VALUES
           ELSE
               READ PYTHON-OUTPUT INTO WS-OUTPUT-LINE
                   AT END
                       PERFORM 3100-SET-DEFAULT-VALUES
                   NOT AT END
                       PERFORM 3300-PARSE-PYTHON-OUTPUT
               END-READ
               CLOSE PYTHON-OUTPUT
           END-IF.

      *****************************************************************
      * PARSE PIPE-DELIMITED PYTHON OUTPUT                            *
      *****************************************************************
       3300-PARSE-PYTHON-OUTPUT.
           INITIALIZE WS-PARSED-OUTPUT
           MOVE 1 TO WS-PARSE-PTR
           MOVE 1 TO WS-FIELD-NUM

           UNSTRING WS-OUTPUT-LINE DELIMITED BY '|'
               INTO WS-WORKABLE
                    WS-DIALABLE
                    WS-RISK-SEGMENT
                    WS-SMALL-BALANCE
                    WS-ACTIVITY
                    WS-TEMP-FIELD
           END-UNSTRING

           IF WS-TEMP-FIELD NOT = SPACES
               MOVE FUNCTION NUMVAL(WS-TEMP-FIELD) 
                   TO WS-DIAL-FREQUENCY
           ELSE
               MOVE 0 TO WS-DIAL-FREQUENCY
           END-IF.

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
           DISPLAY 'PYTHON CALLS MADE:       ' WS-PYTHON-CALLS
           DISPLAY 'PYTHON ERRORS:           ' WS-PYTHON-ERRORS
           DISPLAY '**********************************************'

           CLOSE ACCOUNT-FILE
           CLOSE SCORE-FILE
           CLOSE OUTPUT-FILE.

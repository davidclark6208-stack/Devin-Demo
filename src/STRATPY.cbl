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
      *   - COBOL calls Python via BPXWUNIX (z/OS USS) for each rec  *
      *   - Python returns rule results via STDOUT buffer            *
      *                                                               *
      * Z/OS COMPLIANCE:                                              *
      *   - Uses BPXWUNIX callable service for USS command execution *
      *   - Supports IBM Open Enterprise SDK for Python on z/OS      *
      *   - Uses HFS paths for Python script and I/O files           *
      *   - Handles EBCDIC/ASCII conversion via environment vars     *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS VALID-RETURN-CODE IS 0 THRU 4.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO ACCTINP
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ACCT-STATUS.
           SELECT SCORE-FILE ASSIGN TO SCOREINP
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-SCORE-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO STRATOUT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.

      *****************************************************************
      * ACCOUNT INPUT FILE RECORD LAYOUT                              *
      *****************************************************************
       FD  ACCOUNT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACCOUNT-RECORD.
           05  ACCT-ACCOUNT-NUMBER    PIC X(20).
           05  ACCT-STATUS            PIC X(10).
           05  ACCT-BALANCE           PIC 9(13)V99.
           05  ACCT-PHONE-NUMBER      PIC X(15).
           05  ACCT-PHONE-CONSENT     PIC X(1).

      *****************************************************************
      * SCORE INPUT FILE RECORD LAYOUT (ORDERED BY ACCOUNT NUMBER)    *
      *****************************************************************
       FD  SCORE-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SCORE-RECORD.
           05  SCORE-ACCOUNT-NUMBER   PIC X(20).
           05  SCORE-VALUE            PIC 9(3).

      *****************************************************************
      * OUTPUT FILE RECORD LAYOUT                                     *
      *****************************************************************
       FD  OUTPUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
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
           05  WS-PYTHON-CALLS        PIC 9(9) VALUE 0.
           05  WS-PYTHON-ERRORS       PIC 9(9) VALUE 0.

      *****************************************************************
      * BPXWUNIX CALLABLE SERVICE PARAMETERS                          *
      * Reference: IBM z/OS UNIX System Services Programming          *
      *****************************************************************
       01  WS-BPXWUNIX-PARMS.
           05  WS-COMMAND-LENGTH      PIC S9(8) COMP.
           05  WS-COMMAND-TEXT        PIC X(1024).
           05  WS-STDIN-LENGTH        PIC S9(8) COMP VALUE 0.
           05  WS-STDIN-DATA          PIC X(1).
           05  WS-STDOUT-LENGTH       PIC S9(8) COMP VALUE 1024.
           05  WS-STDOUT-DATA         PIC X(1024).
           05  WS-STDERR-LENGTH       PIC S9(8) COMP VALUE 512.
           05  WS-STDERR-DATA         PIC X(512).
           05  WS-RETURN-VALUE        PIC S9(8) COMP.
           05  WS-RETURN-CODE         PIC S9(8) COMP.
           05  WS-REASON-CODE         PIC S9(8) COMP.

      *****************************************************************
      * ENVIRONMENT VARIABLES FOR PYTHON                              *
      *****************************************************************
       01  WS-ENV-COUNT               PIC S9(8) COMP VALUE 3.
       01  WS-ENV-LENGTH-ARRAY.
           05  WS-ENV-LEN             PIC S9(8) COMP OCCURS 5.
       01  WS-ENV-ARRAY.
           05  WS-ENV-VAR             PIC X(200) OCCURS 5.

      *****************************************************************
      * PYTHON SCRIPT PATH AND COMMAND                                *
      * Adjust paths based on your z/OS installation                  *
      *****************************************************************
       01  WS-PYTHON-PATH             PIC X(100)
               VALUE '/usr/lpp/IBM/cyp/v3r11/pyz/bin/python3'.
       01  WS-SCRIPT-PATH             PIC X(100)
               VALUE '/u/strategy/strategy_rules.py'.

      *****************************************************************
      * WORKING FIELDS FOR DATA FORMATTING                            *
      *****************************************************************
       01  WS-WORK-FIELDS.
           05  WS-MATCHED-SCORE       PIC 9(3) VALUE 0.
           05  WS-BALANCE-DISPLAY     PIC Z(12)9.99.
           05  WS-SCORE-DISPLAY       PIC ZZ9.
           05  WS-CMD-PTR             PIC S9(8) COMP.

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
      * INITIALIZATION - OPEN FILES, SETUP ENV, READ FIRST RECORDS    *
      *****************************************************************
       1000-INITIALIZE.
           DISPLAY '**********************************************'
           DISPLAY 'STRATEGY ENGINE WITH PYTHON RULES - Z/OS'
           DISPLAY 'USING BPXWUNIX FOR USS PYTHON EXECUTION'
           DISPLAY '**********************************************'

           PERFORM 1050-SETUP-ENVIRONMENT

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
      * SETUP ENVIRONMENT VARIABLES FOR PYTHON EXECUTION              *
      *****************************************************************
       1050-SETUP-ENVIRONMENT.
      *    Set PATH to include Python installation
           MOVE 'PATH=/usr/lpp/IBM/cyp/v3r11/pyz/bin:$PATH'
               TO WS-ENV-VAR(1)
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-ENV-VAR(1)))
               TO WS-ENV-LEN(1)

      *    Set PYTHONPATH for module location
           MOVE 'PYTHONPATH=/u/strategy'
               TO WS-ENV-VAR(2)
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-ENV-VAR(2)))
               TO WS-ENV-LEN(2)

      *    Set encoding for proper EBCDIC/ASCII handling
           MOVE '_BPXK_AUTOCVT=ON'
               TO WS-ENV-VAR(3)
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-ENV-VAR(3)))
               TO WS-ENV-LEN(3)

           MOVE 3 TO WS-ENV-COUNT.

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
      * CALL PYTHON SCRIPT VIA BPXWUNIX TO EVALUATE BUSINESS RULES    *
      * Uses IBM z/OS UNIX System Services callable service           *
      * Python script: strategy_rules.py                              *
      * Arguments: account_number status balance phone consent score  *
      * Output: workable|dialable|risk|small_bal|activity|dial_freq   *
      *****************************************************************
       3000-CALL-PYTHON-RULES.
           ADD 1 TO WS-PYTHON-CALLS

           PERFORM 3010-BUILD-PYTHON-COMMAND
           PERFORM 3020-EXECUTE-BPXWUNIX

           IF WS-RETURN-VALUE NOT = 0
               DISPLAY 'PYTHON ERROR FOR ACCOUNT: '
                       ACCT-ACCOUNT-NUMBER
               DISPLAY 'RETURN VALUE: ' WS-RETURN-VALUE
               DISPLAY 'RETURN CODE:  ' WS-RETURN-CODE
               DISPLAY 'REASON CODE:  ' WS-REASON-CODE
               ADD 1 TO WS-PYTHON-ERRORS
               PERFORM 3100-SET-DEFAULT-VALUES
           ELSE
               PERFORM 3200-PARSE-STDOUT-OUTPUT
           END-IF.

      *****************************************************************
      * BUILD PYTHON COMMAND STRING FOR BPXWUNIX                      *
      *****************************************************************
       3010-BUILD-PYTHON-COMMAND.
           MOVE ACCT-BALANCE TO WS-BALANCE-DISPLAY
           MOVE WS-MATCHED-SCORE TO WS-SCORE-DISPLAY

           INITIALIZE WS-COMMAND-TEXT
           MOVE 1 TO WS-CMD-PTR

           STRING WS-PYTHON-PATH DELIMITED BY SPACES
                  ' ' DELIMITED BY SIZE
                  WS-SCRIPT-PATH DELIMITED BY SPACES
                  ' "' DELIMITED BY SIZE
                  ACCT-ACCOUNT-NUMBER DELIMITED BY '  '
                  '" "' DELIMITED BY SIZE
                  ACCT-STATUS DELIMITED BY '  '
                  '" ' DELIMITED BY SIZE
                  WS-BALANCE-DISPLAY DELIMITED BY SPACES
                  ' "' DELIMITED BY SIZE
                  ACCT-PHONE-NUMBER DELIMITED BY '  '
                  '" "' DELIMITED BY SIZE
                  ACCT-PHONE-CONSENT DELIMITED BY '  '
                  '" ' DELIMITED BY SIZE
                  WS-SCORE-DISPLAY DELIMITED BY SPACES
               INTO WS-COMMAND-TEXT
               WITH POINTER WS-CMD-PTR
           END-STRING

           SUBTRACT 1 FROM WS-CMD-PTR
           MOVE WS-CMD-PTR TO WS-COMMAND-LENGTH.

      *****************************************************************
      * EXECUTE PYTHON VIA BPXWUNIX CALLABLE SERVICE                  *
      * This is the z/OS compliant method to call USS commands        *
      *****************************************************************
       3020-EXECUTE-BPXWUNIX.
           INITIALIZE WS-STDOUT-DATA
           INITIALIZE WS-STDERR-DATA
           MOVE 1024 TO WS-STDOUT-LENGTH
           MOVE 512  TO WS-STDERR-LENGTH
           MOVE 0    TO WS-STDIN-LENGTH
           MOVE 0    TO WS-RETURN-VALUE
           MOVE 0    TO WS-RETURN-CODE
           MOVE 0    TO WS-REASON-CODE

           CALL 'BPXWUNIX' USING
               WS-COMMAND-LENGTH
               WS-COMMAND-TEXT
               WS-STDIN-LENGTH
               WS-STDIN-DATA
               WS-STDOUT-LENGTH
               WS-STDOUT-DATA
               WS-STDERR-LENGTH
               WS-STDERR-DATA
               WS-ENV-COUNT
               WS-ENV-LENGTH-ARRAY
               WS-ENV-ARRAY
               WS-RETURN-VALUE
               WS-RETURN-CODE
               WS-REASON-CODE
           END-CALL

           IF WS-STDERR-LENGTH > 0
               DISPLAY 'PYTHON STDERR: '
                       WS-STDERR-DATA(1:WS-STDERR-LENGTH)
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
      * PARSE PYTHON OUTPUT FROM STDOUT BUFFER                        *
      * Format: workable|dialable|risk_segment|small_bal|activity|freq*
      *****************************************************************
       3200-PARSE-STDOUT-OUTPUT.
           IF WS-STDOUT-LENGTH > 0
               MOVE WS-STDOUT-DATA(1:WS-STDOUT-LENGTH)
                   TO WS-OUTPUT-LINE
               PERFORM 3300-PARSE-PYTHON-OUTPUT
           ELSE
               PERFORM 3100-SET-DEFAULT-VALUES
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

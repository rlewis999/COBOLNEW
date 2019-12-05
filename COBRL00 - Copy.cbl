       IDENTIFICATION DIVISION.
       PROGRAM-ID.     COBRL01.
       AUTHOR.         ROBERT LEWIS.
       DATE-WRITTEN.   11/26/19.
       DATE-COMPILED.
      ************************************************
      *  THIS PROGRAM READS A FILE AND CREATES      *
      *  A STUDENT ROSTER REPORT.
      ************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT STUDENT-MASTER
               ASSIGN TO 'E:\COBOL\PAINTEST.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO 'E:\COBOL\PJOBEST.PRT'
               ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.F
       
       FD  STUDENT-MASTER
           LABEL RECORD IS STANDARD       
           DATA RECORD IS PAINT-REC
           RECORD CONTAINS 49 CHARACTERS.

       01  PAINT-REC.
           05 PAINT-EST-NO                 PIC X(4).
           05 PAINT-DATE.
              10   PAINT-YY                PIC 9(4).
              10   PAINT-MM                PIC 99.
              10   PAINT-DD                PIC 99.
           05 PAINT-WALL-SQ-FT             PIC 9(4).
           05 PAINT-DOOR-SQ-FT             PIC 9(3).
           05 PAINT-PRICE-GAL              PIC 99V99.
      *ALL RECS OUT*
       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 30 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.
       
       01  PRTLINE             PIC X(30).


       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  C-TCTR          PIC 999     VALUE 0.
           05  C-PCTR          PIC 99      VALUE ZERO.
           05 MORE-RECS        PIC XXX     VALUE 'YES'.

       01  CURRENT-DATE-AND-TIME.
           05  PRINT-DATE.
               10  PRINT-YY        PIC 9(4).
               10  PRINT-MM        PIC 99.
               10  PRINT-DD        PIC 99.
           05  I-TIME          PIC X(11).

       01  COMPANY-TITLE.
           05  FILLER          PIC X(6)    VALUE 'DATE:'.
           05  PAINT-MM        PIC 99.
           05  FILLER          PIC X       VALUE '/'.
           05  PAINT-DD        PIC 99.
           05  FILLER          PIC X       VALUE '/'.
           05  PAINT-YY        PIC 9(4).
           05  FILLER          PIC X(35)   VALUE SPACES.
           05  FILLER          PIC X(30)
                               VALUE'LEWIS''S PAINT ESTIMATOR'.
           05  FILLER          PIC X(42)   VALUE SPACES.
           05  FILLER          PIC X(6)    VALUE 'PAGE:'.
           05  O-PCTR          PIC Z9.
      *COLUMNS*
       01  PRTCOL-1.
           05  FILLER          PIC X(3)    VALUE SPACES.
           05  FILLER          PIC X(21)   VALUE 'ESTIMATE'.
           05  FILLER          PIC X(21)   VALUE SPACES.
           05  FILLER          PIC X(21)   VALUE 'WALL'.
           05  FILLER          PIC X(21)   VALUE SPACES.
           05  FILLER          PIC X(21)   VALUE 'DOOR'.
           05  FILLER          PIC X(21)    VALUE SPACES.
           05  FILLER          PIC X(21)   VALUE 'TOTAL'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(11)   VALUE 'GALLONS'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(11)   VALUE 'PRICE/'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(11)   VALUE 'PAINT'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(11)   VALUE 'LABOR'.
           05  FILLER          PIC X(9)    VALUE SPACES.
           05  FILLER          PIC X(11)   VALUE 'TOTAL'.

       01  PRTCOL-2.
           05  FILLER          PIC X(1)    VALUE 'NUMBER'.
           05  FILLER          PIC X(5)   VALUE SPACES.
           05  FILLER          PIC X(9)    VALUE 'ESIMATE DATE'.
           05  FILLER          PIC X(5)   VALUE SPACES.
           05  FILLER          PIC X(10)   VALUE 'SQ/FT'.
           05  FILLER          PIC X(5)   VALUE SPACES.
           05  FILLER          PIC X(19)   VALUE 'SQ/FT'.
           05  FILLER          PIC X(5)   VALUE SPACES.
           05  FILLER          PIC X(5)   VALUE 'SQ/FT'.
           05  FILLER          PIC X(5)   VALUE SPACES.
           05  FILLER          PIC X(5)   VALUE 'NEEDED'.
           05  FILLER          PIC X(5)   VALUE SPACES.
           05  FILLER          PIC X(5)   VALUE 'GALLON'.
           05  FILLER          PIC X(5)   VALUE SPACES.
           05  FILLER          PIC X(5)   VALUE 'ESTIMATE'.
           05  FILLER          PIC X(5)   VALUE SPACES.
           05  FILLER          PIC X(15)   VALUE 'ESTIMATE'.
           05  FILLER          PIC X(26)   VALUE SPACES.
           05  FILLER          PIC X(15)   VALUE 'ESTIMATE'.
      *DETAIL*
       01  PRT-DETAIL.
           05  O-EST-NO        PIC X(7).
           05  FILLER          PIC X(20)   VALUE SPACES.
           05  O-YY            PIC X(15).
           05  FILLER          PIC X(20)   VALUE SPACES.
           05  O-MM            PIC X(15).
           05  FILLER          PIC X(20)   VALUE SPACES.
           05  O-DD            PIC Z.99.
           05  FILLER          PIC X(20)   VALUE SPACES.
           05  O-WALL-SQ-FT    PIC ZZZ,ZZZ.99.
           05  FILLER          PIC XX      VALUE SPACES.
           05  O-DOOR-SQ-FT    PIC ZZZ,ZZZ.99.
           05  FILLER          PIC XX      VALUE SPACES.
           05  O-PRICE-SQ-GAL  PIC ZZZ,ZZZ.99.

       01  PRT-GTTOTAL.
           05  FILLER          PIC X(54)   VALUE SPACES.
           05  FILLER          PIC X(15)   VALUE 'TOTAL ESTIMATES:'.
           05  O-SCTR          PIC ZZ9.
           05  FILLER          PIC X(60)   VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           OPEN INPUT STUDENT-MASTER.
           OPEN OUTPUT PRTOUT.

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE PAINT-YY TO O-YY.
           MOVE PAINT-DD TO O-DD.
           MOVE PAINT-MM TO O-MM.

           PERFORM 9000-READ.
           PERFORM 9100-HEADINGS.
       
       2000-MAINLINE.
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

       2100-CALCS.
           ADD 1 TO C-SCTR.

       2200-OUTPUT.
           MOVE PAINT-EST-NO TO O-EST-NO.
           MOVE PAINT-YY TO O-YY.
           MOVE PAINT-MM TO O-MM.
           MOVE PAINT-DD TO O-DD.
           MOVE PAINT-WALL-SQ-FT TO O-DOOR-SQ-FT.
           MOVE PAINT-PRICE-GAL TO O-PRICE-GAL.

           WRITE PRTLINE FROM PRT-DETAIL
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9100-HEADINGS.
       3000-CLOSING.
           MOVE C-GCTR TO O-GCTR.
           WRITE PRTLINE FROM PRT-TOTAL
               AFTER ADVANCING 3 LINES.

           CLOSE STUDENT-MASTER
                 PRTOUT.

       9000-READ.
           READ STUDENT-MASTER
               AT END
                   MOVE 'NO' TO MORE-RECS.

       9100-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR
           WRITE PRTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE
           WRITE PRTLINE FROM PRTCOL-1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM PRTCOL-2
               AFTER ADVANCING 1 LINES.



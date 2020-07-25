       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 RCD-01.
           05 CUST-INFO.
              10 CUST-NAME             PIC X(15) VALUE 'J.B. SMITH'.
              10 CUST-ADDR             PIC X(35) VALUE
                          '444 SPRING ST.,CHICAGO,IL.'.
              10 CUST-PHONE.
                 15 FILLER             PIC X(1) VALUE '('.
                 15 AREA-CODE          PIC X(3).
                 15 FILLER             PIC X(1) VALUE ')'.
                 15 PREFIX             PIC X(3).
                 15 FILLER             PIC X(1) VALUE '-'.
                 15 SUFFIX             PIC X(4).
           05  BILL-INFO.
               10 INV-NO               PIC X(6) VALUE 'A14275'.
               10 INV-AMT              PIC $$,$$$.99 VALUE '$4,736.85'.
               10 AMT-PAID             PIC $$,$$$.99 VALUE '$2,400.00'.
               10 DATE-PAID            PIC X(8) VALUE '09/22/76'.
               10 BAL-DUE              PIC $$,$$$.99 VALUE '$2,336.85'.
               10 DATE-DUE             PIC X(8) VALUE '10/22/76'.

       77  RPT-LINE                    PIC X(120).
       77  LINE-POS                    PIC S9(3).
       77  LINE-NO                     PIC 9(5) VALUE 1.
       77  DEC-POINT                   PIC X(1) VALUE '.'.

       PROCEDURE DIVISION.
           MOVE SPACES TO RPT-LINE.
           MOVE 4 TO LINE-POS.
           STRING
              LINE-NO SPACE CUST-INFO INV-NO SPACE DATE-DUE SPACE
                  DELIMITED BY SIZE
              BAL-DUE
                  DELIMITED BY DEC-POINT
I          INTO RPT-LINE
           WITH POINTER LINE-POS.
           DISPLAY RPT-LINE.
           GOBACK.
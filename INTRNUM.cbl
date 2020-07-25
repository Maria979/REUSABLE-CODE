       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTRNUM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ***
      *** VARIABLES FOR INTRINSIC FUNCTIONS MAX, LENGTH, UPPER CASE
       01  X                 PIC 9(2).
       01  PRICE1            PIC X(8) VALUE '$8000'.
       01  PRICE2            PIC X(8) VALUE '$2000'.
       01  OUTPUT-RECORD.
           05  PRODUCT-NAME  PIC X(20).
           05  PRODUCT-NUMBER PIC 9(9).
           05  PRODUCT-PRICE  PIC 9(6).
      ***
      *** VARIABLE FOR INTRINSIC FUNCTION CURRENT DATE
       01 DATE-VARS.
          05 CURRENT-YEAR      PIC X(4).
          05 CURRENT-MON       PIC X(2).
          05 CURRENT-DAY       PIC X(2).
          05 CURRENT-HOUR      PIC X(2).
          05 CURRENT-MIN       PIC X(2).
          05 CURRENT-SEC       PIC X(2).
          05 CURRENT-MSEC      PIC X(2).
          05 LOCAL-TIME.
             10 TIME-DIF     PIC X(1).
             10 TIME-DIF-H   PIC X(2).
             10 TIME-DIF-M   PIC X(2).
       01 CURRENT-WEEK-DAY   PIC 9(1).
       01 WEEKDAYS-TABLE.
          05                PIC X(9) VALUE "Monday".
          05                PIC X(9) VALUE "Tuesday".
          05                PIC X(9) VALUE "Wednesday".
          05                PIC X(9) VALUE "Thursday".
          05                PIC X(9) VALUE "Friday".
          05                PIC X(9) VALUE "Saturday".
          05                PIC X(9) VALUE "Sunday".
       01               REDEFINES WEEKDAYS-TABLE.
          05 DT-OF-WK            OCCURS 7 TIMES PIC X(9).

      ***
      *** VARIABLES FOR INTRINSIC FUNCTIONS MEAN, MEDIAN, RANGE

       01  TAX-S            PIC 99V999 VALUE .045.
       01  TAX-T            PIC 99V999 VALUE .02.
       01  TAX-W            PIC 99V999 VALUE .035.
       01  TAX-B            PIC 99V999 VALUE .03.

       01  DISPLAY-VARIBALES.
           05  AVE-TAX      PIC 99V999.
           05  MEDIAN-TAX   PIC 99V999.
           05  TAX-RANGE    PIC 99V999.

      ***
      *** VARIABLES FOR INTRINSIC FUNCTIONS PRESENT VALUE, ANNUITY

       01  PRESENT-VALUE-VARS.
           05  Series-Amt1      Pic 9(9)V99       Value 100.
           05  Series-Amt2      Pic 9(9)V99       Value 200.
           05  Series-Amt3      Pic 9(9)V99       Value 300.
           05  Discount-Rate    Pic S9(2)V9(6)    Value .10.
           05  Todays-Value     Pic 9(9)V99.

       01  ANNUITY-VARS.
           05  Loan             Pic 9(9)V99.
           05  Payment          Pic 9(9)V99.
           05  Interest         Pic 9(9)V99.
           05  Number-Periods   Pic 99.

      ***
      *** VARIABLES FOR INTRINSIC FUNCTIONS DISPLAY OF, NATIONAL OF

       01  EBCDIC-CCSID        PIC 9(4) BINARY VALUE 1140.
       01  ASCII-CCSID         PIC 9(4) BINARY VALUE 819.
       01  Input-EBCDIC        PIC X(80) Value Spaces.
       01  ASCII-Output        PIC X(80) Value Spaces.

       PROCEDURE DIVISION.
           COMPUTE PRODUCT-PRICE =
           FUNCTION MAX (FUNCTION NUMVAL-C(PRICE1)
                    FUNCTION NUMVAL-C(PRICE2)).
           DISPLAY 'PRODUCT PRICE: ' PRODUCT-PRICE.
           COMPUTE X = FUNCTION LENGTH(OUTPUT-RECORD).
           DISPLAY 'X: ' X.
           MOVE 'Socks and Stuff' TO PRODUCT-NAME.
           DISPLAY 'PRODUCT NAME: ' PRODUCT-NAME.
           MOVE FUNCTION UPPER-CASE(PRODUCT-NAME) TO PRODUCT-NAME.
           DISPLAY 'PRODUCT NAME: ' PRODUCT-NAME.

           DISPLAY '***'.

           MOVE FUNCTION CURRENT-DATE TO DATE-VARS.
           ACCEPT CURRENT-WEEK-DAY FROM DAY-OF-WEEK.
           DISPLAY "Date: Year " CURRENT-YEAR " Month " CURRENT-MON
                 " Day " CURRENT-DAY "(" DT-OF-WK(CURRENT-WEEK-DAY) ")".

           DISPLAY "Time: Hour " CURRENT-HOUR " Minute " CURRENT-MIN
                 " Second " CURRENT-SEC "." CURRENT-MSEC.

           IF LOCAL-TIME NOT = 0 THEN
             DISPLAY "Time difference with Greenwich mean time for this
      -       "zone: " TIME-DIF "HOURS: " TIME-DIF-H "MINUTES: " TIME-DI
      -        F-M.

           DISPLAY '***'.

           COMPUTE AVE-TAX = FUNCTION MEAN (TAX-S TAX-T TAX-W TAX-B).
           COMPUTE MEDIAN-TAX = FUNCTION MEDIAN(TAX-S TAX-T TAX-W TAX-B)
           COMPUTE TAX-RANGE = FUNCTION RANGE(TAX-S TAX-T TAX-W TAX-B).

           DISPLAY 'AVE TAX: ' AVE-TAX.
           DISPLAY 'MEDIAN-TAX: ' MEDIAN-TAX.
           DISPLAY 'TAX-RANGE: ' TAX-RANGE.

           DISPLAY '***'.

           Compute Todays-Value =
               Function Present-Value
                    (Discount-Rate Series-Amt1 Series-Amt2 Series-Amt3).
           DISPLAY 'TODAY-VALUE: ' TODAYS-VALUE.

           Compute Loan = 15000
           Compute Interest = .12
           Compute Number-Periods = 36
           Compute Payment =
                Loan * Function Annuity((Interest / 12) Number-Periods).
           DISPLAY 'PAYMENT: ' PAYMENT.

           DISPLAY '***'.
           Move Function Display-of
               (Function National-of (Input-EBCDIC EBCDIC-CCSID),
               ASCII-CCSID) to ASCII-output.
           DISPLAY 'EBCDIC-CCSID: ' EBCDIC-CCSID.
           DISPLAY 'ASCII-CCSID: ' ASCII-CCSID.
           DISPLAY 'Input-EBCDIC:' Input-EBCDIC.
           DISPLAY 'ASCII-Output: ' Input-EBCDIC.
           DISPLAY '***'.

           GOBACK.





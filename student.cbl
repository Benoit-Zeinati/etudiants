      ****************************************************************** 
      *    
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. student.
       AUTHOR. Benoit.

      ****************************************************************** 
      *    
      ****************************************************************** 
       ENVIRONMENT DIVISION.
      ****************************************************************** 
      *    
      ******************************************************************        
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ****************************************************************** 
      *    
      ****************************************************************** 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS. 

           SELECT F-OUTPUT
               ASSIGN TO 'output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.           

      ****************************************************************** 
      *    
      ****************************************************************** 
       DATA DIVISION.

      ****************************************************************** 
      *    
      ****************************************************************** 
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2       PIC 9(02).
       01  REC-F-INPUT-10      PIC X(10).
       01  REC-F-INPUT-100     PIC X(100).
       01  REC-F-INPUT-1000    PIC X(1000).

       01  REC-STUDENT.
           03 R-S-KEY          PIC 9(02).       
           03 R-LASTNAME       PIC X(07).       
           03 R-FIRSTNAME      PIC X(06).       
           03 R-AGE            PIC 9(02).       

       01  REC-COURSE.
           03 R-C-KEY          PIC 9(02).       
           03 R-LABEL          PIC X(21).       
           03 R-COEF           PIC 9.9.       
           03 R-GRADE          PIC 99.99.       

       FD  F-OUTPUT
           RECORD CONTAINS 250 CHARACTERS
           RECORDING MODE IS F.
       01  REC-F-OUTPUT        PIC X(250).
       01  REC-ST-OUTPUT.
           05 R-S-KEY-OUT      PIC 9(02).       
           05 R-LASTNAME-OUT   PIC X(07).       
           05 R-FIRSTNAME-OUT  PIC X(06).       
           05 R-AGE-OUT        PIC 9(02).  
           05 REC-LACOGR-OUT OCCURS 5 TIMES.
              10 R-LABEL-OUT      PIC X(21).       
              10 R-COEF-OUT       PIC 9,9.       
              10 R-GRADE-OUT      PIC 99,99.    

      ****************************************************************** 
      *    
      ****************************************************************** 
       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  F-OUTPUT-STATUS     PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

       01  DATA-STUDENT.
           05 STUDENT-COUNT      PIC 9(03) VALUE 0.
           05 COURSE-COUNT       PIC 9(03) VALUE 0.
           05 STUDENT OCCURS 1 TO 999 TIMES DEPENDING ON STUDENT-COUNT.
               10 S-C-ID         PIC 9(03).
               10 S-LASTNAME     PIC X(07).      
               10 S-FIRSTNAME    PIC X(06).
               10 S-AGE          PIC 9(02).
               10 S-AVERAGE      PIC 99v99.

       01  DATA-COURS.
           05 DATA-COURSE-REC OCCURS 1 TO 999 DEPENDING ON COURSE-COUNT.
              10 C-S-ID         PIC 9(03).
              10 C-CODE.
                 15 C-ID1       PIC A(01) VALUE 'C'.
                 15 C-ID2       PIC 9(02).
              10 C-LABEL        PIC X(21).
              10 C-COEF         PIC 9V9.
              10 C-GRADE        PIC 99V99.

       77  WS-LACOGR-IDX        PIC 9(02).

       01  CLASS-REPORT-CARD.
           05 CLASSE              PIC A(06) VALUE 'CLASSE'.
           05 CLASSE-AV-ST        PIC 99V99.
           05 CLASSE-AV-C1        PIC 99V99.
           05 CLASSE-AV-C2        PIC 99V99.
           05 CLASSE-AV-C3        PIC 99V99.
           05 CLASSE-AV-C4        PIC 99V99.
           05 CLASSE-AV-C5        PIC 99V99.
           05 CLASSE-AV-C6        PIC 99v99.
           05 CLASSE-AV-C7        PIC 99v99.
           05 CLASSE-AV-C8        PIC 99v99.
           05 CLASSE-AV-C9        PIC 99v99.
           05 ST-REPORT-CARD OCCURS 1 TO 999 DEPENDING ON WS-NUM-OF-ST.
              10 ST-REPCARD-ID    PIC 9(03).
              10 ST-REPCARD-LN    PIC X(07).
              10 ST-REPCARD-FN    PIC X(06).
              10 ST-REPCARD-AV    PIC 99V99.
              10 ST-REPCARD-COURS-AV OCCURS 5 TIMES.
                 15 ST-REPCARD-AVC PIC 99V99.
                
       77  WS-ST-REPCARD-IDX1     PIC 9(03).
       77  WS-ST-REPCARD-IDX2     PIC 9(03).


       77  WS-ASTERIX-LINE      PIC X(76) value all '*'.
   
       77  WS-OUTFILE-STCLN     PIC 9(03).
       77  WS-OUTFILE-LEN       PIC 9(03).
       77  WS-OUTPUT-FILE       PIC 9(03).
       77  WS-NUM-OF-MATERIALS  PIC 9(02) VALUE 0.
       77  WS-TTL-COEFGR        PIC 9(03)V99.
       77  WS-TTL-COEF          PIC 9(03)V99.
       77  WS-MULTI             PIC 9(03)V99.
       77  WS-NUM-OF-ST         PIC 9(03).
       77  WS-MATERIAL-AV       PIC 999V99.
       77  WS-MULTI-INT         PIC 9(03).

      ****************************************************************** 
      *    
      ****************************************************************** 
       PROCEDURE DIVISION.
      *Open input.dat file, mode input 
       OPEN INPUT F-INPUT.
      *Read from F-INPUT until F-INPUT-STATUS = F-INPUT-STATUS-EOF
       PERFORM UNTIL F-INPUT-STATUS-EOF
         READ F-INPUT  
         IF F-INPUT-STATUS-OK THEN
            IF REC-F-INPUT-2 = 01 THEN
               MOVE 0 TO WS-NUM-OF-MATERIALS
               PERFORM PARA-STUDENT-REC
            ELSE
               PERFORM PARA-COURSE-REC
            END-IF
         END-IF

       END-PERFORM.
       CLOSE F-INPUT.

       OPEN OUTPUT F-OUTPUT.

      *Save DATA-STUDENT and DATA-COURS tabmes in output.dat file
       MOVE 1 TO COURSE-COUNT.
       PERFORM VARYING STUDENT-COUNT FROM 1 BY 1 UNTIL STUDENT-COUNT >
                                        999 OR S-C-ID(STUDENT-COUNT) = 0
         MOVE S-C-ID(STUDENT-COUNT) TO R-S-KEY-OUT
         MOVE S-LASTNAME(STUDENT-COUNT) TO R-LASTNAME-OUT
         MOVE S-FIRSTNAME(STUDENT-COUNT) TO R-FIRSTNAME-OUT
         MOVE S-AGE(STUDENT-COUNT) TO R-AGE-OUT

      *Save student ID in STUDENT-AV-TBL table
      *   MOVE S-C-ID(STUDENT-COUNT) TO AV-ST-ID(STUDENT-COUNT)

         MOVE 1 TO WS-LACOGR-IDX
         MOVE 0 TO WS-TTL-COEFGR WS-TTL-COEF 
         PERFORM VARYING COURSE-COUNT FROM COURSE-COUNT BY 1 UNTIL
               COURSE-COUNT > 999 OR C-S-ID(COURSE-COUNT) <> R-S-KEY-OUT
           MOVE C-LABEL(COURSE-COUNT) TO R-LABEL-OUT(WS-LACOGR-IDX)
           MOVE C-COEF(COURSE-COUNT) TO R-COEF-OUT(WS-LACOGR-IDX)
           MOVE C-GRADE(COURSE-COUNT) TO R-GRADE-OUT(WS-LACOGR-IDX)

      *Accumulate student average in WS-TTL-COEFGR variable
           MULTIPLY C-COEF(COURSE-COUNT) BY C-GRADE(COURSE-COUNT) GIVING
                                                        WS-MULTI ROUNDED
           COMPUTE WS-TTL-COEFGR =  WS-TTL-COEFGR + WS-MULTI

      *Accumulate course coefficient in WS-TTL-COEF
           COMPUTE WS-TTL-COEF = WS-TTL-COEF + C-COEF(COURSE-COUNT)

           ADD 1 TO WS-LACOGR-IDX
         END-PERFORM

      *Calculate student total average
         DIVIDE WS-TTL-COEFGR BY WS-TTL-COEF GIVING 
                                        S-AVERAGE(STUDENT-COUNT) ROUNDED

         WRITE REC-ST-OUTPUT
         IF F-OUTPUT-STATUS-OK THEN
            CONTINUE
         ELSE 
            DISPLAY 'ERROR filing output record'
            CLOSE F-OUTPUT
            STOP RUN
         END-IF
       END-PERFORM.

      *Close output file
       CLOSE F-OUTPUT

      *Save number of students in WS-NUM-OF-ST 
       SUBTRACT 1 FROM STUDENT-COUNT.
       MOVE STUDENT-COUNT TO WS-NUM-OF-ST.

      *Display Student-Cours info
       DISPLAY '*********Report card**********'.

       PERFORM VARYING STUDENT-COUNT FROM 1 BY 1 UNTIL STUDENT-COUNT >
                                        999 OR S-C-ID(STUDENT-COUNT) = 0
         DISPLAY '*----*-----------*------------*-----*'
         DISPLAY '* ID * LAST NAME * FIRST NAME * AGE *'
         DISPLAY '*----*-----------*------------*-----*'
         DISPLAY ' ' S-C-ID(STUDENT-COUNT) '  ' WITH NO ADVANCING
         DISPLAY S-LASTNAME(STUDENT-COUNT) '     ' WITH NO ADVANCING
         DISPLAY S-FIRSTNAME(STUDENT-COUNT) '       ' WITH NO ADVANCING
         DISPLAY S-AGE(STUDENT-COUNT)
         DISPLAY '******************************************'
         DISPLAY '*ID  * COURS NAME          * Coef * GRADE*'
         DISPLAY '******************************************'
        
         PERFORM VARYING COURSE-COUNT FROM 1 BY 1 UNTIL COURSE-COUNT >
                                         999 OR C-S-ID(COURSE-COUNT) = 0
           IF C-S-ID(COURSE-COUNT) = S-C-ID(STUDENT-COUNT) THEN
              DISPLAY ' ' C-S-ID(COURSE-COUNT) '  ' WITH NO ADVANCING
              DISPLAY C-LABEL(COURSE-COUNT) ' ' WITH NO ADVANCING
              DISPLAY C-COEF(COURSE-COUNT) '    ' WITH NO ADVANCING
              DISPLAY C-GRADE(COURSE-COUNT)
           END-IF
         END-PERFORM

       END-PERFORM.

      *Sort STUDENT table by student last-first name in descending order
      *and display
       DISPLAY ' '.
       DISPLAY 'Student table sorted by last/first name'

       SORT STUDENT DESCENDING S-LASTNAME S-FIRSTNAME.

       PERFORM PARA-DISP-ST-TBL.

      *Sort STUDENT table by student average in descending order and
      *display
       DISPLAY ' '.
       DISPLAY 'Student table sorted by average'
       SORT STUDENT DESCENDING S-AVERAGE.

       PERFORM PARA-DISP-ST-TBL.

      *Generate final report card
       PERFORM VARYING STUDENT-COUNT FROM 1 BY 1 UNTIL STUDENT-COUNT >
                                                            WS-NUM-OF-ST
      *MOVE fields from DATA-STUDENT table to CLASS-REPORT-CARD table
         MOVE S-C-ID(STUDENT-COUNT) TO ST-REPCARD-ID(STUDENT-COUNT) 
         MOVE S-LASTNAME(STUDENT-COUNT) TO ST-REPCARD-LN(STUDENT-COUNT)
         MOVE S-FIRSTNAME(STUDENT-COUNT) TO ST-REPCARD-FN(STUDENT-COUNT)
         MOVE S-AVERAGE(STUDENT-COUNT) TO ST-REPCARD-AV(STUDENT-COUNT) 

      *Keep looping until student ids match                                                     
         PERFORM VARYING WS-LACOGR-IDX FROM 1 BY 1 UNTIL
                           S-C-ID(STUDENT-COUNT) = C-S-ID(WS-LACOGR-IDX)
           CONTINUE
         END-PERFORM     

      *MOVE fields from DATA-COURS table to CLASS-REPORT-CARD table
         MOVE 1 TO WS-ST-REPCARD-IDX1
         PERFORM WS-NUM-OF-MATERIALS TIMES 
         MOVE C-GRADE(WS-LACOGR-IDX) TO 
                       ST-REPCARD-AVC(STUDENT-COUNT,WS-ST-REPCARD-IDX1)
         ADD 1 TO WS-ST-REPCARD-IDX1 WS-LACOGR-IDX
         END-PERFORM

       END-PERFORM.

      *Calculate class averge
       MOVE 1 TO WS-ST-REPCARD-IDX1.
       MOVE 0 TO WS-MATERIAL-AV.
       PERFORM WS-NUM-OF-ST TIMES
         COMPUTE WS-MATERIAL-AV = WS-MATERIAL-AV + 
                                       ST-REPCARD-AV(WS-ST-REPCARD-IDX1)
         ADD 1 TO WS-ST-REPCARD-IDX1
       END-PERFORM.
        DIVIDE WS-MATERIAL-AV BY WS-NUM-OF-ST GIVING CLASSE-AV-ST
                                                                 ROUNDED

      *Calculate class average for each material
       PERFORM VARYING WS-ST-REPCARD-IDX2 FROM 1 BY 1 UNTIL 
                                WS-ST-REPCARD-IDX2 > WS-NUM-OF-MATERIALS
         MOVE 0 TO WS-MATERIAL-AV
         PERFORM PARA-MATERIAL-AV
         DIVIDE WS-MATERIAL-AV BY WS-NUM-OF-ST GIVING WS-MATERIAL-AV
                                                                 ROUNDED

     
         EVALUATE WS-ST-REPCARD-IDX2
                  WHEN 1
                       MOVE WS-MATERIAL-AV TO CLASSE-AV-C1
                  WHEN 2 
                       MOVE WS-MATERIAL-AV TO CLASSE-AV-C2
                  WHEN 3 
                       MOVE WS-MATERIAL-AV TO CLASSE-AV-C3
                  WHEN 4 
                       MOVE WS-MATERIAL-AV TO CLASSE-AV-C4
                  WHEN 5 
                       MOVE WS-MATERIAL-AV TO CLASSE-AV-C5
                  WHEN 6 
                       MOVE WS-MATERIAL-AV TO CLASSE-AV-C6
                  WHEN 7 
                       MOVE WS-MATERIAL-AV TO CLASSE-AV-C7
                  WHEN 8 
                       MOVE WS-MATERIAL-AV TO CLASSE-AV-C8
                  WHEN 9 
                       MOVE WS-MATERIAL-AV TO CLASSE-AV-C9
                  
         END-EVALUATE
       
       END-PERFORM.

       DISPLAY ' '.
       DISPLAY WS-ASTERIX-LINE.
       DISPLAY '                             BULLETIN DE NOTES'.
       DISPLAY WS-ASTERIX-LINE.
       DISPLAY ' '.

       DISPLAY 'ID  NOM     PRENOM    MOYENNE  ' WITH NO ADVANCING.
       MOVE 1 TO WS-ST-REPCARD-IDX1.

       PERFORM WS-NUM-OF-MATERIALS TIMES
          DISPLAY C-CODE(WS-ST-REPCARD-IDX1) '     ' WITH NO ADVANCING
          ADD 1 TO WS-ST-REPCARD-IDX1
       END-PERFORM.

       DISPLAY ' '.
       DISPLAY ' '.

       PERFORM VARYING WS-ST-REPCARD-IDX1 FROM 1 BY 1 UNTIL
                                       WS-ST-REPCARD-IDX1 > WS-NUM-OF-ST
         DISPLAY ST-REPCARD-ID(WS-ST-REPCARD-IDX1) ' ' WITH NO ADVANCING
         DISPLAY ST-REPCARD-LN(WS-ST-REPCARD-IDX1) ' ' WITH NO ADVANCING   
         DISPLAY ST-REPCARD-FN(WS-ST-REPCARD-IDX1) '    ' WITH NO 
                                                               ADVANCING  
         DISPLAY ST-REPCARD-AV(WS-ST-REPCARD-IDX1) '    ' WITH NO 
                                                               ADVANCING

         PERFORM VARYING WS-ST-REPCARD-IDX2 FROM 1 BY 1 UNTIL
                                WS-ST-REPCARD-IDX2 > WS-NUM-OF-MATERIALS
         DISPLAY 
          ST-REPCARD-AVC(WS-ST-REPCARD-IDX1,WS-ST-REPCARD-IDX2) '   '
                                                       WITH NO ADVANCING
         END-PERFORM

         DISPLAY ' '
       
       END-PERFORM.

       DISPLAY ' '.
       DISPLAY 'CLASSE                ' CLASSE-AV-ST '    ' WITH NO 
                                                              ADVANCING.

       PERFORM VARYING WS-ST-REPCARD-IDX1 FROM 1 by 1 UNTIL
                                WS-ST-REPCARD-IDX1 > WS-NUM-OF-MATERIALS
         EVALUATE WS-ST-REPCARD-IDX1
           WHEN 1
             IF WS-ST-REPCARD-IDX1 = WS-NUM-OF-MATERIALS
                DISPLAY CLASSE-AV-C1 
             ELSE 
                DISPLAY CLASSE-AV-C1 '   ' WITH NO ADVANCING
             END-IF
           WHEN 2 
                IF WS-ST-REPCARD-IDX1 = WS-NUM-OF-MATERIALS
                   DISPLAY CLASSE-AV-C2 
                ELSE 
                   DISPLAY CLASSE-AV-C2 '   ' WITH NO ADVANCING
                END-IF
           WHEN 3 
                IF WS-ST-REPCARD-IDX1 = WS-NUM-OF-MATERIALS
                   DISPLAY CLASSE-AV-C3 
                ELSE 
                   DISPLAY CLASSE-AV-C3 '   ' WITH NO ADVANCING
                END-IF
           WHEN 4 
                IF WS-ST-REPCARD-IDX1 = WS-NUM-OF-MATERIALS
                   DISPLAY CLASSE-AV-C4 
                ELSE 
                   DISPLAY CLASSE-AV-C4 '   ' WITH NO ADVANCING
                END-IF
       
           WHEN 5 
                IF WS-ST-REPCARD-IDX1 = WS-NUM-OF-MATERIALS
                   DISPLAY CLASSE-AV-C5 
                ELSE 
                   DISPLAY CLASSE-AV-C5 '   ' WITH NO ADVANCING
                END-IF
           WHEN 6 
                DISPLAY CLASSE-AV-C6 '   ' 
           WHEN OTHER
                NEXT SENTENCE
         END-EVALUATE
       END-PERFORM.


       
      * DISPLAY CLASSE-AV-C1 '   ' CLASSE-AV-C2 '   ' CLASSE-AV-C3 WITH
      *                                                     NO ADVANCING.
      * DISPLAY '   ' CLASSE-AV-C4 '   ' CLASSE-AV-C5 '   ' WITH NO
      *                                                        ADVANCING.
      * DISPLAY CLASSE-AV-C6.                            
      * 
       DISPLAY WS-ASTERIX-LINE.
       
       PERFORM VARYING WS-ST-REPCARD-IDX1 FROM 1 by 1 UNTIL
                                WS-ST-REPCARD-IDX1 > WS-NUM-OF-MATERIALS
         DISPLAY C-CODE(WS-ST-REPCARD-IDX1) ' => COEF: ' 
                            C-COEF(WS-ST-REPCARD-IDX1) WITH NO ADVANCING
         DISPLAY ' LABEL: ' C-LABEL(WS-ST-REPCARD-IDX1)
       END-PERFORM.

       DISPLAY WS-ASTERIX-LINE.
       DISPLAY "NOMBRE D'ELEVES =>  " WS-NUM-OF-ST.
       DISPLAY 'NOMBRE DE COURS =>  ' WS-NUM-OF-MATERIALS.
       COMPUTE WS-MULTI-INT = WS-NUM-OF-ST * WS-NUM-OF-MATERIALS.
       DISPLAY 'NOMBRE DE NOTES =>  ' WS-MULTI-INT.
       
       OPEN OUTPUT F-OUTPUT.
       MOVE WS-ASTERIX-LINE TO REC-F-OUTPUT.
       WRITE REC-F-OUTPUT.
       MOVE '                             BULLETIN DE NOTES' TO 
                                                          REC-F-OUTPUT.
       WRITE REC-F-OUTPUT.
       MOVE WS-ASTERIX-LINE TO REC-F-OUTPUT.
       WRITE REC-F-OUTPUT.

       MOVE 'ID  NOM     PRENOM    MOYENNE  ' TO REC-F-OUTPUT.
       MOVE 31 TO WS-OUTFILE-STCLN.
       MOVE 1 TO WS-ST-REPCARD-IDX1.
       MOVE LENGTH OF C-CODE TO WS-OUTPUT-FILE.

       PERFORM WS-NUM-OF-MATERIALS TIMES
         MOVE C-CODE(WS-ST-REPCARD-IDX1) TO 
                           REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
         ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
         ADD 5 TO WS-OUTFILE-STCLN
         ADD 1 TO WS-ST-REPCARD-IDX1
       END-PERFORM. 
       
       WRITE REC-F-OUTPUT.
       MOVE SPACES TO REC-F-OUTPUT.
       WRITE REC-F-OUTPUT.
       WRITE REC-F-OUTPUT.

       PERFORM VARYING WS-ST-REPCARD-IDX1 FROM 1 BY 1 UNTIL
                                       WS-ST-REPCARD-IDX1 > WS-NUM-OF-ST
         MOVE 1 TO WS-OUTFILE-STCLN
         MOVE LENGTH OF ST-REPCARD-ID TO WS-OUTPUT-FILE
         MOVE ST-REPCARD-ID(WS-ST-REPCARD-IDX1) TO 
                         REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
         ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
         ADD 1 TO WS-OUTFILE-STCLN
         MOVE LENGTH OF ST-REPCARD-LN TO WS-OUTPUT-FILE
         MOVE ST-REPCARD-LN(WS-ST-REPCARD-IDX1) TO
                         REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
         ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
         ADD 1 TO WS-OUTFILE-STCLN
         MOVE LENGTH OF ST-REPCARD-FN TO WS-OUTPUT-FILE
         MOVE ST-REPCARD-FN(WS-ST-REPCARD-IDX1) TO
                         REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
         ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
         ADD 4 TO WS-OUTFILE-STCLN
         MOVE 2 TO WS-OUTPUT-FILE 
         MOVE ST-REPCARD-AV(WS-ST-REPCARD-IDX1)(1:2) TO
                         REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
         ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
         MOVE 1 TO WS-OUTPUT-FILE
         MOVE '.' TO REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
         ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
         MOVE 2 TO WS-OUTPUT-FILE
         MOVE ST-REPCARD-AV(WS-ST-REPCARD-IDX1)(3:2) TO
                         REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
         ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
         ADD 3 TO WS-OUTFILE-STCLN
         
         PERFORM VARYING WS-ST-REPCARD-IDX2 FROM 1 BY 1 UNTIL
                                WS-ST-REPCARD-IDX2 > WS-NUM-OF-MATERIALS
           MOVE 2 TO WS-OUTPUT-FILE 
           MOVE ST-REPCARD-AVC(WS-ST-REPCARD-IDX1,WS-ST-REPCARD-IDX2)
                (1:2) TO REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
           ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
           MOVE 1 TO WS-OUTPUT-FILE
           MOVE '.' TO REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
           ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
           MOVE 2 TO WS-OUTPUT-FILE
           MOVE ST-REPCARD-AVC(WS-ST-REPCARD-IDX1,WS-ST-REPCARD-IDX2)
                (3:2) TO REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
           ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
           ADD 3 TO WS-OUTFILE-STCLN
         END-PERFORM
         
         WRITE REC-F-OUTPUT
       
       END-PERFORM.

       MOVE SPACES TO REC-F-OUTPUT.
       WRITE REC-F-OUTPUT.
       MOVE 'CLASSE                ' TO REC-F-OUTPUT.
       MOVE 23 TO WS-OUTFILE-STCLN.
       MOVE 2 TO WS-OUTPUT-FILE.
       MOVE CLASSE-AV-ST(1:2) TO
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE).
       ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN.
       MOVE 1 TO WS-OUTPUT-FILE.
       MOVE '.' TO REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE).
       ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN.
       MOVE 2 TO WS-OUTPUT-FILE.
       MOVE CLASSE-AV-ST(3:2) TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE).
       ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN.
       ADD 3 TO WS-OUTFILE-STCLN.

       PERFORM VARYING WS-ST-REPCARD-IDX1 FROM 1 by 1 UNTIL
                                WS-ST-REPCARD-IDX1 > WS-NUM-OF-MATERIALS
         EVALUATE WS-ST-REPCARD-IDX1
           WHEN 1 
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C1(1:2) TO
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 1 TO WS-OUTPUT-FILE
                MOVE '.' TO 
                           REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C1(3:2) TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                ADD 3 TO WS-OUTFILE-STCLN
           WHEN 2 
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C2(1:2) TO
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 1 TO WS-OUTPUT-FILE
                MOVE '.' TO 
                           REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C2(3:2) TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                ADD 3 TO WS-OUTFILE-STCLN
           WHEN 3 
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C3(1:2) TO
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 1 TO WS-OUTPUT-FILE
                MOVE '.' TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C3(3:2) TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                ADD 3 TO WS-OUTFILE-STCLN
           WHEN 4 
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C4(1:2) TO
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 1 TO WS-OUTPUT-FILE
                MOVE '.' TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C4(3:2) TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                ADD 3 TO WS-OUTFILE-STCLN
           WHEN 5 
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C5(1:2) TO
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 1 TO WS-OUTPUT-FILE
                MOVE '.' TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C5(3:2) TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                ADD 3 TO WS-OUTFILE-STCLN
           WHEN 6
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C6(1:2) TO
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 1 TO WS-OUTPUT-FILE
                MOVE '.' TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                MOVE 2 TO WS-OUTPUT-FILE
                MOVE CLASSE-AV-C6(3:2) TO 
                          REC-F-OUTPUT(WS-OUTFILE-STCLN:WS-OUTPUT-FILE)
                ADD WS-OUTPUT-FILE TO WS-OUTFILE-STCLN
                ADD 3 TO WS-OUTFILE-STCLN
           WHEN OTHER 
                NEXT SENTENCE
         END-EVALUATE
       END-PERFORM.

       WRITE REC-F-OUTPUT.
       MOVE WS-ASTERIX-LINE TO REC-F-OUTPUT.
       WRITE REC-F-OUTPUT.

       MOVE "NOMBRE D'ELEVES =>  " TO REC-F-OUTPUT.
       MOVE WS-NUM-OF-ST TO REC-F-OUTPUT(20:3).
       WRITE REC-F-OUTPUT.

       MOVE 'NOMBRE DE COURS =>  ' TO REC-F-OUTPUT.
       MOVE WS-NUM-OF-MATERIALS TO REC-F-OUTPUT(20:2).
       WRITE REC-F-OUTPUT.

       MOVE 'NOMBRE DE NOTES =>  ' TO REC-F-OUTPUT.
       COMPUTE WS-MULTI-INT = WS-NUM-OF-ST * WS-NUM-OF-MATERIALS.
       MOVE WS-MULTI-INT TO REC-F-OUTPUT(20:3).
       WRITE REC-F-OUTPUT.

       CLOSE F-OUTPUT.

       STOP RUN.

       PARA-MATERIAL-AV.
       PERFORM VARYING WS-ST-REPCARD-IDX1 FROM 1 BY 1 UNTIL
                                  WS-ST-REPCARD-IDX1 > WS-NUM-OF-ST
         COMPUTE WS-MATERIAL-AV = WS-MATERIAL-AV + 
                  ST-REPCARD-AVC(WS-ST-REPCARD-IDX1,WS-ST-REPCARD-IDX2)
       END-PERFORM.

       PARA-DISP-ST-TBL.
       Display 'Id   * Last    * First  * Age * Average *'.
       DISPLAY '---    -------   ------ * ---   ------- *'.
       PERFORM VARYING STUDENT-COUNT FROM 1 BY 1 UNTIL STUDENT-COUNT >
                                        999 OR S-C-ID(STUDENT-COUNT) = 0
         DISPLAY S-C-ID(STUDENT-COUNT) '   ' S-LASTNAME(STUDENT-COUNT)
                                                       WITH NO ADVANCING
         DISPLAY '   ' S-FIRSTNAME(STUDENT-COUNT) '   ' WITH NO
                                                               ADVANCING
         DISPLAY S-AGE(STUDENT-COUNT) '      ' WITH NO ADVANCING
         DISPLAY S-AVERAGE(STUDENT-COUNT) 
       END-PERFORM.

       PARA-STUDENT-REC.
       ADD 1 TO STUDENT-COUNT.
       MOVE STUDENT-COUNT TO S-C-ID(STUDENT-COUNT).
       MOVE R-LASTNAME TO S-LASTNAME(STUDENT-COUNT).
       MOVE R-FIRSTNAME TO S-FIRSTNAME(STUDENT-COUNT).
       MOVE R-AGE TO S-AGE(STUDENT-COUNT).

       PARA-COURSE-REC.
       ADD 1 TO COURSE-COUNT.
       MOVE S-C-ID(STUDENT-COUNT) TO C-S-ID(COURSE-COUNT).
       MOVE R-LABEL TO C-LABEL(COURSE-COUNT).
       MOVE R-COEF TO C-COEF(COURSE-COUNT).
       MOVE R-GRADE TO C-GRADE(COURSE-COUNT).
       ADD 1 TO WS-NUM-OF-MATERIALS.
       MOVE WS-NUM-OF-MATERIALS TO C-ID2(COURSE-COUNT).
    
           

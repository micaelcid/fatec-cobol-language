      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MESES-ANO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 OPCAO        PIC X VALUE SPACES.
       01 OPCAO-INIC   PIC X VALUE SPACES.
       01 ESPACAMENTO  PIC 9(4).
       01 ESPACAMENTO2  PIC 9(4).

       01 MESES-ANO.
           02 FILLER PIC X(9) VALUE "Janeiro".
           02 FILLER PIC X(9) VALUE "Fevereiro".
           02 FILLER PIC X(9) VALUE "Marco".
           02 FILLER PIC X(9) VALUE "Abril".
           02 FILLER PIC X(9) VALUE "Maio".
           02 FILLER PIC X(9) VALUE "Junho".
           02 FILLER PIC X(9) VALUE "Julho".
           02 FILLER PIC X(9) VALUE "Agosto".
           02 FILLER PIC X(9) VALUE "Setembro".
           02 FILLER PIC X(9) VALUE "Outubro".
           02 FILLER PIC X(9) VALUE "Novembro".
           02 FILLER PIC X(9) VALUE "Dezembro".

       01 TABELA-MESES REDEFINES MESES-ANO.
           02 MES-T PIC X(9) OCCURS 12 TIMES.

       01 DATA-QUALQUER.
           02 DIA PIC 99 VALUE ZEROS.
           02 MES PIC 99 VALUE ZEROS.
           02 ANO PIC 9999 VALUE ZEROS.

       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       INICIO.
           PERFORM CORPO UNTIL OPCAO-INIC = "N" OR "n".
           DISPLAY "Fim de Programa" AT 2030.
           STOP RUN.

       CORPO.
           PERFORM ABERTURA.
           MOVE ZEROS TO DATA-QUALQUER.
           MOVE SPACES TO OPCAO.
           MOVE SPACES TO OPCAO-INIC.
           PERFORM RECEBE-DIA UNTIL DIA >=1 AND <= 31.
           PERFORM RECEBE-MES UNTIL MES >=1 AND <= 12.
           PERFORM RECEBE-ANO UNTIL ANO > 0 AND <= 2500.
           PERFORM MOSTRA.
           PERFORM CONTINUA UNTIL OPCAO = "S" OR "N" OR "s" OR "n".
           IF OPCAO = "S" OR "s"
               THEN
                   PERFORM CORPO
               ELSE
                   MOVE OPCAO TO OPCAO-INIC.

       ABERTURA.
           DISPLAY TELA.
           DISPLAY ":::::::::::::::::::::::::::::::::::::::::" AT 0920.
           DISPLAY ":                                       :" AT 1020.
           DISPLAY ": DIGITE UMA DATA NO FORMATO DD/MM/AAAA :" AT 1120.
           DISPLAY ":                                       :" AT 1220.
           DISPLAY ":                                       :" AT 1320.
           DISPLAY ":                                       :" AT 1420.
           DISPLAY ":                                       :" AT 1520.
           DISPLAY ":                                       :" AT 1620.
           DISPLAY ":                                       :" AT 1720.
           DISPLAY ":                                       :" AT 1820.
           DISPLAY ":                                       :" AT 1920.
           DISPLAY ":                                       :" AT 2020.
           DISPLAY ":                                       :" AT 2120.
           DISPLAY ":::::::::::::::::::::::::::::::::::::::::" AT 2120.
       RECEBE-DIA.
           ACCEPT DIA AT 1236 WITH PROMPT AUTO.

       RECEBE-MES.
           ACCEPT MES AT 1239 WITH PROMPT AUTO.
           IF MES = 2 AND DIA > 29
           PERFORM RECEBE-DIA UNTIL DIA >=1 AND <=29.
           IF MES = 4 AND DIA > 30
           PERFORM RECEBE-DIA UNTIL DIA >=1 AND <=30.
           IF MES = 6 AND DIA > 30
           PERFORM RECEBE-DIA UNTIL DIA >=1 AND <=30.
           IF MES = 9 AND DIA > 30
           PERFORM RECEBE-DIA UNTIL DIA >=1 AND <=30.
           IF MES = 11 AND DIA > 30
           PERFORM RECEBE-DIA UNTIL DIA >=1 AND <=30.
       RECEBE-ANO.
           ACCEPT ANO AT 1242 WITH PROMPT AUTO.

       MOSTRA.
           DISPLAY "Data por extenso: " AT 1322.
           DISPLAY DIA AT 1526.
           DISPLAY " de " AT 1528.
           DISPLAY MES-T(MES) AT 1532
           FOREGROUND-COLOR 5.

           IF MES = 5
           MOVE 1537 TO ESPACAMENTO.
           IF MES = 3 or 4 or 6 or 7
           MOVE 1538 TO ESPACAMENTO.
           IF MES = 8
           MOVE 1539 TO ESPACAMENTO.
           IF MES = 10 OR 1
           MOVE 1540 TO ESPACAMENTO.
           IF MES = 9 or 11 OR 12
           MOVE 1541 TO ESPACAMENTO.
           IF MES = 2
           MOVE 1542 TO ESPACAMENTO.


           DISPLAY " de " AT ESPACAMENTO.
           ADD 3 TO ESPACAMENTO
           MOVE ESPACAMENTO TO ESPACAMENTO2
           ADD 1 TO ESPACAMENTO2

           DISPLAY ANO AT ESPACAMENTO2.

       CONTINUA.
           DISPLAY "Continua (S/N)?" AT 1825.
           ACCEPT OPCAO AT 1841 WITH PROMPT AUTO.

       END PROGRAM MESES-ANO.

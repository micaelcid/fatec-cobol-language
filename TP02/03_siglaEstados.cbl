       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIGLA-ESTADOS.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 OPCAO        PIC X VALUE SPACES.
       01 OPCAO-INIC   PIC X VALUE SPACES.
       01 SIGLA        PIC X(2) VALUE SPACES.
       01 SIGLA-UP     PIC X(2) VALUE SPACES.
       01 LOOP         PIC 9(2) VALUE ZEROS.
       01 AAA          PIC 9 VALUE ZERO.
       01 ESTADOS.
           02 FILLER PIC X(19) VALUE "Acre".
           02 FILLER PIC X(19) VALUE "Alagoas".
           02 FILLER PIC X(19) VALUE "Amapa".
           02 FILLER PIC X(19) VALUE "Amazonas".
           02 FILLER PIC X(19) VALUE "Bahia".
           02 FILLER PIC X(19) VALUE "Ceara".
           02 FILLER PIC X(19) VALUE "Distrito Federal".
           02 FILLER PIC X(19) VALUE "Espirito Santo".
           02 FILLER PIC X(19) VALUE "Goias".
           02 FILLER PIC X(19) VALUE "Maranhao".
           02 FILLER PIC X(19) VALUE "Mato Grosso".
           02 FILLER PIC X(19) VALUE "Mato Grosso do Sul".
           02 FILLER PIC X(19) VALUE "Minas Gerais".
           02 FILLER PIC X(19) VALUE "Para".
           02 FILLER PIC X(19) VALUE "Paraiba".
           02 FILLER PIC X(19) VALUE "Parana".
           02 FILLER PIC X(19) VALUE "Pernambuco".
           02 FILLER PIC X(19) VALUE "Piaui".
           02 FILLER PIC X(19) VALUE "Rio de Janeiro".
           02 FILLER PIC X(19) VALUE "Rio Grande do Norte".
           02 FILLER PIC X(19) VALUE "Rio Grande do Sul".
           02 FILLER PIC X(19) VALUE "Rondonia".
           02 FILLER PIC X(19) VALUE "Roraima".
           02 FILLER PIC X(19) VALUE "Santa Catarina".
           02 FILLER PIC X(19) VALUE "Sao Paulo".
           02 FILLER PIC X(19) VALUE "Sergipe".
           02 FILLER PIC X(19) VALUE "Tocantins".
           02 FILLER PIC X(19) VALUE "Estado Inexistente".

       01 TABELA-ESTADOS REDEFINES ESTADOS.
           02 ESTADO-T PIC X(19) OCCURS 28 TIMES.

       01 SIGLAS.
           02 FILLER PIC X(2) VALUE "AC".
           02 FILLER PIC X(2) VALUE "AL".
           02 FILLER PIC X(2) VALUE "AP".
           02 FILLER PIC X(2) VALUE "AM".
           02 FILLER PIC X(2) VALUE "BA".
           02 FILLER PIC X(2) VALUE "CE".
           02 FILLER PIC X(2) VALUE "DF".
           02 FILLER PIC X(2) VALUE "ES".
           02 FILLER PIC X(2) VALUE "GO".
           02 FILLER PIC X(2) VALUE "MA".
           02 FILLER PIC X(2) VALUE "MT".
           02 FILLER PIC X(2) VALUE "MS".
           02 FILLER PIC X(2) VALUE "MG".
           02 FILLER PIC X(2) VALUE "PA".
           02 FILLER PIC X(2) VALUE "PB".
           02 FILLER PIC X(2) VALUE "PR".
           02 FILLER PIC X(2) VALUE "PE".
           02 FILLER PIC X(2) VALUE "PI".
           02 FILLER PIC X(2) VALUE "RJ".
           02 FILLER PIC X(2) VALUE "RN".
           02 FILLER PIC X(2) VALUE "RS".
           02 FILLER PIC X(2) VALUE "RO".
           02 FILLER PIC X(2) VALUE "RR".
           02 FILLER PIC X(2) VALUE "SC".
           02 FILLER PIC X(2) VALUE "SP".
           02 FILLER PIC X(2) VALUE "SE".
           02 FILLER PIC X(2) VALUE "TO".

       01 TABELA-SIGLA REDEFINES SIGLAS.
           02 SIGLA-T PIC X(2) OCCURS 27 TIMES.

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
           MOVE SPACES TO SIGLA.
           MOVE SPACES TO SIGLA-UP.
           MOVE SPACES TO OPCAO.
           MOVE SPACES TO OPCAO-INIC.
           MOVE ZEROS TO LOOP.
           MOVE ZERO TO AAA.
      *     PERFORM MOSTRA.
      *     IF SIGLA-UP EQUAL SPACES
      *         DISPLAY "DIGITE ALGO" AT 1215.

      *     MOVE SPACES TO SIGLA.
      *     MOVE SPACES TO SIGLA-UP.
      *     MOVE SPACES TO OPCAO.
      *     MOVE SPACES TO OPCAO-INIC.
      *     MOVE ZEROS TO LOOP.
      *     MOVE ZERO TO AAA.
           PERFORM MOSTRA-V.
           IF SIGLA-UP EQUAL SPACES
               DISPLAY ESTADO-T(28) AT 1240 FOREGROUND-COLOR 3.

           PERFORM CONTINUA UNTIL OPCAO = "S" OR "s" OR "N" OR "n".
           IF OPCAO = "S" OR "s"
               THEN
                   PERFORM CORPO
               ELSE
                   MOVE OPCAO TO OPCAO-INIC.


       ABERTURA.
           DISPLAY TELA.

      * MOSTRA.
      *     DISPLAY "DIGITE O ESTADO: " AT 1015.
      *     ACCEPT SIGLA AT 1032 WITH PROMPT AUTO.
      *     MOVE FUNCTION UPPER-CASE(SIGLA) TO SIGLA-UP.
      *     EVALUATE SIGLA-UP
      *     WHEN "AC"
      *         DISPLAY ESTADO-T(1) AT 1215
      *     WHEN "AL"
      *         DISPLAY ESTADO-T(2) AT 1215
      *     WHEN "AP"
      *         DISPLAY ESTADO-T(3) AT 1215
      *     WHEN "AM"
      *         DISPLAY ESTADO-T(4) AT 1215
      *     WHEN "BA"
      *         DISPLAY ESTADO-T(5) AT 1215
      *     WHEN "CE"
      *         DISPLAY ESTADO-T(6) AT 1215
      *     WHEN "DF"
      *         DISPLAY ESTADO-T(7) AT 1215
      *     WHEN "ES"
      *         DISPLAY ESTADO-T(8) AT 1215
      *     WHEN "GO"
      *         DISPLAY ESTADO-T(9) AT 1215
      *     WHEN "MA"
      *         DISPLAY ESTADO-T(10) AT 1215
      *     WHEN "MT"
      *         DISPLAY ESTADO-T(11) AT 1215
      *     WHEN "MS"
      *         DISPLAY ESTADO-T(12) AT 1215
      *     WHEN "MG"
      *         DISPLAY ESTADO-T(13) AT 1215
      *     WHEN "PA"
      *         DISPLAY ESTADO-T(14) AT 1215
      *     WHEN "PB"
      *         DISPLAY ESTADO-T(15) AT 1215
      *     WHEN "PR"
      *         DISPLAY ESTADO-T(16) AT 1215
      *     WHEN "PE"
      *         DISPLAY ESTADO-T(17) AT 1215
      *     WHEN "PI"
      *         DISPLAY ESTADO-T(18) AT 1215
      *     WHEN "RJ"
      *         DISPLAY ESTADO-T(19) AT 1215
      *     WHEN "RN"
      *         DISPLAY ESTADO-T(20) AT 1215
      *     WHEN "RS"
      *         DISPLAY ESTADO-T(21) AT 1215
      *     WHEN "RO"
      *         DISPLAY ESTADO-T(22) AT 1215
      *     WHEN "RR"
      *         DISPLAY ESTADO-T(23) AT 1215
      *     WHEN "SC"
      *         DISPLAY ESTADO-T(24) AT 1215
      *     WHEN "SP"
      *         DISPLAY ESTADO-T(25) AT 1215
      *     WHEN "SE"
      *         DISPLAY ESTADO-T(26) AT 1215
      *     WHEN "TO"
      *         DISPLAY ESTADO-T(27) AT 1215
      *     WHEN OTHER
      *         DISPLAY "Estado inexistente" at 1215.

       MOSTRA-V.
           DISPLAY "DIGITE O ESTADO: " AT 1040.
           ACCEPT SIGLA AT 1057 WITH PROMPT AUTO.
           MOVE FUNCTION UPPER-CASE(SIGLA) TO SIGLA-UP.

           PERFORM TEST AFTER VARYING LOOP FROM 1 BY 1 UNTIL LOOP = 28

               IF SIGLA-T(LOOP) EQUALS SIGLA-UP
                   DISPLAY ESTADO-T(LOOP) AT 1240 FOREGROUND-COLOR 3
                   MOVE 1 TO AAA
               END-IF

           END-PERFORM.

               IF AAA = 0
                   DISPLAY ESTADO-T(LOOP) AT 1240.

       CONTINUA.
           DISPLAY "Continua (S/N)?" AT 1635.
           ACCEPT OPCAO AT 1650 WITH PROMPT AUTO.

       END PROGRAM SIGLA-ESTADOS.

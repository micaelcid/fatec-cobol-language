       program-id. EX09-CALCULO-CIRCUNFERENCIA-B as
                   "EX09-CALCULO-CIRCUNFERENCIA-B".

       author. Grupo 01: Cintia Dias,Fabio Carcavali,Fernando Junior,
           Gabriel Azevedo,Julia Fernandes,Micael Cid,Rodolfo Feitosa.

       date-written. 25/09/2019.

      ******************************************************************
      * 9)    Programa Aula3 - Calcular a �rea de uma circunfer�ncia com m�scara.
      *
      ******************************************************************

      *================================================================*
       environment division.
      *================================================================*

       configuration section.
               SPECIAL-NAMES.
               DECIMAL-POINT is COMMA.
       data division.

      *----------------------------------------------------------------*
       working-storage section.
      *----------------------------------------------------------------*

       01 DADOS.
               02 w-raio       PIC  9(3)V99.
               02 MW-Raio      PIC  zz9,99.
               02 W-Area        PIC  zz.zz9,99.


       01 CONTINUAR.
             02 CONTINUA    PIC  X(01)    VALUE SPACE.


        01 MENSAGEMS-DE-TELA.
               02 MENSA1        PIC X(50) VALUE "DIGITE O Raio".
             02 MENSA2       PIC X(30) VALUE "FIM DO PROGRAMA".
             02 MENSA3        PIC X(30) VALUE SPACE.
             02 MENSA4       PIC X(30) VALUE "O RAIO NAO PODE SER ZERO".
       01 DATA-DO-SISTEMA.
               02 ANO       PIC 9(02) VALUE ZEROS.
               02 MES       PIC 9(02) VALUE ZEROS.
              02 DIA       PIC 9(02) VALUE ZEROS.

      *----------------------------------------------------------------*
       SCREEN SECTION.
      *----------------------------------------------------------------*

          01 TELA01.
             02 BLANK SCREEN.
             02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
             02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
             02 LINE 02 COLUMN 11 PIC 9(02)  USING ANO.
             02 LINE 02 COLUMN 28 VALUE
                  "Calcula da Area de um Circulo".
              02 LINE 08 COLUMN 15 VALUE "Raio:".
              02 LINE 10 COLUMN 15 VALUE "Area:".
             02 LINE 12 COLUMN 15 VALUE "Deseja Continuar (S/N): < >".


      *================================================================*
       procedure division.
      *================================================================*

       Inicio.

            ACCEPT  DATA-DO-SISTEMA FROM DATE.
            DISPLAY TELA01      AT    0101.
            MOVE    ZEROS       TO    DADOS.

       Processo.
           PERFORM Entrada
           PERFORM Calcula
           PERFORM FINALIZA
           EXIT.

       Entrada.
            PERFORM ENTRADA-RAIO UNTIL W-Raio > 0
            EXIT.

            ENTRADA-RAIO.
               DISPLAY MENSA1 AT 2030.
               ACCEPT MW-Raio AT 0821.
               MOVE MW-Raio to W-Raio.
               IF W-Raio = ZEROS OR W-Raio = 0
               DISPLAY MENSA4 AT 1830
               MOVE ZEROS TO W-Raio
               ELSE
               DISPLAY MENSA3 AT 1830
               END-IF
               DISPLAY MENSA3 AT 2030.
               EXIT.


       Calcula.
            compute             w-area = 3,1416 * ( w-raio ** 2).
            display                     w-area  at 1021.


       FINALIZA.
             ACCEPT        CONTINUA        AT     1240 PROMPT AUTO
             EVALUATE TRUE
             WHEN  CONTINUA = "n" OR "N"
             DISPLAY MENSA2 AT 1830
             Stop Run
             WHEN  CONTINUA = "S" OR "s"
             DISPLAY " " AT 8080
             WITH BLANK SCREEN
             MOVE  ZEROS TO DADOS
             MOVE  SPACE TO CONTINUA
             PERFORM Inicio
             PERFORM Entrada
             PERFORM CALCULA
             PERFORM FINALIZA
             WHEN OTHER
             MOVE SPACE TO CONTINUA
             PERFORM FINALIZA
             END-EVALUATE
             EXIT.






           goback.


       end program EX09-CALCULO-CIRCUNFERENCIA-B.

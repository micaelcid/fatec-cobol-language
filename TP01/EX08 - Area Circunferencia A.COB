       program-id. Ex08-Area_circunferencia as "Area_circunferencia".

       author. Grupo 01: Cintia Dias,Fabio Carcavali,Fernando Junior,
           Gabriel Azevedo,Julia Fernandes,Micael Cid,Rodolfo Feitosa.

       date-written. 25/09/2019.

      ******************************************************************
      * 8)    Programa Aula3 -  Calcular a �rea de uma circunfer�ncia.
      *
      ******************************************************************

      *================================================================*
       environment division.
      *================================================================*

       configuration section.

       data division.

      *----------------------------------------------------------------*
       working-storage section.
      *----------------------------------------------------------------*

       01 DADOS.
               02 W-Raio      PIC  9(03)v99.
             02 W-Area        PIC  9(05)V99.

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
               ACCEPT W-Raio AT 0821.
               IF W-Raio = ZEROS OR W-Raio = 0
               DISPLAY MENSA4 AT 1830
               MOVE ZEROS TO W-Raio
               ELSE
               DISPLAY MENSA3 AT 1830
               END-IF
               DISPLAY MENSA3 AT 2030.
               EXIT.

       Calcula.
            compute w-Area = 3.1416*(w-Raio**2).
            Display w-Area AT 1021.


       FINALIZA.
             ACCEPT        CONTINUA        AT     1240 PROMPT AUTO
             EVALUATE TRUE
             WHEN  CONTINUA = "n" OR "N"
             DISPLAY MENSA2 AT 1830
             Stop Run
             WHEN  CONTINUA = "S" OR "s"
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


       end program Ex08-Area_circunferencia.

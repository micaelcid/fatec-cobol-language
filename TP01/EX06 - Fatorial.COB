       program-id. EX06_FATORIAL as "EX06_FATORIAL".

       author. Grupo 01: Cintia Dias,Fabio Carcavali,Fernando Junior,
           Gabriel Azevedo,Julia Fernandes,Micael Cid,Rodolfo Feitosa.

       date-written. 25/09/2019.

      ******************************************************************
      * 6)    Programa 6 � Fatorial:  O que este programa n�o trata?;
      *                  Fa�a o mesmo programa anterior, mas aceitando
      *                  qualquer valor para vari�vel WS-NUMERO.
      ******************************************************************


      *================================================================*
       environment division.
       CONFIGURATION SECTION.
      *================================================================*
       special-names.

           decimal-point is comma.

       data division.
      *----------------------------------------------------------------*
       working-storage section.
      *----------------------------------------------------------------*

       01 AREA-TRABALHO.

            02 WS-CONTADOR             PIC 9(02)      VALUE ZEROS.
            02 WS-NUMERO               PIC 9(02)V99      VALUE ZEROS.
            02 WS-MASC-NUMERO          PIC  Z9.
            02 WS-RESULTADO            PIC 9(36)V99   VALUE ZEROS.
            02 WS-MASC-RESULT          PIC
            ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZZ.ZZ9
                                                      VALUE ZEROS.
            02 WS-FL                   PIC 9(01)      VALUE ZEROS.

       01 CONTINUAR.
              02 WS-CONTINUA             PIC A(01)      VALUE SPACE.
              02 WS-NOME                 PIC X(30)      VALUE SPACES.


       01 MENSAGEM-DE-TELA.
            02 MENSA1                  PIC X(35)   VALUE
                   "O NUMERO DEVE SER MENOR QUE 33!".
            02 MENSA2                  PIC X(30)   VALUE
                         "FIM DO PROGRAMA".
            02 MENSA3                  PIC X(50)   VALUE SPACES.
            02 MENSA4                  PIC X(46)   VALUE
               "(Digite um numero entre 0 a 32 para fatorar).".

       01 DATA-DO-SISTEMA.
            02 ANO                     PIC 9(04)   VALUE ZEROS.
            02 MES                     PIC 9(02)   VALUE ZEROS.
            02 DIA                     PIC 9(02)   VALUE ZEROS.


      *----------------------------------------------------------------*
       screen section.
      *----------------------------------------------------------------*

       01 TELA01.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 05    PIC 9(02)/ USING DIA.
           02 LINE 02 COLUMN 08    PIC 9(02)/ USING MES.
           02 LINE 02 COLUMN 11    PIC 9(04)  USING ANO.
           02 LINE 02 COLUMN 28    VALUE      "CALCULO DE FATORIAL.".
           02 LINE 09 COLUMN 21    VALUE      "NUMERO..: ".
           02 LINE 12 COLUMN 21    VALUE      "FATORIAL: ".
           02 LINE 15 COLUMN 21    VALUE      "CONTINUA (S/N): < >".

      *================================================================*
       procedure division.
      *================================================================*


       INCIO.
            ACCEPT DATA-DO-SISTEMA FROM DATE YYYYMMDD.
            PERFORM PROCESSO UNTIL WS-CONTINUA = "N" OR "n".
            PERFORM SAIDA.
            STOP RUN.


       PROCESSO.
            MOVE       ZEROS           TO          AREA-TRABALHO
            MOVE       SPACES          TO          CONTINUAR
            PERFORM    TELA
            MOVE       ZEROS           TO          WS-FL
            PERFORM    ENTRA-DADOS     UNTIL       WS-FL = 1
            PERFORM    CALCULA         UNTIL  WS-CONTADOR > WS-NUMERO
            PERFORM    RESULTADO
            PERFORM    CONTINUA-PROG UNTIL
                       WS-CONTINUA = "N" OR "n" OR "S" OR "s"
            EXIT.


       TELA.
               DISPLAY TELA01          AT          0101.
               MOVE    2               TO          WS-CONTADOR.
               MOVE    ZEROS           TO          WS-NUMERO.
               MOVE    1               TO          WS-RESULTADO.
               EXIT.


       ENTRA-DADOS.
            PERFORM ENTRA-VALOR-NUMERO.
            EXIT.

            ENTRA-VALOR-NUMERO.

               DISPLAY MENSA4             AT    0940
               ACCEPT  WS-MASC-NUMERO     AT    0933
               MOVE    WS-MASC-NUMERO     TO    WS-NUMERO
               IF      WS-NUMERO > 32
               DISPLAY MENSA3             AT    0940
               DISPLAY MENSA1             AT    0941
               STOP " "
               PERFORM ENTRA-VALOR-NUMERO
               ELSE
               DISPLAY MENSA3             AT    0940
               MOVE    1                  TO    WS-FL
               END-IF
               EXIT.


       CALCULA.
               COMPUTE  WS-RESULTADO = WS-RESULTADO * WS-CONTADOR.
               ADD     1                TO    WS-CONTADOR.
               EXIT.


       RESULTADO.

               EVALUATE WS-NUMERO
               WHEN     0 THRU 1
               DISPLAY  1               AT    1232
               WHEN     2 THRU                32
               MOVE     WS-RESULTADO    TO    WS-MASC-RESULT
               DISPLAY  WS-MASC-RESULT  AT    1232
               END-EVALUATE
               EXIT.


       CONTINUA-PROG.
               MOVE     SPACE           TO    WS-CONTINUA
               ACCEPT   WS-CONTINUA     AT    1538 WITH PROMPT AUTO
               EXIT.



       SAIDA.

               DISPLAY  MENSA2.
               STOP "FIM!!"
               EXIT.

           goback.

       end program EX06_FATORIAL.

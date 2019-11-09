       program-id. EX03_EQUACAO_2o_GRAU as "EX03_EQUACAO_2o_GRAU".

       author. Grupo 01: Cintia Dias,Fabio Carcavali,Fernando Junior,
           Gabriel Azevedo,Julia Fernandes,Micael Cid,Rodolfo Feitosa.


       date-written. 24/09/2019.

      ******************************************************************
      *3)    Programa Aula5 - Calcular a equa��o do segundo grau.
      ******************************************************************

      *================================================================*
       environment                 division.
      *================================================================*
       CONFIGURATION SECTION.
           Special-names.
       Decimal-point is comma.


       data division.
      *----------------------------------------------------------------*
       working-storage     section.
      *----------------------------------------------------------------*

       01 VARIAVEIS.
              02 VALOR-A                  PIC S9(04)      VALUE ZEROS.
              02 MASCARA-A                PIC  ---9.
              02 VALOR-B                  PIC S9(03)      VALUE ZEROS.
              02 MASCARA-B                PIC  ---9.
              02 VALOR-C                  PIC S9(03)      VALUE ZEROS.
              02 MASCARA-C                PIC  ---9.
              02 VALOR-DELTA              PIC S9(04)      VALUES ZEROS.
              02 MASCARA-DELTA            PIC ----9.
              02 RAIZ-X1                  PIC S9(04)V99   VALUES ZEROS.
              02 MASCARA-X1               PIC ----9,99.
              02 RAIZ-X2                  PIC S9(04)V99   VALUES ZEROS.
              02 MASCARA-X2               PIC ----9,99.


       01 VARIAVEL-CONTINUA.
              02 CONTINUA                 PIC A(01)       VALUE SPACES.


       01 MENSAGENS.
               02 MENS01               PIC X(33)         VALUE
                      "CALCULO EQUACAO DO 2o GRAU.".
               02 MENS02               PIC X(33)         VALUE
                      "REPRESENTACAO:  ax^2 + bx + c = 0".
               02 MENS03               PIC X(36)         VALUE
                    "ENTRE COM O VALOR DO COEFICIENTE a: ".
               02 MENS04               PIC X(60)         VALUE
            "O Valor do Coeficiente a deve ser diferente de 0 !!".
               02 MENS05               PIC X(36)         VALUE
                    "ENTRE COM O VALOR DO COEFICIENTE b: ".
               02 MENS06               PIC X(36)         VALUE
                    "ENTRE COM O VALOR DO COEFICIENTE C: ".
               02 MENS07               PIC X(64)         VALUE SPACES.
               02 MENS08               PIC X(41)         VALUE
                    "O VALOR DE DELTA E':        , PORTANTO:  ".
               02 MENS09               PIC X(30)         VALUE
                      "DELTA < 0, NAO EXISTEM RAIZES.".
               02 MENS10               PIC X(22)         VALUE
                             "FIM DO PROGRAMA".
               02 MENS11               PIC X(34)         VALUE
                       "DELTA = 0, EXISTE APENAS 01 RAIZ.".
               02 MENS12               PIC X(29)         VALUE
                        "DELTA > 0, EXISTEM 02 RAIZES.".
               02 MENS13                  PIC X(30)         VALUE
                       "DESEJA CONTINUAR (S / N): < >.".



       01 DATA-DO-SISTEMA.
               02 ANO                  PIC 9(02)       VALUE ZEROS.
               02 MES                  PIC 9(02)       VALUE ZEROS.
               02 DIA                  PIC 9(02)       VALUE ZEROS.

      *----------------------------------------------------------------*
       screen              section.
      *----------------------------------------------------------------*

       01 TELA01.
               02 BLANK SCREEN.
               02 LINE 2 COLUMN 05     PIC 9(02)/      USING DIA.
               02 LINE 2 COLUMN 08     PIC 9(02)/      USING MES.
               02 LINE 2 COLUMN 11     PIC 9(02)       USING ANO.
               02 LINE 2 COLUMN 28     PIC X(30)       USING MENS01.
               02 line 4 COLUMN 15     PIC X(33)       USING MENS02.


      *================================================================*
       procedure                   division.
      *================================================================*


       INICIO.
             DISPLAY       SPACE ERASE EOS     AT          0101
             ACCEPT        DATA-DO-SISTEMA     FROM        DATE.
             DISPLAY       TELA01              AT          0101.
             PERFORM       ROTINA-VALOR-A.
             PERFORM       ENTRA-A-B.



      *ENTRADA DO VALOR A AT� A SER DIFERENTE DE 0, USANDO IF E ELSE.
       ROTINA-VALOR-A.
             DISPLAY       MENS03              AT          0715.
             ACCEPT        MASCARA-A           AT          0751.
             MOVE          MASCARA-A           TO          VALOR-A.
             IF            VALOR-A             <>           0
             NEXT SENTENCE
             ELSE
             DISPLAY       MENS04              AT          0915
             MOVE          ZEROS               TO          VALOR-A
             PERFORM       ROTINA-VALOR-A
             END-IF.
             EXIT.

       ENTRA-A-B.
             DISPLAY       MENS07              AT          0915.
             DISPLAY       MENS05              AT          0915.
             ACCEPT        MASCARA-B           AT          0951.
             MOVE          MASCARA-B           TO          VALOR-B.
             DISPLAY       MENS06              AT          1115.
             ACCEPT        MASCARA-C           AT          1151.
             MOVE          MASCARA-C           TO          VALOR-C.
             EXIT.

      *CALCULA O VALOR DE DELTA
             COMPUTE  VALOR-DELTA = (VALOR-B * VALOR-B)
                                 - 4 *VALOR-A * VALOR-C.

      ******************************************************************
      *EVALUATE SEMELHANTE AO "SWITCH CASE".

             EVALUATE TRUE
             WHEN VALOR-DELTA < 0
             PERFORM       MOSTRAR-DELTA
             DISPLAY       MENS09              AT          1355
             PERFORM       FINALIZA

             WHEN VALOR-DELTA = 0
             PERFORM       MOSTRAR-DELTA
             DISPLAY       MENS11              AT          1355
             PERFORM       CALCULA-X1
             PERFORM       FINALIZA

             WHEN OTHER
             PERFORM       MOSTRAR-DELTA
             DISPLAY       MENS12              AT          1355
             PERFORM       CALCULA-X1
             PERFORM       CALCULA-X2
             PERFORM       FINALIZA
             END-EVALUATE.
      *****************************************************************

      *MOSTRA O VALOR DE DELTA
       MOSTRAR-DELTA.
             DISPLAY       MENS08              AT          1315
             MOVE          VALOR-DELTA         TO         MASCARA-DELTA
             DISPLAY       MASCARA-DELTA       AT          1335
             EXIT.

      *CALCULA A RAIZ X1
       CALCULA-X1.
             COMPUTE RAIZ-X1 = (-VALOR-B + FUNCTION SQRT(VALOR-DELTA))
                              / (2 * VALOR-A)
             MOVE          RAIZ-X1             TO          MASCARA-X1
             DISPLAY       "X1 = "             AT          1515
             DISPLAY       MASCARA-X1          AT          1520
             EXIT.

      *CALCULA A RAIZ X2
       CALCULA-X2.
             COMPUTE RAIZ-X2 = (-VALOR-B - FUNCTION SQRT(VALOR-DELTA))
                              / (2 * VALOR-A)
             MOVE          RAIZ-X2             TO          MASCARA-X2
             DISPLAY       "X2 = "             AT          1715
             DISPLAY       MASCARA-X2          AT          1720
             EXIT.

      *PARAGRAFO QUE FINALIZA O PROGRAMA
       FINALIZA.
             DISPLAY       MENS13          AT          1915
             ACCEPT        CONTINUA        AT          1942 PROMPT AUTO
             EVALUATE TRUE
             WHEN  CONTINUA = "n" OR "N"
             DISPLAY       MENS10          AT          2115
             STOP " "
             STOP RUN
             WHEN  CONTINUA = "S" OR "s"
             MOVE  ZEROS TO VARIAVEIS
             MOVE  SPACE TO CONTINUA
             PERFORM INICIO
             WHEN OTHER
             MOVE SPACE TO CONTINUA
             PERFORM FINALIZA
             END-EVALUATE
             EXIT.



           goback.

       end program EX03_EQUACAO_2o_GRAU.

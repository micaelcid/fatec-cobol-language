       program-id.EX04_EQ_2o_GRAU_ESTRU as "EX04_EQUACAO_2o_GRAU_ESTRU".

       author. Grupo 01: Cintia Dias,Fabio Carcavali,Fernando Junior,
           Gabriel Azevedo,Julia Fernandes,Micael Cid,Rodolfo Feitosa.

       date-written. 24/09/2019.

      ******************************************************************
      *4) Programa Aula6 - Transformar o programa de Equa��o
      *                    do Segundo Grau em um programa estruturado.
      ******************************************************************

      *================================================================*
       environment division.
       CONFIGURATION SECTION.
      *================================================================*
           Special-names.
           Decimal-point is comma.
      *ALTERA O PONTO DECIMAL (" PADRAO iNGLES") PARA VIRGULA ("PORTUG")
       data division.
      *----------------------------------------------------------------*
       working-storage section.
      *----------------------------------------------------------------*

       01 VARIAVEIS.
              02 VALOR-A                  PIC S9(03)      VALUE ZEROS.
              02 MASCARA-A                PIC  ---9.
              02 VALOR-B                  PIC S9(03)      VALUE ZEROS.
              02 MASCARA-B                PIC  ---9.
              02 VALOR-C                  PIC S9(03)      VALUE ZEROS.
              02 MASCARA-C                PIC  ---9.
              02 VALOR-DELTA              PIC S9(04)      VALUE ZEROS.
              02 MASCARA-DELTA            PIC ----9.
              02 RAIZ-X1                  PIC S9(04)V99   VALUE ZEROS.
              02 MASCARA-X1               PIC ----9,99.
              02 RAIZ-X2                  PIC S9(04)V99   VALUE ZEROS.
              02 MASCARA-X2               PIC ----9,99.
              02 CONTINUA                 PIC  A(01)      VALUE SPACE.


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
               02 MENS11               PIC X(47)         VALUE
              "DELTA = 0, EXISTE APENAS 01 RAIZ, POIS X1 = X2.".
               02 MENS12               PIC X(40)         VALUE
                  "DELTA > 0, EXISTEM 02 RAIZES DIFERENTES.".
               02 MENS13               PIC X(37)         VALUE
                   "Formula DELTA:  DELTA = (b)^2 -4*a*c.".
               02 MENS14               PIC X(32)         VALUE
                   "RAIZ:  X1 = ----------------  = ".
               02 MENS15               PIC X(32)         VALUE
                   "RAIZ:  X2 = ----------------  = ".
               02 MENS16               PIC X(29)         VALUE
                   "            -b + RAIZ(DELTA)".
               02 MENS17               PIC X(28)         VALUE
                   "                 2 * a      ".
               02 MENS18               PIC X(20)         VALUE
                          "Continua (S/N) ? < >".
               02 MENS19               PIC X(29)         VALUE
                   "            -b - RAIZ(DELTA)".



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
             DISPLAY       SPACE ERASE EOS     AT          0101.
             ACCEPT        DATA-DO-SISTEMA     FROM        DATE.
             PERFORM       PROCESSO  UNTIL CONTINUA = "N" OR "n".
             PERFORM       FINALIZA.



       PROCESSO.
             PERFORM       LIMPA-DADOS.
             MOVE          SPACE  TO CONTINUA.
             PERFORM       TELA.
             PERFORM       VALOR-DE-A UNTIL VALOR-A <> 0.
             PERFORM       VALOR-DE-B-C.
             PERFORM       CALCULA.
             PERFORM       RESULTADO.
             PERFORM       CONTINUA-PROG.
             EXIT.


       LIMPA-DADOS.
             MOVE          ZEROS               TO          VARIAVEIS.


       TELA.
             DISPLAY       TELA01              AT          0101.
             EXIT.



       ENTRA-DADOS.

            VALOR-DE-A.
             MOVE          ZEROS               TO          VALOR-A
             DISPLAY       MENS03              AT          0715
             ACCEPT        MASCARA-A           AT          0751
             MOVE          MASCARA-A           TO          VALOR-A
             IF            VALOR-A             =           0
             DISPLAY       MENS04              AT          0915
             END-IF.
             EXIT.


            VALOR-DE-B-C.
             DISPLAY       MENS07              AT          0915
             DISPLAY       MENS05              AT          0915
             ACCEPT        MASCARA-B           AT          0951
             MOVE          MASCARA-B           TO          VALOR-B
             DISPLAY       MENS06              AT          1115
             ACCEPT        MASCARA-C           AT          1151
             MOVE          MASCARA-C           TO          VALOR-C
             EXIT.


       CALCULA.

             COMPUTE  VALOR-DELTA = (VALOR-B * VALOR-B)
                                 - 4 *VALOR-A * VALOR-C.
             MOVE     VALOR-DELTA              TO        MASCARA-DELTA.


             COMPUTE RAIZ-X1 = (-VALOR-B + FUNCTION SQRT(VALOR-DELTA))
                               / (2 * VALOR-A).
             MOVE          RAIZ-X1             TO          MASCARA-X1.


             COMPUTE RAIZ-X2 = (-VALOR-B - FUNCTION SQRT(VALOR-DELTA))
                              / (2 * VALOR-A).
             MOVE          RAIZ-X2             TO          MASCARA-X2.


       RESULTADO.

             EVALUATE TRUE
             WHEN VALOR-DELTA < 0
             PERFORM       MOSTRAR-DELTA
             DISPLAY       MENS09              AT          1355
             WHEN VALOR-DELTA = 0
             PERFORM       MOSTRAR-DELTA
             DISPLAY       MENS11              AT          1355
             PERFORM       MOSTRAR-X1
             PERFORM       MOSTRAR-X2
             WHEN OTHER
             PERFORM       MOSTRAR-DELTA
             DISPLAY       MENS12              AT          1355
             PERFORM       MOSTRAR-X1
             PERFORM       MOSTRAR-X2
             END-EVALUATE.


            MOSTRAR-DELTA.
             DISPLAY       MENS08              AT          1315
             DISPLAY       MASCARA-DELTA       AT          1335
             EXIT.

            MOSTRAR-X1.
             DISPLAY       MENS16              AT          1515
             DISPLAY       MENS14              AT          1615
             DISPLAY       MENS17              AT          1715
             DISPLAY       MASCARA-X1          AT          1648
             EXIT.

            MOSTRAR-X2.
             DISPLAY       MENS19              AT          1915
             DISPLAY       MENS15              AT          2015
             DISPLAY       MENS17              AT          2115
             DISPLAY       MASCARA-X2          AT          2048
             EXIT.



       CONTINUA-PROG.
             DISPLAY       MENS18              AT          2420
             ACCEPT        CONTINUA            AT          2438
             EXIT.


       FINALIZA.
             DISPLAY       MENS10              AT          2620
             STOP " "
             STOP RUN.


           goback.


       end program EX04_EQ_2o_GRAU_ESTRU.

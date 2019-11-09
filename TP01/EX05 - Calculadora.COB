       program-id. EX05_CALCULADORA as "EX05_CALCULADORA".

       author. Grupo 01: Cintia Dias,Fabio Carcavali,Fernando Junior,
           Gabriel Azevedo,Julia Fernandes,Micael Cid,Rodolfo Feitosa.

       date-written. 25/09/2019.

      ******************************************************************
      * 5)    Programa 5 - Desenvolver um programa que receba
      *                  2 vari�veis num�ricas inteiras e informe:
      *                  soma, subtra��o, multiplica��o, divis�o.
      ******************************************************************


      *================================================================*
       environment division.
       CONFIGURATION SECTION.
           special-names.
            decimal-point is comma.
      *================================================================*
       data division.
      *----------------------------------------------------------------*
       working-storage section.
      *----------------------------------------------------------------*

       01 DADOS.
            02 VALOR01                 PIC  S9(04)V99      VALUE ZEROS.
            02 MASC01                  PIC  --.--9,99      VALUE ZEROS.
            02 VALOR02                 PIC  S9(04)V99      VALUE ZEROS.
            02 MASC02                  PIC  --.--9,99      VALUE ZEROS.
            02 OPCAO                   PIC   9(01)         VALUE ZEROS.
            02 RESULTADO               PIC  S9(08)V99      VALUE ZEROS.
            02 MASC-RESULT             PIC ---.---.--9,99  VALUE ZEROS.

       01 CONTINUA-OP.
            02 CONTINUA-S-N            PIC   A(01)      VALUE SPACE.


       01 MENSAGENS.
            02 MENS01                  PIC X(11)        VALUE
                             "CALCULADORA".
            02 MENS02                  PIC X(20)        VALUE
                        "DIGITE O 1o NUMERO: ".
            02 MENS03                  PIC X(20)        VALUE
                        "DIGITE O 2o NUMERO: ".
            02 MENS04                  PIC X(19)        VALUE
                        "DIGITE A OPCAO: < >".
            02 MENS05                  PIC X(10)        VALUE
                        "<1> SOMAR.".
            02 MENS06                 PIC X(13)         VALUE
                        "<2> SUBTRAIR.".
            02 MENS07                 PIC X(12)         VALUE
                        "<3> DIVIDIR.".
            02 MENS08                 PIC X(17)         VALUE
                        "<4> MULTIPLICAR.".
            02 MENS09                 PIC X(34)         VALUE
                    "DESEJA CONTINUAR (S OU N) ? : < >".
            02 MENS10                 PIC X(19)         VALUE
                           "OPCAO INVALIDA !!!!".
            02 MENS11                 PIC X(20)         VALUE SPACES.
            02 MENS12                 PIC X(11)         VALUE
                              "RESULTADO: ".
            02 MENS13                 PIC X(16)         VALUE
                           "FIM DO PROGRAMA.".



       01 DATA-DO-SISTEMA.
            02 ANO                     PIC 9(02)        VALUE ZEROS.
            02 MES                     PIC 9(02)        VALUE ZEROS.
            02 DIA                     PIC 9(02)        VALUE ZEROS.


      *----------------------------------------------------------------*
       screen section.
      *----------------------------------------------------------------*

       01 TELA01.
            02 BLANK SCREEN.
            02 LINE 02 COLUMN 05       PIC 9(02)/      USING DIA.
            02 LINE 02 COLUMN 08       PIC 9(02)/      USING MES.
            02 LINE 02 COLUMN 11       PIC 9(02)       USING ANO.
            02 LINE 02 COLUMN 28       PIC X(11)       USING MENS01.
            02 LINE 05 COLUMN 15       PIC X(20)       USING MENS02.
            02 LINE 07 COLUMN 15       PIC X(20)       USING MENS03.
            02 LINE 09 COLUMN 15       PIC X(19)       USING MENS04.
            02 LINE 10 COLUMN 15       PIC X(10)       USING MENS05.
            02 LINE 11 COLUMN 15       PIC X(13)       USING MENS06.
            02 LINE 12 COLUMN 15       PIC X(12)       USING MENS07.
            02 LINE 13 COLUMN 15       PIC X(16)       USING MENS08.
            02 LINE 15 COLUMN 15       PIC X(11)       USING MENS12.
            02 LINE 17 COLUMN 15       PIC X(34)       USING MENS09.



      *================================================================*
       procedure division.
      *================================================================*

       INICIO.
            ACCEPT       DATA-DO-SISTEMA     FROM            DATE.
            PERFORM      PROCESSO UNTIL CONTINUA-S-N = "N" OR "n".
            PERFORM      FINALIZA.


       PROCESSO.
            MOVE         ZEROS               TO           DADOS.
            MOVE         SPACES              TO           CONTINUA-OP.
            PERFORM      TELA.
            PERFORM      ENTRA-DADOS.
            PERFORM      ESCOLHA-OPCAO UNTIL OPCAO = 1 OR 2 OR 3 OR 4.
            PERFORM      CALCULA.
            PERFORM      CONTINUAR UNTIL
                         CONTINUA-S-N = "N" OR "n" OR "S" OR "s".



       TELA.
            DISPLAY       TELA01             AT              0101.
            EXIT.


       ENTRA-DADOS.

            ACCEPT        MASC01             AT              0535.
            MOVE          MASC01             TO              VALOR01.
            ACCEPT        MASC02             AT              0735.
            MOVE          MASC02             TO              VALOR02.


       ESCOLHA-OPCAO.
            ACCEPT        OPCAO              AT              0932.
            IF OPCAO < 1 OR OPCAO > 4
            DISPLAY       MENS10             AT              0935
            ELSE
            DISPLAY       MENS11             AT              0935
            END-IF.
            EXIT.



       CALCULA.

            EVALUATE TRUE
            WHEN OPCAO = 1
            ADD VALOR01 VALOR02 TO RESULTADO
            PERFORM MOSTRA-RESULTADO
            WHEN OPCAO = 2
            SUBTRACT VALOR02 FROM VALOR01 GIVING RESULTADO
            PERFORM MOSTRA-RESULTADO
            WHEN OPCAO = 3
            IF VALOR02 <> 0
            DIVIDE VALOR01 BY VALOR02 GIVING RESULTADO
            PERFORM MOSTRA-RESULTADO
            ELSE
            DISPLAY "NAO E POSSIVEL DIVIDIR POR ZERO"  AT  1530
            PERFORM CONTINUAR
            END-IF
            WHEN OPCAO = 4
            MULTIPLY VALOR01 BY VALOR02 GIVING RESULTADO
            PERFORM MOSTRA-RESULTADO
            END-EVALUATE.
            EXIT.


       MOSTRA-RESULTADO.
            MOVE          RESULTADO          TO        MASC-RESULT.
            DISPLAY       MASC-RESULT        AT        1530.


       CONTINUAR.
            MOVE          SPACES             TO        CONTINUA-S-N
            ACCEPT        CONTINUA-S-N       AT        1746 PROMPT AUTO
            EXIT.


       FINALIZA.
            DISPLAY       MENS13             AT       1921
            STOP " "
            STOP RUN.

           goback.

       end program EX05_CALCULADORA.

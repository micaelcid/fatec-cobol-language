       program-id. EX07_REAJUSTE_SAL as "EX07_REAJUSTE_SAL".

       author. Grupo 01: Cintia Dias,Fabio Carcavali,Fernando Junior,
           Gabriel Azevedo,Julia Fernandes,Micael Cid,Rodolfo Feitosa.

       date-written. 25/09/2019.

      ******************************************************************
      * 7)    Programa 7 � Go To:  Por que o programa anterior
      *                  � um programa que n�o devemos fazer?;
      *                  Fa�a o mesmo programa do jeito �mais correto�
      *                  de fazer.
      ******************************************************************
      *         o go to bangun�a as instru��es e consome  tempo de
      *          processamento.

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

       01 VARIAVEIS.
            02 NOME                    PIC A(30)       VALUE SPACES.
            02 IDADE                   PIC 9(02)       VALUE ZEROS.
            02 MASC-IDADE              PIC Z9.
            02 SEXO                    PIC A(01)       VALUE SPACE.
            02 SALARIO                 PIC 9(05)V99    VALUE ZEROS.
            02 MASC-SALARIO            PIC ZZ.ZZ9,99.
            02 CONTINUA                PIC X(01)       VALUE SPACE.


       01 MENSAGENS.
            02 MENS01                  PIC X(29)         VALUE
                      "CALULO DE AUMENTO DE SALARIO.".
            02 MENS02                  PIC X(23)         VALUE
                            "FUNCIONARIO.......:    ".
            02 MENS03                  PIC X(23)         VALUE
                            "IDADE.............:    ".
            02 MENS04                  PIC X(23)         VALUE
                            "SEXO..............:    ".
            02 MENS05                  PIC X(23)         VALUE
                            "SALARIO...........: R$ ".
            02 MENS06                  PIC X(23)         VALUE
                            "SALARIO REAJUSTADO: R$ ".
            02 MENS14                  PIC X(30)         VALUE
                       "DESEJA CONTINUAR (S / N): < >.".
            02 MENS08                  PIC X(58)         VALUE
         "PARA ESTE SALARIO NAO HA REAJUSTE, ENTRE COM NOVO SALARIO.".
            02 MENS09                  PIC X(24)         VALUE
                        "NOME INVALIDO, REDIGITE!".
            02 MENS10                  PIC X(60)         VALUE SPACES.
            02 MENS11                  PIC X(49)         VALUE
            "IDADE INVALIDA, DEVE SER ENTRE 15 A 29, REDIGITE!".
            02 MENS12                  PIC X(34)         VALUE
                       "OPCAO ERRADA, ECOLHA M, m, F ou f.".
            02 MENS13                  PIC X(16)         VALUE
                             "FIM DO PROGRAMA.".



       01 DATA-DO-SISTEMA.
            02 ANO                     PIC 9(04)       VALUE ZEROS.
            02 MES                     PIC 9(02)       VALUE ZEROS.
            02 DIA                     PIC 9(02)       VALUE ZEROS.



      *----------------------------------------------------------------*
       screen section.
      *----------------------------------------------------------------*

       01 TELA01.
            02 BLANK SCREEN.
            02 LINE 02 COLUMN 05       PIC 9(02)/      USING DIA.
            02 LINE 02 COLUMN 08       PIC 9(02)/      USING MES.
            02 LINE 02 COLUMN 11       PIC 9(04)       USING ANO.
            02 LINE 02 COLUMN 28       PIC X(29)       USING MENS01.
            02 LINE 05 COLUMN 15       PIC X(23)       USING MENS02.
            02 LINE 08 COLUMN 15       PIC X(23)       USING MENS03.
            02 LINE 11 COLUMN 15       PIC X(23)       USING MENS04.
            02 LINE 14 COLUMN 15       PIC X(23)       USING MENS05.
            02 LINE 17 COLUMN 15       PIC X(23)       USING MENS06.
            02 LINE 20 COLUMN 15       PIC X(31)       USING MENS14.


      *================================================================*
       procedure division.
      *================================================================*

       INICIO.
            ACCEPT DATA-DO-SISTEMA FROM DATE  YYYYMMDD.
            PERFORM PROCESSO UNTIL CONTINUA = "N" OR "n".
            PERFORM FINALIZA.
            STOP RUN.



       PROCESSO.
            MOVE    ZEROS              TO              VARIAVEIS
            MOVE    SPACE              TO              NOME
            MOVE    SPACE              TO              SEXO
            PERFORM MOSTRA-TELA
            PERFORM ENTRA-DADOS
            PERFORM CALCULA
            PERFORM RESULTADO
            PERFORM CONTINUA-PROG UNTIL
                      CONTINUA = "N" OR "n" OR "S" OR "s"
            EXIT.


       MOSTRA-TELA.
            DISPLAY    TELA01          AT              0101
            EXIT.



       ENTRA-DADOS.
            PERFORM    ENTRA-NOME      UNTIL       NOME <> " "
            PERFORM    ENTRA-IDADE
            PERFORM    ENTRA-SEXO UNTIL SEXO = "M" OR "m" OR "F" OR "f"
            PERFORM    ENTRA-SALARIO UNTIL SALARIO > 4998 AND < 50002
            EXIT.


            ENTRA-NOME.
              ACCEPT   NOME            AT              0538
              IF       NOME = " "
              DISPLAY  MENS09          AT              0638
              ELSE
              DISPLAY  MENS10          AT              0638
              END-IF
              EXIT.

             ENTRA-IDADE.
              ACCEPT   MASC-IDADE      AT              0838
              MOVE     MASC-IDADE      TO              IDADE
              EVALUATE IDADE
              WHEN 15 THRU 29
              DISPLAY  MENS10          AT              0938
              CONTINUE
              WHEN OTHER
              MOVE     ZERO            TO              IDADE
              MOVE     ZERO            TO              MASC-IDADE
              DISPLAY  MENS11          AT              0938
              PERFORM  ENTRA-IDADE
              END-EVALUATE
              EXIT.


             ENTRA-SEXO.
              ACCEPT   SEXO            AT              1138
              IF       SEXO  = "M" OR "m" OR "F" OR "f"
              DISPLAY  MENS10          AT              1238
              CONTINUE
              ELSE
              DISPLAY  MENS12          AT              1238
              MOVE     SPACES          TO              SEXO
              END-IF
              EXIT.

             ENTRA-SALARIO.
              ACCEPT   MASC-SALARIO    AT              1438
              MOVE     MASC-SALARIO    TO              SALARIO
              IF       SALARIO < 4999  OR  SALARIO > 50001
              DISPLAY  MENS08          AT              1538
              MOVE     ZEROS           TO              SALARIO
              MOVE     ZEROS           TO              MASC-SALARIO
              ELSE
              DISPLAY  MENS10          AT              1538
              END-IF
              EXIT.

       CALCULA.
              COMPUTE  SALARIO = SALARIO * 1,25.


       RESULTADO.
              MOVE     SALARIO         TO              MASC-SALARIO
              DISPLAY  MASC-SALARIO    AT              1738
              EXIT.


       CONTINUA-PROG.
              MOVE     SPACE           TO              CONTINUA
              ACCEPT   CONTINUA        AT              2042.
              EXIT.


       FINALIZA.
             DISPLAY   MENS13          AT              2220.
             STOP "  "
             EXIT.



           goback.

       end program EX07_REAJUSTE_SAL.

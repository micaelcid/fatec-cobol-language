       program-id. EX02_CUSTO_MERCADORIA as "EX02_CUSTO_MERCADORIA".

       author. Grupo 01: Cintia Dias,Fabio Carcavali,Fernando Junior,
           Gabriel Azevedo,Julia Fernandes,Micael Cid,Rodolfo Feitosa.

       date-written. 24/09/2019.

      ******************************************************************
      *2) Programa Aula4 - Elaborar um programa para calcular
      *                    o custo de mercadoria de uma distribuidora.
      ******************************************************************

      *================================================================*
       environment division.
      *================================================================*
       CONFIGURATION SECTION.
       special-names.
          decimal-point is comma.


       data division.
      *----------------------------------------------------------------*
       working-storage     section.
      *----------------------------------------------------------------*

       01 MERCADORIA.
               02 NOME-MERCADORIA      PIC A(20)         VALUE SPACES.
               02 PRECO-UNITARIO       PIC 9(04)V99      VALUE ZEROS.
               02 MASC-UNITARIO        PIC ZZ99.99      VALUE ZEROS.
               02 QUANTIDADE           PIC 9(04)         VALUE ZEROS.
               02 MASC-QT              PIC ZZ9           VALUE ZEROS.
               02 PRECO-TOTAL          PIC 9(06)V99      VALUE ZEROS.
               02 MASC-TOTAL           PIC ZZZ.ZZ9,99    VALUE ZEROS.
               02 PRECO-DE-VENDA       PIC 9(07)V99      VALUE ZEROS.
               02 MASC-VENDA           PIC Z.ZZZ.ZZ9,99  VALUE ZEROS.


       01 CONTROLE.
               02 CONTINUA             PIC A(01)         VALUE SPACE.


       01 MESAGENS-DE-TELA.
               02 MENS01               PIC X(30)       VALUE
                    "DISTRIBUIDORA DE PRODUTOS LTDA".
               02 MENS02               PIC X(20)       VALUE
                         "Nome da Mercadoria: ".
               02 MENS03               PIC X(12)       VALUE
                            "Quantidade: ".
               02 MENS04               PIC X(19)       VALUE
                      "Preco Unitario: R$ ".
               02 MENS05               PIC X(19)       VALUE
                        "Preco Total: R$ ".
               02 MENS06               PIC X(19)       VALUE
                      "Preco de Venda: R$ ".
               02 MENS07               PIC X(15)       VALUE
                            "FIM DO PROGRAMA".
               02 MENS08               PIC X(20)       VALUE
                          "Continua (S/N) ? < >".
               02 MENS09               PIC X(40)       VALUE SPACES.


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


      *================================================================*
       procedure                   division.
      *================================================================*

       INICIO.
            ACCEPT   DATA-DO-SISTEMA   FROM            DATE
            PERFORM  PROCESSO          UNTIL     CONTINUA ="N" OR "n"
            PERFORM  FINALIZA
            STOP RUN.


       PROCESSO.
            MOVE       ZEROS           TO              MERCADORIA
            MOVE       SPACES          TO              NOME-MERCADORIA
            MOVE       SPACE           TO              CONTINUA
            PERFORM    TELA
            PERFORM    ENTRADA
            PERFORM    CALCULA
            PERFORM    RESULTADO
            PERFORM    CONTINUA-PROG UNTIL
                     CONTINUA = "s" OR "S" OR "n" OR "N"
            EXIT.



       TELA.

            DISPLAY    TELA01          AT              0101.
            EXIT.


       ENTRADA.
            DISPLAY    MENS02                   AT    0515
            PERFORM    ENTRA-NOME
            PERFORM    ENTRA-QUANTIDADE
            PERFORM    ENTRA-PRECO
            EXIT.


       ENTRA-NOME.
            MOVE       SPACES                   TO    NOME-MERCADORIA
            ACCEPT     NOME-MERCADORIA          AT    0535
            IF NOME-MERCADORIA = " "
            DISPLAY "ENTRE COM NOME PARA PROCEGUIR!."  AT    0635
            PERFORM ENTRA-NOME
            ELSE
            DISPLAY     MENS09                         AT    0635
            END-IF
            EXIT.


       ENTRA-QUANTIDADE.
            MOVE       ZEROS                    TO     QUANTIDADE
            DISPLAY    MENS03                   AT     0715
            ACCEPT     MASC-QT                  AT     0727
            MOVE       MASC-QT                  TO     QUANTIDADE
            IF         QUANTIDADE = 0
            DISPLAY    "QUANTIDADE INVALIDA"    AT     0735
            PERFORM    ENTRA-QUANTIDADE
            ELSE
            DISPLAY    MENS09                   AT     0735
            END-IF
            EXIT.

       ENTRA-PRECO.
            MOVE       ZEROS                    TO     PRECO-UNITARIO
            DISPLAY    MENS04                   AT     0915
            ACCEPT     MASC-UNITARIO            AT     0934
            MOVE       MASC-UNITARIO            TO     PRECO-UNITARIO
            IF         PRECO-UNITARIO = 0
            DISPLAY    "PRECO INVALIDO"         AT     1035
            PERFORM    ENTRA-PRECO
            ELSE
            DISPLAY    MENS09                   AT     1035
            EXIT.


       CALCULA.
            COMPUTE    PRECO-TOTAL = QUANTIDADE * PRECO-UNITARIO
            COMPUTE    PRECO-DE-VENDA = PRECO-TOTAL * 1,30
            EXIT.


       RESULTADO.
            DISPLAY    MENS05          AT              1115.
            MOVE       PRECO-TOTAL     TO              MASC-TOTAL
            DISPLAY    MASC-TOTAL      AT              1131.
            DISPLAY    MENS06          AT              1315.
            MOVE       PRECO-DE-VENDA  TO              MASC-VENDA
            DISPLAY    MASC-VENDA      AT              1334.
            EXIT.


       CONTINUA-PROG.
            MOVE       SPACE           TO              CONTINUA
            DISPLAY    MENS08          AT              1515.
            ACCEPT     CONTINUA        AT              1533 PROMPT AUTO
            EXIT.


       FINALIZA.
            DISPLAY    MENS07          AT              1715
            STOP " "
            EXIT.



           goback.

       end program EX02_CUSTO_MERCADORIA.

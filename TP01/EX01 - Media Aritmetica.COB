       program-id. EX01_Media_Arimetica as "EX01_Media_Arimetica".

       author. Grupo 01: Cintia Dias,Fabio Carcavali,Fernando Junior,
           Gabriel Azevedo,Julia Fernandes,Micael Cid,Rodolfo Feitosa.

       date-written. 24/09/2019.

      ******************************************************************
      *1) Programa Aula4 - Calcular a m�dia aritm�tica das notas
      *                    bimestrais de um aluno.
      ******************************************************************


      *================================================================*
       environment division.
      *================================================================*
       CONFIGURATION SECTION.
           special-names.
       decimal-point is comma.

      ******************************************************************
      *No working-storage section. � o lugar onde vamos criar as
      *                            vari�veis e constantes.
      ******************************************************************
       data division.
       working-storage section.


       01 Dados.
               02 Nome-Aluno           PIC A(20)    VALUE SPACES.
               02 Nota1                PIC 9(02)V9  VALUE ZEROS.
               02 MASC-NOTA1           PIC Z9,9     VALUE ZEROS.
               02 Nota2                PIC 9(02)V9  VALUE ZEROS.
               02 MASC-NOTA2           PIC Z9,9     VALUE ZEROS.
               02 Nota-Exame           PIC 9(02)V9  VALUE ZEROS.
               02 MASC-NOTA-EX         PIC Z9,9     VALUE ZEROS.
               02 MEDIA                PIC 9(02)V9  VALUE ZEROS.
               02 MASC-MEDIA           PIC Z9,9     VALUE ZEROS.
               02 CONTINUA             PIC A(01)    VALUE SPACE.


       01 MENSAGENS-DE-TELA.
               02 MENSAGEM1            PIC X(24) VALUE
                     "Digite o nome do aluno: ".
               02 MENSAGEM2            PIC X(18) VALUE
                        "Digite a Nota P1: ".
               02 MENSAGEM3            PIC X(18) VALUE
                        "Digite a Nota P2: ".
               02 MENSAGEM4            PIC X(15) VALUE
                         "FIM DO PROGRAMA".
               02 MENSAGEM5            PIC X(17) VALUE
                         "Media das Notas: ".
               02 MENSAGEM6            PIC X(16) VALUE
                         "CALCULO DA MEDIA".
               02 MENSAGEM7            PIC X(09) VALUE
                            "O Aluno: ".
               02 MENSAGEM8            PIC X(45) VALUE
             "Esta de Exame!!! Sua media foi menor que 6,0.".
               02 MENSAGEM9            PIC X(45) VALUE SPACES.
               02 MENSAGEM10           PIC X(15) VALUE
                         "Nota de Exame: ".
               02 MENSAGEM11           PIC X(22) VALUE
                         "Nova Media das Notas: ".
               02 MENSAGEM12           PIC X(30) VALUE
                      "Continuar? (Digite S/N): < >".

       01 DATA-DO-SISTEMA.
               02 ANO                  PIC 9(02) VALUE ZEROS.
               02 MES                  PIC 9(02) VALUE ZEROS.
               02 DIA                  PIC 9(02) VALUE ZEROS.


      ******************************************************************
      *No screen section. � o lugar onde definimos as telas usadas
      *                   no programa.
      ******************************************************************
       screen section.

       01 TELA01.
               02 BLANK SCREEN.
               02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
               02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
               02 LINE 02 COLUMN 11 PIC 9(02)  USING ANO.
               02 LINE 02 COLUMN 28            USING MENSAGEM6.


      ******************************************************************
      *No procedure division. � o lugar onde definimos os m�todos
      *                       para execus�o do programa.
      ******************************************************************

       procedure division.

       INICIO.
            ACCEPT DATA-DO-SISTEMA FROM DATE.
            PERFORM   PROCESSO  UNTIL   CONTINUA = "n" OR "N".
            PERFORM FINALIZA.
            STOP RUN.


       PROCESSO.
            MOVE        ZEROS           TO  Dados
            MOVE        SPACES          TO  Nome-Aluno
            MOVE        SPACES          TO  CONTINUA
            PERFORM MOSTRA-TELA
            PERFORM ENTRADA
            PERFORM CALCULA
            PERFORM RESULTADO
            PERFORM CONTINUA-PROG UNTIL
                    CONTINUA = "s" OR "S" OR "n" OR "N"
            EXIT.


       MOSTRA-TELA.
            DISPLAY     TELA01          AT  0101
            EXIT.

       ENTRADA.
            DISPLAY     MENSAGEM1       AT  0515
            PERFORM     ENTRA-NOME  UNTIL  Nome-Aluno <> " "
            DISPLAY     MENSAGEM9       AT  0639
            DISPLAY     MENSAGEM2       AT  0715
            PERFORM     ENTRA-NOTA01
            DISPLAY     MENSAGEM9       AT  0745
            DISPLAY     MENSAGEM3       AT  0915
            PERFORM     ENTRA-NOTA02
            DISPLAY     MENSAGEM9       AT  0945
            EXIT.


       ENTRA-NOME.
           MOVE        SPACES          TO  Nome-Aluno
           ACCEPT      Nome-Aluno      AT  0539
           IF          Nome-Aluno = " "
           DISPLAY "Favor preencher o campo nome para proseguir" AT 0639
           END-IF
           EXIT.


       ENTRA-NOTA01.
            MOVE        ZEROS           TO  MASC-NOTA1
            ACCEPT      MASC-NOTA1      AT  0733
            MOVE        MASC-NOTA1      TO  Nota1
            EVALUATE    Nota1
            WHEN  0 THRU 10
            CONTINUE
            WHEN OTHER
            DISPLAY "Nota invalida, a nota de ser de 0 a 10." AT 0745
            PERFORM ENTRA-NOTA01
            END-EVALUATE
            EXIT.



       ENTRA-NOTA02.
            MOVE        ZEROS           TO  MASC-NOTA2
            MOVE        ZEROS           TO  Nota2
            ACCEPT      MASC-NOTA2      AT  0933
            MOVE        MASC-NOTA2      TO  Nota2
            EVALUATE    Nota2
            WHEN  0 THRU 10
            CONTINUE
            WHEN OTHER
            DISPLAY "Nota invalida, a nota de ser de 0 a 10." AT 0945
            PERFORM ENTRA-NOTA02
            END-EVALUATE
            EXIT.


       ENTRA-NOTA03.
            MOVE        ZEROS             TO  MASC-NOTA-EX
            MOVE        ZEROS             TO  Nota-Exame
            ACCEPT      MASC-NOTA-EX      AT  1533
            MOVE        MASC-NOTA-EX      TO  Nota-Exame
            EVALUATE    Nota-Exame
            WHEN  0 THRU 10
            CONTINUE
            WHEN OTHER
            DISPLAY "Nota invalida, a nota de ser de 0 a 10." AT 1545
            PERFORM ENTRA-NOTA03
            END-EVALUATE
            EXIT.


       CALCULA.
            COMPUTE     MEDIA = (Nota1 + Nota2) / 2
            EXIT.

       CALCULA2.
             IF Nota1 > Nota2
             COMPUTE   MEDIA = (Nota1 + NOTA-EXAME) / 2
             ELSE
             COMPUTE   MEDIA = (Nota2 + NOTA-EXAME) / 2
             EXIT.


       RESULTADO.
            DISPLAY     MENSAGEM5       AT  1115
            MOVE        MEDIA           TO  MASC-MEDIA
            DISPLAY     MASC-MEDIA      AT  1133
            EVALUATE    MEDIA
            WHEN 0 THRU 5,9
            DISPLAY     MENSAGEM7       AT  1315
            DISPLAY     Nome-Aluno      AT  1324
            DISPLAY     MENSAGEM8       AT  1345
            DISPLAY     MENSAGEM10      AT  1515
            PERFORM     ENTRA-NOTA03
            DISPLAY     MENSAGEM9       AT  1545
            PERFORM     CALCULA2
            DISPLAY     MENSAGEM11      AT  1715
            MOVE        MEDIA           TO  MASC-MEDIA
            DISPLAY     MASC-MEDIA      AT  1737
            IF MEDIA > 5,99
            DISPLAY    "ALUNO APROVADO" AT  1745
            ELSE
            DISPLAY   "ALUNO REPROVADO" AT  1745
            WHEN OTHER
            DISPLAY    "ALUNO APROVADO" AT  1141
            END-EVALUATE
            EXIT.


       CONTINUA-PROG.
              MOVE     SPACE           TO   CONTINUA
              DISPLAY  MENSAGEM12      AT   2015
              ACCEPT   CONTINUA        AT   2041 PROMPT AUTO.
              DISPLAY " " AT 8080
              WITH BLANK SCREEN
              EXIT.


       FINALIZA.
            DISPLAY     MENSAGEM4      AT  2215.
            STOP    " ".
            EXIT.


            goback.

       end program EX01_Media_Arimetica.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDICE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT PRODUTOS ASSIGN TO DISK
           ORGANIZATION INDEXED
           ACCESS MODE DYNAMIC
           RECORD KEY CODIG-P
           FILE STATUS ARQST.

       DATA DIVISION.

       FILE SECTION.
       FD  PRODUTOS LABEL RECORD STANDARD
           DATA RECORD IS REG-PROD
           VALUE OF FILE-ID IS "PRODUTOS.DAT".
       01  REG-PROD.
           02 CODIG-P PIC 9(4).
           02 NOME-P PIC X(30).
           02 QTDADE-P PIC 9(4).
           02 UNIT-P PIC 9(5)V99.
           02 TOTAL-P PIC 9(6)V99.

       WORKING-STORAGE SECTION.
       01 DATA-SIS.
           02 ANO PIC 99.
           02 MES PIC 99.
           02 DIA PIC 99.
       01 ARQST PIC X(2).
       01 OPCAO PIC X(1) VALUE SPACES.
       01 SALVA PIC X(1) VALUE SPACES.
       01 IGUAL PIC 9 VALUE ZEROS.
       01 ESPACO PIC X(30) VALUE SPACES.
       01 MENS1 PIC X(20) VALUE "FIM DE PROGRAMA".
       01 DADOS-EDITADOS.
           02 CODIGO PIC 9.999.
           02 NOME PIC X(30) VALUE SPACES.
           02 QT PIC 9.999.
           02 UNITARIO PIC $ZZ.ZZ9,99.
           02 TOT PIC $ZZZ.ZZ9,99.

       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.
           02 LINE 2 COL 23 VALUE "CONTROLE DE MERCADORIAS"
           FOREGROUND-COLOR 6.
           02 LINE 4 COL 19 VALUE "CODIGO DO PRODUTO:".
           02 LINE 6 COL 19 VALUE "NOME DO PRODUTO:".
           02 LINE 8 COL 19 VALUE "QUANTIDADE:".
           02 LINE 10 COL 19 VALUE "CUSTO UNITARIO:".
           02 LINE 12 COL 19 VALUE "CUSTO TOTAL:".
           02 LINE 15 COL 25 VALUE "MENSAGEM:".

       PROCEDURE DIVISION.
       INICIO.
           PERFORM ABRE-ARQ.
           PERFORM INCLUIR UNTIL OPCAO = "N" OR "n".
           DISPLAY MENS1 AT 1535.
           CLOSE PRODUTOS.
           STOP "".
           STOP RUN.

       ABRE-ARQ.
           OPEN I-O PRODUTOS.
           IF ARQST NOT = "00"
               CLOSE PRODUTOS
               OPEN OUTPUT PRODUTOS
               DISPLAY "ARQUIVO MODO LEITURA" AT 1635.

       INCLUIR.
           PERFORM ABERTURA.
           PERFORM RECEBE.
           PERFORM CONTINUA UNTIL OPCAO = "S" OR "N" OR "n" OR "s".

       ABERTURA.
           DISPLAY TELA.
           ACCEPT DATA-SIS FROM DATE.
           DISPLAY DIA AT 0205.
           DISPLAY "/" AT 0207.
           DISPLAY MES AT 0208.
           DISPLAY "/20" AT 0210.
           DISPLAY ANO AT 0213.

           MOVE SPACES TO OPCAO SALVA.
           MOVE SPACES TO NOME.
           MOVE ZEROS TO CODIGO QT UNITARIO TOT.
           MOVE SPACES TO NOME-P.
           MOVE ZEROS TO CODIG-P QTDADE-P UNIT-P TOTAL-P.
           DISPLAY ESPACO AT 1535.

       RECEBE.
           PERFORM TESTA-COD UNTIL CODIG-P > 0 AND IGUAL = 0.
           PERFORM TESTA-NOME UNTIL NOME-P NOT = SPACES.
           PERFORM TESTA-QT UNTIL QTDADE-P >= 10.
           PERFORM TESTA-CUSTO UNTIL UNIT-P > 0.
           PERFORM CALCULO-TOTAL.
           PERFORM GRAVA UNTIL SALVA = "S" OR = "N".

       TESTA-COD.
           SET IGUAL TO 0.
           ACCEPT CODIGO AT 0438 WITH PROMPT AUTO.
           MOVE CODIGO TO CODIG-P.
           IF CODIG-P = ZEROS
           THEN
               DISPLAY "CODIGO IGUAL ZERO" AT 1535
           ELSE
               READ PRODUTOS NOT INVALID KEY
               PERFORM JA-CADASTRADO
               END-READ
           END-IF.

       JA-CADASTRADO.
           DISPLAY ESPACO AT 1535.
           DISPLAY "JA CADASTRADO" AT 1535.
           SET IGUAL TO 1.

           MOVE SPACES TO NOME-P.
           MOVE ZEROS TO CODIG-P QTDADE-P UNIT-P TOTAL-P.

       TESTA-NOME.
           DISPLAY ESPACO AT 1535.
           ACCEPT NOME AT 0636 WITH PROMPT AUTO.
           MOVE NOME TO NOME-P.
           IF NOME-P = SPACES
               DISPLAY "DIGITE O NOME DO PRODUTO" AT 1535
           ELSE
               DISPLAY ESPACO AT 1535.

       TESTA-QT.
           ACCEPT QT AT 0831 WITH PROMPT AUTO.
           MOVE QT TO QTDADE-P.
           IF QTDADE-P < 10
               DISPLAY "QUANTIDADE MINIMA = 10" AT 1535
           ELSE
               DISPLAY ESPACO AT 1535.

       TESTA-CUSTO.
           ACCEPT UNITARIO AT 1035 WITH PROMPT AUTO.
           MOVE UNITARIO TO UNIT-P.
           IF UNIT-P = ZEROS
               DISPLAY "CUSTO INVALIDO" AT 1535
           ELSE
               DISPLAY ESPACO AT 1535.

       CALCULO-TOTAL.
           COMPUTE TOTAL-P = QTDADE-P * UNIT-P.
           MOVE TOTAL-P TO TOT.
           DISPLAY TOT AT 1232.

       GRAVA.
           DISPLAY "SALVAR (S/N)? [ ]" AT 1430.
           ACCEPT SALVA AT 1445 WITH PROMPT AUTO.
           IF SALVA = "S"
               WRITE REG-PROD INVALID KEY STOP RUN.

       CONTINUA.
           DISPLAY "CONTINUA (S/N)? [ ]" AT 1430.
           ACCEPT OPCAO AT 1447 WITH PROMPT AUTO.
           IF OPCAO = "S" OR = "N"
           THEN
               DISPLAY ESPACO AT 1430
               DISPLAY ESPACO AT 1535
           ELSE
               DISPLAY ESPACO AT 1535
               DISPLAY "DIGITE S OU N" AT 1535
           END-IF.
       END PROGRAM INDICE.

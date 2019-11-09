       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTO-MERCADORIA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-PRODUTO ASSIGN TO DISK
           ORGANIZATION LINE SEQUENTIAL
           ACCESS MODE SEQUENTIAL
      *     RECORD KEY CODIGO
           FILE STATUS ARQ-OK.
       DATA DIVISION.
       FILE SECTION.
           FD  ARQ-PRODUTO LABEL RECORD STANDARD
           DATA RECORD IS REG-PROD
           VALUE OF FILE-ID IS "PRODUTO.DAT".

           01  REG-PROD.
               02 CODIGO           PIC 9(4).
               02 MERCADORIA       PIC X(30).
               02 PRECO-UNITARIO   PIC 9(5)V99.
               02 PRECO-TOTAL      PIC 9(5)V99.
               02 QUANTIDADE       PIC 9(4).

       WORKING-STORAGE SECTION.
       01  DATA-SIS.
           02 ANO PIC 99.
           02 MES PIC 99.
           02 DIA PIC 99.

       01  ARQ-OK      PIC X(02).
       01  OPCAO       PIC X VALUE SPACES.
       01  OPCAO-INIC   PIC X VALUE SPACES.
       01  CONTINUAx   PIC X VALUE SPACE.
       01  SALVA       PIC X VALUE SPACE.
       01  IGUAL       PIC 9 VALUE ZEROS.
       01  ESPACO      PIC X(30) VALUE SPACES.
       01  MENS1       PIC X(20) VALUE "FIM DE PROGRAMA".

       01  DADOS-EDITADOS.
           02 CODIGO-L         PIC 9.999.
           02 MERCADORIA-L     PIC X(30) VALUE SPACES.
           02 PRECO-UNITARIO-L PIC $ZZ.ZZ9,99.
           02 PRECO-TOTAL-L    PIC $ZZZ.ZZ9,99.
           02 QUANTIDADE-L     PIC 9.999.



       SCREEN SECTION.
       01  TELA.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 15 VALUE "CONTROLE DE ESTOQUE".
           02 LINE 02 COLUMN 45 VALUE "  /  /20  ".
           02 LINE 05 COLUMN 02 VALUE "Codigo do produto:".
           02 LINE 07 COLUMN 02 VALUE "Nome do produto:".
           02 LINE 09 COLUMN 02 VALUE "Quantidade:".
           02 LINE 11 COLUMN 02 VALUE "Preco unitario: ".
           02 LINE 13 COLUMN 02 VALUE "Preco total: ".
           02 LINE 20 COLUMN 20 VALUE "Salvar (S/N)? [ ]".



       PROCEDURE DIVISION.
       INICIO.
           PERFORM ABRE-ARQ.
           PERFORM INCLUIR UNTIL OPCAO = "S" OR "N" OR "n" OR "s".
           DISPLAY MENS1 AT 2535.
           CLOSE ARQ-PRODUTO.
           STOP "".
           STOP RUN.

       ABRE-ARQ.
           OPEN EXTEND ARQ-PRODUTO.
           IF ARQ-OK NOT = "00"
               CLOSE ARQ-PRODUTO
               OPEN OUTPUT ARQ-PRODUTO
               DISPLAY "IMPOSSIVEL SALVAR" AT 2520
               CALL "C$SLEEP" USING 2.
       INCLUIR.
           PERFORM ABERTURA.
           PERFORM RECEBE.
           PERFORM CONTINUA UNTIL OPCAO = "S" OR "s" OR "N" OR "n".
           IF OPCAO = "S" OR "s"
               THEN
                   PERFORM INCLUIR
           ELSE
                   MOVE OPCAO TO OPCAO-INIC.

       ABERTURA.
           DISPLAY TELA.
           ACCEPT DATA-SIS FROM DATE.
           DISPLAY DIA AT 0245.
           DISPLAY MES AT 0248.
           DISPLAY ANO AT 0253.

           MOVE SPACES TO OPCAO SALVA.
           MOVE ZEROS TO CODIGO.
           MOVE ZEROS TO QUANTIDADE.
           MOVE ZEROS TO PRECO-TOTAL.
           MOVE ZEROS TO PRECO-UNITARIO.
           MOVE SPACES TO MERCADORIA.
           DISPLAY ESPACO AT 1535.

       RECEBE.
           PERFORM TESTA-COD UNTIL CODIGO > 0.
           PERFORM TESTA-NOME UNTIL MERCADORIA NOT = SPACES.
           PERFORM TESTA-QT UNTIL QUANTIDADE NOT = ZEROS.
           PERFORM TESTA-UNIT UNTIL PRECO-UNITARIO NOT = ZEROS.
           PERFORM CALCULO.
           PERFORM GRAVA UNTIL SALVA = "S" OR "N" OR "n" OR "s".


       TESTA-COD.
           SET IGUAL TO 0.
           ACCEPT CODIGO AT 0522 WITH PROMPT AUTO.
           MOVE CODIGO TO CODIGO-L.
           IF CODIGO-L = ZEROS
           THEN
               DISPLAY "CODIGO IGUAL A ZERO" AT 2510
           ELSE
               DISPLAY CODIGO-L AT 0522
           END-IF.

       TESTA-NOME.
           ACCEPT MERCADORIA AT 0719 WITH PROMPT AUTO.
           MOVE MERCADORIA TO MERCADORIA-L.
           IF MERCADORIA-L = SPACES
               DISPLAY "DIGITE O NOME DO PRODUTO" AT 2510

           ELSE
               DISPLAY MERCADORIA-L AT 0719.

       TESTA-QT.
           ACCEPT QUANTIDADE AT 0914 WITH PROMPT AUTO.
           MOVE QUANTIDADE TO QUANTIDADE-L.
           IF QUANTIDADE-L = ZEROS
               DISPLAY "QUANTIDADE INVALIDA" AT 2510
           ELSE
               DISPLAY QUANTIDADE-L AT 0914.

       TESTA-UNIT.
           ACCEPT PRECO-UNITARIO AT 1120.
           MOVE PRECO-UNITARIO TO PRECO-UNITARIO-L.
           IF PRECO-UNITARIO-L = ZEROS
               DISPLAY "PRECO INVALIDO" AT 2510
           ELSE
               DISPLAY PRECO-UNITARIO-L AT 1120.

       CALCULO.
           COMPUTE PRECO-TOTAL = QUANTIDADE * PRECO-UNITARIO.
           MOVE PRECO-TOTAL TO PRECO-TOTAL-L.
           DISPLAY PRECO-TOTAL-L AT 1317.


       GRAVA.
           ACCEPT SALVA AT 2035 WITH PROMPT AUTO.
           IF SALVA = "S" OR "N"
               MOVE CODIGO-L TO CODIGO
               MOVE MERCADORIA-L TO MERCADORIA
               MOVE PRECO-UNITARIO-L TO PRECO-UNITARIO
               MOVE PRECO-TOTAL-L TO PRECO-TOTAL
               MOVE PRECO-TOTAL-L TO PRECO-TOTAL
               MOVE QUANTIDADE-L TO QUANTIDADE
               DISPLAY "INCLUSAO REALIZADA COM SUCESSO" AT 2410
               CALL "C$SLEEP" USING 2
               WRITE REG-PROD.

       CONTINUA.
           DISPLAY "Continua (S/N)?" AT 2220.
           ACCEPT OPCAO AT 2237 WITH PROMPT AUTO.


       END PROGRAM CUSTO-MERCADORIA.

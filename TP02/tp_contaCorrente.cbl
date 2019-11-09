       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCOPEL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAD ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS FFSALV
               RECORD KEY IS CD-CPF.
       DATA DIVISION.
       FILE SECTION.
       FD CAD VALUE OF FILE-ID IS "Agencia.dat".

       01 CADASTRO-REGISTRO.
           02 CD-CPF       PIC 9(11) VALUE ZEROS.
           02 NM-USER      PIC X(35).
           02 DS-ENDER     PIC X(45).
           02 CD-CONTA     PIC 9(5).
           02 CD-AGENCIA   PIC 9(3).
           02 VL-DEP       PIC 9(7).
           02 VL-SAQUE     PIC 9(7).
           02 VL-SALDO     PIC 9(8).

       WORKING-STORAGE SECTION.
       01 FS-SDATA.
           05 SDATADMA.
               10 SDATTEMPO.
                   15 FS-SANO       PIC  9(4).
                   15 FS-SMES       PIC  9(2).
                   15 FS-SDIA       PIC  9(2).
               10 SDATAHORA.
                   15 FS-SHORA      PIC  9(2).
                   15 FS-SMIN       PIC  9(2).
                   15 FS-SSEGS      PIC  9(2).
               10 FS-SMMS           PIC  9(4).

       01 FS-SFORDATA.
           05 SFORDATTEMP.
               15 FS-FDIA            PIC  9(2).
               15 FILLER             PIC X VALUE '/'.
               15 FS-FMES            PIC  9(2).
               15 FILLER             PIC X VALUE '/'.
               15 FS-FANO            PIC  9(4).

           05 SFORHORA.
               15 FS-FHORA           PIC  9(2).
               15 FILLER             PIC X VALUE ':'.
               15 FS-FMNTO           PIC  9(2).
               15 FILLER             PIC X VALUE ':'.
               15 FS-FSGNDO          PIC  9(2).

           01 MSG.
               02 MSG-OP PIC X.
                   88 MSG-OP-N VALUE 'N' 'n'.
                   88 MSG-OP-S VALUE 'S' 's'.

       01 FS-MOVIMENTO.
           02 FDEPOS     PIC 9(7) VALUE ZEROS.
           02 FSAQU      PIC 9(7) VALUE ZEROS.
           02 FSALDO     PIC S9(8) VALUE ZEROS.

       77 FS-MSGOP PIC X.
           88 MSAINCLUIR   VALUE IS "1".
           88 MSCONSUL     VALUE IS "2".
           88 MSMUDAR      VALUE IS "3".
           88 MSAPAGA      VALUE IS "4".
           88 MSSAQU       VALUE IS "6".
           88 MSDEP        VALUE IS "8".
           88 MSSAIR       VALUE IS "9".

       77 FFSALV PIC 9(02).
           88 FS-OK        VALUE 00.
           88 FS-EMPTY     VALUE 35.
           88 FS-OTHER     VALUE 99.

       77 MS-GMENSAGEMS PIC X(80).

       COPY screenio.

       SCREEN SECTION.
       01 EE-LIMPA
           BLANK SCREEN.

       01 ES-MENU.
           02 LINE 01 COLUMN 25 VALUE ':::::::::::::::::::::::::::::'-
           ':::::::::::::::::::::::::::'.
           02 LINE 02 COLUMN 25 VALUE '::                             '-
                                      '                       ::'.
           02 LINE 03 COLUMN 25 VALUE '::              B A N C O      '-
           ' P E L A D O           ::'.
           02 LINE 04 COLUMN 25 VALUE '::                             '-
                                      '                       ::'.
           02 LINE 05 COLUMN 25 VALUE '::::::::::::::::::::::::::::::'-
           '::::::::::::::::::::::::::'.
           02 LINE 06 COLUMN 25 VALUE '::                             '-
           '                       ::'.
           02 LINE 07 COLUMN 25 VALUE  '::                 '-
           '                                   ::'.
           02 LINE 08 COLUMN 25 VALUE '::..................... M E '-
           'N U ......................::'.
           02 LINE 09 COLUMN 25 VALUE '::                             '-
                   '                       ::'.
           02 LINE 10 COLUMN 25 VALUE '::     CADASTRO     '-
           '            ATIVIDADE             ::'.
           02 LINE 11 COLUMN 25 VALUE '::                         '-
           '     FINANCEIRA            ::'.
           02 LINE 12 COLUMN 25 VALUE '::  1 - INCLUIR  '-
           '                                     ::'.
           02 LINE 13 COLUMN 25 VALUE '::  2 - CONSULTAR   '-
           '         6 - DEPOSITAR            ::'.
           02 LINE 14 COLUMN 25 VALUE '::  3 - ALTERAR   '-
           '           8 - SACAR                ::'.
           02 LINE 15 COLUMN 25 VALUE '::  4 - EXCLUIR            '-
           '                           ::'.
           02 LINE 16 COLUMN 25 VALUE '::                '-
           '                                    ::'.
           02 LINE 17 COLUMN 25 VALUE '::                '-
           '                                    ::'.
           02 LINE 17 COLUMN 25 VALUE '::                   '-
           '        9 - ENCERRAR             ::'.
            02 LINE 18 COLUMN 25 VALUE '::               '-
           '                                     ::'.
            02 LINE 19 COLUMN 25 VALUE '::               '-
           '                                     ::'.
           02 LINE 19 COLUMN 25 VALUE ':::::::::::::::::::::::::'-
           ':::::::::::::::::::::::::::::::'.
           02 LINE 18 COLUMN 25 VALUE ":: ESCOLHA A OPCAO ".
           02 LINE 18 COL PLUS 1 USING FS-MSGOP AUTO.

       01 FF-TLCPF.
           02 ES-DCPF.
               03 LINE 09 COLUMN 29 VALUE 'CPF:'.
               03 LINE 10 COLUMN 29 PIC 9(11) USING CD-CPF
               BLANK WHEN ZEROS.

           02 ES-DNOME.
               03 LINE 11 COLUMN 29 VALUE 'NOME:'.
               03 LINE 12 COLUMN 29 USING NM-USER.

           02 ES-DENDER.
               03 LINE 13 COLUMN 29 VALUE 'ENDERECO:'.
               03 LINE 14 COLUMN 29 USING DS-ENDER.

           02 ES-DAGENCIA.
               03 LINE 15 COLUMN 29 VALUE 'AGENCIA:'.
               03 LINE 16 COLUMN 29 PIC 9(3) USING CD-AGENCIA
               BLANK WHEN ZEROS.

           02 ES-DCONTA.
               03 LINE 17 COLUMN 29 VALUE 'CONTA:'.
               03 LINE 18 COLUMN 29 PIC 9(5) USING CD-CONTA
               BLANK WHEN ZEROS.

           02 ES-MOVIMENTA.
               03 LINE 14 COLUMN 05 VALUE "DEPOSITO: R$ ".
               03 LINE 14 COLUMN 25 PIC 9(7) USING FDEPOS
               BLANK WHEN ZEROS.
               03 LINE 15 COLUMN 05 VALUE "SAQUE: R$ ".
               03 LINE 15 COLUMN 25 PIC 9(7) USING FSAQU
               BLANK WHEN ZEROS.
               03 LINE 17 COLUMN 05 VALUE "SALDO: R$ ".
               03 LINE 17 COLUMN 25 PIC 9(8) USING VL-SALDO
               BLANK WHEN ZEROS.

       PROCEDURE DIVISION.
       EE-FTELA.
           DISPLAY ':::::::::::::::::::::::::::::'-
           ':::::::::::::::::::::::::::' AT 0125
           DISPLAY '::                                               '-
           '     ::' AT 0225
           DISPLAY '::              B A N C O    '-
           ' P E L A D O             ::' AT 0325
           DISPLAY '::                                               '-
           '     ::' AT 0425
           DISPLAY ':::::::::::::::::::::::::::::'-
           ':::::::::::::::::::::::::::' AT 0525
           DISPLAY '::                                               '-
           '     ::' AT 0625
           DISPLAY '::                                               '-
           '     ::' AT 0725
           DISPLAY '::                                               '-
           '     ::' AT 0825
           DISPLAY '::                                               '-
           '     ::' AT 1025
           DISPLAY '::                                               '-
           '     ::' AT 1225
           DISPLAY '::                                               '-
           '     ::' AT 1425
           DISPLAY '::                                               '-
           '     ::' AT 0925
           DISPLAY '::                                               '-
           '     ::' AT 1125
           DISPLAY '::                                               '-
           '     ::' AT 1325
           DISPLAY '::                                               '-
           '     ::' AT 1525
           DISPLAY '::                                               '-
           '     ::' AT 1625
           DISPLAY '::                                               '-
           '     ::' AT 1725
           DISPLAY '::                                               '-
           '     ::' AT 1825
           DISPLAY '::                                               '-
           '     ::' AT 1925
           DISPLAY '::                                               '-
           '     ::' AT 2025
           DISPLAY ':::::::::::::::::::::::::::::'-
           ':::::::::::::::::::::::::::' AT 2125.

       INICIO.
           MOVE FUNCTION CURRENT-DATE TO SDATADMA
           MOVE FS-SHORA   TO FS-FHORA
           MOVE FS-SMIN    TO FS-FMNTO
           MOVE FS-SSEGS   TO FS-FSGNDO
           MOVE FS-SANO    TO FS-FANO
           MOVE FS-SMES    TO FS-FMES
           MOVE FS-SDIA    TO FS-FDIA

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.


           PERFORM ABREARQ
           PERFORM UNTIL MSSAIR
               MOVE SPACES TO FS-MSGOP
               DISPLAY EE-LIMPA
               DISPLAY SFORDATTEMP AT 2525
               DISPLAY SFORHORA AT 2573
               ACCEPT ES-MENU
               EVALUATE TRUE
                   WHEN MSAINCLUIR
                       PERFORM ADCIONA THRU END-ADCIONA
                   WHEN MSCONSUL
                       PERFORM PESQUISA THRU END-PESQUISA
                   WHEN MSMUDAR
                       PERFORM ALTERA THRU END-ALTERA
                   WHEN MSAPAGA
                       PERFORM EXCLUI THRU END-EXCLUI
                   WHEN MSSAQU
                       PERFORM DEPOSITO THRU END-DEPOSITO
                   WHEN MSDEP
                       PERFORM SAQUE THRU END-SAQUE
               END-EVALUATE
           END-PERFORM.
       FINALIZA.
           CLOSE CAD.
           STOP RUN.

       ADCIONA.

       ADCIONAW.
           DISPLAY EE-LIMPA.
           MOVE SPACES TO CADASTRO-REGISTRO.

           PERFORM EE-FTELA
           DISPLAY 'N O V O  C L I E N T E ' AT 0642
           DISPLAY 'INFORME CPF' AT 0729
           DISPLAY "ESC -> MENU" AT 0202
           DISPLAY SFORDATTEMP AT 2525
           DISPLAY SFORHORA AT 2573

           ACCEPT ES-DCPF.
           IF COB-CRT-STATUS = COB-SCR-ESC
               GO END-ADCIONA
           END-IF

           IF CD-CPF IS < 1
               MOVE "INFORME O CPF" TO MS-GMENSAGEMS
               DISPLAY MS-GMENSAGEMS AT 2330
               FOREGROUND-COLOR 7
               ACCEPT MS-GMENSAGEMS AT 2360 FOREGROUND-COLOR 0
               GO ADCIONAW
           END-IF
           ACCEPT ES-DNOME.
           IF COB-CRT-STATUS = COB-SCR-ESC
               GO END-ADCIONA
           END-IF
            IF NM-USER EQUAL SPACES
               MOVE "INFORME O NOME" TO MS-GMENSAGEMS
               DISPLAY MS-GMENSAGEMS AT 2330
               FOREGROUND-COLOR 7
               ACCEPT MS-GMENSAGEMS AT 2365 FOREGROUND-COLOR 0
               GO ADCIONAW
           END-IF
           ACCEPT ES-DENDER.
           IF COB-CRT-STATUS = COB-SCR-ESC
               GO END-ADCIONA
           END-IF
           IF DS-ENDER EQUAL SPACES
               MOVE "INFORME O ENDERECO" TO MS-GMENSAGEMS
               DISPLAY MS-GMENSAGEMS AT 2330
               FOREGROUND-COLOR 7
               ACCEPT MS-GMENSAGEMS AT 2365 FOREGROUND-COLOR 0
               GO ADCIONAW
           END-IF
           ACCEPT ES-DAGENCIA.
           IF COB-CRT-STATUS = COB-SCR-ESC
               GO END-ADCIONA
           END-IF
           IF CD-AGENCIA EQUAL ZEROS
               MOVE "INFORME A AGENCIA" TO MS-GMENSAGEMS
               DISPLAY MS-GMENSAGEMS AT 2330
               FOREGROUND-COLOR 7
               ACCEPT MS-GMENSAGEMS AT 2365 FOREGROUND-COLOR 0
               GO ADCIONAW
           END-IF
           ACCEPT ES-DCONTA.
           IF COB-CRT-STATUS = COB-SCR-ESC
               GO END-ADCIONA
           END-IF
           IF CD-CONTA EQUAL ZEROS
               MOVE "INFORME A CONTA" TO MS-GMENSAGEMS
               DISPLAY MS-GMENSAGEMS AT 2330
               FOREGROUND-COLOR 7
               ACCEPT MS-GMENSAGEMS AT 2365 FOREGROUND-COLOR 0
               GO ADCIONAW
           END-IF

           WRITE CADASTRO-REGISTRO

           INVALID KEY
               MOVE "CLIENTE JA CADASTRADO" TO MS-GMENSAGEMS
               DISPLAY MS-GMENSAGEMS AT 2320
               FOREGROUND-COLOR 7
               ACCEPT MS-GMENSAGEMS AT 2346 FOREGROUND-COLOR 0
               MOVE ZEROS TO CD-CPF

           NOT INVALID KEY
               DISPLAY "CADASTRO REALIZADO" AT 1659
               DISPLAY "COM SUCESSO" AT 1763
               BACKGROUND-COLOR 0 FOREGROUND-COLOR 3
               ACCEPT MS-GMENSAGEMS AT 2325 FOREGROUND-COLOR 0
               GO ADCIONAW

           END-WRITE
           GO ADCIONA.
       END-ADCIONA.

       PESQUISA.

       PESQUISA-GERAL.

           DISPLAY EE-LIMPA.
           IF COB-CRT-STATUS = COB-SCR-ESC
               GO END-PESQUISA
           END-IF
           MOVE SPACES TO CADASTRO-REGISTRO.

           PERFORM EE-FTELA
           DISPLAY 'C O N S U L T A' AT 0645
           DISPLAY 'INFORME CPF' AT 0729
           DISPLAY "ESC -> MENU" AT 0202
           DISPLAY SFORDATTEMP AT 2525
           DISPLAY SFORHORA AT 2573

           DISPLAY ES-DCPF.
           PERFORM LUSUARIO THRU END-LUSUARIO.

           IF FS-OTHER
               GO END-PESQUISA
           END-IF

           IF FS-OK
               DISPLAY ES-DNOME
               DISPLAY ES-DENDER
               DISPLAY ES-DAGENCIA
               DISPLAY ES-DCONTA
               DISPLAY "SALDO     R$ " AT 1929 VL-SALDO
               MOVE "ENTER CONTINUA" TO MS-GMENSAGEMS
               DISPLAY MS-GMENSAGEMS AT 2326
               FOREGROUND-COLOR 7
               ACCEPT MS-GMENSAGEMS AT 2341 FOREGROUND-COLOR 0
           END-IF.

           GO PESQUISA-GERAL.

       END-PESQUISA.

       ALTERA.

       ALTERAW.
           DISPLAY EE-LIMPA.
           MOVE SPACES TO CADASTRO-REGISTRO.

           PERFORM EE-FTELA
           DISPLAY 'A L T E R A  C A D A S T R O ' AT 0642
           DISPLAY 'INFORME CPF' AT 0729
           DISPLAY "ESC -> MENU" AT 0202
           DISPLAY SFORDATTEMP AT 2525
           DISPLAY SFORHORA AT 2573

           DISPLAY ES-DCPF.
           PERFORM LUSUARIO THRU END-LUSUARIO.
           IF FS-OTHER
               GO END-ALTERA
           END-IF

           IF FS-OK
               ACCEPT ES-DNOME
               IF COB-CRT-STATUS = COB-SCR-ESC
                   GO ALTERAW
           END-IF

           IF FS-OK
               ACCEPT ES-DENDER
               IF COB-CRT-STATUS = COB-SCR-ESC
                   GO ALTERAW
           END-IF

           MOVE "ALTERACOES CONCLUIDAS" TO MS-GMENSAGEMS
           DISPLAY MS-GMENSAGEMS AT 2330
           FOREGROUND-COLOR 7
           ACCEPT MS-GMENSAGEMS AT 2360 FOREGROUND-COLOR 0
           REWRITE CADASTRO-REGISTRO
           END-REWRITE.
           GO ALTERAW.
       END-ALTERA.

       EXCLUI.

       EXCLUIW.
           DISPLAY EE-LIMPA.
           MOVE SPACES TO CADASTRO-REGISTRO.

           PERFORM EE-FTELA
           DISPLAY 'A P A G A R  C L I E N T E' AT 0642
           DISPLAY 'INFORME CPF' AT 0729
           DISPLAY "ESC -> MENU" AT 0202
           DISPLAY SFORDATTEMP AT 2525
           DISPLAY SFORHORA AT 2573

           DISPLAY ES-DCPF.

           PERFORM LUSUARIO THRU END-LUSUARIO.

           IF FS-OTHER
               GO END-EXCLUI
           END-IF

           IF NOT FS-OK
               GO EXCLUI
           END-IF

               DISPLAY ES-DNOME
               DISPLAY ES-DENDER
               DISPLAY ES-DAGENCIA
               DISPLAY ES-DCONTA
           DISPLAY "SALDO   R$ " AT 2029 VL-SALDO
           MOVE "DESEJA EXCLUIR? (S/N) " TO MS-GMENSAGEMS
           DISPLAY MS-GMENSAGEMS AT 2330
           ACCEPT MS-GMENSAGEMS AT 2351

           IF MS-GMENSAGEMS EQUAL "S" OR MS-GMENSAGEMS EQUAL "s"
               DELETE CAD
               END-DELETE
           IF COB-CRT-STATUS = COB-SCR-ESC
                   GO EXCLUIW
           END-IF

           GO EXCLUIW.
       END-EXCLUI.

       LUSUARIO.
           ACCEPT ES-DCPF.
           IF NOT COB-CRT-STATUS = COB-SCR-ESC
               READ CAD
                   INVALID KEY
                       MOVE "CLIENTE SEM CADASTRO ATIVO" TO
                       MS-GMENSAGEMS
                   DISPLAY MS-GMENSAGEMS AT 2326
                   ACCEPT MS-GMENSAGEMS AT 2357
                   GO END-LUSUARIO
               END-READ
           ELSE
               MOVE 99 to FFSALV

           END-IF.
       END-LUSUARIO.

       LINFOS.
           ACCEPT ES-DAGENCIA.
           IF NOT COB-CRT-STATUS = COB-SCR-ESC
*               READ CAD
                       INVALID KEY
                           MOVE "AGENCIA E CONTAS ATIVAS" TO
                             MS-GMENSAGEMS
                      DISPLAY MS-GMENSAGEMS AT 2330
                      ACCEPT MS-GMENSAGEMS AT 2354
               END-READ
           ELSE
               MOVE 99 TO FFSALV
           END-IF.
       END-LINFOS.

       ABREARQ.
           OPEN I-O CAD
           IF FS-EMPTY THEN
               OPEN OUTPUT CAD
               CLOSE CAD
               OPEN I-O CAD
           END-IF.


      *     /// ADICIONAIS //
       DEPOSITO.
       DEPOSITOW.
           DISPLAY EE-LIMPA.
           MOVE SPACES TO CADASTRO-REGISTRO.

           PERFORM EE-FTELA
           DISPLAY 'D E P O S I T O' AT 0645
           DISPLAY 'INFORME CPF' AT 0729
           DISPLAY "ESC -> MENU" AT 0202
           DISPLAY SFORDATTEMP AT 2525
           DISPLAY SFORHORA AT 2573

           PERFORM LUSUARIO THRU END-LUSUARIO.

           IF FS-OTHER
               GO END-DEPOSITO
           END-IF

           IF FS-OK
               DISPLAY ES-DNOME
               DISPLAY ES-DENDER
               DISPLAY ES-DAGENCIA
               DISPLAY ES-DCONTA
               DISPLAY "DEPOSITO  R$ " AT 2029
               DISPLAY "SALDO     R$ " AT 2055 VL-SALDO
               MOVE "PRESSIONE ENTER" TO MS-GMENSAGEMS
               ACCEPT FDEPOS AT 2045
                   IF COB-CRT-STATUS = COB-SCR-ESC
                       GO DEPOSITOW
                   END-IF

               ADD FDEPOS TO FSALDO GIVING FSALDO
               MOVE FSALDO TO VL-SALDO
               DISPLAY "SALDO: R$    " AT 2055 VL-SALDO
               MOVE "ENTER PARA CONTINUAR..." TO MS-GMENSAGEMS
               DISPLAY MS-GMENSAGEMS AT 2330
               FOREGROUND-COLOR 7
               ACCEPT MS-GMENSAGEMS AT 2365 FOREGROUND-COLOR 0
           END-IF.
           REWRITE CADASTRO-REGISTRO
           END-REWRITE.
           GO DEPOSITOW.
       END-DEPOSITO.

       SAQUE.
       SAQUEW.

           DISPLAY EE-LIMPA.
           MOVE SPACES TO CADASTRO-REGISTRO.

           PERFORM EE-FTELA
           DISPLAY 'S A C A R' AT 0650
           DISPLAY 'INFORME CPF' AT 0729
           DISPLAY "ESC -> MENU" AT 0202
           DISPLAY SFORDATTEMP AT 2525
           DISPLAY SFORHORA AT 2573

           PERFORM LUSUARIO THRU END-LUSUARIO.

           IF FS-OTHER
               GO END-SAQUE
           END-IF

           IF FS-OK
               DISPLAY ES-DNOME
               DISPLAY ES-DENDER
               DISPLAY ES-DAGENCIA
               DISPLAY ES-DCONTA
               DISPLAY "SAQUE  R$ " AT 2029
               DISPLAY "SALDO R$ " AT 2055  VL-SALDO
               ACCEPT FSAQU AT 2039
                   IF COB-CRT-STATUS = COB-SCR-ESC
                       GO SAQUEW
                   END-IF

               SUBTRACT FSAQU FROM FSALDO GIVING FSALDO
               MOVE FSALDO TO VL-SALDO
               IF VL-SALDO < 0
                   DISPLAY "SALDO: R$ " AT 2055 VL-SALDO
               ELSE
                   DISPLAY "SALDO: R$ " AT 2055 VL-SALDO
               END-IF
               MOVE "ENTER PARA CONTINUAR..." TO MS-GMENSAGEMS
               DISPLAY MS-GMENSAGEMS AT 2330
               FOREGROUND-COLOR 7
               ACCEPT MS-GMENSAGEMS AT 2365 FOREGROUND-COLOR 0
           END-IF.
           REWRITE CADASTRO-REGISTRO
           END-REWRITE.
           GO SAQUEW.
       END-SAQUE.

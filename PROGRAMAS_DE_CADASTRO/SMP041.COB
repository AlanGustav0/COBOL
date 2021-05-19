       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP041.
      *AUTHOR."ALAN GUSTAVO".
      **************************************************
      * CONSULTA DE CARGOS *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT ARQCARG ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOM
                    WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD ARQCARG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCARG.DAT".
       01 REGCARG.
           03 COD                PIC 9(03).
           03 DENOM              PIC X(45).
           03 TPSALARIO          PIC X(01).
           03 SALARIOBASE        PIC 9(06)V99.
           03 STATUSS            PIC X(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO         PIC X(01) VALUE SPACES.
       77 W-ACT           PIC 9(02) VALUE ZEROS.
       77 MENS            PIC X(50) VALUE SPACES.
       77 LIMPA           PIC X(50) VALUE SPACES. 
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL           PIC 9(01) VALUE ZEROS.
       01 IND             PIC 9(02) VALUE ZEROS.
       01 INDSALARIO      PIC 9(01) VALUE ZEROS.
       01 TXTTSALARIO     PIC X(20) VALUE SPACES.
      *-----------------------------------------------------------------
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  TELACARGOS.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "-------------------------------CADASTRO".
           05  LINE 02  COLUMN 41 
               VALUE  "DE CARGOS-------------------------------".
           05  LINE 05  COLUMN 01 
               VALUE  "   CODIGO DO CARGO:".
           05  LINE 07  COLUMN 01 
               VALUE  "   DENOMINACAO DO CARGO:".
           05  LINE 09  COLUMN 01 
               VALUE  "   TIPO DE SALARIO:".
           05  LINE 11  COLUMN 01 
               VALUE  "   SALARIO BASE:".
           05  LINE 13  COLUMN 01 
               VALUE  "   STATUS:".
           05  LINE 22  COLUMN 01 
               VALUE  "   MENSAGEM:".
           05  TCOD
               LINE 05  COLUMN 22  PIC 9(03) 
               USING  COD
               HIGHLIGHT.
           05  TDENOM
               LINE 07  COLUMN 25  PIC X(45) 
               USING  DENOM
               HIGHLIGHT.
           05  TTPSALARIO
               LINE 09  COLUMN 21  PIC X 
               USING  TPSALARIO
               HIGHLIGHT.
           05  TXTSALARIO
               LINE 09  COLUMN 23  PIC X(20)
               USING  TXTTSALARIO
               HIGHLIGHT.
           05  TSALARIOBASE
               LINE 11  COLUMN 18  PIC 999999.99
               USING  SALARIOBASE
               HIGHLIGHT.
           05  TSTATUSS
               LINE 13  COLUMN 12  PIC X(25) 
               USING  STATUSS
               HIGHLIGHT.
           05  TW-OPCAO
               LINE 21  COLUMN 23  PIC X(01)
               USING W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT ARQCARG
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE ARQCARG NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQCARG"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
      *
       INC-001.
           MOVE SPACES TO DENOM STATUSS TPSALARIO
           MOVE ZEROS TO COD W-SEL SALARIOBASE 
           DISPLAY  TELACARGOS.
       INC-001A.
           ACCEPT TCOD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                 CLOSE ARQCARG
                 GO TO ROT-FIM.
           IF W-ACT > 02
                 MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.

                 START ARQCARG KEY IS NOT LESS COD INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-RD2.
           READ ARQCARG NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
      *           GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQCARG"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
              DISPLAY TELACARGOS.
       ROT-SOL.

           ACCEPT  TW-OPCAO
           IF W-OPCAO = "E"
                  GO TO ROT-FIM
           ELSE
              IF W-OPCAO = "P"
                  GO TO INC-RD2
              ELSE
                IF W-OPCAO NOT = "A"
                  MOVE "*** OPCAO NAO DISPONIVEL ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL
                ELSE
                   START ARQCARG KEY IS GREATER
                   COD INVALID KEY
                   MOVE "** CHAVE NAO ENCONTRADA **" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
				   GO TO ROT-FIM.
				    
           GO TO INC-RD2.
      *
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE ARQCARG.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (22, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (22, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** FIM DE PROGRAMA ***--------------------*
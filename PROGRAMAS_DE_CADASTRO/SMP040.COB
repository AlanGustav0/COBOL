       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP040.
      *AUTHOR."ALAN GUSTAVO".
      **************************************************
      * CONSULTA DE DEPARTAMENTOS *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT ARQDEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO
                    WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT".
       01 REGDEP.
          03 CODIGO                   PIC 9(03).
          03 DENOMINACAO              PIC X(45).
          03 STATUSS                  PIC X(01).
          03 TPHIERARC                PIC 9(02).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL             PIC 9(01) VALUE ZEROS.
       01 W-CONT            PIC 9(04) VALUE ZEROS.
       01 W-OPCAO           PIC X(01) VALUE SPACES.
       01 ST-ERRO           PIC X(02) VALUE "00".
       01 W-ACT             PIC 9(02) VALUE ZEROS.
       01 MENS              PIC X(50) VALUE SPACES.
       01 LIMPA             PIC X(55) VALUE SPACES.
       01 SOLIC             PIC X(20) VALUE SPACES.
       01 CONLIN            PIC 9(03) VALUE 001.
       01 TXTTHIERARC       PIC X(45) VALUE SPACES.
      *-----------------------------------------------------------------
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  SMT040.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "                             CONSULTA DE".
           05  LINE 02  COLUMN 41
               VALUE  " DEPARTAMENTOS".
           05  LINE 05  COLUMN 01
               VALUE  "    CODIGO: ".
           05  LINE 07  COLUMN 01
               VALUE  "    DENOMINACAO :".
           05  LINE 09  COLUMN 01
               VALUE  "    NIVEL HIERARQUICO: ".
           05  LINE 11  COLUMN 01
               VALUE  "    STATUS: ".
           05  LINE 21  COLUMN 01
               VALUE  "              OPCAO :   (E=ENCERRA     P".
           05  LINE 21  COLUMN 41
               VALUE  "=PROXIMO     A=ANTERIOR )".
           05  LINE 22  COLUMN 41
               VALUE  "   ==>            <==".
           05  LINE 23  COLUMN 01
               VALUE  " MENSAGEM :".
           05  TCODIGO
               LINE 05  COLUMN 15  PIC 9(03)
               USING CODIGO
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 07  COLUMN 20  PIC X(25)
               USING DENOMINACAO
               HIGHLIGHT.
           05  TTPHIERARC
               LINE 09 COLUMN 25  PIC 9
               USING TPHIERARC
               HIGHLIGHT.
           05  TXTHIERAC
               LINE 09  COLUMN 27  PIC X(45)
               USING TXTTHIERARC
               REVERSE-VIDEO.
           05  TSTATUSS
               LINE 11  COLUMN 15  PIC X(25)
               USING STATUSS
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
           OPEN INPUT ARQDEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE ARQDEP NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQDEP"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
      *
       INC-001.
           MOVE ZEROS TO CODIGO TPHIERARC
           MOVE SPACES TO DENOMINACAO STATUSS
           DISPLAY  SMT040.
       INC-001A.
           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                 CLOSE ARQDEP
                 GO TO ROT-FIM.
           IF W-ACT > 02
                 MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.

                 START ARQDEP KEY IS NOT LESS CODIGO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-RD2.
           READ ARQDEP NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
      *           GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQDEP"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
              DISPLAY SMT040.
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
                   START ARQDEP KEY IS GREATER
                   CODIGO INVALID KEY
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
           CLOSE ARQDEP.
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
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** FIM DE PROGRAMA ***--------------------*
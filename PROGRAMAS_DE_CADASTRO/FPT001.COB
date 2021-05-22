       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPT001.
      *AUTHOR."ALAN GUSTAVO".
      ********************************************************
      * MANUTENCAO DO CADASTRO DE DEPARTAMENTO   *
      ********************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQDEP ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CODIGO
                      ALTERNATE RECORD KEY IS DENOMINACAO
                      WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT".
       01 REGDEP.
          03 CODIGO                   PIC 9(03).
          03 DENOMINACAO              PIC X(25).
          03 DEPSTATUS                PIC X(01).
          03 TPHIERARC                PIC 9(02).

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
       01 TXTTHIERARC     PIC X(45) VALUE SPACES.


       01 TABHIERARC.
           03 TBHIERARC      PIC X(45) OCCURS 9 TIMES.

       01 TABSTATUS.
           03 FILLER PIC X(20) VALUE "AATIVA".
           03 FILLER PIC X(20) VALUE "SSUSPENSA".
           03 FILLER PIC X(20) VALUE "DDESATIVADA".

       01 TABSTATUSX      REDEFINES TABSTATUS.
           03 TBSTATUS    PIC X(20) OCCURS 3 TIMES.

       01 TXTSTATUS.
           03 TXTSTATUS1 PIC X(01) VALUE SPACES.
           03 TXTSTATUS2 PIC X(19) VALUE SPACES.

       SCREEN SECTION.

       01  TELACADASTRO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "                         *** CADASTRO DE".
           05  LINE 02  COLUMN 41
               VALUE  " DEPARTAMENTO ***".
           05  LINE 03  COLUMN 01
               VALUE  "----------------------------------------".
           05  LINE 03  COLUMN 41
               VALUE  "----------------------------------------".
           05  LINE 06  COLUMN 01
               VALUE  "CODIGO: ".
           05  LINE 08  COLUMN 01
               VALUE  "DENOMINACAO: ".
           05  LINE 10  COLUMN 01
               VALUE  "NIVEL HIERARQUICO: ".
           05  LINE 12  COLUMN 01
               VALUE  "STATUS: ".
           05  LINE 23  COLUMN 01
               VALUE  "MENSAGEM: ".


           05  TCODIGO
               LINE 06  COLUMN 9  PIC 9(03)
               USING  CODIGO
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 08  COLUMN 13  PIC X(25)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TTPHIERARC
               LINE 10  COLUMN 23  PIC 9
               USING  TPHIERARC
               HIGHLIGHT.
           05  TXTHIERAC
               LINE 10  COLUMN 26  PIC X(45)
               USING  TXTTHIERARC
               REVERSE-VIDEO.
           05  TSTATUSS
               LINE 12  COLUMN 9  PIC X(25)
               USING  DEPSTATUS
               HIGHLIGHT.

      *--------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "01-CONSELHO CONSULTIVO "     TO TBHIERARC(1)
           MOVE "02-CONSELHO FISCAL"          TO TBHIERARC(2)
           MOVE "03-PRESIDENCIA"              TO TBHIERARC(3)
           MOVE "04-DIRETORIA ADMINISTRATIVA" TO TBHIERARC(4)
           MOVE "05-DIRETORIA INDUSTRIAL"     TO TBHIERARC(5)
           MOVE "06-DIRETORIA COMERCIAL"      TO TBHIERARC(6)
           MOVE "07-DIRETORIA MARKETING"      TO TBHIERARC(7)
           MOVE "08-DIRETORIA JURIDICA"       TO TBHIERARC(8)
           MOVE "09-CONSULTORIA"              TO TBHIERARC(9).

       R0.
           OPEN I-O ARQDEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT ARQDEP
                 CLOSE ARQDEP
                 MOVE "*** ARQUIVO ARQDEP FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                IF ST-ERRO = "95"
                    MOVE "*** ISAM NAO EXCUTADO **" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
                ELSE
                    MOVE "ERRO NA ABERTURA DO ARQUIVO ARQDEP" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

      *------------[ INICIALIZACAO DAS VARIAVEIS ]----------------------
       R1.
           MOVE SPACES TO DENOMINACAO DEPSTATUS   TXTTHIERARC
           MOVE ZEROS TO CODIGO TPHIERARC W-SEL

           DISPLAY TELACADASTRO.

       R2.
           ACCEPT TCODIGO.
           IF CODIGO < 1
              MOVE "CODIGO INVALIDO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.

       LER-ARQDEP.
           READ ARQDEP
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R4A
                DISPLAY TELACADASTRO
                MOVE "*** DEP. JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO ARQDEP" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** DEP. NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.

       R2A.
           ACCEPT TDENOMINACAO.
           IF DENOMINACAO = SPACES
              MOVE "CAMPO DENOMINACAO VAZIO!" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2A.

       R3.

           ACCEPT TTPHIERARC.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                DISPLAY TELACADASTRO
                GO TO R3.
           IF TPHIERARC < 1 OR TPHIERARC > 9
                MOVE "*** DIGITE APENAS DE 1 ATE 9 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R3.
       R3A.
           MOVE TBHIERARC(TPHIERARC) TO TXTTHIERARC
           DISPLAY TXTHIERAC.

       R4.
           ACCEPT TSTATUSS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.

       R4A.
           MOVE TBSTATUS(IND) TO TXTSTATUS
           IF TXTSTATUS1 NOT = DEPSTATUS
              ADD 1 TO IND
              IF IND < 4
                 GO TO R4A
              ELSE
                 MOVE "STATUS INVALIDO" TO MENS
                 MOVE 1 TO IND
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
           ELSE
              DISPLAY TELACADASTRO.
              MOVE 1 TO IND
              DISPLAY (12 , 9) TXTSTATUS2.


      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGDEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DEPARTAMENTO GRAVADO *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* DEPARTAMENTO JA EXISTE,DADOS NAO GRAVADOS *"
                  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO ARQDEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO DEPARTAMENTO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A"
                    AND W-OPCAO NOT = "E"
                    GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R2A.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQDEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO DE DEPARTAMENTO EXCLUIDO ***"
                   TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGDEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO DE DEP."   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *----------------------------------------------------------------
       ROT-FIM.
           CLOSE ARQDEP.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 20) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 20) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

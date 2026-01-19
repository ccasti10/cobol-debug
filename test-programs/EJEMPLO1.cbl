      *================================================================
      * PROGRAMA: EJEMPLO1
      * DESCRIPCION: Programa de ejemplo para testing COBOL Debug
      * AUTOR: Nicolas
      * FECHA: 2026-01-18
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJEMPLO1.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           05  WS-CONTADOR           PIC 9(03)      VALUE ZEROS.
           05  WS-TOTAL-MONTO        PIC 9(09)V99   VALUE ZEROS.
           05  WS-MONTO-ACTUAL       PIC 9(07)V99   VALUE ZEROS.
           05  WS-INTERES            PIC 9(05)V99   VALUE ZEROS.
           05  WS-TASA-INTERES       PIC 9V9999     VALUE 0.1500.
       
       01  WS-CLIENTE-RECORD.
           05  CLI-RUT               PIC X(12).
           05  CLI-NOMBRE            PIC X(40).
           05  CLI-SALDO             PIC 9(11)V99.
       
       01  WS-FLAGS.
           05  WS-FIN-ARCHIVO        PIC X(01)      VALUE 'N'.
               88  FIN-ARCHIVO                      VALUE 'S'.
           05  WS-PROCESO-OK         PIC X(01)      VALUE 'S'.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INICIALIZAR
           PERFORM PROCESAR-DATOS
           PERFORM FINALIZAR
           STOP RUN.
       
       INICIALIZAR.
           DISPLAY 'INICIO DEL PROGRAMA EJEMPLO1'.
           MOVE ZEROS TO WS-CONTADOR.
           MOVE ZEROS TO WS-TOTAL-MONTO.
           
       PROCESAR-DATOS.
           PERFORM CALCULAR-INTERES.
           PERFORM ACUMULAR-TOTALES.
           PERFORM VALIDAR-MONTO.
           
       CALCULAR-INTERES.
           MOVE 1000000 TO WS-MONTO-ACTUAL.
           COMPUTE WS-INTERES = WS-MONTO-ACTUAL * WS-TASA-INTERES.
           ADD WS-INTERES TO WS-MONTO-ACTUAL.
           
       ACUMULAR-TOTALES.
           ADD 1 TO WS-CONTADOR.
           ADD WS-MONTO-ACTUAL TO WS-TOTAL-MONTO.
           
       VALIDAR-MONTO.
           IF WS-TOTAL-MONTO > 5000000
               DISPLAY 'ALERTA: MONTO TOTAL EXCEDE LIMITE'
               MOVE 'N' TO WS-PROCESO-OK
           ELSE
               DISPLAY 'MONTO TOTAL DENTRO DEL LIMITE'
           END-IF.
           
       FINALIZAR.
           DISPLAY 'CONTADOR FINAL: ' WS-CONTADOR.
           DISPLAY 'TOTAL MONTO: ' WS-TOTAL-MONTO.
           DISPLAY 'FIN DEL PROGRAMA EJEMPLO1'.

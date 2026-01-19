      *================================================================
      * PROGRAMA: TEST001
      * DESCRIPCION: Programa de prueba completo para COBOL Debug
      * FUNCIONALIDAD: Procesa transacciones bancarias con validaciones
      *================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST001.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TRANSACCION.
           05  WS-TIPO-TRX           PIC X(02).
               88  TRX-DEPOSITO                  VALUE 'DE'.
               88  TRX-RETIRO                    VALUE 'RE'.
               88  TRX-TRANSFERENCIA             VALUE 'TR'.
           05  WS-MONTO-TRX          PIC 9(09)V99   VALUE ZEROS.
           05  WS-COMISION           PIC 9(05)V99   VALUE ZEROS.
           05  WS-MONTO-NETO         PIC 9(09)V99   VALUE ZEROS.
       
       01  WS-CUENTA.
           05  WS-NRO-CUENTA         PIC 9(10).
           05  WS-SALDO-ACTUAL       PIC 9(11)V99   VALUE ZEROS.
           05  WS-SALDO-DISPONIBLE   PIC 9(11)V99   VALUE ZEROS.
           05  WS-SALDO-RETENIDO     PIC 9(09)V99   VALUE ZEROS.
       
       01  WS-LIMITES.
           05  WS-LIMITE-DIARIO      PIC 9(09)V99   VALUE 5000000.
           05  WS-LIMITE-POR-TRX     PIC 9(09)V99   VALUE 1000000.
           05  WS-ACUM-DIA           PIC 9(09)V99   VALUE ZEROS.
       
       01  WS-TASAS.
           05  WS-TASA-COMISION      PIC 9V9999     VALUE 0.0150.
           05  WS-TASA-IVA           PIC 9V9999     VALUE 0.1900.
           05  WS-IVA-COMISION       PIC 9(05)V99   VALUE ZEROS.
       
       01  WS-CONTADORES.
           05  WS-TRX-PROCESADAS     PIC 9(05)      VALUE ZEROS.
           05  WS-TRX-RECHAZADAS     PIC 9(05)      VALUE ZEROS.
           05  WS-TRX-EXITOSAS       PIC 9(05)      VALUE ZEROS.
       
       01  WS-FLAGS.
           05  WS-TRX-VALIDA         PIC X(01)      VALUE 'S'.
           05  WS-ERROR-CODE         PIC X(04)      VALUE SPACES.
           05  WS-MENSAJE            PIC X(80)      VALUE SPACES.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY 'INICIO PROCESAMIENTO TRANSACCIONES'.
           PERFORM INICIALIZAR
           PERFORM PROCESAR-LOTE
           PERFORM MOSTRAR-RESUMEN
           STOP RUN.
       
       INICIALIZAR.
           MOVE ZEROS TO WS-TRX-PROCESADAS
           MOVE ZEROS TO WS-TRX-RECHAZADAS
           MOVE ZEROS TO WS-TRX-EXITOSAS
           MOVE ZEROS TO WS-ACUM-DIA
           MOVE 10000000.50 TO WS-SALDO-ACTUAL
           COMPUTE WS-SALDO-DISPONIBLE = WS-SALDO-ACTUAL - 
                                          WS-SALDO-RETENIDO.
           
       PROCESAR-LOTE.
           PERFORM PROCESAR-DEPOSITO
           PERFORM PROCESAR-RETIRO
           PERFORM PROCESAR-TRANSFERENCIA.
           
       PROCESAR-DEPOSITO.
           MOVE 'DE' TO WS-TIPO-TRX
           MOVE 500000.00 TO WS-MONTO-TRX
           PERFORM VALIDAR-TRANSACCION
           IF WS-TRX-VALIDA = 'S'
               PERFORM CALCULAR-COMISION
               PERFORM EJECUTAR-DEPOSITO
               ADD 1 TO WS-TRX-EXITOSAS
           ELSE
               ADD 1 TO WS-TRX-RECHAZADAS
           END-IF
           ADD 1 TO WS-TRX-PROCESADAS.
           
       PROCESAR-RETIRO.
           MOVE 'RE' TO WS-TIPO-TRX
           MOVE 250000.00 TO WS-MONTO-TRX
           PERFORM VALIDAR-TRANSACCION
           IF WS-TRX-VALIDA = 'S'
               PERFORM CALCULAR-COMISION
               PERFORM EJECUTAR-RETIRO
               ADD 1 TO WS-TRX-EXITOSAS
           ELSE
               ADD 1 TO WS-TRX-RECHAZADAS
           END-IF
           ADD 1 TO WS-TRX-PROCESADAS.
           
       PROCESAR-TRANSFERENCIA.
           MOVE 'TR' TO WS-TIPO-TRX
           MOVE 1500000.00 TO WS-MONTO-TRX
           PERFORM VALIDAR-TRANSACCION
           IF WS-TRX-VALIDA = 'S'
               PERFORM CALCULAR-COMISION
               PERFORM EJECUTAR-TRANSFERENCIA
               ADD 1 TO WS-TRX-EXITOSAS
           ELSE
               ADD 1 TO WS-TRX-RECHAZADAS
           END-IF
           ADD 1 TO WS-TRX-PROCESADAS.
           
       VALIDAR-TRANSACCION.
           MOVE 'S' TO WS-TRX-VALIDA
           MOVE SPACES TO WS-ERROR-CODE
           
           IF WS-MONTO-TRX <= 0
               MOVE 'N' TO WS-TRX-VALIDA
               MOVE 'E001' TO WS-ERROR-CODE
               MOVE 'MONTO INVALIDO' TO WS-MENSAJE
           END-IF
           
           IF WS-MONTO-TRX > WS-LIMITE-POR-TRX
               MOVE 'N' TO WS-TRX-VALIDA
               MOVE 'E002' TO WS-ERROR-CODE
               MOVE 'EXCEDE LIMITE POR TRANSACCION' TO WS-MENSAJE
           END-IF
           
           COMPUTE WS-ACUM-DIA = WS-ACUM-DIA + WS-MONTO-TRX
           IF WS-ACUM-DIA > WS-LIMITE-DIARIO
               MOVE 'N' TO WS-TRX-VALIDA
               MOVE 'E003' TO WS-ERROR-CODE
               MOVE 'EXCEDE LIMITE DIARIO' TO WS-MENSAJE
           END-IF
           
           IF TRX-RETIRO OR TRX-TRANSFERENCIA
               IF WS-MONTO-TRX > WS-SALDO-DISPONIBLE
                   MOVE 'N' TO WS-TRX-VALIDA
                   MOVE 'E004' TO WS-ERROR-CODE
                   MOVE 'SALDO INSUFICIENTE' TO WS-MENSAJE
               END-IF
           END-IF.
           
       CALCULAR-COMISION.
           COMPUTE WS-COMISION = WS-MONTO-TRX * WS-TASA-COMISION
           COMPUTE WS-IVA-COMISION = WS-COMISION * WS-TASA-IVA
           ADD WS-IVA-COMISION TO WS-COMISION
           COMPUTE WS-MONTO-NETO = WS-MONTO-TRX - WS-COMISION.
           
       EJECUTAR-DEPOSITO.
           ADD WS-MONTO-NETO TO WS-SALDO-ACTUAL
           COMPUTE WS-SALDO-DISPONIBLE = WS-SALDO-ACTUAL - 
                                          WS-SALDO-RETENIDO
           MOVE 'DEPOSITO EXITOSO' TO WS-MENSAJE.
           
       EJECUTAR-RETIRO.
           SUBTRACT WS-MONTO-TRX FROM WS-SALDO-ACTUAL
           SUBTRACT WS-COMISION FROM WS-SALDO-ACTUAL
           COMPUTE WS-SALDO-DISPONIBLE = WS-SALDO-ACTUAL - 
                                          WS-SALDO-RETENIDO
           MOVE 'RETIRO EXITOSO' TO WS-MENSAJE.
           
       EJECUTAR-TRANSFERENCIA.
           SUBTRACT WS-MONTO-TRX FROM WS-SALDO-ACTUAL
           SUBTRACT WS-COMISION FROM WS-SALDO-ACTUAL
           COMPUTE WS-SALDO-DISPONIBLE = WS-SALDO-ACTUAL - 
                                          WS-SALDO-RETENIDO
           MOVE 'TRANSFERENCIA EXITOSA' TO WS-MENSAJE.
           
       MOSTRAR-RESUMEN.
           DISPLAY '============================================'.
           DISPLAY 'RESUMEN DE PROCESAMIENTO'.
           DISPLAY '============================================'.
           DISPLAY 'TRANSACCIONES PROCESADAS: ' WS-TRX-PROCESADAS.
           DISPLAY 'TRANSACCIONES EXITOSAS:   ' WS-TRX-EXITOSAS.
           DISPLAY 'TRANSACCIONES RECHAZADAS: ' WS-TRX-RECHAZADAS.
           DISPLAY 'SALDO FINAL:              ' WS-SALDO-ACTUAL.
           DISPLAY 'ACUMULADO DEL DIA:        ' WS-ACUM-DIA.
           DISPLAY '============================================'.

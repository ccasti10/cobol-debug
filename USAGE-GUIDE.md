# Guía de Uso - COBOL Debug Extension

## Ejemplo Completo: Debugging de EJEMPLO1.cbl

### Paso 1: Abrir el Programa

1. Abre VSCode
2. Abre el archivo `test-programs/EJEMPLO1.cbl`
3. Asegúrate de que el lenguaje está configurado como COBOL

### Paso 2: Agregar Debug Points

#### 2.1 Track WS-MONTO-ACTUAL (Tracking completo)

1. Busca la línea donde se define `WS-MONTO-ACTUAL` (línea 16)
2. Selecciona el texto `WS-MONTO-ACTUAL`
3. Click derecho → **COBOL Debug** → **Track Variable (All Occurrences)**
4. Verás un mensaje: "Tracking WS-MONTO-ACTUAL in X locations"

**¿Qué hace esto?**
- Encuentra todas las líneas donde se usa WS-MONTO-ACTUAL
- Agrega DISPLAYs BEFORE/AFTER en cada operación que lo modifica
- Agrega DISPLAY en operaciones que solo lo leen (IF, DISPLAY)

#### 2.2 Watch WS-INTERES (BEFORE/AFTER)

1. Ve al párrafo `CALCULAR-INTERES` (línea 50)
2. En la línea del COMPUTE, selecciona `WS-INTERES`
3. Click derecho → **COBOL Debug** → **Watch Variable (BEFORE/AFTER)**

**¿Qué hace esto?**
- Agrega DISPLAY antes del COMPUTE mostrando valor anterior
- Agrega DISPLAY después del COMPUTE mostrando nuevo valor y el cambio

#### 2.3 Trace Paragraph CALCULAR-INTERES

1. Click en cualquier línea dentro del párrafo `CALCULAR-INTERES`
2. Click derecho → **COBOL Debug** → **Trace Paragraph (Entry/Exit)**

**¿Qué hace esto?**
- Agrega DISPLAY al entrar al párrafo
- Agrega DISPLAY al salir del párrafo

### Paso 3: Generar Versión DEBUG

1. Presiona `Ctrl+Shift+D` (Mac: `Cmd+Shift+D`)
2. Aparecerá un diálogo con resumen:
   ```
   Ready to generate debug version:
   
   • 1 single point watches
   • 1 variable tracking
   • 1 paragraph traces
   
   Total DISPLAY statements: 12
   
   Continue?
   ```
3. Click en **Generate**
4. Se abrirá un nuevo archivo: `EJEMPLO1-DEBUG.cbl`

### Paso 4: Revisar el Código Generado

El archivo `EJEMPLO1-DEBUG.cbl` tendrá:

#### 4.1 Variables Temporales (agregadas después de WORKING-STORAGE SECTION):

```cobol
CCASTI*===== DEBUG TEMPORARY VARIABLES =====
CCASTI 77  WS-MONTO-ACTUAL-DEBUG-BEFORE PIC X(50).
CCASTI 77  WS-INTERES-DEBUG-BEFORE      PIC X(50).
CCASTI*====================================
```

#### 4.2 En el párrafo CALCULAR-INTERES:

```cobol
       CALCULAR-INTERES.
CCASTI DISPLAY '>>> ENTERING: CALCULAR-INTERES (LINE 50)'.
CCASTI DISPLAY '┌─ LINE 51: MOVE ─────────────────┐'.
CCASTI DISPLAY '| BEFORE:  WS-MONTO-ACTUAL = ' WS-MONTO-ACTUAL.
CCASTI MOVE WS-MONTO-ACTUAL TO WS-MONTO-ACTUAL-DEBUG-BEFORE.
           MOVE 1000000 TO WS-MONTO-ACTUAL.
CCASTI DISPLAY '| AFTER:   WS-MONTO-ACTUAL = ' WS-MONTO-ACTUAL
CCASTI         ' (was: ' WS-MONTO-ACTUAL-DEBUG-BEFORE ')'.
CCASTI DISPLAY '└─────────────────────────────────┘'.

CCASTI DISPLAY '┌─ LINE 52: COMPUTE ──────────────┐'.
CCASTI DISPLAY '| BEFORE:  WS-INTERES = ' WS-INTERES.
CCASTI DISPLAY '|          WS-MONTO-ACTUAL = ' WS-MONTO-ACTUAL.
CCASTI DISPLAY '|          WS-TASA-INTERES = ' WS-TASA-INTERES.
CCASTI MOVE WS-INTERES TO WS-INTERES-DEBUG-BEFORE.
           COMPUTE WS-INTERES = WS-MONTO-ACTUAL * WS-TASA-INTERES.
CCASTI DISPLAY '| AFTER:   WS-INTERES = ' WS-INTERES
CCASTI         ' (was: ' WS-INTERES-DEBUG-BEFORE ')'.
CCASTI DISPLAY '└─────────────────────────────────┘'.

CCASTI DISPLAY '┌─ LINE 53: ADD ──────────────────┐'.
CCASTI DISPLAY '| BEFORE:  WS-MONTO-ACTUAL = ' WS-MONTO-ACTUAL.
CCASTI DISPLAY '|          WS-INTERES = ' WS-INTERES.
CCASTI MOVE WS-MONTO-ACTUAL TO WS-MONTO-ACTUAL-DEBUG-BEFORE.
           ADD WS-INTERES TO WS-MONTO-ACTUAL.
CCASTI DISPLAY '| AFTER:   WS-MONTO-ACTUAL = ' WS-MONTO-ACTUAL
CCASTI         ' (was: ' WS-MONTO-ACTUAL-DEBUG-BEFORE ')'.
CCASTI DISPLAY '└─────────────────────────────────┘'.
CCASTI DISPLAY '<<< EXITING: CALCULAR-INTERES'.
```

### Paso 5: Subir al Mainframe y Compilar

1. **Usando Zowe:**
   ```bash
   zowe files upload ftu "EJEMPLO1-DEBUG.cbl" "YOUR.DATASET.COBOL(EJEMPLO1D)"
   ```

2. **Compilar con tu JCL:**
   - Usa tu JCL estándar de compilación
   - Apunta al member EJEMPLO1D
   - Compila normalmente

3. **Ejecutar:**
   - Ejecuta el programa con tus datos de prueba
   - El SYSOUT tendrá todos los DISPLAYs

### Paso 6: Analizar el SYSOUT

El SYSOUT mostrará algo como:

```
INICIO DEL PROGRAMA EJEMPLO1
>>> ENTERING: CALCULAR-INTERES (LINE 50)
┌─ LINE 51: MOVE ─────────────────────────┐
| BEFORE:  WS-MONTO-ACTUAL = 0000000.00   |
| AFTER:   WS-MONTO-ACTUAL = 1000000.00 (was: 0000000.00) |
└─────────────────────────────────────────┘

┌─ LINE 52: COMPUTE ──────────────────────┐
| BEFORE:  WS-INTERES = 0000000.00        |
|          WS-MONTO-ACTUAL = 1000000.00   |
|          WS-TASA-INTERES = 0.1500       |
| AFTER:   WS-INTERES = 0150000.00 (was: 0000000.00) |
└─────────────────────────────────────────┘

┌─ LINE 53: ADD ──────────────────────────┐
| BEFORE:  WS-MONTO-ACTUAL = 1000000.00   |
|          WS-INTERES = 0150000.00        |
| AFTER:   WS-MONTO-ACTUAL = 1150000.00 (was: 1000000.00) |
└─────────────────────────────────────────┘
<<< EXITING: CALCULAR-INTERES
CONTADOR FINAL: 001
TOTAL MONTO: 0001150000.00
FIN DEL PROGRAMA EJEMPLO1
```

### Paso 7: Guardar Configuración (Opcional)

1. En VSCode, abre Command Palette (Ctrl+Shift+P)
2. Escribe "COBOL Debug: Save Debug Configuration"
3. Se guardará en `.vscode/cobol-debug/EJEMPLO1.debug.json`

**Beneficio:** La próxima vez que abras EJEMPLO1.cbl, puedes cargar la configuración con "Load Debug Configuration" y regenerar sin tener que agregar los debug points nuevamente.

## Comandos Rápidos

- `Ctrl+Shift+D` - Generate Debug Version
- Click derecho en variable seleccionada → COBOL Debug → ...
- Command Palette → "COBOL Debug: ..."

## Tips

1. **Antes de generar**: Revisa cuántos DISPLAYs se generarán en el preview
2. **Variables con muchas ocurrencias**: La extensión te advertirá si hay 50+
3. **Configuración guardada**: Útil cuando haces debugging iterativo
4. **Formato ASCII vs Unicode**: Si los caracteres se ven mal en SYSOUT, cambia en Settings

## Troubleshooting

### Los caracteres especiales se ven mal en SYSOUT
- Solución: Ve a Settings → COBOL Debug → Output Format → Selecciona "ascii"

### El programa no compila
- Verifica que las columnas 1-6 tengan el marker correcto (CCASTI por defecto)
- Verifica que no haya líneas muy largas (>72 caracteres en área de código)

### No veo mis debug points
- Asegúrate de haber generado la versión DEBUG (Ctrl+Shift+D)
- Verifica que estés ejecutando EJEMPLO1-DEBUG.cbl, no EJEMPLO1.cbl

### Quiero más/menos información
- Usa diferentes tipos de watch:
  - "Current Line" = mínimo
  - "BEFORE/AFTER" = medio
  - "Track All Occurrences" = máximo

## Próximos Pasos

Una vez que domines el flujo básico:

1. Prueba con tus propios programas COBOL
2. Experimenta con diferentes tipos de tracking
3. Guarda configuraciones para programas complejos
4. Espera la Fase 2 con el parser automático de SYSOUT!

# Comandos Avanzados - COBOL Debug

## 🎯 Debug Variables in Selection

Este comando detecta automáticamente todas las variables en un bloque de código seleccionado y te permite elegir cuáles debuggear.

### Cuándo Usarlo

- Tienes un bloque complejo con múltiples variables
- Quieres debuggear varias variables a la vez en el mismo punto
- No estás seguro de qué variables están involucradas en un cálculo

### Cómo Usar

#### Paso 1: Seleccionar Código

```cobol
       CALCULAR-DESCUENTO.
           COMPUTE WS-DESCUENTO = WS-PRECIO * WS-TASA-DESC.
           SUBTRACT WS-DESCUENTO FROM WS-PRECIO GIVING WS-PRECIO-FINAL.
           ADD WS-DESCUENTO TO WS-TOTAL-DESCUENTOS.
```

Selecciona las líneas que quieres analizar (por ejemplo, las 3 líneas COMPUTE/SUBTRACT/ADD).

#### Paso 2: Ejecutar Comando

- **Opción A**: Click derecho → **COBOL Debug** → **Debug Variables in Selection**
- **Opción B**: Command Palette → "COBOL Debug: Debug Variables in Selection"

#### Paso 3: Seleccionar Variables

Aparece un diálogo mostrando variables detectadas:

```
┌────────────────────────────────────────────────────┐
│ Debug Variables in Selection                       │
├────────────────────────────────────────────────────┤
│ 6 variables detected. Select which ones to debug: │
│                                                    │
│ ☑ WS-DESCUENTO                                     │
│ ☑ WS-PRECIO                                        │
│ ☑ WS-TASA-DESC                                     │
│ ☑ WS-PRECIO-FINAL                                  │
│ ☑ WS-TOTAL-DESCUENTOS                              │
│ ☐ WS-CONTADOR                                      │
│                                                    │
│ [OK] [Cancel]                                      │
└────────────────────────────────────────────────────┘
```

Usa las flechas y espacio para seleccionar/deseleccionar.

#### Paso 4: Elegir Modo

```
┌────────────────────────────────────────────────────┐
│ Select debug mode                                  │
├────────────────────────────────────────────────────┤
│ › BEFORE only                                      │
│   AFTER only                                       │
│   BEFORE/AFTER                                     │
│                                                    │
└────────────────────────────────────────────────────┘
```

- **BEFORE only**: Ver valores antes del bloque
- **AFTER only**: Ver valores después del bloque
- **BEFORE/AFTER**: Ver valores antes y después

#### Paso 5: Confirmación

```
✓ Added 5 variable(s) to debug at line 234
```

### Resultado en Código Generado

Con modo BEFORE/AFTER seleccionado:

```cobol
CCASTI DISPLAY '┌─ LINE 234: BEFORE ──────────────┐'.
CCASTI DISPLAY '| BEFORE:  WS-DESCUENTO = ' WS-DESCUENTO.
CCASTI DISPLAY '|          WS-PRECIO = ' WS-PRECIO.
CCASTI DISPLAY '|          WS-TASA-DESC = ' WS-TASA-DESC.
CCASTI DISPLAY '|          WS-PRECIO-FINAL = ' WS-PRECIO-FINAL.
CCASTI DISPLAY '|          WS-TOTAL-DESCUENTOS = ' WS-TOTAL-DESCUENTOS.
CCASTI MOVE WS-DESCUENTO TO WS-DESCUENTO-DEBUG-BEFORE.
CCASTI MOVE WS-PRECIO TO WS-PRECIO-DEBUG-BEFORE.
CCASTI MOVE WS-TASA-DESC TO WS-TASA-DESC-DEBUG-BEFORE.
CCASTI MOVE WS-PRECIO-FINAL TO WS-PRECIO-FINAL-DEBUG-BEFORE.
CCASTI MOVE WS-TOTAL-DESCUENTOS TO WS-TOTAL-DESCUENTOS-DEBUG-BEFORE.
       CALCULAR-DESCUENTO.
           COMPUTE WS-DESCUENTO = WS-PRECIO * WS-TASA-DESC.
           SUBTRACT WS-DESCUENTO FROM WS-PRECIO GIVING WS-PRECIO-FINAL.
           ADD WS-DESCUENTO TO WS-TOTAL-DESCUENTOS.
CCASTI DISPLAY '┌─ LINE 234: AFTER ───────────────┐'.
CCASTI DISPLAY '| AFTER:   WS-DESCUENTO = ' WS-DESCUENTO
CCASTI         ' (was: ' WS-DESCUENTO-DEBUG-BEFORE ')'.
CCASTI DISPLAY '|          WS-PRECIO = ' WS-PRECIO
CCASTI         ' (was: ' WS-PRECIO-DEBUG-BEFORE ')'.
CCASTI DISPLAY '|          WS-TASA-DESC = ' WS-TASA-DESC
CCASTI         ' (was: ' WS-TASA-DESC-DEBUG-BEFORE ')'.
CCASTI DISPLAY '|          WS-PRECIO-FINAL = ' WS-PRECIO-FINAL
CCASTI         ' (was: ' WS-PRECIO-FINAL-DEBUG-BEFORE ')'.
CCASTI DISPLAY '|          WS-TOTAL-DESCUENTOS = ' WS-TOTAL-DESCUENTOS
CCASTI         ' (was: ' WS-TOTAL-DESCUENTOS-DEBUG-BEFORE ')'.
CCASTI DISPLAY '└─────────────────────────────────┘'.
```

### Casos de Uso

#### Caso 1: Debugging de Cálculo Complejo

```cobol
       CALCULAR-INTERES-COMPUESTO.
           COMPUTE WS-TASA-DIARIA = WS-TASA-ANUAL / 365.
           COMPUTE WS-FACTOR = (1 + WS-TASA-DIARIA) ** WS-DIAS.
           COMPUTE WS-MONTO-FINAL = WS-CAPITAL * WS-FACTOR.
```

Selecciona las 3 líneas → Debug Variables in Selection → BEFORE/AFTER

**Beneficio**: Ves todos los valores de entrada y salida del cálculo en un solo punto.

#### Caso 2: Debugging de Validaciones

```cobol
       VALIDAR-DATOS.
           IF WS-EDAD < 18 OR WS-EDAD > 100
               MOVE 'N' TO WS-VALIDO
           END-IF.
           IF WS-SALDO < 0
               MOVE 'N' TO WS-VALIDO
           END-IF.
           IF WS-RUT = SPACES
               MOVE 'N' TO WS-VALIDO
           END-IF.
```

Selecciona el bloque completo → Debug Variables in Selection → BEFORE only

**Beneficio**: Ves los valores que se están evaluando en las condiciones.

---

## 🎯 Debug All Variables in Paragraph

Este comando encuentra automáticamente todas las variables usadas en un párrafo y las agrega a tracking.

### Cuándo Usarlo

- Quieres debuggear un párrafo completo
- No sabes exactamente qué variables están involucradas
- Quieres tracking exhaustivo de un proceso específico

### Cómo Usar

#### Paso 1: Posicionar Cursor

Coloca el cursor en cualquier línea dentro del párrafo que quieres debuggear:

```cobol
       PROCESAR-PAGO.
           MOVE WS-MONTO-PAGO TO WS-MONTO-TEMPORAL.
           COMPUTE WS-COMISION = WS-MONTO-TEMPORAL * 0.03.
           SUBTRACT WS-COMISION FROM WS-MONTO-TEMPORAL.
           ADD WS-MONTO-TEMPORAL TO WS-TOTAL-PAGOS.
           ADD 1 TO WS-CONTADOR-PAGOS.
           IF WS-CONTADOR-PAGOS > 100
               PERFORM PROCESAR-LOTE
           END-IF.
```

Cursor en cualquier línea dentro de PROCESAR-PAGO.

#### Paso 2: Ejecutar Comando

- **Opción A**: Click derecho → **COBOL Debug** → **Debug All Variables in Paragraph**
- **Opción B**: Command Palette → "COBOL Debug: Debug All Variables in Paragraph"

#### Paso 3: Seleccionar Variables

Aparece diálogo con variables detectadas:

```
┌────────────────────────────────────────────────────┐
│ Debug Variables in Paragraph                       │
├────────────────────────────────────────────────────┤
│ 6 variables detected in PROCESAR-PAGO.            │
│ Select which ones to track:                        │
│                                                    │
│ ☑ WS-MONTO-PAGO                                    │
│ ☑ WS-MONTO-TEMPORAL                                │
│ ☑ WS-COMISION                                      │
│ ☑ WS-TOTAL-PAGOS                                   │
│ ☑ WS-CONTADOR-PAGOS                                │
│ ☐ WS-FLAG-PROCESO                                  │
│                                                    │
│ [OK] [Cancel]                                      │
└────────────────────────────────────────────────────┘
```

#### Paso 4: Elegir Alcance

```
┌────────────────────────────────────────────────────┐
│ Select tracking scope                              │
├────────────────────────────────────────────────────┤
│ › Track in entire program                          │
│   Find all occurrences throughout the program      │
│                                                    │
│   Track only in this paragraph                     │
│   Only track within PROCESAR-PAGO                  │
│                                                    │
└────────────────────────────────────────────────────┘
```

**Opción A: Track in entire program**
- Encuentra TODAS las ocurrencias de las variables en TODO el programa
- Útil si el problema puede estar en otro párrafo

**Opción B: Track only in this paragraph**
- Solo trackea las ocurrencias dentro de PROCESAR-PAGO
- Útil para debugging enfocado
- Genera menos DISPLAYs

#### Paso 5: Confirmación

```
✓ Added tracking for 5 variable(s) in entire program
```

o

```
✓ Added tracking for 5 variable(s) in paragraph PROCESAR-PAGO
```

### Diferencia entre Alcances

#### Scope: "Entire Program"

Si WS-MONTO-TEMPORAL aparece en 3 párrafos diferentes:

```cobol
       INICIALIZAR.
           MOVE ZEROS TO WS-MONTO-TEMPORAL.     ← Trackeado
           
       PROCESAR-PAGO.
           MOVE WS-MONTO-PAGO TO WS-MONTO-TEMPORAL.  ← Trackeado
           COMPUTE WS-COMISION = WS-MONTO-TEMPORAL * 0.03.  ← Trackeado
           
       FINALIZAR.
           DISPLAY WS-MONTO-TEMPORAL.           ← Trackeado
```

**Resultado**: 4 puntos de tracking para WS-MONTO-TEMPORAL.

#### Scope: "Only in This Paragraph"

```cobol
       INICIALIZAR.
           MOVE ZEROS TO WS-MONTO-TEMPORAL.     ← NO trackeado
           
       PROCESAR-PAGO.
           MOVE WS-MONTO-PAGO TO WS-MONTO-TEMPORAL.  ← Trackeado
           COMPUTE WS-COMISION = WS-MONTO-TEMPORAL * 0.03.  ← Trackeado
           
       FINALIZAR.
           DISPLAY WS-MONTO-TEMPORAL.           ← NO trackeado
```

**Resultado**: Solo 2 puntos de tracking para WS-MONTO-TEMPORAL.

### Casos de Uso

#### Caso 1: Debugging de Proceso Completo

**Situación**: El párrafo CALCULAR-TOTAL da resultados incorrectos pero no sabes qué variable tiene el valor malo.

**Solución**:
1. Cursor en CALCULAR-TOTAL
2. Debug All Variables in Paragraph
3. Selecciona TODAS las variables
4. Scope: "Only in this paragraph" (enfoque)
5. Genera y ejecuta
6. Analizas SYSOUT para ver cuál variable tiene valor incorrecto

#### Caso 2: Debugging de Variable que Cambia Inesperadamente

**Situación**: WS-SALDO tiene un valor incorrecto al final del programa, pero no sabes en qué párrafo se modifica incorrectamente.

**Solución**:
1. Busca un párrafo donde sepas que se usa WS-SALDO
2. Debug All Variables in Paragraph
3. Selecciona solo WS-SALDO
4. Scope: "Track in entire program"
5. Ve TODO el historial de cambios de WS-SALDO

#### Caso 3: Debugging Inicial (No sabes qué buscar)

**Situación**: Un proceso falla pero no tienes idea de qué variable es el problema.

**Solución**:
1. Identifica el párrafo donde crees que está el problema
2. Debug All Variables in Paragraph
3. Selecciona TODAS las variables
4. Scope: "Only in this paragraph"
5. Primera ejecución: ves qué variables tienen valores sospechosos
6. Segunda iteración: Enfocas solo en las variables problemáticas

---

## 💡 Comparación de Comandos

| Comando | Input | Granularidad | Alcance | Mejor Para |
|---------|-------|--------------|---------|------------|
| **Watch Variable (Current Line)** | 1 variable | 1 línea | Local | Debugging puntual |
| **Watch Variable (BEFORE/AFTER)** | 1 variable | 1 línea | Local | Ver cambio en operación |
| **Track Variable (All Occurrences)** | 1 variable | Todo el programa | Global | Seguir flujo de 1 variable |
| **Debug Variables in Selection** | Múltiples variables | Bloque de código | Local | Debugging de bloque |
| **Debug All Variables in Paragraph** | Múltiples variables | Párrafo completo | Local o Global | Debugging de proceso |

---

## 🚀 Workflow Recomendado

### Debugging Progresivo (De más general a más específico)

**Iteración 1 - Exploración**
```
Debug All Variables in Paragraph
→ Scope: "Only in paragraph"
→ Selecciona: TODAS las variables
→ Objetivo: Identificar qué variables tienen valores sospechosos
```

**Iteración 2 - Enfoque**
```
Track Variable (All Occurrences)
→ Selecciona: Solo las 2-3 variables sospechosas
→ Objetivo: Ver dónde cambian esas variables en todo el programa
```

**Iteración 3 - Precisión**
```
Watch Variable (BEFORE/AFTER)
→ Selecciona: La variable problemática
→ En la línea exacta donde el valor se vuelve incorrecto
→ Objetivo: Ver exactamente qué operación causa el problema
```

### Debugging de Cálculo Complejo

**Paso 1: Seleccionar el bloque completo**
```
Debug Variables in Selection
→ Modo: BEFORE/AFTER
→ Objetivo: Ver entrada y salida del cálculo completo
```

**Paso 2: Si hay problema, enfocarse en operación específica**
```
Watch Variable (BEFORE/AFTER)
→ En la línea específica que da valor incorrecto
→ Con showRelatedVars = true
```

---

## ⚡ Tips y Trucos

### Tip 1: Usa el Panel para Revisar

Después de usar estos comandos, abre el panel lateral para:
- Ver cuántas variables agregaste
- Verificar que no haya duplicados
- Remover variables innecesarias antes de generar

### Tip 2: Combina con Trace Paragraph

Para debugging súper completo:
1. Trace Paragraph (para ver entrada/salida del párrafo)
2. Debug All Variables in Paragraph (para ver todas las variables)
3. Genera y tienes visibilidad total del párrafo

### Tip 3: Selección Estratégica en Quick Pick

En el diálogo de selección múltiple:
- `Ctrl+A` (Mac: `Cmd+A`) = Seleccionar todo
- `Espacio` = Toggle selección individual
- Usa búsqueda incremental escribiendo el nombre

### Tip 4: Scope Según Fase

**Primera vez debuggeando**: "Only in paragraph" (menos ruido)  
**Bug persiste**: "Track in entire program" (visión completa)

---

## ❓ Troubleshooting

### No detecta mis variables

**Problema**: El comando dice "No variables detected"

**Soluciones**:
1. Verifica que las variables estén definidas en WORKING-STORAGE
2. Asegúrate de haber seleccionado código ejecutable (no comentarios)
3. Revisa que uses nombres válidos de COBOL (no hay espacios en nombres de variables)

### Detecta demasiadas variables

**Problema**: Lista 20+ variables y muchas no son relevantes

**Solución**:
- Usa el diálogo de selección múltiple para deseleccionar las irrelevantes
- Considera usar comandos más específicos (Watch Variable individual)

### El scope "Only in paragraph" no funciona como esperaba

**Problema**: Esperabas ver más/menos ocurrencias

**Verificación**:
- Abre el panel lateral
- Expande la variable en Variable Tracking
- Verifica que las líneas mostradas estén dentro del rango del párrafo
- Si algo está mal, remueve y vuelve a agregar con el scope correcto

---

## 🎯 Resumen

**Debug Variables in Selection** = Múltiples variables, punto específico  
**Debug All Variables in Paragraph** = Múltiples variables, proceso completo  

Ambos comandos te dan **control total** sobre qué debuggear sin tener que ir variable por variable.

**Usa estos comandos cuando**:
- Tienes muchas variables relacionadas
- No estás seguro qué debuggear
- Quieres visibilidad completa de un proceso
- Estás haciendo debugging exploratorio

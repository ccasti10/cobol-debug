# Guía del Panel Lateral - COBOL Debug

## 📊 Vista General del Panel

El panel lateral "COBOL Debug" te da una vista completa y organizada de todos tus debug points configurados para el programa actual.

### Ubicación del Panel

1. **Activity Bar** (barra lateral izquierda de VSCode)
2. Busca el ícono con "CBL" y una "D" roja
3. Click para abrir el panel "COBOL Debug"

Alternativamente:
- Command Palette (`Ctrl+Shift+P`) → "COBOL Debug: Show Debug Panel"

---

## 🎯 Estructura del Panel

El panel se organiza en secciones:

```
COBOL Debug
├─ 📊 Summary
│  ├─ Program: EJEMPLO1
│  ├─ Total debug points: 3
│  ├─ Est. DISPLAY statements: 12
│  └─ Last modified: 18/01/2026 10:30
│
├─ 📍 Single Point Watches (1)
│  └─ WS-INTERES (BEFORE_AFTER) - Line 52
│
├─ 🔍 Variable Tracking (1)
│  └─ WS-MONTO-ACTUAL (3 occurrences)
│     ├─ Line 51: MOVE (WRITE)
│     ├─ Line 52: COMPUTE (READ)
│     └─ Line 53: ADD (WRITE)
│
└─ 📋 Paragraph Traces (1)
   └─ CALCULAR-INTERES - Lines 50-53
```

---

## 🔧 Funcionalidades

### 1. Navegación Rápida

**Click en cualquier item** para saltar a esa línea en el código:

- Click en "WS-MONTO-ACTUAL" → Salta a la definición
- Click en "Line 52: COMPUTE" → Salta a esa línea
- Click en "CALCULAR-INTERES" → Salta al párrafo

### 2. Remover Debug Points

**Icono de basura** (🗑️) aparece al pasar el mouse sobre items:

- Hover sobre "WS-INTERES (BEFORE_AFTER)"
- Click en icono de basura
- Confirmación: "Removed watch for WS-INTERES at line 52"

### 3. Expandir/Colapsar

**Variable Tracking** muestra detalles expandibles:

- Click en `▶` junto a "WS-MONTO-ACTUAL"
- Se expande mostrando todas las ocurrencias
- Click en `▼` para colapsar

### 4. Refresh

**Botón de refresh** en la parte superior del panel:

- Actualiza la vista si algo no se sincronizó
- Generalmente no es necesario (actualización automática)

---

## 📝 Secciones Detalladas

### Summary (Resumen)

Muestra información general:

```
📊 Summary
├─ Program: EJEMPLO1              ← Nombre del programa
├─ Total debug points: 3          ← Total de puntos configurados
├─ Est. DISPLAY statements: 12    ← DISPLAYs que se generarán
└─ Last modified: 18/01/2026      ← Última modificación
```

**Útil para:**
- Ver rápidamente cuántos debug points tienes
- Estimar el tamaño del SYSOUT
- Saber cuándo fue la última vez que modificaste la config

### Single Point Watches

Lista de variables con watch en líneas específicas:

```
📍 Single Point Watches (2)
├─ WS-CONTADOR (CURRENT_LINE) - Line 234
└─ WS-INTERES (BEFORE_AFTER) - Line 52
```

**Información mostrada:**
- Nombre de la variable
- Modo: CURRENT_LINE, BEFORE, AFTER, o BEFORE_AFTER
- Número de línea

**Acciones disponibles:**
- Click → Navega a la línea
- Icono basura → Remueve el watch

### Variable Tracking

Variables que se están trackeando en todas sus ocurrencias:

```
🔍 Variable Tracking (1)
└─ WS-MONTO-ACTUAL (3 occurrences)    ← Expandible
   ├─ Line 51: MOVE (WRITE)
   ├─ Line 52: COMPUTE (READ)
   └─ Line 53: ADD (WRITE)
```

**Información mostrada:**
- Nombre de la variable
- Cantidad de ocurrencias
- Al expandir: cada ocurrencia con:
  - Línea
  - Tipo de operación (MOVE, COMPUTE, etc.)
  - Tipo de acceso (READ/WRITE)

**Acciones disponibles:**
- Click en variable → Navega a la definición
- Click en ocurrencia → Navega a esa línea
- Icono basura (en variable) → Remueve TODO el tracking

### Paragraph Traces

Párrafos con tracing de entrada/salida:

```
📋 Paragraph Traces (1)
└─ CALCULAR-INTERES - Lines 50-53
```

**Información mostrada:**
- Nombre del párrafo
- Rango de líneas

**Acciones disponibles:**
- Click → Navega al inicio del párrafo
- Icono basura → Remueve el trace

---

## 🎨 Iconos Usados

| Icono | Significado |
|-------|-------------|
| 📊 | Resumen |
| 📍 | Single point watch |
| 🔍 | Variable tracking |
| 📋 | Paragraph trace |
| 💡 | Información |
| 🗑️ | Remover |
| 🔄 | Refresh |
| ▶/▼ | Expandir/Colapsar |

---

## 💡 Casos de Uso

### Caso 1: Verificar qué estás debuggeando

**Antes de generar el archivo DEBUG:**

1. Abre el panel lateral
2. Revisa la sección Summary
3. Verifica que tienes los debug points correctos
4. Si falta algo → Agrega desde el código
5. Si sobra algo → Remueve desde el panel

### Caso 2: Remover debug points específicos

**Tienes demasiados DISPLAYs:**

1. Abre el panel
2. Ve a "Variable Tracking"
3. Identifica variables con muchas ocurrencias
4. Click en icono de basura para remover
5. Regenera el archivo DEBUG

### Caso 3: Navegar entre debug points

**Quieres revisar todas las líneas que estás debuggeando:**

1. Abre el panel
2. Expande "Variable Tracking"
3. Click en cada ocurrencia
4. El editor salta a esa línea
5. Verificas que sea correcto

### Caso 4: Debugging iterativo

**Primera iteración:**
- Agregas WS-MONTO en tracking
- Generas DEBUG
- Ejecutas en mainframe
- Encuentras que el problema está en WS-INTERES

**Segunda iteración:**
1. Abre el panel
2. Remueve tracking de WS-MONTO (ya no lo necesitas)
3. Agrega tracking de WS-INTERES
4. El panel muestra solo lo que necesitas ahora
5. Regeneras DEBUG más limpio

---

## ⚙️ Configuración del Panel

### Auto-refresh

El panel se actualiza automáticamente cuando:
- Agregas un nuevo debug point
- Remueves un debug point
- Cargas una configuración
- Limpias todos los debug points

### Persistencia

La configuración se mantiene:
- Entre sesiones de VSCode
- Si guardas la configuración (Save Debug Configuration)
- Se carga automáticamente al abrir el mismo programa

---

## 🚀 Tips y Trucos

### Tip 1: Usa el Summary para calcular tiempo

**Estimación rápida del tamaño del SYSOUT:**
- Cada DISPLAY ≈ 1-2 líneas en SYSOUT
- Summary muestra "Est. DISPLAY statements: 12"
- Aproximadamente 12-24 líneas en SYSOUT
- Útil para saber si será mucho o poco output

### Tip 2: Expande antes de remover

**Para tracking con muchas ocurrencias:**
1. Expande para ver todas las líneas
2. Revisa si realmente necesitas trackear todas
3. Si no, remueve y agrega solo single point watches en líneas específicas

### Tip 3: Panel como checklist

**Antes de generar DEBUG:**
- [ ] Summary muestra programa correcto
- [ ] Cantidad de debug points es razonable (< 10 es manejable)
- [ ] Est. DISPLAYs no es excesivo (< 50 es bueno)
- [ ] No hay debug points duplicados

### Tip 4: Usa Search en VSCode

**Si tienes muchos debug points:**
1. Ctrl+Shift+P → "Focus on COBOL Debug View"
2. Empieza a escribir el nombre de variable
3. VSCode filtra el árbol automáticamente

---

## ❓ Troubleshooting

### El panel está vacío

**Solución:**
- Verifica que hayas agregado debug points primero
- Si acabas de abrir VSCode, carga la configuración: `Load Debug Configuration`
- Si sigue vacío, agrega al menos un debug point desde el código

### Los números no coinciden

**Panel dice "3 occurrences" pero veo solo 2:**
- Click en Refresh (icono 🔄)
- Si persiste, limpia y vuelve a agregar el tracking

### No puedo remover un item

**El icono de basura no aparece:**
- Asegúrate de pasar el mouse sobre el item correcto
- Solo aparece en items que se pueden remover (no en raíces)
- Prueba hacer click derecho y buscar opción "Remove"

### El panel no se actualiza

**Agregaste debug point pero no aparece:**
1. Click en Refresh
2. Si no funciona, cierra y reabre el panel
3. Como última opción: Reload Window (Ctrl+Shift+P → "Reload Window")

---

## 🎯 Resumen

El panel lateral es tu **centro de control** para debug:

✅ **Vista rápida** de todos los debug points  
✅ **Navegación** directa al código  
✅ **Gestión** fácil (agregar/remover)  
✅ **Estadísticas** útiles  
✅ **Actualización** automática  

**Úsalo siempre** antes de generar la versión DEBUG para asegurarte de que tienes exactamente lo que necesitas.

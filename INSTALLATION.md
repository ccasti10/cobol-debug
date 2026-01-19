# Instalación y Despliegue - COBOL Debug Extension

## 📦 Opciones de Instalación

Hay 3 formas de instalar la extensión:

1. **Development Mode** (para testing)
2. **Local Installation** (instalar .vsix)
3. **Marketplace** (futuro - publicar en VSCode Marketplace)

---

## Opción 1: Development Mode (Recomendado para Testing)

### Requisitos Previos

- Node.js instalado (v18 o superior)
- VSCode instalado
- Git (opcional, si clonas desde repositorio)

### Pasos de Instalación

#### 1. Descomprimir el Proyecto

```bash
# Si tienes el archivo tar.gz
tar -xzf cobol-debug-extension-v2.tar.gz
cd cobol-debug-extension
```

#### 2. Instalar Dependencias

```bash
npm install
```

**Output esperado:**
```
added 270 packages, and audited 271 packages in 12s
75 packages are looking for funding
found 0 vulnerabilities
```

#### 3. Compilar el Proyecto

```bash
npm run compile
```

**Output esperado:**
```
> cobol-debug@0.1.0 compile
> tsc -p ./

(sin errores)
```

#### 4. Abrir en VSCode

```bash
code .
```

O desde VSCode: `File > Open Folder` → Selecciona `cobol-debug-extension`

#### 5. Ejecutar en Development Host

1. En VSCode, presiona `F5`
2. Se abrirá una nueva ventana con título `[Extension Development Host]`
3. En esta ventana, abre un archivo COBOL
4. Prueba los comandos de la extensión

#### 6. Ver Logs de Debug

En la ventana original (no la de Development Host):
- `View > Output`
- Selecciona "Extension Host" en el dropdown
- Verás logs de la extensión

### Ventajas del Development Mode

✅ Cambios en tiempo real (hot reload con `npm run watch`)  
✅ Debug completo con breakpoints  
✅ Fácil de probar cambios  
✅ No afecta tu VSCode principal  

### Desventajas

❌ Requiere abrir dos ventanas de VSCode  
❌ No persiste después de cerrar VSCode  
❌ Solo para testing, no para uso diario  

---

## Opción 2: Local Installation (Recomendado para Uso Diario)

### Paso 1: Empaquetar la Extensión

Desde el directorio del proyecto:

```bash
npm run package
```

**Output esperado:**
```
> cobol-debug@0.1.0 package
> vsce package

Executing prepublish script 'npm run vscode:prepublish'...
...
DONE  Packaged: /path/to/cobol-debug-0.1.0.vsix (XX.XX KB)
```

Se creará un archivo: `cobol-debug-0.1.0.vsix`

### Paso 2: Instalar el VSIX

#### Opción A: Línea de Comandos

```bash
code --install-extension cobol-debug-0.1.0.vsix
```

#### Opción B: Interfaz Gráfica

1. Abre VSCode
2. Ve a Extensions (`Ctrl+Shift+X` o `Cmd+Shift+X`)
3. Click en `...` (menú de opciones) en la parte superior
4. Selecciona `Install from VSIX...`
5. Navega y selecciona `cobol-debug-0.1.0.vsix`

**Confirmación:**
```
✓ Successfully installed 'cobol-debug' extension
```

### Paso 3: Verificar Instalación

1. Reinicia VSCode (o `Reload Window`)
2. Ve a Extensions
3. Busca "COBOL Debug"
4. Deberías verla en la lista de instaladas

### Paso 4: Verificar Funcionamiento

1. Abre cualquier archivo `.cbl`
2. Busca el ícono "CBL D" en la Activity Bar (barra izquierda)
3. Click derecho en una variable → Deberías ver menú "COBOL Debug"

### Ventajas de Local Installation

✅ Instalación permanente  
✅ Disponible en todos los proyectos  
✅ Funciona como cualquier otra extensión  
✅ Fácil de compartir (enviar .vsix a colegas)  

### Desventajas

❌ Actualizaciones manuales  
❌ No aparece en Marketplace  

---

## Opción 3: Publicar en Marketplace (Futuro)

### Requisitos

- Cuenta de Azure DevOps
- Personal Access Token (PAT)
- Publisher ID registrado

### Pasos (Referencia Futura)

```bash
# 1. Crear publisher (solo una vez)
vsce create-publisher <your-publisher-name>

# 2. Login
vsce login <your-publisher-name>

# 3. Publicar
vsce publish

# 4. La extensión estará en:
# https://marketplace.visualstudio.com/items?itemName=<publisher>.<extension>
```

**Ventajas:**
- Instalación directa desde VSCode
- Actualizaciones automáticas
- Visibilidad pública

**Desventajas:**
- Requiere proceso de publicación
- Revisión de Microsoft
- No privada

---

## 🔧 Configuración Post-Instalación

### 1. Verificar Settings

`File > Preferences > Settings` → Buscar "COBOL Debug"

Deberías ver:
- Output Width
- Output Format
- Debug Marker
- Include Read-Only Operations
- Warn On Many Occurrences

### 2. Configurar Workspace (Opcional)

Crea `.vscode/settings.json` en tu proyecto:

```json
{
  "cobolDebug.outputWidth": 133,
  "cobolDebug.outputFormat": "auto",
  "cobolDebug.debugMarker": "CCASTI",
  "cobolDebug.includeReadOnlyOperations": true,
  "cobolDebug.warnOnManyOccurrences": 50
}
```

### 3. Verificar Formato de Output

Si los caracteres especiales no se ven bien en tu SYSOUT:

```json
{
  "cobolDebug.outputFormat": "ascii"
}
```

---

## 🚀 Instalación en Equipo

### Para un equipo de desarrollo:

#### Opción A: Compartir VSIX

1. Genera el .vsix una vez
2. Súbelo a un shared folder o repositorio interno
3. Cada desarrollador instala con:
   ```bash
   code --install-extension cobol-debug-0.1.0.vsix
   ```

#### Opción B: Script de Instalación

Crea `install.sh`:

```bash
#!/bin/bash
# Script de instalación COBOL Debug

echo "Instalando COBOL Debug Extension..."

# Verificar VSCode instalado
if ! command -v code &> /dev/null; then
    echo "Error: VSCode no está instalado"
    exit 1
fi

# Instalar extensión
code --install-extension cobol-debug-0.1.0.vsix

echo "✓ Instalación completa"
echo "Por favor reinicia VSCode"
```

Ejecuta:
```bash
chmod +x install.sh
./install.sh
```

#### Opción C: Configuración Compartida

En el repositorio del proyecto, incluye `.vscode/`:

```
.vscode/
├── settings.json          # Settings recomendados
├── extensions.json        # Lista de extensiones recomendadas
└── cobol-debug/          # Configuraciones pre-guardadas
    └── PROGRAMA1.debug.json
```

**extensions.json**:
```json
{
  "recommendations": [
    "ibm.zopeneditor",
    "your-org.cobol-debug"
  ]
}
```

---

## 🔄 Actualización de la Extensión

### Development Mode

```bash
git pull  # Si usas git
npm install
npm run compile
# Presiona F5 para recargar
```

### Local Installation

```bash
# 1. Desinstalar versión anterior
code --uninstall-extension cobol-debug

# 2. Instalar nueva versión
npm run package
code --install-extension cobol-debug-0.2.0.vsix

# 3. Recargar VSCode
```

### Verificar Versión

En VSCode:
1. Extensions
2. Busca "COBOL Debug"
3. Verás "v0.1.0" (o la versión instalada)

---

## ❓ Troubleshooting de Instalación

### Problema: "npm install" falla

**Síntoma:**
```
npm ERR! code ENOTARGET
```

**Solución:**
```bash
# Limpiar cache
npm cache clean --force

# Reinstalar
rm -rf node_modules package-lock.json
npm install
```

### Problema: "npm run compile" da errores

**Síntoma:**
```
error TS2307: Cannot find module 'vscode'
```

**Solución:**
```bash
npm install --save-dev @types/vscode
npm run compile
```

### Problema: Extensión no aparece después de instalar

**Síntoma:** No ves "COBOL Debug" en Extensions

**Soluciones:**
1. Reload Window: `Ctrl+Shift+P` → "Reload Window"
2. Verificar instalación:
   ```bash
   code --list-extensions | grep cobol
   ```
3. Reinstalar:
   ```bash
   code --uninstall-extension cobol-debug
   code --install-extension cobol-debug-0.1.0.vsix
   ```

### Problema: Comandos no funcionan

**Síntoma:** Click derecho no muestra menú "COBOL Debug"

**Verificaciones:**
1. ¿Estás en un archivo `.cbl`?
2. ¿El lenguaje está configurado como COBOL?
   - Bottom right de VSCode → Debe decir "COBOL"
3. ¿Tienes IBM Z Open Editor u otra extensión COBOL instalada?

**Solución:**
Si el lenguaje no es COBOL:
1. Click en el lenguaje (bottom right)
2. Busca "COBOL"
3. Selecciona "COBOL"

### Problema: Panel lateral no aparece

**Síntoma:** No ves ícono "CBL D" en Activity Bar

**Soluciones:**
1. Busca en la lista de views:
   - View → Open View → "COBOL Debug"
2. Verifica que la extensión esté activada:
   - Extensions → COBOL Debug → "Enable"

### Problema: "Command not found" al ejecutar comandos

**Síntoma:**
```
command 'cobolDebug.generateDebugVersion' not found
```

**Solución:**
La extensión no se activó correctamente:
1. Abre un archivo .cbl (activa la extensión)
2. Reload Window
3. Verifica en Output (Extension Host) si hay errores

---

## 📋 Checklist de Instalación Exitosa

Verifica que todo funcione:

### ✅ Checklist Básico

- [ ] Extensión aparece en lista de Extensions
- [ ] Ícono "CBL D" visible en Activity Bar
- [ ] Panel lateral se abre al hacer click en ícono
- [ ] Click derecho en variable muestra menú "COBOL Debug"
- [ ] Settings de "COBOL Debug" están disponibles

### ✅ Checklist de Funcionalidad

- [ ] Puedo agregar un watch variable
- [ ] El panel muestra el watch agregado
- [ ] Puedo generar versión DEBUG (Ctrl+Shift+D)
- [ ] Se crea archivo PROGRAMA-DEBUG.cbl
- [ ] El archivo generado tiene DISPLAYs con CCASTI
- [ ] Puedo remover debug points desde el panel

### ✅ Checklist Avanzado

- [ ] Track variable all occurrences funciona
- [ ] Debug variables in selection funciona
- [ ] Debug all variables in paragraph funciona
- [ ] Save/Load configuration funciona
- [ ] Navegación desde panel al código funciona

---

## 🎓 Capacitación del Equipo

### Sesión de Onboarding Recomendada (30-45 min)

**1. Introducción (5 min)**
- Qué es COBOL Debug
- Por qué lo necesitamos
- Flujo general de uso

**2. Demo en Vivo (15 min)**
- Abrir EJEMPLO1.cbl
- Agregar track variable
- Generar versión DEBUG
- Mostrar resultado esperado en SYSOUT

**3. Práctica Guiada (15 min)**
- Cada persona prueba con archivo de ejemplo
- Agrega varios debug points
- Usa el panel lateral
- Genera versión DEBUG

**4. Q&A y Tips (10 min)**
- Preguntas del equipo
- Tips avanzados
- Mejores prácticas

### Material de Capacitación

Compartir estos documentos:
1. README.md - Overview general
2. USAGE-GUIDE.md - Guía paso a paso
3. ADVANCED-COMMANDS.md - Comandos avanzados
4. PANEL-GUIDE.md - Uso del panel

---

## 📞 Soporte

### Para Problemas de Instalación

1. Revisa esta guía completa
2. Verifica logs en Output > Extension Host
3. Intenta reinstalación limpia
4. Si persiste, contacta al autor

### Para Bugs o Feature Requests

1. Documenta el problema con screenshots
2. Incluye versión de VSCode
3. Incluye versión de la extensión
4. Describe pasos para reproducir

---

## ✅ Siguiente Paso

Una vez instalado correctamente, ve a:
- **USAGE-GUIDE.md** - Para aprender a usar la extensión
- **test-programs/EJEMPLO1.cbl** - Para probar con ejemplo

¡Disfruta debuggeando COBOL sin editar manualmente el código! 🚀

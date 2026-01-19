# COBOL Debug

A powerful VSCode extension for debugging COBOL programs on mainframe systems by automatically instrumenting code with DISPLAY statements and parsing SYSOUT results.

## Features

### 🎯 Intelligent Debug Instrumentation

- **Watch Variables**: Monitor variable values at specific points
  - Current line watch
  - BEFORE/AFTER watch (see values before and after operations)
  
- **Variable Tracking**: Track a variable across all occurrences in your program
  - Automatically finds all MOVE, COMPUTE, ADD, etc. operations
  - Shows BEFORE and AFTER values for write operations
  - Shows current value for read-only operations (IF, EVALUATE)

- **Paragraph Tracing**: Trace entry and exit of COBOL paragraphs

### 📝 Clean Code Generation

- Generates a separate `-DEBUG.cbl` file (never modifies your original)
- Adds temporary variables to WORKING-STORAGE for tracking
- Uses configurable 6-character marker (default: `CCASTI`) in columns 1-6
- Formatted output optimized for SYSOUT readability

### 💾 Configuration Management

- Save/load debug configurations as JSON
- Persistent across sessions
- Easy to share with team members

### 📊 Debug Panel (Sidebar)

- Visual tree view of all debug points
- Quick navigation to code by clicking items
- Remove debug points with trash icon
- Summary with statistics
- Auto-refresh when configuration changes
- Expandable sections for detailed view

### 📊 SYSOUT Parser (Coming Soon)

- Automatic parsing of debug output
- Variable timeline visualization
- Quick navigation to code where values changed

## Usage

### Basic Workflow

1. **Open a COBOL file** in VSCode
2. **Add debug points** using context menu:
   - Select a variable → Right-click → "COBOL Debug" → Choose action
   - Click in a paragraph → Right-click → "COBOL Debug" → "Trace Paragraph"
3. **Review in sidebar panel**: Check the "COBOL Debug" view in the Activity Bar
4. **Generate debug version**: `Ctrl+Shift+D` (Mac: `Cmd+Shift+D`)
5. **Upload to mainframe** and compile using your JCL
6. **Execute** and download SYSOUT
7. **Analyze results** in the SYSOUT

### Context Menu Options

#### On Variable Selection:
- **Watch Variable (Current Line)**: Add DISPLAY at current line only
- **Watch Variable (BEFORE/AFTER)**: Show values before and after the operation
- **Track Variable (All Occurrences)**: Find and track variable throughout the program

#### On Paragraph:
- **Trace Paragraph (Entry/Exit)**: Add entry/exit displays
- **Debug All Variables in Paragraph**: Track all variables used in the paragraph

#### On Code Selection:
- **Debug Variables in Selection**: Extract and track variables in selected lines

### Example Output in SYSOUT

```
┌─ LINE 250: COMPUTE ──────────────────────────────────┐
│ BEFORE:  WS-MONTO = 0000000.00                        │
│          WS-BASE  = 0005250.00                        │
│ AFTER:   WS-MONTO = 0006247.50 (was: 0000000.00)     │
└───────────────────────────────────────────────────────┘

┌─ LINE 380: ADD ──────────────────────────────────────┐
│ BEFORE:  WS-MONTO   = 0006247.50                      │
│          WS-INTERES = 0000130.05                      │
│ AFTER:   WS-MONTO   = 0006377.55 (was: 0006247.50)   │
└───────────────────────────────────────────────────────┘
```

## Configuration

Access settings via: `Preferences > Settings > Extensions > COBOL Debug`

### Available Settings

- **Output Width** (default: 133): Width of SYSOUT display in columns
  - Options: 80, 120, 133

- **Output Format** (default: auto): Format for debug displays
  - `auto`: Auto-detect based on system
  - `unicode`: Use Unicode box-drawing characters (╔═╗)
  - `ascii`: Use ASCII characters (+==+)

- **Debug Marker** (default: CCASTI): 6-character marker for columns 1-6

- **Include Read-Only Operations** (default: true): Include IF/EVALUATE when tracking

- **Warn On Many Occurrences** (default: 50): Warn when tracking finds many occurrences

## Commands

- `COBOL Debug: Generate Debug Version` - Generate instrumented code
- `COBOL Debug: Save Debug Configuration` - Save current debug points
- `COBOL Debug: Load Debug Configuration` - Load saved configuration
- `COBOL Debug: Clear All Debug Points` - Remove all debug points
- `COBOL Debug: Parse SYSOUT` - Parse SYSOUT file (coming soon)

## Requirements

- VSCode 1.85.0 or higher
- COBOL language support (e.g., IBM Z Open Editor)
- Access to mainframe compilation and execution

## Known Limitations

- Currently supports fixed-format COBOL only (columns 7-72)
- Parser may not detect all variable types (complex structures, REDEFINES)
- SYSOUT parser not yet implemented (Phase 2)

## Development

### Building from Source

```bash
cd cobol-debug-extension
npm install
npm run compile
```

### Installing Locally

```bash
npm run package
code --install-extension cobol-debug-0.1.0.vsix
```

## Roadmap

- [ ] SYSOUT parser with timeline visualization
- [ ] Support for free-format COBOL
- [ ] Better detection of REDEFINES and complex structures
- [ ] Integration with Zowe for direct mainframe interaction
- [ ] Variable value change highlighting
- [ ] Export debug results to CSV/JSON

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

MIT

## Author

Nicolas - Banco del Estado de Chile

---

**Note**: This extension generates debug code for testing purposes. Always review the generated code before uploading to production systems.

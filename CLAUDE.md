# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

COBOL Debug is a VSCode extension that automates instrumentation of fixed-format COBOL programs with DISPLAY statements for mainframe debugging. It generates a separate `-DEBUG.cbl` file with formatted debug output, never modifying the original source.

## Build Commands

```bash
npm run compile    # Compile TypeScript to JavaScript (outputs to out/)
npm run watch      # Watch mode for development
npm run lint       # ESLint code quality checks
npm run package    # Create .vsix extension package
```

## Testing

Press F5 in VSCode to launch the Extension Development Host. Open a test COBOL file from `test-programs/` and test commands via the context menu or command palette (Ctrl+Shift+D to generate debug version).

## Architecture

```
src/
├── extension.ts      # Entry point, registers 12 VSCode commands, orchestrates all operations
├── types.ts          # TypeScript interfaces (DebugMode, OperationType, AccessType, etc.)
├── cobolParser.ts    # Parses fixed-format COBOL (columns 7-72), extracts variables/paragraphs
├── debugGenerator.ts # Generates instrumented code with formatted DISPLAY statements
├── debugState.ts     # Singleton state manager with EventEmitter for UI reactivity
├── debugPanel.ts     # TreeView provider for VSCode sidebar panel
└── configManager.ts  # Persists debug configs to .vscode/cobol-debug/{PROGRAM}.debug.json
```

**Data Flow**: User Selection → Command Handler → Parser (extract structure) → State Manager (store debug points) → Panel (display UI) → Generator (create -DEBUG.cbl) → Config Manager (persist)

## Key Technical Details

### Fixed-Format COBOL Handling
- Column 6: Indicator (* or / for comments)
- Columns 8-72: Code area (indices 7-71 in strings)
- Debug marker inserted in columns 1-6
- All generated code must respect this format

### Debug Modes
- `CURRENT_LINE`: Show value at specific line
- `BEFORE_AFTER`: Show value before and after operation
- Tracking: Find all occurrences of a variable across the program

### Code Generation
1. Parse program structure (variables, paragraphs)
2. Collect debug displays by line number
3. Generate temporary variables with `-DEBUG-BEFORE` suffix
4. Insert DISPLAY statements with box-drawing formatting
5. Output new file with `-DEBUG.cbl` suffix

## VSCode Extension Points

- **Activation**: `onLanguage:cobol`
- **Main Commands**: `generateDebugVersion` (Ctrl+Shift+D), `watchVariableCurrentLine`, `watchVariableBeforeAfter`, `trackVariableAllOccurrences`, `traceParagraph`
- **Settings**: `cobolDebug.outputWidth` (80/120/133), `cobolDebug.outputFormat` (unicode/ascii), `cobolDebug.debugMarker`

## Design Patterns

- **Singleton**: `DebugState.getInstance()` for global state
- **Observer**: EventEmitter notifies panel of state changes
- **Builder**: DebugGenerator constructs complex COBOL code
- **Strategy**: Different handling per debug mode

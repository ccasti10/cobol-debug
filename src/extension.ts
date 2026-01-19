/**
 * COBOL Debug Extension
 * Main entry point
 */

import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { DebugState } from './debugState';
import { CobolParser } from './cobolParser';
import { DebugGenerator } from './debugGenerator';
import { DebugMode, AccessType } from './types';
import { registerDebugPanel } from './debugPanel';

export function activate(context: vscode.ExtensionContext) {
    console.log('COBOL Debug extension is now active');

    // Initialize debug state
    const workspaceRoot = vscode.workspace.workspaceFolders?.[0].uri.fsPath;
    if (workspaceRoot) {
        DebugState.getInstance().initialize(workspaceRoot);
    }

    // Register debug panel (tree view)
    registerDebugPanel(context);

    // Register commands
    registerCommands(context);
}

function registerCommands(context: vscode.ExtensionContext) {
    // Generate Debug Version
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.generateDebugVersion', async () => {
            await generateDebugVersion();
        })
    );

    // Watch Variable (Current Line)
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.watchVariableCurrentLine', async () => {
            await watchVariable(DebugMode.CURRENT_LINE);
        })
    );

    // Watch Variable (BEFORE/AFTER)
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.watchVariableBeforeAfter', async () => {
            await watchVariable(DebugMode.BEFORE_AFTER);
        })
    );

    // Track Variable (All Occurrences)
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.trackVariableAllOccurrences', async () => {
            await trackVariableAll();
        })
    );

    // Trace Paragraph
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.traceParagraph', async () => {
            await traceParagraph();
        })
    );

    // Debug All Variables in Paragraph
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.debugAllVariablesInParagraph', async () => {
            await debugAllVariablesInParagraph();
        })
    );

    // Debug Variables in Selection
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.debugVariablesInSelection', async () => {
            await debugVariablesInSelection();
        })
    );

    // Remove from Debug
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.removeFromDebug', async () => {
            await removeFromDebug();
        })
    );

    // Clear All Debug Points
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.clearAllDebugPoints', async () => {
            const answer = await vscode.window.showWarningMessage(
                'Are you sure you want to clear all debug points?',
                'Yes', 'No'
            );
            if (answer === 'Yes') {
                DebugState.getInstance().clearAll();
                vscode.window.showInformationMessage('All debug points cleared');
            }
        })
    );

    // Save Configuration
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.saveConfiguration', async () => {
            const saved = await DebugState.getInstance().saveConfig();
            if (saved) {
                vscode.window.showInformationMessage('Debug configuration saved');
            }
        })
    );

    // Load Configuration
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.loadConfiguration', async () => {
            await loadConfiguration();
        })
    );
}

/**
 * Generate instrumented debug version of the current file
 */
async function generateDebugVersion() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        vscode.window.showErrorMessage('No active editor');
        return;
    }

    const document = editor.document;
    if (document.languageId !== 'cobol') {
        vscode.window.showErrorMessage('Not a COBOL file');
        return;
    }

    const state = DebugState.getInstance();
    const config = state.getConfig();

    if (!config) {
        vscode.window.showErrorMessage('No debug configuration. Please add debug points first.');
        return;
    }

    // Show preview
    const totalDisplays = state.estimateTotalDisplays();
    const answer = await vscode.window.showInformationMessage(
        `Ready to generate debug version:\n\n` +
        `• ${config.debugPoints.singlePoint.length} single point watches\n` +
        `• ${config.debugPoints.variableTracking.length} variable tracking\n` +
        `• ${config.debugPoints.paragraphs.length} paragraph traces\n\n` +
        `Total DISPLAY statements: ${totalDisplays}\n\n` +
        `Continue?`,
        'Generate', 'Show Details', 'Cancel'
    );

    if (answer === 'Cancel' || !answer) {
        return;
    }

    if (answer === 'Show Details') {
        // TODO: Show detailed preview
        return;
    }

    // Generate debug code
    const sourceContent = document.getText();
    const generator = new DebugGenerator();
    
    try {
        const debugCode = await generator.generate(sourceContent, config);

        // Determine output filename
        const sourcePath = document.uri.fsPath;
        const dir = path.dirname(sourcePath);
        const baseName = path.basename(sourcePath, '.cbl');
        const debugPath = path.join(dir, `${baseName}-DEBUG.cbl`);

        // Write debug file
        fs.writeFileSync(debugPath, debugCode, 'utf8');

        // Open the generated file
        const debugDoc = await vscode.workspace.openTextDocument(debugPath);
        await vscode.window.showTextDocument(debugDoc);

        vscode.window.showInformationMessage(`Debug version generated: ${path.basename(debugPath)}`);

        // Auto-save configuration
        await state.saveConfig();

    } catch (error) {
        vscode.window.showErrorMessage(`Failed to generate debug version: ${error}`);
    }
}

/**
 * Watch a variable at current line
 */
async function watchVariable(mode: DebugMode) {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return;
    }

    const selection = editor.selection;
    const selectedText = editor.document.getText(selection).trim();

    if (!selectedText) {
        vscode.window.showErrorMessage('Please select a variable name');
        return;
    }

    const state = DebugState.getInstance();
    let config = state.getConfig();

    // Create config if doesn't exist
    if (!config) {
        const document = editor.document;
        const content = document.getText();
        const program = CobolParser.parse(content);
        state.createNew(program.programName, path.basename(document.uri.fsPath));
        config = state.getConfig()!;
    }

    // Add watch
    state.addSinglePointWatch({
        variable: selectedText,
        line: selection.start.line,
        mode: mode,
        showRelatedVars: mode === DebugMode.BEFORE_AFTER
    });

    vscode.window.showInformationMessage(`Added watch for ${selectedText} at line ${selection.start.line + 1}`);
}

/**
 * Track variable in all occurrences
 */
async function trackVariableAll() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return;
    }

    const selection = editor.selection;
    const selectedText = editor.document.getText(selection).trim();

    if (!selectedText) {
        vscode.window.showErrorMessage('Please select a variable name');
        return;
    }

    const document = editor.document;
    const content = document.getText();
    const program = CobolParser.parse(content);

    // Find all occurrences
    const lines = content.split('\n');
    const occurrences = CobolParser.findVariableOccurrences(lines, selectedText, program.procedureDivisionStart);

    if (occurrences.length === 0) {
        vscode.window.showInformationMessage(`No occurrences found for ${selectedText}`);
        return;
    }

    // Warn if many occurrences
    const config = vscode.workspace.getConfiguration('cobolDebug');
    const warnThreshold = config.get('warnOnManyOccurrences', 50);
    
    if (occurrences.length > warnThreshold) {
        const answer = await vscode.window.showWarningMessage(
            `⚠️ Warning: ${selectedText} found in ${occurrences.length} locations.\n` +
            `This will generate ~${occurrences.length * 2} DISPLAY statements.\n\n` +
            `Continue?`,
            'Continue Anyway', 'Cancel'
        );

        if (answer !== 'Continue Anyway') {
            return;
        }
    }

    const state = DebugState.getInstance();
    let debugConfig = state.getConfig();

    // Create config if doesn't exist
    if (!debugConfig) {
        state.createNew(program.programName, path.basename(document.uri.fsPath));
        debugConfig = state.getConfig()!;
    }

    // Add tracking
    state.addVariableTracking({
        variable: selectedText,
        trackAllOccurrences: true,
        includeReadOnly: config.get('includeReadOnlyOperations', true),
        occurrences: occurrences
    });

    vscode.window.showInformationMessage(`Tracking ${selectedText} in ${occurrences.length} locations`);
}

/**
 * Trace paragraph entry/exit
 */
async function traceParagraph() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return;
    }

    const document = editor.document;
    const content = document.getText();
    const program = CobolParser.parse(content);

    // Get paragraph at current cursor position
    const cursorLine = editor.selection.start.line;
    const paragraph = CobolParser.getParagraphAtLine(program.paragraphs, cursorLine);

    if (!paragraph) {
        vscode.window.showErrorMessage('Cursor is not within a paragraph');
        return;
    }

    const state = DebugState.getInstance();
    let config = state.getConfig();

    // Create config if doesn't exist
    if (!config) {
        state.createNew(program.programName, path.basename(document.uri.fsPath));
        config = state.getConfig()!;
    }

    // Add trace
    state.addParagraphTrace({
        name: paragraph.name,
        startLine: paragraph.startLine,
        endLine: paragraph.endLine,
        traceEntry: true,
        traceExit: true
    });

    vscode.window.showInformationMessage(`Added trace for paragraph ${paragraph.name}`);
}

/**
 * Debug all variables in paragraph
 */
async function debugAllVariablesInParagraph() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return;
    }

    const document = editor.document;
    const content = document.getText();
    const program = CobolParser.parse(content);

    // Get paragraph at current cursor position
    const cursorLine = editor.selection.start.line;
    const paragraph = CobolParser.getParagraphAtLine(program.paragraphs, cursorLine);

    if (!paragraph) {
        vscode.window.showErrorMessage('Cursor is not within a paragraph');
        return;
    }

    // Extract all variables used in the paragraph
    const lines = content.split('\n');
    const detectedVars = new Set<string>();
    const allVars = program.variables.map(v => v.name);

    for (let i = paragraph.startLine; i <= paragraph.endLine; i++) {
        const line = lines[i];
        const codeLine = CobolParser.getCodeArea(line);
        
        // Find all words that could be variables
        const words = codeLine.match(/\b[A-Z][A-Z0-9\-]*\b/gi);
        if (words) {
            for (const word of words) {
                // Check if it's actually a defined variable
                if (allVars.some(v => v.toUpperCase() === word.toUpperCase())) {
                    if (!CobolParser.isCobolKeyword(word)) {
                        detectedVars.add(word);
                    }
                }
            }
        }
    }

    if (detectedVars.size === 0) {
        vscode.window.showInformationMessage(
            `No variables detected in paragraph ${paragraph.name}`
        );
        return;
    }

    // Show quick pick to select which variables to debug
    const varArray = Array.from(detectedVars);
    const selected = await vscode.window.showQuickPick(varArray, {
        canPickMany: true,
        placeHolder: `${varArray.length} variables detected in ${paragraph.name}. Select which ones to track:`,
        title: 'Debug Variables in Paragraph'
    });

    if (!selected || selected.length === 0) {
        return;
    }

    // Ask if they want to track all occurrences or just in this paragraph
    const scope = await vscode.window.showQuickPick([
        { 
            label: 'Track in entire program', 
            description: 'Find all occurrences throughout the program',
            value: 'all' 
        },
        { 
            label: 'Track only in this paragraph', 
            description: `Only track within ${paragraph.name}`,
            value: 'paragraph' 
        }
    ], {
        placeHolder: 'Select tracking scope'
    });

    if (!scope) {
        return;
    }

    const state = DebugState.getInstance();
    let config = state.getConfig();

    // Create config if doesn't exist
    if (!config) {
        state.createNew(program.programName, path.basename(document.uri.fsPath));
        config = state.getConfig()!;
    }

    const configObj = vscode.workspace.getConfiguration('cobolDebug');

    // Add tracking for each selected variable
    for (const varName of selected) {
        if (scope.value === 'all') {
            // Track in entire program
            const occurrences = CobolParser.findVariableOccurrences(
                lines, 
                varName, 
                program.procedureDivisionStart
            );

            if (occurrences.length > 0) {
                state.addVariableTracking({
                    variable: varName,
                    trackAllOccurrences: true,
                    includeReadOnly: configObj.get('includeReadOnlyOperations', true),
                    occurrences: occurrences
                });
            }
        } else {
            // Track only in paragraph
            const occurrences = CobolParser.findVariableOccurrences(
                lines, 
                varName, 
                program.procedureDivisionStart
            ).filter(occ => occ.line >= paragraph.startLine && occ.line <= paragraph.endLine);

            if (occurrences.length > 0) {
                state.addVariableTracking({
                    variable: varName,
                    trackAllOccurrences: false,
                    includeReadOnly: configObj.get('includeReadOnlyOperations', true),
                    occurrences: occurrences
                });
            }
        }
    }

    if (scope.value === 'all') {
        vscode.window.showInformationMessage(
            `Added tracking for ${selected.length} variable(s) in entire program`
        );
    } else {
        vscode.window.showInformationMessage(
            `Added tracking for ${selected.length} variable(s) in paragraph ${paragraph.name}`
        );
    }
}

/**
 * Debug variables in selection
 */
async function debugVariablesInSelection() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return;
    }

    const selection = editor.selection;
    if (selection.isEmpty) {
        vscode.window.showErrorMessage('Please select lines of code');
        return;
    }

    const document = editor.document;
    const content = document.getText();
    const program = CobolParser.parse(content);

    // Get selected lines
    const startLine = selection.start.line;
    const endLine = selection.end.line;
    const selectedLines: string[] = [];

    for (let i = startLine; i <= endLine; i++) {
        selectedLines.push(document.lineAt(i).text);
    }

    // Extract all variables from selected lines
    const detectedVars = new Set<string>();
    const allVars = program.variables.map(v => v.name);

    for (const line of selectedLines) {
        const codeLine = CobolParser.getCodeArea(line);
        
        // Find all words that could be variables
        const words = codeLine.match(/\b[A-Z][A-Z0-9\-]*\b/gi);
        if (words) {
            for (const word of words) {
                // Check if it's actually a defined variable
                if (allVars.some(v => v.toUpperCase() === word.toUpperCase())) {
                    if (!CobolParser.isCobolKeyword(word)) {
                        detectedVars.add(word);
                    }
                }
            }
        }
    }

    if (detectedVars.size === 0) {
        vscode.window.showInformationMessage('No variables detected in selection');
        return;
    }

    // Show quick pick to select which variables to debug
    const varArray = Array.from(detectedVars);
    const selected = await vscode.window.showQuickPick(varArray, {
        canPickMany: true,
        placeHolder: `${varArray.length} variables detected. Select which ones to debug:`,
        title: 'Debug Variables in Selection'
    });

    if (!selected || selected.length === 0) {
        return;
    }

    // Ask for debug mode
    const mode = await vscode.window.showQuickPick([
        { label: 'BEFORE only', value: DebugMode.BEFORE },
        { label: 'AFTER only', value: DebugMode.AFTER },
        { label: 'BEFORE/AFTER', value: DebugMode.BEFORE_AFTER }
    ], {
        placeHolder: 'Select debug mode'
    });

    if (!mode) {
        return;
    }

    const state = DebugState.getInstance();
    let config = state.getConfig();

    // Create config if doesn't exist
    if (!config) {
        state.createNew(program.programName, path.basename(document.uri.fsPath));
        config = state.getConfig()!;
    }

    // Add watch for each selected variable at the first line of selection
    for (const varName of selected) {
        state.addSinglePointWatch({
            variable: varName,
            line: startLine,
            mode: mode.value as DebugMode,
            showRelatedVars: mode.value === DebugMode.BEFORE_AFTER
        });
    }

    vscode.window.showInformationMessage(
        `Added ${selected.length} variable(s) to debug at line ${startLine + 1}`
    );
}

/**
 * Remove from debug
 */
async function removeFromDebug() {
    vscode.window.showInformationMessage('Remove from debug - Coming soon!');
    // TODO: Implement
}

/**
 * Load configuration
 */
async function loadConfiguration() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return;
    }

    const document = editor.document;
    const content = document.getText();
    const program = CobolParser.parse(content);

    const loaded = await DebugState.getInstance().loadConfig(program.programName);
    
    if (loaded) {
        vscode.window.showInformationMessage(`Configuration loaded for ${program.programName}`);
    } else {
        vscode.window.showWarningMessage(`No saved configuration found for ${program.programName}`);
    }
}

export function deactivate() {}

"use strict";
/**
 * Debug Code Generator
 * Generates instrumented COBOL code with DEBUG displays
 */
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.DebugGenerator = void 0;
const vscode = __importStar(require("vscode"));
const types_1 = require("./types");
const cobolParser_1 = require("./cobolParser");
class DebugGenerator {
    constructor() {
        this.config = vscode.workspace.getConfiguration('cobolDebug');
        this.marker = this.config.get('debugMarker', 'CCASTI');
        this.outputWidth = this.config.get('outputWidth', 133);
        const format = this.config.get('outputFormat', 'auto');
        this.useUnicode = format === 'unicode' || (format === 'auto' && this.detectUnicodeSupport());
    }
    /**
     * Generate instrumented COBOL code
     */
    async generate(sourceContent, debugConfig) {
        const lines = sourceContent.split('\n');
        const program = cobolParser_1.CobolParser.parse(sourceContent);
        // Build list of all debug displays to insert
        const displays = new Map();
        // Process single point watches
        for (const watch of debugConfig.debugPoints.singlePoint) {
            this.addSinglePointDisplays(displays, watch, lines, program);
        }
        // Process variable tracking
        for (const tracking of debugConfig.debugPoints.variableTracking) {
            this.addTrackingDisplays(displays, tracking, lines, program);
        }
        // Process paragraph traces
        for (const paragraph of debugConfig.debugPoints.paragraphs) {
            this.addParagraphDisplays(displays, paragraph);
        }
        // Insert debug displays and add temporary variables
        const result = this.insertDisplays(lines, displays, debugConfig, program);
        return result;
    }
    /**
     * Add displays for single point watches
     */
    addSinglePointDisplays(displays, watch, lines, program) {
        const line = watch.line;
        const variable = watch.variable;
        const mode = watch.mode;
        if (mode === 'BEFORE_AFTER') {
            // Add BEFORE display
            const beforeDisplays = this.generateBeforeDisplay(line, [variable], watch.showRelatedVars ? this.getRelatedVars(lines[line], variable) : []);
            this.addToDisplayMap(displays, line, beforeDisplays, 'BEFORE');
            // Add AFTER display
            const afterDisplays = this.generateAfterDisplay(line, [variable], variable);
            this.addToDisplayMap(displays, line, afterDisplays, 'AFTER');
        }
        else if (mode === 'CURRENT_LINE') {
            const singleDisplays = this.generateSingleDisplay(line, [variable]);
            this.addToDisplayMap(displays, line, singleDisplays, 'BEFORE');
        }
    }
    /**
     * Add displays for variable tracking
     */
    addTrackingDisplays(displays, tracking, lines, program) {
        const variable = tracking.variable;
        for (const occurrence of tracking.occurrences) {
            const line = occurrence.line;
            const isWrite = occurrence.type === types_1.AccessType.WRITE;
            const isRead = occurrence.type === types_1.AccessType.READ;
            if (isRead) {
                // Read-only: just show current value
                const readDisplays = this.generateReadOnlyDisplay(line, variable, occurrence.operation);
                this.addToDisplayMap(displays, line, readDisplays, 'BEFORE');
            }
            else if (isWrite) {
                // Write: show BEFORE and AFTER
                const relatedVars = occurrence.relatedVars || [];
                const beforeDisplays = this.generateBeforeDisplay(line, [variable, ...relatedVars], []);
                this.addToDisplayMap(displays, line, beforeDisplays, 'BEFORE');
                const afterDisplays = this.generateAfterDisplay(line, [variable], variable);
                this.addToDisplayMap(displays, line, afterDisplays, 'AFTER');
            }
        }
    }
    /**
     * Add displays for paragraph tracing
     */
    addParagraphDisplays(displays, paragraph) {
        if (paragraph.traceEntry) {
            const entryDisplays = this.generateParagraphEntry(paragraph.name, paragraph.startLine);
            this.addToDisplayMap(displays, paragraph.startLine, entryDisplays, 'AFTER');
        }
        if (paragraph.traceExit) {
            const exitDisplays = this.generateParagraphExit(paragraph.name, paragraph.endLine);
            this.addToDisplayMap(displays, paragraph.endLine, exitDisplays, 'BEFORE');
        }
    }
    /**
     * Generate BEFORE display statements
     */
    generateBeforeDisplay(line, variables, relatedVars) {
        const displays = [];
        const allVars = [...new Set([...variables, ...relatedVars])];
        displays.push(this.makeDisplayLine(`'${this.makeTopBorder('LINE ' + line + ': BEFORE')}'`));
        for (const v of allVars) {
            displays.push(this.makeDisplayLine(`'| BEFORE:  ${this.padVariable(v)} = ' ${v}`));
        }
        return displays;
    }
    /**
     * Generate AFTER display statements
     */
    generateAfterDisplay(line, variables, trackedVar) {
        const displays = [];
        for (const v of variables) {
            const beforeVar = this.getBeforeVarName(v);
            displays.push(this.makeDisplayLine(`'| AFTER:   ${this.padVariable(v)} = ' ${v} ' (was: ' ${beforeVar} ')'`));
        }
        displays.push(this.makeDisplayLine(`'${this.makeBottomBorder()}'`));
        return displays;
    }
    /**
     * Generate single point display
     */
    generateSingleDisplay(line, variables) {
        const displays = [];
        displays.push(this.makeDisplayLine(`'${this.makeTopBorder('LINE ' + line)}'`));
        for (const v of variables) {
            displays.push(this.makeDisplayLine(`'| ${this.padVariable(v)} = ' ${v}`));
        }
        displays.push(this.makeDisplayLine(`'${this.makeBottomBorder()}'`));
        return displays;
    }
    /**
     * Generate read-only display
     */
    generateReadOnlyDisplay(line, variable, operation) {
        const displays = [];
        displays.push(this.makeDisplayLine(`'${this.makeTopBorder('LINE ' + line + ': ' + operation + ' (READ ONLY)')}'`));
        displays.push(this.makeDisplayLine(`'| VALUE:   ${this.padVariable(variable)} = ' ${variable}`));
        displays.push(this.makeDisplayLine(`'${this.makeBottomBorder()}'`));
        return displays;
    }
    /**
     * Generate paragraph entry display
     */
    generateParagraphEntry(name, line) {
        const displays = [];
        displays.push(this.makeDisplayLine(`'>>> ENTERING: ${name} (LINE ${line})'`));
        return displays;
    }
    /**
     * Generate paragraph exit display
     */
    generateParagraphExit(name, line) {
        const displays = [];
        displays.push(this.makeDisplayLine(`'<<< EXITING: ${name}'`));
        return displays;
    }
    /**
     * Insert debug displays into source code
     */
    insertDisplays(lines, displays, debugConfig, program) {
        const result = [];
        const tempVars = this.collectTempVariables(debugConfig);
        // Copy lines up to WORKING-STORAGE
        for (let i = 0; i <= program.workingStorageStart; i++) {
            result.push(lines[i]);
        }
        // Add temporary variables right after WORKING-STORAGE SECTION
        result.push(this.marker + '*===== DEBUG TEMPORARY VARIABLES =====');
        for (const varName of tempVars) {
            // TODO: Need to detect actual PIC clause from original variable
            result.push(this.marker + ` 77  ${this.getBeforeVarName(varName).padEnd(30)} PIC X(50).`);
        }
        result.push(this.marker + '*====================================');
        // Copy rest of WORKING-STORAGE
        for (let i = program.workingStorageStart + 1; i < program.procedureDivisionStart; i++) {
            result.push(lines[i]);
        }
        // Copy PROCEDURE DIVISION with inserted displays
        for (let i = program.procedureDivisionStart; i < lines.length; i++) {
            // Check if we need to insert displays BEFORE this line
            if (displays.has(i)) {
                const lineDisplays = displays.get(i);
                for (const display of lineDisplays) {
                    if (!display.includes('AFTER') || i === program.procedureDivisionStart) {
                        result.push(display);
                    }
                }
                // Save BEFORE values
                for (const varName of tempVars) {
                    if (lineDisplays.some(d => d.includes(varName))) {
                        result.push(this.makeMoveStatement(varName, this.getBeforeVarName(varName)));
                    }
                }
            }
            // Original line
            result.push(lines[i]);
            // Check if we need to insert displays AFTER this line
            if (displays.has(i)) {
                const lineDisplays = displays.get(i);
                for (const display of lineDisplays) {
                    if (display.includes('AFTER') || display.includes('EXITING')) {
                        result.push(display);
                    }
                }
            }
        }
        return result.join('\n');
    }
    /**
     * Collect all variables that need temporary BEFORE versions
     */
    collectTempVariables(debugConfig) {
        const vars = new Set();
        for (const watch of debugConfig.debugPoints.singlePoint) {
            if (watch.mode === 'BEFORE_AFTER') {
                vars.add(watch.variable);
            }
        }
        for (const tracking of debugConfig.debugPoints.variableTracking) {
            vars.add(tracking.variable);
        }
        return vars;
    }
    /**
     * Helper methods
     */
    makeDisplayLine(content) {
        return `${this.marker} DISPLAY ${content}.`;
    }
    makeMoveStatement(from, to) {
        return `${this.marker} MOVE ${from} TO ${to}.`;
    }
    getBeforeVarName(varName) {
        // Truncate if needed to fit DEBUG-BEFORE suffix
        const maxLen = 30 - 13; // 30 is COBOL max, 13 is "-DEBUG-BEFORE"
        const truncated = varName.length > maxLen ? varName.substring(0, maxLen) : varName;
        return `${truncated}-DEBUG-BEFORE`;
    }
    padVariable(varName, width = 20) {
        if (varName.length > width) {
            return varName.substring(0, width - 3) + '...';
        }
        return varName.padEnd(width);
    }
    makeTopBorder(label) {
        if (this.useUnicode) {
            return `┌─ ${label} ${'─'.repeat(Math.max(0, this.outputWidth - label.length - 10))}┐`;
        }
        else {
            return `+-- ${label} ${'-'.repeat(Math.max(0, this.outputWidth - label.length - 10))}+`;
        }
    }
    makeBottomBorder() {
        if (this.useUnicode) {
            return `└${'─'.repeat(this.outputWidth - 2)}┘`;
        }
        else {
            return `+${'-'.repeat(this.outputWidth - 2)}+`;
        }
    }
    getRelatedVars(line, mainVar) {
        // Extract variables from the line
        return cobolParser_1.CobolParser.extractRelatedVariables(cobolParser_1.CobolParser.getCodeArea(line), mainVar);
    }
    addToDisplayMap(displays, line, newDisplays, position) {
        if (!displays.has(line)) {
            displays.set(line, []);
        }
        const existing = displays.get(line);
        // Add position marker to help with insertion order
        const marked = newDisplays.map(d => position === 'AFTER' ? d + ' /*AFTER*/' : d);
        existing.push(...marked);
    }
    detectUnicodeSupport() {
        // Simple heuristic: assume Unicode support on modern systems
        // Users can override in settings if needed
        return process.platform !== 'win32' || process.env.WT_SESSION !== undefined;
    }
}
exports.DebugGenerator = DebugGenerator;
//# sourceMappingURL=debugGenerator.js.map
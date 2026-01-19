/**
 * Types and interfaces for COBOL Debug extension
 */

/**
 * Type of debug operation
 */
export enum DebugMode {
    CURRENT_LINE = 'CURRENT_LINE',
    BEFORE = 'BEFORE',
    AFTER = 'AFTER',
    BEFORE_AFTER = 'BEFORE_AFTER'
}

/**
 * Type of COBOL operation
 */
export enum OperationType {
    MOVE = 'MOVE',
    COMPUTE = 'COMPUTE',
    ADD = 'ADD',
    SUBTRACT = 'SUBTRACT',
    MULTIPLY = 'MULTIPLY',
    DIVIDE = 'DIVIDE',
    IF = 'IF',
    EVALUATE = 'EVALUATE',
    PERFORM = 'PERFORM',
    READ = 'READ',
    WRITE = 'WRITE',
    REWRITE = 'REWRITE',
    UNKNOWN = 'UNKNOWN'
}

/**
 * Type of variable access
 */
export enum AccessType {
    READ = 'READ',
    WRITE = 'WRITE'
}

/**
 * Single debug point for a variable at a specific line
 */
export interface SinglePointWatch {
    variable: string;
    line: number;
    mode: DebugMode;
    showRelatedVars?: boolean;
}

/**
 * Occurrence of a variable in the code
 */
export interface VariableOccurrence {
    line: number;
    operation: OperationType;
    type: AccessType;
    relatedVars?: string[];
    lineText?: string;
}

/**
 * Variable tracking configuration
 */
export interface VariableTracking {
    variable: string;
    trackAllOccurrences: boolean;
    includeReadOnly: boolean;
    occurrences: VariableOccurrence[];
}

/**
 * Paragraph trace configuration
 */
export interface ParagraphTrace {
    name: string;
    startLine: number;
    endLine: number;
    traceEntry: boolean;
    traceExit: boolean;
}

/**
 * Complete debug configuration
 */
export interface DebugConfiguration {
    programName: string;
    sourceFile: string;
    lastModified: string;
    outputWidth: number;
    useUnicodeChars: boolean | null;
    debugPoints: {
        singlePoint: SinglePointWatch[];
        variableTracking: VariableTracking[];
        paragraphs: ParagraphTrace[];
    };
}

/**
 * COBOL variable definition
 */
export interface CobolVariable {
    name: string;
    level: number;
    line: number;
    picture?: string;
    value?: string;
    isGroup: boolean;
    parent?: string;
}

/**
 * COBOL paragraph definition
 */
export interface CobolParagraph {
    name: string;
    startLine: number;
    endLine: number;
}

/**
 * Parsed COBOL program structure
 */
export interface CobolProgram {
    programName: string;
    workingStorageStart: number;
    workingStorageEnd: number;
    procedureDivisionStart: number;
    variables: CobolVariable[];
    paragraphs: CobolParagraph[];
}

/**
 * Debug display statement to be generated
 */
export interface DebugDisplay {
    line: number;
    position: 'BEFORE' | 'AFTER' | 'ENTRY' | 'EXIT';
    variables: string[];
    label: string;
    marker: string;
}

/**
 * SYSOUT parsed value
 */
export interface SysoutValue {
    variable: string;
    line: number;
    moment: 'BEFORE' | 'AFTER' | 'READ';
    value: string;
    operation?: OperationType;
}

/**
 * Variable timeline for SYSOUT analysis
 */
export interface VariableTimeline {
    variable: string;
    changes: {
        line: number;
        beforeValue: string | null;
        afterValue: string;
        changed: boolean;
    }[];
    summary: {
        totalChanges: number;
        finalValue: string;
        firstChangeAt: number | null;
    };
}

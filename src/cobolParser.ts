/**
 * COBOL Parser
 * Parses fixed-format COBOL code to extract structure and variables
 */

import { CobolProgram, CobolVariable, CobolParagraph, OperationType, AccessType, VariableOccurrence } from './types';

export class CobolParser {
    /**
     * Parse a COBOL source file
     */
    public static parse(content: string): CobolProgram {
        const lines = content.split('\n');
        const program: CobolProgram = {
            programName: this.extractProgramName(lines),
            workingStorageStart: -1,
            workingStorageEnd: -1,
            procedureDivisionStart: -1,
            variables: [],
            paragraphs: []
        };

        // Find sections
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const codeLine = this.getCodeArea(line);

            if (codeLine.match(/^\s*WORKING-STORAGE\s+SECTION/i)) {
                program.workingStorageStart = i;
            } else if (program.workingStorageStart !== -1 && program.workingStorageEnd === -1) {
                if (codeLine.match(/^\s*PROCEDURE\s+DIVISION/i)) {
                    program.workingStorageEnd = i - 1;
                    program.procedureDivisionStart = i;
                }
            }
        }

        // Parse variables from WORKING-STORAGE
        if (program.workingStorageStart !== -1) {
            program.variables = this.parseVariables(lines, program.workingStorageStart, program.workingStorageEnd);
        }

        // Parse paragraphs from PROCEDURE DIVISION
        if (program.procedureDivisionStart !== -1) {
            program.paragraphs = this.parseParagraphs(lines, program.procedureDivisionStart);
        }

        return program;
    }

    /**
     * Extract program name from PROGRAM-ID
     */
    private static extractProgramName(lines: string[]): string {
        for (const line of lines) {
            const codeLine = this.getCodeArea(line);
            const match = codeLine.match(/PROGRAM-ID\.\s+(\S+)/i);
            if (match) {
                return match[1];
            }
        }
        return 'UNKNOWN';
    }

    /**
     * Parse variables from WORKING-STORAGE SECTION
     */
    private static parseVariables(lines: string[], start: number, end: number): CobolVariable[] {
        const variables: CobolVariable[] = [];
        const actualEnd = end === -1 ? lines.length : end;

        for (let i = start; i <= actualEnd; i++) {
            const line = lines[i];
            const codeLine = this.getCodeArea(line);

            // Match variable definition: 01/77/05 etc.
            const varMatch = codeLine.match(/^\s*(\d{2})\s+([A-Z0-9\-]+)(?:\s+PIC(?:TURE)?\s+([^\s\.]+))?/i);
            if (varMatch) {
                const level = parseInt(varMatch[1]);
                const name = varMatch[2];
                const picture = varMatch[3];

                variables.push({
                    name: name,
                    level: level,
                    line: i,
                    picture: picture,
                    isGroup: !picture && level < 77
                });
            }
        }

        return variables;
    }

    /**
     * Parse paragraphs from PROCEDURE DIVISION
     */
    private static parseParagraphs(lines: string[], start: number): CobolParagraph[] {
        const paragraphs: CobolParagraph[] = [];
        let currentParagraph: CobolParagraph | null = null;

        for (let i = start; i < lines.length; i++) {
            const line = lines[i];
            const codeLine = this.getCodeArea(line);

            // Match paragraph name (starts in column 8, ends with period, no leading spaces in code area)
            const paragraphMatch = codeLine.match(/^([A-Z0-9\-]+)\.\s*$/i);
            if (paragraphMatch) {
                // Close previous paragraph
                if (currentParagraph) {
                    currentParagraph.endLine = i - 1;
                    paragraphs.push(currentParagraph);
                }

                // Start new paragraph
                currentParagraph = {
                    name: paragraphMatch[1],
                    startLine: i,
                    endLine: -1
                };
            }
        }

        // Close last paragraph
        if (currentParagraph) {
            currentParagraph.endLine = lines.length - 1;
            paragraphs.push(currentParagraph);
        }

        return paragraphs;
    }

    /**
     * Find all occurrences of a variable in the code
     */
    public static findVariableOccurrences(lines: string[], variable: string, procedureStart: number): VariableOccurrence[] {
        const occurrences: VariableOccurrence[] = [];
        const varPattern = new RegExp(`\\b${variable}\\b`, 'i');

        for (let i = procedureStart; i < lines.length; i++) {
            const line = lines[i];
            const codeLine = this.getCodeArea(line);

            if (varPattern.test(codeLine)) {
                const operation = this.detectOperation(codeLine);
                const accessType = this.determineAccessType(codeLine, variable, operation);
                const relatedVars = this.extractRelatedVariables(codeLine, variable);

                occurrences.push({
                    line: i,
                    operation: operation,
                    type: accessType,
                    relatedVars: relatedVars,
                    lineText: codeLine.trim()
                });
            }
        }

        return occurrences;
    }

    /**
     * Detect the type of operation in a line
     */
    private static detectOperation(codeLine: string): OperationType {
        const upperLine = codeLine.toUpperCase();

        if (upperLine.includes(' MOVE ')) return OperationType.MOVE;
        if (upperLine.includes(' COMPUTE ')) return OperationType.COMPUTE;
        if (upperLine.includes(' ADD ')) return OperationType.ADD;
        if (upperLine.includes(' SUBTRACT ')) return OperationType.SUBTRACT;
        if (upperLine.includes(' MULTIPLY ')) return OperationType.MULTIPLY;
        if (upperLine.includes(' DIVIDE ')) return OperationType.DIVIDE;
        if (upperLine.match(/^\s*IF\s/)) return OperationType.IF;
        if (upperLine.includes(' EVALUATE ')) return OperationType.EVALUATE;
        if (upperLine.includes(' PERFORM ')) return OperationType.PERFORM;
        if (upperLine.includes(' READ ')) return OperationType.READ;
        if (upperLine.match(/^\s*WRITE\s/)) return OperationType.WRITE;
        if (upperLine.includes(' REWRITE ')) return OperationType.REWRITE;

        return OperationType.UNKNOWN;
    }

    /**
     * Determine if variable is being read or written
     */
    private static determineAccessType(codeLine: string, variable: string, operation: OperationType): AccessType {
        const upperLine = codeLine.toUpperCase();
        const upperVar = variable.toUpperCase();

        // Read-only operations
        if (operation === OperationType.IF || operation === OperationType.EVALUATE) {
            return AccessType.READ;
        }

        // Check MOVE pattern: MOVE source TO target
        if (operation === OperationType.MOVE) {
            const moveMatch = upperLine.match(/MOVE\s+(.+?)\s+TO\s+(.+)/);
            if (moveMatch) {
                const target = moveMatch[2];
                if (target.includes(upperVar)) {
                    return AccessType.WRITE;
                }
            }
            return AccessType.READ;
        }

        // COMPUTE, ADD TO, SUBTRACT FROM, etc. - target is written
        if (operation === OperationType.COMPUTE) {
            const computeMatch = upperLine.match(/COMPUTE\s+(\S+)\s*=/);
            if (computeMatch && computeMatch[1] === upperVar) {
                return AccessType.WRITE;
            }
        }

        if (operation === OperationType.ADD) {
            if (upperLine.match(new RegExp(`ADD\\s+.+\\s+TO\\s+${upperVar}`, 'i'))) {
                return AccessType.WRITE;
            }
        }

        // Default: assume write for arithmetic operations
        if ([OperationType.ADD, OperationType.SUBTRACT, OperationType.MULTIPLY, OperationType.DIVIDE].includes(operation)) {
            return AccessType.WRITE;
        }

        return AccessType.READ;
    }

    /**
     * Extract related variables from a statement
     */
    public static extractRelatedVariables(codeLine: string, mainVariable: string): string[] {
        const variables: string[] = [];
        
        // Simple pattern to match COBOL variable names (letters, numbers, hyphens)
        const varPattern = /\b[A-Z][A-Z0-9\-]*\b/gi;
        const matches = codeLine.match(varPattern);

        if (matches) {
            for (const match of matches) {
                // Exclude COBOL keywords and the main variable
                if (!this.isCobolKeyword(match) && match.toUpperCase() !== mainVariable.toUpperCase()) {
                    if (!variables.includes(match)) {
                        variables.push(match);
                    }
                }
            }
        }

        return variables;
    }

    /**
     * Check if a word is a COBOL keyword
     */
    public static isCobolKeyword(word: string): boolean {
        const keywords = [
            'MOVE', 'TO', 'COMPUTE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
            'IF', 'ELSE', 'END-IF', 'EVALUATE', 'WHEN', 'PERFORM', 'UNTIL',
            'VARYING', 'FROM', 'BY', 'AFTER', 'BEFORE', 'GIVING', 'INTO',
            'READ', 'WRITE', 'REWRITE', 'DELETE', 'START', 'OPEN', 'CLOSE',
            'ACCEPT', 'DISPLAY', 'STOP', 'RUN', 'EXIT', 'PROGRAM', 'GOBACK',
            'THRU', 'THROUGH', 'TIMES', 'NOT', 'AND', 'OR', 'EQUAL', 'GREATER',
            'LESS', 'THAN', 'ZERO', 'ZEROS', 'ZEROES', 'SPACE', 'SPACES'
        ];
        return keywords.includes(word.toUpperCase());
    }

    /**
     * Get code area from fixed-format COBOL line (columns 8-72)
     */
    public static getCodeArea(line: string): string {
        if (line.length < 7) {
            return '';
        }
        // Column 7 is indicator (usually space or *)
        const indicator = line[6];
        if (indicator === '*' || indicator === '/') {
            return ''; // Comment line
        }
        // Code is from column 8 to 72 (indices 7-71)
        return line.substring(7, Math.min(72, line.length));
    }

    /**
     * Check if a line is a comment
     */
    public static isComment(line: string): boolean {
        if (line.length < 7) {
            return false;
        }
        const indicator = line[6];
        return indicator === '*' || indicator === '/';
    }

    /**
     * Get paragraph at a specific line
     */
    public static getParagraphAtLine(paragraphs: CobolParagraph[], line: number): CobolParagraph | null {
        for (const paragraph of paragraphs) {
            if (line >= paragraph.startLine && line <= paragraph.endLine) {
                return paragraph;
            }
        }
        return null;
    }
}

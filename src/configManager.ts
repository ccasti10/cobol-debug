/**
 * Configuration Manager
 * Handles loading and saving debug configurations to JSON files
 */

import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import { DebugConfiguration } from './types';

export class ConfigManager {
    private workspaceRoot: string;
    private configDir: string;

    constructor(workspaceRoot: string) {
        this.workspaceRoot = workspaceRoot;
        this.configDir = path.join(workspaceRoot, '.vscode', 'cobol-debug');
        this.ensureConfigDir();
    }

    /**
     * Ensure config directory exists
     */
    private ensureConfigDir(): void {
        if (!fs.existsSync(this.configDir)) {
            fs.mkdirSync(this.configDir, { recursive: true });
        }
    }

    /**
     * Get config file path for a program
     */
    private getConfigPath(programName: string): string {
        return path.join(this.configDir, `${programName}.debug.json`);
    }

    /**
     * Load debug configuration for a program
     */
    public async load(programName: string): Promise<DebugConfiguration | null> {
        const configPath = this.getConfigPath(programName);

        if (!fs.existsSync(configPath)) {
            return null;
        }

        try {
            const content = fs.readFileSync(configPath, 'utf8');
            const config: DebugConfiguration = JSON.parse(content);
            return config;
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to load debug configuration: ${error}`);
            return null;
        }
    }

    /**
     * Save debug configuration for a program
     */
    public async save(config: DebugConfiguration): Promise<boolean> {
        const configPath = this.getConfigPath(config.programName);

        try {
            const content = JSON.stringify(config, null, 2);
            fs.writeFileSync(configPath, content, 'utf8');
            vscode.window.showInformationMessage(`Debug configuration saved: ${path.basename(configPath)}`);
            return true;
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to save debug configuration: ${error}`);
            return false;
        }
    }

    /**
     * Check if configuration exists for a program
     */
    public exists(programName: string): boolean {
        return fs.existsSync(this.getConfigPath(programName));
    }

    /**
     * Delete configuration for a program
     */
    public async delete(programName: string): Promise<boolean> {
        const configPath = this.getConfigPath(programName);

        if (!fs.existsSync(configPath)) {
            return true;
        }

        try {
            fs.unlinkSync(configPath);
            return true;
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to delete debug configuration: ${error}`);
            return false;
        }
    }

    /**
     * List all debug configurations
     */
    public async listAll(): Promise<string[]> {
        if (!fs.existsSync(this.configDir)) {
            return [];
        }

        try {
            const files = fs.readdirSync(this.configDir);
            return files
                .filter(f => f.endsWith('.debug.json'))
                .map(f => f.replace('.debug.json', ''));
        } catch (error) {
            return [];
        }
    }

    /**
     * Create empty configuration
     */
    public createEmpty(programName: string, sourceFile: string): DebugConfiguration {
        const config = vscode.workspace.getConfiguration('cobolDebug');
        
        return {
            programName: programName,
            sourceFile: sourceFile,
            lastModified: new Date().toISOString(),
            outputWidth: config.get('outputWidth', 133),
            useUnicodeChars: null,
            debugPoints: {
                singlePoint: [],
                variableTracking: [],
                paragraphs: []
            }
        };
    }
}

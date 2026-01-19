/**
 * Debug State Manager
 * Maintains the current debug configuration in memory
 */

import * as vscode from 'vscode';
import { DebugConfiguration, SinglePointWatch, VariableTracking, ParagraphTrace } from './types';
import { ConfigManager } from './configManager';

export class DebugState {
    private static instance: DebugState;
    private currentConfig: DebugConfiguration | null = null;
    private configManager: ConfigManager | null = null;
    private _onDidChangeState: vscode.EventEmitter<void> = new vscode.EventEmitter<void>();
    public readonly onDidChangeState: vscode.Event<void> = this._onDidChangeState.event;

    private constructor() {}

    public static getInstance(): DebugState {
        if (!DebugState.instance) {
            DebugState.instance = new DebugState();
        }
        return DebugState.instance;
    }

    public initialize(workspaceRoot: string): void {
        this.configManager = new ConfigManager(workspaceRoot);
    }

    public getConfig(): DebugConfiguration | null {
        return this.currentConfig;
    }

    public setConfig(config: DebugConfiguration): void {
        this.currentConfig = config;
        this.notifyChange();
    }

    public async loadConfig(programName: string): Promise<boolean> {
        if (!this.configManager) {
            return false;
        }

        const config = await this.configManager.load(programName);
        if (config) {
            this.currentConfig = config;
            this.notifyChange();
            return true;
        }
        return false;
    }

    public async saveConfig(): Promise<boolean> {
        if (!this.configManager || !this.currentConfig) {
            return false;
        }

        this.currentConfig.lastModified = new Date().toISOString();
        return await this.configManager.save(this.currentConfig);
    }

    public createNew(programName: string, sourceFile: string): void {
        if (!this.configManager) {
            return;
        }

        this.currentConfig = this.configManager.createEmpty(programName, sourceFile);
        this.notifyChange();
    }

    // Add debug points
    public addSinglePointWatch(watch: SinglePointWatch): void {
        if (!this.currentConfig) {
            return;
        }
        
        // Check for duplicates
        const exists = this.currentConfig.debugPoints.singlePoint.some(
            w => w.variable === watch.variable && w.line === watch.line
        );
        
        if (!exists) {
            this.currentConfig.debugPoints.singlePoint.push(watch);
            this.notifyChange();
        }
    }

    public addVariableTracking(tracking: VariableTracking): void {
        if (!this.currentConfig) {
            return;
        }

        // Remove existing tracking for same variable
        this.currentConfig.debugPoints.variableTracking = 
            this.currentConfig.debugPoints.variableTracking.filter(t => t.variable !== tracking.variable);
        
        this.currentConfig.debugPoints.variableTracking.push(tracking);
        this.notifyChange();
    }

    public addParagraphTrace(trace: ParagraphTrace): void {
        if (!this.currentConfig) {
            return;
        }

        // Check for duplicates
        const exists = this.currentConfig.debugPoints.paragraphs.some(
            p => p.name === trace.name
        );

        if (!exists) {
            this.currentConfig.debugPoints.paragraphs.push(trace);
            this.notifyChange();
        }
    }

    // Remove debug points
    public removeSinglePointWatch(variable: string, line: number): void {
        if (!this.currentConfig) {
            return;
        }

        this.currentConfig.debugPoints.singlePoint = 
            this.currentConfig.debugPoints.singlePoint.filter(
                w => !(w.variable === variable && w.line === line)
            );
        this.notifyChange();
    }

    public removeVariableTracking(variable: string): void {
        if (!this.currentConfig) {
            return;
        }

        this.currentConfig.debugPoints.variableTracking = 
            this.currentConfig.debugPoints.variableTracking.filter(t => t.variable !== variable);
        this.notifyChange();
    }

    public removeParagraphTrace(name: string): void {
        if (!this.currentConfig) {
            return;
        }

        this.currentConfig.debugPoints.paragraphs = 
            this.currentConfig.debugPoints.paragraphs.filter(p => p.name !== name);
        this.notifyChange();
    }

    public clearAll(): void {
        if (!this.currentConfig) {
            return;
        }

        this.currentConfig.debugPoints = {
            singlePoint: [],
            variableTracking: [],
            paragraphs: []
        };
        this.notifyChange();
    }

    // Query methods
    public getTotalDebugPoints(): number {
        if (!this.currentConfig) {
            return 0;
        }

        return this.currentConfig.debugPoints.singlePoint.length +
               this.currentConfig.debugPoints.variableTracking.length +
               this.currentConfig.debugPoints.paragraphs.length;
    }

    public estimateTotalDisplays(): number {
        if (!this.currentConfig) {
            return 0;
        }

        let total = 0;

        // Single point watches
        for (const watch of this.currentConfig.debugPoints.singlePoint) {
            total += watch.mode === 'BEFORE_AFTER' ? 6 : 3;
        }

        // Variable tracking
        for (const tracking of this.currentConfig.debugPoints.variableTracking) {
            for (const occurrence of tracking.occurrences) {
                total += occurrence.type === 'WRITE' ? 6 : 3;
            }
        }

        // Paragraph traces
        total += this.currentConfig.debugPoints.paragraphs.length * 2;

        return total;
    }

    private notifyChange(): void {
        this._onDidChangeState.fire();
    }
}

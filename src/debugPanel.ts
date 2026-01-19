/**
 * Debug Panel - Tree View Provider
 * Displays debug configuration in sidebar
 */

import * as vscode from 'vscode';
import { DebugState } from './debugState';
import { DebugConfiguration, SinglePointWatch, VariableTracking, ParagraphTrace } from './types';

/**
 * Tree item types
 */
enum TreeItemType {
    ROOT_SINGLE_POINT = 'root_single_point',
    ROOT_TRACKING = 'root_tracking',
    ROOT_PARAGRAPHS = 'root_paragraphs',
    ROOT_SUMMARY = 'root_summary',
    SINGLE_POINT_ITEM = 'single_point_item',
    TRACKING_ITEM = 'tracking_item',
    PARAGRAPH_ITEM = 'paragraph_item',
    OCCURRENCE_ITEM = 'occurrence_item',
    SUMMARY_ITEM = 'summary_item'
}

/**
 * Custom tree item with metadata
 */
class DebugTreeItem extends vscode.TreeItem {
    constructor(
        public readonly label: string,
        public readonly collapsibleState: vscode.TreeItemCollapsibleState,
        public readonly itemType: TreeItemType,
        public readonly metadata?: any
    ) {
        super(label, collapsibleState);
        this.setupItem();
    }

    private setupItem(): void {
        switch (this.itemType) {
            case TreeItemType.ROOT_SINGLE_POINT:
                this.iconPath = new vscode.ThemeIcon('debug-breakpoint');
                this.contextValue = 'rootSinglePoint';
                break;

            case TreeItemType.ROOT_TRACKING:
                this.iconPath = new vscode.ThemeIcon('eye');
                this.contextValue = 'rootTracking';
                break;

            case TreeItemType.ROOT_PARAGRAPHS:
                this.iconPath = new vscode.ThemeIcon('symbol-method');
                this.contextValue = 'rootParagraphs';
                break;

            case TreeItemType.ROOT_SUMMARY:
                this.iconPath = new vscode.ThemeIcon('info');
                this.contextValue = 'rootSummary';
                break;

            case TreeItemType.SINGLE_POINT_ITEM:
                this.iconPath = new vscode.ThemeIcon('circle-outline');
                this.contextValue = 'singlePointItem';
                this.setupSinglePointItem();
                break;

            case TreeItemType.TRACKING_ITEM:
                this.iconPath = new vscode.ThemeIcon('eye-watch');
                this.contextValue = 'trackingItem';
                this.setupTrackingItem();
                break;

            case TreeItemType.PARAGRAPH_ITEM:
                this.iconPath = new vscode.ThemeIcon('symbol-method');
                this.contextValue = 'paragraphItem';
                this.setupParagraphItem();
                break;

            case TreeItemType.OCCURRENCE_ITEM:
                this.iconPath = new vscode.ThemeIcon('go-to-file');
                this.contextValue = 'occurrenceItem';
                this.setupOccurrenceItem();
                break;

            case TreeItemType.SUMMARY_ITEM:
                this.iconPath = new vscode.ThemeIcon('lightbulb');
                this.contextValue = 'summaryItem';
                break;
        }
    }

    private setupSinglePointItem(): void {
        const watch: SinglePointWatch = this.metadata;
        this.tooltip = `Variable: ${watch.variable}\nLine: ${watch.line + 1}\nMode: ${watch.mode}`;
        this.description = `Line ${watch.line + 1}`;
        
        // Make clickable to jump to line
        this.command = {
            command: 'cobolDebug.goToLine',
            title: 'Go to Line',
            arguments: [watch.line]
        };
    }

    private setupTrackingItem(): void {
        const tracking: VariableTracking = this.metadata;
        this.tooltip = `Tracking ${tracking.variable} in ${tracking.occurrences.length} locations`;
        this.description = `${tracking.occurrences.length} occurrences`;
    }

    private setupParagraphItem(): void {
        const paragraph: ParagraphTrace = this.metadata;
        this.tooltip = `Paragraph: ${paragraph.name}\nLines: ${paragraph.startLine + 1}-${paragraph.endLine + 1}`;
        this.description = `Lines ${paragraph.startLine + 1}-${paragraph.endLine + 1}`;
        
        // Make clickable to jump to paragraph
        this.command = {
            command: 'cobolDebug.goToLine',
            title: 'Go to Line',
            arguments: [paragraph.startLine]
        };
    }

    private setupOccurrenceItem(): void {
        const occurrence = this.metadata;
        this.tooltip = `Line ${occurrence.line + 1}: ${occurrence.operation} (${occurrence.type})`;
        
        // Make clickable to jump to line
        this.command = {
            command: 'cobolDebug.goToLine',
            title: 'Go to Line',
            arguments: [occurrence.line]
        };
    }
}

/**
 * Debug Configuration Tree Data Provider
 */
export class DebugConfigProvider implements vscode.TreeDataProvider<DebugTreeItem> {
    private _onDidChangeTreeData: vscode.EventEmitter<DebugTreeItem | undefined | null | void> = 
        new vscode.EventEmitter<DebugTreeItem | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<DebugTreeItem | undefined | null | void> = 
        this._onDidChangeTreeData.event;

    constructor() {
        // Listen to debug state changes
        DebugState.getInstance().onDidChangeState(() => {
            this.refresh();
        });
    }

    refresh(): void {
        this._onDidChangeTreeData.fire();
    }

    getTreeItem(element: DebugTreeItem): vscode.TreeItem {
        return element;
    }

    getChildren(element?: DebugTreeItem): Thenable<DebugTreeItem[]> {
        const config = DebugState.getInstance().getConfig();

        if (!element) {
            // Root level
            return Promise.resolve(this.getRootItems(config));
        }

        // Child items
        switch (element.itemType) {
            case TreeItemType.ROOT_SINGLE_POINT:
                return Promise.resolve(this.getSinglePointItems(config));

            case TreeItemType.ROOT_TRACKING:
                return Promise.resolve(this.getTrackingItems(config));

            case TreeItemType.ROOT_PARAGRAPHS:
                return Promise.resolve(this.getParagraphItems(config));

            case TreeItemType.ROOT_SUMMARY:
                return Promise.resolve(this.getSummaryItems(config));

            case TreeItemType.TRACKING_ITEM:
                return Promise.resolve(this.getOccurrenceItems(element.metadata));

            default:
                return Promise.resolve([]);
        }
    }

    private getRootItems(config: DebugConfiguration | null): DebugTreeItem[] {
        if (!config) {
            return [
                new DebugTreeItem(
                    'No debug configuration',
                    vscode.TreeItemCollapsibleState.None,
                    TreeItemType.SUMMARY_ITEM
                )
            ];
        }

        const items: DebugTreeItem[] = [];

        // Summary
        items.push(new DebugTreeItem(
            'Summary',
            vscode.TreeItemCollapsibleState.Expanded,
            TreeItemType.ROOT_SUMMARY,
            config
        ));

        // Single Point Watches
        const singlePointCount = config.debugPoints.singlePoint.length;
        if (singlePointCount > 0) {
            items.push(new DebugTreeItem(
                `Single Point Watches (${singlePointCount})`,
                vscode.TreeItemCollapsibleState.Expanded,
                TreeItemType.ROOT_SINGLE_POINT,
                config
            ));
        }

        // Variable Tracking
        const trackingCount = config.debugPoints.variableTracking.length;
        if (trackingCount > 0) {
            items.push(new DebugTreeItem(
                `Variable Tracking (${trackingCount})`,
                vscode.TreeItemCollapsibleState.Expanded,
                TreeItemType.ROOT_TRACKING,
                config
            ));
        }

        // Paragraph Traces
        const paragraphCount = config.debugPoints.paragraphs.length;
        if (paragraphCount > 0) {
            items.push(new DebugTreeItem(
                `Paragraph Traces (${paragraphCount})`,
                vscode.TreeItemCollapsibleState.Expanded,
                TreeItemType.ROOT_PARAGRAPHS,
                config
            ));
        }

        return items;
    }

    private getSummaryItems(config: DebugConfiguration | null): DebugTreeItem[] {
        if (!config) {
            return [];
        }

        const items: DebugTreeItem[] = [];
        const state = DebugState.getInstance();

        // Program name
        items.push(new DebugTreeItem(
            `Program: ${config.programName}`,
            vscode.TreeItemCollapsibleState.None,
            TreeItemType.SUMMARY_ITEM
        ));

        // Total debug points
        const totalPoints = state.getTotalDebugPoints();
        items.push(new DebugTreeItem(
            `Total debug points: ${totalPoints}`,
            vscode.TreeItemCollapsibleState.None,
            TreeItemType.SUMMARY_ITEM
        ));

        // Estimated DISPLAYs
        const totalDisplays = state.estimateTotalDisplays();
        items.push(new DebugTreeItem(
            `Est. DISPLAY statements: ${totalDisplays}`,
            vscode.TreeItemCollapsibleState.None,
            TreeItemType.SUMMARY_ITEM
        ));

        // Last modified
        if (config.lastModified) {
            const date = new Date(config.lastModified);
            items.push(new DebugTreeItem(
                `Last modified: ${date.toLocaleString()}`,
                vscode.TreeItemCollapsibleState.None,
                TreeItemType.SUMMARY_ITEM
            ));
        }

        return items;
    }

    private getSinglePointItems(config: DebugConfiguration | null): DebugTreeItem[] {
        if (!config) {
            return [];
        }

        return config.debugPoints.singlePoint.map(watch => 
            new DebugTreeItem(
                `${watch.variable} (${watch.mode})`,
                vscode.TreeItemCollapsibleState.None,
                TreeItemType.SINGLE_POINT_ITEM,
                watch
            )
        );
    }

    private getTrackingItems(config: DebugConfiguration | null): DebugTreeItem[] {
        if (!config) {
            return [];
        }

        return config.debugPoints.variableTracking.map(tracking => 
            new DebugTreeItem(
                tracking.variable,
                vscode.TreeItemCollapsibleState.Collapsed,
                TreeItemType.TRACKING_ITEM,
                tracking
            )
        );
    }

    private getParagraphItems(config: DebugConfiguration | null): DebugTreeItem[] {
        if (!config) {
            return [];
        }

        return config.debugPoints.paragraphs.map(paragraph => 
            new DebugTreeItem(
                paragraph.name,
                vscode.TreeItemCollapsibleState.None,
                TreeItemType.PARAGRAPH_ITEM,
                paragraph
            )
        );
    }

    private getOccurrenceItems(tracking: VariableTracking): DebugTreeItem[] {
        return tracking.occurrences.map(occurrence => 
            new DebugTreeItem(
                `Line ${occurrence.line + 1}: ${occurrence.operation} (${occurrence.type})`,
                vscode.TreeItemCollapsibleState.None,
                TreeItemType.OCCURRENCE_ITEM,
                occurrence
            )
        );
    }
}

/**
 * Register debug panel and related commands
 */
export function registerDebugPanel(context: vscode.ExtensionContext): void {
    // Create tree view provider
    const treeDataProvider = new DebugConfigProvider();
    
    // Register tree view
    const treeView = vscode.window.createTreeView('cobolDebugConfiguration', {
        treeDataProvider: treeDataProvider,
        showCollapseAll: true
    });

    context.subscriptions.push(treeView);

    // Register refresh command
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.refreshPanel', () => {
            treeDataProvider.refresh();
        })
    );

    // Register go to line command
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.goToLine', async (line: number) => {
            const editor = vscode.window.activeTextEditor;
            if (editor) {
                const position = new vscode.Position(line, 0);
                editor.selection = new vscode.Selection(position, position);
                editor.revealRange(
                    new vscode.Range(position, position),
                    vscode.TextEditorRevealType.InCenter
                );
            }
        })
    );

    // Register remove single point command
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.removeSinglePoint', (item: DebugTreeItem) => {
            const watch: SinglePointWatch = item.metadata;
            DebugState.getInstance().removeSinglePointWatch(watch.variable, watch.line);
            vscode.window.showInformationMessage(`Removed watch for ${watch.variable} at line ${watch.line + 1}`);
        })
    );

    // Register remove tracking command
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.removeTracking', (item: DebugTreeItem) => {
            const tracking: VariableTracking = item.metadata;
            DebugState.getInstance().removeVariableTracking(tracking.variable);
            vscode.window.showInformationMessage(`Removed tracking for ${tracking.variable}`);
        })
    );

    // Register remove paragraph command
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.removeParagraph', (item: DebugTreeItem) => {
            const paragraph: ParagraphTrace = item.metadata;
            DebugState.getInstance().removeParagraphTrace(paragraph.name);
            vscode.window.showInformationMessage(`Removed trace for paragraph ${paragraph.name}`);
        })
    );

    // Register show panel command
    context.subscriptions.push(
        vscode.commands.registerCommand('cobolDebug.showDebugPanel', () => {
            vscode.commands.executeCommand('cobolDebugConfiguration.focus');
        })
    );
}

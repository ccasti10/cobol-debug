"use strict";
/**
 * Debug Panel - Tree View Provider
 * Displays debug configuration in sidebar
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
exports.DebugConfigProvider = void 0;
exports.registerDebugPanel = registerDebugPanel;
const vscode = __importStar(require("vscode"));
const debugState_1 = require("./debugState");
/**
 * Tree item types
 */
var TreeItemType;
(function (TreeItemType) {
    TreeItemType["ROOT_SINGLE_POINT"] = "root_single_point";
    TreeItemType["ROOT_TRACKING"] = "root_tracking";
    TreeItemType["ROOT_PARAGRAPHS"] = "root_paragraphs";
    TreeItemType["ROOT_SUMMARY"] = "root_summary";
    TreeItemType["SINGLE_POINT_ITEM"] = "single_point_item";
    TreeItemType["TRACKING_ITEM"] = "tracking_item";
    TreeItemType["PARAGRAPH_ITEM"] = "paragraph_item";
    TreeItemType["OCCURRENCE_ITEM"] = "occurrence_item";
    TreeItemType["SUMMARY_ITEM"] = "summary_item";
})(TreeItemType || (TreeItemType = {}));
/**
 * Custom tree item with metadata
 */
class DebugTreeItem extends vscode.TreeItem {
    constructor(label, collapsibleState, itemType, metadata) {
        super(label, collapsibleState);
        this.label = label;
        this.collapsibleState = collapsibleState;
        this.itemType = itemType;
        this.metadata = metadata;
        this.setupItem();
    }
    setupItem() {
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
    setupSinglePointItem() {
        const watch = this.metadata;
        this.tooltip = `Variable: ${watch.variable}\nLine: ${watch.line + 1}\nMode: ${watch.mode}`;
        this.description = `Line ${watch.line + 1}`;
        // Make clickable to jump to line
        this.command = {
            command: 'cobolDebug.goToLine',
            title: 'Go to Line',
            arguments: [watch.line]
        };
    }
    setupTrackingItem() {
        const tracking = this.metadata;
        this.tooltip = `Tracking ${tracking.variable} in ${tracking.occurrences.length} locations`;
        this.description = `${tracking.occurrences.length} occurrences`;
    }
    setupParagraphItem() {
        const paragraph = this.metadata;
        this.tooltip = `Paragraph: ${paragraph.name}\nLines: ${paragraph.startLine + 1}-${paragraph.endLine + 1}`;
        this.description = `Lines ${paragraph.startLine + 1}-${paragraph.endLine + 1}`;
        // Make clickable to jump to paragraph
        this.command = {
            command: 'cobolDebug.goToLine',
            title: 'Go to Line',
            arguments: [paragraph.startLine]
        };
    }
    setupOccurrenceItem() {
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
class DebugConfigProvider {
    constructor() {
        this._onDidChangeTreeData = new vscode.EventEmitter();
        this.onDidChangeTreeData = this._onDidChangeTreeData.event;
        // Listen to debug state changes
        debugState_1.DebugState.getInstance().onDidChangeState(() => {
            this.refresh();
        });
    }
    refresh() {
        this._onDidChangeTreeData.fire();
    }
    getTreeItem(element) {
        return element;
    }
    getChildren(element) {
        const config = debugState_1.DebugState.getInstance().getConfig();
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
    getRootItems(config) {
        if (!config) {
            return [
                new DebugTreeItem('No debug configuration', vscode.TreeItemCollapsibleState.None, TreeItemType.SUMMARY_ITEM)
            ];
        }
        const items = [];
        // Summary
        items.push(new DebugTreeItem('Summary', vscode.TreeItemCollapsibleState.Expanded, TreeItemType.ROOT_SUMMARY, config));
        // Single Point Watches
        const singlePointCount = config.debugPoints.singlePoint.length;
        if (singlePointCount > 0) {
            items.push(new DebugTreeItem(`Single Point Watches (${singlePointCount})`, vscode.TreeItemCollapsibleState.Expanded, TreeItemType.ROOT_SINGLE_POINT, config));
        }
        // Variable Tracking
        const trackingCount = config.debugPoints.variableTracking.length;
        if (trackingCount > 0) {
            items.push(new DebugTreeItem(`Variable Tracking (${trackingCount})`, vscode.TreeItemCollapsibleState.Expanded, TreeItemType.ROOT_TRACKING, config));
        }
        // Paragraph Traces
        const paragraphCount = config.debugPoints.paragraphs.length;
        if (paragraphCount > 0) {
            items.push(new DebugTreeItem(`Paragraph Traces (${paragraphCount})`, vscode.TreeItemCollapsibleState.Expanded, TreeItemType.ROOT_PARAGRAPHS, config));
        }
        return items;
    }
    getSummaryItems(config) {
        if (!config) {
            return [];
        }
        const items = [];
        const state = debugState_1.DebugState.getInstance();
        // Program name
        items.push(new DebugTreeItem(`Program: ${config.programName}`, vscode.TreeItemCollapsibleState.None, TreeItemType.SUMMARY_ITEM));
        // Total debug points
        const totalPoints = state.getTotalDebugPoints();
        items.push(new DebugTreeItem(`Total debug points: ${totalPoints}`, vscode.TreeItemCollapsibleState.None, TreeItemType.SUMMARY_ITEM));
        // Estimated DISPLAYs
        const totalDisplays = state.estimateTotalDisplays();
        items.push(new DebugTreeItem(`Est. DISPLAY statements: ${totalDisplays}`, vscode.TreeItemCollapsibleState.None, TreeItemType.SUMMARY_ITEM));
        // Last modified
        if (config.lastModified) {
            const date = new Date(config.lastModified);
            items.push(new DebugTreeItem(`Last modified: ${date.toLocaleString()}`, vscode.TreeItemCollapsibleState.None, TreeItemType.SUMMARY_ITEM));
        }
        return items;
    }
    getSinglePointItems(config) {
        if (!config) {
            return [];
        }
        return config.debugPoints.singlePoint.map(watch => new DebugTreeItem(`${watch.variable} (${watch.mode})`, vscode.TreeItemCollapsibleState.None, TreeItemType.SINGLE_POINT_ITEM, watch));
    }
    getTrackingItems(config) {
        if (!config) {
            return [];
        }
        return config.debugPoints.variableTracking.map(tracking => new DebugTreeItem(tracking.variable, vscode.TreeItemCollapsibleState.Collapsed, TreeItemType.TRACKING_ITEM, tracking));
    }
    getParagraphItems(config) {
        if (!config) {
            return [];
        }
        return config.debugPoints.paragraphs.map(paragraph => new DebugTreeItem(paragraph.name, vscode.TreeItemCollapsibleState.None, TreeItemType.PARAGRAPH_ITEM, paragraph));
    }
    getOccurrenceItems(tracking) {
        return tracking.occurrences.map(occurrence => new DebugTreeItem(`Line ${occurrence.line + 1}: ${occurrence.operation} (${occurrence.type})`, vscode.TreeItemCollapsibleState.None, TreeItemType.OCCURRENCE_ITEM, occurrence));
    }
}
exports.DebugConfigProvider = DebugConfigProvider;
/**
 * Register debug panel and related commands
 */
function registerDebugPanel(context) {
    // Create tree view provider
    const treeDataProvider = new DebugConfigProvider();
    // Register tree view
    const treeView = vscode.window.createTreeView('cobolDebugConfiguration', {
        treeDataProvider: treeDataProvider,
        showCollapseAll: true
    });
    context.subscriptions.push(treeView);
    // Register refresh command
    context.subscriptions.push(vscode.commands.registerCommand('cobolDebug.refreshPanel', () => {
        treeDataProvider.refresh();
    }));
    // Register go to line command
    context.subscriptions.push(vscode.commands.registerCommand('cobolDebug.goToLine', async (line) => {
        const editor = vscode.window.activeTextEditor;
        if (editor) {
            const position = new vscode.Position(line, 0);
            editor.selection = new vscode.Selection(position, position);
            editor.revealRange(new vscode.Range(position, position), vscode.TextEditorRevealType.InCenter);
        }
    }));
    // Register remove single point command
    context.subscriptions.push(vscode.commands.registerCommand('cobolDebug.removeSinglePoint', (item) => {
        const watch = item.metadata;
        debugState_1.DebugState.getInstance().removeSinglePointWatch(watch.variable, watch.line);
        vscode.window.showInformationMessage(`Removed watch for ${watch.variable} at line ${watch.line + 1}`);
    }));
    // Register remove tracking command
    context.subscriptions.push(vscode.commands.registerCommand('cobolDebug.removeTracking', (item) => {
        const tracking = item.metadata;
        debugState_1.DebugState.getInstance().removeVariableTracking(tracking.variable);
        vscode.window.showInformationMessage(`Removed tracking for ${tracking.variable}`);
    }));
    // Register remove paragraph command
    context.subscriptions.push(vscode.commands.registerCommand('cobolDebug.removeParagraph', (item) => {
        const paragraph = item.metadata;
        debugState_1.DebugState.getInstance().removeParagraphTrace(paragraph.name);
        vscode.window.showInformationMessage(`Removed trace for paragraph ${paragraph.name}`);
    }));
    // Register show panel command
    context.subscriptions.push(vscode.commands.registerCommand('cobolDebug.showDebugPanel', () => {
        vscode.commands.executeCommand('cobolDebugConfiguration.focus');
    }));
}
//# sourceMappingURL=debugPanel.js.map
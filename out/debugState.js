"use strict";
/**
 * Debug State Manager
 * Maintains the current debug configuration in memory
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
exports.DebugState = void 0;
const vscode = __importStar(require("vscode"));
const configManager_1 = require("./configManager");
class DebugState {
    constructor() {
        this.currentConfig = null;
        this.configManager = null;
        this._onDidChangeState = new vscode.EventEmitter();
        this.onDidChangeState = this._onDidChangeState.event;
    }
    static getInstance() {
        if (!DebugState.instance) {
            DebugState.instance = new DebugState();
        }
        return DebugState.instance;
    }
    initialize(workspaceRoot) {
        this.configManager = new configManager_1.ConfigManager(workspaceRoot);
    }
    getConfig() {
        return this.currentConfig;
    }
    setConfig(config) {
        this.currentConfig = config;
        this.notifyChange();
    }
    async loadConfig(programName) {
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
    async saveConfig() {
        if (!this.configManager || !this.currentConfig) {
            return false;
        }
        this.currentConfig.lastModified = new Date().toISOString();
        return await this.configManager.save(this.currentConfig);
    }
    createNew(programName, sourceFile) {
        if (!this.configManager) {
            return;
        }
        this.currentConfig = this.configManager.createEmpty(programName, sourceFile);
        this.notifyChange();
    }
    // Add debug points
    addSinglePointWatch(watch) {
        if (!this.currentConfig) {
            return;
        }
        // Check for duplicates
        const exists = this.currentConfig.debugPoints.singlePoint.some(w => w.variable === watch.variable && w.line === watch.line);
        if (!exists) {
            this.currentConfig.debugPoints.singlePoint.push(watch);
            this.notifyChange();
        }
    }
    addVariableTracking(tracking) {
        if (!this.currentConfig) {
            return;
        }
        // Remove existing tracking for same variable
        this.currentConfig.debugPoints.variableTracking =
            this.currentConfig.debugPoints.variableTracking.filter(t => t.variable !== tracking.variable);
        this.currentConfig.debugPoints.variableTracking.push(tracking);
        this.notifyChange();
    }
    addParagraphTrace(trace) {
        if (!this.currentConfig) {
            return;
        }
        // Check for duplicates
        const exists = this.currentConfig.debugPoints.paragraphs.some(p => p.name === trace.name);
        if (!exists) {
            this.currentConfig.debugPoints.paragraphs.push(trace);
            this.notifyChange();
        }
    }
    // Remove debug points
    removeSinglePointWatch(variable, line) {
        if (!this.currentConfig) {
            return;
        }
        this.currentConfig.debugPoints.singlePoint =
            this.currentConfig.debugPoints.singlePoint.filter(w => !(w.variable === variable && w.line === line));
        this.notifyChange();
    }
    removeVariableTracking(variable) {
        if (!this.currentConfig) {
            return;
        }
        this.currentConfig.debugPoints.variableTracking =
            this.currentConfig.debugPoints.variableTracking.filter(t => t.variable !== variable);
        this.notifyChange();
    }
    removeParagraphTrace(name) {
        if (!this.currentConfig) {
            return;
        }
        this.currentConfig.debugPoints.paragraphs =
            this.currentConfig.debugPoints.paragraphs.filter(p => p.name !== name);
        this.notifyChange();
    }
    clearAll() {
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
    getTotalDebugPoints() {
        if (!this.currentConfig) {
            return 0;
        }
        return this.currentConfig.debugPoints.singlePoint.length +
            this.currentConfig.debugPoints.variableTracking.length +
            this.currentConfig.debugPoints.paragraphs.length;
    }
    estimateTotalDisplays() {
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
    notifyChange() {
        this._onDidChangeState.fire();
    }
}
exports.DebugState = DebugState;
//# sourceMappingURL=debugState.js.map
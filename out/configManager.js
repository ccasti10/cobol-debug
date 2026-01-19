"use strict";
/**
 * Configuration Manager
 * Handles loading and saving debug configurations to JSON files
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
exports.ConfigManager = void 0;
const vscode = __importStar(require("vscode"));
const fs = __importStar(require("fs"));
const path = __importStar(require("path"));
class ConfigManager {
    constructor(workspaceRoot) {
        this.workspaceRoot = workspaceRoot;
        this.configDir = path.join(workspaceRoot, '.vscode', 'cobol-debug');
        this.ensureConfigDir();
    }
    /**
     * Ensure config directory exists
     */
    ensureConfigDir() {
        if (!fs.existsSync(this.configDir)) {
            fs.mkdirSync(this.configDir, { recursive: true });
        }
    }
    /**
     * Get config file path for a program
     */
    getConfigPath(programName) {
        return path.join(this.configDir, `${programName}.debug.json`);
    }
    /**
     * Load debug configuration for a program
     */
    async load(programName) {
        const configPath = this.getConfigPath(programName);
        if (!fs.existsSync(configPath)) {
            return null;
        }
        try {
            const content = fs.readFileSync(configPath, 'utf8');
            const config = JSON.parse(content);
            return config;
        }
        catch (error) {
            vscode.window.showErrorMessage(`Failed to load debug configuration: ${error}`);
            return null;
        }
    }
    /**
     * Save debug configuration for a program
     */
    async save(config) {
        const configPath = this.getConfigPath(config.programName);
        try {
            const content = JSON.stringify(config, null, 2);
            fs.writeFileSync(configPath, content, 'utf8');
            vscode.window.showInformationMessage(`Debug configuration saved: ${path.basename(configPath)}`);
            return true;
        }
        catch (error) {
            vscode.window.showErrorMessage(`Failed to save debug configuration: ${error}`);
            return false;
        }
    }
    /**
     * Check if configuration exists for a program
     */
    exists(programName) {
        return fs.existsSync(this.getConfigPath(programName));
    }
    /**
     * Delete configuration for a program
     */
    async delete(programName) {
        const configPath = this.getConfigPath(programName);
        if (!fs.existsSync(configPath)) {
            return true;
        }
        try {
            fs.unlinkSync(configPath);
            return true;
        }
        catch (error) {
            vscode.window.showErrorMessage(`Failed to delete debug configuration: ${error}`);
            return false;
        }
    }
    /**
     * List all debug configurations
     */
    async listAll() {
        if (!fs.existsSync(this.configDir)) {
            return [];
        }
        try {
            const files = fs.readdirSync(this.configDir);
            return files
                .filter(f => f.endsWith('.debug.json'))
                .map(f => f.replace('.debug.json', ''));
        }
        catch (error) {
            return [];
        }
    }
    /**
     * Create empty configuration
     */
    createEmpty(programName, sourceFile) {
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
exports.ConfigManager = ConfigManager;
//# sourceMappingURL=configManager.js.map
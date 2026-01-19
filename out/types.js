"use strict";
/**
 * Types and interfaces for COBOL Debug extension
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.AccessType = exports.OperationType = exports.DebugMode = void 0;
/**
 * Type of debug operation
 */
var DebugMode;
(function (DebugMode) {
    DebugMode["CURRENT_LINE"] = "CURRENT_LINE";
    DebugMode["BEFORE"] = "BEFORE";
    DebugMode["AFTER"] = "AFTER";
    DebugMode["BEFORE_AFTER"] = "BEFORE_AFTER";
})(DebugMode || (exports.DebugMode = DebugMode = {}));
/**
 * Type of COBOL operation
 */
var OperationType;
(function (OperationType) {
    OperationType["MOVE"] = "MOVE";
    OperationType["COMPUTE"] = "COMPUTE";
    OperationType["ADD"] = "ADD";
    OperationType["SUBTRACT"] = "SUBTRACT";
    OperationType["MULTIPLY"] = "MULTIPLY";
    OperationType["DIVIDE"] = "DIVIDE";
    OperationType["IF"] = "IF";
    OperationType["EVALUATE"] = "EVALUATE";
    OperationType["PERFORM"] = "PERFORM";
    OperationType["READ"] = "READ";
    OperationType["WRITE"] = "WRITE";
    OperationType["REWRITE"] = "REWRITE";
    OperationType["UNKNOWN"] = "UNKNOWN";
})(OperationType || (exports.OperationType = OperationType = {}));
/**
 * Type of variable access
 */
var AccessType;
(function (AccessType) {
    AccessType["READ"] = "READ";
    AccessType["WRITE"] = "WRITE";
})(AccessType || (exports.AccessType = AccessType = {}));
//# sourceMappingURL=types.js.map
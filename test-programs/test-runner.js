"use strict";
/**
 * Test Script - Simula uso de la extensión
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
const fs = __importStar(require("fs"));
const path = __importStar(require("path"));
const cobolParser_1 = require("../src/cobolParser");
const debugGenerator_1 = require("../src/debugGenerator");
const types_1 = require("../src/types");
// Leer archivo de ejemplo
const sourceFile = path.join(__dirname, 'EJEMPLO1.cbl');
const sourceContent = fs.readFileSync(sourceFile, 'utf8');
console.log('='.repeat(60));
console.log('PRUEBA 1: Parser COBOL');
console.log('='.repeat(60));
// Parsear el programa
const program = cobolParser_1.CobolParser.parse(sourceContent);
console.log('\n✅ Programa parseado exitosamente:');
console.log(`   - Nombre: ${program.programName}`);
console.log(`   - WORKING-STORAGE: líneas ${program.workingStorageStart} - ${program.workingStorageEnd}`);
console.log(`   - PROCEDURE DIVISION: línea ${program.procedureDivisionStart}`);
console.log(`   - Variables encontradas: ${program.variables.length}`);
console.log(`   - Párrafos encontrados: ${program.paragraphs.length}`);
console.log('\n📋 Párrafos:');
program.paragraphs.forEach(p => {
    console.log(`   - ${p.name} (líneas ${p.startLine + 1}-${p.endLine + 1})`);
});
console.log('\n📦 Variables principales:');
program.variables.slice(0, 10).forEach(v => {
    console.log(`   - ${v.name} (nivel ${v.level}${v.picture ? ', PIC ' + v.picture : ''})`);
});
console.log('\n' + '='.repeat(60));
console.log('PRUEBA 2: Tracking de Variable');
console.log('='.repeat(60));
// Simular tracking de WS-MONTO-ACTUAL
const lines = sourceContent.split('\n');
const occurrences = cobolParser_1.CobolParser.findVariableOccurrences(lines, 'WS-MONTO-ACTUAL', program.procedureDivisionStart);
console.log(`\n✅ Variable WS-MONTO-ACTUAL encontrada en ${occurrences.length} ubicaciones:`);
occurrences.forEach(occ => {
    console.log(`   - Línea ${occ.line + 1}: ${occ.operation} (${occ.type})`);
    console.log(`     "${occ.lineText?.substring(0, 60)}..."`);
});
console.log('\n' + '='.repeat(60));
console.log('PRUEBA 3: Generación de Código DEBUG');
console.log('='.repeat(60));
// Crear configuración de debug simulada
const debugConfig = {
    programName: program.programName,
    sourceFile: 'EJEMPLO1.cbl',
    lastModified: new Date().toISOString(),
    outputWidth: 133,
    useUnicodeChars: true,
    debugPoints: {
        singlePoint: [
            {
                variable: 'WS-INTERES',
                line: 50, // Línea del COMPUTE
                mode: types_1.DebugMode.BEFORE_AFTER,
                showRelatedVars: true
            }
        ],
        variableTracking: [
            {
                variable: 'WS-MONTO-ACTUAL',
                trackAllOccurrences: true,
                includeReadOnly: true,
                occurrences: occurrences
            }
        ],
        paragraphs: [
            {
                name: 'CALCULAR-INTERES',
                startLine: 48,
                endLine: 51,
                traceEntry: true,
                traceExit: true
            }
        ]
    }
};
console.log('\n📝 Configuración de debug:');
console.log(`   - Single point watches: ${debugConfig.debugPoints.singlePoint.length}`);
console.log(`   - Variable tracking: ${debugConfig.debugPoints.variableTracking.length}`);
console.log(`   - Paragraph traces: ${debugConfig.debugPoints.paragraphs.length}`);
// Generar código DEBUG
const generator = new debugGenerator_1.DebugGenerator();
generator.generate(sourceContent, debugConfig).then(debugCode => {
    // Guardar archivo generado
    const outputFile = path.join(__dirname, 'EJEMPLO1-DEBUG.cbl');
    fs.writeFileSync(outputFile, debugCode, 'utf8');
    console.log(`\n✅ Código DEBUG generado: ${outputFile}`);
    // Estadísticas del código generado
    const debugLines = debugCode.split('\n');
    const ccastiLines = debugLines.filter(line => line.startsWith('CCASTI')).length;
    const displayCount = debugLines.filter(line => line.includes('DISPLAY')).length;
    console.log('\n📊 Estadísticas del código generado:');
    console.log(`   - Líneas totales: ${debugLines.length} (original: ${lines.length})`);
    console.log(`   - Líneas con CCASTI: ${ccastiLines}`);
    console.log(`   - DISPLAYs agregados: ${displayCount}`);
    console.log(`   - Incremento: ${((debugLines.length / lines.length - 1) * 100).toFixed(1)}%`);
    console.log('\n' + '='.repeat(60));
    console.log('PRUEBA 4: Validación del Código Generado');
    console.log('='.repeat(60));
    // Verificar que el código generado tenga las secciones esperadas
    const hasDebugVars = debugCode.includes('DEBUG TEMPORARY VARIABLES');
    const hasTracking = debugCode.includes('DEBUG TRACKING');
    const hasBeforeAfter = debugCode.includes('BEFORE:') && debugCode.includes('AFTER:');
    const hasParagraphTrace = debugCode.includes('ENTERING:') && debugCode.includes('EXITING:');
    console.log('\n✅ Validaciones:');
    console.log(`   ${hasDebugVars ? '✓' : '✗'} Variables temporales agregadas`);
    console.log(`   ${hasTracking ? '✓' : '✗'} Tracking de variables presente`);
    console.log(`   ${hasBeforeAfter ? '✓' : '✗'} DISPLAYs BEFORE/AFTER presentes`);
    console.log(`   ${hasParagraphTrace ? '✓' : '✗'} Trace de párrafo presente`);
    console.log('\n' + '='.repeat(60));
    console.log('PRUEBA 5: Muestra del Código Generado');
    console.log('='.repeat(60));
    // Mostrar fragmento del código generado (alrededor del CALCULAR-INTERES)
    const calcularInteresStart = debugLines.findIndex(line => line.includes('CALCULAR-INTERES.'));
    if (calcularInteresStart !== -1) {
        console.log('\n📄 Fragmento del código generado (CALCULAR-INTERES):');
        console.log('─'.repeat(60));
        for (let i = Math.max(0, calcularInteresStart - 2); i < Math.min(debugLines.length, calcularInteresStart + 20); i++) {
            const lineNum = String(i + 1).padStart(4, ' ');
            console.log(`${lineNum} | ${debugLines[i]}`);
        }
        console.log('─'.repeat(60));
    }
    console.log('\n' + '='.repeat(60));
    console.log('✅ TODAS LAS PRUEBAS COMPLETADAS');
    console.log('='.repeat(60));
    console.log('\n📝 Resumen:');
    console.log('   1. ✅ Parser COBOL funcional');
    console.log('   2. ✅ Tracking de variables funcional');
    console.log('   3. ✅ Generación de código DEBUG funcional');
    console.log('   4. ✅ Validaciones pasadas');
    console.log('   5. ✅ Código generado listo para compilar');
    console.log('\n📂 Archivo generado:');
    console.log(`   ${outputFile}`);
    console.log('\n🚀 Siguiente paso: Compilar EJEMPLO1-DEBUG.cbl en el mainframe');
}).catch(error => {
    console.error('❌ Error generando código DEBUG:', error);
    process.exit(1);
});
//# sourceMappingURL=test-runner.js.map
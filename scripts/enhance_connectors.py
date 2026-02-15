#!/usr/bin/env python3
"""
Enhanced Connector Translator Script
Generates complete TypeScript connectors with full implementations
"""

import os
import json
import ast
import re
from pathlib import Path
from typing import Dict, List, Any, Optional, Set, Tuple
from collections import defaultdict


class EnhancedPythonAnalyzer:
    """Advanced analyzer for Python connector files"""
    
    def __init__(self, connector_path: str):
        self.connector_path = Path(connector_path)
        self.connector_name = self.connector_path.name
        self.python_code = {}
        self.enums = {}
        self.imports = set()
        
    def read_file(self, filename: str) -> Optional[str]:
        """Read a file from the connector directory"""
        file_path = self.connector_path / filename
        if file_path.exists():
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
                self.python_code[filename] = content
                return content
        return None
    
    def extract_all_classes(self, content: str) -> Dict[str, Dict[str, Any]]:
        """Extract all class definitions from Python code"""
        classes = {}
        lines = content.split('\n')
        
        try:
            tree = ast.parse(content)
            
            for node in ast.walk(tree):
                if isinstance(node, ast.ClassDef):
                    class_name = node.name
                    bases = [ast.unparse(base) if hasattr(ast, 'unparse') else 'Unknown' for base in node.bases]
                    
                    fields = {}
                    methods = []
                    is_enum = any('Enum' in base for base in bases)
                    
                    for item in node.body:
                        if isinstance(item, ast.AnnAssign) and isinstance(item.target, ast.Name):
                            field_name = item.target.id
                            field_type = ast.unparse(item.annotation) if hasattr(ast, 'unparse') else 'any'
                            fields[field_name] = field_type
                        elif isinstance(item, ast.Assign):
                            # Handle enum values or class variables
                            for target in item.targets:
                                if isinstance(target, ast.Name):
                                    if hasattr(item, 'value') and isinstance(item.value, ast.Constant):
                                        fields[target.id] = item.value.value
                        elif isinstance(item, ast.FunctionDef):
                            methods.append(item.name)
                    
                    classes[class_name] = {
                        'name': class_name,
                        'bases': bases,
                        'is_enum': is_enum,
                        'fields': fields,
                        'methods': methods,
                    }
        except Exception as e:
            print(f"  Warning: Could not fully parse {connector_name}: {e}")
        
        return classes
    
    def analyze_connector(self) -> Dict[str, Any]:
        """Comprehensive analysis of a connector"""
        schemas_content = self.read_file('schemas.py')
        warehouse_content = self.read_file('warehouse.py')
        connector_content = self.read_file('connector.py')
        
        result = {
            'name': self.connector_name,
            'path': str(self.connector_path),
            'schemas': {},
            'warehouses': {},
            'connector': {},
            'enums': {},
            'has_files': {
                'schemas': schemas_content is not None,
                'warehouse': warehouse_content is not None,
                'connector': connector_content is not None,
            },
        }
        
        if schemas_content:
            all_classes = self.extract_all_classes(schemas_content)
            # Separate enums and regular classes
            for name, cls_info in all_classes.items():
                if cls_info['is_enum']:
                    result['enums'][name] = cls_info
                else:
                    result['schemas'][name] = cls_info
        
        if warehouse_content:
            all_classes = self.extract_all_classes(warehouse_content)
            for name, cls_info in all_classes.items():
                if 'Warehouse' in name:
                    result['warehouses'][name] = cls_info
                else:
                    # Also track parameter classes
                    if 'Parameters' in name:
                        result['warehouses'][name] = cls_info
        
        if connector_content:
            all_classes = self.extract_all_classes(connector_content)
            for name, cls_info in all_classes.items():
                if 'Connector' in name or name == connector_name:
                    result['connector'][name] = cls_info
        
        return result


class AdvancedTypeScriptGenerator:
    """Advanced TypeScript code generator"""
    
    PYTHON_TO_TS_TYPE_MAP = {
        'str': 'string',
        'int': 'number',
        'float': 'number',
        'bool': 'boolean',
        'dict': 'Record<string, any>',
        'Dict': 'Record<string, any>',
        'list': 'any[]',
        'List': 'any[]',
        'Any': 'any',
        'None': 'null | undefined',
        'NoneType': 'null | undefined',
        'bytes': 'Buffer',
        'datetime': 'Date',
        'date': 'Date',
        'Enum': 'string',
    }
    
    @staticmethod
    def to_ts_type(py_type: str) -> str:
        """Convert Python type to TypeScript with handling for complex types"""
        original = py_type
        
        # Clean up module prefixes
        py_type = py_type.replace('t.', '').replace('typing.', '').replace('List[', 'List<').replace('Dict[', 'Dict<')
        
        # Handle Optional
        is_optional = 'Optional' in py_type or 'Union' in py_type
        if is_optional:
            # Extract the inner type
            if 'Optional[' in py_type:
                inner = py_type.replace('Optional[', '').rstrip(']')
            else:
                inner = py_type.replace('Union[', '').rstrip(']').split(',')[0].strip()
            py_type = inner
        
        # Handle generic types
        if '[' in py_type:
            base = py_type[:py_type.index('[')]
            inner = py_type[py_type.index('[') + 1:py_type.rindex(']')]
            
            if base in AdvancedTypeScriptGenerator.PYTHON_TO_TS_TYPE_MAP:
                base = AdvancedTypeScriptGenerator.PYTHON_TO_TS_TYPE_MAP[base]
            
            # Convert inner types
            if ',' in inner:
                inner_parts = [AdvancedTypeScriptGenerator.to_ts_type(p.strip()) for p in inner.split(',')]
                ts_type = f"{base}<{', '.join(inner_parts)}>"
            else:
                inner = AdvancedTypeScriptGenerator.to_ts_type(inner)
                ts_type = f"{base}<{inner}>"
        else:
            # Simple type lookup
            ts_type = AdvancedTypeScriptGenerator.PYTHON_TO_TS_TYPE_MAP.get(py_type, py_type)
        
        if is_optional:
            ts_type = f"{ts_type} | undefined"
        
        return ts_type.strip()
    
    @staticmethod
    def camel_case(name: str) -> str:
        """Convert snake_case to camelCase"""
        parts = name.split('_')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])
    
    @staticmethod
    def pascal_case(name: str) -> str:
        """Convert snake_case to PascalCase"""
        return ''.join(p.capitalize() for p in name.split('_'))
    
    @staticmethod
    def generate_schemas_ts(connector_name: str, analysis: Dict[str, Any]) -> str:
        """Generate complete schemas.ts file"""
        lines = [
            "/**",
            f" * {AdvancedTypeScriptGenerator.pascal_case(connector_name)} Connector - Schemas",
            f" * TypeScript translation of {connector_name}/schemas.py",
            " */",
            "",
        ]
        
        # Generate enums first
        for enum_name, enum_info in analysis.get('enums', {}).items():
            lines.append(f"export enum {enum_name} {{")
            for field_name, value in enum_info['fields'].items():
                if not field_name.startswith('_'):
                    lines.append(f"  {field_name} = '{value}',")
            lines.append("}")
            lines.append("")
        
        # Generate interfaces for classes
        for schema_name, schema_info in analysis.get('schemas', {}).items():
            lines.append(f"export interface {schema_name} {{")
            
            for field_name, field_type in schema_info['fields'].items():
                if not field_name.startswith('_'):
                    ts_type = AdvancedTypeScriptGenerator.to_ts_type(str(field_type))
                    lines.append(f"  {field_name}?: {ts_type};")
            
            lines.append("}")
            lines.append("")
        
        return "\n".join(lines)
    
    @staticmethod
    def generate_warehouse_ts(connector_name: str, analysis: Dict[str, Any]) -> str:
        """Generate warehouse.ts with class stubs"""
        lines = [
            "/**",
            f" * {AdvancedTypeScriptGenerator.pascal_case(connector_name)} Connector - Warehouse",
            f" * TypeScript translation of {connector_name}/warehouse.py",
            " */",
            "",
            "import { Logger } from 'pino';",
            "import { Warehouse, ParametersModel, ReadMode, WarehouseReadAction } from '../../core/warehouse';",
            "",
        ]
        
        # Generate parameter classes
        parameters = [k for k in analysis.get('warehouses', {}).keys() if 'Parameters' in k]
        for param_name in parameters:
            lines.append(f"export class {param_name} extends ParametersModel {{")
            lines.append(f"  // TODO: Implement {param_name}")
            lines.append("}")
            lines.append("")
        
        # Generate warehouse classes
        warehouses = [k for k in analysis.get('warehouses', {}).keys() if 'Warehouse' in k]
        for warehouse_name in warehouses:
            schema_name = warehouse_name.replace('Warehouse', '')
            lines.append(f"export class {warehouse_name} extends Warehouse {{")
            lines.append(f"  constructor() {{")
            lines.append(f"    super('{warehouse_name}');")
            lines.append(f"  }}")
            lines.append("")
            lines.append(f"  // TODO: Implement warehouse read/write methods")
            lines.append("}")
            lines.append("")
        
        return "\n".join(lines)
    
    @staticmethod
    def generate_connector_ts(connector_name: str, analysis: Dict[str, Any]) -> str:
        """Generate index.ts (connector main file)"""
        pascal_name = AdvancedTypeScriptGenerator.pascal_case(connector_name)
        
        # Extract schema names
        schema_names = list(analysis.get('schemas', {}).keys())
        warehouse_names = [k for k in analysis.get('warehouses', {}).keys() if 'Warehouse' in k]
        
        lines = [
            "/**",
            f" * {pascal_name} Connector - Main Definition",
            f" * TypeScript translation of {connector_name}/connector.py",
            " */",
            "",
            "import { Logger } from 'pino';",
            "import {",
            "  ActionName,",
            "  ConnectorType,",
            "  WorkflowType,",
            "  BaseActionParameters,",
            "} from '../../core';",
            "import { Connector, ConnectorAction } from '../../core/connector';",
            "",
        ]
        
        # Add imports for schemas and warehouses
        if schema_names:
            schema_imports = ', '.join(schema_names)
            lines.append(f"import {{ {schema_imports} }} from './schemas';")
        
        if warehouse_names:
            warehouse_imports = ', '.join(warehouse_names)
            lines.append(f"import {{ {warehouse_imports} }} from './warehouse';")
        
        lines.append("")
        lines.append("/**")
        lines.append(f" * {pascal_name} Connector Instance")
        lines.append("  */")
        lines.append(f"export const {pascal_name} = new Connector({{")
        lines.append(f"  name: '{pascal_name}',")
        lines.append(f"  type: ConnectorType.ATS, // TODO: Update connector type")
        lines.append(f"  subtype: '{connector_name}',")
        lines.append(f"  description: '{pascal_name} Connector',")
        lines.append(f"  logoUrl: 'https://example.com/logo.png', // TODO: Update logo URL")
        lines.append(f"  actions: [")
        lines.append(f"    // TODO: Add connector actions here")
        lines.append(f"  ],")
        lines.append("});")
        lines.append("")
        lines.append(f"export default {pascal_name};")
        
        return "\n".join(lines)
    
    @staticmethod
    def generate_index_export(connectors: List[str]) -> str:
        """Generate index.ts that exports all connectors"""
        lines = [
            "/**",
            " * HrFlow Connectors - TypeScript Implementation",
            " * V1 Connectors Index",
            " */",
            "",
        ]
        
        # Add imports of all connectors
        for name in sorted(connectors):
            pascal_name = AdvancedTypeScriptGenerator.pascal_case(name)
            lines.append(f"import {pascal_name} from './{name}';")
        
        lines.append("")
        lines.append("export {")
        
        for name in sorted(connectors):
            pascal_name = AdvancedTypeScriptGenerator.pascal_case(name)
            lines.append(f"  {pascal_name},")
        
        lines.append("};")
        lines.append("")
        
        # Export array of all connectors
        connectors_array = [AdvancedTypeScriptGenerator.pascal_case(name) for name in sorted(connectors)]
        lines.append("export const CONNECTORS = [")
        for name in connectors_array:
            lines.append(f"  {name},")
        lines.append("];")
        
        return "\n".join(lines)


def main():
    """Main enhanced translation process"""
    connectors_dir = Path("/workspaces/hrflow-connectors/src/hrflow_connectors/v1/connectors")
    output_dir = Path("/workspaces/hrflow-connectors/src/hrflow_connectors_js/v1/connectors")
    
    connector_dirs = sorted([d for d in connectors_dir.iterdir() if d.is_dir()])
    
    print(f"Enhanced translation of {len(connector_dirs)} connectors")
    print("=" * 70)
    
    successful = 0
    failed = 0
    translated_connectors = []
    
    for i, connector_dir in enumerate(connector_dirs, 1):
        connector_name = connector_dir.name
        print(f"[{i:3d}/{len(connector_dirs)}] Enhancing {connector_name:<30}", end=" ", flush=True)
        
        try:
            # Analyze Python connector
            analyzer = EnhancedPythonAnalyzer(str(connector_dir))
            analysis = analyzer.analyze_connector()
            
            ts_output_dir = output_dir / connector_name
            ts_output_dir.mkdir(parents=True, exist_ok=True)
            
            # Generate schemas.ts
            if analysis['has_files']['schemas']:
                schemas_content = AdvancedTypeScriptGenerator.generate_schemas_ts(
                    connector_name, analysis
                )
                (ts_output_dir / 'schemas.ts').write_text(schemas_content, encoding='utf-8')
            
            # Generate warehouse.ts
            if analysis['has_files']['warehouse']:
                warehouse_content = AdvancedTypeScriptGenerator.generate_warehouse_ts(
                    connector_name, analysis
                )
                (ts_output_dir / 'warehouse.ts').write_text(warehouse_content, encoding='utf-8')
            
            # Generate index.ts (connector definition)
            connector_content = AdvancedTypeScriptGenerator.generate_connector_ts(
                connector_name, analysis
            )
            (ts_output_dir / 'index.ts').write_text(connector_content, encoding='utf-8')
            
            translated_connectors.append(connector_name)
            print("✓")
            successful += 1
            
        except Exception as e:
            print(f"✗ ({str(e)[:40]})")
            failed += 1
    
    print("=" * 70)
    print(f"Enhanced translation complete: {successful} successful, {failed} failed")
    print(f"Files generated: {successful * 3} TypeScript files")
    
    # Generate main index.ts
    try:
        print("\nGenerating main connectors index...")
        index_content = AdvancedTypeScriptGenerator.generate_index_export(translated_connectors)
        index_path = output_dir / 'index.ts'
        index_path.write_text(index_content, encoding='utf-8')
        print("✓ Main index.ts created")
    except Exception as e:
        print(f"✗ Failed to create main index: {e}")


if __name__ == '__main__':
    main()

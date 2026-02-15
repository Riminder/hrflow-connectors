#!/usr/bin/env python3
"""
Connector Translator Script
Translates Python connectors to TypeScript
"""

import os
import json
import ast
import re
from pathlib import Path
from typing import Dict, List, Any, Optional
from collections import defaultdict


class PythonConnectorAnalyzer:
    """Analyzes Python connector files for translation to TypeScript"""
    
    def __init__(self, connector_path: str):
        self.connector_path = Path(connector_path)
        self.connector_name = self.connector_path.name
        
    def read_file(self, filename: str) -> Optional[str]:
        """Read a file from the connector directory"""
        file_path = self.connector_path / filename
        if file_path.exists():
            with open(file_path, 'r', encoding='utf-8') as f:
                return f.read()
        return None
    
    def extract_schema_classes(self, schemas_content: str) -> Dict[str, Dict[str, Any]]:
        """Extract Pydantic model definitions from schemas.py"""
        schemas = {}
        
        try:
            tree = ast.parse(schemas_content)
            
            for node in ast.walk(tree):
                if isinstance(node, ast.ClassDef):
                    class_name = node.name
                    fields = {}
                    
                    for item in node.body:
                        if isinstance(item, ast.AnnAssign) and isinstance(item.target, ast.Name):
                            field_name = item.target.id
                            # Get the annotation string
                            field_type = ast.unparse(item.annotation) if hasattr(ast, 'unparse') else 'any'
                            fields[field_name] = {
                                'name': field_name,
                                'type': field_type,
                                'optional': 'Optional' in field_type or 't.Optional' in field_type,
                            }
                    
                    if fields:
                        schemas[class_name] = {
                            'name': class_name,
                            'fields': fields,
                        }
        except Exception as e:
            print(f"Warning: Could not parse schemas for {self.connector_name}: {e}")
        
        return schemas
    
    def extract_warehouse_classes(self, warehouse_content: str) -> Dict[str, Dict[str, Any]]:
        """Extract warehouse class definitions"""
        warehouses = {}
        
        try:
            tree = ast.parse(warehouse_content)
            
            for node in ast.walk(tree):
                if isinstance(node, ast.ClassDef):
                    class_name = node.name
                    if 'Warehouse' in class_name:
                        methods = []
                        
                        for item in node.body:
                            if isinstance(item, ast.FunctionDef):
                                methods.append(item.name)
                        
                        warehouses[class_name] = {
                            'name': class_name,
                            'methods': methods,
                        }
        except Exception as e:
            print(f"Warning: Could not parse warehouse for {self.connector_name}: {e}")
        
        return warehouses
    
    def analyze(self) -> Dict[str, Any]:
        """Analyze the connector and extract all necessary information"""
        schemas_content = self.read_file('schemas.py')
        warehouse_content = self.read_file('warehouse.py')
        connector_content = self.read_file('connector.py')
        
        result = {
            'name': self.connector_name,
            'path': str(self.connector_path),
            'schemas': {},
            'warehouses': {},
            'has_connector': connector_content is not None,
        }
        
        if schemas_content:
            result['schemas'] = self.extract_schema_classes(schemas_content)
        
        if warehouse_content:
            result['warehouses'] = self.extract_warehouse_classes(warehouse_content)
        
        return result


class TypeScriptGenerator:
    """Generates TypeScript connector code"""
    
    @staticmethod
    def python_type_to_ts(py_type: str) -> str:
        """Convert Python type annotation to TypeScript"""
        # Remove module prefixes
        ts_type = py_type.replace('t.', '').replace('typing.', '')
        
        # Handle Optional types
        if 'Optional' in ts_type or 'Union' in ts_type:
            ts_type = ts_type.replace('Optional[', '').replace('Union[', '').rstrip(']')
            if '|' in ts_type:
                parts = [p.strip() for p in ts_type.split('|')]
                ts_type = ' | '.join(parts)
            ts_type = ts_type + ' | undefined'
        
        # Map Python types to TypeScript
        type_mapping = {
            'str': 'string',
            'int': 'number',
            'float': 'number',
            'bool': 'boolean',
            'dict': 'Record<string, any>',
            'list': 'any[]',
            'List': 'any[]',
            'Dict': 'Record<string, any>',
            'Any': 'any',
        }
        
        for py_t, ts_t in type_mapping.items():
            ts_type = re.sub(r'\b' + py_t + r'\b', ts_t, ts_type)
        
        return ts_type.strip()
    
    @staticmethod
    def generate_schemas(connector_name: str, schemas: Dict[str, Dict[str, Any]]) -> str:
        """Generate schemas.ts content"""
        lines = [
            f"/**",
            f" * {connector_name} Connector - Schemas",
            f" * TypeScript translation of {connector_name}/schemas.py",
            f" */",
            f"",
        ]
        
        for schema_name, schema_info in schemas.items():
            lines.append(f"export interface {schema_name} {{")
            
            for field_name, field_info in schema_info['fields'].items():
                ts_type = TypeScriptGenerator.python_type_to_ts(field_info['type'])
                optional_mark = field_info.get('optional', False)
                question_mark = '?' if optional_mark else ''
                lines.append(f"  {field_name}{question_mark}: {ts_type};")
            
            lines.append("}")
            lines.append("")
        
        return "\n".join(lines)
    
    @staticmethod
    def generate_warehouse_stub(connector_name: str, warehouses: Dict[str, Dict[str, Any]]) -> str:
        """Generate warehouse.ts stub"""
        lines = [
            f"/**",
            f" * {connector_name} Connector - Warehouse",
            f" * TypeScript translation of {connector_name}/warehouse.py",
            f" */",
            f"",
            f"import {{ Warehouse, ParametersModel, ReadMode }} from '../../core/warehouse';",
            f"",
        ]
        
        for warehouse_name, warehouse_info in warehouses.items():
            lines.append(f"export class {warehouse_name} extends Warehouse {{")
            lines.append(f"  // TODO: Implement {warehouse_name}")
            lines.append(f"}}")
            lines.append("")
        
        return "\n".join(lines)
    
    @staticmethod
    def generate_index(connector_name: str) -> str:
        """Generate index.ts stub"""
        import_name = ''.join(word.capitalize() for word in connector_name.split('_'))
        
        lines = [
            f"/**",
            f" * {connector_name} Connector - Main Definition",
            f" * TypeScript translation of {connector_name}/connector.py",
            f" */",
            f"",
            f"import {{ Connector }} from '../../core/connector';",
            f"",
            f"// TODO: Import schemas and warehouse",
            f"// import {{ {import_name}Profile }} from './schemas';",
            f"// import {{ {import_name}Warehouse }} from './warehouse';",
            f"",
            f"export const {import_name} = new Connector({{",
            f"  name: '{import_name}',",
            f"  // TODO: Configure connector",
            f"}});",
            f"",
            f"export default {import_name};",
        ]
        
        return "\n".join(lines)


def main():
    """Main translation process"""
    connectors_dir = Path("/workspaces/hrflow-connectors/src/hrflow_connectors/v1/connectors")
    output_dir = Path("/workspaces/hrflow-connectors/src/hrflow_connectors_js/v1/connectors")
    
    # Get all connector directories
    connector_dirs = sorted([d for d in connectors_dir.iterdir() if d.is_dir()])
    
    print(f"Found {len(connector_dirs)} connectors to translate")
    print("=" * 60)
    
    successful = 0
    failed = 0
    
    for i, connector_dir in enumerate(connector_dirs, 1):
        connector_name = connector_dir.name
        print(f"[{i:3d}/{len(connector_dirs)}] Translating {connector_name}...", end=" ", flush=True)
        
        try:
            # Analyze Python connector
            analyzer = PythonConnectorAnalyzer(str(connector_dir))
            analysis = analyzer.analyze()
            
            # Create output directory
            ts_output_dir = output_dir / connector_name
            ts_output_dir.mkdir(parents=True, exist_ok=True)
            
            # Generate and write schemas.ts
            if analysis['schemas']:
                schemas_content = TypeScriptGenerator.generate_schemas(
                    connector_name.title(),
                    analysis['schemas']
                )
                (ts_output_dir / 'schemas.ts').write_text(schemas_content, encoding='utf-8')
            
            # Generate and write warehouse.ts
            if analysis['warehouses']:
                warehouse_content = TypeScriptGenerator.generate_warehouse_stub(
                    connector_name.title(),
                    analysis['warehouses']
                )
                (ts_output_dir / 'warehouse.ts').write_text(warehouse_content, encoding='utf-8')
            
            # Generate and write index.ts
            index_content = TypeScriptGenerator.generate_index(connector_name)
            (ts_output_dir / 'index.ts').write_text(index_content, encoding='utf-8')
            
            print("✓")
            successful += 1
            
        except Exception as e:
            print(f"✗ ({str(e)[:40]}...)")
            failed += 1
    
    print("=" * 60)
    print(f"Translation complete: {successful} successful, {failed} failed")


if __name__ == '__main__':
    main()

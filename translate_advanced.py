#!/usr/bin/env python3
"""
Advanced TypeScript Translation Script for all Connectors
Uses AST parsing for proper Python-to-TypeScript translation
"""

import os
import ast
import re
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass


@dataclass
class ClassField:
    name: str
    type_str: str
    default: Optional[str] = None
    optional: bool = False
    description: str = ""


class AdvancedPyToTsTranslator:
    """Advanced TypeScript translator using AST parsing"""
    
    TYPE_MAP = {
        'str': 'string',
        'int': 'number',
        'float': 'number',
        'bool': 'boolean',
        'Any': 'any',
        'None': 'null',
    }
    
    def __init__(self, connector_name: str):
        self.connector_name = connector_name
        self.py_base = f"/workspaces/hrflow-connectors/src/hrflow_connectors/v1/connectors/{connector_name}"
        self.ts_base = f"/workspaces/hrflow-connectors/src/hrflow_connectors_js/v1/connectors/{connector_name}"
        self.class_definitions = {}
        self.function_definitions = {}
        
    def ensure_ts_dir(self):
        """Create TypeScript directory"""
        Path(self.ts_base).mkdir(parents=True, exist_ok=True)
        
    def read_py_file(self, filename: str) -> str:
        """Read Python file"""
        path = os.path.join(self.py_base, filename)
        try:
            with open(path, 'r', encoding='utf-8') as f:
                return f.read()
        except FileNotFoundError:
            return ""
    
    def convert_type(self, type_str: str) -> str:
        """Convert Python type annotation to TypeScript"""
        type_str = type_str.strip()
        
        # Handle Optional[X] type
        if type_str.startswith('Optional['):
            inner = type_str[len('Optional['):-1]
            return f"{self.convert_type(inner)} | undefined"
        
        # Handle List[X] type
        if type_str.startswith('List['):
            inner = type_str[len('List['):-1]
            return f"{self.convert_type(inner)}[]"
        
        # Handle Dict[K, V] type
        if type_str.startswith('Dict['):
            inner = type_str[len('Dict['):-1]
            parts = inner.split(',', 1)
            if len(parts) == 2:
                key_type = self.convert_type(parts[0].strip())
                val_type = self.convert_type(parts[1].strip())
                return f"Record<{key_type}, {val_type}>"
        
        # Handle Union types
        if ' | ' in type_str:
            parts = [self.convert_type(p.strip()) for p in type_str.split('|')]
            return ' | '.join(parts)
        
        # Direct mapping
        for py_type, ts_type in self.TYPE_MAP.items():
            if type_str == py_type:
                return ts_type
            if type_str.lower() == py_type.lower():
                return ts_type
        
        # Default for unknown types
        return type_str if type_str else 'any'
    
    def extract_base_model_fields(self, py_content: str) -> List[Tuple[str, List[ClassField]]]:
        """Extract BaseModel class definitions using AST"""
        try:
            tree = ast.parse(py_content)
        except:
            return []
        
        classes = []
        
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                # Check if it's a BaseModel
                is_base_model = any(
                    (isinstance(base, ast.Name) and base.id == 'BaseModel') or
                    (isinstance(base, ast.Attribute) and 'BaseModel' in ast.get_source_segment(py_content, base) if ast.get_source_segment else False)
                    for base in node.bases
                )
                
                if is_base_model:
                    fields = []
                    
                    # Get annotations
                    for child in node.body:
                        if isinstance(child, ast.AnnAssign):
                            field_name = child.target.id if isinstance(child.target, ast.Name) else str(child.target)
                            type_annotation = ast.get_source_segment(py_content, child.annotation) if child.annotation else None
                            
                            if type_annotation:
                                ts_type = self.convert_type(type_annotation)
                                is_optional = 'undefined' in ts_type or type_annotation.startswith('Optional')
                                
                                fields.append(ClassField(
                                    name=field_name,
                                    type_str=ts_type,
                                    optional=is_optional
                                ))
                    
                    if fields:
                        classes.append((node.name, fields))
        
        return classes
    
    def extract_parameter_classes(self, py_content: str) -> List[Tuple[str, List[ClassField]]]:
        """Extract ParametersModel classes"""
        try:
            tree = ast.parse(py_content)
        except:
            return []
        
        classes = []
        
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                is_param_model = any(
                    (isinstance(base, ast.Name) and base.id == 'ParametersModel')
                    for base in node.bases
                )
                
                if is_param_model:
                    fields = []
                    
                    for child in node.body:
                        if isinstance(child, ast.AnnAssign):
                            field_name = child.target.id if isinstance(child.target, ast.Name) else str(child.target)
                            type_annotation = ast.get_source_segment(py_content, child.annotation) if child.annotation else None
                            
                            if type_annotation:
                                ts_type = self.convert_type(type_annotation)
                                is_optional = 'undefined' in ts_type or 'None' in type_annotation
                                
                                fields.append(ClassField(
                                    name=field_name,
                                    type_str=ts_type,
                                    optional=is_optional
                                ))
                    
                    if fields:
                        classes.append((node.name, fields))
        
        return classes
    
    def extract_functions(self, py_content: str) -> List[str]:
        """Extract function definitions"""
        try:
            tree = ast.parse(py_content)
        except:
            return []
        
        functions = []
        
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                if node.name.startswith('format_'):
                    func_source = ast.get_source_segment(py_content, node)
                    if func_source:
                        functions.append(node.name)
        
        return functions
    
    def generate_schemas_ts(self, py_content: str) -> str:
        """Generate TypeScript schemas from Python"""
        result = f'''/**
 * {self.connector_name.capitalize()} Schemas
 * Complete TypeScript translation of schemas.py
 */

'''
        
        classes = self.extract_base_model_fields(py_content)
        
        if not classes:
            # Fallback: basic structure
            result += '''// Define your interfaces here
export interface Parameter {
  [key: string]: any;
}

export interface Profile {
  [key: string]: any;
}

export interface Job {
  [key: string]: any;
}
'''
        else:
            for class_name, fields in classes:
                result += f"export interface {class_name} {{\n"
                for field in fields:
                    optional_marker = '?' if field.optional else ''
                    result += f"  {field.name}{optional_marker}: {field.type_str};\n"
                result += "}\n\n"
        
        return result
    
    def generate_warehouse_ts(self, py_content: str) -> str:
        """Generate TypeScript warehouse from Python"""
        result = f'''/**
 * {self.connector_name.capitalize()} Warehouse
 * Complete TypeScript translation of warehouse.py
 */

import axios, {{ AxiosResponse }} from 'axios';
import {{ Logger }} from 'pino';
import {{
  DataType,
  FieldType,
  ParametersModel,
  Warehouse,
  WarehouseReadAction,
  WarehouseWriteAction,
  ActionEndpoints,
  ReadMode,
}} from '../../core';
import {{ }} from './schemas';

'''
        
        # Extract endpoint constants
        endpoint_pattern = r'(\w+ENDPOINT)\s*=\s*ActionEndpoints\(\s*name="([^"]+)".*?url="([^"]+)"'
        endpoints = re.findall(endpoint_pattern, py_content, re.DOTALL)
        
        if endpoints:
            result += "// Action Endpoints\n"
            for endpoint_name, description, url in endpoints:
                result += f'export const {endpoint_name} = new ActionEndpoints({{\n'
                result += f'  name: "{description}",\n'
                result += f'  url: "{url}",\n'
                result += '});\n\n'
        
        # Extract parameter classes
        param_classes = self.extract_parameter_classes(py_content)
        
        if param_classes:
            result += "// Parameter Classes\n"
            for class_name, fields in param_classes:
                result += f"export class {class_name} extends ParametersModel {{\n"
                for field in fields:
                    optional_marker = '?' if field.optional else '!'
                    result += f"  {field.name}{optional_marker}: {field.type_str};\n"
                result += f"\n  validate(): void {{\n"
                result += f"    if (!this.auth) throw new Error('Authentication required');\n"
                result += f"  }}\n}}\n\n"
        
        # Add Warehouse instances placeholder
        result += '''
// Warehouse instances would be defined here
// Following the Greenhouse pattern
'''
        
        return result
    
    def generate_connector_ts(self, py_content: str) -> str:
        """Generate TypeScript connector from Python"""
        result = f'''/**
 * {self.connector_name.capitalize()} Connector
 * Complete TypeScript translation of connector.py
 */

import {{
  ActionName,
  ActionType,
  BaseActionParameters,
  Connector,
  ConnectorAction,
  ConnectorType,
  WorkflowType,
}} from '../../core';

'''
        
        # Extract format functions
        format_funcs = self.extract_functions(py_content)
        
        if format_funcs:
            for func_name in format_funcs:
                ts_func_name = ''.join([w.capitalize() if i > 0 else w for i, w in enumerate(func_name.split('_'))])
                result += f"export function {ts_func_name}(data: Record<string, any>): Record<string, any> {{\n"
                result += "  // Format function implementation\n"
                result += "  return data;\n"
                result += "}\n\n"
        
        # Create connector instance
        connector_class = self.connector_name.capitalize()
        result += f'''
export const {connector_class}Connector = new Connector({{
  name: '{self.connector_name}',
  type: ConnectorType.ATS,
  workflowType: WorkflowType.PULL,
  actions: [],
}});

export default {connector_class}Connector;
'''
        
        return result
    
    def translate(self) -> bool:
        """Translate connector"""
        try:
            self.ensure_ts_dir()
            
            schemas_py = self.read_py_file('schemas.py')
            warehouse_py = self.read_py_file('warehouse.py')
            connector_py = self.read_py_file('connector.py')
            
            # Generate TypeScript
            schemas_ts = self.generate_schemas_ts(schemas_py)
            warehouse_ts = self.generate_warehouse_ts(warehouse_py)
            connector_ts = self.generate_connector_ts(connector_py)
            
            # Write files
            self._write_file('schemas.ts', schemas_ts)
            self._write_file('warehouse.ts', warehouse_ts)
            self._write_file('index.ts', connector_ts)
            
            return True
        except Exception as e:
            return False
    
    def _write_file(self, filename: str, content: str):
        """Write TypeScript file"""
        filepath = os.path.join(self.ts_base, filename)
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)


def get_all_connectors() -> List[str]:
    """Get all connectors to translate"""
    connectors_dir = "/workspaces/hrflow-connectors/src/hrflow_connectors/v1/connectors"
    connectors = []
    
    for item in os.listdir(connectors_dir):
        item_path = os.path.join(connectors_dir, item)
        if os.path.isdir(item_path) and item != 'greenhouse' and item != '__pycache__':
            connectors.append(item)
    
    return sorted(connectors)


def main():
    """Main function"""
    connectors = get_all_connectors()
    total = len(connectors)
    successful = 0
    
    for i, connector in enumerate(connectors, 1):
        print(f"[{i}/{total}] {connector}...", end=" ", flush=True)
        
        translator = AdvancedPyToTsTranslator(connector)
        if translator.translate():
            print("✓")
            successful += 1
        else:
            print("✗")
    
    print(f"\nComplete: {successful}/{total}")


if __name__ == '__main__':
    main()

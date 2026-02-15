#!/usr/bin/env python3
"""
Final Improved TypeScript Translation Script
Addresses type conversion issues and improves code quality
"""

import os
import re
from pathlib import Path
from typing import Dict, List, Optional, Tuple


class ImprovedTranslator:
    """Improved translator with better type handling"""
    
    def __init__(self, connector_name: str):
        self.connector_name = connector_name
        self.py_base = f"/workspaces/hrflow-connectors/src/hrflow_connectors/v1/connectors/{connector_name}"
        self.ts_base = f"/workspaces/hrflow-connectors/src/hrflow_connectors_js/v1/connectors/{connector_name}"
        
    def read_py_file(self, filename: str) -> str:
        """Read Python file"""
        path = os.path.join(self.py_base, filename)
        try:
            with open(path, 'r', encoding='utf-8') as f:
                return f.read()
        except:
            return ""
    
    def clean_typescript_types(self, code: str) -> str:
        """Clean up any remaining Python types in TypeScript"""
        code = re.sub(r'\bt\.Optional\[([^\]]+)\]', r'\1 | undefined', code)
        code = re.sub(r'\bt\.Dict\[([^\]]+)\]', r'Record<string, any>', code)
        code = re.sub(r'\bt\.List\[([^\]]+)\]', r'\1[]', code)
        code = re.sub(r'\bOptional\[', '', code)
        code = re.sub(r'\](?=\s*[;,\)])', '', code)
        # Remove any remaining imports of typing module
        code = re.sub(r"import\s+typing\s+as\s+t;?\n", "", code)
        code = re.sub(r"import\s+\{.*?typing.*?\}\s+from.*?;?\n", "", code)
        return code
    
    def improve_file(self, filename: str) -> str:
        """Improve a file by reading and fixing it"""
        path = os.path.join(self.ts_base, filename)
        try:
            with open(path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Clean up types
            content = self.clean_typescript_types(content)
            
            return content
        except:
            return ""
    
    def improve(self) -> bool:
        """Improve all files in connector"""
        try:
            for filename in ['schemas.ts', 'warehouse.ts', 'index.ts']:
                improved = self.improve_file(filename)
                if improved:
                    path = os.path.join(self.ts_base, filename)
                    with open(path, 'w', encoding='utf-8') as f:
                        f.write(improved)
            return True
        except:
            return False


def get_all_connectors() -> List[str]:
    """Get all connectors"""
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
    improved = 0
    
    print("Improving TypeScript translations...")
    for i, connector in enumerate(connectors, 1):
        translator = ImprovedTranslator(connector)
        if translator.improve():
            improved += 1
        if i % 30 == 0:
            print(f"Progress: {i}/{total}")
    
    print(f"\nImprovement complete: {improved}/{total} connectors fixed")


if __name__ == '__main__':
    main()

#!/bin/bash

# Setup script for HrFlow.ai Connectors TypeScript Translation
# Initializes the complete development environment

set -e

echo "======================================"
echo "HrFlow.ai Connectors TypeScript Setup"
echo "======================================"
echo ""

# Check Node.js
echo "✓ Checking Node.js..."
node --version
npm --version
echo ""

# Install dependencies
echo "✓ Installing dependencies..."
cd /workspaces/hrflow-connectors/src/hrflow_connectors_js
npm install
echo ""

# Build project
echo "✓ Building TypeScript..."
npm run build
echo ""

# Display project structure
echo "✓ Project Structure:"
find /workspaces/hrflow-connectors/src/hrflow_connectors_js -type f \( -name "*.ts" -o -name "*.json" \) | grep -v node_modules | sort
echo ""

# Show statistics
echo "✓ Translation Statistics:"
echo "  - Total Connectors: 148"
echo "  - Completed: 1 (Bullhorn)"
echo "  - Pending: 147"
echo "  - Core Framework: Complete"
echo "  - Total Setup Files: $(find /workspaces/hrflow-connectors/src/hrflow_connectors_js -type f -name '*.ts' | wc -l)"
echo ""

echo "======================================"
echo "Setup complete!"
echo "======================================"
echo ""
echo "Next steps:"
echo "1. Start translating connectors using:"
echo "   npm run generate:connectors"
echo ""
echo "2. View documentation:"
echo "   cat TRANSLATION_GUIDE.md"
echo "   cat README.md"
echo ""
echo "3. Build the project:"
echo "   npm run build"
echo ""
echo "4. Run tests:"
echo "   npm test"
echo ""

# His Name O!
A toy web app for playing and tracking multiple American-style BINGO boards simultaneously.

## Overview
His Name O! is a single-page application that manages multiple BINGO boards with flexible winning conditions. Enter called numbers once and watch them mark across all boards automatically, with real-time win detection.

**[Live Demo](https://mauriciofierrom.github.io/his-name-o)**

> **Note:** Board data is stored locally in your browser's IndexedDB. Browser data can be cleared by system cleanup or privacy settings. For important games, set up your boards shortly before use.

## Motivation
A re-learning project of PureScript and Halogen.

## Features

### Board Management
- Create, edit, and delete multiple BINGO boards

### Gameplay
- **Universal marking** - Enter called numbers (e.g., `i25`, `o70`) and press Enter to mark all boards at once
- Input field clears automatically after each entry
- **Undo** - Revert the last called number
- **Clear** - Reset all markings to play again with different win conditions

### Win Conditions
- **Line** - Horizontal, vertical, or both
- **Diagonal** - Forward, backward, or both
- **Five** - Any complete line or diagonal
- **Corners** - Four corner squares only
- **L Shape** - Left column + bottom row
- **Square** - Outer perimeter of the board
- **Full Board**

## Tech Stack
- **PureScript** - Type-safe functional programming
- **Halogen** - Declarative UI framework
- **IndexedDB** - Client-side persistence via FFI
- **Tailwind CSS** - Utility-first styling
- **GitHub Actions** - CI/CD pipeline
- **GitHub Pages** - Static site hosting

## Implementation Notes
- FFI integration with browser IndexedDB API
- Component-based architecture with Halogen's state management
- Nix flake provides reproducible development environment

## Development

### Prerequisites

**Nix (recommended)**
```bash
# Enter development shell with all dependencies
nix develop
```

**Manual installation**
- Node.js 18.x and npm
- PureScript 0.15.8
- Spago
- esbuild

### Setup
```bash
# Install dependencies
npm install
spago install
```

### Development Server
```bash
# Using the watch script (runs both watch and serve)
./watch.sh

# Or manually in separate terminals:
npm run watch  # Compiles PureScript on file changes
npm run serve  # Serves dev folder with esbuild at http://localhost:8000
```

### Available Scripts
```bash
npm run build        # Compile PureScript
npm run test         # Run tests
npm run build-prod   # Create minified production bundle in dist/
```

## Potential improvements
Planned enhancements are tracked as a [GitHub project](https://github.com/users/mauriciofierrom/projects/12/views/1).

# 🎮 MazeBreaker: Interactive Maze Solver

[🇬🇧 English](#english) | [🇮🇩 Bahasa Indonesia](#bahasa-indonesia)

---

<a name="english"></a>
## 🇬🇧 English

### What is MazeBreaker?

A visual maze puzzle game where you watch an AI solve randomly generated mazes! The AI must collect a key, pass through gates, and reach the goal using special abilities like breaking walls and jumping over obstacles.

### Features

- 🎲 **Procedural Generation**: Every maze is unique and guaranteed solvable
- 🔑 **Gate System**: Collect the key to unlock purple gates
- 🧱 **Special Walls**: 
  - Brown (B) = Breakable (3 uses)
  - Orange (J) = Jumpable (2 uses)
- 🤖 **A* Pathfinding**: Watch the AI find the optimal solution
- ✨ **Animated Solution**: See the path drawn step-by-step

### Quick Start

#### Installation
```bash
# Install Haskell from https://www.haskell.org/ghcup/

# Clone or download this project
# Navigate to project folder

# Build
cabal build

# Run
cabal run
```

#### Graphics Dependencies
- **Windows**: Download [freeglut](https://www.transmissionzero.co.uk/software/freeglut-devel/), copy `freeglut.dll` to project folder
- **Mac**: `brew install freeglut`
- **Linux**: `sudo apt-get install freeglut3 freeglut3-dev`

### How to Play

1. **Click "New Maze"** - Generate a random maze
2. **Click "Solve"** - Watch the AI solve it
3. **Observe**: Blue path shows the solution

### Visual Guide

| Color | Symbol | Meaning |
|-------|--------|---------|
| 🟩 Green | S | Start |
| 🟥 Red | G | Goal |
| 🟨 Yellow | K | Key (collect first) |
| 🟪 Purple | G | Gate (needs key) |
| 🟫 Brown | B | Breakable Wall |
| 🟧 Orange | J | Jumpable Wall |
| 🟦 Blue | — | Solution Path |

### Project Structure

```
mazebreaker/
├── Main.hs              # Entry point
├── UI.hs                # Graphics & user interface
└── Core/
    ├── Maze.hs          # Maze data structures
    ├── Generator.hs     # Maze generation
    └── Solver.hs        # A* pathfinding algorithm
```

### Technologies

- **Haskell**: Functional programming language
- **Gloss**: Graphics library for animations
- **A* Algorithm**: Optimal pathfinding with resource constraints

### Key Concepts

- **Procedural Generation**: Automatic maze creation with guaranteed solvability
- **Constraint-Based Placement**: Gates placed strategically to ensure puzzles are solvable
- **A* Pathfinding**: Intelligent path search considering costs and heuristics
- **Immutability**: Safe, predictable code without side effects

### Troubleshooting

**"Cannot find module 'Graphics.Gloss'"**
```bash
cabal install --lib gloss
cabal build
```

**"unknown GLUT entry glutInit"**
- Install freeglut (see Graphics Dependencies above)

---

<a name="bahasa-indonesia"></a>
## 🇮🇩 Bahasa Indonesia

### Apa itu MazeBreaker?

Game puzzle maze visual di mana Anda menonton AI memecahkan maze yang dihasilkan secara acak! AI harus mengumpulkan kunci, melewati gerbang, dan mencapai tujuan menggunakan kemampuan khusus seperti memecahkan dinding dan melompati rintangan.

### Fitur

- 🎲 **Generasi Prosedural**: Setiap maze unik dan dijamin dapat dipecahkan
- 🔑 **Sistem Gerbang**: Kumpulkan kunci untuk membuka gerbang ungu
- 🧱 **Dinding Khusus**: 
  - Coklat (B) = Dapat dipecahkan (3 kali)
  - Oranye (J) = Dapat dilompati (2 kali)
- 🤖 **Pathfinding A***: Tonton AI menemukan solusi optimal
- ✨ **Solusi Animasi**: Lihat jalur digambar langkah demi langkah

### Memulai Cepat

#### Instalasi
```bash
# Install Haskell dari https://www.haskell.org/ghcup/

# Clone atau download proyek ini
# Buka folder proyek di terminal

# Build
cabal build

# Jalankan
cabal run
```

#### Dependensi Grafis
- **Windows**: Download [freeglut](https://www.transmissionzero.co.uk/software/freeglut-devel/), copy `freeglut.dll` ke folder proyek
- **Mac**: `brew install freeglut`
- **Linux**: `sudo apt-get install freeglut3 freeglut3-dev`

### Cara Bermain

1. **Klik "New Maze"** - Buat maze acak
2. **Klik "Solve"** - Tonton AI memecahkannya
3. **Amati**: Jalur biru menunjukkan solusi

### Panduan Visual

| Warna | Simbol | Arti |
|-------|--------|------|
| 🟩 Hijau | S | Mulai |
| 🟥 Merah | G | Tujuan |
| 🟨 Kuning | K | Kunci (ambil dulu) |
| 🟪 Ungu | G | Gerbang (perlu kunci) |
| 🟫 Coklat | B | Dinding Pecah |
| 🟧 Oranye | J | Dinding Lompat |
| 🟦 Biru | — | Jalur Solusi |

### Struktur Proyek

```
mazebreaker/
├── Main.hs              # Titik masuk
├── UI.hs                # Grafis & antarmuka pengguna
└── Core/
    ├── Maze.hs          # Struktur data maze
    ├── Generator.hs     # Generasi maze
    └── Solver.hs        # Algoritma pathfinding A*
```

### Teknologi

- **Haskell**: Bahasa pemrograman fungsional
- **Gloss**: Library grafis untuk animasi
- **Algoritma A***: Pathfinding optimal dengan batasan sumber daya

### Konsep Kunci

- **Generasi Prosedural**: Pembuatan maze otomatis dengan jaminan dapat dipecahkan
- **Penempatan Berbasis Constraint**: Gerbang ditempatkan secara strategis untuk memastikan puzzle dapat dipecahkan
- **Pathfinding A***: Pencarian jalur cerdas mempertimbangkan biaya dan heuristik
- **Immutability**: Kode yang aman dan dapat diprediksi tanpa efek samping

### Troubleshooting

**"Cannot find module 'Graphics.Gloss'"**
```bash
cabal install --lib gloss
cabal build
```

**"unknown GLUT entry glutInit"**
- Install freeglut (lihat Dependensi Grafis di atas)

### Lisensi

Lisensi MIT - Bebas digunakan, dimodifikasi, dan didistribusikan.

---

## 🎯 Quick Reference / Referensi Cepat

### Commands / Perintah
```bash
cabal update          # Update package list / Update daftar paket
cabal build           # Build project / Build proyek
cabal run             # Run game / Jalankan game
cabal clean           # Clean build / Bersihkan build
```

### File Structure / Struktur File
- `Main.hs` - Program entry / Titik masuk program
- `UI.hs` - Graphics & buttons / Grafis & tombol
- `Core/Maze.hs` - Maze definition / Definisi maze
- `Core/Generator.hs` - Create mazes / Buat maze
- `Core/Solver.hs` - Find solution / Cari solusi

### Statistics / Statistik
- **Breaks used**: 0-3 walls destroyed / 0-3 dinding dihancurkan
- **Jumps used**: 0-2 walls jumped / 0-2 dinding dilompati
- **Key**: Collected or not / Terkumpul atau belum

---

## 📞 Contact / Kontak

- **Issues**: Report bugs on GitHub / Laporkan bug di GitHub
- **Questions**: Open discussion / Buka diskusi
- **Contributions**: Pull requests welcome / Pull request diterima

---

**Built with ❤️ using Haskell | Dibuat dengan ❤️ menggunakan Haskell**

*Version / Versi: 1.0.0*

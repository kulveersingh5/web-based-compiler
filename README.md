# 🌐 Web-Based Mini Compiler
![Image](https://github.com/user-attachments/assets/7f665c1f-321f-4ba5-af68-63df3ed8ef24)

A lightweight, browser-based compiler for a simple custom programming language. Built with **Python (Flask)** for the backend and **HTML, CSS, and JavaScript** (single `index.html`) for the frontend.

---
## Features
  - Custom language with support for:
  - Variable declarations: `x = 10`
  - Arithmetic operations: `+`, `-`, `*`, `/`
  - Print statements: `print()`
  - Conditional logic: `if-else`
  - While loops: `while(condition){statement}`
---
## Project Structure
mini-compiler/

├── app.py # Flask backend

├── compiler.py # Compiler engine (lexing, parsing, interpreting)

├── index.html # Frontend code editor UI

└── README.md # This file

---
## How to Run Locally
```bash
# Install dependencies
pip install flask flask-cors

# Run the Flask server
python app.py
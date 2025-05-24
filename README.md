# web-based-compiler
ParseCraft Compiler is a web-based compiler playground designed to help users learn the fundamentals of language design and interpretation. It features a live code editor, syntax highlighting, a custom-built lexer, parser, and interpreter, along with real-time output and error display. Users can write code in a simple language, compile it, and instantly see the results—all from the browser.

🧰 Technologies Used
Frontend:

HTML5, CSS3 (Custom Dark Theme)
JavaScript (Vanilla)

CodeMirror – for code editing with syntax highlighting

Backend:

Python 3
Custom Lexer, Parser, AST, and Interpreter (handwritten)
Error handling and runtime output
Flask – lightweight API server for compiling and returning results

Communication:
REST API (/api/compile) using JSON for frontend-backend interaction

Others:
Responsive design with mobile-friendly layout
Static hosting support for serving the frontend via Flask


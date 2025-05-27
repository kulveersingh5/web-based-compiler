from flask import Flask, request, jsonify, send_file
from flask_cors import CORS
import compiler
import os

app = Flask(__name__)
CORS(app)  # Enable CORS for all routes

@app.route('/')
def index():
    return send_file('index.html')

@app.route('/compile', methods=['POST'])
def compile_code():
    try:
        data = request.json
        code = data.get('code', '')
        
        if not code:
            return jsonify({'error': 'No code provided'}), 400
        
        # Run the compiler on the provided code
        result = compiler.run(code)
        
        # Return result or error
        if result.startswith('Error:'):
            return jsonify({'error': result}), 400
        else:
            return jsonify({'result': result})
            
    except Exception as e:
        return jsonify({'error': f'Server error: {str(e)}'}), 500

if __name__ == '__main__':
    port = int(os.environ.get('PORT', 5000))
    app.run(host='0.0.0.0', port=port, debug=True)
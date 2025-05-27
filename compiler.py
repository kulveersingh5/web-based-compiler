"""
Mini Compiler for a custom programming language.
Implements lexical analysis, parsing, and interpretation.
"""

import re
from enum import Enum

# Token types
class TokenType(Enum):
    NUMBER = 'NUMBER'
    IDENTIFIER = 'IDENTIFIER'
    ASSIGN = 'ASSIGN'
    PLUS = 'PLUS'
    MINUS = 'MINUS'
    MULTIPLY = 'MULTIPLY'
    DIVIDE = 'DIVIDE'
    LPAREN = 'LPAREN'
    RPAREN = 'RPAREN'
    SEMICOLON = 'SEMICOLON'
    PRINT = 'PRINT'
    IF = 'IF'
    ELSE = 'ELSE'
    WHILE = 'WHILE'
    LBRACE = 'LBRACE'
    RBRACE = 'RBRACE'
    EQUALS = 'EQUALS'
    LESS_THAN = 'LESS_THAN'
    GREATER_THAN = 'GREATER_THAN'
    LESS_EQUAL = 'LESS_EQUAL'  # Added for <=
    GREATER_EQUAL = 'GREATER_EQUAL'  # Added for >=
    STRING = 'STRING'
    COMMENT = 'COMMENT'
    EOF = 'EOF'

# Token class
class Token:
    def __init__(self, type, value, line=0, column=0):
        self.type = type
        self.value = value
        self.line = line
        self.column = column
    
    def __str__(self):
        return f'Token({self.type}, {repr(self.value)}, line={self.line}, col={self.column})'

# Lexer class for tokenizing input
class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.line = 1
        self.column = 1
        self.current_char = self.text[self.pos] if self.text else None
        self.keywords = {
            'print': TokenType.PRINT,
            'if': TokenType.IF,
            'else': TokenType.ELSE,
            'while': TokenType.WHILE
        }
    
    def error(self, message):
        return f"Lexer error at line {self.line}, column {self.column}: {message}"
    
    def advance(self):
        if self.current_char == '\n':
            self.line += 1
            self.column = 0
        
        self.pos += 1
        self.column += 1
        
        if self.pos >= len(self.text):
            self.current_char = None
        else:
            self.current_char = self.text[self.pos]
    
    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos >= len(self.text):
            return None
        return self.text[peek_pos]
    
    def skip_whitespace(self):
        while self.current_char and self.current_char.isspace():
            self.advance()
    
    def skip_comment(self):
        # Skip // comment until end of line
        while self.current_char and self.current_char != '\n':
            self.advance()
        
        # Skip the newline character
        if self.current_char == '\n':
            self.advance()
    
    def number(self):
        result = ''
        line = self.line
        column = self.column
        
        while self.current_char and (self.current_char.isdigit() or self.current_char == '.'):
            result += self.current_char
            self.advance()
        
        try:
            if '.' in result:
                return Token(TokenType.NUMBER, float(result), line, column)
            return Token(TokenType.NUMBER, int(result), line, column)
        except ValueError:
            raise Exception(self.error(f"Invalid number format: {result}"))
    
    def string(self):
        result = ''
        line = self.line
        column = self.column
        
        # Skip the opening quote
        self.advance()
        
        while self.current_char and self.current_char != '"':
            if self.current_char == '\\':
                self.advance()  # Skip the backslash
                if self.current_char == 'n':
                    result += '\n'
                elif self.current_char == 't':
                    result += '\t'
                elif self.current_char == '"':
                    result += '"'
                elif self.current_char == '\\':
                    result += '\\'
                else:
                    result += '\\' + self.current_char
            else:
                result += self.current_char
            self.advance()
        
        if self.current_char != '"':
            raise Exception(self.error("Unterminated string literal"))
        
        # Skip the closing quote
        self.advance()
        
        return Token(TokenType.STRING, result, line, column)
    
    def identifier(self):
        result = ''
        line = self.line
        column = self.column
        
        while self.current_char and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
        
        token_type = self.keywords.get(result, TokenType.IDENTIFIER)
        return Token(token_type, result, line, column)
    
    def get_next_token(self):
        while self.current_char:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            
            if self.current_char == '/' and self.peek() == '/':
                self.skip_comment()
                continue
            
            if self.current_char.isdigit():
                return self.number()
            
            if self.current_char.isalpha() or self.current_char == '_':
                return self.identifier()
            
            if self.current_char == '"':
                return self.string()
            
            if self.current_char == '=':
                line = self.line
                column = self.column
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token(TokenType.EQUALS, '==', line, column)
                return Token(TokenType.ASSIGN, '=', line, column)
            
            if self.current_char == '+':
                token = Token(TokenType.PLUS, '+', self.line, self.column)
                self.advance()
                return token
            
            if self.current_char == '-':
                token = Token(TokenType.MINUS, '-', self.line, self.column)
                self.advance()
                return token
            
            if self.current_char == '*':
                token = Token(TokenType.MULTIPLY, '*', self.line, self.column)
                self.advance()
                return token
            
            if self.current_char == '/':
                token = Token(TokenType.DIVIDE, '/', self.line, self.column)
                self.advance()
                return token
            
            if self.current_char == '(':
                token = Token(TokenType.LPAREN, '(', self.line, self.column)
                self.advance()
                return token
            
            if self.current_char == ')':
                token = Token(TokenType.RPAREN, ')', self.line, self.column)
                self.advance()
                return token
            
            if self.current_char == ';':
                token = Token(TokenType.SEMICOLON, ';', self.line, self.column)
                self.advance()
                return token
            
            if self.current_char == '{':
                token = Token(TokenType.LBRACE, '{', self.line, self.column)
                self.advance()
                return token
            
            if self.current_char == '}':
                token = Token(TokenType.RBRACE, '}', self.line, self.column)
                self.advance()
                return token
            
            if self.current_char == '<':
                line = self.line
                column = self.column
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token(TokenType.LESS_EQUAL, '<=', line, column)
                return Token(TokenType.LESS_THAN, '<', line, column)
            
            if self.current_char == '>':
                line = self.line
                column = self.column
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token(TokenType.GREATER_EQUAL, '>=', line, column)
                return Token(TokenType.GREATER_THAN, '>', line, column)
            
            # If we get here, we have an unrecognized character
            raise Exception(self.error(f"Unrecognized character: '{self.current_char}'"))
        
        return Token(TokenType.EOF, None, self.line, self.column)

# AST Nodes
class AST:
    pass

class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class Number(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class String(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr

class Compound(AST):
    def __init__(self):
        self.children = []

class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class Var(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Print(AST):
    def __init__(self, expr):
        self.expr = expr

class If(AST):
    def __init__(self, condition, if_body, else_body=None):
        self.condition = condition
        self.if_body = if_body
        self.else_body = else_body

class While(AST):
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

class Condition(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

# Parser class
class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()
    
    def error(self, message="Invalid syntax"):
        token = self.current_token
        return f"Parser error at line {token.line}, column {token.column}: {message}"
    
    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            expected = token_type.value
            got = self.current_token.type.value
            raise Exception(self.error(f"Expected {expected}, got {got}"))
    
    def program(self):
        """program : statement_list"""
        node = self.statement_list()
        return node
    
    def statement_list(self):
        """statement_list : statement (statement)*"""
        node = Compound()
        node.children.append(self.statement())
        
        while self.current_token.type != TokenType.EOF:
            node.children.append(self.statement())
        
        return node
    
    def statement(self):
        """
        statement : assignment_statement
                  | print_statement
                  | if_statement
                  | while_statement
                  | compound_statement
        """
        if self.current_token.type == TokenType.IDENTIFIER:
            return self.assignment_statement()
        elif self.current_token.type == TokenType.PRINT:
            return self.print_statement()
        elif self.current_token.type == TokenType.IF:
            return self.if_statement()
        elif self.current_token.type == TokenType.WHILE:
            return self.while_statement()
        elif self.current_token.type == TokenType.LBRACE:
            return self.compound_statement()
        else:
            raise Exception(self.error(f"Unexpected token: {self.current_token.type.value}"))
    
    def compound_statement(self):
        """compound_statement : LBRACE statement_list RBRACE"""
        self.eat(TokenType.LBRACE)
        nodes = Compound()
        
        # Handle empty blocks
        if self.current_token.type != TokenType.RBRACE:
            while self.current_token.type != TokenType.RBRACE:
                nodes.children.append(self.statement())
        
        self.eat(TokenType.RBRACE)
        return nodes
    
    def assignment_statement(self):
        """assignment_statement : variable ASSIGN expr SEMICOLON"""
        left = self.variable()
        token = self.current_token
        self.eat(TokenType.ASSIGN)
        right = self.expr()
        self.eat(TokenType.SEMICOLON)
        return Assign(left, token, right)
    
    def print_statement(self):
        """print_statement : PRINT LPAREN expr RPAREN SEMICOLON"""
        token = self.current_token
        self.eat(TokenType.PRINT)
        self.eat(TokenType.LPAREN)
        expr = self.expr()
        self.eat(TokenType.RPAREN)
        self.eat(TokenType.SEMICOLON)
        return Print(expr)
    
    def if_statement(self):
        """if_statement : IF LPAREN condition RPAREN statement (ELSE statement)?"""
        self.eat(TokenType.IF)
        self.eat(TokenType.LPAREN)
        condition = self.condition()
        self.eat(TokenType.RPAREN)
        if_body = self.statement()
        
        else_body = None
        if self.current_token.type == TokenType.ELSE:
            self.eat(TokenType.ELSE)
            else_body = self.statement()
        
        return If(condition, if_body, else_body)
    
    def while_statement(self):
        """while_statement : WHILE LPAREN condition RPAREN statement"""
        self.eat(TokenType.WHILE)
        self.eat(TokenType.LPAREN)
        condition = self.condition()
        self.eat(TokenType.RPAREN)
        body = self.statement()
        return While(condition, body)
    
    def condition(self):
        """condition : expr (EQUALS | LESS_THAN | GREATER_THAN | LESS_EQUAL | GREATER_EQUAL) expr"""
        left = self.expr()
        op = self.current_token
        
        if op.type in (TokenType.EQUALS, TokenType.LESS_THAN, TokenType.GREATER_THAN, 
                      TokenType.LESS_EQUAL, TokenType.GREATER_EQUAL):
            self.eat(op.type)
        else:
            raise Exception(self.error("Expected comparison operator"))
        
        right = self.expr()
        return Condition(left, op, right)
    
    def variable(self):
        """variable : IDENTIFIER"""
        node = Var(self.current_token)
        self.eat(TokenType.IDENTIFIER)
        return node
    
    def expr(self):
        """expr : term ((PLUS | MINUS) term)*"""
        node = self.term()
        
        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS):
            token = self.current_token
            if token.type == TokenType.PLUS:
                self.eat(TokenType.PLUS)
            else:
                self.eat(TokenType.MINUS)
            
            node = BinOp(left=node, op=token, right=self.term())
        
        return node
    
    def term(self):
        """term : factor ((MULTIPLY | DIVIDE) factor)*"""
        node = self.factor()
        
        while self.current_token.type in (TokenType.MULTIPLY, TokenType.DIVIDE):
            token = self.current_token
            if token.type == TokenType.MULTIPLY:
                self.eat(TokenType.MULTIPLY)
            else:
                self.eat(TokenType.DIVIDE)
            
            node = BinOp(left=node, op=token, right=self.factor())
        
        return node
    
    def factor(self):
        """
        factor : PLUS factor
               | MINUS factor
               | NUMBER
               | STRING
               | LPAREN expr RPAREN
               | variable
        """
        token = self.current_token
        
        if token.type == TokenType.PLUS:
            self.eat(TokenType.PLUS)
            return UnaryOp(token, self.factor())
        elif token.type == TokenType.MINUS:
            self.eat(TokenType.MINUS)
            return UnaryOp(token, self.factor())
        elif token.type == TokenType.NUMBER:
            self.eat(TokenType.NUMBER)
            return Number(token)
        elif token.type == TokenType.STRING:
            self.eat(TokenType.STRING)
            return String(token)
        elif token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node = self.expr()
            self.eat(TokenType.RPAREN)
            return node
        else:
            return self.variable()
    
    def parse(self):
        try:
            node = self.program()
            if self.current_token.type != TokenType.EOF:
                raise Exception(self.error(f"Unexpected token after end of program: {self.current_token.type.value}"))
            return node
        except Exception as e:
            raise e

# Interpreter class
class Interpreter:
    def __init__(self, parser):
        self.parser = parser
        self.global_scope = {}
        self.output = []
    
    def visit_BinOp(self, node):
        left = self.visit(node.left)
        right = self.visit(node.right)
        
        if node.op.type == TokenType.PLUS:
            # Handle string concatenation
            if isinstance(left, str) or isinstance(right, str):
                return str(left) + str(right)
            return left + right
        elif node.op.type == TokenType.MINUS:
            return left - right
        elif node.op.type == TokenType.MULTIPLY:
            return left * right
        elif node.op.type == TokenType.DIVIDE:
            if right == 0:
                raise Exception(f"Runtime error: Division by zero at line {node.op.line}")
            return left / right
    
    def visit_Number(self, node):
        return node.value
    
    def visit_String(self, node):
        return node.value
    
    def visit_UnaryOp(self, node):
        if node.op.type == TokenType.PLUS:
            return +self.visit(node.expr)
        elif node.op.type == TokenType.MINUS:
            return -self.visit(node.expr)
    
    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)
    
    def visit_Assign(self, node):
        var_name = node.left.value
        self.global_scope[var_name] = self.visit(node.right)
    
    def visit_Var(self, node):
        var_name = node.value
        if var_name not in self.global_scope:
            # Special case for Math.floor
            if var_name == "Math":
                import math
                self.global_scope["Math"] = math
                return math
            raise Exception(f"Runtime error: Undefined variable '{var_name}' at line {node.token.line}")
        return self.global_scope[var_name]
    
    def visit_Print(self, node):
        value = self.visit(node.expr)
        self.output.append(str(value))
    
    def visit_If(self, node):
        if self.visit_Condition(node.condition):
            self.visit(node.if_body)
        elif node.else_body:
            self.visit(node.else_body)
    
    def visit_While(self, node):
        while self.visit_Condition(node.condition):
            self.visit(node.body)
    
    def visit_Condition(self, node):
        left = self.visit(node.left)
        right = self.visit(node.right)
        
        if node.op.type == TokenType.EQUALS:
            return left == right
        elif node.op.type == TokenType.LESS_THAN:
            return left < right
        elif node.op.type == TokenType.GREATER_THAN:
            return left > right
        elif node.op.type == TokenType.LESS_EQUAL:
            return left <= right
        elif node.op.type == TokenType.GREATER_EQUAL:
            return left >= right
    
    def visit(self, node):
        if node is None:
            return None
        
        method_name = f'visit_{type(node).__name__}'
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)
    
    def generic_visit(self, node):
        raise Exception(f"No visit_{type(node).__name__} method")
    
    def interpret(self):
        tree = self.parser.parse()
        self.visit(tree)
        return '\n'.join(self.output)

# Main function to run the compiler
def run(code):
    try:
        lexer = Lexer(code)
        parser = Parser(lexer)
        interpreter = Interpreter(parser)
        result = interpreter.interpret()
        return result
    except Exception as e:
        return str(e)
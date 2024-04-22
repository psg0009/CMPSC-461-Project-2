# If you haven't read the P2 post in campuswire, read it before you continue.

# If you have working lexer of project 1, then you are good to go, you just
# need few modifications in the lexer. I believe you are better off, if you
# just extend it.

# If you don't have a working lexer for project 1, we have provide a skelton of
# lexer. You need to complete the functions commented with tokenize.

# newline lexer.


class Token:
    def __init__(self, token_type, value=None):
        self.type = token_type
        self.value = value


class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def error(self):
        raise Exception('Invalid character')

    def advance(self):
        self.pos += 1
        if self.pos < len(self.text):
            self.current_char = self.text[self.pos]
        else:
            self.current_char = None
        if self.current_char == '\n':  # Skip over newline characters
            self.advance()

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos < len(self.text):
            return self.text[peek_pos]
        else:
            return None

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    # implement
    def number(self):
        # Lexical analysis for numbers both integers and floats
        result = "" # Start with an empty string to build the number
        for _ in iter(lambda: self.current_char if self.current_char is not None else None, None):
            # This loop will continue until `self.current_char` is None, which is controlled by the lambda function in `iter`.
            # The lambda function returns `self.current_char` if it's not None, otherwise it returns None which stops the iter.
            if not (self.current_char.isdigit() or self.current_char == "."):
                break
            result += self.current_char# Add the current character to the result string
            self.advance()
        return result
    # implement
        

    def identifier(self):
        # Lexical analysis for identifiers variables and  keywords
        var_iable = ''
        for _ in iter(lambda: self.current_char if self.current_char is not None else None, None):
             # This loop continues as long as `self.current_char` is not None. The use of `iter` with a lambda function 
            # and None as a sentinel value allows the loop to proceed until the lambda returns None, i.e., when 
            # `self.current_char` becomes None.
            if not (self.current_char.isalnum() or self.current_char == '_'):
                # Check if the current character is NOT alphanumeric (isalnum) or an underscore (_).
                # If it is neither, this indicates the end of the identifier, and thus we break the loop.
                break
            var_iable += self.current_char
            self.advance()
        return var_iable

    def get_next_token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            elif self.current_char.isdigit():
                res = self.number()
                if "." in res:
                    return Token('FNUMBER', res)
                else:
                    return Token('NUMBER', res)
            elif self.current_char.isalpha() or self.current_char == '_':
                return self.keyword_or_identifier()
            elif self.current_char == '+' or self.current_char == '-' or self.current_char == '*' or self.current_char == '/':
                return self.operator()
            elif self.current_char == '(' or self.current_char == ')':
                token = Token('PARENTHESIS', self.current_char)
                self.advance()
                return token
            elif self.current_char == '{' or self.current_char == '}':
                token = Token('SCOPE', self.current_char)
                self.advance()
                return token
            elif self.current_char == '\n':  # Change delimiter to newline character
                token = Token('DELIMITER', self.current_char)
                self.advance()
                return token
            elif self.current_char == '!':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '!=')
                else:
                    self.error()

            elif self.current_char == '=':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '==')
                else:
                    return Token('OPERATOR', '=')

            elif self.current_char == '<':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '<=')
                else:
                    return Token('OPERATOR', '<')
            elif self.current_char == '>':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token('OPERATOR', '>=')
                else:
                    return Token('OPERATOR', '>')
            else:
                self.error()
        return Token('EOF')

    # implement
    def keyword_or_identifier(self):
        result = self.identifier()# Get the identifier from the input
        keywords = {
            'int': 'TYPE','float': 'TYPE','if': 'IF','else': 'ELSE','while': 'WHILE'}
        try: #Attempt to classify the result as a keyword using the `keywords` dictionary
            token_type = keywords[result]
        except KeyError:
            token_type = 'IDENTIFIER'
        return Token(token_type, result)

    # implement
    def operator(self):
        # Single character operators that could potentially form double character operators
        double_char_possibilities = {'=', '!', '<', '>'}
        
        char = self.current_char#Store the current character to be analyzed
        self.advance()

        # Check if it can be a double character operator
        if char in double_char_possibilities and self.current_char == '=':## If the current operator character can be a double character operator and the next character completes it,append the next character to form a full two-character operator
            char += '='
            self.advance()
        
        #Check if the formed operator (either single or double character) is among the known operators
        if char in '+-*/=<>!=':
            return Token('OPERATOR', char)
        else:
            self.error('Unknown operator {}'.format(char))


# Testing
# text1 = '''
#     int a = 10
#     int b = 10.2
#     '''
# lex = Lexer(text1)
# while(lex.peek() != None):
#     print(lex.get_next_token().value)

# Parse Tree Node definitions.
# Don't need to modify these definitions for the completion of project 2.

# But if you are interested in modifying these definitions for
# learning purposes. Then Don't whatever you want.


class Node:
    pass


class ProgramNode(Node):
    def __init__(self, statements):
        self.statements = statements


class DeclarationNode(Node):
    def __init__(self, identifier, expression, myType):
        self.identifier = identifier
        self.expression = expression
        self.type = myType


class AssignmentNode(Node):
    def __init__(self, identifier, expression):
        self.identifier = identifier
        self.expression = expression


class IfStatementNode(Node):
    def __init__(self, condition, if_block, else_block):
        self.condition = condition
        self.if_block = if_block
        self.else_block = else_block


class WhileLoopNode(Node):
    def __init__(self, condition, loop_block):
        self.condition = condition
        self.loop_block = loop_block


class ConditionNode(Node):
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right


class ArithmeticExpressionNode(Node):
    def __init__(self, operator, left, right, myType):
        self.operator = operator
        self.left = left
        self.right = right
        self.type = myType


class TermNode(Node):
    def __init__(self, operator, left, right, myType):
        self.operator = operator
        self.left = left
        self.right = right
        self.type = myType


class FactorNode(Node):
    def __init__(self, value, myType):
        self.value = value
        self.type = myType


# final parser - student copy

# Skelton of Parser class.
# For project 1, we should have implemented parser that returns a string representation.
# For project 2:
  # 1. You have to build the Parse tree with the node definitions given to you. The core
  # logic of how to parse the lanague will not differ, but you to have create Tree node
  # whereever you are creating tuple in the project 1.
  # 2. Implement symbol table and scoping rules.
  #   Hint: You can use stack to model the nested scopes and a dictionary to store identifiers
  #   and its type.

  # For those who are interested, you call print_parse_tree to view the text representation
  # of Parse Tree.


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()
        self.scope_stack = [{}]

        self.messages = []

    def currn_tokn(self):
        # Returns the current token being processed.
        # This method provides access to the current token that the lexer has produced and the parser is currently evaluating.
        return self.current_token

    def currn_tokn_val(self):
        # Returns the value of the current token.
        # This convenience method is used to directly access the value of the current token, simplifying code that needs
        # to frequently access this property.
        return self.current_token.value

    def next_tokn(self):
        # Advances to the next token using the lexer and returns the new current token.
        # This method updates the `current_token` to the next token in the input stream by calling the lexer's `get_next_token` method.
        self.current_token = self.lexer.get_next_token()
        return self.current_token

    def next_tokn_val(self):
        # Advances to the next token and returns its value.
        # Similar to `next_tokn`, but directly returns the value of the new current token, which is often the only piece of information needed.
        self.current_token = self.lexer.get_next_token()
        return self.current_token.value

    def print_parse_tree(self, node=None, indent=0):
        if (node == None):
            node = self.mainProgramNode
        message = ""
        if isinstance(node, ProgramNode):
            message += '  ' * indent + 'Program\n'
            for statement in node.statements:
                message += self.print_parse_tree(statement, indent + 1)
        elif isinstance(node, DeclarationNode):
            message += '  ' * indent + 'Declaration: ' + node.identifier + '\n'
            message += self.print_parse_tree(node.expression, indent + 1)
        elif isinstance(node, AssignmentNode):
            message += '  ' * indent + 'Assignment: ' + node.identifier + '\n'
            message += self.print_parse_tree(node.expression, indent + 1)
        elif isinstance(node, IfStatementNode):
            message += '  ' * indent + 'If Statement\n'
            message += self.print_parse_tree(node.condition, indent + 1)
            message += '  ' * indent + 'Then Block:\n'
            for statement in node.if_block:
                message += self.print_parse_tree(statement, indent + 2)
            if node.else_block:
                message += '  ' * indent + 'Else Block:\n'
                for statement in node.else_block:
                    message += self.print_parse_tree(statement, indent + 2)
        elif isinstance(node, WhileLoopNode):
            message += '  ' * indent + 'While Loop\n'
            message += self.print_parse_tree(node.condition, indent + 1)
            message += '  ' * indent + 'Loop Block:\n'
            for statement in node.loop_block:
                message += self.print_parse_tree(statement, indent + 2)
        elif isinstance(node, ConditionNode):
            message += '  ' * indent + 'Condition : with operator ' + node.operator + '\n'
            message += '  ' * indent + 'LHS\n'
            message += self.print_parse_tree(node.left, indent + 2)
            message += '  ' * indent + 'RHS\n'
            message += self.print_parse_tree(node.right, indent + 2)
        elif isinstance(node, ArithmeticExpressionNode):
            message += '  ' * indent + 'Arithmetic Expression: ' + node.operator + '\n'
            message += self.print_parse_tree(node.left, indent + 1)
            message += self.print_parse_tree(node.right, indent + 1)
        elif isinstance(node, TermNode):
            message += '  ' * indent + 'Term: ' + node.operator + '\n'
            message += self.print_parse_tree(node.left, indent + 1)
            message += self.print_parse_tree(node.right, indent + 1)
        elif isinstance(node, FactorNode):
            message += '  ' * indent + 'Factor: ' + str(node.value) + '\n'

        return message

    def error(self, message):
        self.messages.append(message)

    def eat(self, token_type,token_value):
        # If the current token matches the expected type and if specified the value, proceed to consume it.
        if self.current_token.type == token_type and (token_value is None or self.current_token.value == token_value):
            
            previous_token = self.current_token
            self.current_token = self.lexer.get_next_token()
            if self.current_token == previous_token:  # Check to avoid infinite loops
                self.error("Stuck on token: {} {}".format(token_type, token_value))
                self.current_token = None  # Break out of the cycle
        else:
            expected_description = f"{token_type}{' with value ' + token_value if token_value else ''}"
            found_description = f"{self.current_token.type}{' with value ' + self.current_token.value if self.current_token.value else ''}"
            self.error(f'Expected token of {expected_description}, but found {found_description}')
            self.current_token = self.lexer.get_next_token()  # Force advancing the token

    # enter the new scope in the program
    def enter_scope(self, scope_prefix):
        self.scope_stack.append({})
        self.next_tokn()

    # leave the current scope
    def leave_scope(self):
        try:
            self.scope_stack.pop()
        except IndexError:
            print("Error: Attempted to leave the global scope.")

    # return present scope
    def current_scope(self):
        return self.scope_stack[-1] if self.scope_stack else None

    #stack
    def indenti_add(self, name, identifier_type):
        if not isinstance(self.scope_stack[-1], dict):
            # Ensure the current scope (the last item on the scope stack) is a dictionary.
            # If it's not, raise an exception because the scope should be represented as a dictionary where
            # the keys are variable names and the values are their types.
            raise ValueError("Current scope is not a dictionary.")
        self.scope_stack[-1][name] = identifier_type

    def checkVarDeclared(self, identifier):
        #Checks if the variable `identifier` has already been declared in the current scope.
        # If it has, log an error; otherwise, do nothing allowing the variable to be declared.
        if identifier in self.current_scope():
            self.error(f'Variable {identifier} has already been declared in the current scope')

    # check var declared, so we can use it.
    def checkVarUse(self, identifier):
        #Verifies if a variable (identifier) is declared in the current or any outer scope.
        # If not declared, an error is raised.
        if not isinstance(identifier, Node) or identifier.type != "IDENTIFIER":
            return

        found = False
        # Iterate from the most recent scope to the outermost
        for scope in reversed(self.scope_stack):
            if identifier.value in scope:
                found = True
                break

        if not found:
            self.error(f"Variable {identifier.value} has not been declared in the current or any enclosing scopes")

    def checkTypeMatch(self, var_tpe, exp_):
        #Checks if the variable type matches the expected type from an expression or value assignment.
        word_ = "None"
        if (exp_ == "NUMBER" or exp_ == "int"):
            word_ = "int"
            if (var_tpe == "int"):
                return "NUMBER"# If types match, return 'NUMBER' for integers.
        elif (exp_ == "FNUMBER" or exp_ == "float"):
            word_ = "float"
            if (var_tpe == "float"):
                return "FNUMBER"## If types match, return 'FNUMBER' for floating-point numbers.

        self.error(f'Type Mismatch between {var_tpe} and {word_}')## If no type match is found, log an error.
        return "None" 
    # return its type or None if not found

    def getMyType(self, identifier):# Retrieves the type of the identifier from the available scopes.
        index = len(self.scope_stack) - 1
        while index >= 0:
            scope = self.scope_stack[index]
            if identifier.value in scope:
                return scope[identifier.value]
            index -= 1

        # If identifier is not found in any scope, check its basic type
        if identifier.type == "NUMBER":
            return 'int'
        elif identifier.type == "FNUMBER":
            return 'float'

        return "psgpg"

    def parse_program(self):
        statements = [] # Initialize an empty list to collect all top-level statements.
        while self.current_token.type != 'EOF':# Continue parsing until the end of file token is reached.
            if self.current_token.type in ['TYPE', 'IDENTIFIER', 'IF', 'WHILE', '{']:
                statement = self.parse_statement()
                if statement:
                    statements.append(statement)
            else:
                self.eat(self.current_token.type)# Consume tokens that are not directly handled.
        return ProgramNode(statements)

    def parse_statement(self):
        # Depending on the current token type, dispatch the parsing to the respective function.
        if self.current_token.type == 'TYPE':
            return self.parse_declaration()
        elif self.current_token.type == 'IDENTIFIER':
            return self.parse_assignment()
        elif self.current_token.type == 'IF':
            return self.parse_if_statement()
        elif self.current_token.type == 'WHILE':
            return self.parse_while_loop()
        elif self.current_token.type == 'SCOPE':
            return self.parse_block()
        else:
            self.eat(self.current_token.type)
    
    def parse_block(self):
        
        if self.current_token.type != 'SCOPE' or self.current_token.value != '{':
            self.error("Expected '{' at the beginning of a block")
            return None

        # Consume the opening brace
        self.eat('SCOPE', '{')# Consume the opening brace.

        # List to hold all statements in the block
        block_statements = []

        # Process all statements until a closing brace is encountered
        while self.current_token.type != 'SCOPE' or self.current_token.value != '}':
            if self.current_token.type == 'EOF':
                self.error("Reached end of file without finding closing '}' for block")
                return None
            # Parse the next statement and append it to the block
            statement = self.parse_statement()
            if statement is not None:
                block_statements.append(statement)
            else:
                # Error recovery: skip tokens until a statement boundary (e.g., semicolon or a brace) is found
                self.advance_to_next_statement()

        # Consume the closing brace
        self.eat('SCOPE', '}')


    def parse_declaration(self): 

        currnt_data = self.currn_tokn_val() # Get the type (e.g., 'int', 'float').
        curr_value = self.next_tokn_val()# Get the variable name.
        self.next_tokn_val()#Consume the identifier token.
        self.next_tokn_val()
        self.checkVarDeclared(curr_value)
        
        curr_expr_ = self.parse_arithmetic_expression()# Parse the initialization expression.
        self.indenti_add(curr_value, currnt_data)

        curr_ = self.getMyType(curr_expr_) if curr_expr_.type == 'IDENTIFIER' else curr_expr_.type
        self.checkTypeMatch(currnt_data, curr_)

        return DeclarationNode(curr_value, curr_expr_, currnt_data)

    def parse_assignment(self):

        current_token = self.currn_tokn()
        self.next_tokn()
        self.next_tokn()
        curr_expr_ = self.parse_arithmetic_expression()# Parse the expression to the right of the '=' operator.
        # Get the type of the current expression; check if it's an identifier for a more complex type resolution.
        curr_token_data = self.getMyType(curr_expr_) if curr_expr_.type == 'IDENTIFIER' else curr_expr_.type
        self.checkTypeMatch(self.getMyType(current_token), curr_token_data)
        return AssignmentNode(current_token.value, curr_expr_)

    def parse_if_statement(self):
        condition = self.parse_condition()# Parse the conditional expression following 'if'.
        self.next_tokn()
        self.enter_scope('None')# Enter a new scope for the 'if' block.
        ifblck = self.parse_block_statements()# Parse all statements inside the 'if' block.
        # Check for an 'else' clause and process it similarly.
        if (self.currn_tokn_val() == "else"):
            self.leave_scope()
            self.enter_scope('None')

            self.next_tokn()
            else_statment = self.parse_block_statements()
            return IfStatementNode(condition, ifblck, else_statment)
        self.leave_scope()

        return IfStatementNode(condition, ifblck, None)

    def parse_block_statements(self):
        stmt = []
        for _ in iter(int, 1):  # Using `iter` with constant true condition to mimic 'while True'
            if self.current_token.type in ['EOF', 'SCOPE']:
                break
            stmt.append(self.parse_statement())
            if self.current_token.type == 'DELIMITER':
                self.eat('DELIMITER')

        self.next_tokn()  # Move past the block-closing token (like '}')
        return stmt

    def parse_while_loop(self):
        cond = self.parse_condition()# Parse the condition part of the 'while' statement.
        self.next_tokn() # Consume the token following the condition, typically '{'.
        self.enter_scope(None)# Enter a new scope for the loop body.
        while_loop_b = self.parse_block_statements()# Parse all statements in the loop body.
        self.leave_scope() # Exit the scope after the loop ends.
        return WhileLoopNode(cond, while_loop_b)

    # # No need to check type mismatch here.
    def parse_condition(self):
        self.next_tokn_val() # Optionally consume a token; adjust based on actual use case.
        left = self.parse_arithmetic_expression()  # Parse the left side of the condition
        if left is None:
            self.error("Failed to parse the left side of the condition.")
            return None
        operator = self.currn_tokn_val()
    
        self.next_tokn_val()# Move to the right-hand side expression.
        
        right = self.parse_arithmetic_expression()
        if right is None:
            self.error("Failed to parse the right side of the condition.")
            return None

        return ConditionNode(left, operator, right)

    def parse_arithmetic_expression(self):
        term = self.parse_term()
        return self.expr_elp(term)

    def expr_elp(self, term):
        self.checkVarUse(term) #This should ideally loop to handle multiple operations in an expression.
        opr = self.currn_tokn_val()
        if (str(opr) in "+-"):
            self.next_tokn()
            nxt_ = self.parse_term()
            curr_data = self.getMyType(nxt_) if nxt_.type == 'IDENTIFIER' else nxt_.type

            return TermNode(opr, term, self.expr_elp(nxt_), self.checkTypeMatch(self.getMyType(term), curr_data))
        return term

    def parse_term(self):
        fact = self.parse_factor()
        opr = self.next_tokn_val()
        if str(opr) in "*/":# Handle terms connected by '*' or '/'.
            self.next_tokn()
            nex_ = self.parse_term()
            curr_data_type = self.getMyType(
                nex_) if nex_.type == 'IDENTIFIER' else nex_.type
            return TermNode(opr, fact, nex_, self.checkTypeMatch(self.getMyType(fact), curr_data_type))
        return fact

    def parse_factor(self):
        # This method handles the parsing of the smallest elements in expressions, which include numbers- integers and floats and identifiers variable names
        next_tokn_val = self.currn_tokn()

        if (next_tokn_val.type == "NUMBER"):# Check the type of the token to determine how to construct the factor node.
            return FactorNode(next_tokn_val.value, 'NUMBER')# If the token is a NUMBER, consume the token and create a factor node for an integer literal.
        elif (next_tokn_val.type == "FNUMBER"):# If the token is a FNUMBER, consume the token and create a factor node for a floating-point literal.
            return FactorNode(next_tokn_val.value, 'FNUMBER')
        elif (next_tokn_val.type == "IDENTIFIER"):# If the token is an IDENTIFIER, it is a variable name. Consume the token and create a factor node for it.
            return FactorNode(next_tokn_val.value, 'IDENTIFIER')

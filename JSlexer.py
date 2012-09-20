# JavaScript
# 
#
# We will assume that JavaScript is case sensitive and that keywords like
# 'if' and 'true' must be written in lowercase. There are 26 possible
# tokens that you must handle. The 'tokens' variable below has been 
# initialized below, listing each token's formal name (i.e., the value of
# token.type). In addition, each token has its associated textual string
# listed in a comment. For example, your lexer must convert && to a token
# with token.type 'ANDAND' (unless the && is found inside a comment). 
# 
#
# For this assignment, a JavaScript IDENTIFIER must start with an upper- or
# lower-case character. It can then contain any number of upper- or
# lower-case characters or underscores. Its token.value is the textual
# string of the identifier. 
#       Yes:    my_age
#       Yes:    cRaZy
#       No:     _starts_with_underscore
#
# For this assignment, a JavaScript NUMBER is one or more digits. A NUMBER
# can start with an optional negative sign. A NUMBER can contain a decimal
# point, which can then be followed by zero or more additional digits. Do
# not worry about hexadecimal (only base 10 is allowed in this problem).
# The token.value of a NUMBER is its floating point value (NOT a string).
#       Yes:    123
#       Yes:    -456
#       Yes:    78.9
#       Yes:    10.
#       No:     +5
#       No:     1.2.3
#
# For this assignment, a JavaScript STRING is zero or more characters
# contained in double quotes. A STRING may contain escaped characters.
# Notably, \" does not end a string. The token.value of a STRING is
# its contents (not including the outer double quotes). 
#       Yes:    "hello world"
#       Yes:    "this has \"escaped quotes\""
#       No:     "no"t one string" 
#

import ply.lex as lex

tokens = (
	'ANDAND',       # &&
        'COMMA',        # ,
        'DIVIDE',       # /
        'ELSE',         # else
        'EQUAL',        # =
        'EQUALEQUAL',   # ==
        'FALSE',        # false
        'FUNCTION',     # function
        'GE',           # >=
        'GT',           # >
        'IDENTIFIER',   #### Not used in this problem.
        'IF',           # if
        'LBRACE',       # {
        'LE',           # <=
        'LPAREN',       # (
        'LT',           # <
        'MINUS',        # -
        'NOT',          # !
        'NUMBER',       #### Not used in this problem.
        'OROR',         # ||
        'PLUS',         # +
        'RBRACE',       # }
        'RETURN',       # return
        'RPAREN',       # )
        'SEMICOLON',    # ;
        'STRING',       #### Not used in this problem. 
        'TIMES',        # *
        'TRUE',         # true
        'VAR',          # var
)

states = (
          ('COMMENT','exclusive'),
)

#
# Write your code here. 
#

def t_IDENTIFIER(token):
  r'[a-zA-Z]+[\_?[a-zA-Z]*]*'
  token.type = 'IDENTIFIER'
  return token

def t_STRING(token):
  r'[\"|\'].*[\"|\']'
  token.type = 'STRING'
  token.value = token.value[1:-1]
  return token

def t_NUMBER(token):
  r'\-?[0-9]+\.?[0-9]*'
  token.type = 'NUMBER'
  token.value = float(token.value)
  return token

def t_EOLCOMMENT(token):
  r'\/\/.*'
  print 'Single line comment'
  pass

def t_COMMENT(token):
  r'\/\*'
  print 'Multiline comment'
  token.lexer.begin('COMMENT')
    
def t_COMMENT_END(token):
  r'\*\/'
  print 'Multiline comment ends'
  token.lexer.lineno+=token.value.count('\n')
  token.lexer.begin('INITIAL')
  pass

def t_COMMENT_error(t):
  t.lexer.skip(1)
    
def t_ANDAND(token):
  r'&&'
  token.type = 'ANDAND'
  return token

def t_COMMA(token):
  r','
  token.type = 'COMMA'
  return token

def t_EQUALEQUAL(token):
  r'=='
  token.type = 'EQUALEQUAL'
  return token

def t_GE(token):
  r'>='
  token.type = 'GE'
  return token

def t_LE(token):
  r'<='
  token.type = 'LE'
  return token

def t_DIVID(token):
  r'/'
  token.type = 'DIVIDE'
  return token

def t_ELSE(token):
  r'else'
  token.type = 'ELSE'
  return token

def t_EQUAL(token):
  r'='
  token.type = 'EQUAL'
  return token

def t_FALSE(token):
  r'false'
  token.type = 'FALSE'
  return token

def t_GT(token):
  r'>'
  token.type = 'GT'
  return token

def t_FUNCTION(token):
  r'function'
  token.type = 'FUNCTION'
  return token

def t_IF(token):
  r'if'
  token.type = 'IF'
  return token

def t_LPAREN(token):
  r'\('
  token.type = 'LPAREN'
  return token

def t_LBRACE(token):
  r'\{'
  token.type = 'LBRACE'
  return token

def t_LT(token):
  r'<'
  token.type = 'LT'
  return token

def t_RPAREN(token):
  r'\)'
  token.type = 'RPAREN'
  return token

def t_RBRACE(token):
  r'\}'
  token.type = 'RBRACE'
  return token

def t_MINUS(token):
  r'\-'
  token.type = 'MINUS'
  return token

def t_NOT(token):
  r'!'
  token.type = 'NOT'
  return token

def t_OROR(token):
  r'\|\|'
  token.type = 'OROR'
  return token

def t_PLUS(token):
  r'\+'
  token.type = 'PLUS'
  return token

def t_RETURN(token):
  r'return'
  token.type = 'RETURN'
  return token

def t_SEMICOLON(token):
  r'\;'
  token.type = 'SEMICOLON'
  return token

def t_times(token):
  r'\*'
  token.type = 'TIMES'
  return token

def t_true(token):
  r'true'
  token.type = 'TRUE'
  return token

def t_VARIABLE(token):
  r'var'
  token.type = 'VAR'
  return token

t_ignore                = ' \t\v\r' # whitespace 

def t_newline(t):
        r'\n'
        t.lexer.lineno += 1

def t_error(t):
        print "JavaScript Lexer: Illegal character " + t.value[0]
        t.lexer.skip(1)

# Part which returns the processed tokens 

lexer = lex.lex() 

def test_lexer(input_string):
  lexer.input(input_string)
  result = [ ] 
  while True:
    tok = lexer.token()
    if not tok: break
    result = result + [tok.type,tok.value]
  return result

#A few test cases - from www.udacity.com have been included
#You might probably want to include some of your own

input1 = 'some_identifier -12.34 "a \\"escape\\" b"'
output1 = ['IDENTIFIER', 'some_identifier', 'NUMBER', -12.34, 'STRING', 
'a \\"escape\\" b']
print test_lexer(input1)
print test_lexer(input1) == output1


input2 = '-12x34' 
output2 = ['NUMBER', -12.0, 'IDENTIFIER', 'x', 'NUMBER', 34.0]
print test_lexer(input2)
print test_lexer(input2) == output2

#incomplete test cases. Finish asap

input3 = """ - !  && () * , / ; { || } + < <= = == > >= else false function
if return true var """

output3 = ['MINUS', 'NOT', 'ANDAND', 'LPAREN', 'RPAREN', 'TIMES', 'COMMA',
'DIVIDE', 'SEMICOLON', 'LBRACE', 'OROR', 'RBRACE', 'PLUS', 'LT', 'LE',
'EQUAL', 'EQUALEQUAL', 'GT', 'GE', 'ELSE', 'FALSE', 'FUNCTION', 'IF',
'RETURN', 'TRUE', 'VAR']

print test_lexer(input3)

input4 = """
if // else mystery  
=/*=*/= 
true /* false 
*/ return"""

output4 = ['IF', 'EQUAL', 'EQUAL', 'TRUE', 'RETURN']

print test_lexer(input4)

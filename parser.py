import ply.lex as lex
import ply.yacc as yacc

def print_tree(node, indent=0):
    if isinstance(node, tuple):
        print('  ' * indent + node[0] + ' :')
        for child in node[1:]:
            print_tree(child, indent + 1)
    else:
        print('  ' * (indent + 1) + str(node))

tokens = [
    'IDENTIFIER',
    'INTCON',
    'FLOATCON',
    'STRING',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'ASSIGN',
    'EQ',
    'NE',
    'LT',
    'LE',
    'GT',
    'GE',
    'AND',
    'OR',
    'NOT',
    'LP',
    'RP',
    'LBR',
    'RBR',
    'LBK',
    'RBK',
    'CM',
    'SC',
    'IF',
    'ELSE',
    'WHILE',
    'RETURN',
    'READ',
    'WRITE',
    'EXIT',
    'INT',
    'FLOAT',
]

reserved = {
    'else': 'ELSE',
    'exit': 'EXIT',
    'float': 'FLOAT',
    'if': 'IF',
    'int': 'INT',
    'read': 'READ',
    'return': 'RETURN',
    'while': 'WHILE',
    'write': 'WRITE',
}

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_ASSIGN = r'='
t_EQ = r'=='
t_NE = r'!='
t_LT = r'<'
t_LE = r'<='
t_GT = r'>'
t_GE = r'>='
t_AND = r'&&'
t_OR = r'\|\|'
t_NOT = r'!'
t_LP = r'\('
t_RP = r'\)'
t_LBR = r'{'
t_RBR = r'}'
t_LBK = r'\['
t_RBK = r'\]'
t_CM = r','
t_SC = r';'
t_ignore = r' '

def t_COMMENT(t):
    r'/\*.*?\*/'
    pass

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_IDENTIFIER(t):
    r'[a-zA-Z]([a-zA-Z]|[0-9])*'
    t.type = reserved.get(t.value, 'IDENTIFIER')
    return t

def t_INTCON(t):
    r'0|[1-9][0-9]*'
    t.value = int(t.value)
    return t

def t_FLOATCON(t):
    r'[0-9]+\.[0-9]+'
    t.value = float(t.value)
    return t

def t_STRING(t):
    r'\'[a-zA-Z0-9\']+\''
    return t

def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)

lexer = lex.lex()

def p_program(p):
    '''Program : DeclList Procedures
               | Procedures'''
    if len(p) == 3:
        p[0] = ("Program", p[1], p[2])
    else:
        p[0] = ("Program", p[1])

def p_Procedures(p):
    '''Procedures : ProcedureDecl Procedures
                  | ProcedureDecl'''
    if len(p) == 3:
        p[0] = ("Procedures", p[1], p[2])
    else:
        p[0] = ("Procedures", p[1])

def p_ProcedureDecl(p):
    '''ProcedureDecl : ProcedureHead ProcedureBody'''
    p[0] = ("ProcedureDecl", p[1], p[2])

def p_ProcedureHead(p):
    '''ProcedureHead : FunctionDecl DeclList
                     | FunctionDecl'''
    if len(p) == 3:
        p[0] = ("ProcedureHead", p[1], p[2])
    else:
        p[0] = ("ProcedureHead", p[1])

def p_FunctionDecl(p):
    '''FunctionDecl : Type IDENTIFIER LP RP LBR'''
    p[0] = ("FunctionDecl", p[1], p[2])

def p_ProcedureBody(p):
    '''ProcedureBody : StatementList RBR'''
    p[0] = ("ProcedureBody", p[1])

def p_DeclList(p):
    '''DeclList : Type IdentifierList SC
                | DeclList Type IdentifierList SC'''
    if len(p) == 4:
        p[0] = ("DeclList", p[1], p[2])
    else:
        p[0] = ("DeclList", p[1], p[2], p[3])

def p_IdentifierList(p):
    '''IdentifierList : VarDecl
                     | IdentifierList CM VarDecl '''
    if len(p) == 2:
        p[0] = ("IdentifierList", p[1])
    else:
        p[0] = ("IdentifierList", p[1], p[3])

def p_VarDecl(p):
    '''VarDecl : IDENTIFIER
               | IDENTIFIER LBK INTCON RBK'''
    if len(p) == 2:
        p[0] = ("VarDecl", p[1])
    else:
        p[0] = ("VarDecl", p[1], p[3])

def p_Type(p):
    '''Type : INT
            | FLOAT'''
    p[0] = ("Type", p[1])

def p_StatementList(p):
    '''StatementList : Statement
                    | StatementList Statement'''
    if len(p) == 2:
        p[0] = ("StatementList", p[1])
    else:
        p[0] = ("StatementList", p[1], p[2])

def p_Statement(p):
    '''Statement : Assignment
                 | IfStatement
                 | WhileStatement
                 | IOStatement
                 | ReturnStatement
                 | ExitStatement
                 | CompoundStatement'''
    p[0] = ("Statement", p[1])

def p_Assignment(p):
    '''Assignment : Variable ASSIGN Expr SC'''
    p[0] = ("Assignment", p[1], p[3])

def p_IfStatement(p):
    '''IfStatement : IF Test CompoundStatement
                   | IF Test CompoundStatement ELSE CompoundStatement'''
    if len(p) == 4:
        p[0] = ("IfStatement", p[1], p[2], p[3])
    else:
        p[0] = ("IfStatement", p[1], p[2], p[3], p[5])

def p_Test(p):
    '''Test : LP Expr RP'''
    p[0] = ("Test", p[2])

def p_WhileStatement(p):
    '''WhileStatement : while_token WhileExpr Statement'''
    p[0] = ("WhileStatement", p[2], p[3])

def p_while_token(p):
    '''while_token : WHILE'''
    p[0] = ("while_token", p[1])

def p_WhileExpr(p):
    '''WhileExpr : LP Expr RP'''
    p[0] = ("WhileExpr", p[2])

def p_IOStatement(p):
    '''IOStatement : READ LP Variable RP SC
                   | WRITE LP Expr RP SC
                   | WRITE LP StringConstant RP SC'''
    if p[1] == 'read':
        p[0] = ("IOStatement", "READ", p[3])
    elif p[1] == 'write' and isinstance(p[3], tuple):
        p[0] = ("IOStatement", "WRITE", p[3])
    else:
        p[0] = ("IOStatement", "WRITE", p[3])

def p_ReturnStatement(p):
    '''ReturnStatement : RETURN Expr SC'''
    p[0] = ("ReturnStatement", p[2])

def p_ExitStatement(p):
    '''ExitStatement : EXIT SC'''
    p[0] = ("ExitStatement",)

def p_CompoundStatement(p):
    '''CompoundStatement : LBR StatementList RBR'''
    p[0] = ("CompoundStatement", p[2])

def p_Expr(p):
    '''Expr : Expr AND SimpleExpr
            | Expr OR SimpleExpr
            | SimpleExpr
            | NOT SimpleExpr'''
    if len(p) == 2:
        p[0] = ("Expr", p[1])
    elif p[1] == 'not':
        p[0] = ("Expr", "NOT", p[2])
    else:
        p[0] = ("Expr", p[1], p[3])

def p_SimpleExpr(p):
    '''SimpleExpr : SimpleExpr EQ AddExpr
                  | SimpleExpr NE AddExpr
                  | SimpleExpr LE AddExpr
                  | SimpleExpr LT AddExpr
                  | SimpleExpr GE AddExpr
                  | SimpleExpr GT AddExpr
                  | AddExpr'''
    if len(p) == 2:
        p[0] = ("SimpleExpr", p[1])
    else:
        p[0] = ("SimpleExpr", p[2], p[1], p[3])

def p_AddExpr(p):
    '''AddExpr : AddExpr PLUS MulExpr
               | AddExpr MINUS MulExpr
               | MulExpr'''
    if len(p) == 2:
        p[0] = ("AddExpr", p[1])
    else:
        p[0] = ("AddExpr", p[2], p[1], p[3])

def p_MulExpr(p):
    '''MulExpr : MulExpr TIMES Factor
               | MulExpr DIVIDE Factor
               | Factor'''
    if len(p) == 2:
        p[0] = ("MulExpr", p[1])
    else:
        p[0] = ("MulExpr", p[2], p[1], p[3])

def p_Factor(p):
    '''Factor : Variable
              | Constant
              | IDENTIFIER LP RP
              | LP Expr RP'''
    if len(p) == 2:
        p[0] = ("Factor", p[1])
    elif len(p) == 4:
        p[0] = ("Factor", p[1], p[2], p[3])
    else:
        p[0] = ("Factor", p[1], p[2])

def p_Variable(p):
    '''Variable : IDENTIFIER
                | IDENTIFIER LBK Expr RBK'''
    if len(p) == 2:
        p[0] = ("Variable", p[1])
    else:
        p[0] = ("Variable", p[1], p[3])

def p_StringConstant(p):
    '''StringConstant : STRING'''
    p[0] = ("StringConstant", p[1])

def p_Constant(p):
    '''Constant : INTCON
                | FLOATCON'''
    p[0] = ("Constant", p[1])

def p_error(p):
    print(f"Syntax error at line {p.lineno}")

parser = yacc.yacc()
file_path = "Downloads//hw4test.txt"

with open(file_path, 'r', encoding='utf-8') as file:
    data = file.read()

result = parser.parse(data, lexer=lexer)

if result:
    print_tree(result)
else:
    print("Error during parsing")

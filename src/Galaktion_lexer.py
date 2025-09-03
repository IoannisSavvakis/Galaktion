import ply.lex as lex
from Galaktion_core import SymbolType


states = (
    ('recording', 'exclusive'),
    ('xmlTag', 'exclusive'),
    ('linkCode', 'exclusive'),
)


reservedTokens = {
    'folder': str(SymbolType.folder.name),
    'xmlfile': str(SymbolType.xmlfile.name),
    'jsonfile': str(SymbolType.jsonfile.name),
    'csvfile': str(SymbolType.csvfile.name),
    'flexible': str(SymbolType.flexible.name),

    'none': 'NONE',
    'true': 'TRUE',
    'false': 'FALSE',

    'generate': 'GENERATE',
    'extract': 'EXTRACT',
    'print': 'PRINT',

    'from': 'FROM',
    # 'where': 'WHERE',
    'as': 'AS',
    'in': 'IN',
    'if': 'IF',
    'else': 'ELSE',
    'foreach': 'FOR_EACH',
    'break': 'BREAK',
    'continue': 'CONTINUE',

    'and': 'AND',
    'or': 'OR',
    'not': 'NOT'
}

tokens = [
    'BLOCK_WITH_CODE',
    'CURLY_BRACE_L',
    'ID',
    'XML_TAG',
    'XML_ATTRIBUTE',
    'ARROW',
    'DIRECTIVE',

    'STRING_VALUE',
    'INTEGER',
    'REAL',

    'EQUAL_EQUAL',
    'GREATER_LESS',
    'LESS',
    'LESS_EQUAL',
    'GREATER',
    'GREATER_EQUAL',
    'COLON',
    'COLON_COLON',
    'BRACE_L',
    'BRACE_R',
    'PAR_L',
    'PAR_R',
    'MINUS',
    'PLUS',
    'PLUS_PLUS',
    'MINUS_EQUAL',
    'PLUS_EQUAL',
    'PLUS_PLUS_EQUAL',
    'MINUS_COLON',
    'PLUS_COLON',
    'PLUS_PLUS_COLON',
    'SLASH',
    'EQUAL',
    'COMMA',
    'VERTICAL_BAR'
] + list(reservedTokens.values())


t_DIRECTIVE = r'@[a-zA-Z_0-9]+'
t_PLUS_PLUS_EQUAL = r'\+\+='
t_PLUS_PLUS_COLON = r'\+\+:'
t_PLUS_PLUS = r'\+\+'
t_MINUS_EQUAL = r'\-='
t_MINUS_COLON = r'\-:'
t_PLUS_EQUAL = r'\+='
t_PLUS_COLON = r'\+:'
t_ARROW = r'->'
t_EQUAL_EQUAL = r'=='
t_GREATER_LESS = r'><'
t_LESS_EQUAL = r'<='
t_GREATER_EQUAL = r'>='
t_COLON_COLON = r'::'
t_INITIAL_recording_xmlTag_linkCode_ignore = ' \t'
t_LESS = r'<'
t_GREATER = r'>'
t_COLON = r':'
t_BRACE_L = r'\['
t_BRACE_R = r'\]'
t_INITIAL_linkCode_PAR_L = r'\('
t_INITIAL_linkCode_PAR_R = r'\)'
t_MINUS = r'\-'
t_INITIAL_linkCode_PLUS = r'\+'
t_EQUAL = r'='
t_COMMA = r','


# literals = '[]()-+/=,'


def t_CURLY_BRACE_L(t):
    r'\{'
    t.lexer.code_start = t.lexer.lexpos-1
    t.lexer.begin('recording')
    t.lexer.open_blocks = 1


def t_recording_error(t):
    t.lexer.skip(1)


def t_recording_CURLY_BRACE_L(t):
    r'\{'
    t.lexer.open_blocks += 1


def t_recording_CURLY_BRACE_R(t):
    r'\}'
    t.lexer.open_blocks -= 1

    if t.lexer.open_blocks == 0:
        t.value = t.lexer.lexdata[t.lexer.code_start: t.lexer.lexpos]
        t.type = 'BLOCK_WITH_CODE'
        # t.lexer.lineno += t.value.count('\n')
        t.lexer.begin('INITIAL')
        return t


def t_INITIAL_xmlTag_linkCode_COMMENT_SINGLE_LINE(t):
    r'//.*'
    pass


def t_INITIAL_xmlTag_linkCode_COMMENT_MULTI_LINE(t):
    r'/\*(.|\n)*\*/'
    t.lexer.lineno += t.value.count('\n')
    pass


def t_SLASH(t):
    r'/'
    t.lexer.begin('xmlTag')
    return t


def t_xmlTag_XML_TAG(t):
    # (->)|\[|\s)'
    # r'([a-zA-Z_][a-zA-Z_0-9\.\-]*:)?[a-zA-Z_][a-zA-Z_0-9\.\-]*?(?=(->)|$|[\[\/,]|\s|^[a-zA-Z_0-9\.\-])'
    r'([a-zA-Z_][a-zA-Z_0-9\.\-]*:)?[a-zA-Z_][a-zA-Z_0-9\.\-]*?(?=(->)|$|[^a-zA-Z_0-9\.\-])'
    t.lexer.begin('INITIAL')
    # t.value = t.value[1:]
    return t


def t_INITIAL_linkCode_VERTICAL_BAR(t):
    r'\|'

    state = t.lexer.current_state()

    if state == 'INITIAL':
        t.lexer.begin('linkCode')
    elif state == 'linkCode':
        t.lexer.begin('INITIAL')
    else:
        assert (False)

    return t


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reservedTokens.get(t.value, 'ID')
    return t


def t_linkCode_XML_ATTRIBUTE(t):
    r'([a-zA-Z_][a-zA-Z_0-9\.\-]*:)?[a-zA-Z_][a-zA-Z_0-9\.\-]*?(?=(->)|$|[^a-zA-Z_0-9\.\-])'
    return t


def processString(s: str):
    '''Replaces escape sequences in s with the actual escape characters'''

    ret = s

    l = ret.split('\\n')
    ret = ''
    for i in l:
        ret += i + '\n'
    ret = ret[:-1]

    l = ret.split('\\t')
    ret = ''
    for i in l:
        ret += i + '\t'
    ret = ret[:-1]

    return ret


def t_INITIAL_linkCode_STRING_VALUE(t):
    r"'([^\\\n]|(\\.))*?'"
    # r"'[^('|\n)]*'" # old
    t.value = processString(t.value[1:-1])
    return t


def t_REAL(t):
    r'\d*\.\d+'
    t.value = float(t.value)
    return t


def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_INITIAL_recording_xmlTag_linkCode_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_INITIAL_xmlTag_linkCode_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


####################  T E S T   L E X E R  ####################


def newLexer():
    return lex.lex()


with open('D:/CSD-Codes/Thesis/Python/langTest2.txt', 'r') as f:
    data = f.read()


def runTest(data):
    testLexer = newLexer()

    # Give the lexer some input
    testLexer.input(data)

    # Tokenize
    while True:
        tok = testLexer.token()
        if not tok:
            exit()      # No more input
        print(tok, tok.lineno)


data = "print 'Hello world!'"

# runTest(str(data))


###############################################################


if __name__ == '__main__':
    lex.runmain()

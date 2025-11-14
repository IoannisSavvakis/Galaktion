# Galaktion_lexer.py

import re
import ply.lex as lex
from Galaktion_core import SymbolType, Directives
from lxml import etree


states = (
    ('recordingCodeBlock', 'exclusive'),
    ('recordingParBlock', 'exclusive'),
    ('recordingBraceBlock', 'exclusive'),
    ('scanBraceBlock', 'exclusive'),
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
    'as': 'AS',
    'in': 'IN',
    'if': 'IF',
    'else': 'ELSE',
    'foreach': 'FOR_EACH',
    'break': 'BREAK',
    'continue': 'CONTINUE',

    'and': 'AND',
    'or': 'OR',
    'not': 'NOT',

    '*': 'MUL',
    'div': 'DIV',
    'mod': 'MOD',

    'exit': 'EXIT'
}

tokens = [
    'BLOCK_WITH_CODE',
    'CURLY_BRACE_L',

    'ID',

    'ARROW',

    'DIRECTIVE',

    'STRING_LITERAL',
    'INTEGER',
    'REAL',

    'EQUAL',
    'NOT_EQUAL',
    'LESS',
    'LESS_EQUAL',
    'GREATER',
    'GREATER_EQUAL',

    'SLICER',

    'BRACE_L',
    'BRACE_R',
    'PAR_L',
    'PAR_R',

    'MINUS',
    'PLUS',
    'EXTEND',

    'MINUS_ASSIGN_VAR',
    'PLUS_ASSIGN_VAR',
    'MUL_ASSIGN_VAR',
    'DIV_ASSIGN_VAR',
    'MOD_ASSIGN_VAR',
    'EXTEND_ASSIGN_VAR',

    'MINUS_ASSIGN_CONST',
    'PLUS_ASSIGN_CONST',
    'MUL_ASSIGN_CONST',
    'DIV_ASSIGN_CONST',
    'MOD_ASSIGN_CONST',
    'EXTEND_ASSIGN_CONST',

    'ASSIGN_VAR',
    'ASSIGN_CONST',

    'COMMA',

    'PATH',
    'PAR_PATH',
    'PREDICATE',

    'DUMMY_STMT',
] + list(reservedTokens.values())


t_INITIAL_scanBraceBlock_ID = '(\\$|\0)[a-zA-Z_0-9]+'
t_INITIAL_scanBraceBlock_DIRECTIVE = fr'\{Directives.prefix}' + \
    r'[a-zA-Z_0-9]+' + Directives.postfix
t_INITIAL_scanBraceBlock_DIV_ASSIGN_VAR = r':div'
t_INITIAL_scanBraceBlock_MOD_ASSIGN_VAR = r':mod'
t_INITIAL_scanBraceBlock_EXTEND_ASSIGN_VAR = r':\+\+'
t_INITIAL_scanBraceBlock_EXTEND_ASSIGN_CONST = r'\.\+\+'
t_INITIAL_scanBraceBlock_EXTEND = r'\+\+'
t_INITIAL_scanBraceBlock_ASSIGN_VAR = r':='
t_INITIAL_scanBraceBlock_MINUS_ASSIGN_VAR = r':\-'
t_INITIAL_scanBraceBlock_PLUS_ASSIGN_VAR = r':\+'
t_INITIAL_scanBraceBlock_MUL_ASSIGN_VAR = r':\*'
t_INITIAL_scanBraceBlock_ARROW = r'->'
t_INITIAL_scanBraceBlock_NOT_EQUAL = r'!='
t_INITIAL_scanBraceBlock_LESS_EQUAL = r'<='
t_INITIAL_scanBraceBlock_GREATER_EQUAL = r'>='
t_INITIAL_scanBraceBlock_recordingCodeBlock_recordingBraceBlock_recordingParBlock_ignore = ' \t'
t_INITIAL_scanBraceBlock_LESS = r'<'
t_INITIAL_scanBraceBlock_GREATER = r'>'
t_INITIAL_scanBraceBlock_SLICER = r'~'
t_INITIAL_scanBraceBlock_MINUS = r'\-'
t_INITIAL_scanBraceBlock_PLUS = r'\+'
t_INITIAL_scanBraceBlock_EQUAL = r'='
t_INITIAL_scanBraceBlock_COMMA = r','
t_INITIAL_DUMMY_STMT = r';'


def t_INITIAL_linkCode_recordingCodeBlock_COMMENT_MULTI_LINE(t):
    r'\`(.|\n)*?\`'
    t_INITIAL_scanBraceBlock_recordingCodeBlock_recordingBraceBlock_newline(t)
    pass


def t_CURLY_BRACE_L(t):
    r'\{'
    t.lexer.codeBlock_start = t.lexer.lexpos-1
    t.lexer.begin('recordingCodeBlock')
    t.lexer.open_blocks = 1


def t_recordingCodeBlock_error(t):
    t.lexer.skip(1)


def t_recordingCodeBlock_CURLY_BRACE_L(t):
    r'\{'
    t.lexer.open_blocks += 1


def t_recordingCodeBlock_CURLY_BRACE_R(t):
    r'\}'
    t.lexer.open_blocks -= 1

    if t.lexer.open_blocks == 0:
        t.value = t.lexer.lexdata[t.lexer.codeBlock_start: t.lexer.lexpos]
        t.type = 'BLOCK_WITH_CODE'
        t.lexer.begin('INITIAL')
        return t


def processString(s: str):
    '''Replaces escape sequences in s with the corresponding escape characters'''

    escChars = '\n\t\r\'\"\\'

    ret = s

    for ec in escChars:
        escSeq = '\\' + ('%r' % ec)[-2]

        l = ret.split(escSeq)

        ret = ''
        for i in l:
            ret += i + ec
        ret = ret[:-1]

    return ret


def t_INITIAL_scanBraceBlock_STRING_LITERAL(t):
    r'("([^\\\n]|(\\.))*?")|(\'([^\\\n]|(\\.))*?\')'
    t.value = processString(t.value[1:-1])
    return t


def t_INITIAL_scanBraceBlock_REAL(t):
    r'\d*\.\d+'
    t.value = float(t.value)
    return t


def t_INITIAL_scanBraceBlock_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_INITIAL_scanBraceBlock_ASSIGN_CONST(t):
    r'\.='
    return t


def t_INITIAL_scanBraceBlock_MINUS_ASSIGN_CONST(t):
    r'\.\-'
    return t


def t_INITIAL_scanBraceBlock_PLUS_ASSIGN_CONST(t):
    r'\.\+'
    return t


def t_INITIAL_scanBraceBlock_MUL_ASSIGN_CONST(t):
    r'\.\*'
    return t


def t_INITIAL_scanBraceBlock_DIV_ASSIGN_CONST(t):
    r'\.div'
    return t


def t_INITIAL_scanBraceBlock_MOD_ASSIGN_CONST(t):
    r'\.mod'
    return t


def t_INITIAL_BRACE_L(t):
    r'\['
    t.lexer.lexpos -= 1
    t.lexer.braceBlock_start = t.lexer.lexpos
    t.lexer.begin('recordingBraceBlock')
    t.lexer.openBraceBlocks = 0


def t_recordingBraceBlock_recordingParBlock_error(t):
    t.lexer.skip(1)


def t_recordingBraceBlock_BRACE_L(t):
    r'\['
    t.lexer.openBraceBlocks += 1


def t_recordingBraceBlock_BRACE_R(t):
    r'\]'
    t.lexer.openBraceBlocks -= 1

    if t.lexer.openBraceBlocks == 0:
        t.value = t.lexer.lexdata[t.lexer.braceBlock_start: t.lexer.lexpos]
        if t.lexer.last_token.type in {'PATH', 'PREDICATE'}:
            try:
                # In case brace block containes only a negative number (e.g.: [-1])
                float(t.value[1:-1])
            except ValueError:
                try:
                    # Check the validity of the brace block as predicate.
                    # If it's not valid, means it contains Galaktion code
                    # and needs to be tokenized.
                    # An exception will be caught if so.
                    varNames = re.findall(
                        t_INITIAL_scanBraceBlock_ID, '/*'+t.value)
                    etree.fromstring('<root/>').xpath('/*'+t.value, **
                                                      {name[1:]: 'dummy value' for name in varNames})

                    # Since we are here it means that
                    # it is a valid XPath 1.0 predicate.
                    t.type = 'PREDICATE'
                    t.lexer.begin('INITIAL')
                    return t
                except (etree.XPathSyntaxError, etree.XPathEvalError):
                    pass
                except Exception as e:
                    print('> Lexer unhandled exception:\t\t', type(e))
                    assert (False)

        # If we are here it means that it needs tokenization.
        t.lexer.lexpos -= len(t.value)
        t.lexer.openBraceBlocks = 0     # reset it beccause it's used in scanBraceBlock state
        t.lexer.begin('scanBraceBlock')


def t_scanBraceBlock_BRACE_L(t):
    r'\['
    t.lexer.openBraceBlocks += 1
    return t


def t_scanBraceBlock_BRACE_R(t):
    r'\]'

    t.lexer.openBraceBlocks -= 1

    if t.lexer.openBraceBlocks == 0:
        t.lexer.begin('INITIAL')

    return t


lastPathLastChar = '/'


def t_INITIAL_scanBraceBlock_PATH(t):
    r'[\.\/\*a-zA-Z_@]+[^\[\s]*?(?=(->)|$|[^a-zA-Z_0-9\.\-])'

    global lastPathLastChar

    t.type = reservedTokens.get(t.value, 'PATH')

    if t.type in ['PATH', 'MUL']:
        if t.value[0] != '/' and ((t.lexer.last_token.type == 'PATH' and lastPathLastChar != '/') or t.lexer.last_token.type in ['PAR_PATH', 'PREDICATE', 'BRACE_R']):
            t.lexer.lexpos -= len(t.value)

            t.type = 'DUMMY_STMT'
            t.value = ';'
            return t

        lastPathLastChar = t.value[-1]

    return t


def t_scanBraceBlock_PAR_L(t):
    r'\('
    return t


def t_INITIAL_PAR_L(t):
    r'\('
    if t.lexer.last_token.type == 'PATH':
        t.lexer.lexpos -= 1
        t.lexer.parBlock_start = t.lexer.lexpos
        t.lexer.push_state('recordingParBlock')
        t.lexer.openParBlocks = 0
    else:
        return t


def t_recordingParBlock_PAR_L(t):
    r'\('
    t.lexer.openParBlocks += 1


def t_recordingParBlock_PAR_R(t):
    r'\)'
    t.lexer.openParBlocks -= 1

    if t.lexer.openParBlocks == 0:
        t.value = t.lexer.lexdata[t.lexer.parBlock_start:t.lexer.lexpos]
        t.type = 'PAR_PATH'
        t_INITIAL_scanBraceBlock_recordingCodeBlock_recordingBraceBlock_newline(
            t)
        t.lexer.pop_state()
        return t


def t_INITIAL_scanBraceBlock_PAR_R(t):
    r'\)'
    return t


def t_INITIAL_scanBraceBlock_recordingCodeBlock_recordingBraceBlock_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count('\n')


def t_INITIAL_scanBraceBlock_error(t):
    print(
        f"Illegal character '{t.value[0]}' at line {t.lexer.lineno}. last_token='{t.lexer.last_token}', state='{t.lexer.current_state()}'")
    t.lexer.skip(1)


def newLexer(startingLine: int):
    lxr = lex.lex()
    lxr.lineno = startingLine

    return lxr


####################  T E S T   L E X E R  ####################


def runTest(data):
    testLexer = newLexer(1)

    testLexer.input(data)

    while True:
        tok = testLexer.token()
        if not tok:
            exit()      # No more input
        print(tok, tok.lineno, type(tok))


data = """ //refs[-1~]/@sps_id """

# runTest(str(data))

###############################################################


if __name__ == '__main__':
    lex.runmain()

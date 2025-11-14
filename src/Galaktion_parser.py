# Galaktion_parser.py

import ply.yacc as yacc
from Galaktion_lexer import tokens, newLexer
import os
import sys
from pathlib import Path
from lxml import etree
import json
import csv
import pandas as pd
import re
from enum import Enum
import Galaktion_core as G
from Galaktion_core import ExpressionType as ExprT, Expression, Symbol, SymbolType as SymT, Checks, Directives


############## Variables ##############################

UI = G.UserInterface()
symbolTable = G.SymbolTable()

userSrcFiles = G.Stack()


linkCode = None
linkCodeStartingLine = 0
linkSubfolders = None
currentSrcElement = None
fileLocation = None
fileName = None


activeLoopCounter = 0


afterErrorExpression = Expression(ExprT.NONE, True)

######################################################


#################### Functions #############################

def getDynamicXPathVariables(expr: str):
    vars = re.findall(r'\$[a-zA-Z_0-9]+', str(expr))

    definedVars = {}
    undeclaredVars = []

    for var in vars:
        val = symbolTable.get(var)

        if val == None:
            undeclaredVars.append(var)
            continue

        val = val.value
        if type(val) == list:
            val = [i.value for i in val]

        definedVars[var[1:]] = val

    return (definedVars, undeclaredVars)


def checkIndex(e: Expression, line: int):
    global UI

    if e == None or e.type != ExprT.ARITHMETIC:
        UI.emitError(
            G.Error('non-Integer index', line))
        return afterErrorExpression
    else:
        idx = e.value

        if type(idx) == float:
            UI.emitWarning(
                G.Warning('Floating point index - treated as integer', line))
            idx = int(idx)

        return G.Index(idx)


# Degrees Minutes Seconds to Decimal Degrees
def DMStoDD(dmsString):
    dms = [0, 0, 0]
    extracted = re.findall(r'[-+]?\d*\.\d+|\d+', dmsString)

    for i in range(len(extracted)):
        dms[i] = float(extracted[i])

    #   Conversion:
    #   Decimal Degrees = degrees + (minutes/60) + (seconds/3600)
    return dms[0] + dms[1]/60 + dms[2]/3600


def findFile(place, fileName):
    p = Path(place)

    if not p.is_dir():
        return None

    for i in p.iterdir():
        if i.is_file() and i.name == fileName:
            return str(i.resolve())

        if i.is_dir() and linkSubfolders:
            f = findFile(i.resolve(), fileName)

            if f != None:
                return f

    return None


class PathType(Enum):
    RELATIVE = 0
    ABSOLUTE = 1


def getPath2File(root, pathType: PathType = PathType.RELATIVE):
    global fileName, fileLocation

    assert (currentSrcElement != None)
    parser.parse(linkCode[1:-1], lexer=newLexer(linkCodeStartingLine))
    assert (fileLocation != None and fileName != None)

    fileLocation.value = root + '/' + fileLocation.value

    if len(fileLocation.value) < len(root + '/'):
        return None
    assert len(fileLocation.value) >= len(root + '/')

    absPath = findFile(fileLocation.value, fileName.value)

    fileLocation = None
    fileName = None

    if absPath == None:
        return None

    if pathType == PathType.RELATIVE:
        return absPath[len(root)+1:]

    if pathType == PathType.ABSOLUTE:
        return absPath

    # error - invalid argument
    assert (False)


# For the source file having the coordinates written first lat then long
def getLatLong(val):
    if val == None:
        return None
        lat = None
        long = None
    elif '\u00b0' in val:
        latlong = re.split('N|n|S|s|\u039d', val)
        lat = DMStoDD(latlong[0])
        long = DMStoDD(latlong[1])
    else:
        latlong = re.findall(r'[-+]?\d*\.\d+|\d+', val)
        lat = float(latlong[0])
        long = float(latlong[1])

    return {
        'lat': lat,
        'long': long
    }


# For the source file having the coordinates written first long then lat
def getLongLat(val):
    if val == None:
        return None
        lat = None
        long = None
    elif '\u00b0' in val:
        longlat = re.split('N|n|S|s|\u039d', val)
        lat = DMStoDD(longlat[1])
        long = DMStoDD(longlat[0])
    else:
        longlat = re.findall(r'[-+]?\d*\.\d+|\d+', val)
        lat = float(longlat[1])
        long = float(longlat[0])

    return {
        'lat': lat,
        'long': long
    }


class BreakStatement(Exception):
    pass


class ContinueStatement(Exception):
    pass


def exec_foreach_folder(folder, block, line, temp, directives):
    try:
        lexer = newLexer(line)

        for i in Path(folder).iterdir():
            file = folder+'/'+i.name

            if i.is_dir():
                if Directives.subfolders in directives:
                    exec_foreach_folder(file, block, line,
                                        temp, directives)
            elif not Checks.isXMLfile(file):
                continue
            else:
                fileVar = temp if temp != None else symbolTable.hidden()
                code = 'from xmlfile ' + fileVar + " := '" + file + "' " + \
                    block  # keep the {} for the 'from' to be scoped!
                lexer.lineno = line
                try:
                    parser.parse(code, lexer)
                except ContinueStatement:
                    pass

                if temp == None:
                    symbolTable.delete(fileVar)

    except FileNotFoundError as e:
        UI.emitError(G.Error(f'Folder "{folder}" not found'))


def exec_foreach_array(l, block, line, temp, directives):
    for i in l.value:
        iASstr = (("'" if i.type.value <= ExprT.STRING.value else "") +
                  str(i) + ("'" if i.type.value <= ExprT.STRING.value else ""))

        code = ''
        if temp != None:
            code = temp + " := " + iASstr + ' '
        code += block[1:-1]

        try:
            parser.parse(code, lexer=newLexer(line))
        except ContinueStatement:
            pass

        if temp != None:
            tempSym = symbolTable.get(temp)

            i.type = ExprT.bySymbol(tempSym)
            i.value = tempSym.value


def p_error(p):
    UI.emitError(G.Error("Syntax error in input: " + p.value, p.lineno))

#########################################################


###################### Grammar #############################
precedence = (
    ('right', 'ASSIGN_VAR', 'ASSIGN_CONST'),
    ('left', 'SLICER'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'EQUAL', 'NOT_EQUAL'),
    ('nonassoc', 'GREATER', 'GREATER_EQUAL', 'LESS', 'LESS_EQUAL'),
    ('left', 'PLUS', 'MINUS', 'MUL', 'DIV', 'MOD', 'EXTEND'),
    ('right', 'NOT', 'UMINUS', 'UPLUS'),
    ('left', 'BRACE_L', 'BRACE_R'),
    ('left', 'PAR_L', 'PAR_R'),
)


start = 'program'


def p_empty(p):
    'empty :'
    p[0] = None


def p_program(p):
    '''
    program : empty
            | stmts
    '''


def p_stmts(p):
    '''
    stmts : stmt
          | stmts stmt
    '''


def p_stmt(p):
    '''
    stmt : DUMMY_STMT
         | exitstmt
         | expr
         | decl
         | linkdef
         | linkexec
         | fromstmt
         | extractstmt
         | printstmt
         | ifstmt
         | foreachstmt
         | BREAK
         | CONTINUE
         | generatestmt
         | CURLY_BRACE_L BLOCK_WITH_CODE
    '''
    global activeLoopCounter

    assert activeLoopCounter >= 0

    if p[1] in ['break', 'continue']:
        if activeLoopCounter == 0:
            p[0] = afterErrorExpression
            UI.emitError(G.Error(f'"{p[1]}" statement outside of a loop'))
        else:
            if p[1] == 'break':
                raise BreakStatement
            raise ContinueStatement


def p_exitstmt(p):
    '''
    exitstmt : EXIT
    '''
    sys.exit()


def p_linkdef(p):
    '''
    linkdef : ARROW BLOCK_WITH_CODE
    '''
    global linkCode, linkCodeStartingLine
    linkCode = p[2]
    linkCodeStartingLine = p.lineno(2)


def p_linkexec(p):
    '''
    linkexec : expr drctvs COMMA expr drctvs
    '''
    global fileLocation, fileName, linkSubfolders

    directivesError = False

    if Directives.filelocation in p[2] and Directives.filename in p[5]:
        if p[1].type.value > ExprT.STRING.value:
            p[0] = afterErrorExpression
            UI.emitError(G.Error(
                f'File location in "->" code block must be an expression of type {ExprT.STRING.name}, not {p[1].type.name}', p.lineno(1)))
        elif p[4].type.value > ExprT.STRING.value:
            p[0] = afterErrorExpression
            UI.emitError(G.Error(
                f'File name in "->" code block must be an expression of type {ExprT.STRING.name}, not {p[4].type.name}', p.lineno(4)))
        else:
            p[2].remove(Directives.filelocation)
            p[5].remove(Directives.filename)

            fileLocation = p[1]
            fileName = p[4]

            try:
                p[2].remove(Directives.subfolders)
                linkSubfolders = True
            except KeyError:
                linkSubfolders = False

            if len(p[2]) > 0 or len(p[5]) > 0:
                directivesError = True
    elif Directives.filelocation in p[5] and Directives.filename in p[2]:
        if p[4].type.value > ExprT.STRING.value:
            p[0] = afterErrorExpression
            UI.emitError(G.Error(
                f'File location in "->" code block must be an expression of type {ExprT.STRING.name}, not {p[4].type.name}', p.lineno(4)))
        elif p[1].type.value > ExprT.STRING.value:
            p[0] = afterErrorExpression
            UI.emitError(G.Error(
                f'File name in "->" code block must be an expression of type {ExprT.STRING.name}, not {p[1].type.name}', p.lineno(1)))
        else:
            p[5].remove(Directives.filelocation)
            p[2].remove(Directives.filename)

            fileLocation = p[4]
            fileName = p[1]

            try:
                p[5].remove(Directives.subfolders)
                linkSubfolders = True
            except KeyError:
                linkSubfolders = False

            if len(p[2]) > 0 or len(p[5]) > 0:
                directivesError = True
    else:
        directivesError = True

    if directivesError:
        p[0] = afterErrorExpression
        UI.emitError(G.Error(
            f'Invalid directives - unable to perform linking', linkCodeStartingLine))


def p_ifstmt(p):
    '''
    ifstmt : IF expr BLOCK_WITH_CODE
           | IF expr BLOCK_WITH_CODE ELSE BLOCK_WITH_CODE
           | IF expr BLOCK_WITH_CODE ELSE empty ifstmt
    '''
    if bool(p[2]):
        parser.parse(p[3][1:-1], lexer=newLexer(p.lineno(1)))
    elif len(p) == 6:
        parser.parse(p[5][1:-1], lexer=newLexer(p.lineno(1)))
    else:
        pass


def p_foreachstmt(p):
    '''
    foreachstmt : FOR_EACH decl IN expr BLOCK_WITH_CODE drctvs
    '''
    global activeLoopCounter

    if p[4].type == ExprT.ARRAY:
        temp = None if p[2].symbol == None else p[2].symbol.name

        activeLoopCounter += 1
        try:
            exec_foreach_array(p[4], p[5], p.lineno(5), temp, p[6])
        except BreakStatement:
            pass
        activeLoopCounter -= 1
        assert activeLoopCounter >= 0
    elif p[4].type == ExprT.folder:
        if p[2].type != ExprT.xmlfile:
            p[0] = afterErrorExpression
            UI.emitError(G.Error(
                '"foreach" statement iterating through a folder excpects a "xmlfile" type', p.lineno(2)))
        else:
            fold = p[4].value
            temp = None if p[2].symbol == None else p[2].symbol.name

            activeLoopCounter += 1
            try:
                exec_foreach_folder(fold, p[5], p.lineno(5), temp, p[6])
            except BreakStatement:
                pass
            activeLoopCounter -= 1
            assert activeLoopCounter >= 0
    else:
        p[0] = afterErrorExpression
        UI.emitError(G.Error(
            'Tried to iterate through non-iterable type '+p[4].type.name, p.lineno(4)))
        pass


def p_expr(p):
    '''
    expr : assignexpr
         | augmassignexpr
         | linkedxpathsexpr
         | expr GREATER expr
         | expr GREATER_EQUAL expr
         | expr LESS expr
         | expr LESS_EQUAL expr
         | expr EQUAL expr
         | expr NOT_EQUAL expr
         | expr AND expr
         | expr OR expr
         | expr PLUS expr
         | expr MINUS expr
         | expr MUL expr
         | expr DIV expr
         | expr MOD expr
         | expr EXTEND expr
         | term
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        err = False

        if p[2] == '>':
            if p[1].type not in [ExprT.NONE, ExprT.BOOLEAN, ExprT.ARRAY] and p[3].type not in [ExprT.NONE, ExprT.BOOLEAN, ExprT.ARRAY]:
                v = p[1].value > p[3].value
                p[0] = Expression(ExprT.BOOLEAN, True, value=v)
            else:
                err = True

        elif p[2] == '>=':
            if p[1].type not in [ExprT.NONE, ExprT.BOOLEAN, ExprT.ARRAY] and p[3].type not in [ExprT.NONE, ExprT.BOOLEAN, ExprT.ARRAY]:
                v = p[1].value >= p[3].value
                p[0] = Expression(ExprT.BOOLEAN, True, value=v)
            else:
                err = True

        elif p[2] == '<':
            if p[1].type not in [ExprT.NONE, ExprT.BOOLEAN, ExprT.ARRAY] and p[3].type not in [ExprT.NONE, ExprT.BOOLEAN, ExprT.ARRAY]:
                v = p[1].value < p[3].value
                p[0] = Expression(ExprT.BOOLEAN, True, value=v)
            else:
                err = True

        elif p[2] == '<=':
            if p[1].type not in [ExprT.NONE, ExprT.BOOLEAN, ExprT.ARRAY] and p[3].type not in [ExprT.NONE, ExprT.BOOLEAN, ExprT.ARRAY]:
                v = p[1].value <= p[3].value
                p[0] = Expression(ExprT.BOOLEAN, True, value=v)
            else:
                err = True

        elif p[2] == '=':
            left = p[1].value
            right = p[3].value
            if p[1].type == ExprT.ARRAY:
                left = [i.value for i in p[1].value]
            if p[3].type == ExprT.ARRAY:
                right = [i.value for i in p[3].value]

            v = left == right
            p[0] = Expression(ExprT.BOOLEAN, True, value=v)

        elif p[2] == '!=':
            left = p[1].value
            right = p[3].value
            if p[1].type == ExprT.ARRAY:
                left = [i.value for i in p[1].value]
            if p[3].type == ExprT.ARRAY:
                right = [i.value for i in p[3].value]

            v = left != right
            p[0] = Expression(ExprT.BOOLEAN, True, value=v)

        elif p[2] == 'and':
            v = bool(p[1]) and bool(p[3])
            p[0] = Expression(ExprT.BOOLEAN, True, value=v)

        elif p[2] == 'or':
            v = bool(p[1]) or bool(p[3])
            p[0] = Expression(ExprT.BOOLEAN, True, value=v)

        elif p[2] == '+':
            if (p[1].type == ExprT.ARITHMETIC and p[3].type == ExprT.ARITHMETIC) or (p[1].type.value <= ExprT.STRING.value and p[3].type != ExprT.ARRAY):
                v = p[1].value + (
                    p[3].value if p[1].type == ExprT.ARITHMETIC else str(p[3])
                )
                p[0] = Expression(p[1].type, True, value=v)
            elif p[1].type == ExprT.ARRAY:
                p[1].value.append(Expression(
                    p[3].type,
                    False,
                    None,
                    p[3].value if p[3].type != ExprT.ARRAY else [
                        i for i in p[3].value],
                    arrayItem=True
                ))
                v = p[1].value
                p[0] = Expression(p[1].type, True, value=v)
            else:
                err = True

        elif p[2] == '-':
            if p[1].type == ExprT.ARITHMETIC and p[3].type == ExprT.ARITHMETIC:
                v = p[1].value - p[3].value
                p[0] = Expression(p[1].type, True, value=v)
            else:
                err = True

        elif p[2] == '*':
            if (p[1].type == ExprT.ARITHMETIC or p[1].type.value <= ExprT.STRING.value) and p[3].type == ExprT.ARITHMETIC:
                v = p[1].value * p[3].value
                p[0] = Expression(
                    ExprT.ARITHMETIC if p[1].type == ExprT.ARITHMETIC else ExprT.STRING,
                    True,
                    value=v
                )
            else:
                err = True

        elif p[2] == 'div':
            if p[1].type == ExprT.ARITHMETIC and p[3].type == ExprT.ARITHMETIC:
                v = p[1].value / p[3].value
                p[0] = Expression(p[1].type, True, value=v)
            else:
                err = True

        elif p[2] == 'mod':
            if p[1].type == ExprT.ARITHMETIC and p[3].type == ExprT.ARITHMETIC:
                v = p[1].value % p[3].value
                p[0] = Expression(p[1].type, True, value=v)
            else:
                err = True

        elif p[2] == '++':
            if p[1].type == ExprT.ARRAY:
                p[1].value.extend(
                    [i for i in p[3].value] if p[3].type == ExprT.ARRAY else [
                        Expression(p[3].type, False, None, p[3].value, True)]
                )
                v = p[1].value
                p[0] = Expression(p[1].type, True, value=v)
            else:
                err = True
        else:
            assert (False)

        if err:
            p[0] = afterErrorExpression
            UI.emitError(
                G.Error('Invalid operand type(s) for operator "' + p[2] + '". Operand types are "' + p[1].type.name + '" and "' + p[3].type.name + '"', p.lineno(2)))


def p_assignexpr(p):
    '''
    assignexpr : lvalue ASSIGN_VAR expr
               | lvalue ASSIGN_CONST expr
    '''
    if p[1].indexed == True and p[1].arrayItem == False:
        p[0] = afterErrorExpression
        UI.emitError(G.Error(
            'Tried to change contents of an immuatable STRING-based type', p.lineno(1)))
    elif p[2] == '.=':
        if p[1].value == None or not p[1].const:
            if p[1].type.isStrict():
                if p[1].type != p[3].type and p[3].type != ExprT.STRING:
                    e = G.Error('lvalue of type ' + str(
                        p[1].type.name) + ' can only be assigned values of same type or of type STRING', p.lineno(2))
                    UI.emitError(e)
                    p[0] = afterErrorExpression
                else:
                    sym = p[1].symbol
                    val = p[3].value

                    if p[1].arrayItem == True:
                        p[0] = afterErrorExpression
                        UI.emitError(
                            G.Error('An array item can not be constant', p.lineno(2)))
                    else:
                        sym.value = val
                        sym.const = True
                        symbolTable.update(sym)

                        if sym.name == '$GALAKTION_ROOT':
                            os.chdir(val)

                        p[0] = Expression(
                            p[1].type,
                            True,
                            value=val,
                            arrayItem=p[1].arrayItem
                        )
            elif p[1].type.isFlex():
                typ = p[3].type
                sym = p[1].symbol
                val = p[3].value

                if p[1].arrayItem == True:
                    p[0] = afterErrorExpression
                    UI.emitError(
                        G.Error('An array item can not be constant', p.lineno(2)))
                else:
                    sym.value = val
                    sym.const = True
                    if p[3].symbol != None:
                        sym.type = p[3].symbol.type
                    else:
                        sym.type = SymT.flexible if typ.isFlex(
                        ) else SymT(int(typ))
                    symbolTable.update(sym)

                    p[0] = Expression(
                        typ,
                        True,
                        value=val,
                        arrayItem=p[1].arrayItem
                    )
            else:
                assert False
        else:
            UI.emitError(G.Error(
                'Assignment to a constant', p.lineno(2)))
            p[0] = afterErrorExpression
    else:   # p[2] == ASSIGN_VAR
        if p[1].const:
            UI.emitError(G.Error(
                'Assignment to a constant', p.lineno(2)))
            p[0] = afterErrorExpression
        elif p[1].type.isStrict():
            if p[1].type != p[3].type and p[3].type != ExprT.STRING:
                e = G.Error('lvalue of type ' + str(
                    p[1].type.name) + ' can only be assigned values of same type or of type STRING', p.lineno(2))
                UI.emitError(e)
                p[0] = afterErrorExpression
            else:
                sym = p[1].symbol
                val = p[3].value

                if p[1].arrayItem == True:
                    p[1].value = val
                else:
                    sym.value = val
                    symbolTable.update(sym)

                    if sym.name == '$GALAKTION_ROOT':
                        os.chdir(val)

                p[0] = Expression(
                    p[1].type,
                    False,
                    value=val,
                    arrayItem=p[1].arrayItem
                )
        elif p[1].type.isFlex():
            typ = p[3].type
            sym = p[1].symbol
            val = p[3].value

            if p[1].arrayItem == True:
                p[1].type = typ
                p[1].value = val
            else:
                sym.value = val

                if p[3].symbol != None:
                    sym.type = p[3].symbol.type
                else:
                    sym.type = SymT.flexible if typ.isFlex(
                    ) else SymT(int(typ))
                symbolTable.update(sym)

            p[0] = Expression(
                typ,
                False,
                value=val,
                arrayItem=p[1].arrayItem
            )
        else:
            assert False


def p_augmassignexpr(p):
    '''
    augmassignexpr : lvalue augmassign expr
    '''
    asgn, op = p[2]
    asgn += '='

    p[2] = op
    p_expr(p)

    p[3] = p[0]
    p[2] = asgn
    p[0] = None
    p_assignexpr(p)


def p_augmassign(p):
    '''
    augmassign : MINUS_ASSIGN_VAR
               | PLUS_ASSIGN_VAR
               | MUL_ASSIGN_VAR
               | DIV_ASSIGN_VAR
               | MOD_ASSIGN_VAR
               | EXTEND_ASSIGN_VAR
               | MINUS_ASSIGN_CONST
               | PLUS_ASSIGN_CONST
               | MUL_ASSIGN_CONST
               | DIV_ASSIGN_CONST
               | MOD_ASSIGN_CONST
               | EXTEND_ASSIGN_CONST
    '''
    p[0] = (p[1][0], p[1][1:])
    p.set_lineno(0, p.lineno(1))


def p_term(p):
    '''
    term : PAR_L expr PAR_R
         | MINUS expr %prec UMINUS
         | PLUS expr %prec UPLUS
         | NOT expr
         | primary
    '''
    if p[1] == '(':
        p[0] = Expression(
            p[2].type,
            True,
            symbol=None,
            value=p[2].value
        )
    elif p[1] == '-':
        if p[2].type == ExprT.ARITHMETIC:
            p[0] = Expression(
                ExprT.ARITHMETIC,
                True,
                symbol=None,
                value=-p[2].value
            )
        else:
            UI.emitError(
                G.Error('UMINUS on non arithmetic expression', p.lineno(2)))
            p[0] = afterErrorExpression
    elif p[1] == '+':
        if p[2].type == ExprT.ARITHMETIC:
            p[0] = Expression(
                ExprT.ARITHMETIC,
                True,
                symbol=None,
                value=p[2].value
            )
        else:
            UI.emitError(
                G.Error('UPLUS on non arithmetic expression', p.lineno(2)))
            p[0] = afterErrorExpression
    elif p[1] == 'not':
        p[0] = Expression(
            ExprT.BOOLEAN,
            True,
            symbol=None,
            value=not bool(p[2])
        )
    else:   # primary
        p[0] = p[1]


def p_primary(p):
    '''
    primary : lvalue
            | arraydef
            | const
            | sliced
    '''
    p[0] = p[1]


def p_lvalue(p):
    '''
    lvalue : ID
           | decl
           | indexed
    '''
    if type(p[1]) == str:
        if not symbolTable.lookup(p[1]):
            UI.emitError(
                G.Error('"' + p[1] + '" is not declared', p.lineno(1)))
            p[0] = afterErrorExpression
        else:
            sym = symbolTable.get(p[1])

            p[0] = Expression(
                ExprT.bySymbol(sym),
                sym.const,
                symbol=sym,
                value=sym.value
            )
    else:
        p[0] = p[1]


def p_arraydef(p):
    '''
    arraydef : BRACE_L exprlist BRACE_R
    '''
    val = []
    for i in p[2]:
        val.append(Expression(
            i.type,
            False,
            None,
            i.value,
            arrayItem=True
        ))

    p[0] = Expression(
        ExprT.ARRAY,
        True,
        None,
        val
    )


def p_indexed(p):
    '''
    indexed : expr index
    '''
    if p[1].type != ExprT.ARRAY and p[1].type.value > ExprT.STRING.value:
        p[0] = afterErrorExpression
        UI.emitError(
            G.Error('Cannot index type ' + p[1].type.name, p.lineno(1)))
    else:
        if p[2].index_galaktion == 0:
            t = ExprT.STRING
            v = ''
            if p[1].type == ExprT.ARRAY:
                t = ExprT.ARRAY
                v = []

            # the 2 last are False in order an error not to be caused at assignexpr rule, first if statement
            p[0] = Expression(t, True, None, v, arrayItem=False, indexed=False)
        else:
            p[0] = Expression(
                ExprT.STRING,
                True,
                None,
                p[1].value[p[2].index_python],
                arrayItem=False,
                indexed=True
            )

            if p[1].type == ExprT.ARRAY:
                p0 = p[1].value[p[2].index_python]

                assert p0.arrayItem == True
                p0.indexed = True

                p[0] = p0


def p_sliced(p):
    '''
    sliced : expr slice
    '''
    if p[1].type != ExprT.ARRAY and p[1].type.value > ExprT.STRING.value:
        p[0] = afterErrorExpression
        UI.emitError(
            G.Error('Cannot slice type ' + p[1].type.name, p.lineno(1)))
    else:
        p[0] = Expression(
            ExprT.ARRAY if p[1].type == ExprT.ARRAY else ExprT.STRING,
            True,
            None,
            p[1].value[p[2].start_python: p[2].stop_python]
        )


def p_decl(p):
    '''
    decl : type
         | type ID
    '''
    if len(p) == 2:
        p[0] = Expression(
            ExprT(SymT.byName(p[1]).value),
            True,
            symbol=None,
            value=None
        )
    else:
        sym = Symbol(SymT.byName(p[1]), False, p[2], None)
        symbolTable.update(sym)

        p[0] = Expression(
            ExprT(sym.type.value),
            False,
            symbol=sym,
            value=sym.value
        )


def p_type(p):
    '''
    type : stricttype
         | flexible
    '''
    p[0] = p[1]


def p_stricttype(p):
    '''
    stricttype : folder
               | filetype
    '''
    p[0] = p[1]


def p_filetype(p):
    '''
    filetype : xmlfile
             | jsonfile
             | csvfile
    '''
    p[0] = p[1]


def p_printstmt(p):
    '''
    printstmt : PRINT exprlist
    '''
    if len(p[2]) == 0:
        p[2] = ['']

    what2print = ''
    for e in p[2]:
        what2print += str(e)

    print(what2print, flush=True)


def p_exprlist(p):
    '''
    exprlist : empty
             | expr
             | exprlist COMMA expr
    '''
    if p[1] == None:
        p[0] = []
    elif len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[3])
        p[0] = p[1]


def p_const(p):
    '''
    const : STRING_LITERAL
          | number
          | TRUE
          | FALSE
          | NONE
    '''
    if p[1] == 'none':
        p[0] = Expression(
            ExprT.NONE,
            True,
            symbol=None,
            value=None
        )
    elif p[1] == 'false':
        p[0] = Expression(
            ExprT.BOOLEAN,
            True,
            symbol=None,
            value=False
        )
    elif p[1] == 'true':
        p[0] = Expression(
            ExprT.BOOLEAN,
            True,
            symbol=None,
            value=True
        )
    elif type(p[1]) == Expression:
        p[0] = p[1]
    else:   # STRING_LITERAL
        p[0] = Expression(
            ExprT.STRING,
            True,
            symbol=None,
            value=p[1]
        )


def p_number(p):
    '''
    number : INTEGER
           | REAL
    '''
    p[0] = Expression(
        ExprT.ARITHMETIC,
        True,
        symbol=None,
        value=p[1]
    )


def p_drctvs(p):
    '''
    drctvs : empty
           | DIRECTIVE
           | drctvs DIRECTIVE
    '''
    if p[1] == None:
        p[0] = {}
    elif len(p) == 2:
        p[0] = {p[1]}
    else:
        p[1].add(p[2])
        p[0] = p[1]


def p_generatestmt(p):
    '''
    generatestmt : GENERATE decl name drctvs
    '''
    t = p[2].type

    if p[2] is afterErrorExpression:
        UI.emitError(G.Error('Invalid declaration', p.lineno(2)))

    if not Checks.isTypeGeneratable(t):
        UI.emitError(
            G.Error('Generation for type "' + str(t.name) + '" is not supported' + (' yet' if t == ExprT.xmlfile else ''), p.lineno(1)))
        return

    if Checks.isExtensionInvalid(t, p[3]):
        UI.emitError(G.Error('Invalid extension for type ' +
                     str(p[2].type.name), p.lineno(4)))
        return

    if p[2].symbol != None and not G.SymbolTable.isHiddenSymbol(p[2].symbol):
        p[2].symbol.value = p[3]
        symbolTable.update(p[2].symbol)
        p[2].value = p[2].symbol.value

    try:
        if t == ExprT.jsonfile:
            with open(p[3], 'w') as out:
                try:
                    out.write(json.dumps(list(G.memory.values()), indent=4))
                except (UnicodeEncodeError, UnicodeDecodeError, UnicodeError):
                    UI.emitWarning(G.Warning(
                        "Something went wrong with the encoding", p.lineno(1)))
        else:  # t == ExprT.csvfile
            try:
                df = pd.json_normalize(G.memory.values())
                df.to_csv(p[3], index=False)
            except (UnicodeEncodeError, UnicodeDecodeError, UnicodeError):
                UI.emitWarning(
                    "Something went wrong with the encoding", p.lineno(1))
    except Exception as e:
        UI.emitWarning(
            G.Warning('Could not generate file "' + p[3] + '" - ' + str(e), p.lineno(1)))

    if Directives.flush in p[4]:
        p[4].remove(Directives.flush)

        G.memory.clear()

    if len(p[4]) > 0:
        UI.emitWarning(
            G.Warning('One or more directives were ignored', p.lineno(4)))


def p_fromstmt(p):
    '''
    fromstmt : FROM expr
             | FROM expr BLOCK_WITH_CODE
    '''
    global userSrcFiles

    if p[2].type != ExprT.xmlfile:
        p[0] = afterErrorExpression
        UI.emitError(G.Error(
            '"from" statement excpects an expression of type "xmlfile"', p.lineno(2)))
    elif len(p) == 3:
        userSrcFiles.pop()
        userSrcFiles.push(p[2].value)
    else:
        userSrcFiles.push(p[2].value)
        parser.parse(p[3][1:-1], lexer=newLexer(p.lineno(3)))
        userSrcFiles.pop()


def jsonSerializable(expr: Expression):
    assert expr.type == ExprT.ARRAY

    ret = []

    for i in expr.value:
        if i.type != ExprT.ARRAY:
            ret.append(i.value)
        else:
            ret.append(jsonSerializable(i))

    return ret


def p_extractstmt(p):
    '''
    extractstmt : EXTRACT expr name drctvs
    '''
    key = p[3]
    val = p[2].value

    if p[2].type == ExprT.ARRAY:
        val = jsonSerializable(p[2])

    if p[4] != None:
        # The set is not empty, so it contains directives.
        # Check which directives exist and each one you find,
        # remove it from the set.
        # This is important for the warning at the last step.

        if Directives.latlong in p[4]:
            p[4].remove(Directives.latlong)

            if val:
                if type(val) == list:
                    val = [getLatLong(i) for i in val]
                else:
                    val = getLatLong(val)
        elif Directives.longlat in p[4]:
            p[4].remove(Directives.longlat)

            if val:
                if type(val) == list:
                    val = [getLongLat(i) for i in val]
                else:
                    val = getLongLat(val)

    srcFile = userSrcFiles.top()

    if G.memory.get(srcFile, None) == None:
        G.memory[srcFile] = {}

    G.memory[srcFile][key] = val

    # At this point, if a directive is still contained
    # in the set, it means that it was ignored (because
    # was invalid for extract statement).
    # If so, throw a warning.
    if len(p[4]) > 0:
        UI.emitWarning(
            G.Warning('One or more directives were ignored', p.lineno(4)))


def p_name(p):
    '''
    name : AS expr
    '''
    if p[2].type.value > ExprT.STRING.value:
        p[0] = afterErrorExpression
        UI.emitError(G.Error(
            'Expected expression of string-based type, but got of type ' + p[2].type.name, p.lineno(2)))
    else:
        p[0] = p[2].value


def p_linkedxpathsexpr(p):
    '''
    linkedxpathsexpr : linkedxpaths
    '''

    xpathResultsExprs = []

    # Eliminate XML element/node results
    for result in p[1]:
        if type(result) != etree._Element:
            xpathResultsExprs.append(
                Expression(
                    type=ExprT.byValue(result),
                    symbol=None,
                    value=result,
                    arrayItem=True,
                    const=False
                )
            )

    if len(xpathResultsExprs) == 1:
        p[0] = xpathResultsExprs[0]
        p[0].arrayItem = False
        p[0].const = True
    else:
        p[0] = Expression(
            ExprT.ARRAY,
            True,
            None,
            xpathResultsExprs
        )


def p_linkedxpaths(p):
    '''
    linkedxpaths : xpath
                 | linkedxpaths ARROW xpath
    '''
    global currentSrcElement

    if len(p) == 2:
        if userSrcFiles.size() == 0:
            p[0] = afterErrorExpression
            UI.emitError(G.Error('No source XML file spcified', p.lineno(1)))
        else:
            file = None
            err = False

            xpathVars, undeclaredVars = getDynamicXPathVariables(p[1])

            if len(undeclaredVars) > 0:
                err = True
                UI.emitError(G.Error('Undeclared identifier'+(''if len(undeclaredVars)
                             == 1 else 's')+': '+str(undeclaredVars), p.lineno(1)))
            else:
                try:
                    if currentSrcElement == None:
                        val = etree.parse(userSrcFiles.top()).xpath(
                            p[1], **xpathVars)
                    else:  # when executing link code
                        val = currentSrcElement.xpath(p[1], **xpathVars)
                except FileNotFoundError:
                    UI.emitWarning(
                        G.Warning(f'Source file "{userSrcFiles.top()}" not found by XPath expressions "{p[1]}"', p.lineno(1)))
                except (etree.XPathSyntaxError, etree.XPathEvalError) as e:
                    err = True
                    UI.emitError(
                        G.Error('Invalid XPath expression: '+str(e), p.lineno(1)))
                except Exception as e:
                    err = True
                    UI.emitError(
                        G.Error(f'Unexpected error {userSrcFiles.top()}: ' + str(e), p.lineno(1)))
                else:
                    if type(val) != list:
                        val = [val]
                    p[0] = val

        if err:
            p[0] = afterErrorExpression
    else:
        if linkCode == None:
            p[0] = afterErrorExpression
            UI.emitError(G.Error('Undefined code block for "->"', p.lineno(2)))
        elif symbolTable.get('$GALAKTION_ROOT').value == None:
            p[0] = afterErrorExpression
            UI.emitError(
                G.Error('In order to use "->", $GALAKTION_ROOT must be set', p.lineno(2)))
        else:
            # Perform linking...
            xpathResults = p[1]
            xpathResults_new = []

            for r in xpathResults:
                if type(r) == etree._Element:
                    currentSrcElement = r
                    r = getPath2File(symbolTable.get('$GALAKTION_ROOT').value)
                    if r == None:
                        file = currentSrcElement.getroottree().docinfo.URL
                        elementPath = currentSrcElement.getroottree().getpath(currentSrcElement)
                        UI.emitWarning(G.Warning(
                            f'{file}: {elementPath}: Could not link', p.lineno(2)))
                    currentSrcElement = None

                if r != None:
                    if type(r) != list:
                        r = [r]
                    xpathResults_new.extend(r)

            xpathResults = xpathResults_new

            err = False
            xpathVars, undeclaredVars = getDynamicXPathVariables(p[3])

            if len(undeclaredVars) > 0:
                err = True
                UI.emitError(G.Error('Undeclared identifier'+(''if len(undeclaredVars)
                             == 1 else 's')+': '+str(undeclaredVars), p.lineno(1)))
            else:
                xpathResults_new = []
                for r in xpathResults:
                    try:
                        if type(r) == str:
                            val = etree.parse(r).getroot().xpath(
                                p[3], **xpathVars)
                        elif type(r) == etree._Element:
                            assert False
                        else:
                            val = r
                    except FileNotFoundError:
                        UI.emitWarning(
                            G.Warning(f'Source file "{r}" not found by XPath expressions "{p[1]}"', p.lineno(1)))
                    except (etree.XPathSyntaxError, etree.XPathEvalError) as e:
                        err = True
                        UI.emitError(
                            G.Error('Invalid XPath expression: '+str(e), p.lineno(1)))
                    else:
                        if type(val) != list:
                            val = [val]
                        xpathResults_new.extend(val)

                xpathResults = xpathResults_new
                p[0] = xpathResults
            if err:
                p[0] = afterErrorExpression


def p_xpath(p):
    '''
    xpath : MUL
          | PATH
          | PATH PAR_PATH
          | xpath predicates
          | xpath index
          | xpath slice
          | xpath PATH
          | xpath PATH PAR_PATH
          | PAR_L xpath PAR_R
    '''
    if len(p) == 2:
        p[0] = p[1]

    else:
        if type(p[2]) == G.Index:
            p2 = '[' + p[2].index_xpath + ']'
        elif type(p[2]) == G.Slice:
            p2 = '[' + p[2].start_stop_xpath + ']'

            if p2 == '[]':
                p2 = ''
        else:
            p2 = p[2]

        p3 = ''
        if len(p) == 4:
            p3 = p[3]

        p[0] = p[1] + p2 + p3


def p_predicates(p):
    '''
    predicates : PREDICATE
               | predicates PREDICATE
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[2]


def p_slice(p):
    '''
    slice : BRACE_L expr SLICER expr BRACE_R
          | BRACE_L SLICER expr BRACE_R
          | BRACE_L expr SLICER BRACE_R
          | BRACE_L SLICER BRACE_R
    '''
    if len(p) == 4:
        p[0] = G.Slice(G.Index(None), G.Index(None))
    elif len(p) == 5:
        if p[2] == '~':
            p[0] = G.Slice(
                G.Index(None),
                checkIndex(p[3], p.lineno(3))
            )
        else:
            p[0] = G.Slice(
                checkIndex(p[2], p.lineno(2)),
                G.Index(None)
            )
    else:
        p[0] = G.Slice(
            checkIndex(p[2], p.lineno(2)),
            checkIndex(p[4], p.lineno(4))
        )


def p_index(p):
    '''
    index : BRACE_L expr BRACE_R
    '''
    p[0] = checkIndex(p[2], p.lineno(2))

##############################################################


##############   I N I T I A L I Z A T I O N   ###############

symbolTable.update(
    Symbol(
        type=SymT.folder,
        const=False,
        name='$GALAKTION_ROOT',
        value=os.getcwd()
    )
)

##############################################################


########################   R U N   ###########################

try:
    with open(UI.userSourceFile, 'r') as inp:
        parserInput = inp.read()
except FileNotFoundError:
    UI.print('Error: Input file not found')
    sys.exit()


# Test Input (uncomment to use)
# parserInput = '''
#
# '''


parser = yacc.yacc()  # Create the parser object.

try:
    parser.parse(parserInput, lexer=newLexer(1))
except KeyboardInterrupt:
    UI.print('Aborting...')
    sys.exit()

UI.printWarnings()
UI.printErrors()

##############################################################

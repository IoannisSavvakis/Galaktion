import ply.yacc as yacc
from Galaktion_lexer import tokens, newLexer
import os
import sys
import argparse
from pathlib import Path
import xml.etree.ElementTree as ET
import json
import pandas as pd
import re
import Galaktion_core as G
from Galaktion_core import ExpressionType as ExprT, Expression, Symbol, SymbolType as SymT, Checks, ActivationRecord, ActivationRecordManager


############## Variables ##############################


couldNotLink = False


UI = G.UserInterface(True)
symbolTable = G.SymbolTable()

srcFiles = G.SourceFileManager()

activRecs = ActivationRecordManager()
activRecs.push(ActivationRecord(newLexer()))
activRecs.top().linkingFiles = []


linkCode = None
linkSubfolders = None
currentSrcElement = None
fileLocation = None
fileName = None


afterErrorExpression = Expression(ExprT.NONE, True)


######################################################


#################### Methods #############################


def checkIndex(e: Expression, line: int):
    global UI, afterErrorExpression

    if e.type != ExprT.ARITHMETIC:
        UI.emitError(G.Error('Indeces should be integer numbers', line))
        return afterErrorExpression
    else:
        idx = e.value
        if type(idx) == float:
            UI.emitWarning(G.Warning('Floating point index', line))
            idx = int(idx)

        return G.Index(idx)


#   Degrees Minutes Seconds to Decimal Degrees
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

        if i.is_dir():
            f = findFile(i.resolve(), fileName)

            if f != None:
                return f

    return None


def getPath2File(root, pathType='relative'):
    global currentSrcElement, fileName, fileLocation

    assert (currentSrcElement != None)

    activRecs.push(ActivationRecord(
        lexer=newLexer(),
        code=linkCode[1:-1]
    ))
    parser.parse(activRecs.top().code,
                 lexer=activRecs.top().lexer)
    activRecs.pop()

    currentSrcElement = None

    assert (fileLocation != None and fileName != None)

    fileLocation.value = root + '/' + fileLocation.value

    if len(fileLocation.value) < len(root + '/'):
        # print('fileLocation:', fileLocation)
        # print('root:', root+'/')
        return None
    assert len(fileLocation.value) >= len(root + '/')

    absPath = findFile(fileLocation.value, fileName.value)

    fileLocation = None
    fileName = None

    if absPath == None:
        return None

    if pathType == 'relative':
        return absPath[len(root)+1:]

    if pathType == 'absolute':
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


def exec_foreach_folder(folder, block, temp, directives):
    try:
        for i in Path(folder).iterdir():
            file = folder+'/'+i.name

            if i.is_dir():
                if '@subfolders' in directives:
                    exec_foreach_folder(file, block, temp, directives)
            elif not Checks.isXMLfile(file):
                continue
            else:
                code = temp + " = '" + file + "'\nfrom " + temp + block

                activRecs.push(ActivationRecord(
                    lexer=newLexer(),
                    code=code
                ))
                parser.parse(activRecs.top().code,
                             lexer=activRecs.top().lexer)
                activRecs.pop()
    except FileNotFoundError:
        UI.emitError(G.Error('File not found'))


def exec_foreach_list(l, block, temp, directives):
    for i in l:
        iASstr = (("'" if i.type == ExprT.STRING else "") +
                  str(i) + ("'" if i.type == ExprT.STRING else ""))
        code = temp + " = " + iASstr + '\n' + block[1:-1]

        activRecs.push(ActivationRecord(
            lexer=newLexer(),
            code=code
        ))
        parser.parse(activRecs.top().code,
                     lexer=activRecs.top().lexer)
        activRecs.pop()


def p_error(p):
    print("Syntax error in input:", p.value)
    sys.exit()


#########################################################


###################### Grammar #############################


precedence = (
    ('right', 'EQUAL', 'COLON'),
    ('left', 'COLON_COLON'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'EQUAL_EQUAL', 'GREATER_LESS'),
    ('nonassoc', 'GREATER', 'GREATER_EQUAL', 'LESS', 'LESS_EQUAL'),
    ('left', 'PLUS', 'MINUS', 'PLUS_PLUS'),
    ('right', 'NOT', 'UMINUS', 'UPLUS'),
    # ('left', 'ARROW'),
    # ('left', 'SLASH'),  # very important to have greater precedence than ARROW
    ('left', 'BRACE_L', 'BRACE_R'),
    ('left', 'PAR_L', 'PAR_R'),
)


start = 'program'


def p_empty(p):
    'empty :'
    p[0] = None


def p_program(p):
    '''
    program : stmts
    '''


def p_stmts(p):
    '''
    stmts : empty
          | stmt
          | stmts stmt
    '''


# TODO break continue
def p_stmt(p):
    '''
    stmt : expr
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


def p_linkdef(p):
    '''
    linkdef : ARROW BLOCK_WITH_CODE
    '''
    global linkCode
    linkCode = p[2]


def p_linkexec(p):
    '''
    linkexec : VERTICAL_BAR expr VERTICAL_BAR drctvs COMMA VERTICAL_BAR expr VERTICAL_BAR drctvs
    '''
    global fileLocation, fileName, linkSubfolders
    err = False

    if '@filelocation' in p[4] and '@filename' in p[9]:
        p[4].remove('@filelocation')
        p[9].remove('@filename')

        fileLocation = p[2]
        fileName = p[7]

        try:
            p[4].remove('@subfolders')
            linkSubfolders = True
        except KeyError:
            linkSubfolders = False

        if len(p[4]) > 0 or len(p[9]) > 0:
            err = True

    elif '@filelocation' in p[9] and '@filename' in p[4]:
        p[9].remove('@filelocation')
        p[4].remove('@filename')

        fileLocation = p[7]
        fileName = p[2]

        try:
            p[9].remove('@subfolders')
            linkSubfolders = True
        except KeyError:
            linkSubfolders = False

        if len(p[9]) > 0 or len(p[4]) > 0:
            err = True

    else:
        err = True

    if err:
        p[0] = afterErrorExpression
        UI.emitError(G.Error('Invalid directives', p.lineno(1)))


def p_ifstmt(p):
    '''
    ifstmt : IF expr BLOCK_WITH_CODE
           | IF expr BLOCK_WITH_CODE ELSE BLOCK_WITH_CODE
           | IF expr BLOCK_WITH_CODE ELSE empty ifstmt
    '''
    if bool(p[2]):
        activRecs.push(ActivationRecord(
            lexer=newLexer(),
            code=p[3][1:-1]
        ))
        parser.parse(activRecs.top().code, lexer=activRecs.top().lexer)
        activRecs.pop()
    elif len(p) == 6:
        activRecs.push(ActivationRecord(
            lexer=newLexer(),
            code=p[5][1:-1]
        ))
        parser.parse(activRecs.top().code, lexer=activRecs.top().lexer)
        activRecs.pop()
    else:
        pass


def p_foreachstmt(p):
    '''
    foreachstmt : FOR_EACH decl IN expr BLOCK_WITH_CODE drctvs
    '''
    if p[4].type == ExprT.LIST:
        temp = p[2].symbol.name
        exec_foreach_list(p[4].value, p[5], temp, p[6])
    elif p[4].type == ExprT.folder:
        if p[2].type != ExprT.xmlfile:
            p[0] = afterErrorExpression
            UI.emitError(G.Error(
                '"foreach" statement iterating through a folder excpects a "xmlfile" type', p.lineno(1)))
        else:
            # fold = symbolTable.get(p[4].symbol.name).value
            fold = p[4].value
            temp = p[2].symbol.name

            exec_foreach_folder(fold, p[5], temp, p[6])
    else:
        p[0] = afterErrorExpression
        UI.emitError(G.Error(
            'Tried to iterate through non-iterable type '+str(p[4].type), p.lineno(3)))
        pass


def p_expr(p):
    '''
    expr : assignexpr
         | augmassignexpr
         | tagslinks
         | expr GREATER expr
         | expr GREATER_EQUAL expr
         | expr LESS expr
         | expr LESS_EQUAL expr
         | expr EQUAL_EQUAL expr
         | expr GREATER_LESS expr
         | expr AND expr
         | expr OR expr
         | expr PLUS expr
         | expr MINUS expr
         | expr PLUS_PLUS expr
         | term
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        err = False

        if p[2] == '>':
            if p[1].type != ExprT.BOOLEAN and p[3].type != ExprT.BOOLEAN:
                v = p[1].value > p[3].value
                p[0] = Expression(ExprT.BOOLEAN, True, value=v)
            else:
                err = True
        elif p[2] == '>=':
            if p[1].type != ExprT.BOOLEAN and p[3].type != ExprT.BOOLEAN:
                v = p[1].value >= p[3].value
                p[0] = Expression(ExprT.BOOLEAN, True, value=v)
            else:
                err = True
        elif p[2] == '<':
            if p[1].type != ExprT.BOOLEAN and p[3].type != ExprT.BOOLEAN:
                v = p[1].value < p[3].value
                p[0] = Expression(ExprT.BOOLEAN, True, value=v)
            else:
                err = True
        elif p[2] == '<=':
            if p[1].type != ExprT.BOOLEAN and p[3].type != ExprT.BOOLEAN:
                v = p[1].value <= p[3].value
                p[0] = Expression(ExprT.BOOLEAN, True, value=v)
            else:
                err = True
        elif p[2] == '==':
            v = p[1].value == p[3].value
            p[0] = Expression(ExprT.BOOLEAN, True, value=v)
        elif p[2] == '><':
            v = p[1].value != p[3].value
            p[0] = Expression(ExprT.BOOLEAN, True, value=v)
        elif p[2] == 'and':
            v = bool(p[1]) and bool(p[3])
            p[0] = Expression(ExprT.BOOLEAN, True, value=v)
        elif p[2] == 'or':
            v = bool(p[1]) or bool(p[3])
            p[0] = Expression(ExprT.BOOLEAN, True, value=v)
        elif p[2] == '+':
            if (p[1].type == ExprT.ARITHMETIC and p[3].type == ExprT.ARITHMETIC) or (p[1].type == ExprT.STRING and (p[3].type == ExprT.STRING or p[3].type == ExprT.ARITHMETIC)):
                v = p[1].value + (
                    str(p[3].value) if p[1].type == ExprT.STRING and p[3].type == ExprT.ARITHMETIC else p[3].value
                )
                p[0] = Expression(p[1].type, True, value=v)
            elif p[1].type == ExprT.LIST:
                p[1].value.append(Expression(
                    p[3].type,
                    False,
                    None,
                    p[3].value if p[3].type != ExprT.LIST else [
                        i for i in p[3].value],
                    listItem=True
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
        elif p[2] == '++':
            if p[1].type == ExprT.LIST:
                p[1].value.extend(
                    [i for i in p[3].value] if p[3].type == ExprT.LIST else [
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
    assignexpr : lvalue EQUAL expr
               | lvalue COLON expr
    '''
    if p[2] == ':':
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

                    # assert p[1].symbol != None
                    if p[1].listItem == True:
                        p[0] = afterErrorExpression
                        UI.emitError(
                            G.Error('A list item can not be constant', p.lineno(2)))
                    else:
                        sym.value = val
                        sym.const = True
                        symbolTable.insert(sym)

                        p[0] = Expression(
                            p[1].type,
                            True,
                            # symbol=sym,
                            value=val,
                            listItem=p[1].listItem
                        )
            elif p[1].type.isFlex():
                typ = p[3].type
                sym = p[1].symbol
                val = p[3].value

                # assert p[1].symbol != None
                if p[1].listItem == True:
                    p[0] = afterErrorExpression
                    UI.emitError(
                        G.Error('A list item can not be constant', p.lineno(2)))
                else:
                    sym.value = val
                    sym.const = True
                    if p[3].symbol != None:
                        sym.type = p[3].symbol.type
                    else:
                        sym.type = SymT.flexible if typ.isFlex(
                        ) else SymT(int(typ))
                    symbolTable.insert(sym)

                    p[0] = Expression(
                        typ,
                        True,
                        # symbol=sym,
                        value=val,
                        listItem=p[1].listItem
                    )
            else:
                assert False
        else:
            UI.emitError(G.Error(
                'Assignment to a constant', p.lineno(2)))
            p[0] = afterErrorExpression
    else:   # p[2] == EQUAL
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

                # assert p[1].symbol != None
                if p[1].listItem == True:
                    p[1].value = val
                else:
                    sym.value = val
                    symbolTable.insert(sym)

                    if sym.name == 'GALAKTION_ROOT':
                        os.chdir(val)

                p[0] = Expression(
                    p[1].type,
                    False,
                    # symbol=sym,
                    value=val,
                    listItem=p[1].listItem
                )
        elif p[1].type.isFlex():
            typ = p[3].type
            sym = p[1].symbol
            val = p[3].value

            # assert p[1].symbol != None
            if p[1].listItem == True:
                p[1].type = typ
                p[1].value = val
            else:
                sym.value = val

                if p[3].symbol != None:
                    sym.type = p[3].symbol.type
                else:
                    sym.type = SymT.flexible if typ.isFlex(
                    ) else SymT(int(typ))
                symbolTable.insert(sym)

            p[0] = Expression(
                typ,
                False,
                # symbol=sym,
                value=val,
                listItem=p[1].listItem
            )
        else:
            assert False


def p_augmassignexpr(p):
    '''
    augmassignexpr : lvalue augmassign expr
    '''
    op = p[2][0]
    asgn = p[2][1]

    p[2] = op
    p_expr(p)

    p[3] = p[0]
    p[2] = asgn
    p[0] = None
    p_assignexpr(p)


def p_augmassign(p):
    '''
    augmassign : PLUS_EQUAL
               | MINUS_EQUAL
               | PLUS_PLUS_EQUAL
               | PLUS_COLON
               | MINUS_COLON
               | PLUS_PLUS_COLON
    '''
    p[0] = (p[1][:-1], p[1][-1])
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
            | listdef
            | srcelement XML_ATTRIBUTE
            | const
    '''
    if type(p[1]) == Expression:
        p[0] = p[1]
    else:
        attr = p[1].attrib[p[2]]

        if attr == None:
            p[0] = afterErrorExpression
            UI.emitWarning(
                G.Warning('Failed to find xml attribute', p.lineno(0)))
        else:
            p[0] = Expression(
                ExprT.STRING,
                True,
                None,
                attr
            )


def p_srcelement(p):
    '''
    srcelement : empty
    '''
    p[0] = currentSrcElement


def p_lvalue(p):
    '''
    lvalue : ID
           | decl
           | listitem
    '''
    if type(p[1]) == str:
        if not symbolTable.lookup(p[1]):
            UI.emitError(
                G.Error('"' + p[1] + '" is not declared', p.lineno(1)))
            p[0] = afterErrorExpression
        else:
            sym = symbolTable.get(p[1])

            t = ExprT(sym.type.value)

            if sym.type == SymT.flexible:
                if sym.value == None:
                    t = ExprT.NONE
                elif type(sym.value) == int or type(sym.value) == float:
                    t = ExprT.ARITHMETIC
                elif type(sym.value) == str:
                    t = ExprT.STRING
                elif type(sym.value) == bool:
                    t = ExprT.BOOLEAN
                elif type(sym.value) == list:
                    t = ExprT.LIST
                else:
                    assert False

            p[0] = Expression(
                t,
                sym.const,
                symbol=sym,
                value=sym.value
            )
    else:
        p[0] = p[1]


def p_listdef(p):
    '''
    listdef : BRACE_L exprlist BRACE_R
    '''
    val = []
    for i in p[2]:
        val.append(Expression(
            i.type,
            False,
            None,
            i.value,
            listItem=True
        ))

    p[0] = Expression(
        ExprT.LIST,
        True,
        None,
        val
    )


def p_listitem(p):
    # TODO check this grammar!!
    '''
    listitem : primary index
             | primary slice
    '''
    if p[1].type != ExprT.LIST:
        p[0] = afterErrorExpression
        UI.emitError(
            G.Error('Attempted to index or slice non-list type', p.lineno(1)))
    elif type(p[2]) == G.Index:
        p[0] = p[1].value[p[2].index]
    else:
        p[0] = Expression(
            ExprT.LIST,
            True,
            None,
            p[1].value[p[2].start: p[2].stop]
        )


def p_decl(p):
    '''
    decl : type
         | type ID
    '''
    if len(p) == 2:
        p[0] = Expression(
            ExprT(SymT.fromStr(p[1]).value),
            True,
            symbol=None,
            value=None
        )
    else:
        sym = Symbol(SymT.fromStr(p[1]), False, p[2], None)
        symbolTable.insert(sym)

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
        what2print += str(e) + ' '

    print(what2print[:-1])

    p[0] = None


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
    const : STRING_VALUE
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
    else:   # STRING_VALUE
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

    p[0] = afterErrorExpression

    if p[2] == afterErrorExpression:
        UI.emitError(G.Error('Invalid declaration', p.lineno(2)))
        return

    if not Checks.isTypeGeneratable(t):
        UI.emitError(
            G.Error('Generation for type "' + str(t.name) + '" is not supported' + (' yet' if t == ExprT.xmlfile else ''), p.lineno(1)))
        return

    if Checks.isExtensionInvalid(t, p[3]):
        UI.emitError(G.Error('Invalid extension for type ' +
                     str(p[2].type.name), p.lineno(4)))
        return

    p[0] = None

    if p[2].symbol != None and not p[2].symbol.name.startswith(symbolTable._temporarySymbolPrefix):
        p[2].symbol.value = p[3]
        symbolTable.insert(p[2].symbol)
        p[2].value = p[2].symbol.value

    try:
        if t == ExprT.jsonfile:
            # print('Genarating json file...')
            with open(p[3], 'w') as out:
                out.write(json.dumps(list(G.memory.values()), indent=4))
            # print('ok')
        else:  # t == ExprT.csvfile
            # print('Generating csv file...')

            with open('Galaktion_temp.json', 'w') as out:
                out.write(json.dumps(list(G.memory.values()), indent=4))

            with open('Galaktion_temp.json', 'r') as temp:
                df = pd.read_json(temp)
            df.to_csv(p[3], index=False)  # TODO delimiters

            os.remove('Galaktion_temp.json')

            # print('ok')

            # p[0] = p[2]
    except PermissionError:
        UI.emitWarning(
            G.Warning('Could not generate file "' + p[3] + '"', p.lineno(1)))
        p[0] = None

    if '@flush' in p[4]:
        p[4].remove('@flush')

        G.memory.clear()

    if len(p[4]) > 0:
        UI.emitWarning(
            G.Warning('One or more directives were ignored', p.lineno(4)))


def p_fromstmt(p):
    '''
    fromstmt : FROM lvalue
             | FROM assignexpr
             | FROM lvalue BLOCK_WITH_CODE
             | FROM assignexpr BLOCK_WITH_CODE
    '''
    global activRecs, srcFiles

    if p[2].symbol.type != SymT.xmlfile:
        p[0] = afterErrorExpression
        UI.emitError(
            G.Error('"from" statement excpects a "xmlfile" type', p.lineno(2)))
        return

    if len(p) == 3:
        srcFiles.pop()
        srcFiles.push(p[2].symbol.value)
    else:
        srcFiles.push(p[2].symbol.value)

        activRecs.push(ActivationRecord(
            lexer=newLexer(),
            code=p[3][1:-1]
        ))
        parser.parse(activRecs.top().code, lexer=activRecs.top().lexer)
        activRecs.pop()

        srcFiles.pop()

    p[0] = p[2]


def p_extractstmt(p):
    '''
    extractstmt : EXTRACT expr name drctvs
    '''
    global activRecs

    key = p[3] if p[3] else ''
    val = p[2].value

    if p[4] != None:
        # The set is not empty, so it contains directives.
        # Check which directives exist and each one you find,
        # remove it from the set.
        # This is important for the warning at the last step.

        if '@latlong' in p[4]:
            p[4].remove('@latlong')

            if val:
                if type(val) == list:
                    val = [getLatLong(i) for i in val]
                else:
                    val = getLatLong(val)
        elif '@longlat' in p[4]:
            p[4].remove('@longlat')

            if val:
                if type(val) == list:
                    val = [getLongLat(i) for i in val]
                else:
                    val = getLongLat(val)

    if G.memory.get(srcFiles.top().path, None) == None:
        G.memory[srcFiles.top().path] = {}

    G.memory[srcFiles.top().path][key] = val
    activRecs.top().linkingFiles = []

    # At this point, if a directive is still contained
    # in the set, means that it was not processed (because
    # was invalid for extract statement).
    # If so, throw a warning.
    if len(p[4]) > 0:
        UI.emitWarning(
            G.Warning('One or more directives were ignored', p.lineno(1)))


def p_name(p):
    '''
    name : AS STRING_VALUE
    '''
    p[0] = p[2]


def p_tagslinks(p):
    '''
    tagslinks : tags
              | tagslinks update_files ARROW tags
    '''
    val = []

    for f in activRecs.top().linkingFiles:
        for e in f.elements:
            if e == None:
                pass  # TODO !
            else:
                val.append(e.text)

    if len(val) == 0:
        p[0] = Expression(
            ExprT.NONE,
            True,
            symbol=None,
            value=None
        )
    elif len(val) == 1:
        p[0] = Expression(
            ExprT.STRING,
            True,
            symbol=None,
            value=val[0]
        )
    else:
        p[0] = Expression(
            ExprT.LIST,
            True,
            symbol=None,
            value=val
        )


def p_update_files(p):
    '''
    update_files : empty
    '''
    global activRecs, currentSrcElement

    # if symbolTable.get('GALAKTION_ROOT').value == None:  # TODO
    #     p[0] = afterErrorExpression
    #     UI.emitError(G.Error(
    #         'GALAKTION_ROOT must be initialized'))
    # el
    if linkCode == None:
        p[0] = afterErrorExpression
        UI.emitError(G.Error("Undefined code block for '->'"))
    else:
        r = symbolTable.get('GALAKTION_ROOT').value
        newFiles = []

        for f in activRecs.top().linkingFiles:
            for element in f.elements:
                # if element == None:
                #     UI.emitWarning(
                #         G.Warning(f.path + ': Tag was not found FROM UPDATE_FILES'))
                # else:
                currentSrcElement = element
                path = getPath2File(r)
                currentSrcElement = None

                if path == None:
                    UI.emitWarning(
                        G.Warning(f.path + ': "' + element.tag + '" tag: Failed to link'))
                else:
                    newFiles.append(G.SourceFile(path))

        activRecs.top().linkingFiles = newFiles


def p_tags(p):
    '''
    tags : tag
         | tags SLASH XML_TAG sliceorindex
    '''
    global activRecs

    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + '/' + p[3] + ('' if p[4] == None else str(p[4]))

        for i in range(len(activRecs.top().linkingFiles)):
            newl = []

            for element in activRecs.top().linkingFiles[i].elements:
                l = element.findall(p[3])

                if len(l) == 0:
                    UI.emitWarning(G.Warning(
                        activRecs.top().linkingFiles[i].path + ': Tag "' + p[3] + '" was not found'))
                    continue

                # TODO handle out of range indeces
                if type(p[4]) == G.Index:
                    newl.extend([l[p[4].index]])
                elif type(p[4]) == G.Slice:
                    newl.extend(l[p[4].start: p[4].stop])
                else:  # p[4] == None
                    # newl.append(l[0])
                    newl.extend(l)

            activRecs.top().linkingFiles[i].elements = newl


def p_tag(p):
    '''
    tag : SLASH XML_TAG sliceorindex
    '''
    global activRecs

    p[0] = p[2] + ('' if p[3] == None else str(p[3]))

    try:
        # print('try:', activRecs)
        if len(activRecs.top().linkingFiles) == 0:
            if srcFiles.isEmpty():
                UI.emitError(G.Error('No source file specified', p.lineno(1)))
                p[0] = afterErrorExpression
            else:
                # print(srcFiles.top().path)
                activRecs.top().linkingFiles.append(G.SourceFile(srcFiles.top().path))
    except AttributeError:
        print('except', activRecs)
        pass

    for i in range(len(activRecs.top().linkingFiles)):
        l = ET.parse(
            activRecs.top().linkingFiles[i].path).getroot().findall(p[2])

        if len(l) == 0:
            UI.emitWarning(G.Warning(
                activRecs.top().linkingFiles[i].path + ': Tag "' + p[2] + '" was not found'))
            continue

        # TODO handle out of range indeces
        if type(p[3]) == G.Index:
            l = [l[p[3].index]]
        elif type(p[3]) == G.Slice:
            l = l[p[3].start: p[3].stop]
        else:  # p[3] == None
            # l = [l[0]]
            pass

        activRecs.top().linkingFiles[i].elements = l


def p_sliceorindex(p):
    '''
    sliceorindex : empty
                 | slice
                 | index
    '''
    p[0] = None if p[1] == None else p[1]


def p_slice(p):
    '''
    slice : BRACE_L expr COLON_COLON expr BRACE_R
          | BRACE_L COLON_COLON expr BRACE_R
          | BRACE_L expr COLON_COLON BRACE_R
          | BRACE_L COLON_COLON BRACE_R
    '''
    if len(p) == 4:
        p[0] = G.Slice(G.Index(None), G.Index(None))
    elif len(p) == 5:
        if p[2] == ':':
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


##############   I N I T I A L I Z A T I O N   ###############

symbolTable.insert(Symbol(SymT.folder, False, 'GALAKTION_ROOT', os.getcwd()))


argParser = argparse.ArgumentParser()
argParser.add_argument('input_file')

args = argParser.parse_args()

try:
    inp = open(args.input_file, 'r')
except FileNotFoundError:
    UI.print('Error: Input file not found')
    sys.exit()
parserInput = inp.read()
inp.close()


# parserInput = '''
# generate jsonfile testFile as 'f'
# generate csvfile testFile as 'f'

# generate xmlfile testFile as 'f'
# '''


parser = yacc.yacc()

try:
    parser.parse(parserInput, lexer=activRecs.top().lexer)
except KeyboardInterrupt:
    UI.print('Aborting...')
    sys.exit()

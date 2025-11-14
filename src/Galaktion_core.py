# Galaktion_core.py

from enum import Enum
import random
from pathlib import Path
import sys
import argparse
from lxml.etree import _ElementUnicodeResult


class Stack:
    def __init__(self, initialData: list = []) -> None:
        self._data = initialData

    def size(self):
        return len(self._data)

    def isEmpty(self) -> bool:
        return self.size() == 0

    def top(self):
        return None if self.isEmpty() else self._data[-1]

    def push(self, i):
        # print('pushed what:', str(i))
        self._data.append(i)

    def pop(self):
        # print('popped what:', self.top())
        return None if self.isEmpty() else self._data.pop()

    def flush(self):
        while not self.isEmpty():
            self.pop()

    def __str__(self) -> str:
        return str(self._data)


memory = {}


class SymbolType(Enum):
    folder = 0
    xmlfile = 1
    jsonfile = 2
    csvfile = 3
    flexible = 4

    @classmethod
    def byName(self, n: str):
        for t in SymbolType:
            if t.name == n:
                return t
        assert False

    def __str__(self) -> str:
        # if self.name == SymbolType.func.name else super().__str__()
        return SymbolType.flexible.name


class Symbol:
    def __init__(self, type: SymbolType, const: bool, name: str, value) -> None:
        self.type = type
        self.const = const
        self.name = name
        self.value = value

    def __str__(self) -> str:
        return 'Symbol: ' + str(self.type.name) + (' const' if self.const else ' var') + ' ' + self.name + ' ' + str(self.value)


class SymbolTable:
    hiddenSymbolPrefix = '\0'

    @classmethod
    def isHiddenSymbol(self, sym: Symbol):
        return sym.name.startswith(self.hiddenSymbolPrefix)

    def __init__(self) -> None:
        self._symbolTable = {}

    def hidden(self) -> str:
        while True:
            ret = SymbolTable.hiddenSymbolPrefix + \
                str(random.getrandbits(10))
            if not self.lookup(ret):
                break

        return ret

    def lookup(self, name: str) -> bool:
        return self._symbolTable.get(name, None) != None

    def get(self, name: str):
        return self._symbolTable.get(name, None)

    def update(self, sym: Symbol) -> Symbol:
        '''Inserts or updates symbol'''

        self._symbolTable[sym.name] = sym

        assert self.lookup(sym.name)
        return sym

    def delete(self, name: str) -> Symbol:
        assert self.lookup(name)
        return self._symbolTable.pop(name)

    def __str__(self) -> str:
        ret = ''
        for i in self._symbolTable.values():
            ret += str(i) + '\n'
        return ret[:-1]


class ExpressionType(Enum):
    folder = SymbolType.folder.value
    xmlfile = SymbolType.xmlfile.value
    jsonfile = SymbolType.jsonfile.value
    csvfile = SymbolType.csvfile.value

    # func = SymbolType.func.value

    STRING = 4
    NONE = 5
    ARITHMETIC = 6
    BOOLEAN = 7

    ARRAY = 8

    @classmethod
    def byValue(self, v):
        'ATTENTION: It returns ExpressionType.STRING for all string-based types'

        if v == None:
            return self.NONE

        if type(v) in {int, float}:
            return self.ARITHMETIC

        if type(v) in {str, _ElementUnicodeResult}:
            return self.STRING

        if type(v) == bool:
            return self.BOOLEAN

        if type(v) == list:
            return self.ARRAY

        print('Unhandled type:', type(v))
        assert False

    @classmethod
    def bySymbol(self, sym: Symbol):
        if sym.type == SymbolType.flexible:
            return ExpressionType.byValue(sym.value)

        return ExpressionType(sym.type.value)

    def isStrict(self):
        return self.value <= self.csvfile.value

    def isFlex(self):
        return self.value >= self.STRING.value


class Expression:
    def __init__(self, type: ExpressionType, const: bool = False, symbol: Symbol = None, value=None, arrayItem=False, indexed=False) -> None:
        self.type = type
        self.const = const
        self.symbol = symbol
        self.value = value
        self.arrayItem = arrayItem
        self.indexed = indexed

    def print(self):
        print('Expression:', self.type.name, 'const' if self.const else 'var', 'sym=' +
              ('none' if self.symbol == None else self.symbol.name), 'val=' + str(self.value))

    def __bool__(self):
        if self.symbol != None:
            return bool(self.symbol.value)
        return bool(self.value)

    def __str__(self) -> str:
        if self.value == None:
            return 'none'

        if type(self.value) == bool and self.value == True:
            return 'true'

        if type(self.value) == bool and self.value == False:
            return 'false'

        if self.type == ExpressionType.ARRAY:
            ret = '['
            for i in self.value:
                ret += ' '
                if i.type.value <= ExpressionType.STRING.value:
                    ret += "'" + str(i) + "'"
                else:
                    ret += str(i)
                ret += ','
            ret = (ret[:-1]+' ' if ret.endswith(',') else ret) + ']'

            return ret

        return str(self.value)


class Index:
    def __init__(self, idx) -> None:

        self.index_galaktion = idx

        if idx == None:
            self.index_galaktion = ''
            self.index_python = None
            self.index_xpath = ''
        elif idx == 0:
            self.index_python = None
            self.index_xpath = '0'
        elif idx < 0:
            self.index_python = idx
            self.index_xpath = f'last(){str(idx)}+1'
        else:
            self.index_python = idx-1
            self.index_xpath = str(idx)

    def __str__(self) -> str:
        return '[' + str(self.index_galaktion) + ']'


class Slice:
    def __init__(self, startidx: Index, stopidx: Index) -> None:
        self.start_galaktion = startidx.index_galaktion
        self.start_python = startidx.index_python

        self.stop_galaktion = stopidx.index_galaktion
        self.stop_python = stopidx.index_python

        if type(self.stop_python) == int:
            self.stop_python += 1
            if self.stop_python == 0:
                self.stop_python = None

        strt = ('position() >= ' +
                startidx.index_xpath) if startidx.index_xpath else ''
        mid = ' and ' if startidx.index_xpath and stopidx.index_xpath else ''
        stp = ('position() <= ' + stopidx.index_xpath) if stopidx.index_xpath else ''
        self.start_stop_xpath = strt + mid + stp

    def __str__(self) -> str:
        return '[' + str(self.start_galaktion) + '~' + str(self.stop_galaktion) + ']'


class Directives:
    prefix: str = '#'
    postfix: str = ''

    latlong: str = prefix + 'latlong' + postfix
    longlat: str = prefix + 'longlat' + postfix
    subfolders: str = prefix + 'subfolders' + postfix
    filelocation: str = prefix + 'filelocation' + postfix
    filename: str = prefix + 'filename' + postfix
    flush: str = prefix + 'flush' + postfix


class Error:
    def __init__(self, msg='-', line=None) -> None:
        self.message = msg
        self.line = line

    def __str__(self):
        lineStr = ('' if self.line == None else (
            'at line ' + str(self.line) + ': '))
        return 'Error: ' + lineStr + self.message


class Warning:
    def __init__(self, msg='-', line=None) -> None:
        self.message = msg
        self.line = line

    def __str__(self):
        lineStr = ('' if self.line == None else (
            'at line ' + str(self.line) + ': '))
        return 'Warning: ' + lineStr + self.message


class Checks:
    filetype2extensions = {
        SymbolType.xmlfile: ['.xml', '.html', '.htm'],
        SymbolType.jsonfile: ['.json'],
        SymbolType.csvfile: ['.csv']
    }

    generationSupportedTypes = {
        ExpressionType.jsonfile,
        ExpressionType.csvfile
    }

    def __init__(self) -> None:
        pass

    @classmethod
    def isFile(self, path: str):
        return Path(path).is_file()

    @classmethod
    def isFolder(self, path: str):
        return Path(path).is_dir()

    @classmethod
    def isExtensionValid(self, fileType: SymbolType, fileName: str):
        return not Checks.isExtensionInvalid(fileType, fileName)

    @classmethod
    def isExtensionInvalid(self, fileType: SymbolType, fileName: str):
        # in case ExpressioType was given - convert anyway
        fileType = SymbolType(fileType.value)

        for validExt in Checks.filetype2extensions[fileType]:
            ext = fileName[-len(validExt):]
            if ext == validExt:
                return False
        return True

    @classmethod
    def isXMLfile(self, fileName: str):
        return Checks.isFile(fileName) and Checks.isExtensionValid(SymbolType.xmlfile, fileName)

    @classmethod
    def isJSONfile(self, fileName: str):
        return Checks.isFile(fileName) and Checks.isExtensionValid(SymbolType.jsonfile, fileName)

    @classmethod
    def isCSVfile(self, fileName: str):
        return Checks.isFile(fileName) and Checks.isExtensionValid(SymbolType.csvfile, fileName)

    @classmethod
    def isTypeGeneratable(self, t):
        return t in self.generationSupportedTypes


class UserInterface:
    def __init__(self):
        self.errors: list[Error] = []
        self.warnings: list[Warning] = []

        argParser = argparse.ArgumentParser()
        argParser.add_argument('source_code_file')
        args = argParser.parse_args()

        self.userSourceFile = args.source_code_file
        self.terminateOnError = True
        self.printErrorImmediately = True
        self.printWarningImmediately = True

    def emitError(self, error: Error):
        self.errors.append(error)

        if self.printErrorImmediately:
            print(self.errors.pop(), flush=True)
        if self.terminateOnError:
            sys.exit()

    def emitWarning(self, warning: Warning):
        self.warnings.append(warning)

        if self.printWarningImmediately:
            print(self.warnings.pop(), flush=True)

    def printErrors(self):
        for e in self.errors:
            print(e)

    def printWarnings(self):
        for w in self.warnings:
            print(w)

    def print(self, mywhat):
        print(mywhat, flush=True)

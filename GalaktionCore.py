from enum import Enum
import random
from stack import Stack
import xml.etree.ElementTree as ET
from pathlib import Path
from sys import exit


class State(Enum):
    GENERAL = 0
    FOREACH_REC = 1
    FOREACH_EXEC = 2


state: State = State.GENERAL


class ActivationRecord:
    def __init__(self, lexer, code: str = '') -> None:
        # self.srcFiles = SourceFileManager()
        self.linkingFiles = []
        # self.state = State.GENERAL
        self.code: str = code
        self.lexer = lexer


class ActivationRecordManager:
    def __init__(self) -> None:
        self._recs = []

    def size(self):
        return len(self._recs)

    def isEmpty(self):
        return self.size() == 0

    def top(self):
        return None if self.isEmpty() else self._recs[-1]

    def push(self, rec: ActivationRecord):
        self._recs.append(rec)

    def pop(self):
        return None if self.isEmpty() else self._recs.pop()

    def flush(self):
        while not self.isEmpty():
            self.pop()

    def __str__(self) -> str:
        return str(self._recs)


memory = {}  # TODO !!!!!!!!!!!!!!!


class SymbolType(Enum):
    folder = 0
    xmlfile = 1
    jsonfile = 2
    csvfile = 3
    flexible = 4

    # func = -1

    @classmethod
    def fromStr(self, s: str):
        for t in SymbolType:
            if t.name == s:
                return t
        assert False

    def __str__(self) -> str:
        return SymbolType.flexible.name if self.name == SymbolType.func.name else super().__str__()


class Symbol:
    def __init__(self, type: SymbolType, const: bool, name: str, value) -> None:
        self.type = type
        self.const = const
        self.name = name
        self.value = value

    def __str__(self) -> str:
        return 'Symbol: ' + str(self.type.name) + (' const' if self.const else ' var') + ' ' + self.name + ' ' + str(self.value)


class SymbolTable:
    def __init__(self) -> None:
        self._symbolTable = {}
        self._temporarySymbolPrefix = '$'

    def temporary(self) -> str:
        while True:
            ret = self._temporarySymbolPrefix + str(random.getrandbits(10))
            if not self.lookup(ret):
                break

        return ret

    def isTemporarySymbol(self, sym: Symbol):
        return sym.name.startswith(self._temporarySymbolPrefix)

    def lookup(self, name: str) -> bool:
        return self._symbolTable.get(name, None) != None

    def get(self, name: str):
        if self.lookup(name):
            return self._symbolTable.get(name)

        return None

    def insert(self, sym: Symbol) -> Symbol:
        '''Inserts or updates symbol'''
        if state != State.FOREACH_EXEC:
            assert (not self.lookup(id))

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


class SourceFile:
    def __init__(self, path: str, flag: bool = None, ellist: list = []) -> None:
        self.path = path
        self.flag = flag
        self.elements = ellist

    # def __str__(self) -> str:
    #     return self.path


class SourceFileManager(Stack):
    def push(self, path: str, popAtBlockExit: bool = False):
        super().push(SourceFile(path, popAtBlockExit))

    def atBlockExit(self):
        t: SourceFile = self.top()
        if t.flag:
            return self.pop()


# class XMLTag:
#     def __init__(self, tag: str, filePath: str) -> None:
#         self.tag = tag
#         self.file = filePath


# class ExpressionType(Enum):
#     ASSIGN = 0

#     VAR_STRING = 1
#     VAR_ARITHMETIC = 2
#     VAR_FOLDER = 3
#     VAR_XMLFILE = 4
#     VAR_JSONFILE = 5
#     VAR_CSVFILE = 6

#     CONST_STRING = 7
#     CONST_ARITHMETIC = 8
#     CONST_FOLDER = 9
#     CONST_XMLFILE = 10
#     CONST_JSONFILE = 11
#     CONST_CSVFILE = 12

#     BOOLEAN = 13
#     ARITHMETIC = 14
#     # CONST = 15


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

    LIST = 8

    def isStrict(self):
        return self.value <= self.csvfile.value

    def isFlex(self):
        return self.value >= self.STRING.value


depth = -1
spaces = '   '


class Expression:
    def __init__(self, type: ExpressionType, const: bool = False, symbol: Symbol = None, value=None, listItem=False) -> None:
        self.type = type
        self.const = const
        self.symbol = symbol
        self.value = value
        self.listItem = listItem

    def print(self):
        print('Expression:', self.type.name, 'const' if self.const else 'var', 'sym=' +
              ('none' if self.symbol == None else self.symbol.name), 'val=' + str(self.value))

    def __bool__(self):
        if self.symbol != None:
            return bool(self.symbol.value)
        return bool(self.value)

    def __str__(self) -> str:
        global depth

        if self.value == None:
            return 'none'

        if type(self.value) == bool and self.value == True:
            return 'true'

        if type(self.value) == bool and self.value == False:
            return 'false'

        if self.type == ExpressionType.LIST:
            # single line print:
            ret = '[ '
            for i in self.value:
                if i.type.value <= ExpressionType.STRING.value:
                    ret += "'" + str(i) + "'"
                else:
                    ret += str(i)
                ret += ', '
            ret = ret[:-2] + ' ]'

            return ret

            # multi line print:
            # depth += 1
            # ret = spaces*(depth-1) + '[\n'
            # for i in self.value:
            #     ret += spaces*(depth+1)
            #     if i.type.value <= ExpressionType.STRING.value:
            #         ret += "'" + str(i) + "'"
            #     else:
            #         ret += str(i)
            #     ret += ',\n'
            # ret = ret[:-2] + '\n' + spaces*depth + ']'

            # depth -= 1

            # return ret

        return str(self.value)


class Index:
    def __init__(self, idx) -> None:
        self.index = idx

    def __str__(self) -> str:
        return '[' + str(self.index) + ']'


class Slice:
    def __init__(self, startidx: Index, stopidx: Index) -> None:
        self.start = startidx.index
        self.stop = stopidx.index

    def __str__(self) -> str:
        return '[' + str(self.start) + ':' + str(self.stop) + ']'


class Declaration:
    def __init__(self, type, id) -> None:
        self.type = type
        self.id = id


class Error:
    def __init__(self, msg='-', line='*') -> None:
        self.message = msg
        self.line = line

    def __str__(self):
        return 'Error: at line ' + str(self.line) + ': ' + self.message


class Warning:
    def __init__(self, msg='-', line='*') -> None:
        self.message = msg
        self.line = line

    def __str__(self):
        return 'Warning: at line ' + str(self.line) + ': ' + self.message


class Checks:
    filetype2extensions = {
        SymbolType.xmlfile: ['.xml', '.html', '.htm'],
        SymbolType.jsonfile: ['.json'],
        SymbolType.csvfile: ['.csv']
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


class UserInterface:
    def __init__(self, terminateOnError=True):
        self.errors: list[Error] = []
        self.warnings: list[Warning] = []
        self.terminateOnError = terminateOnError

    def emitError(self, error: Error):
        self.errors.append(error)

        if self.terminateOnError:
            print(self.errors[-1])
            exit()

    def emitWarning(self, warning: Warning):
        self.warnings.append(warning)
        print(self.warnings[-1])

    def print(self, mywhat):
        print(mywhat)

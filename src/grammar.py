###################### Grammar #############################


#   Precedence from low to high
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


def p_linkexec(p):
    '''
    linkexec : VERTICAL_BAR expr VERTICAL_BAR drctvs COMMA VERTICAL_BAR expr VERTICAL_BAR drctvs
    '''


def p_ifstmt(p):
    '''
    ifstmt : IF expr BLOCK_WITH_CODE
           | IF expr BLOCK_WITH_CODE ELSE BLOCK_WITH_CODE
           | IF expr BLOCK_WITH_CODE ELSE empty ifstmt
    '''


def p_foreachstmt(p):
    '''
    foreachstmt : FOR_EACH decl IN expr BLOCK_WITH_CODE drctvs
    '''


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


def p_assignexpr(p):
    '''
    assignexpr : lvalue EQUAL expr
               | lvalue COLON expr
    '''


def p_augmassignexpr(p):
    '''
    augmassignexpr : lvalue augmassign expr
    '''


def p_augmassign(p):
    '''
    augmassign : PLUS_EQUAL
               | MINUS_EQUAL
               | PLUS_PLUS_EQUAL
               | PLUS_COLON
               | MINUS_COLON
               | PLUS_PLUS_COLON
    '''


def p_term(p):
    '''
    term : PAR_L expr PAR_R
         | MINUS expr %prec UMINUS
         | PLUS expr %prec UPLUS
         | NOT expr
         | primary
    '''


def p_primary(p):
    '''
    primary : lvalue
            | listdef
            | srcelement XML_ATTRIBUTE
            | const
    '''


def p_srcelement(p):
    '''
    srcelement : empty
    '''


def p_lvalue(p):
    '''
    lvalue : ID
           | decl
           | listitem
    '''


def p_listdef(p):
    '''
    listdef : BRACE_L exprlist BRACE_R
    '''


def p_listitem(p):
    # TODO check this grammar!!
    '''
    listitem : primary index
             | primary slice
    '''


def p_decl(p):
    '''
    decl : type
         | type ID
    '''


def p_type(p):
    '''
    type : stricttype
         | flexible
    '''


def p_stricttype(p):
    '''
    stricttype : folder
               | filetype
    '''


def p_filetype(p):
    '''
    filetype : xmlfile
             | jsonfile
             | csvfile
    '''


def p_printstmt(p):
    '''
    printstmt : PRINT exprlist
    '''


def p_exprlist(p):
    '''
    exprlist : empty
             | expr
             | exprlist COMMA expr
    '''


def p_const(p):
    '''
    const : STRING_VALUE
          | number
          | TRUE
          | FALSE
          | NONE
    '''


def p_number(p):
    '''
    number : INTEGER
           | REAL
    '''


def p_drctvs(p):
    '''
    drctvs : empty
           | DIRECTIVE
           | drctvs DIRECTIVE
    '''


def p_generatestmt(p):
    '''
    generatestmt : GENERATE decl name drctvs
    '''


def p_fromstmt(p):
    '''
    fromstmt : FROM lvalue
             | FROM assignexpr
             | FROM lvalue BLOCK_WITH_CODE
             | FROM assignexpr BLOCK_WITH_CODE
    '''


def p_extractstmt(p):
    '''
    extractstmt : EXTRACT expr name drctvs
    '''


def p_name(p):
    '''
    name : AS STRING_VALUE
    '''


def p_tagslinks(p):
    '''
    tagslinks : tags
              | tagslinks update_files ARROW tags
    '''


def p_update_files(p):
    '''
    update_files : empty
    '''


def p_tags(p):
    '''
    tags : tag
         | tags SLASH XML_TAG sliceorindex
    '''


def p_tag(p):
    '''
    tag : SLASH XML_TAG sliceorindex
    '''


def p_sliceorindex(p):
    '''
    sliceorindex : empty
                 | slice
                 | index
    '''


def p_slice(p):
    '''
    slice : BRACE_L expr COLON_COLON expr BRACE_R
          | BRACE_L COLON_COLON expr BRACE_R
          | BRACE_L expr COLON_COLON BRACE_R
          | BRACE_L COLON_COLON BRACE_R
    '''


def p_index(p):
    '''
    index : BRACE_L expr BRACE_R
    '''

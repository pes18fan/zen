" Vim syntax file
" Language: zen
" Maintainer: pes18fan
" Latest Revision: 2026

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword zenKeyword use var val echo func return pub exit discard in try catch
syn keyword zenConditional if else switch
syn keyword zenRepeat while for break continue
syn keyword zenRepeat for
syn keyword zenLogicalOperator and or not

" Built-in Constants and Values
syn keyword zenBoolean true false
syn keyword zenConstant it nil this super

" Built-in Functions
syn keyword zenBuiltin puts gets assert panic len typeof str parse copy dirname filename

" Strings, both multiline and single-line
syn region zenString start=/"/ skip=/\\./ end=/"/ contains=zenEscape
syn region zenString start=/'/ skip=/\\./ end=/'/ contains=zenEscape
syn region zenString start=/\\\\/ end=/\n/
syn match  zenEscape /\\./ contained

" Operators
syn match zenOperator "+"
syn match zenOperator "-"
syn match zenOperator "\*"
syn match zenOperator "/"
syn match zenOperator "%"
syn match zenOperator "\.\."
syn match zenOperator "=="
syn match zenOperator "!="
syn match zenOperator "<="
syn match zenOperator ">="
syn match zenOperator "<"
syn match zenOperator ">"
syn match zenOperator "="
syn match zenOperator "|>"
syn match zenOperator "=>"

" Numbers (Matches integers and floats)
syn match zenNumber "\v<\d+(\.\d+)?>"

" Comments and todo markers
syn keyword zenTodo TODO FIXME NOTE HACK contained
syn match zenComment "\/\/.*$" contains=zenTodo,@Spell

" Map our custom groups to Vim's standard highlight groups
hi def link zenKeyword         Keyword
hi def link zenConditional     Conditional
hi def link zenRepeat          Repeat
hi def link zenBoolean         Boolean
hi def link zenConstant        Constant
hi def link zenBuiltin         Function
hi def link zenFuncDecl        Function
hi def link zenString          String
hi def link zenNumber          Number
hi def link zenComment         Comment
hi def link zenTodo            Todo
hi def link zenOperator        Operator
hi def link zenLogicalOperator Operator

let b:current_syntax = "zen"

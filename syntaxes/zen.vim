" Vim syntax file
" Language: zen
" Maintainer: pes18fan
" Latest Revision: 2026

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword zenKeyword use var val print func return pub exit
syn keyword zenConditional if else switch
syn keyword zenRepeat while for break continue
syn keyword zenRepeat for in
syn keyword zenLogicalOperator and or not
syn keyword zenKeyword class

" Built-in Constants and Values
syn keyword zenBoolean true false
syn keyword zenConstant it nil this super

" Built-in Functions
syn keyword zenBuiltin puts gets len typeof str parse copy dirname filename

" Strings (Matches "..." and handles escaped quotes \")
syn region zenString start=/"/ skip=/\\./ end=/"/
syn region zenString start=/'/ skip=/\\./ end=/'/

" Numbers (Matches integers and floats)
syn match zenNumber "\v<\d+(\.\d+)?>\ze"

" Operators (Matches math, comparison, pipes, and arrows)
syn match zenOperator "\v\+"
syn match zenOperator "\v-"
syn match zenOperator "\v\*"
syn match zenOperator "\v/"
syn match zenOperator "\v\%"
syn match zenOperator "\v\=\="
syn match zenOperator "\v\!\="
syn match zenOperator "\v\<="
syn match zenOperator "\v\>="
syn match zenOperator "\v\<"
syn match zenOperator "\v\>"
syn match zenOperator "\v\="
syn match zenOperator "\v\|\>"
syn match zenOperator "\v\=\>"

" Comments (Matches // to the end of the line)
syn match zenComment "\/\/.*$" contains=@Spell

" Map our custom groups to Vim's standard highlight groups
hi def link zenKeyword         Keyword
hi def link zenConditional     Conditional
hi def link zenRepeat          Repeat
hi def link zenBoolean         Boolean
hi def link zenConstant        Constant
hi def link zenBuiltin         Function
hi def link zenString          String
hi def link zenNumber          Number
hi def link zenComment         Comment
hi def link zenOperator        Operator
hi def link zenLogicalOperator Operator

let b:current_syntax = "zen"

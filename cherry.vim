" Vim syntax file
" Language: Cherry
" Mainter: Hannes Lohmander
" Latest revision: 24 April 2017

if exists("b:current_syntax")
  finish
end

" Keywords
syn keyword cherryImport  module runs from fromjs import
syn keyword cherryKeyword infix infixl infixr

" Types
syn match cherryType     '^[a-zA-Z0-9]\+' nextgroup=cherryTypeSign
syn match cherryTypeSign '::.*$' contains=cherryTypeCon
syn match cherryTypeCon  '[A-Z][a-zA-Z0-9]*' contained containedin=cherryTypeSign

" Constants
syn keyword cherryBool True False

syn match cherryNumber '\d\+'
syn match cherryNumber '[+-]\d\+'

syn region cherryString start='"' end='"'

" Operators
syn match cherryOp '[\$\|\>\<\?\^\&\~\+\-]\+'

" Functions
syn match cherryFunc '^\w.\+=' 
syn match cherryFuncParam '[a-zA-Z0-9_]\+' contained containedin=cherryFunc
syn match cherryFuncName '^[a-zA-Z0-9_]\+' contained containedin=cherryFunc

hi def link cherryImport    Include
hi def link cherryKeyword   Keyword
hi def link cherryType      Define
hi def link cherryTypeCon   Type
hi def link cherryFuncParam Special
hi def link cherryFuncName  Function
hi def link cherryBool      Constant
hi def link cherryNumber    Constant
hi def link cherryString    Constant
hi def link cherryOp        Operator

let b:current_syntax = "cherry"

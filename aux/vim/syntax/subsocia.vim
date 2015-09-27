" Vim syntax file
" Language: Subsocia Schema
" Maintainer: Petter A. Urkedal

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match sssPathOperator '[+-/={}]'
syn match sssOperator '[():<>]'
syn match sssPathOperator ':[a-zA-Z]\@='
syn match sssOperator '->'

syn match sssIntroducer '^:!\s' skipwhite nextgroup=sssAtCreateName
syn match sssIntroducer '^:[*@]\s' skipwhite nextgroup=sssTypeX
syn match sssIntroducer '^\*\s' skipwhite nextgroup=sssType
syn match sssIntroducer '^\(@\|@?\)\s'
syn match sssModifier '\([!?]\|?!\)\s\@='
syn match sssModifier '[!?]<\s\@='
syn match sssModifier '%[a-zA-Z0-9_]\+'
syn match sssType contained '[a-zA-Z0-9_]\+'
syn match sssTypeX contained '[a-zA-Z0-9_]\+' skipwhite nextgroup=sssTypeModifier
syn match sssTypeY contained '[a-zA-Z0-9_]\+' skipwhite nextgroup=sssTypeAttr
syn match sssTypeAttr contained '/[a-zA-Z0-9_:]\+'
  \ skipwhite nextgroup=sssTypeModifier
syn match sssTypeModifier contained '[!?]' skipwhite nextgroup=sssTypeY
syn match sssTypeModifier contained '[!?]<' skipwhite nextgroup=sssTypeX
syn match sssAtCreateName contained '[a-zA-Z0-9_:]\+' nextgroup=sssAtCreateOp
syn match sssAtCreateOp contained '=' nextgroup=sssAtCreateType
syn keyword sssAtCreateType contained string int bool

syn region sssString start='"' skip='\\.' end='"'
syn match sssComment '#\(\s.*\)\?$'

hi def link sssIntroducer Conditional
hi def link sssModifier Preproc
hi def link sssOperator Operator
hi def link sssPathOperator Special
hi def link sssType Type
hi def link sssTypeX sssType
hi def link sssTypeY sssType
hi def link sssTypeModifier sssModifier
hi def link sssString String
hi def link sssComment Comment
hi def link sssAttribute Function

hi def link sssTypeAttr sssAttribute

hi def link sssAtCreateName sssAttribute
hi def link sssAtCreateOp sssPathOperator
hi def link sssAtCreateType sssType

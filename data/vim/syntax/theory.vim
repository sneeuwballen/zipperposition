" Vim syntax file
" Language:		Theory Description
" Filename extensions:	*.theory
" Maintainer:		Simon Cruanes
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
	syntax clear
"elseif exists("b:current_syntax")
"	finish
endif

if version >= 600
  setlocal iskeyword=@,48-57,_,-,!,#,$,%
else
  set iskeyword=@,48-57,_,-,!,#,$,%
endif

" tabs = evil
set expandtab

syn case match

syn keyword     theoryRole        val axiom theory holds
syn keyword     theoryRole        lemma rewrite pre_rewrite

syn match       theoryDollar      "\<\$\w+\>"
syn match       theoryDollarDollar "\<\$\$\w+\>"

syn match       theoryQuote       "'[^"]*'"
syn match       theoryDoubleQuote "\"[^"]*\""

syn match       theoryConnective  ":"
syn match       theoryConnective  "|"
syn match       theoryConnective  "&"
syn match       theoryConnective  "="
syn match       theoryConnective  "=>"
syn match       theoryConnective  "-->"
syn match       theoryConnective  "->"
syn match       theoryConnective  "<-"
syn match       theoryConnective  "<="
syn match       theoryConnective  "<=>"
syn match       theoryConnective  "<\~>"
syn match       theoryConnective  "!"
syn match       theoryConnective  "!>"
syn match       theoryConnective  "!="
syn match       theoryConnective  "\~"
syn match       theoryConnective  "\."
syn match       theoryConnective  "\*"
syn match       theoryConnective  ">"

syn match       theoryVar         "\<\u\w*\>"

syn match       theoryMetaVar     "?\w*\>"

" errors

"syn match       theoryBraceError  "\]"
"syn match       theoryParenError  ")"

" delimiters

syn region      theoryParen       matchgroup=theoryDelim start="("  end=")" contains=ALLBUT,theoryParenError keepend contained
syn region      theoryParen       matchgroup=theoryDelim start="\[" end="\]" contains=ALLBUT,theoryBraceError keepend contained

syn keyword	theoryTodo	containedin=theoryComment TODO FIXME BUG FIX

syn region      theoryComment	start=+/\*+ end=+\*/+ contains=theoryTodo
syn match       theoryComment     contains=TODO +%.*+

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_theory_syntax_inits")
  if version < 508
    let did_theory_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink theoryTodo               Todo

  HiLink theoryComment		Comment
  HiLink theoryComment		Comment

  HiLink theoryRole               Keyword
  HiLink theoryConnective         Keyword
  HiLink theoryDelim              Delimiter

  HiLink theoryDollar             String
  HiLink theoryDollarDollar       String
  HiLink theoryQuote              String
  HiLink theoryDoubleQuote        String

  HiLink theoryVar                Constant
  HiLink theoryMetaVar            Special

  HiLink theoryBraceError         Error
  HiLink theoryParenError         Error

  delcommand HiLink
end

let b:current_syntax = "theory"

" vim: ts=8 sw=8

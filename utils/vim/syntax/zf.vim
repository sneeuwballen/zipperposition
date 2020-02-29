" Vim syntax file
" Language:		ZF
" Filename extensions:	*.zf
" Maintainer:		Simon Cruanes (heavily inspired from progress.vim file)
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

syn match       zfQuoted      "\"[^"]*\""
syn match       zfQuoted      "'[^']*'"

syn keyword     zfRole        val assert goal data def rewrite include lemma where

syn match       zfBuiltin     "\<type\>"
syn match       zfBuiltin     "\<prop\>"
syn match       zfBuiltin     "\<int\>"
syn match       zfBuiltin     "\<true\>"
syn match       zfBuiltin     "\<false\>"

syn match       zfConnective  "\."
syn match       zfConnective  ":"
syn match       zfConnective  ":="
syn match       zfConnective  "="
syn match       zfConnective  "!="
syn match       zfConnective  "->"
syn match       zfConnective  "=>"
syn match       zfConnective  "<=>"
syn match       zfConnective  "\<and\>"
syn match       zfConnective  "&&"
syn match       zfConnective  "||"
syn match       zfConnective  "|"
syn match       zfConnective  "\~"
syn match       zfConnective  "+"
syn match       zfConnective  "-"
syn match       zfConnective  "*"
syn match       zfConnective  "<"
syn match       zfConnective  "<="
syn match       zfConnective  ">="
syn match       zfConnective  ">"
syn match       zfConnective  "\<forall\>"
syn match       zfConnective  "\<exists\>"
syn match       zfConnective  "\<fun\>"
syn match       zfConnective  "\<if\>"
syn match       zfConnective  "\<then\>"
syn match       zfConnective  "\<else\>"
syn match       zfConnective  "\<let\>"
syn match       zfConnective  "\<in\>"
syn match       zfConnective  "\<pi\>"
syn match       zfConnective  "\<match\>"
syn match       zfConnective  "\<with\>"
syn match       zfConnective  "\<end\>"

syn match       zfVar         "\<\u\w*\>"

" delimiters

syn region      zfParen       matchgroup=zfDelim start="("  end=")" contains=ALLBUT,zfParenError keepend contained

syn keyword	zfTodo	contained TODO BUG FIX FIXME NOTE

syn match       zfComment     +#.*+ contains=zfTodo

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_zf_syntax_inits")
  if version < 508
    let did_zf_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink zfTodo               Todo

  HiLink zfComment		Comment
  HiLink zfComment		Comment

  HiLink zfRole               Keyword
  HiLink zfLogic              Keyword
  HiLink zfConnective         Keyword
  HiLink zfDelim              Delimiter

  HiLink zfBuiltin            Special

  HiLink zfDollar             String
  HiLink zfDollarDollar       String
  HiLink zfQuote              String
  HiLink zfDoubleQuote        String

  HiLink zfVar                Constant

  HiLink zfNum                Number

  HiLink zfBraceError         Error
  HiLink zfParenError         Error

  HiLink zfQuoted             String

  delcommand HiLink
end

let b:current_syntax = "zf"

" vim: ts=8 sw=8

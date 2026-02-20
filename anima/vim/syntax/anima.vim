if exists("b:current_syntax")
  finish
endif

syn clear

syn match animaIdentifier /\\[:!\u2020\u266d]*[a-zA-Z0-9\-,.*`~$#"_@]*[:!]*/
syn match animaIdentifier /♯[a-zA-Z0-9]+/
hi link animaIdentifier Label

syn region animaComment start=/\\--/ end=/$/ contains=@Spell
hi link animaComment Comment

syn match animaIdentifier /\\---/
syn match animaIdentifier /\\\.\.\./ conceal cchar=…

syn match animaPunctuation /\v[{}]/
hi link animaPunctuation Operator

syn match animaKeyword /\\def[:!]*/
syn match animaKeyword /\\return[:!]*/
syn match animaKeyword /\\arg[:!]*/
syn match animaKeyword /\\once[:!]*/
syn match animaKeyword /\\flag[:!]*/
syn match animaKeyword /\\many[:!]*/
syn match animaKeyword /\\rest[:!]*/
syn match animaKeyword /\\quoted[:!]*/
syn match animaKeyword /\\hoisted[:!]*/
syn match animaIdentifier /\\deflink[:!]*/
hi link animaKeyword Keyword

" let b:current_syntax = "anima"

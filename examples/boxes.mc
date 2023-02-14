let bring = \x. let box u <- x in u
in
let a = bring (box 5)
in
a

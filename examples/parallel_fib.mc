let bring = \x. let box u <- x in u
in
let rapp = \f. \x.
  let g = bring f in
  let y = bring x in
  box (g y)
in
rapp (*(box (\x. x + 1)) (box 4)*)

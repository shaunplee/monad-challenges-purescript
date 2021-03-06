// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1857287686"] = [{"values":[{"sourceSpan":{"start":[255,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/FoldableWithIndex.purs","end":[261,7]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"anyWithIndex","moduleName":"Data.FoldableWithIndex","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["i",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","FoldableWithIndex"],"FoldableWithIndex"],"constraintArgs":[{"tag":"TypeVar","contents":"i"},{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","HeytingAlgebra"],"HeytingAlgebra"],"constraintArgs":[{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"b"}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`anyWithIndex f` is the same as `or <<< mapWithIndex f`; map a function over the\nstructure, and then get the disjunction of the results.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[244,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/FoldableWithIndex.purs","end":[250,7]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"allWithIndex","moduleName":"Data.FoldableWithIndex","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["i",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","FoldableWithIndex"],"FoldableWithIndex"],"constraintArgs":[{"tag":"TypeVar","contents":"i"},{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","HeytingAlgebra"],"HeytingAlgebra"],"constraintArgs":[{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"b"}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`allWithIndex f` is the same as `and <<< mapWithIndex f`; map a function over the\nstructure, and then get the conjunction of the results.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[101,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/FoldableWithIndex.purs","end":[107,7]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"foldMapWithIndexDefaultL","moduleName":"Data.FoldableWithIndex","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["i",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","FoldableWithIndex"],"FoldableWithIndex"],"constraintArgs":[{"tag":"TypeVar","contents":"i"},{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Monoid"],"Monoid"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"m"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"m"}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A default implementation of `foldMapWithIndex` using `foldlWithIndex`.\n\nNote: when defining a `FoldableWithIndex` instance, this function is\nunsafe to use in combination with `foldlWithIndexDefault`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[88,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/FoldableWithIndex.purs","end":[94,7]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"foldMapWithIndexDefaultR","moduleName":"Data.FoldableWithIndex","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["i",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","FoldableWithIndex"],"FoldableWithIndex"],"constraintArgs":[{"tag":"TypeVar","contents":"i"},{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Monoid"],"Monoid"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"m"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"m"}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A default implementation of `foldMapWithIndex` using `foldrWithIndex`.\n\nNote: when defining a `FoldableWithIndex` instance, this function is\nunsafe to use in combination with `foldrWithIndexDefault`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[56,3],"name":".spago/foldable-traversable/v4.1.1/src/Data/FoldableWithIndex.purs","end":[56,72]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"foldMapWithIndex","moduleName":"Data.FoldableWithIndex","info":{"values":[{"typeClassArguments":[["i",null],["f",null]],"typeClass":[["Data","FoldableWithIndex"],"FoldableWithIndex"],"type":{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","FoldableWithIndex"],"FoldableWithIndex"],"constraintArgs":[{"tag":"TypeVar","contents":"i"},{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Monoid"],"Monoid"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"m"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"m"}]}]}]}]},null]},null]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]
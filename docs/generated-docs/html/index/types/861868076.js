// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["861868076"] = [{"values":[{"sourceSpan":{"start":[86,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Semigroup/Foldable.purs","end":[86,56]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"minimum","moduleName":"Data.Semigroup.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semigroup","Foldable"],"Foldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[83,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Semigroup/Foldable.purs","end":[83,56]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"maximum","moduleName":"Data.Semigroup.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semigroup","Foldable"],"Foldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[43,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Semigroup/Foldable.purs","end":[43,67]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"fold1Default","moduleName":"Data.Semigroup.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["t",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semigroup","Foldable"],"Foldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"t"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semigroup"],"Semigroup"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"t"},{"tag":"TypeVar","contents":"m"}]}]},{"tag":"TypeVar","contents":"m"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A default implementation of `fold1` using `foldMap1`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[40,3],"name":".spago/foldable-traversable/v4.1.1/src/Data/Semigroup/Foldable.purs","end":[40,45]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"fold1","moduleName":"Data.Semigroup.Foldable","info":{"values":[{"typeClassArguments":[["t",null]],"typeClass":[["Data","Semigroup","Foldable"],"Foldable1"],"type":{"tag":"ForAll","contents":["t",{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semigroup","Foldable"],"Foldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"t"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semigroup"],"Semigroup"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"t"},{"tag":"TypeVar","contents":"m"}]}]},{"tag":"TypeVar","contents":"m"}]}]}]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[408,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[408,49]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"null","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Test whether the structure is empty.\nOptimized for structures that are similar to cons-lists, because there\nis no general way to do better.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[330,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[330,60]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"product","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semiring"],"Semiring"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Find the product of the numeric values in a data structure.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[326,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[326,56]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"sum","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semiring"],"Semiring"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Find the sum of the numeric values in a data structure.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[312,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[312,61]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"or","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","HeytingAlgebra"],"HeytingAlgebra"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The disjunction of all the values in a data structure. When specialized\nto `Boolean`, this function will test whether any of the values in a data\nstructure is `true`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[306,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[306,62]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"and","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","HeytingAlgebra"],"HeytingAlgebra"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The conjunction of all the values in a data structure. When specialized\nto `Boolean`, this function will test whether all of the values in a data\nstructure are `true`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[173,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[173,55]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"fold","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Monoid"],"Monoid"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"m"}]}]},{"tag":"TypeVar","contents":"m"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Fold a data structure, accumulating values in some `Monoid`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[21,3],"name":".spago/control/v4.2.0/src/Control/Comonad.purs","end":[21,32]},"score":24,"packageInfo":{"values":["control"],"tag":"Package"},"name":"extract","moduleName":"Control.Comonad","info":{"values":[{"typeClassArguments":[["w",null]],"typeClass":[["Control","Comonad"],"Comonad"],"type":{"tag":"ForAll","contents":["w",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Comonad"],"Comonad"],"constraintArgs":[{"tag":"TypeVar","contents":"w"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"w"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]
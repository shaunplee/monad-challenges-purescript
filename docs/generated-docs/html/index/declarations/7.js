// This file was generated by purescript-docs-search.
window.DocsSearchIndex["7"] = [["nan",[{"values":[{"sourceSpan":{"start":[24,1],"name":".spago/globals/v5.0.0/src/Global.purs","end":[24,29]},"score":6,"packageInfo":{"values":["globals"],"tag":"Package"},"name":"nan","moduleName":"Global","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Not a number (NaN)\n"}],"tag":"SearchResult"}]],["naturaltransformation",[{"values":[{"sourceSpan":{"start":[17,1],"name":".spago/prelude/v4.1.1/src/Data/NaturalTransformation.purs","end":[17,54]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"NaturalTransformation","moduleName":"Data.NaturalTransformation","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeVar","contents":"a"}]}]},null]},"arguments":[["f",null],["g",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":"A type for natural transformations.\n\nA natural transformation is a mapping between type constructors of kind\n`Type -> Type` where the mapping operation has no ability to manipulate the\ninner values.\n\nAn example of this is the `fromFoldable` function provided in\n`purescript-lists`, where some foldable structure containing values of\ntype `a` is converted into a `List a`.\n\nThe definition of a natural transformation in category theory states that\n`f` and `g` should be functors, but the `Functor` constraint is not\nenforced here; that the types are of kind `Type -> Type` is enough for our\npurposes.\n"}],"tag":"SearchResult"}]],["lcm",[{"values":[{"sourceSpan":{"start":[94,1],"name":".spago/prelude/v4.1.1/src/Data/EuclideanRing.purs","end":[94,56]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"lcm","moduleName":"Data.EuclideanRing","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Eq"],"Eq"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","EuclideanRing"],"EuclideanRing"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The *least common multiple* of two values.\n"}],"tag":"SearchResult"}]],["ifm",[{"values":[{"sourceSpan":{"start":[115,1],"name":".spago/prelude/v4.1.1/src/Control/Bind.purs","end":[115,60]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"ifM","moduleName":"Control.Bind","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Bind"],"Bind"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Execute a monadic action if a condition holds.\n\nFor example:\n\n```purescript\nmain = ifM ((< 0.5) <$> random)\n         (trace \"Heads\")\n         (trace \"Tails\")\n```\n"}],"tag":"SearchResult"}]],["field",[{"values":[{"sourceSpan":{"start":[39,1],"name":".spago/prelude/v4.1.1/src/Data/Field.purs","end":[39,51]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Field","moduleName":"Data.Field","info":{"values":[{"superclasses":[{"constraintClass":[["Data","EuclideanRing"],"EuclideanRing"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"constraintClass":[["Data","DivisionRing"],"DivisionRing"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]}],"fundeps":[],"arguments":[["a",null]]}],"tag":"TypeClassResult"},"hashAnchor":"t","comments":"The `Field` class is for types that are (commutative) fields.\n\nMathematically, a field is a ring which is commutative and in which every\nnonzero element has a multiplicative inverse; these conditions correspond\nto the `CommutativeRing` and `DivisionRing` classes in PureScript\nrespectively. However, the `Field` class has `EuclideanRing` and\n`DivisionRing` as superclasses, which seems like a stronger requirement\n(since `CommutativeRing` is a superclass of `EuclideanRing`). In fact, it\nis not stronger, since any type which has law-abiding `CommutativeRing`\nand `DivisionRing` instances permits exactly one law-abiding\n`EuclideanRing` instance. We use a `EuclideanRing` superclass here in\norder to ensure that a `Field` constraint on a function permits you to use\n`div` on that type, since `div` is a member of `EuclideanRing`.\n\nThis class has no laws or members of its own; it exists as a convenience,\nso a single constraint can be used when field-like behaviour is expected.\n\nThis module also defines a single `Field` instance for any type which has\nboth `EuclideanRing` and `DivisionRing` instances. Any other instance\nwould overlap with this instance, so no other `Field` instances should be\ndefined in libraries. Instead, simply define `EuclideanRing` and\n`DivisionRing` instances, and this will permit your type to be used with a\n`Field` constraint.\n"}],"tag":"SearchResult"}]],["filter",[{"values":[{"sourceSpan":{"start":[281,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[281,65]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"filter","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[587,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[587,72]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"filter","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Filter an array, keeping the elements which satisfy a predicate function,\ncreating a new array.\n\n```purescript\nfilter (_ > 0) [-1, 4, -5, 7] = [4, 7]\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[157,1],"name":".spago/stringutils/v0.0.10/src/Data/String/Utils.purs","end":[157,50]},"score":1,"packageInfo":{"values":["stringutils"],"tag":"Package"},"name":"filter","moduleName":"Data.String.Utils","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Keep only those characters that satisfy the predicate.\nThis function uses `String` instead of `Char` because PureScript\n`Char`s must be UTF-16 code units and hence cannot represent all Unicode\ncode points.\n"}],"tag":"SearchResult"}]],["filtera",[{"values":[{"sourceSpan":{"start":[291,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[296,17]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"filterA","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Applicative"],"Applicative"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[609,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[609,83]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"filterA","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Applicative"],"Applicative"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Filter where the predicate returns a `Boolean` in some `Applicative`.\n\n```purescript\npowerSet :: forall a. Array a -> Array (Array a)\npowerSet = filterA (const [true, false])\n```\n"}],"tag":"SearchResult"}]],["filtered",[{"values":[{"sourceSpan":{"start":[113,1],"name":".spago/gen/v2.1.1/src/Control/Monad/Gen.purs","end":[113,71]},"score":7,"packageInfo":{"values":["gen"],"tag":"Package"},"name":"filtered","moduleName":"Control.Monad.Gen","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Rec","Class"],"MonadRec"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad","Gen","Class"],"MonadGen"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Creates a generator that repeatedly run another generator until it produces\n`Just` node. This will never halt if the input generator always produces `Nothing`.\n"}],"tag":"SearchResult"}]],["find",[{"values":[{"sourceSpan":{"start":[366,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[366,67]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"find","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Try to find an element in a data structure which satisfies a predicate.\n"}],"tag":"SearchResult"}]],["findindex",[{"values":[{"sourceSpan":{"start":[245,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[245,70]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"findIndex","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[426,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[426,62]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"findIndex","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Find the first index for which a predicate holds.\n\n```purescript\nfindIndex (contains $ Pattern \"b\") [\"a\", \"bb\", \"b\", \"d\"] = Just 1\nfindIndex (contains $ Pattern \"x\") [\"a\", \"bb\", \"b\", \"d\"] = Nothing\n```\n\n"}],"tag":"SearchResult"}]],["findlastindex",[{"values":[{"sourceSpan":{"start":[248,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[248,74]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"findLastIndex","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[444,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[444,66]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"findLastIndex","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Find the last index for which a predicate holds.\n\n```purescript\nfindLastIndex (contains $ Pattern \"b\") [\"a\", \"bb\", \"b\", \"d\"] = Just 2\nfindLastIndex (contains $ Pattern \"x\") [\"a\", \"bb\", \"b\", \"d\"] = Nothing\n```\n\n"}],"tag":"SearchResult"}]],["findmap",[{"values":[{"sourceSpan":{"start":[373,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[373,72]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"findMap","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"b"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"b"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Try to find an element in a data structure which satisfies a predicate mapping.\n"}],"tag":"SearchResult"}]],["findwithindex",[{"values":[{"sourceSpan":{"start":[266,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/FoldableWithIndex.purs","end":[271,38]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"findWithIndex","moduleName":"Data.FoldableWithIndex","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["i",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","FoldableWithIndex"],"FoldableWithIndex"],"constraintArgs":[{"tag":"TypeVar","contents":"i"},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["index",{"tag":"TypeVar","contents":"i"},{"tag":"RCons","contents":["value",{"tag":"TypeVar","contents":"a"},{"tag":"REmpty","contents":{}}]}]}]}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Try to find an element in a data structure which satisfies a predicate\nwith access to the index.\n"}],"tag":"SearchResult"}]],["first",[{"values":[{"sourceSpan":{"start":[21,1],"name":".spago/maybe/v4.0.1/src/Data/Maybe/First.purs","end":[21,34]},"score":49,"packageInfo":{"values":["maybe"],"tag":"Package"},"name":"First","moduleName":"Data.Maybe.First","info":{"values":[{"typeArguments":[["a",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Monoid returning the first (left-most) non-`Nothing` value.\n\n``` purescript\nFirst (Just x) <> First (Just y) == First (Just x)\nFirst Nothing <> First (Just y) == First (Just y)\nFirst Nothing <> Nothing == First Nothing\nmempty :: First _ == First Nothing\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":49,"packageInfo":{"values":["maybe"],"tag":"Package"},"name":"First","moduleName":"Data.Maybe.First","info":{"values":[{"arguments":[{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[13,1],"name":".spago/prelude/v4.1.1/src/Data/Semigroup/First.purs","end":[13,26]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"First","moduleName":"Data.Semigroup.First","info":{"values":[{"typeArguments":[["a",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Semigroup where `append` always takes the first option.\n\n``` purescript\nFirst x <> First y == First x\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"First","moduleName":"Data.Semigroup.First","info":{"values":[{"arguments":[{"tag":"TypeVar","contents":"a"}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["firstnames",[{"values":[{"sourceSpan":{"start":[100,1],"name":"src/MCPrelude.purs","end":[100,27]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"firstNames","moduleName":"MCPrelude","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["fix",[{"values":[{"sourceSpan":{"start":[22,1],"name":".spago/control/v4.2.0/src/Control/Lazy.purs","end":[22,41]},"score":24,"packageInfo":{"values":["control"],"tag":"Package"},"name":"fix","moduleName":"Control.Lazy","info":{"values":[{"type":{"tag":"ForAll","contents":["l",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Lazy"],"Lazy"],"constraintArgs":[{"tag":"TypeVar","contents":"l"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"l"}]},{"tag":"TypeVar","contents":"l"}]}}]},{"tag":"TypeVar","contents":"l"}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`fix` defines a value as the fixed point of a function.\n\nThe `Lazy` instance allows us to generate the result lazily.\n"}],"tag":"SearchResult"}]],["clamp",[{"values":[{"sourceSpan":{"start":[180,1],"name":".spago/prelude/v4.1.1/src/Data/Ord.purs","end":[180,45]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"clamp","moduleName":"Data.Ord","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Clamp a value between a minimum and a maximum. For example:\n\n``` purescript\nlet f = clamp 0 10\nf (-5) == 0\nf 5    == 5\nf 15   == 10\n```\n"}],"tag":"SearchResult"}]],["clear",[{"values":[{"sourceSpan":{"start":[42,1],"name":".spago/console/v4.4.0/src/Effect/Class/Console.purs","end":[42,43]},"score":68,"packageInfo":{"values":["console"],"tag":"Package"},"name":"clear","moduleName":"Effect.Class.Console","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Effect","Class"],"MonadEffect"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[58,1],"name":".spago/console/v4.4.0/src/Effect/Console.purs","end":[58,36]},"score":68,"packageInfo":{"values":["console"],"tag":"Package"},"name":"clear","moduleName":"Effect.Console","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Clears the console\n"}],"tag":"SearchResult"}]],["clown",[{"values":[{"sourceSpan":{"start":[12,1],"name":".spago/bifunctors/v4.0.0/src/Data/Bifunctor/Clown.purs","end":[12,34]},"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"Clown","moduleName":"Data.Bifunctor.Clown","info":{"values":[{"typeArguments":[["f",null],["a",null],["b",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Make a `Functor` over the first argument of a `Bifunctor`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"Clown","moduleName":"Data.Bifunctor.Clown","info":{"values":[{"arguments":[{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["and",[{"values":[{"sourceSpan":{"start":[306,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[306,62]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"and","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","HeytingAlgebra"],"HeytingAlgebra"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The conjunction of all the values in a data structure. When specialized\nto `Boolean`, this function will test whether all of the values in a data\nstructure are `true`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[13,1],"name":".spago/integers/v4.0.0/src/Data/Int/Bits.purs","end":[13,40]},"score":16,"packageInfo":{"values":["integers"],"tag":"Package"},"name":"and","moduleName":"Data.Int.Bits","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Bitwise AND.\n"}],"tag":"SearchResult"}]],["any",[{"values":[{"sourceSpan":{"start":[322,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[322,76]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"any","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","HeytingAlgebra"],"HeytingAlgebra"],"constraintArgs":[{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"b"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`any f` is the same as `or <<< map f`; map a function over the structure,\nand then get the disjunction of the results.\n"}],"tag":"SearchResult"}]],["anywithindex",[{"values":[{"sourceSpan":{"start":[255,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/FoldableWithIndex.purs","end":[261,7]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"anyWithIndex","moduleName":"Data.FoldableWithIndex","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["i",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","FoldableWithIndex"],"FoldableWithIndex"],"constraintArgs":[{"tag":"TypeVar","contents":"i"},{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","HeytingAlgebra"],"HeytingAlgebra"],"constraintArgs":[{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"b"}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`anyWithIndex f` is the same as `or <<< mapWithIndex f`; map a function over the\nstructure, and then get the disjunction of the results.\n"}],"tag":"SearchResult"}]]]
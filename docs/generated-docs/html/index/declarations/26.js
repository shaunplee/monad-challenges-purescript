// This file was generated by purescript-docs-search.
window.DocsSearchIndex["26"] = [["some",[{"values":[{"sourceSpan":{"start":[183,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[187,32]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"some","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Alternative"],"Alternative"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Lazy"],"Lazy"],"constraintArgs":[{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[192,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[192,78]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"some","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Alternative"],"Alternative"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Lazy"],"Lazy"],"constraintArgs":[{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Attempt a computation multiple times, requiring at least one success.\n\nThe `Lazy` constraint is used to generate the result lazily, to ensure\ntermination.\n"}],"tag":"SearchResult"}]],["sort",[{"values":[{"sourceSpan":{"start":[305,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[305,62]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sort","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[83,1],"name":".spago/arrays/v5.3.1/src/Data/Array/ST.purs","end":[83,63]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sort","moduleName":"Data.Array.ST","info":{"values":[{"type":{"tag":"ForAll","contents":["h",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","ST"],"STArray"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"ST"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","ST"],"STArray"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sort a mutable array in place.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[688,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[688,46]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sort","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sort the elements of an array in increasing order, creating a new array.\n\n```purescript\nsort [2, -3, 1] = [-3, 1, 2]\n```\n\n"}],"tag":"SearchResult"}]],["sortby",[{"values":[{"sourceSpan":{"start":[308,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[308,79]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sortBy","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[98,1],"name":".spago/arrays/v5.3.1/src/Data/Array/ST.purs","end":[102,24]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sortBy","moduleName":"Data.Array.ST","info":{"values":[{"type":{"tag":"ForAll","contents":["h",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","ST"],"STArray"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"ST"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","ST"],"STArray"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sort a mutable array in place using a comparison function.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[699,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[699,63]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sortBy","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sort the elements of an array in increasing order, where elements are\ncompared using the specified partial ordering, creating a new array.\n\n```purescript\ncompareLength a b = compare (length a) (length b)\nsortBy compareLength [[1, 2, 3], [7, 9], [-2]] = [[-2],[7,9],[1,2,3]]\n```\n\n"}],"tag":"SearchResult"}]],["sortwith",[{"values":[{"sourceSpan":{"start":[311,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[311,80]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sortWith","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[117,1],"name":".spago/arrays/v5.3.1/src/Data/Array/ST.purs","end":[122,24]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sortWith","moduleName":"Data.Array.ST","info":{"values":[{"type":{"tag":"ForAll","contents":["h",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","ST"],"STArray"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"ST"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","ST"],"STArray"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeVar","contents":"a"}]}}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sort a mutable array in place based on a projection.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[715,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[715,64]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"sortWith","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sort the elements of an array in increasing order, where elements are\nsorted based on a projection\n\n```purescript\nsortWith (_.age) [{name: \"Alice\", age: 42}, {name: \"Bob\", age: 21}]\n   = [{name: \"Bob\", age: 21}, {name: \"Alice\", age: 42}]\n```\n\n"}],"tag":"SearchResult"}]],["source",[{"values":[{"sourceSpan":{"start":[49,1],"name":".spago/strings/v4.0.2/src/Data/String/Regex.purs","end":[49,41]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"source","moduleName":"Data.String.Regex","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Regex"],"Regex"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the pattern string used to construct the given `Regex`.\n"}],"tag":"SearchResult"}]],["rproxy",[{"values":[{"sourceSpan":{"start":[19,1],"name":".spago/prelude/v4.1.1/src/Type/Data/Row.purs","end":[20,11]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"RProxy","moduleName":"Type.Data.Row","info":{"values":[{"typeArguments":[["row",{"tag":"Row","contents":{"tag":"NamedKind","contents":[["Prim"],"Type"]}}]],"dataDeclType":"data"}],"tag":"DataResult"},"hashAnchor":"t","comments":"A proxy data type whose type parameter is a type of kind `# Type` (a row\nof types).\n\nCommonly used for specialising a function with a quantified type.\nFor example, suppose we have an identity function for records of type:\n```purescript\nrecordIdentity :: forall row . RProxy row -> Record row -> Record row\nrecordIdentity _ rec = rec\n```\nThen applying this function to an `RProxy` with a specialised type\nallows us to specify a concrete type for `row`:\n```purescript\n:t recordIdentity (RProxy :: RProxy ( x :: Int, y :: Int ))\n{ x :: Int, y :: Int } -> { x :: Int, y :: Int }\n```\nHere `row` has been specialised to `( x :: Int, y :: Int )`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"RProxy","moduleName":"Type.Data.Row","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["pred",[{"values":[{"sourceSpan":{"start":[52,3],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[52,23]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"pred","moduleName":"Data.Enum","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","Enum"],"Enum"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"Enum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["prependstring",[{"values":[{"sourceSpan":{"start":[87,1],"name":".spago/strings/v4.0.2/src/Data/String/NonEmpty/Internal.purs","end":[87,60]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"prependString","moduleName":"Data.String.NonEmpty.Internal","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Prepends a string to this non-empty string. Since one of the strings is\nnon-empty we know the result will be too.\n\n```purescript\nprependString \"be\" (NonEmptyString \"fore\") == NonEmptyString \"before\"\nprependString \"\" (NonEmptyString \"fore\") == NonEmptyString \"fore\"\n```\n"}],"tag":"SearchResult"}]],["prj",[{"values":[{"sourceSpan":{"start":[10,3],"name":".spago/either/v4.1.1/src/Data/Either/Inject.purs","end":[10,22]},"score":27,"packageInfo":{"values":["either"],"tag":"Package"},"name":"prj","moduleName":"Data.Either.Inject","info":{"values":[{"typeClassArguments":[["a",null],["b",null]],"typeClass":[["Data","Either","Inject"],"Inject"],"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Either","Inject"],"Inject"],"constraintArgs":[{"tag":"TypeVar","contents":"a"},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["product",[{"values":[{"sourceSpan":{"start":[11,1],"name":".spago/bifunctors/v4.0.0/src/Data/Bifunctor/Product.purs","end":[11,47]},"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"Product","moduleName":"Data.Bifunctor.Product","info":{"values":[{"typeArguments":[["f",null],["g",null],["a",null],["b",null]],"dataDeclType":"data"}],"tag":"DataResult"},"hashAnchor":"t","comments":"The product of two `Bifunctor`s.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"Product","moduleName":"Data.Bifunctor.Product","info":{"values":[{"arguments":[{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[330,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Foldable.purs","end":[330,60]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"product","moduleName":"Data.Foldable","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semiring"],"Semiring"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Find the product of the numeric values in a data structure.\n"}],"tag":"SearchResult"}]],["mul",[{"values":[{"sourceSpan":{"start":[38,3],"name":".spago/prelude/v4.1.1/src/Data/Semiring.purs","end":[38,22]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"mul","moduleName":"Data.Semiring","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","Semiring"],"Semiring"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semiring"],"Semiring"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["mulrecord",[{"values":[{"sourceSpan":{"start":[83,3],"name":".spago/prelude/v4.1.1/src/Data/Semiring.purs","end":[83,76]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"mulRecord","moduleName":"Data.Semiring","info":{"values":[{"typeClassArguments":[["rowlist",null],["row",null],["subrow",null]],"typeClass":[["Data","Semiring"],"SemiringRecord"],"type":{"tag":"ForAll","contents":["rowlist",{"tag":"ForAll","contents":["row",{"tag":"ForAll","contents":["subrow",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semiring"],"SemiringRecord"],"constraintArgs":[{"tag":"TypeVar","contents":"rowlist"},{"tag":"TypeVar","contents":"row"},{"tag":"TypeVar","contents":"subrow"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Type","Data","RowList"],"RLProxy"]},{"tag":"TypeVar","contents":"rowlist"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"TypeVar","contents":"row"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"TypeVar","contents":"row"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"TypeVar","contents":"subrow"}]}]}]}]}]},null]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["multiline",[{"values":[{"sourceSpan":{"start":[50,1],"name":".spago/strings/v4.0.2/src/Data/String/Regex/Flags.purs","end":[50,24]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"multiline","moduleName":"Data.String.Regex.Flags","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Data","String","Regex","Flags"],"RegexFlags"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Only multiline flag set to true\n"}],"tag":"SearchResult"}]],["multiplicative",[{"values":[{"sourceSpan":{"start":[14,1],"name":".spago/prelude/v4.1.1/src/Data/Monoid/Multiplicative.purs","end":[14,44]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Multiplicative","moduleName":"Data.Monoid.Multiplicative","info":{"values":[{"typeArguments":[["a",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Monoid and semigroup for semirings under multiplication.\n\n``` purescript\nMultiplicative x <> Multiplicative y == Multiplicative (x * y)\n(mempty :: Multiplicative _) == Multiplicative one\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Multiplicative","moduleName":"Data.Monoid.Multiplicative","info":{"values":[{"arguments":[{"tag":"TypeVar","contents":"a"}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["&&",[{"values":[{"sourceSpan":{"start":[47,1],"name":".spago/prelude/v4.1.1/src/Data/HeytingAlgebra.purs","end":[47,20]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"(&&)","moduleName":"Data.HeytingAlgebra","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]]]
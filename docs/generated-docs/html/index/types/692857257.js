// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["692857257"] = [{"values":[{"sourceSpan":{"start":[186,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple.purs","end":[186,34]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"snd","moduleName":"Data.Tuple","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeVar","contents":"b"}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the second component of a tuple.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[182,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple.purs","end":[182,34]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"fst","moduleName":"Data.Tuple","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeVar","contents":"a"}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the first component of a tuple.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[101,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[101,32]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"get1","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["z",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T2"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"z"}]}]},{"tag":"TypeVar","contents":"a"}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Given at least a singleton tuple, gets the first value.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[60,1],"name":".spago/nonempty/v5.0.0/src/Data/NonEmpty.purs","end":[60,38]},"score":9,"packageInfo":{"values":["nonempty"],"tag":"Package"},"name":"head","moduleName":"Data.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","NonEmpty"],"NonEmpty"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeVar","contents":"a"}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Get the 'first' element of a non-empty container.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[261,1],"name":".spago/either/v4.1.1/src/Data/Either.purs","end":[261,52]},"score":27,"packageInfo":{"values":["either"],"tag":"Package"},"name":"fromRight","moduleName":"Data.Either","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Prim"],"Partial"],"constraintArgs":[]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeVar","contents":"b"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A partial function that extracts the value from the `Right` data constructor.\nPassing a `Left` to `fromRight` will throw an error at runtime.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[256,1],"name":".spago/either/v4.1.1/src/Data/Either.purs","end":[256,51]},"score":27,"packageInfo":{"values":["either"],"tag":"Package"},"name":"fromLeft","moduleName":"Data.Either","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Prim"],"Partial"],"constraintArgs":[]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A partial function that extracts the value from the `Left` data constructor.\nPassing a `Right` to `fromLeft` will throw an error at runtime.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[251,1],"name":".spago/either/v4.1.1/src/Data/Either.purs","end":[251,45]},"score":27,"packageInfo":{"values":["either"],"tag":"Package"},"name":"isRight","moduleName":"Data.Either","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns `true` when the `Either` value was constructed with `Right`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[247,1],"name":".spago/either/v4.1.1/src/Data/Either.purs","end":[247,44]},"score":27,"packageInfo":{"values":["either"],"tag":"Package"},"name":"isLeft","moduleName":"Data.Either","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns `true` when the `Either` value was constructed with `Left`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[127,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Bifoldable.purs","end":[127,61]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"bifold","moduleName":"Data.Bifoldable","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["t",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Bifoldable"],"Bifoldable"],"constraintArgs":[{"tag":"TypeVar","contents":"t"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Monoid"],"Monoid"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"t"},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"m"}]}]},{"tag":"TypeVar","contents":"m"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Fold a data structure, accumulating values in a monoidal type.\n"}],"tag":"SearchResult"}]
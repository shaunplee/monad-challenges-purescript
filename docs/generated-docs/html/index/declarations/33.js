// This file was generated by purescript-docs-search.
window.DocsSearchIndex["33"] = [["wrap",[{"values":[{"sourceSpan":{"start":[12,1],"name":".spago/bifunctors/v4.0.0/src/Data/Bifunctor/Wrap.purs","end":[12,34]},"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"Wrap","moduleName":"Data.Bifunctor.Wrap","info":{"values":[{"typeArguments":[["p",null],["a",null],["b",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Provides a `Functor` over the second argument of a `Bifunctor`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"Wrap","moduleName":"Data.Bifunctor.Wrap","info":{"values":[{"arguments":[{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"p"},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[37,3],"name":".spago/newtype/v3.0.0/src/Data/Newtype.purs","end":[37,17]},"score":26,"packageInfo":{"values":["newtype"],"tag":"Package"},"name":"wrap","moduleName":"Data.Newtype","info":{"values":[{"typeClassArguments":[["t",null],["a",null]],"typeClass":[["Data","Newtype"],"Newtype"],"type":{"tag":"ForAll","contents":["t",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Newtype"],"Newtype"],"constraintArgs":[{"tag":"TypeVar","contents":"t"},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"t"}]}]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["write",[{"values":[{"sourceSpan":{"start":[109,1],"name":".spago/st/v4.1.1/src/Control/Monad/ST/Internal.purs","end":[109,61]},"score":7,"packageInfo":{"values":["st"],"tag":"Package"},"name":"write","moduleName":"Control.Monad.ST.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["r",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"STRef"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"ST"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Set the value of a mutable reference.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[36,1],"name":".spago/refs/v4.1.0/src/Effect/Ref.purs","end":[36,60]},"score":8,"packageInfo":{"values":["refs"],"tag":"Package"},"name":"write","moduleName":"Effect.Ref","info":{"values":[{"type":{"tag":"ForAll","contents":["s",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Ref"],"Ref"]},{"tag":"TypeVar","contents":"s"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Update the value of a mutable reference to the specified value.\n"}],"tag":"SearchResult"}]],["tuple",[{"values":[{"sourceSpan":{"start":[32,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple.purs","end":[32,27]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple","moduleName":"Data.Tuple","info":{"values":[{"typeArguments":[["a",null],["b",null]],"dataDeclType":"data"}],"tag":"DataResult"},"hashAnchor":"t","comments":"A simple product type for wrapping a pair of component values.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple","moduleName":"Data.Tuple","info":{"values":[{"arguments":[{"tag":"TypeVar","contents":"a"},{"tag":"TypeVar","contents":"b"}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["tuple1",[{"values":[{"sourceSpan":{"start":[38,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[38,26]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple1","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T2"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},"arguments":[["a",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[61,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[61,34]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"tuple1","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"Tuple1"]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Creates a singleton tuple.\n"}],"tag":"SearchResult"}]],["tuple10",[{"values":[{"sourceSpan":{"start":[47,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[47,64]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple10","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T11"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"j"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},"arguments":[["a",null],["b",null],["c",null],["d",null],["e",null],["f",null],["g",null],["h",null],["i",null],["j",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[97,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[97,117]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"tuple10","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["j",{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["h",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["d",{"tag":"ForAll","contents":["c",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"j"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"Tuple10"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"j"}]}]}]}]}]}]}]}]}]}]}]},null]},null]},null]},null]},null]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Given 10 values, creates a nested 10-tuple.\n"}],"tag":"SearchResult"}]],["tuple2",[{"values":[{"sourceSpan":{"start":[39,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[39,30]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple2","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T3"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},"arguments":[["a",null],["b",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[65,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[65,43]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"tuple2","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"Tuple2"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Given 2 values, creates a 2-tuple.\n"}],"tag":"SearchResult"}]],["tuple3",[{"values":[{"sourceSpan":{"start":[40,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[40,34]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple3","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T4"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},"arguments":[["a",null],["b",null],["c",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[69,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[69,52]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"tuple3","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["c",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"Tuple3"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Given 3 values, creates a nested 3-tuple.\n"}],"tag":"SearchResult"}]],["tuple4",[{"values":[{"sourceSpan":{"start":[41,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[41,38]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple4","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T5"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},"arguments":[["a",null],["b",null],["c",null],["d",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[73,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[73,61]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"tuple4","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["d",{"tag":"ForAll","contents":["c",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"Tuple4"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Given 4 values, creates a nested 4-tuple.\n"}],"tag":"SearchResult"}]],["tuple5",[{"values":[{"sourceSpan":{"start":[42,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[42,41]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple5","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T6"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},"arguments":[["a",null],["b",null],["c",null],["d",null],["e",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[77,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[77,70]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"tuple5","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["d",{"tag":"ForAll","contents":["c",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"Tuple5"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]}]}]}]}]}]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Given 5 values, creates a nested 5-tuple.\n"}],"tag":"SearchResult"}]],["tuple6",[{"values":[{"sourceSpan":{"start":[43,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[43,46]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple6","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T7"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},"arguments":[["a",null],["b",null],["c",null],["d",null],["e",null],["f",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[81,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[81,79]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"tuple6","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["d",{"tag":"ForAll","contents":["c",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"Tuple6"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"f"}]}]}]}]}]}]}]},null]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Given 6 values, creates a nested 6-tuple.\n"}],"tag":"SearchResult"}]],["tuple7",[{"values":[{"sourceSpan":{"start":[44,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[44,50]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple7","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T8"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},"arguments":[["a",null],["b",null],["c",null],["d",null],["e",null],["f",null],["g",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[85,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[85,88]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"tuple7","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["d",{"tag":"ForAll","contents":["c",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"Tuple7"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"g"}]}]}]}]}]}]}]}]},null]},null]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Given 7 values, creates a nested 7-tuple.\n"}],"tag":"SearchResult"}]],["tuple8",[{"values":[{"sourceSpan":{"start":[45,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[45,54]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple8","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T9"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},"arguments":[["a",null],["b",null],["c",null],["d",null],["e",null],["f",null],["g",null],["h",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[89,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[89,97]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"tuple8","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["h",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["d",{"tag":"ForAll","contents":["c",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"Tuple8"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"h"}]}]}]}]}]}]}]}]}]},null]},null]},null]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Given 8 values, creates a nested 8-tuple.\n"}],"tag":"SearchResult"}]],["tuple9",[{"values":[{"sourceSpan":{"start":[46,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[46,59]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"Tuple9","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T10"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},"arguments":[["a",null],["b",null],["c",null],["d",null],["e",null],["f",null],["g",null],["h",null],["i",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[93,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[93,106]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"tuple9","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["i",{"tag":"ForAll","contents":["h",{"tag":"ForAll","contents":["g",{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["d",{"tag":"ForAll","contents":["c",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"Tuple9"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]},{"tag":"TypeVar","contents":"d"}]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"g"}]},{"tag":"TypeVar","contents":"h"}]},{"tag":"TypeVar","contents":"i"}]}]}]}]}]}]}]}]}]}]},null]},null]},null]},null]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Given 9 values, creates a nested 9-tuple.\n"}],"tag":"SearchResult"}]]]
// This file was generated by purescript-docs-search.
window.DocsSearchIndex["17"] = [["test",[{"values":[{"sourceSpan":{"start":[80,1],"name":".spago/strings/v4.0.2/src/Data/String/Regex.purs","end":[80,50]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"test","moduleName":"Data.String.Regex","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","Regex"],"Regex"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns `true` if the `Regex` matches the string. In contrast to\n`RegExp.prototype.test()` in JavaScript, `test` does not affect\nthe `lastIndex` property of the Regex.\n"}],"tag":"SearchResult"}]],["text",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"Text","moduleName":"Prim.TypeError","info":{"values":[{"kind":{"tag":"FunKind","contents":[{"tag":"NamedKind","contents":[["Prim"],"Symbol"]},{"tag":"NamedKind","contents":[["Prim","TypeError"],"Doc"]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"The Text type constructor makes a Doc from a Symbol\nto be used in a custom type error.\n\nFor more information, see\n[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md).\n"}],"tag":"SearchResult"}]],["t3",[{"values":[{"sourceSpan":{"start":[50,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple/Nested.purs","end":[50,33]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"T3","moduleName":"Data.Tuple.Nested","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple","Nested"],"T2"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"z"}]}}]},"arguments":[["a",null],["b",null],["z",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]],["pi",[{"values":[{"sourceSpan":{"start":[88,1],"name":".spago/math/v2.1.1/src/Math.purs","end":[88,28]},"score":12,"packageInfo":{"values":["math"],"tag":"Package"},"name":"pi","moduleName":"Math","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The ratio of the circumference of a circle to its diameter, around 3.14159.\n"}],"tag":"SearchResult"}]],["lmap",[{"values":[{"sourceSpan":{"start":[22,1],"name":".spago/bifunctors/v4.0.0/src/Data/Bifunctor.purs","end":[22,66]},"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"lmap","moduleName":"Data.Bifunctor","info":{"values":[{"type":{"tag":"ForAll","contents":["c",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Bifunctor"],"Bifunctor"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"c"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]}]}]}]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Map a function over the first type argument of a `Bifunctor`.\n"}],"tag":"SearchResult"}]],["join",[{"values":[{"sourceSpan":{"start":[83,1],"name":".spago/prelude/v4.1.1/src/Control/Bind.purs","end":[83,45]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"join","moduleName":"Control.Bind","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Bind"],"Bind"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Collapse two applications of a monadic type constructor into one.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[12,1],"name":".spago/bifunctors/v4.0.0/src/Data/Bifunctor/Join.purs","end":[12,32]},"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"Join","moduleName":"Data.Bifunctor.Join","info":{"values":[{"typeArguments":[["p",null],["a",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Turns a `Bifunctor` into a `Functor` by equating the two type arguments.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"Join","moduleName":"Data.Bifunctor.Join","info":{"values":[{"arguments":[{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"p"},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["join1with",[{"values":[{"sourceSpan":{"start":[203,1],"name":".spago/strings/v4.0.2/src/Data/String/NonEmpty/Internal.purs","end":[203,83]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"join1With","moduleName":"Data.String.NonEmpty.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semigroup","Foldable"],"Foldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Joins non-empty strings in a non-empty container together as a new\nnon-empty string, inserting a possibly empty string as separator between\nthem. The result is guaranteed to be non-empty.\n\n```purescript\n-- array syntax is used for demonstration here, it would need to be a real `Foldable1`\njoin1With \", \" [NonEmptyString \"apple\", NonEmptyString \"banana\"] == NonEmptyString \"apple, banana\"\njoin1With \"\" [NonEmptyString \"apple\", NonEmptyString \"banana\"] == NonEmptyString \"applebanana\"\n```\n"}],"tag":"SearchResult"}]],["joinwith",[{"values":[{"sourceSpan":{"start":[96,1],"name":".spago/strings/v4.0.2/src/Data/String/Common.purs","end":[96,60]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"joinWith","moduleName":"Data.String.Common","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Joins the strings in the array together, inserting the first argument\nas separator between them.\n\n```purescript\njoinWith \", \" [\"apple\", \"banana\", \"orange\"] == \"apple, banana, orange\"\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[188,1],"name":".spago/strings/v4.0.2/src/Data/String/NonEmpty/Internal.purs","end":[188,73]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"joinWith","moduleName":"Data.String.NonEmpty.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Joins the strings in a container together as a new string, inserting the\nfirst argument as separator between them. The result is not guaranteed to\nbe non-empty.\n\n```purescript\njoinWith \", \" [NonEmptyString \"apple\", NonEmptyString \"banana\"] == \"apple, banana\"\njoinWith \", \" [] == \"\"\n```\n"}],"tag":"SearchResult"}]],["joinwith1",[{"values":[{"sourceSpan":{"start":[215,1],"name":".spago/strings/v4.0.2/src/Data/String/NonEmpty/Internal.purs","end":[215,83]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"joinWith1","moduleName":"Data.String.NonEmpty.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Semigroup","Foldable"],"Foldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Joins possibly empty strings in a non-empty container together as a new\nnon-empty string, inserting a non-empty string as a separator between them.\nThe result is guaranteed to be non-empty.\n\n```purescript\n-- array syntax is used for demonstration here, it would need to be a real `Foldable1`\njoinWith1 (NonEmptyString \", \") [\"apple\", \"banana\"] == NonEmptyString \"apple, banana\"\njoinWith1 (NonEmptyString \"/\") [\"a\", \"b\", \"\", \"c\", \"\"] == NonEmptyString \"a/b//c/\"\n```\n"}],"tag":"SearchResult"}]],["joker",[{"values":[{"sourceSpan":{"start":[12,1],"name":".spago/bifunctors/v4.0.0/src/Data/Bifunctor/Joker.purs","end":[12,34]},"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"Joker","moduleName":"Data.Bifunctor.Joker","info":{"values":[{"typeArguments":[["g",null],["a",null],["b",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Make a `Functor` over the second argument of a `Bifunctor`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":12,"packageInfo":{"values":["bifunctors"],"tag":"Package"},"name":"Joker","moduleName":"Data.Bifunctor.Joker","info":{"values":[{"arguments":[{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"g"},{"tag":"TypeVar","contents":"b"}]}}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["greaterthan",[{"values":[{"sourceSpan":{"start":[128,1],"name":".spago/prelude/v4.1.1/src/Data/Ord.purs","end":[128,52]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"greaterThan","moduleName":"Data.Ord","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Test whether one value is _strictly greater than_ another.\n"}],"tag":"SearchResult"}]],["greaterthanoreq",[{"values":[{"sourceSpan":{"start":[140,1],"name":".spago/prelude/v4.1.1/src/Data/Ord.purs","end":[140,56]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"greaterThanOrEq","moduleName":"Data.Ord","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Test whether one value is _non-strictly greater than_ another.\n"}],"tag":"SearchResult"}]],["greekdata",[{"values":[{"sourceSpan":{"start":[70,1],"name":"src/MCPrelude.purs","end":[71,37]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"GreekData","moduleName":"MCPrelude","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}}]}}]},"arguments":[]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]],["greekdataa",[{"values":[{"sourceSpan":{"start":[73,1],"name":"src/MCPrelude.purs","end":[73,24]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"greekDataA","moduleName":"MCPrelude","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["MCPrelude"],"GreekData"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["greekdatab",[{"values":[{"sourceSpan":{"start":[81,1],"name":"src/MCPrelude.purs","end":[81,24]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"greekDataB","moduleName":"MCPrelude","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["MCPrelude"],"GreekData"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["group",[{"values":[{"sourceSpan":{"start":[843,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[843,62]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"group","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Eq"],"Eq"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Group equal, consecutive elements of an array into arrays.\n\n```purescript\ngroup [1,1,2,2,1] == [NonEmpty 1 [1], NonEmpty 2 [2], NonEmpty 1 []]\n```\n"}],"tag":"SearchResult"}]],["group'",[{"values":[{"sourceSpan":{"start":[851,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[851,64]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"group'","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sort and then group the elements of an array into arrays.\n\n```purescript\ngroup' [1,1,2,2,1] == [NonEmpty 1 [1,1],NonEmpty 2 [2]]\n```\n"}],"tag":"SearchResult"}]],["groupby",[{"values":[{"sourceSpan":{"start":[862,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[862,79]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"groupBy","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Group equal, consecutive elements of an array into arrays, using the\nspecified equivalence relation to detemine equality.\n\n```purescript\ngroupBy (\\a b -> odd a && odd b) [1, 3, 2, 4, 3, 3]\n   = [NonEmpty 1 [3], NonEmpty 2 [] , NonEmpty 4 [], NonEmpty 3 [3]]\n```\n\n"}],"tag":"SearchResult"}]],["fst",[{"values":[{"sourceSpan":{"start":[182,1],"name":".spago/tuples/v5.1.0/src/Data/Tuple.purs","end":[182,34]},"score":26,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"fst","moduleName":"Data.Tuple","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeVar","contents":"a"}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the first component of a tuple.\n"}],"tag":"SearchResult"}]],["dual",[{"values":[{"sourceSpan":{"start":[14,1],"name":".spago/prelude/v4.1.1/src/Data/Monoid/Dual.purs","end":[14,24]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Dual","moduleName":"Data.Monoid.Dual","info":{"values":[{"typeArguments":[["a",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"The dual of a monoid.\n\n``` purescript\nDual x <> Dual y == Dual (y <> x)\n(mempty :: Dual _) == Dual mempty\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Dual","moduleName":"Data.Monoid.Dual","info":{"values":[{"arguments":[{"tag":"TypeVar","contents":"a"}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["duplicate",[{"values":[{"sourceSpan":{"start":[58,1],"name":".spago/control/v4.2.0/src/Control/Extend.purs","end":[58,52]},"score":24,"packageInfo":{"values":["control"],"tag":"Package"},"name":"duplicate","moduleName":"Control.Extend","info":{"values":[{"type":{"tag":"ForAll","contents":["w",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Extend"],"Extend"],"constraintArgs":[{"tag":"TypeVar","contents":"w"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"w"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"w"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"w"},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Duplicate a comonadic context.\n\n`duplicate` is dual to `Control.Bind.join`.\n"}],"tag":"SearchResult"}]]]
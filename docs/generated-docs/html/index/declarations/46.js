// This file was generated by purescript-docs-search.
window.DocsSearchIndex["46"] = [["cardinality",[{"values":[{"sourceSpan":{"start":[114,3],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[114,31]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"cardinality","moduleName":"Data.Enum","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","Enum"],"BoundedEnum"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"BoundedEnum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Enum"],"Cardinality"]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[163,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[163,40]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"Cardinality","moduleName":"Data.Enum","info":{"values":[{"typeArguments":[["a",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"A type for the size of finite enumerations.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"Cardinality","moduleName":"Data.Enum","info":{"values":[{"arguments":[{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["caseinsensitivenonemptystring",[{"values":[{"sourceSpan":{"start":[9,1],"name":".spago/strings/v4.0.2/src/Data/String/NonEmpty/CaseInsensitive.purs","end":[9,85]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"CaseInsensitiveNonEmptyString","moduleName":"Data.String.NonEmpty.CaseInsensitive","info":{"values":[{"typeArguments":[],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"A newtype for case insensitive string comparisons and ordering.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"CaseInsensitiveNonEmptyString","moduleName":"Data.String.NonEmpty.CaseInsensitive","info":{"values":[{"arguments":[{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["caseinsensitivestring",[{"values":[{"sourceSpan":{"start":[9,1],"name":".spago/strings/v4.0.2/src/Data/String/CaseInsensitive.purs","end":[9,61]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"CaseInsensitiveString","moduleName":"Data.String.CaseInsensitive","info":{"values":[{"typeArguments":[],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"A newtype for case insensitive string comparisons and ordering.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"CaseInsensitiveString","moduleName":"Data.String.CaseInsensitive","info":{"values":[{"arguments":[{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["category",[{"values":[{"sourceSpan":{"start":[16,1],"name":".spago/prelude/v4.1.1/src/Control/Category.purs","end":[17,30]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Category","moduleName":"Control.Category","info":{"values":[{"superclasses":[{"constraintClass":[["Control","Semigroupoid"],"Semigroupoid"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]}],"fundeps":[],"arguments":[["a",null]]}],"tag":"TypeClassResult"},"hashAnchor":"t","comments":"`Category`s consist of objects and composable morphisms between them, and\nas such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`\nmust have an identity element.\n\nInstances must satisfy the following law in addition to the\n`Semigroupoid` law:\n\n- Identity: `identity <<< p = p <<< identity = p`\n"}],"tag":"SearchResult"}]],["catmaybes",[{"values":[{"sourceSpan":{"start":[302,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[302,58]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"catMaybes","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[635,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[635,50]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"catMaybes","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Filter an array of optional values, keeping only the elements which contain\na value, creating a new array.\n\n```purescript\ncatMaybes [Nothing, Just 2, Nothing, Just 4] = [2, 4]\n```\n\n"}],"tag":"SearchResult"}]],["accum",[{"values":[{"sourceSpan":{"start":[5,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/Traversable/Accum.purs","end":[5,44]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"Accum","moduleName":"Data.Traversable.Accum","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["accum",{"tag":"TypeVar","contents":"s"},{"tag":"RCons","contents":["value",{"tag":"TypeVar","contents":"a"},{"tag":"REmpty","contents":{}}]}]}]},"arguments":[["s",null],["a",null]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"}]],["acos",[{"values":[{"sourceSpan":{"start":[12,1],"name":".spago/math/v2.1.1/src/Math.purs","end":[12,41]},"score":12,"packageInfo":{"values":["math"],"tag":"Package"},"name":"acos","moduleName":"Math","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}]},{"tag":"TypeConstructor","contents":[["Math"],"Radians"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns the inverse cosine of the argument.\n"}],"tag":"SearchResult"}]],["<$",[{"values":[{"sourceSpan":{"start":[67,1],"name":".spago/prelude/v4.1.1/src/Data/Functor.purs","end":[67,25]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"(<$)","moduleName":"Data.Functor","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["<$>",[{"values":[{"sourceSpan":{"start":[27,1],"name":".spago/prelude/v4.1.1/src/Data/Functor.purs","end":[27,20]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"(<$>)","moduleName":"Data.Functor","info":{"values":[],"tag":"ValueAliasResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]]]
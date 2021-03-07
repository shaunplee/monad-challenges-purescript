// This file was generated by purescript-docs-search.
window.DocsSearchIndex["11"] = [["radians",[{"values":[{"sourceSpan":{"start":[6,1],"name":".spago/math/v2.1.1/src/Math.purs","end":[6,22]},"score":12,"packageInfo":{"values":["math"],"tag":"Package"},"name":"Radians","moduleName":"Math","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"Number"]},"arguments":[]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":"An alias to make types in this module more explicit.\n"}],"tag":"SearchResult"}]],["radix",[{"values":[{"sourceSpan":{"start":[170,1],"name":".spago/integers/v4.0.0/src/Data/Int.purs","end":[170,26]},"score":16,"packageInfo":{"values":["integers"],"tag":"Package"},"name":"Radix","moduleName":"Data.Int","info":{"values":[{"typeArguments":[],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"The number of unique digits (including zero) used to represent integers in\na specific base.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[193,1],"name":".spago/integers/v4.0.0/src/Data/Int.purs","end":[193,28]},"score":16,"packageInfo":{"values":["integers"],"tag":"Package"},"name":"radix","moduleName":"Data.Int","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Data","Int"],"Radix"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a `Radix` from a number between 2 and 36.\n"}],"tag":"SearchResult"}]],["rand",[{"values":[{"sourceSpan":{"start":[62,1],"name":"src/MCPrelude.purs","end":[62,31]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"rand","moduleName":"MCPrelude","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["MCPrelude"],"Seed"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeConstructor","contents":[["MCPrelude"],"Seed"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["range",[{"values":[{"sourceSpan":{"start":[174,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[174,41]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"range","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[174,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[174,48]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"range","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create an array containing a range of integers, including both endpoints.\n```purescript\nrange 2 5 = [2, 3, 4, 5]\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[107,1],"name":".spago/unfoldable/v4.1.0/src/Data/Unfoldable1.purs","end":[107,56]},"score":14,"packageInfo":{"values":["unfoldable"],"tag":"Package"},"name":"range","moduleName":"Data.Unfoldable1","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Unfoldable1"],"Unfoldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create an `Unfoldable1` containing a range of values, including both\nendpoints.\n\n``` purescript\nrange 0 0 == (NEL.singleton 0 :: NEL.NonEmptyList Int)\nrange 1 2 == (NEL.cons 1 (NEL.singleton 2) :: NEL.NonEmptyList Int)\nrange 2 0 == (NEL.cons 2 (NEL.cons 1 (NEL.singleton 0)) :: NEL.NonEmptyList Int)\n```\n"}],"tag":"SearchResult"}]],["odd",[{"values":[{"sourceSpan":null,"score":16,"packageInfo":{"values":["integers"],"tag":"Package"},"name":"Odd","moduleName":"Data.Int","info":{"values":[{"arguments":[]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[165,1],"name":".spago/integers/v4.0.0/src/Data/Int.purs","end":[165,22]},"score":16,"packageInfo":{"values":["integers"],"tag":"Package"},"name":"odd","moduleName":"Data.Int","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The negation of `even`.\n\n``` purescript\nodd 0 == false\nodd 1 == true\n```\n"}],"tag":"SearchResult"}]],["negate",[{"values":[{"sourceSpan":{"start":[43,1],"name":".spago/prelude/v4.1.1/src/Data/Ring.purs","end":[43,37]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"negate","moduleName":"Data.Ring","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ring"],"Ring"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`negate x` can be used as a shorthand for `zero - x`.\n"}],"tag":"SearchResult"}]],["nes",[{"values":[{"sourceSpan":{"start":[32,3],"name":".spago/strings/v4.0.2/src/Data/String/NonEmpty/Internal.purs","end":[32,36]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"nes","moduleName":"Data.String.NonEmpty.Internal","info":{"values":[{"typeClassArguments":[["s",{"tag":"NamedKind","contents":[["Prim"],"Symbol"]}]],"typeClass":[["Data","String","NonEmpty","Internal"],"MakeNonEmpty"],"type":{"tag":"ForAll","contents":["s",{"tag":"NamedKind","contents":[["Prim"],"Symbol"]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","String","NonEmpty","Internal"],"MakeNonEmpty"],"constraintArgs":[{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Symbol"],"SProxy"]},{"tag":"TypeVar","contents":"s"}]}]},{"tag":"TypeConstructor","contents":[["Data","String","NonEmpty","Internal"],"NonEmptyString"]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["new",[{"values":[{"sourceSpan":{"start":[93,1],"name":".spago/st/v4.1.1/src/Control/Monad/ST/Internal.purs","end":[93,56]},"score":7,"packageInfo":{"values":["st"],"tag":"Package"},"name":"new","moduleName":"Control.Monad.ST.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["r",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"ST"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"STRef"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a new mutable reference.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[18,1],"name":".spago/refs/v4.1.0/src/Effect/Ref.purs","end":[18,52]},"score":8,"packageInfo":{"values":["refs"],"tag":"Package"},"name":"new","moduleName":"Effect.Ref","info":{"values":[{"type":{"tag":"ForAll","contents":["s",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect"],"Effect"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Ref"],"Ref"]},{"tag":"TypeVar","contents":"s"}]}}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a new mutable reference containing the specified value.\n"}],"tag":"SearchResult"}]],["newtype",[{"values":[{"sourceSpan":{"start":[36,1],"name":".spago/newtype/v3.0.0/src/Data/Newtype.purs","end":[38,19]},"score":26,"packageInfo":{"values":["newtype"],"tag":"Package"},"name":"Newtype","moduleName":"Data.Newtype","info":{"values":[{"superclasses":[],"fundeps":[[["t"],["a"]]],"arguments":[["t",null],["a",null]]}],"tag":"TypeClassResult"},"hashAnchor":"t","comments":"A type class for `newtype`s to enable convenient wrapping and unwrapping,\nand the use of the other functions in this module.\n\nThe compiler can derive instances of `Newtype` automatically:\n\n``` purescript\nnewtype EmailAddress = EmailAddress String\n\nderive instance newtypeEmailAddress :: Newtype EmailAddress _\n```\n\nNote that deriving for `Newtype` instances requires that the type be\ndefined as `newtype` rather than `data` declaration (even if the `data`\nstructurally fits the rules of a `newtype`), and the use of a wildcard for\nthe wrapped type.\n\nInstances must obey the following laws:\n``` purescript\nunwrap <<< wrap = id\nwrap <<< unwrap = id\n```\n"}],"tag":"SearchResult"}]],["next",[{"values":[{"sourceSpan":{"start":[48,1],"name":".spago/arrays/v5.3.1/src/Data/Array/ST/Iterator.purs","end":[48,51]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"next","moduleName":"Data.Array.ST.Iterator","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["r",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","ST","Iterator"],"Iterator"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"ST"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Get the next item out of an iterator, advancing it. Returns Nothing if the\nIterator is exhausted.\n"}],"tag":"SearchResult"}]],["global",[{"values":[{"sourceSpan":{"start":[14,1],"name":".spago/st/v4.1.1/src/Control/Monad/ST/Global.purs","end":[14,37]},"score":7,"packageInfo":{"values":["st"],"tag":"Package"},"name":"Global","moduleName":"Control.Monad.ST.Global","info":{"values":[{"kind":{"tag":"NamedKind","contents":[["Control","Monad","ST","Internal"],"Region"]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"This region allows `ST` computations to be converted into `Effect`\ncomputations so they can be run in a global context.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[30,1],"name":".spago/strings/v4.0.2/src/Data/String/Regex/Flags.purs","end":[30,21]},"score":16,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"global","moduleName":"Data.String.Regex.Flags","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Data","String","Regex","Flags"],"RegexFlags"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Only global flag set to true\n"}],"tag":"SearchResult"}]],["encodeuri",[{"values":[{"sourceSpan":{"start":[82,1],"name":".spago/globals/v5.0.0/src/Global.purs","end":[82,36]},"score":6,"packageInfo":{"values":["globals"],"tag":"Package"},"name":"encodeURI","moduleName":"Global","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"URI encoding. Returns `Nothing` when given a value with unencodeable\ncharacters.\n"}],"tag":"SearchResult"}]],["encodeuricomponent",[{"values":[{"sourceSpan":{"start":[92,1],"name":".spago/globals/v5.0.0/src/Global.purs","end":[92,45]},"score":6,"packageInfo":{"values":["globals"],"tag":"Package"},"name":"encodeURIComponent","moduleName":"Global","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"URI component encoding. Returns `Nothing` when given a value with\nunencodeable characters.\n"}],"tag":"SearchResult"}]],["endo",[{"values":[{"sourceSpan":{"start":[14,1],"name":".spago/prelude/v4.1.1/src/Data/Monoid/Endo.purs","end":[14,32]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Endo","moduleName":"Data.Monoid.Endo","info":{"values":[{"typeArguments":[["c",null],["a",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"Monoid and semigroup for category endomorphisms.\n\nWhen `c` is instantiated with `->` this composes functions of type\n`a -> a`:\n\n``` purescript\nEndo f <> Endo g == Endo (f <<< g)\n(mempty :: Endo _) == Endo identity\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"Endo","moduleName":"Data.Monoid.Endo","info":{"values":[{"arguments":[{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"c"},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"a"}]}}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["endswith",[{"values":[{"sourceSpan":{"start":[133,1],"name":".spago/stringutils/v0.0.10/src/Data/String/Utils.purs","end":[133,40]},"score":1,"packageInfo":{"values":["stringutils"],"tag":"Package"},"name":"endsWith","moduleName":"Data.String.Utils","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Determine whether the second string ends with the first one.\n"}],"tag":"SearchResult"}]],["endswith'",[{"values":[{"sourceSpan":{"start":[140,1],"name":".spago/stringutils/v0.0.10/src/Data/String/Utils.purs","end":[140,48]},"score":1,"packageInfo":{"values":["stringutils"],"tag":"Package"},"name":"endsWith'","moduleName":"Data.String.Utils","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Determine whether the second string ends with the first one\nbut search as if the string were only as long as the given argument.\n"}],"tag":"SearchResult"}]],["enum",[{"values":[{"sourceSpan":{"start":[50,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[52,23]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"Enum","moduleName":"Data.Enum","info":{"values":[{"superclasses":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]}],"fundeps":[],"arguments":[["a",null]]}],"tag":"TypeClassResult"},"hashAnchor":"t","comments":"Type class for enumerations.\n\nLaws:\n- Successor: `all (a < _) (succ a)`\n- Predecessor: `all (_ < a) (pred a)`\n- Succ retracts pred: `pred >=> succ >=> pred = pred`\n- Pred retracts succ: `succ >=> pred >=> succ = succ`\n- Non-skipping succ: `b <= a || any (_ <= b) (succ a)`\n- Non-skipping pred: `a <= b || any (b <= _) (pred a)`\n\nThe retraction laws can intuitively be understood as saying that `succ` is\nthe opposite of `pred`; if you apply `succ` and then `pred` to something,\nyou should end up with what you started with (although of course this\ndoesn't apply if you tried to `succ` the last value in an enumeration and\ntherefore got `Nothing` out).\n\nThe non-skipping laws can intuitively be understood as saying that `succ`\nshouldn't skip over any elements of your type. For example, _without_ the\nnon-skipping laws, it would be permissible to write an `Enum Int` instance\nwhere `succ x = Just (x+2)`, and similarly `pred x = Just (x-2)`.\n"}],"tag":"SearchResult"}]],["enumfromthento",[{"values":[{"sourceSpan":{"start":[205,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[205,95]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"enumFromThenTo","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Unfoldable"],"Unfoldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Functor"],"Functor"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"BoundedEnum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]}]}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns a sequence of elements from the first value, taking steps\naccording to the difference between the first and second value, up to\n(but not exceeding) the third value.\n\n``` purescript\nenumFromThenTo 0 2 6 = [0, 2, 4, 6]\nenumFromThenTo 0 3 5 = [0, 3]\n```\n\nNote that there is no `BoundedEnum` instance for integers, they're just\nbeing used here for illustrative purposes to help clarify the behaviour.\n\nThe example shows `Array` return values, but the result can be any type\nwith an `Unfoldable1` instance.\n"}],"tag":"SearchResult"}]],["enumfromto",[{"values":[{"sourceSpan":{"start":[182,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[182,67]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"enumFromTo","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["u",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"Enum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Unfoldable1"],"Unfoldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"u"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"u"},{"tag":"TypeVar","contents":"a"}]}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Returns a contiguous sequence of elements from the first value to the\nsecond value (inclusive).\n\n``` purescript\nenumFromTo 0 3 = [0, 1, 2, 3]\nenumFromTo 'c' 'a' = ['c', 'b', 'a']\n```\n\nThe example shows `Array` return values, but the result can be any type\nwith an `Unfoldable1` instance.\n"}],"tag":"SearchResult"}]],["doc",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"Doc","moduleName":"Prim.TypeError","info":{"values":[],"tag":"ExternKindResult"},"hashAnchor":"k","comments":"`Doc` is the kind of type-level documents.\n\nThis kind is used with the `Fail` and `Warn` type clases.\nBuild up a `Doc` with `Text`, `Quote`, `QuoteLabel`, `Beside`, and `Above`.\n"}],"tag":"SearchResult"}]],["done",[{"values":[{"sourceSpan":null,"score":14,"packageInfo":{"values":["tailrec"],"tag":"Package"},"name":"Done","moduleName":"Control.Monad.Rec.Class","info":{"values":[{"arguments":[{"tag":"TypeVar","contents":"b"}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["down",[{"values":[{"sourceSpan":{"start":[12,1],"name":".spago/orders/v4.0.0/src/Data/Ord/Down.purs","end":[12,24]},"score":1,"packageInfo":{"values":["orders"],"tag":"Package"},"name":"Down","moduleName":"Data.Ord.Down","info":{"values":[{"typeArguments":[["a",null]],"dataDeclType":"newtype"}],"tag":"DataResult"},"hashAnchor":"t","comments":"A newtype wrapper which provides a reversed `Ord` instance. For example:\n\n    sortBy (comparing Down) [1,2,3] = [3,2,1]\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":null,"score":1,"packageInfo":{"values":["orders"],"tag":"Package"},"name":"Down","moduleName":"Data.Ord.Down","info":{"values":[{"arguments":[{"tag":"TypeVar","contents":"a"}]}],"tag":"DataConstructorResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["downfrom",[{"values":[{"sourceSpan":{"start":[229,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[229,59]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"downFrom","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["u",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"Enum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Unfoldable"],"Unfoldable"],"constraintArgs":[{"tag":"TypeVar","contents":"u"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"u"},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Produces all predecessors of an `Enum` value, excluding the start value.\n"}],"tag":"SearchResult"}]],["downfromincluding",[{"values":[{"sourceSpan":{"start":[236,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[236,69]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"downFromIncluding","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["u",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"Enum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Unfoldable1"],"Unfoldable1"],"constraintArgs":[{"tag":"TypeVar","contents":"u"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"u"},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Produces all predecessors of an `Enum` value, including the start value.\n\n`downFromIncluding top` will return all values in an `Enum`, in reverse\norder.\n"}],"tag":"SearchResult"}]],["array",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"Array","moduleName":"Prim","info":{"values":[{"kind":{"tag":"FunKind","contents":[{"tag":"NamedKind","contents":[["Prim"],"Type"]},{"tag":"NamedKind","contents":[["Prim"],"Type"]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"An Array: a data structure supporting efficient random access. In\nthe JavaScript backend, values of this type are represented as JavaScript\nArrays at runtime.\n\nConstruct values using literals:\n\n    x = [1,2,3,4,5] :: Array Int\n"}],"tag":"SearchResult"}]]]
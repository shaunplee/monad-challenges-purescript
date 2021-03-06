// This file was generated by purescript-docs-search.
window.DocsSearchIndex["1"] = [["e",[{"values":[{"sourceSpan":{"start":[73,1],"name":".spago/math/v2.1.1/src/Math.purs","end":[73,27]},"score":12,"packageInfo":{"values":["math"],"tag":"Package"},"name":"e","moduleName":"Math","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Prim"],"Number"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The base of natural logarithms, *e*, around 2.71828.\n"}],"tag":"SearchResult"}]],["decimal",[{"values":[{"sourceSpan":{"start":[181,1],"name":".spago/integers/v4.0.0/src/Data/Int.purs","end":[181,17]},"score":16,"packageInfo":{"values":["integers"],"tag":"Package"},"name":"decimal","moduleName":"Data.Int","info":{"values":[{"type":{"tag":"TypeConstructor","contents":[["Data","Int"],"Radix"]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The base-10 system.\n"}],"tag":"SearchResult"}]],["decodeuri",[{"values":[{"sourceSpan":{"start":[77,1],"name":".spago/globals/v5.0.0/src/Global.purs","end":[77,36]},"score":6,"packageInfo":{"values":["globals"],"tag":"Package"},"name":"decodeURI","moduleName":"Global","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"URI decoding. Returns `Nothing` when given a value with undecodeable\nescape sequences.\n"}],"tag":"SearchResult"}]],["decodeuricomponent",[{"values":[{"sourceSpan":{"start":[87,1],"name":".spago/globals/v5.0.0/src/Global.purs","end":[87,45]},"score":6,"packageInfo":{"values":["globals"],"tag":"Package"},"name":"decodeURIComponent","moduleName":"Global","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"URI component decoding. Returns `Nothing` when given a value with\nundecodeable escape sequences.\n"}],"tag":"SearchResult"}]],["defaultcardinality",[{"values":[{"sourceSpan":{"start":[270,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[270,69]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"defaultCardinality","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Bounded"],"Bounded"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"Enum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Enum"],"Cardinality"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Provides a default implementation for `cardinality`.\n\nRuns in `O(n)` where `n` is `fromEnum top`\n"}],"tag":"SearchResult"}]],["defaultfromenum",[{"values":[{"sourceSpan":{"start":[303,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[303,48]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"defaultFromEnum","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"Enum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Provides a default implementation for `fromEnum`.\n\n- Assumes `toEnum 0 = Just bottom`.\n- Cannot be used in conjuction with `defaultPred`.\n\nRuns in `O(n)` where `n` is `fromEnum a`.\n"}],"tag":"SearchResult"}]],["defaultpred",[{"values":[{"sourceSpan":{"start":[264,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[264,72]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"defaultPred","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Provides a default implementation for `pred`, given a function that maps\nintegers to values in the `Enum`, and a function that maps values in the\n`Enum` back to integers. The integer mapping must agree in both directions\nfor this to implement a law-abiding `pred`.\n\nIf a `BoundedEnum` instance exists for `a`, the `toEnum` and `fromEnum`\nfunctions can be used here:\n\n``` purescript\npred = defaultPred toEnum fromEnum\n```\n"}],"tag":"SearchResult"}]],["defaultsucc",[{"values":[{"sourceSpan":{"start":[250,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[250,72]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"defaultSucc","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Provides a default implementation for `succ`, given a function that maps\nintegers to values in the `Enum`, and a function that maps values in the\n`Enum` back to integers. The integer mapping must agree in both directions\nfor this to implement a law-abiding `succ`.\n\nIf a `BoundedEnum` instance exists for `a`, the `toEnum` and `fromEnum`\nfunctions can be used here:\n\n``` purescript\nsucc = defaultSucc toEnum fromEnum\n```\n"}],"tag":"SearchResult"}]],["defaulttoenum",[{"values":[{"sourceSpan":{"start":[283,1],"name":".spago/enums/v4.0.1/src/Data/Enum.purs","end":[283,65]},"score":9,"packageInfo":{"values":["enums"],"tag":"Package"},"name":"defaultToEnum","moduleName":"Data.Enum","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Bounded"],"Bounded"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Enum"],"Enum"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Provides a default implementation for `toEnum`.\n\n- Assumes `fromEnum bottom = 0`.\n- Cannot be used in conjuction with `defaultSucc`.\n\nRuns in `O(n)` where `n` is `fromEnum a`.\n"}],"tag":"SearchResult"}]],["defer",[{"values":[{"sourceSpan":{"start":[11,3],"name":".spago/control/v4.2.0/src/Control/Lazy.purs","end":[11,28]},"score":24,"packageInfo":{"values":["control"],"tag":"Package"},"name":"defer","moduleName":"Control.Lazy","info":{"values":[{"typeClassArguments":[["l",null]],"typeClass":[["Control","Lazy"],"Lazy"],"type":{"tag":"ForAll","contents":["l",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Lazy"],"Lazy"],"constraintArgs":[{"tag":"TypeVar","contents":"l"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]},{"tag":"TypeVar","contents":"l"}]}}]},{"tag":"TypeVar","contents":"l"}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["degree",[{"values":[{"sourceSpan":{"start":[64,3],"name":".spago/prelude/v4.1.1/src/Data/EuclideanRing.purs","end":[64,21]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"degree","moduleName":"Data.EuclideanRing","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Data","EuclideanRing"],"EuclideanRing"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","EuclideanRing"],"EuclideanRing"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]],["delete",[{"values":[{"sourceSpan":{"start":[376,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[376,60]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"delete","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Eq"],"Eq"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[969,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[969,52]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"delete","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Eq"],"Eq"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Delete the first element of an array which is equal to the specified value,\ncreating a new array.\n\n```purescript\ndelete 7 [1, 7, 3, 7] = [1, 3, 7]\ndelete 7 [1, 2, 3] = [1, 2, 3]\n```\n\nRunning time: `O(n)`\n"}],"tag":"SearchResult"}]],["deleteat",[{"values":[{"sourceSpan":{"start":[254,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[254,64]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"deleteAt","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[483,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[483,56]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"deleteAt","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Delete the element at the specified index, creating a new array, or\nreturning `Nothing` if the index is out of bounds.\n\n```purescript\ndeleteAt 0 [\"Hello\", \"World\"] = Just [\"World\"]\ndeleteAt 10 [\"Hello\", \"World\"] = Nothing\n```\n\n"}],"tag":"SearchResult"}]],["deleteby",[{"values":[{"sourceSpan":{"start":[379,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[379,77]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"deleteBy","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[981,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[981,69]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"deleteBy","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Delete the first element of an array which matches the specified value,\nunder the equivalence relation provided in the first argument, creating a\nnew array.\n\n```purescript\nmod3eq a b = a `mod` 3 == b `mod` 3\ndeleteBy mod3eq 6 [1, 3, 4, 3] = [1, 4, 3]\n```\n\n"}],"tag":"SearchResult"}]]]
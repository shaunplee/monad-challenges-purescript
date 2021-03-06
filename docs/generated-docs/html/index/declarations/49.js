// This file was generated by purescript-docs-search.
window.DocsSearchIndex["49"] = [["fail",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"Fail","moduleName":"Prim.TypeError","info":{"values":[{"superclasses":[],"fundeps":[],"arguments":[["message",{"tag":"NamedKind","contents":[["Prim","TypeError"],"Doc"]}]]}],"tag":"TypeClassResult"},"hashAnchor":"t","comments":"The Fail type class is part of the custom type errors feature. To provide\na custom type error when someone tries to use a particular instance,\nwrite that instance out with a Fail constraint.\n\nFor more information, see\n[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md).\n"}],"tag":"SearchResult"}]],["false",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"False","moduleName":"Prim.Boolean","info":{"values":[{"kind":{"tag":"NamedKind","contents":[["Prim","Boolean"],"Boolean"]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"The 'False' boolean type.\n"}],"tag":"SearchResult"}]],["beside",[{"values":[{"sourceSpan":null,"score":0,"packageInfo":{"values":[],"tag":"Builtin"},"name":"Beside","moduleName":"Prim.TypeError","info":{"values":[{"kind":{"tag":"FunKind","contents":[{"tag":"NamedKind","contents":[["Prim","TypeError"],"Doc"]},{"tag":"FunKind","contents":[{"tag":"NamedKind","contents":[["Prim","TypeError"],"Doc"]},{"tag":"NamedKind","contents":[["Prim","TypeError"],"Doc"]}]}]}}],"tag":"ExternDataResult"},"hashAnchor":"t","comments":"The Beside type constructor combines two Docs horizontally\nto be used in a custom type error.\n\nFor more information, see\n[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md).\n"}],"tag":"SearchResult"}]],["between",[{"values":[{"sourceSpan":{"start":[194,1],"name":".spago/prelude/v4.1.1/src/Data/Ord.purs","end":[194,53]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"between","moduleName":"Data.Ord","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Test whether a value is between a minimum and a maximum (inclusive).\nFor example:\n\n``` purescript\nlet f = between 0 10\nf 0    == true\nf (-5) == false\nf 5    == true\nf 10   == true\nf 15   == false\n```\n"}],"tag":"SearchResult"}]]]
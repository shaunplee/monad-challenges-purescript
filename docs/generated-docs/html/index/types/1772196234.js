// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1772196234"] = [{"values":[{"sourceSpan":{"start":[25,3],"name":".spago/prelude/v4.1.1/src/Data/Functor.purs","end":[25,44]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"map","moduleName":"Data.Functor","info":{"values":[{"typeClassArguments":[["f",null]],"typeClass":[["Data","Functor"],"Functor"],"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Functor"],"Functor"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"b"}]}]}]}]},null]},null]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[47,1],"name":".spago/prelude/v4.1.1/src/Control/Monad.purs","end":[47,58]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"liftM1","moduleName":"Control.Monad","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["m",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"b"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`liftM1` provides a default implementation of `(<$>)` for any\n[`Monad`](#monad), without using `(<$>)` as provided by the\n[`Functor`](#functor)-[`Monad`](#monad) superclass relationship.\n\n`liftM1` can therefore be used to write [`Functor`](#functor) instances\nas follows:\n\n```purescript\ninstance functorF :: Functor F where\n  map = liftM1\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[53,1],"name":".spago/prelude/v4.1.1/src/Control/Applicative.purs","end":[53,64]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"liftA1","moduleName":"Control.Applicative","info":{"values":[{"type":{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Applicative"],"Applicative"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"f"},{"tag":"TypeVar","contents":"b"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`liftA1` provides a default implementation of `(<$>)` for any\n[`Applicative`](#applicative) functor, without using `(<$>)` as provided\nby the [`Functor`](#functor)-[`Applicative`](#applicative) superclass\nrelationship.\n\n`liftA1` can therefore be used to write [`Functor`](#functor) instances\nas follows:\n\n```purescript\ninstance functorF :: Functor F where\n  map = liftA1\n```\n"}],"tag":"SearchResult"}]
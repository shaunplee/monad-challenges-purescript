// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1649597852"] = [{"values":[{"sourceSpan":{"start":[99,1],"name":".spago/foldable-traversable/v4.1.1/src/Data/TraversableWithIndex.purs","end":[105,13]},"score":36,"packageInfo":{"values":["foldable-traversable"],"tag":"Package"},"name":"forWithIndex","moduleName":"Data.TraversableWithIndex","info":{"values":[{"type":{"tag":"ForAll","contents":["t",{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["i",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Applicative"],"Applicative"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","TraversableWithIndex"],"TraversableWithIndex"],"constraintArgs":[{"tag":"TypeVar","contents":"i"},{"tag":"TypeVar","contents":"t"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"t"},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"TypeVar","contents":"b"}]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"m"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeVar","contents":"t"},{"tag":"TypeVar","contents":"b"}]}}]}]}]}]}]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A version of `traverseWithIndex` with its arguments flipped.\n\n\nThis can be useful when running an action written using do notation\nfor every element in a data structure:\n\nFor example:\n\n```purescript\nfor [1, 2, 3] \\i x -> do\n  logShow i\n  pure (x * x)\n```\n"}],"tag":"SearchResult"}]
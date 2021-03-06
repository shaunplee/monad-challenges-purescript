// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["828621320"] = [{"values":[{"sourceSpan":{"start":[139,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[139,53]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"toUnfoldable","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Unfoldable"],"Unfoldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","NaturalTransformation"],"~>"]},{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"f"}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Convert an `Array` into an `Unfoldable` structure.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[154,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[154,51]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"fromFoldable","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Foldable"],"Foldable"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","NaturalTransformation"],"~>"]},{"tag":"TypeVar","contents":"f"},{"tag":"TypeConstructor","contents":[["Prim"],"Array"]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Convert a `Foldable` structure into an `Array`.\n\n```purescript\nfromFoldable (Just 1) = [1]\nfromFoldable (Nothing) = []\n```\n\n"}],"tag":"SearchResult"}]
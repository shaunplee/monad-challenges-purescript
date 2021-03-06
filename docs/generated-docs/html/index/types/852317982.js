// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["852317982"] = [{"values":[{"sourceSpan":{"start":[342,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[342,68]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"uncons","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"RCons","contents":["head",{"tag":"TypeVar","contents":"a"},{"tag":"RCons","contents":["tail",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"REmpty","contents":{}}]}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Break an array into its first element and remaining elements.\n\nUsing `uncons` provides a way of writing code that would use cons patterns\nin Haskell or pre-PureScript 0.7:\n``` purescript\nf (x : xs) = something\nf [] = somethingElse\n```\nBecomes:\n``` purescript\nf arr = case uncons arr of\n  Just { head: x, tail: xs } -> something\n  Nothing -> somethingElse\n```\n"}],"tag":"SearchResult"}]
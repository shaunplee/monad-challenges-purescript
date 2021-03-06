// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1123272490"] = [{"values":[{"sourceSpan":{"start":[106,1],"name":".spago/prelude/v4.1.1/src/Data/Function.purs","end":[106,61]},"score":84,"packageInfo":{"values":["prelude"],"tag":"Package"},"name":"on","moduleName":"Data.Function","info":{"values":[{"type":{"tag":"ForAll","contents":["c",{"tag":"ForAll","contents":["b",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"c"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"c"}]}]}]}]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The `on` function is used to change the domain of a binary operator.\n\nFor example, we can create a function which compares two records based on the values of their `x` properties:\n\n```purescript\ncompareX :: forall r. { x :: Number | r } -> { x :: Number | r } -> Ordering\ncompareX = compare `on` _.x\n```\n"}],"tag":"SearchResult"}]
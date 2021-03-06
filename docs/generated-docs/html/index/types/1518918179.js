// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1518918179"] = [{"values":[{"sourceSpan":{"start":[851,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[851,64]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"group'","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sort and then group the elements of an array into arrays.\n\n```purescript\ngroup' [1,1,2,2,1] == [NonEmpty 1 [1,1],NonEmpty 2 [2]]\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[843,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[843,62]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"group","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Eq"],"Eq"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Group equal, consecutive elements of an array into arrays.\n\n```purescript\ngroup [1,1,2,2,1] == [NonEmpty 1 [1], NonEmpty 2 [2], NonEmpty 1 []]\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[323,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[323,45]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"init","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Get all but the last element of an array, creating a new array, or\n`Nothing` if the array is empty.\n\n```purescript\ninit [1, 2, 3, 4] = Just [1, 2, 3]\ninit [] = Nothing\n```\n\nRunning time: `O(n)` where `n` is the length of the array\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[311,1],"name":".spago/arrays/v5.3.1/src/Data/Array.purs","end":[311,45]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"tail","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Get all but the first element of an array, creating a new array, or\n`Nothing` if the array is empty\n\n```purescript\ntail [1, 2, 3, 4] = Just [2, 3, 4]\ntail [] = Nothing\n```\n\nRunning time: `O(n)` where `n` is the length of the array\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[134,1],"name":".spago/arrays/v5.3.1/src/Data/Array/NonEmpty.purs","end":[134,58]},"score":20,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"fromArray","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]
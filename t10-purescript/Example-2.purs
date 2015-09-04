module Example where

import Prelude

brian :: { name :: String, age :: Int }
brian = { name: "Brian", age: 25 }

getName :: forall r. { name :: String | r } -> String
getName { name = n } = n

data Hello = Hello Int String
           | World String

getHello :: Hello -> String
getHello (Hello _ s) = s
getHello (World s)   = s

hello :: Hello
hello Hello 0 "Hello"

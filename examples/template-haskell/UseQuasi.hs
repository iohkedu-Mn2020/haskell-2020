{-# LANGUAGE QuasiQuotes #-}

module UseQuasi where

import Quasi
import Text.Hamlet
import Text.Blaze.Html.Renderer.Text

fortyTwo :: Double
fortyTwo = [ternary|1120|]

weird :: Int -> String
weird 37              = "hmmm - 37!"
weird [ternary|1120|] = "42"
weird _               = "something else"

myHtml = [shamlet|
    <html>
        <head>
            <title>Haskell
        <body>
            <h1>Haskell Course
            <p>Haskell is great!
|]

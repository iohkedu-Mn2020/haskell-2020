import           P2P (node, parseConfig)

main :: IO ()
main = do
    cfg <- parseConfig
    print cfg
    node cfg

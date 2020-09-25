import Hello
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    run 8081 $ userApp "user.db"

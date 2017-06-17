module Main where

import           Control.Exception
import           Control.Monad.Reader
import           Data.List
import           Network
import           System.Exit
import           System.IO
import           Text.Printf

server = "irc.freenode.org"
port = 6667
chan = "#tutbot-testing"
nick = "randomGuy1"

newtype Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop = runReaderT run

connect :: IO Bot
connect = notify $ do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  return $ Bot h
  where
    notify = bracket_
        (printf "Connecting to %s ..." server >> hFlush stdout)
        (putStrLn "done.")

run :: Net ()
run = do
  write "NICK" nick
  write "USER" (nick ++ " 0 * :tutorial bot")
  write "JOIN" chan
  listen

listen :: Net ()
listen = forever $ do
  h <- asks socket
  s <- init <$> io (hGetLine h)
  io $ putStrLn s
  if ping s
    then pong s
  else eval (clean s)
  where
    clean = drop 1 . dropWhile (/= ':') . drop 1
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval "!quit" = write "QUIT" ":Exiting" >> io exitSuccess
eval x       | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval _       = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  io $ hPrintf h "%s %s\r\n" s t
  io $ printf "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO

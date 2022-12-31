-- ghc --make day6-solution.hs && ./day6-solution
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Either as Either
import System.IO

main = do
  handle <- openFile "day6-input.txt" ReadMode
  signalInput <- hGetContents handle

  let
      folder :: Int -> Either (Int, String) Int -> Char ->  Either (Int, String) Int
      folder messageLength eitherAcc char =
          let
              newSequence :: [Char] -> [Char]
              newSequence oldSequence = List.concat [List.drop 1 oldSequence, [char]]

              setSize :: [Char] -> Int
              setSize oldSequence = Set.size $ Set.fromList $ newSequence oldSequence

              newAcc :: Int -> [Char] -> Either (Int, String) Int
              newAcc idx oldSequence =
                  if (setSize oldSequence) == messageLength then
                      Either.Right idx
                  else
                      Either.Left (idx + 1 , newSequence oldSequence)

          in
          case eitherAcc of
              Either.Right startIndex ->
                  Either.Right startIndex

              Either.Left (idx, oldSequence) ->
                   newAcc idx oldSequence

      calculatePosition :: Int -> Either (Int, String) Int
      calculatePosition messageLength =
          let
              initSequence :: [Char]
              initSequence = List.take messageLength signalInput

              tailSequence :: [Char]
              tailSequence = List.drop messageLength signalInput
          in
          List.foldl (folder messageLength)
                     (Either.Left (messageLength, initSequence))
                     tailSequence

  case (calculatePosition 4, calculatePosition 14) of
      (Either.Right firstAnswer, Either.Right secondAnswer) ->
          putStr $ "First answer: " ++
                   (show $ firstAnswer + 1) ++
                   "\nSecond answer: " ++
                   (show $ secondAnswer + 1)

      (_, _) ->
          putStr $ "Found no signal!"

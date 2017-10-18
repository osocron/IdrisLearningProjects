module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" args = Just (Add args)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" query = Just (Search query)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) ->
           (store : DataStore) ->
           Maybe (String, DataStore)
getEntry pos store = let storeItems = items store in
                         case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              (Just id) => Just (index id storeItems ++ "\n", store)

mapIndeces : (store : DataStore) ->
             (xs : List (Fin (size store))) ->
             List (String, DataStore)
mapIndeces store xs = map (\pos => ((show (finToNat pos)) ++ ": " ++ (index pos (items store)), store)) xs

reduce : List (String, DataStore) -> (String, DataStore)
reduce xs = foldl (\(acc, old), (entry, store) => (acc ++ entry ++ "\n", store)) ("", MkData _ []) xs

searchQuery : (query : String) ->
              (store : DataStore) ->
              Maybe (String, DataStore)
searchQuery query store = let indices = findIndices (\str => isInfixOf query str) (items store) in
                              Just (reduce (mapIndeces store indices))

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              (Just (Get pos)) => getEntry pos store
                              (Just (Search query)) => searchQuery query store
                              (Just Size) => Just ((show $ size store) ++ "\n", store)
                              (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput

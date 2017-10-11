module Main

betterSubstr : (start: Nat) -> (howMany: Nat) -> ?StringWithLength (start + howMany) -> String
betterSubstr start howMany withLength = substr start howMany ?missing

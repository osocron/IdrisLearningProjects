module Main

record Character Franchise where
  constructor MkCharacter
  name: String

record Franchise where
  constructor MkFranchise
  name : String

createFanFiction : Character starTrek ->
                   Character starTrek ->
                   (Character starTrek, Character starTrek)
createFanFiction x y = (x, y)

starTrek : Franchise
starTrek = MkFranchise "Star Trek"

starWars : Franchise
starWars = MkFranchise "Star Wars"

quark : Character starTrek
quark = MkCharacter "Quark"

jadzia : Character starWars
jadzia = MkCharacter "Jadzia Dax"

luke : Character starWars
luke = MkCharacter "Luke Skywalker"

yoda : Character starWars
yoda = MkCharacter "Yoda"

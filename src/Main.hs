module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact

width, height, offset :: Int
width = 800
height = 600
offset = 100
fps = 60

playerSpeed = 4.0

carWidth = 20
carLength = 30

window :: Display
window = InWindow "Lambda Racing 2019" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = play window background fps initialState draw handleKeys update

update :: Float -> LambdaRacing -> LambdaRacing
update seconds game = (movePlayers) game

type Position = (Float, Float)
data PlayerMovement = PlayerLeft | PlayerStill | PlayerRight deriving Show

data LambdaRacing = Game
  { player :: Position
  , playerMovement :: PlayerMovement
  } deriving Show

initialState :: LambdaRacing
initialState = Game
  (120.0, 100.0) PlayerStill

mkCar :: Position -> Picture
mkCar (x, y) = pictures
  [ translate x y $ color orange $ rectangleSolid (2 * carWidth) (2 * carLength)
  ]

draw :: LambdaRacing -> Picture
draw game = pictures [mkCar (player game)]

movePlayer :: Position -> PlayerMovement -> Position
movePlayer (px, py) PlayerLeft = (px - playerSpeed, py)
movePlayer (px, py) PlayerStill = (px, py)
movePlayer (px, py) PlayerRight = (px + playerSpeed, py)

movePlayers :: LambdaRacing -> LambdaRacing
movePlayers game = game {  player = movePlayer (player game) (playerMovement game)
                        }
handleKeys :: Event -> LambdaRacing -> LambdaRacing
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { playerMovement = PlayerLeft }
--handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { playerMovement = PlayerStill }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { playerMovement = PlayerRight }
--handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { playerMovement = PlayerStill }

handleKeys _ game = game

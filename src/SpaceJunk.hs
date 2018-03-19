module SpaceJunk where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
--import System.Random

-- | Запустить моделирование с заданным начальным состоянием вселенной.
startGame :: Images -> IO ()
startGame images
  = play display bgColor fps initGame (drawGameState images) handleGame updateGameState
  where
    display = FullScreen --InWindow "Монополия" (screenWidth, screenHeight) (10, 10)
    bgColor = white    -- цвет фона
    fps     = 100      -- кол-во кадров в секунду

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just pieceRed     <- loadJuicyPNG "images/pieceRed.png"
  Just pieceBlue    <- loadJuicyPNG "images/pieceBlue.png"
  Just pieceGreen   <- loadJuicyPNG "images/pieceGreen.png"
  Just pieceYellow  <- loadJuicyPNG "images/pieceYellow.png"
  Just playingField <- loadJuicyPNG "images/field.png"
  Just payMenu <- loadJuicyPNG "images/payMenu.png"
  return Images
    { imagePieceRed    = scale 0.1 0.1 pieceRed
    , imagePieceBlue   = scale 0.1 0.1 pieceBlue
    , imagePieceGreen  = scale 0.1 0.1 pieceGreen
    , imagePieceYellow = scale 0.1 0.1 pieceYellow
    , imagePlayingField = playingField
    , imagePayMenu = scale 0.6 0.6 payMenu
    }

-- =========================================
-- Модель
-- =========================================

-- | Изображения объектов.
data Images = Images
  { imagePieceRed    :: Picture   -- ^ Изображение фишек.
  , imagePieceBlue   :: Picture
  , imagePieceGreen  :: Picture
  , imagePieceYellow :: Picture
  , imagePlayingField :: Picture
  , imagePayMenu :: Picture
  }

data GameState = GameState
  { players :: [Player]
  , gamePlayer :: Int
  , haveWinner :: Maybe Int
  , cubes :: Cubes
  , land :: [Street]
  , typeStep :: String
  }

data Player = Player
  { colour :: Int
  --, name :: String
  , money :: Int
  , property :: [Street]
  , playerCell :: Int
  , playerPosition :: Point
  --, position :: Int
  }

data Street = Street
  { name :: String
  , zone :: Int
  , price :: Int
  , isRent :: Bool
  , priceRent :: Int
  , owner :: Int
  }

data Cubes = Cubes
  { firstCube :: Int
  , secondCube :: Int
  }

-- | Объект с изменяемым положением.
class Physical a where
  getPosition :: a -> Point
  getCell :: a -> Int
  setPosition :: Point -> a -> a
  setCell :: Int -> a -> a
  
-- | Переместить физический объект.
--
-- prop> \(object :: Asteroid) -> move 0 object == object
-- prop> \(object :: Asteroid) -> move x (move y object) ~= move (x + y) object
move :: Physical a => Float -> a -> a
move dt object = setPosition new object
  where
    new = getPosition object

-- | Фишка.
{-data Piece = Piece
  { piecePosition :: Point      -- ^ Положение фишки.
  , pieceCell :: Int
  --, pieceSize :: Float
  } deriving (Eq, Show)
-}

instance Physical Player where
  getPosition = playerPosition
  getCell = playerCell
  setPosition new player = player
    { playerPosition = new }
  setCell new player = player
    { playerCell = new }


-- | Сгенерировать начальное состояние игры.
initGame :: GameState
initGame = GameState
  { players = 
      [ Player 
        { colour = 1
        , money = 15000
        , property = []
        , playerCell = 1
        , playerPosition = getPlayerPosition 1 1
        }
      ,  Player 
        { colour = 2
        , money = 15000
        , property = []
        , playerCell = 1
        , playerPosition = getPlayerPosition 2 1
        }
      , Player 
        { colour = 3
        , money = 15000
        , property = []
        , playerCell = 1
        , playerPosition = getPlayerPosition 3 1
        }
      , Player 
        { colour = 4
        , money = 15000
        , property = []
        , playerCell = 1
        , playerPosition = getPlayerPosition 4 1
        }
      ]
  , cubes = Cubes 
      { firstCube = 1
      , secondCube = 0
      }
  , haveWinner = Nothing
  , gamePlayer = 1
  , typeStep = "ход"
  , land = 
    [ Street 
      { name ="Старт"
      , zone = 0
      }
    , Street
      { name = "СКИ Квантовая информатика"
      , price = 60
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Общественная казна"
      , isRent = False
      }
    , Street 
      { name = "СКИ Параллельные вычисления"
      , price = 60
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Налог"
      , price =200
      , priceRent = 0
      }
    , Street 
      { name = "Машзал 1"
      , price = 200
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "СП ПЦД"
      , price = 100
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Шанс"
      , zone = 15
      }
    , Street 
      { name = "СП УДИС"
      , price = 100
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "СП Корректность программ"
      , price = 120
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Академ"
      , zone = 17
      }
    , Street
      { name = "МатКиб ДММК"
      , price = 140
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Потеряшки"
      , price = 150
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "МатКиб ДФСА"
      , price = 140
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Маткиб Дискретный анализ"
      , price = 160
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Машзал 2"
      , price = 200
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "ИО Морозов"
      , price = 180
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Общественная казна"
      , isRent = False
      }
    , Street 
      { name = "ИО Новикова"
      , price = 180
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "ИО Денисов"
      , price = 200
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Бесплатная курилка"
      }
    , Street 
      { name = "МАТСТАТ Теория рисков"
      , price = 220
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Шанс"
      }
    , Street 
      { name = "МАТСТАТ ДГМС"
      , price = 220
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "МАТСТАТ МОТВЫ"
      , price = 240
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Машзал 3"
      , price = 200
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "АСВК ЛБИС"
      , price = 260
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "АСВК ЛВК"
      , price = 260
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Техносфера"
      , price = 150
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "АСВК Медиалаб"
      , price = 280
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Отправляйся в академ"
      }
    , Street 
      { name = "АЯ Парадигмы программирования"
      , price = 300
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "АЯ Компьютерная лингвистика"
      , price = 300
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Общественая казна"
      }
    , Street 
      { name = "АЯ Искусственный интеллект"
      , price = 320
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Машзал 4"
      , price = 200
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Шанс"
      }
    , Street 
      { name = "ММП МАТ"
      , price = 350
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    , Street 
      { name = "Сверхналог"
      , price = 100
      }
    , Street 
      { name = "ММП БММО"
      , price = 400
      , priceRent = 0
      , isRent = False
      , owner = 0
      }
    ]
  }


-- =========================================
-- Функции отрисовки
-- =========================================

-- | Отобразить состояние игры.
drawGameState :: Images -> GameState -> Picture
drawGameState images gameState 
    | (typeStep gameState) == "ход" ||
      (typeStep gameState) == "старт" = pictures
        [ drawPlayingField (imagePlayingField images)
        , drawPiece (imagePieceRed images) player1
        , drawPiece (imagePieceBlue  images) player2
        , drawPiece (imagePieceGreen  images) player3
        , drawPiece (imagePieceYellow  images) player4
        ]
    | (typeStep gameState) == "улица" = pictures
        [ drawPlayingField (imagePlayingField images)
        , drawPiece (imagePieceRed images) player1
        , drawPiece (imagePieceBlue  images) player2
        , drawPiece (imagePieceGreen  images) player3
        , drawPiece (imagePieceYellow  images) player4
        , drawPayMenu (imagePayMenu images)
        ]
    | (typeStep gameState) == "" = pictures[]
  where
    player1 = ((players gameState) !! 0)
    player2 = ((players gameState) !! 1)
    player3 = ((players gameState) !! 2)
    player4 = ((players gameState) !! 3)


drawPayMenu :: Picture -> Picture
drawPayMenu image = translate 0 0 image

-- | Отобразить фишки.
drawPiece :: Picture -> Player -> Picture
drawPiece image player = translate x y (scale r r image)
  where
    (x, y) = getPosition player
    r = 2

drawPlayingField :: Picture -> Picture
drawPlayingField image = translate 0 0 image --(scale r r image)
  --where
    --r = 1
-- =========================================
-- Обработка событий
-- =========================================

handleGame :: Event -> GameState -> GameState
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) gameState
    | (typeStep gameState) == "ход" && (isStep mouse) = doStep mouse gameState
    | ((typeStep gameState) == "улица") = case (isPay mouse) of
        Just True -> makePay gameState
        Just False -> gameState
          { typeStep = "ход"
          , gamePlayer = (mod (gamePlayer gameState) 4) + 1
          }
        Nothing -> gameState
    | otherwise = gameState
handleGame _ gameState = gameState

isStep :: Point -> Bool
isStep _ = True

isntPay :: Point -> Bool
isntPay (x, y) | x > 0 = True
             | otherwise = False

isPay :: Point -> Maybe Bool
isPay (x, y) | x < 0 && x > -100 && y > -50 && y < 50 = Just True
             | x > 0 && x < 100 && y > -50 && y < 50 = Just False
             | otherwise = Nothing

doStep :: Point -> GameState -> GameState
doStep point gameState =
  case (haveWinner gameState) of
    Just _ -> id gameState
    Nothing ->  case canGo gameState of
          Nothing -> gameState
          Just a -> makeMove gameState


canGo :: GameState -> Maybe GameState
canGo gameState = Just (gameState)

makeMove :: GameState -> GameState
makeMove gameState = (makeStepFeatures (changePlayerCell (throwCubes gameState)))

makeStepFeatures :: GameState -> GameState
makeStepFeatures gameState
    | field == "старт" = gameState
    | field == "налог" = payTax gameState 200
    | field == "сверхналог" = payTax gameState 100
    | field == "стоянка" = gameState { gamePlayer = mod (gamePlayer gameState) 4 + 1}
    | otherwise = gameState
  where
    field = (typeStep gameState)

payTax :: GameState -> Int -> GameState
payTax gameState count = gameState
    { players = firstPlayers ++ [(player) { money = (money (player)) - count}]
        ++ lastPlayers
    , gamePlayer = mod (gamePlayer gameState) 4 + 1
    }
  where
    firstPlayers = take ((gamePlayer gameState) - 1) (players gameState)
    player = (players gameState) !! ((gamePlayer gameState) - 1)
    lastPlayers = reverse (take (length (players gameState) - (gamePlayer gameState)) (reverse (players gameState)))

throwCubes :: GameState -> GameState
throwCubes gameState = gameState
    { cubes = Cubes
        { firstCube = 1
        , secondCube = 2
        }
    }

makePay :: GameState -> GameState
makePay gameState = gameState 
    { typeStep = "ход"
    , gamePlayer = (mod (gamePlayer gameState) 4) + 1
    }



handlePay :: Event -> GameState -> GameState
handlePay (EventKey (MouseButton LeftButton) Down _ mouse) = id
handlePay _ = id

updatePay :: Float -> GameState -> GameState
updatePay _ = id

initPay :: GameState -> GameState
initPay gameState = gameState

changePlayerCell :: GameState -> GameState
changePlayerCell gameState = gameState
    { players = firstPlayers ++ [(movePlayer player (gamePlayer gameState) cubesSum)] ++ lastPlayers
    , typeStep = getTypeCell (playerCell (movePlayer player (gamePlayer gameState) cubesSum))
    }
{-| (gamePlayer gameState) == 2 = gameState
    { player2  = movePlayer (player2 gameState) 2 cubesSum
    , typeStep = getTypeCell (playerCell (movePlayer (player2 gameState) 2 cubesSum))
    }
  | (gamePlayer gameState) == 3 = gameState
    { player3  = movePlayer (player3 gameState) 3 cubesSum
    , typeStep = getTypeCell (playerCell (movePlayer (player3 gameState) 3 cubesSum))
    }
  | (gamePlayer gameState) == 4 = gameState
    { player4  = movePlayer (player4 gameState) 4 cubesSum
    , typeStep = getTypeCell (playerCell (movePlayer (player4 gameState) 4 cubesSum))
    }-}
  where
    firstPlayers = take ((gamePlayer gameState) - 1) (players gameState)
    player = (players gameState) !! ((gamePlayer gameState) - 1)
    lastPlayers = reverse (take (length (players gameState) - (gamePlayer gameState)) (reverse (players gameState)))
    cubesSum = (firstCube (cubes gameState)) + (secondCube (cubes gameState))

getTypeCell :: Int -> String
getTypeCell num 
  | num == 1 = "старт"
  | num == 3 || num == 18 || num == 34 = "казна"
  | num == 5 = "налог"
  | num == 8 || num == 23 || num == 37 = "шанс"
  | num == 11 || num == 21 = "стоянка"
  | num == 31 = "тюрьма"
  | num == 39 = "сверхналог"
  | otherwise = "улица"

movePlayer :: Player -> Int -> Int -> Player
movePlayer player num cubesSum = player 
    { playerCell = (mod ((playerCell player) + cubesSum - 1) 40) + 1
    , playerPosition = getPlayerPosition num ((mod ((playerCell player) + cubesSum - 1) 40) + 1)
    }

chanceCard :: Player -> Player
chanceCard player = player
--здесь написать различные варианты карточки шанс

actionCell :: Player -> Int -> GameState -> Int -> Player
actionCell player colour gameState num 
    | (zone ((land gameState) !! (num - 1)) ) == 0 = player {money = (money player) + 200}
    | (zone ((land gameState) !! (num - 1)) ) == 15 = chanceCard player
    | (zone ((land gameState) !! (num - 1)) ) == 16 = player {money = (money player) - 200}
    | (zone ((land gameState) !! (num - 1)) ) == 17 = player
    | (zone ((land gameState) !! (num - 1)) ) == 18 = player {money = (money player) - 200, playerCell = 11}
    | (zone ((land gameState) !! (num - 1)) ) == 19 = player {money = (money player) - 100}
    | otherwise = player 
--здесь нужно для всех остальных ячеек придумать функцию, которая будет предлагать
--либо купить недвижимость, либо отказаться, и проверять принадлежит ли она кому



getPlayerPosition :: Int -> Int -> Point
getPlayerPosition colour num
    | (num >= 1  && num <= 11) =  (fromIntegral (-375 + 15 + colour * 15), fromIntegral (-365 + 15 + (num) * 60))
    | (num >= 12 && num <= 21) = (fromIntegral (-365 + (num - 10) * 60), fromIntegral (375 - 15 - colour * 15))
    | (num >= 22 && num <= 31) = (fromIntegral (375 - 15 - colour * 15), fromIntegral (365 - (num - 20) * 60))
    | otherwise = (fromIntegral(365 - (num - 30) * 60), fromIntegral(-375 + 15 + colour * 15))


-- =========================================
-- Функции обновления
-- =========================================

-- | Обновить космический мусор.
updateGameState :: Float -> GameState -> GameState
updateGameState _ = id

-- =========================================
-- Параметры моделирования
-- =========================================

-- | Ширина экрана.
screenWidth :: Num a => a
screenWidth = 800

-- | Высота экрана.
screenHeight :: Num a => a
screenHeight = 800

-- | Ускорение НЛО.
ufoAccel :: Float
ufoAccel = 15

-- | Скорость вращения спутников.
satelliteRotationSpeed :: Float
satelliteRotationSpeed = 0.1

-- =========================================
-- Секция для настроек автоматических тестов
-- =========================================

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> class AlmostEq a where (~=) :: a -> a -> Bool
-- >>> instance AlmostEq Float where x ~= y = x == y || abs (x - y) / max (abs x) (abs y) < 0.001
-- >>> instance (AlmostEq a, AlmostEq b) => AlmostEq (a, b) where (x, y) ~= (u, v) = x ~= u && y ~= v
-- >>> instance AlmostEq Asteroid where Asteroid p1 v1 s1 ~= Asteroid p2 v2 s2 = p1 ~= p2 && v1 ~= v2 && s1 ~= s2
-- >>> instance Arbitrary Asteroid where arbitrary = Asteroid <$> arbitrary <*> arbitrary <*> arbitrary

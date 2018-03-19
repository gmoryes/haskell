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
  { player1 :: Player
  , player2 :: Player
  , player3 :: Player
  , player4 :: Player
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
  , isrent :: Bool
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
  { player1 = Player 
      { colour = 1
      , money = 15000
      , property = []
      , playerCell = 1
      , playerPosition = getPlayerPosition 1 1
      }
  , player2 = Player 
      { colour = 2
      , money = 15000
      , property = []
      , playerCell = 1
      , playerPosition = getPlayerPosition 2 1
      }
  , player3 = Player 
      { colour = 3
      , money = 15000
      , property = []
      , playerCell = 1
      , playerPosition = getPlayerPosition 3 1
      }
  , player4 = Player 
      { colour = 4
      , money = 15000
      , property = []
      , playerCell = 1
      , playerPosition = getPlayerPosition 4 1
      }
  , cubes = Cubes 
      { firstCube = 1
      , secondCube = 0
      }
  , haveWinner = Nothing
  , gamePlayer = 1
  , typeStep = "ход"
  , land = [ 
  	Street {name ="Старт"
  	, zone = 0
  	}
  	,Street {name = "Тернополь"
    , zone = 1
    , price = 60
    , isrent = False
    , owner = 0}
    ,Street {name = "Общественная казна"
    , zone = 15
    , isrent = False}
    ,Street {name = "Ровно"
    , zone = 1
    , price = 60
    , isrent = False
    , owner = 0}
    ,Street {name = "Подоходный налог"
    , zone = 16
    , price =200}
    ,Street {name = "Арена львов"
    , zone = 9
    , price = 200
    , isrent = False
    , owner = 0}
    ,Street {name = "Ужгород"
    , zone = 2
    , price = 100
    , isrent = False
    , owner = 0}
    ,Street {name = "Шанс"
    , zone = 15}
    ,Street {name = "Иваново-Франковск"
    , zone = 2
    , price = 100
    , isrent = False
    , owner = 0}
    ,Street {name = "Львов"
    , zone = 2
    , price = 120
    , isrent = False
    , owner = 0}
    ,Street {name = "Тюрьма"
    , zone = 17}
    ,Street{name = "Чернигов"
    , zone = 3
    , price = 140
    , isrent = False
    , owner = 0}
    ,Street {name = "ДНЕПРОГЭС"
    , zone = 10
    , price = 150
    , isrent = False
    , owner = 0}
    ,Street {name = "Сумы"
    , zone = 3
    , price = 140
    , isrent = False
    , owner = 0}
    ,Street {name = "Полтава"
    , zone = 3
    , price = 160
    , isrent = False
    , owner = 0}
    ,Street {name = "ОСК Металлист"
    , zone = 11
    , price = 200
    , isrent = False
    , owner = 0}
    ,Street {name = "Хмельницкий"
    , zone = 4
    , price = 180
    , isrent = False
    , owner = 0}
    ,Street {name = "Общественная казна"
    , zone = 15
    , isrent = False}
    ,Street {name = "Житомир"
    , zone = 4
    , price = 180
    , isrent = False
    , owner = 0}
    ,Street {name = "Винница"
    , zone = 4
    , price = 200
    , isrent = False
    , owner = 0}
    ,Street {name = "Бесплатная стоянка"
    , zone = 17}
    ,Street {name = "Херсон"
    , zone = 5
    , price = 220
    , isrent = False
    , owner = 0}
    ,Street {name = "Шанс"
    , zone = 15}
    ,Street {name = "Николаев"
    , zone = 5
    , price = 220
    , isrent = False
    , owner = 0}
    ,Street {name = "Одесса"
    , zone = 5
    , price = 240
    , isrent = False
    , owner = 0}
    ,Street {name = "Донбасс Арена"
    , zone = 12
    , price = 200
    , isrent = False
    , owner = 0}
    ,Street {name = "Кривой рог"
    , zone = 6
    , price = 260
    , isrent = False
    , owner = 0}
    ,Street {name = "Запорожье"
    , zone = 6
    , price = 260
    , isrent = False
    , owner = 0}
    ,Street {name = "Каховское водохранилище"
    , zone = 13
    , price = 150
    , isrent = False
    , owner = 0}
    ,Street {name = "Днепропетровск"
    , zone = 6
    , price = 280
    , isrent = False
    , owner = 0}
    ,Street {name = "Отправляйся в тюрьму"
    , zone = 18}
    ,Street {name = "Луганск"
    , zone = 7
    , price = 300
    , isrent = False
    , owner = 0}
    ,Street {name = "Донецк"
    , zone = 7
    , price = 300
    , isrent = False
    , owner = 0}
    ,Street {name = "Общественая казна"
    , zone = 15}
    ,Street {name = "Харьков"
    , zone = 7
    , price = 320
    , isrent = False
    , owner = 0}
    ,Street {name = "НСК Олимпийский"
    , zone = 14
    , price = 200
    , isrent = False
    , owner = 0}
    ,Street {name = "Шанс"
    , zone = 0}
    ,Street {name = "АР Крым"
    , zone = 8
    , price = 350
    , isrent = False
    , owner = 0}
    ,Street {name = "Сверхналог"
    , zone = 19
    , price = 100}
    ,Street {name = "Киев"
    , zone = 8
    , price = 400
    , isrent = False
    , owner = 0}
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
        , drawPiece (imagePieceRed images) (player1 gameState)
        , drawPiece (imagePieceBlue  images) (player2 gameState)
        , drawPiece (imagePieceGreen  images) (player3 gameState)
        , drawPiece (imagePieceYellow  images) (player4 gameState)
        ]
    | (typeStep gameState) == "улица" = pictures
        [ drawPlayingField (imagePlayingField images)
        , drawPiece (imagePieceRed images) (player1 gameState)
        , drawPiece (imagePieceBlue  images) (player2 gameState)
        , drawPiece (imagePieceGreen  images) (player3 gameState)
        , drawPiece (imagePieceYellow  images) (player4 gameState)
        , drawPayMenu (imagePayMenu images)
        ]
    | (typeStep gameState) == "" = pictures[]

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
    | field == "налог" = gameState
    | otherwise = gameState
  where
    field = (typeStep gameState)

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
changePlayerCell gameState
  | (gamePlayer gameState) == 1 = gameState
    { player1  = movePlayer (player1 gameState) 1 cubesSum
    , typeStep = getTypeCell (playerCell (movePlayer (player1 gameState) 1 cubesSum))
    }
  | (gamePlayer gameState) == 2 = gameState
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
    }

    where
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

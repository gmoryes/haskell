module SpaceJunk where

import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game


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
  , price :: Int
  , isRent :: Bool
  , priceRent :: Int
  , owner :: Int
  }

data Cubes = Cubes
  { firstCube :: Int
  , secondCube :: Int
  }


-- | Сгенерировать начальное состояние игры.
initGame :: GameState
initGame = GameState
  { players = 
      [ Player 
        { colour = 1
        , money = 1500
        , property = []
        , playerCell = 1
        , playerPosition = getPlayerPosition 1 1
        }
      ,  Player 
        { colour = 2
        , money = 1500
        , property = []
        , playerCell = 1
        , playerPosition = getPlayerPosition 2 1
        }
      , Player 
        { colour = 3
        , money = 1500
        , property = []
        , playerCell = 1
        , playerPosition = getPlayerPosition 3 1
        }
      , Player 
        { colour = 4
        , money = 1500
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
      (typeStep gameState) == "кафедра занята" = pictures
        [ drawPlayingField (imagePlayingField images)
        , drawPiece (imagePieceRed images) player1
        , drawPiece (imagePieceBlue  images) player2
        , drawPiece (imagePieceGreen  images) player3
        , drawPiece (imagePieceYellow  images) player4
        ]
    | (typeStep gameState) == "кафедра свободная" = pictures
        [ drawPlayingField (imagePlayingField images)
        , drawPiece (imagePieceRed images) player1
        , drawPiece (imagePieceBlue  images) player2
        , drawPiece (imagePieceGreen  images) player3
        , drawPiece (imagePieceYellow  images) player4
        , drawPayMenu (imagePayMenu images)
        ]
    | otherwise = pictures 
        [ drawPlayingField (imagePlayingField images)
        , drawPiece (imagePieceRed images) player1
        , drawPiece (imagePieceBlue  images) player2
        , drawPiece (imagePieceGreen  images) player3
        , drawPiece (imagePieceYellow  images) player4
        ]

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
    (x, y) = (playerPosition player)
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
    | ((typeStep gameState) == "кафедра свободная") = case (isPay mouse) of
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
    | field == "кафедра занята" = getPriceRent (payPrictRent gameState)
    | otherwise = gameState
  where
    field = (typeStep gameState)

payPrictRent :: GameState -> GameState
payPrictRent gameState = gameState 
    { players = firstPlayers ++ [(changeBalance player sum)] ++ lastPlayers
    }
  where
    firstPlayers = take ((gamePlayer gameState) - 1) (players gameState)
    player = (players gameState) !! ((gamePlayer gameState) - 1)
    lastPlayers = reverse (take (length (players gameState) - (gamePlayer gameState)) (reverse (players gameState)))
    sum = (priceRent ((land gameState) !! ((playerCell player) - 1))) * (-1)

getPriceRent :: GameState -> GameState
getPriceRent gameState = gameState
    { players = firstPlayers ++ [(changeBalance player sum)] ++ lastPlayers
    }
  where
    player = (players gameState) !! ((owner ((land gameState) !! ((playerCell ((players gameState) !! (gamePlayer gameState))) - 1))) - 1)
    firstPlayers = take ((colour player) - 1) (players gameState)
    lastPlayers = reverse (take (length (players gameState) - (colour player)) (reverse (players gameState)))
    sum = (priceRent ((land gameState) !! ((playerCell player) - 1))) * (-1)

changeBalance :: Player -> Int -> Player
changeBalance player sum = player
    { money = (money player) + sum }

data ChanceCard = ChanceCard
	{ num :: Int
	, price2 :: Int
	, text2 :: String
	}

type Cards = [ChanceCard]

initCards :: Cards -> Cards
initCards cards =
  [ ChanceCard
    { num = 0
        , price2 = 100
        , text2 = "Вам подарок 100 баллов!"
    }
  , ChanceCard
    { num = 1
    , price2 = -100
    , text2 = "Штраф 100 баллов!"
    }
  , ChanceCard
    { num = 2
    , price2 = 200
    , text2 = "С днем рождения! Вам подарок 200 баллов"
    }
  , ChanceCard
    { num = 3
    , price2 = -200
    , text2 = "Штраф 200 баллов!"
    }
  , ChanceCard
    { num = 4
    , price2 = 0
    , text2 = "Живите спокойно!"
    }
  ]

chanceCard :: GameState -> GameState
chanceCard gameState = gameState



streetMove :: GameState -> GameState
streetMove gameState
    | (isRent field) == False = gameState
    | otherwise = (payTax gameState (priceRent field))
  where
    player = (players gameState) !! ((gamePlayer gameState) - 1) 
    field = ((land gameState) !! ((playerCell player) - 1))

{-
buyField :: GameState -> GameState
buyField gameState | (price field) <= (money player) = gameState 
		{ players = firstPlayers ++ [(player) { money = (money (player)) + (price field), property = (property player) ++ (name field) }] ++ lastPlayers
    , gamePlayer = mod (gamePlayer gameState) 4 + 1
    }
									 --как отметить, что недвижимость купили у данного поля
									 | otherwise = gameState
									 --вместо просто геймстейт надо выдать завершение для него игры, ибо денег нет
	where
		firstPlayers = take ((gamePlayer gameState) - 1) (players gameState)
    player = (players gameState) !! ((gamePlayer gameState) - 1)
    lastPlayers = reverse (take (length (players gameState) - (gamePlayer gameState)) (reverse (players gameState))) 
		field = ((land gameState) !! ((playerCell player) - 1))
-}

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

{-throwCubes :: GameState -> IO GameState
throwCubes gameState = do 
    gen <- getStdGen
    gen' <- newStdGen
    gameState
        { cubes = Cubes
            { firstCube = randomR (1,6) gen 
            , secondCube = randomR (1,6) gen'
            }
        }-}
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
    , typeStep = getTypeCell (playerCell (movePlayer player (gamePlayer gameState) cubesSum)) gameState
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

getTypeCell :: Int -> GameState -> String
getTypeCell num gameState
  | num == 1 = "старт"
  | num == 3 || num == 18 || num == 34 = "казна"
  | num == 5 = "налог"
  | num == 8 || num == 23 || num == 37 = "шанс"
  | num == 11 || num == 21 = "стоянка"
  | num == 31 = "тюрьма"
  | num == 39 = "сверхналог"
  | (isRent ((land gameState) !! (num - 1))) == False  = "кафедра свободная"
  | otherwise = "кафедра занята"

movePlayer :: Player -> Int -> Int -> Player
movePlayer player num cubesSum = player 
    { playerCell = (mod ((playerCell player) + cubesSum - 1) 40) + 1
    , playerPosition = getPlayerPosition num ((mod ((playerCell player) + cubesSum - 1) 40) + 1)
    }


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


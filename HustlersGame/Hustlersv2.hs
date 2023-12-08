import System.IO
import Data.List (intercalate)
import Control.Exception (bracket, evaluate, IOException, try)
import Control.DeepSeq (NFData(rnf))

data GameState = GameState
  { backpack :: [String]
  , wallet :: Int
  , location :: String
  } deriving (Show)

initialState :: GameState
initialState = GameState
  { backpack = ["napkin", "map"]
  , wallet = 10
  , location = "home"
  }

displayState :: GameState -> IO ()
displayState state = do
  putStrLn $ "Backpack: " ++ show (backpack state)
  putStrLn $ "Wallet : $" ++ show (wallet state)
  putStrLn $ "Location: " ++ location state

formatShopItems :: String -> String
formatShopItems shopContent =
  "Items for sale in neighbor:\n" ++ shopContent

readShopItems :: FilePath -> IO (Either String String)
readShopItems filePath = do
  result <- tryReadFile filePath
  case result of
    Left err -> return $ Left err
    Right content -> return $ Right $ formatShopItems content

tryReadFile :: FilePath -> IO (Either String String)
tryReadFile filePath = do
  result <- try $ readFile filePath
  return $ case result of
    Left (ex :: IOException) -> Left $ "Error reading file: " ++ show ex
    Right content -> Right content

updateShopFile :: FilePath -> String -> IO (Either String ())
updateShopFile filePath updatedContent = do
  result <- tryWriteFile filePath updatedContent
  return $ case result of
    Left err -> Left err
    Right _ -> Right ()

tryWriteFile :: FilePath -> String -> IO (Either String ())
tryWriteFile filePath content = do
  result <- try $ writeFile filePath content
  return $ case result of
    Left (ex :: IOException) -> Left $ "Error writing to file: " ++ show ex
    Right _ -> Right ()




buyItem :: GameState -> FilePath -> String -> Int -> IO GameState
buyItem state shopFilePath item quantity = do
  shopResult <- readShopItems shopFilePath
  case shopResult of
    Left err -> do
      putStrLn $ "Error reading shop file: " ++ err
      return state
    Right shopContent -> do
      writeResult <- tryWriteFile shopFilePath (updateShopQuantity shopContent item quantity)
      case writeResult of
        Left err -> do
          let (updatedWallet, updatedBackpack) = updateWalletAndBackpack state item quantity
          if (updatedWallet <= 0)
            then handleAction state "quit" False
             else do
              let newState = state { wallet = updatedWallet, backpack = updatedBackpack }
              displayState newState  -- Display the updated state
              return newState
                
          
          
        
        Right _ -> do
          let (updatedWallet, updatedBackpack) = updateWalletAndBackpack state item quantity
          putStrLn $ "You bought " ++ show quantity ++ " " ++ item ++ "(s)."
          let newState = state { wallet = updatedWallet, backpack = updatedBackpack}
          displayState newState  -- Display the updated state
          return $ travel newState "neighbor"

updateWalletAndBackpack :: GameState -> String -> Int -> (Int, [String])
updateWalletAndBackpack state item quantity =
  let itemInfo = getItemInfo item
      updatedWallet = wallet state - itemValue itemInfo * quantity
      updatedBackpack = replicate quantity item ++ backpack state
  in (updatedWallet, updatedBackpack)

updateShopQuantity :: String -> String -> Int -> String
updateShopQuantity shopContent item quantity =
  let linesList = lines shopContent
      updatedLines = map (\line -> if line == itemLine then updatedItemLine else line) linesList
      updatedShopContent = unlines updatedLines
  in updatedShopContent
  where
    itemInfo = getItemInfo item
    itemLine = item ++ "\n-----------------------------------"
    updatedItemLine = item ++ "\n-----------------------------------\nQuantity of item [ " ++ show quantity ++ " ]\nBase value to purchase [ $" ++ show (itemValue itemInfo) ++ " ]\nRe-sell value [ $" ++ show (resellValue itemInfo) ++ " ]"

getItemInfo :: String -> ItemInfo
getItemInfo "cigarette" = ItemInfo 1 0.50
getItemInfo "Liberty Pistol" = ItemInfo 50 25
getItemInfo _ = ItemInfo 0 0  -- Default item info

data ItemInfo = ItemInfo
  { itemValue :: Int
  , resellValue :: Double
  }

main :: IO ()
main = loop initialState False False

getPrompt :: GameState -> Bool -> Bool -> IO String
getPrompt state hasReadNapkin hintDisplayed =
  case location state of
    "home" -> return $ if hasReadNapkin || hintDisplayed
                        then "Where you headed? "
                        else "Hint: enter 'napkin' and hit enter to read it to checkout the rules. 'map' will show the distance from shop.\nWAKE UP CHOOM CHECKOUT THAT NAPKIN IN YOUR BACKPACK. (type 'quit' to exit): "
    "neighbor" -> case wallet state <= 0 of
      True -> return "You're out of money! Better luck next time. (type 'quit' to exit): "
      False -> do
        shopItemsResult <- readShopItems "neighborShop.txt"
        case shopItemsResult of
          Left err -> do
            putStrLn $ "Error reading shop items: " ++ err
            return ""
          Right shopContent -> do
            putStrLn shopContent
            buyPromptResult <- buyPrompt
            return buyPromptResult
    _ -> return "Where would you like to travel next? (type 'quit' to exit): "

loop :: GameState -> Bool -> Bool -> IO ()
loop gameState hasReadNapkin hintDisplayed = do
  putStr =<< getPrompt gameState hasReadNapkin hintDisplayed
  action <- getLine
  if action == "quit" || wallet gameState <= 0
    then putStrLn "Exiting the game."
    else do
      newState <- handleAction gameState action hintDisplayed
      let nextHasReadNapkin = hasReadNapkin || action == "napkin"
          nextHintDisplayed = hintDisplayed || action == "napkin"
      loop newState nextHasReadNapkin nextHintDisplayed


handleAction :: GameState -> String -> Bool -> IO GameState
handleAction state "quit" _ = return state
handleAction state "napkin" _ = do
  howToPlayContent <- readFile "HowToPlay.txt"
  putStrLn howToPlayContent
  return state
handleAction state "map" _ = do
    mapContent <- readFile "map.txt"
    putStrLn mapContent
    return state
handleAction state "neighbor" _ = do
  let newState = travel state "neighbor"
  putStrLn "Hey, my name is Adam Smasher, what can I do for you?"
  putStrLn "Options: 'leave' to travel, 'shop' to see items."
  action <- getLine
  case action of
    "leave" -> do
      handleAction newState "leave" False
    "shop" -> do
      putStrLn "buyin or sellin??"
      buyOrSellAction <- getLine
      case buyOrSellAction of
        "buyin" -> do
          shopItemsResult <- readShopItems "neighborShop.txt"
          case shopItemsResult of
            Left err -> do
              putStrLn $ "Error reading shop items: " ++ err
              return state
            Right shopContent -> do
              putStrLn shopContent
              putStrLn ""
              displayState newState
              buyAction <- buyPrompt
              newStateAfterBuy <- handleAction newState buyAction False
              return newStateAfterBuy
        "sellin" -> do
          sellAction <- sellPrompt
          newState <- sellItem state sellAction
          handleAction newState "leave" False
        _ -> do
          putStrLn "Invalid action. Please try again."
          return state
    _ -> do
      putStrLn "Invalid option. Please try again."
      handleAction newState action False
handleAction state "leave" _ = do
  putStrLn "Where would you like to travel next?"
  destination <- getLine
  let newState = travel state destination
  --displayState newState
  return newState
handleAction state "cigarette" _ = do
  putStrLn "How many would you like to buy?"
  quantity <- readLn
  newState <- buyItem state "neighborShop.txt" "cigarette" quantity
  handleAction newState "leave" False
handleAction state _ _ = do
  return state


buyPrompt :: IO String
buyPrompt = do
  putStrLn "GET CHROMED OUT CHOOM, What do you need"
  input <- getLine
  if input `elem` ["cigarette", "Liberty Pistol", "home", "buyin", "sellin"]
    then return input
    else do
      putStrLn "Invalid choice. Try again."
      buyPrompt

-- Implement the sellItem function
sellItem :: GameState -> String -> IO GameState
sellItem state item = do
  case item `elem` backpack state of
    False -> do
      putStrLn $ "You don't have any " ++ item ++ "s to sell."
      return state
    True -> do
      putStrLn $ "How many " ++ item ++ "s would you like to sell?"
      quantity <- readLn
      case quantity <= 0 || quantity > length (filter (== item) (backpack state)) of
        True -> do
          putStrLn "Invalid quantity. Please try again."
          return state
        False -> do
          let sellValue = calculateSellValue item quantity
              updatedWallet = wallet state + round sellValue
              updatedBackpack = removeItems (backpack state) item quantity
              
              newState = state { wallet = updatedWallet, backpack = updatedBackpack}
          putStrLn $ "You sold " ++ show quantity ++ " " ++ item ++ "(s) for $" ++ show sellValue ++ "."
          displayState newState
          return $ travel newState "home"

-- Calculate the sell value based on the item
calculateSellValue :: String -> Int -> Double
calculateSellValue "cigarette" quantity = fromIntegral quantity * resellValue (getItemInfo "cigarette")
calculateSellValue _ _ = 0


-- Remove items from the backpack
removeItems :: [String] -> String -> Int -> [String]
removeItems [] _ _ = []
removeItems (x:xs) item quantity
  | x == item && quantity > 0 = removeItems xs item (quantity - 1)
  | otherwise = x : removeItems xs item quantity

-- Add a new prompt for selling
sellPrompt :: IO String
sellPrompt = do
  putStrLn "What item would you like to sell?"
  getLine

travel :: GameState -> String -> GameState
travel state destination =
  case destination of
    "neighbor" -> state { wallet = wallet state - 2, location = destination}
    "home" -> state {wallet = wallet state - 2, location = destination}
    
    _ -> state
    

-- costToTravel :: GameState -> Int -> Int
-- costToTravel state distance =
--   case lookup (location state, distance) (map (\(src, dist, cost) -> ((src, dist), cost)) (travelCosts state)) of
--     Just cost -> cost
--     Nothing   -> 0  -- If the travel cost is not found, assume 0 cost

-- travelCosts :: GameState -> [(String, Int, Int)]
-- travelCosts state = map (\(name, dist, cost) -> (name, dist, cost)) (shops state)
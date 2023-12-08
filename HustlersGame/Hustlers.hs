-- Hustler.hs

import System.IO
import Text.XHtml (action)
import Control.Monad
import Data.Maybe (fromJust, isJust, Maybe(Just))
import Data.List
import Control.Monad.Trans.Maybe (MaybeT)
import Text.Read (Lexeme(String))



 
data GameState = GameState
  { backpack :: [String]
  , wallet :: Int
  , location :: String
  , shops :: [(String, Int, [(String,Int,Int, Int)])]
  }

initialState :: GameState
initialState = GameState
  { backpack = ["napkin"]
  , wallet = 10
  , location = "Home"
  , shops = [("Neighbor", 1, [("Cigarette",2,5,1), ("Vape",5,2,2), ("Crayons",5,1,10)])]
  
  }

displayState :: GameState -> IO ()
displayState state = do
  putStrLn $ "Backpack: " ++ show (backpack state)
  putStrLn $ "Wallet : $" ++ show (wallet state)
  putStrLn $ "Location: " ++ location state
  putStrLn $ "Shops: " ++ show (map (\(name,_,_) -> name)(shops state))
  putStrLn $ "Cost to Travel: 1mi = $" ++ show (costToTravel state 1) ++ "\n"

costToTravel :: GameState -> Int -> Int
costToTravel state distance = distance * 2

main :: IO ()
main = do
  let loop gameState = do
        putStr $ getPrompt gameState
        action <- getLine
        if action == "quit" || wallet gameState <= 0
          then putStrLn "Exiting the game."
          else do
            
            newState <- handleAction gameState action
            displayState newState
            loop newState

      getPrompt :: GameState -> String
      getPrompt newState =
        case location newState of
          "Home" -> "WAKE UP CHECKOUT THAT NAPKIN IN YOUR BACKPACK (type 'quit' to exit): "
          "Neighbor" -> "Hey, my name is Adam Smasher, what can I do for you? (type 'leave' to travel or 'shop' to see items): "
          _ -> "Where would you like to travel next? (type 'quit' to exit): "
      
      handleAction :: GameState -> String -> IO GameState
      handleAction newState "quit" = return newState
      handleAction newState "napkin" = do
        howToPlayContent <- readFile "HowToPlay.txt"
        putStrLn howToPlayContent
        return newState  -- Return the original state
      handleAction state "Neighbor" = do
        let newState = travel state "Neighbor"
        putStrLn $ "Traveled to " ++ "Neighbor"
        displayState newState
        return newState  
        putStrLn "Hey, my name is Adam Smasher, what can I do for you?\n"
        putStrLn "Options: 'leave' to travel , 'shop' to see items or 'sell' to sell items.\n"
        action <- getLine
        case action of
          "leave" -> do
            putStr "Where would you like to travel next? "
            destination <- getLine
            let newState = travel state destination
            putStrLn $ "Traveled to " ++ destination
            displayState newState
            return newState
          "shop" -> do
            let shopItems = case lookup "Neighbor" (map (\(name, _, items) -> (name, items)) (shops state)) of
                  Just items -> items
                  Nothing    -> []
            putStrLn $ "Items for sale in Neighbor: " ++ show shopItems
            displayState newState
            putStrLn $ "enter buy"
            action <- getLine
            case action of
                "buy" -> do
                    putStrLn $ "Which item would you like to buy?" ++ show shopItems
                    item <- getLine
                    let newState = buyItem newState item
                    return newState
          _ -> do
            putStrLn "Invalid action. Please try again."
            handleAction state "Neighbor"  -- Retry for invalid actions
      handleAction state _ = do
        displayState state  -- Display the state for other actions
        return state

  loop initialState


travel :: GameState -> String -> GameState
travel state destination =
  if destination `elem` map (\(name,_,_) -> name) (shops state)
    then
      let travelCost = costToTravel state (distanceTo destination state)
      in state { wallet = wallet state - travelCost, location = destination }
    else
      state

distanceTo :: String -> GameState -> Int
distanceTo destination state =
  case lookup destination (map (\(name, dist, _) -> (name, dist)) (shops state)) of
    Just distance -> distance
    Nothing       -> 0  -- If the destination is not found, assume 0 distance


buyItem :: GameState -> String -> GameState
buyItem state item =
  case lookup (location state) (map (\(name, _, items) -> (name, items)) (shops state)) of
    Just shopItems ->
      case find (\(name, _, _, _) -> name == item) shopItems of
        Just (name, quantity, price, weight) ->
          if price <= wallet state
            then
              let updatedState = updateShopInventory item state
                  updatedShopsItems = getShopItemsFromState updatedState
                  updatedShops = updateShopsList (shops state) (location state, 0, updatedShopsItems)
              in updatedState { wallet = wallet state - price, backpack = item : backpack state, shops = updatedShops }
            else
              state -- Not enough funds to buy the item
        Nothing ->
          state -- Item not found in the shop
    Nothing ->
      state -- Shop not found in the current location

getShopItemsFromState :: GameState -> [(String, Int, Int, Int)]
getShopItemsFromState state =
  case lookup (location state) (map (\(name, _, items) -> (name, items)) (shops state)) of
    Just shopItems -> shopItems
    Nothing        -> []

updateShopInventory :: String -> GameState -> GameState
updateShopInventory itemName state =
    case lookup (location state) (map (\(name, _, items) -> (name, items)) (shops state)) of
        Just shopItems ->
            case find (\(name, _, _, _) -> name == itemName) shopItems of
                Just (name, quantity, price, weight) ->
                    let newQuantity = quantity - 1
                        updatedShopItems = updateShopItemQuantity itemName newQuantity shopItems
                        updatedShops = updateShopsList (shops state) (location state, 0, updatedShopItems)
                    in state { shops = updatedShops }
                Nothing -> state  -- Item not found in the shop
        Nothing -> state  -- Shop not found in the current location
updateShopsList :: [(String, Int, [(String, Int, Int, Int)])] -> (String, Int, [(String, Int, Int, Int)]) -> [(String, Int, [(String, Int, Int, Int)])]
updateShopsList shopList updatedShop =
  map (\(shopName, shopDist, items) ->
    if shopName == fst3 updatedShop then updatedShop else (shopName, shopDist, items)
  ) shopList
  where
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x

updateShopItemQuantity :: String -> Int -> [(String, Int, Int, Int)] -> [(String, Int, Int, Int)]
updateShopItemQuantity itemName newQuantity shopItems =
    map (\(name, currentQuantity, price, weight) ->
        if name == itemName
            then (name, newQuantity, price, weight)
            else (name, currentQuantity, price, weight)
    ) shopItems

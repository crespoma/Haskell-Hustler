# WANNA HUSTLE? (STEPS TO RUN PROGRAM)
  - clone the repo
  - ghci Hustler.hs
  -





      ////^\\\\
      | ^   ^ |
     @ (o) (o) @
      |   <   |
      |  ___  |
       \_____/
     ____|  |____
    /    \__/    \
                 








      ////^\\\\
      | ^   ^ |
     @ (o) (o) @
      |   <   |
      |  ___  |
       \_____/
     ____|  |____
    /    \__/    \








# MAIN
### KEY INFORMATION

- Welcome user
  - Display Instructions of the game
  - Begin game
  - Display game UI:
    - User wallet (starting value: 10)
    - List of shops [Each shop has a distance value from the user's house]
    - Distance cost (distance value - 5)
    - Starting position (house)
    - *Narrator* ASCII
    - Prompt user for destination
      - check if wallet >= distanceCost
        - update wallet = wallet - distanceCost
        - update Location
        - Prompt user to buy or sell
            - list Shop items
            - check if wallet > itemCost
            - update wallet
            - prompt user for next step
        -Prompt user for new destination
        - Exit on user input or wallet <= 0
   
### UI DISPLAY
  ghci <fileName>
  ghci> main
  *main goes here* (prompts)
                    Welcome Hustler, my name is David. take a look at that napkin if you need some help choom.
                    
      ////^\\\\ 
      | ^   ^ |                                        Backpack: "HowToPlay.txt" "napkin" 
     @ (o) (o) @                                       Wallet : $10
      |   <   |                                        Location: Home
      |  ___  |                                        Shops: Neighbor 1mi, DoorMart 5mi, FoodCat 10mi, Hustlers Market 15mi 
       \_____/                                         Cost to Travel: 1mi = $2
     ____|  |____  
    /    \__/    \ : (user input)
                  
                      ### Option 1 user reads napkin ###
                                      --- display napkin ---

                      ### option 2 user travels to neighbor ###
      ////^\\\\        Hey
      | ^   ^ |
     @ (o) (o) @
      |   <   |
      |  ___  |
       \_____/
     ____|  |____
    /    \__/    \

                  
                  
# KEY FUNCTIONS
readNapkin :: FilePath -> IO String
  - Description
    Read content of textFile and return it as a string
  readNapkin filePath = readFile filePath

checkoutBackpack :: Wallet -> IO()
 - Description
     Display user's backpack and wallet
     checkoutBackpack wallet = putStrLn $ "Wallet: $" ++ show wallet ++ "\nBackpack: [list of items]"

selectDestination :: IO Shop
 - Description
     Propmts the user to select a destination(shop) to travel to
  selectDestination = do
  putStrLn "Select a destination:"
  putStrLn "1. Dingles"
  putStrLn "2. DoodLion"
  putStrLn "3. HustlersMarket"
  choice <- getLine
  case choice of
    "1" -> return Dingles
    "2" -> return DoodLion
    "3" -> return HustlersMarket
    _   -> putStrLn "Invalid choice" >> selectDestination
   

 calculateTravelCost :: Shop -> Distance
   - Description
       Calculates the cost to travel
     calculateTravelCost shop = case shop of
      Dingles         -> 2
      DoodLion        -> 4
      HustlersMarket  -> 6

Trade :: Wallet -> Inventory -> Shop -> IO (Wallet, Inventory)
  - Simulates the trading process with the selected shop
      handles users buying and selling

updateInventory :: Inventory -> Item -> Quantity -> Inventory
  - Updates the user's inventory based on buying or selling items
    
   
addItem :: Inventory -> Item -> Quantity -> Inventory
  - adds items to the users inventory

removeItem :: Inventory -> Item -> Quantity -> Inventory
  - Removes a specified quantity of an item from the inventory
    









# HELPER FUNCTIONS

-- | Parses the content of the napkin (text file) into a structured format.
parseNapkin :: String -> Inventory
parseNapkin napkinContent = -- Implementation to convert napkin content into an inventory

-- | Displays the available items and their prices in the current shop.
displayShopInventory :: Shop -> IO ()
displayShopInventory shop = -- Implementation to print the shop's inventory

-- | Checks if the user has enough money to afford a specified cost.
hasEnoughMoney :: Wallet -> Cost -> Bool
hasEnoughMoney wallet cost = -- Implementation to check if the wallet has enough money

-- | Displays the result of a trade, showing the changes in wallet and inventory.
displayTradeResult :: Wallet -> Inventory -> IO ()
displayTradeResult wallet inventory = -- Implementation to print trade results

-- | Updates the user's position based on the selected destination.
updatePosition :: Position -> Shop -> Position
updatePosition currentPosition shop = -- Implementation to update the user's position

-- | Calculates the remaining distance from the current position to the selected shop.
calculateRemainingDistance :: Position -> Shop -> Distance
calculateRemainingDistance currentPosition shop = -- Implementation to calculate remaining distance

-- | Checks if the user has reached the destination based on the remaining distance.
hasReachedDestination :: Distance -> Bool
hasReachedDestination remainingDistance = -- Implementation to check if the user has reached the destination

-- | Handles the user's input for buying or selling items during a trade.
getUserTradeInput :: IO TradeAction
getUserTradeInput = -- Implementation to get user input for trading action

-- | Validates the user's input during a trade to ensure it's a valid option.
isValidTradeInput :: String -> Bool
isValidTradeInput input = -- Implementation to validate user input during trading

-- | Displays a goodbye message when the user decides to exit the game.
displayGoodbyeMessage :: IO ()
displayGoodbyeMessage = putStrLn "Thanks for playing! Goodbye!"

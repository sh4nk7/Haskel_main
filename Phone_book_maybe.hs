{- Phone book with Maybe
Implement a f. for PhoneBook
Change signature to return Maybe PhoneNumber
getPhoneNumber :: Name -> PhoneBook -> Maybe PhoneNumber
Result if name not found:
Nothing
Use (tail) recursion-}

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

getPhoneNumber :: Name -> PhoneBook -> Maybe PhoneNumber
getPhoneNumber _ [] = Nothing 
getPhoneNumber name ((n, number):xs)
    | name == n = Just number 
    | otherwise = getPhoneNumber name xs 

-- Example phone book
examplePhoneBook :: PhoneBook
examplePhoneBook = [("Alice", "123"), ("Bob", "456"), ("Charlie", "789")]

-- Test cases
main :: IO ()
main = do
    putStrLn $ "Alice's phone number: " ++ show (getPhoneNumber "Alice" examplePhoneBook)
    putStrLn $ "Bob's phone number: " ++ show (getPhoneNumber "Bob" examplePhoneBook)
    putStrLn $ "Charlie's phone number: " ++ show (getPhoneNumber "Charlie" examplePhoneBook)
    putStrLn $ "Eve's phone number: " ++ show (getPhoneNumber "Eve" examplePhoneBook)


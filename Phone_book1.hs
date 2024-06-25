{-Phone book with recursion
Implement a lookup f. for PhoneBook
getPhoneNumber :: Name -> PhoneBook -> PhoneNumber
Pattern
x:xs
Result if name not found:
""
Use (tail) recursion-}

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

getPhoneNumber :: Name -> PhoneBook -> PhoneNumber
getPhoneNumber _ [] = "" 
getPhoneNumber name ((n, number):xs)
    | name == n = number 
    | otherwise = getPhoneNumber name xs 
    



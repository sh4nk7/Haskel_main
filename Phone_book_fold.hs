{-Phone book with fold
Implement a lookup f. for PhoneBook
getPhoneNumber :: Name -> PhoneBook -> Maybe PhoneNumber
Pattern
(k,v)
Result if name not found:
Nothing-}

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

getPhoneNumber :: Name -> PhoneBook -> Maybe PhoneNumber
getPhoneNumber name phoneBook = foldr (\(n, number) acc -> if n == name then Just number else acc) Nothing phoneBook

-- Example phone book
examplePhoneBook :: PhoneBook
examplePhoneBook = [("Alice", "123"), ("Bob", "456"), ("Charlie", "789")]

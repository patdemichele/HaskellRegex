--REGULAR EXPRESSION TYPE DEF
data Reg = Eps | Emp | Cha Char | Rep Reg | Cat Reg Reg | Alt Reg Reg deriving (Show) 
-- (Show) works fine for printing

--creates the parentheses matching function p_split in three steps
--scan_inc is an auxiliary function that counts opening and closing parentheses in a "stack" manner
--p_split_no_star returns the length of the first nonzero length matching-paren substring
--p_split does this, and includes any extra stars afterwards
--e.g. p_split_no_star (())** = 4, p_split (())** = 6
scan_inc :: Int -> Char -> Int -- used in parentheses matching function
scan_inc acc x
	| x == '(' = acc + 1
	| x == ')' = acc - 1
	| otherwise = acc

p_split_no_star :: [Char] -> Int -- returns length of first parentheses matched string, e.g. "(((hi)hi))whatsup" => 10
p_split_no_star x
	| length x == 0 = 0 -- this case does not actually arise in code, but we do it for formality
	| head x /= '(' = 1
	| otherwise = length (takeWhile (/= 0) (scanl scan_inc 1 (tail x))) + 1 -- some map function with a stack

p_split :: [Char] -> Int
p_split x = let k=p_split_no_star x in k + length (takeWhile (== '*') (drop k x)) -- adds stars

--REGULAR EXPRESSION CONSTRUCTION FROM STRING
parsereg :: [Char] -> Reg -- parses a regex string to Reg
parsereg [] = Emp -- empty string corresponds to empty expression
parsereg ['$'] = Eps -- we use dollar sign for eps
parsereg [x] = (Cha x) -- single character
parsereg other
	| last other == '*' = Rep (parsereg (init other))
	| head other == '(' && last other == ')' && l == length other = parsereg (tail (init other))
	| head y == '+' = Alt (parsereg x) (parsereg (tail y))
	| otherwise = Cat (parsereg x) (parsereg y)
	where l = p_split other; x = take l other; y = drop l other

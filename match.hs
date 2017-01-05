-- somehow import reg.hs

-- USES REG TYPE AND DERIVATIVES PARSING TO RECOGNIZE STRING REGEX

-- nullability function: returns True iff Reg contains empty string
-- serves as auxiliary function in deriv
nullable :: Reg -> Bool
nullable Emp = False
nullable Eps = True
nullabale Rep _ = True -- always contains empty string
nullable Char _ = False -- never contains empty string
nullable Alt r1 r2 = (nullable r1) or (nullable r2) -- either works
nullable Cat r1 r2 = (nullable r1) and (nullable r2) -- must both be nullable

-- implementation of Brzozowski derivative of language with respect to character
deriv :: Reg -> Char -> Reg
-- for Reg R and Char c, find R' matching all w s.t. cw \in R
deriv Emp c = Emp -- still empty!
deriv Eps c = Emp -- no such suffixes work, they are all non-eps after adding c as pred
deriv (Rep r) c = Cat (deriv r c) (Rep r) -- only care about derivative of first, then repeats
deriv (Char c') c = if c' == c then Eps else Emp -- unique matching, and serves as base case
deriv (Alt r1 r2) c = Alt (deriv r1 c) (deriv r2 c) -- fairly self-explanatory
deriv (Cat r1 r2) c
	| nullable r1 = Alt (Cat (deriv r1 c) r2) (deriv r2 c)
	| otherwise = Cat (deriv r1 c) r2
	-- uses our nullability
reg_match :: Reg -> [Char] -> Bool -- Regex, then candidate, then result
reg_match reg [] = nullable reg
reg_match reg (x:xs) = pd_match (deriv reg x) xs -- recursive string match

match :: [Char] -> [Char] -> Bool -- full implementation with strings
match sreg s = reg_match (parsereg sreg) s



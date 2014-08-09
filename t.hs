import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap
lockers = Map.fromList
  [(100,(Taken,"ZD39I"))
  ,(101,(Free,"JAH3I"))
  ,(103,(Free,"IQSA9"))
  ,(105,(Free,"QOTSA"))
  ,(109,(Taken,"893JJ"))
  ,(110,(Taken,"99292"))
  ] 

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber lockerMap = c $ Map.lookup lockerNumber lockerMap
  where
    c Nothing = Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    c (Just (state, code)) = if state == Taken
                             then Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
                             else Right code


data List a = Empty | Cons { listHead :: a, listTail :: List a } deriving (Show, Read, Eq, Ord)


class Tofu t where
  tofu :: j a -> t a j

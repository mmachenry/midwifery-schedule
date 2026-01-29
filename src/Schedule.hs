module Schedule (main, basicPrintModel) where

import Data.Time.Calendar
import Data.Time.Calendar.Month
import Control.Monad (forM_)
import Data.SBV hiding (listArray, fromEnum)
import Data.Array
import Text.Printf

data Visit = W16 | W24 | W28 | W30 | W32 | W34 | W37 | W38 | W39 | W46
  deriving (Show, Eq, Ord, Bounded, Enum, Ix)

oneYearOfCohorts :: [Month]
oneYearOfCohorts = [YearMonth 2026 09 .. YearMonth 2027 08]

basicPrintModel :: IO ()
basicPrintModel = do
  result <- schedule oneYearOfCohorts
  print result

main :: IO ()
main = do
  let cohorts = oneYearOfCohorts
  LexicographicResult smtRes <- schedule cohorts

  case smtRes of
    Satisfiable _config _model -> do
      let vars = [varName c v | c <- cohorts, v <- [minBound .. maxBound :: Visit]]
      forM_ vars $ \nameStr -> do
        case getModelValue nameStr smtRes of
          Just mjd -> putStr $ nameStr ++ " = " ++ show (ModifiedJulianDay mjd)
          Nothing  -> putStr $ nameStr ++ " = <no value>"
        case getModelValue (nameStr ++ "_deviation") smtRes of
          Just nDays -> putStrLn $ " +/- " ++ show (nDays :: Integer) ++ " days"
          Nothing -> putStrLn $ "<no deviation>"
    _ -> putStrLn "Not satisfiable"

schedule :: [Month] -> IO OptimizeResult
schedule cohorts = optimize Lexicographic $ do
  allDays <- mapM buildCohortEvents cohorts
  constrain $ distinct (concatMap elems allDays)

  forM_ (zip cohorts allDays) $ \(cohort, cohortDays)->
    forM_ [minBound .. maxBound] $ \visit-> do
      minimize
        ((varName cohort visit) ++ "_deviation")
        (visitDeviation cohort visit cohortDays)

buildCohortEvents :: Month -> Symbolic (Array Visit SInteger)
buildCohortEvents cohort = do
  days <- genArrayM (minBound, maxBound) (sInteger . varName cohort)

  -- All visits must be on the same day of the week
  constrain $ allEqual (map (`sMod` 7) (elems days))
  -- Visits must be on one of the available visit days
  constrain $ isAvailableWeekDay (days!W16)

  -- The first two visits can be 2 weeks odd of target
  constrain $ visitDeviation cohort W16 days .<= 2 * 7
  constrain $ visitDeviation cohort W24 days .<= 2 * 7

  -- The W28 visit can be one week off target and all
  -- of the two-week visits must be two weeks apart 
  -- exactly.
  constrain $ visitDeviation cohort W28 days .<= 1 * 7
  constrain $ nWeeksApart 2 W28 W30 days
  constrain $ nWeeksApart 2 W30 W32 days
  constrain $ nWeeksApart 2 W32 W34 days

  -- The W37 visit can be one week off target and all
  -- of the one-week visits must be one week apart
  -- exactly.
  constrain $ visitDeviation cohort W37 days .<= 1 * 7
  constrain $ nWeeksApart 1 W37 W38 days
  constrain $ nWeeksApart 1 W38 W39 days

  -- The final 6 week post partum visit can be two
  -- weeks off of target
  constrain $ visitDeviation cohort W46 days .<= 2 * 7

  pure days

varName :: Month -> Visit -> String
varName (YearMonth year month) visit = printf "%04d-%02d-%s" year month (show visit)

nWeeksApart :: SInteger -> Visit -> Visit -> Array Visit SInteger -> SBool
nWeeksApart n v1 v2 days = days!v1 + (n * 7) .== days!v2

visitDeviation :: Month -> Visit -> Array Visit SInteger -> SInteger
visitDeviation cohort visit days =
  let optimal = literal (toModifiedJulianDay (targetDay cohort visit))
  in abs (optimal - days!visit)

isAvailableWeekDay :: SInteger -> SBool
isAvailableWeekDay d = sElem (d `sMod` 7) [2,5,6] -- Monday, Tuesday, Friday

dueDateTolastMenstrualPeriod :: Day -> Day
dueDateTolastMenstrualPeriod = addDays (-40 * 7)

cohortDueDate :: Month -> Day
cohortDueDate (YearMonth year month) = YearMonthDay year month 15

targetDay :: Month -> Visit -> Day
targetDay cohort visit =
  addDays (7 * numWeeks!visit)
          (dueDateTolastMenstrualPeriod (cohortDueDate cohort))
  where numWeeks =
          listArray (minBound, maxBound) [16, 24, 28, 30, 32, 34, 37, 38, 39, 46]

genArrayM
  :: (Ix i, Monad m)
  => (i,i) -> (i -> m e) -> m (Array i e)
genArrayM b f =
  listArray b <$> mapM f (range b)

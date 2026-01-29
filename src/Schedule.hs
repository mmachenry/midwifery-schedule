module Schedule (main, basicPrintModel) where

import Data.Time.Calendar
import Data.Time.Calendar.Month
import Control.Monad (forM_)
import Data.SBV hiding (listArray)
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
  constrain $ distinct (concat allDays)

  forM_ (zip cohorts allDays) $ \(cohort, cohortDays)->
    forM_ [minBound .. maxBound] $ \visit-> do
      minimize
        ((varName cohort visit) ++ "_deviation")
        (visitDeviation cohort visit (listArray (minBound,maxBound) cohortDays))


buildCohortEvents :: Month -> Symbolic [SInteger]
buildCohortEvents cohort = do
  vars <- mapM (sInteger . (varName cohort)) [minBound::Visit .. maxBound]
  let days = listArray (minBound,maxBound) vars

  -- All visits must be on the same day of the week
  constrain $ allEqual (map (flip sMod 7) vars)
  -- Visits must be on one of the available visit days
  constrain $ isAvailableWeekDay (days!W16)

  -- The first two visits can be 2 weeks odd of perfect
  constrain $ visitDeviation cohort W16 days .<= 2 * 7
  constrain $ visitDeviation cohort W24 days .<= 2 * 7

  -- The W28 visit can be one week off perfect and all
  -- of the two-week visits must be two weeks apart 
  -- exactly.
  constrain $ visitDeviation cohort W28 days .<= 1 * 7
  constrain $ nWeeksApart 2 (days!W28) (days!W30)
  constrain $ nWeeksApart 2 (days!W30) (days!W32)
  constrain $ nWeeksApart 2 (days!W32) (days!W34)

  -- The W37 visit can be one week off perfect and all
  -- of the one-week visits must be one week apart
  -- exactly.
  constrain $ visitDeviation cohort W37 days .<= 1 * 7
  constrain $ nWeeksApart 1 (days!W37) (days!W38)
  constrain $ nWeeksApart 1 (days!W38) (days!W39)

  -- The final 6 week post partum visit can be two
  -- weeks off of perfect
  constrain $ visitDeviation cohort W46 days .<= 2 * 7

  pure vars

varName :: Month -> Visit -> String
varName (YearMonth year month) visit = printf "%04d-%02d-%s" year month (show visit)

nWeeksApart :: SInteger -> SInteger -> SInteger -> SBool
nWeeksApart n d1 d2 = d1 + (n * 7) .== d2

visitDeviation :: Month -> Visit -> Array Visit SInteger -> SInteger
visitDeviation cohort visit days =
  let optimal = literal (toModifiedJulianDay (targetDay cohort visit))
  in abs (optimal - days!visit)

isAvailableWeekDay :: SInteger -> SBool
isAvailableWeekDay d =
     d `sMod` 7 .== 2   -- Friday
 .|| d `sMod` 7 .== 6   -- Tuesday
 .|| d `sMod` 7 .== 5   -- Monday

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


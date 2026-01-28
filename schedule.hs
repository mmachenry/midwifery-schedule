import Data.Time.Calendar
import Data.List (intersect)
import Control.Monad (guard, forM_)
import Data.SBV
import Data.Array
import Data.Maybe (fromJust)

data Visit = W16 | W24 | W28 | W30 | W32 | W34 | W37 | W38 | W39 | W46
  deriving (Show, Eq, Ord, Bounded, Enum, Ix)
data Cohort = Cohort Year MonthOfYear deriving (Eq, Show)

oneYearOfCohorts = [Cohort 2026 09, Cohort 2026 10, Cohort 2026 11, Cohort 2026 12, Cohort 2027 01, Cohort 2027 02, Cohort 2027 03, Cohort 2027 04, Cohort 2027 05, Cohort 2027 06, Cohort 2027 07, Cohort 2027 08]

othermain = do
  result <- schedule oneYearOfCohorts
  print result

main = do
  let cohorts = oneYearOfCohorts
  res <- schedule cohorts
  case res of
    SatResult (Satisfiable _ _) -> do
      forM_ cohorts $ \cohort-> do
        let names = map (varName cohort) [minBound .. maxBound :: Visit]
        days <- mapM
          (\n -> pure (ModifiedJulianDay (fromJust (getModelValue n res))))
          names
        forM_ (zip names days) print
    _ ->
      putStrLn "No solution"

schedule cohorts = sat $ do
  allDays <- mapM buildCohortEvents cohorts
  constrain $ distinct (concat allDays)

buildCohortEvents :: Cohort -> Symbolic [SInteger]
buildCohortEvents cohort = do
  vars <- mapM (sInteger . (varName cohort)) [minBound::Visit .. maxBound]
  let days = listArray (minBound,maxBound) vars

  -- All visits must be on the same day of the week
  constrain $ allEqual (map (flip sMod 7) vars)
  -- Visits must be on one of the available visit days
  constrain $ isAvailableWeekDay (days!W16)

  -- The first two visits can be 2 weeks odd of perfect
  constrain $ visitInRange cohort W16 (days!W16) 2
  constrain $ visitInRange cohort W24 (days!W24) 2

  -- The W28 visit can be one week off perfect and all
  -- of the two-week visits must be two weeks apart 
  -- exactly.
  constrain $ visitInRange cohort W28 (days!W28) 1
  constrain $ nWeeksApart 2 (days!W28) (days!W30)
  constrain $ nWeeksApart 2 (days!W30) (days!W32)
  constrain $ nWeeksApart 2 (days!W32) (days!W34)

  -- The W37 visit can be one week off perfect and all
  -- of the one-week visits must be one week apart
  -- exactly.
  constrain $ visitInRange cohort W37 (days!W37) 1
  constrain $ nWeeksApart 1 (days!W37) (days!W38)
  constrain $ nWeeksApart 1 (days!W38) (days!W39)

  -- The final 6 week post partum visit can be two
  -- weeks off of perfect
  constrain $ visitInRange cohort W46 (days!W46) 2

  pure vars

varName (Cohort year month) visit =
  show year ++ "-" ++ show month ++ "-" ++ show visit

nWeeksApart :: SInteger -> SInteger -> SInteger -> SBool
nWeeksApart n d1 d2 = d1 + (n * 7) .== d2

visitInRange :: Cohort -> Visit -> SInteger -> SInteger -> SBool
visitInRange cohort visit day plusMinusWeeks =
  let optimal = literal (toModifiedJulianDay (targetDay cohort visit))
  in abs (optimal - day) .<= 7 * plusMinusWeeks

isAvailableWeekDay :: SInteger -> SBool
isAvailableWeekDay d =
     d `sMod` 7 .== 2   -- Friday
 .|| d `sMod` 7 .== 6   -- Tuesday
 .|| d `sMod` 7 .== 5   -- Monday

dueDateTolastMenstrualPeriod :: Day -> Day
dueDateTolastMenstrualPeriod = addDays (-40 * 7)

cohortDueDate :: Cohort -> Day
cohortDueDate (Cohort year month) = YearMonthDay year month 15

targetDay :: Cohort -> Visit -> Day
targetDay cohort visit =
    addDays (7 * numWeeks!visit)
            (dueDateTolastMenstrualPeriod (cohortDueDate cohort))
  where numWeeks =
          listArray (minBound, maxBound) [16, 24, 28, 30, 32, 34, 37, 38, 39, 46]


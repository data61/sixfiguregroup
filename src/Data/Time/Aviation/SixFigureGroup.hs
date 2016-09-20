{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Time.Aviation.SixFigureGroup(
  DayOfMonth(..)
, HasDayOfMonth(..)
, AsDayOfMonth(..)
, parseDayOfMonth
, HourOfDay(..)
, HasHourOfDay(..)
, AsHourOfDay(..)
, parseHourOfDay
, MinuteOfHour(..)
, HasMinuteOfHour(..)
, AsMinuteOfHour(..)
, parseMinuteOfHour
, SixFigureGroup(..)
, HasSixFigureGroup(..)
, parseSixFigureGroup
, parseSixFigureGroupz
) where

import Control.Applicative((<|>), empty, (<*>), (<*))
import Control.Category((.))
import Control.Lens(makeClassy, makeClassyPrisms, Prism', prism')
import Data.Eq(Eq)
import Data.Foldable(foldr)
import Data.Functor((<$>), (<$))
import Data.Int(Int)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Text.Parser.Char(CharParsing, string, char)
import Text.Parser.Combinators((<?>), try)
import Prelude(Show, Num, Integer, Float, Double)

-- $setup
--
-- >>> import Text.Parsec(parse)
-- >>> import Control.Lens
-- >>> import Prelude(show)

data DayOfMonth = 
  DayOfMonth_01
  | DayOfMonth_02
  | DayOfMonth_03
  | DayOfMonth_04
  | DayOfMonth_05
  | DayOfMonth_06
  | DayOfMonth_07
  | DayOfMonth_08
  | DayOfMonth_09
  | DayOfMonth_10
  | DayOfMonth_11
  | DayOfMonth_12
  | DayOfMonth_13
  | DayOfMonth_14
  | DayOfMonth_15
  | DayOfMonth_16
  | DayOfMonth_17
  | DayOfMonth_18
  | DayOfMonth_19
  | DayOfMonth_20
  | DayOfMonth_21
  | DayOfMonth_22
  | DayOfMonth_23
  | DayOfMonth_24
  | DayOfMonth_25
  | DayOfMonth_26
  | DayOfMonth_27
  | DayOfMonth_28
  | DayOfMonth_29
  | DayOfMonth_30
  | DayOfMonth_31
  deriving (Eq, Ord, Show)

makeClassy ''DayOfMonth
makeClassyPrisms ''DayOfMonth

integralDayOfMonth ::
  (Eq a, Num a) =>
  Prism' a DayOfMonth
integralDayOfMonth =
  prism'
    (\d ->  case d of
              DayOfMonth_01 ->
               1
              DayOfMonth_02 ->
               2
              DayOfMonth_03 ->
               3
              DayOfMonth_04 ->
               4
              DayOfMonth_05 ->
               5
              DayOfMonth_06 ->
               6
              DayOfMonth_07 ->
               7
              DayOfMonth_08 ->
               8
              DayOfMonth_09 ->
               9
              DayOfMonth_10 ->
               10
              DayOfMonth_11 ->
               11
              DayOfMonth_12 ->
               12
              DayOfMonth_13 ->
               13
              DayOfMonth_14 ->
               14
              DayOfMonth_15 ->
               15
              DayOfMonth_16 ->
               16
              DayOfMonth_17 ->
               17
              DayOfMonth_18 ->
               18
              DayOfMonth_19 ->
               19
              DayOfMonth_20 ->
               20
              DayOfMonth_21 ->
               21
              DayOfMonth_22 ->
               22
              DayOfMonth_23 ->
               23
              DayOfMonth_24 ->
               24
              DayOfMonth_25 ->
               25
              DayOfMonth_26 ->
               26
              DayOfMonth_27 ->
               27
              DayOfMonth_28 ->
               28
              DayOfMonth_29 ->
               29
              DayOfMonth_30 ->
               30
              DayOfMonth_31 ->
               31)
    (\n ->  case n of
              1 ->
                Just DayOfMonth_01
              2 ->
                Just DayOfMonth_02
              3 ->
                Just DayOfMonth_03
              4 ->
                Just DayOfMonth_04
              5 ->
                Just DayOfMonth_05
              6 ->
                Just DayOfMonth_06
              7 ->
                Just DayOfMonth_07
              8 ->
                Just DayOfMonth_08
              9 ->
                Just DayOfMonth_09
              10 ->
                Just DayOfMonth_10
              11 ->
                Just DayOfMonth_11
              12 ->
                Just DayOfMonth_12
              13 ->
                Just DayOfMonth_13
              14 ->
                Just DayOfMonth_14
              15 ->
                Just DayOfMonth_15
              16 ->
                Just DayOfMonth_16
              17 ->
                Just DayOfMonth_17
              18 ->
                Just DayOfMonth_18
              19 ->
                Just DayOfMonth_19
              20 ->
                Just DayOfMonth_20
              21 ->
                Just DayOfMonth_21
              22 ->
                Just DayOfMonth_22
              23 ->
                Just DayOfMonth_23
              24 ->
                Just DayOfMonth_24
              25 ->
                Just DayOfMonth_25
              26 ->
                Just DayOfMonth_26
              27 ->
                Just DayOfMonth_27
              28 ->
                Just DayOfMonth_28
              29 ->
                Just DayOfMonth_29
              30 ->
                Just DayOfMonth_30
              31 ->
                Just DayOfMonth_31
              _ ->
                Nothing)

instance AsDayOfMonth Int where
  _DayOfMonth =
    integralDayOfMonth

instance AsDayOfMonth Integer where
  _DayOfMonth =
    integralDayOfMonth

instance AsDayOfMonth Float where
  _DayOfMonth =
    integralDayOfMonth

instance AsDayOfMonth Double where
  _DayOfMonth =
    integralDayOfMonth

-- | Parse two digits to a day of month (01-31).
--
-- >>> parse parseDayOfMonth "test" "01"
-- Right DayOfMonth_01
--
-- >>> parse parseDayOfMonth "test" "02"
-- Right DayOfMonth_02
--
-- >>> parse parseDayOfMonth "test" "18"
-- Right DayOfMonth_18
--
-- >>> parse parseDayOfMonth "test" "30"
-- Right DayOfMonth_30
--
-- >>> parse parseDayOfMonth "test" "31"
-- Right DayOfMonth_31
--
-- >>> isn't _Right (parse parseDayOfMonth "test" "00")
-- True
--
-- >>> isn't _Right (parse parseDayOfMonth "test" "32")
-- True
--
-- >>> isn't _Right (parse parseDayOfMonth "test" "9")
-- True
--
-- >>> parse (show <$> parseDayOfMonth <|> string "00") "test" "00"
-- Right "00"
parseDayOfMonth ::
  CharParsing f =>
  f DayOfMonth
parseDayOfMonth =
  let t = [
            (DayOfMonth_01, "01")
          , (DayOfMonth_02, "02")
          , (DayOfMonth_03, "03")
          , (DayOfMonth_04, "04")
          , (DayOfMonth_05, "05")
          , (DayOfMonth_06, "06")
          , (DayOfMonth_07, "07")
          , (DayOfMonth_08, "08")
          , (DayOfMonth_09, "09")
          , (DayOfMonth_10, "10")
          , (DayOfMonth_11, "11")
          , (DayOfMonth_12, "12")
          , (DayOfMonth_13, "13")
          , (DayOfMonth_14, "14")
          , (DayOfMonth_15, "15")
          , (DayOfMonth_16, "16")
          , (DayOfMonth_17, "17")
          , (DayOfMonth_18, "18")
          , (DayOfMonth_19, "19")
          , (DayOfMonth_20, "20")
          , (DayOfMonth_21, "21")
          , (DayOfMonth_22, "22")
          , (DayOfMonth_23, "23")
          , (DayOfMonth_24, "24")
          , (DayOfMonth_25, "25")
          , (DayOfMonth_26, "26")
          , (DayOfMonth_27, "27")
          , (DayOfMonth_28, "28")
          , (DayOfMonth_29, "29")
          , (DayOfMonth_30, "30")
          , (DayOfMonth_31, "31")
          ]
  in  foldr (\(a, b) x -> try (a <$ string b) <|> x) empty t      
        <?> "day of month"

data HourOfDay = 
  HourOfDay_00
  | HourOfDay_01
  | HourOfDay_02
  | HourOfDay_03
  | HourOfDay_04
  | HourOfDay_05
  | HourOfDay_06
  | HourOfDay_07
  | HourOfDay_08
  | HourOfDay_09
  | HourOfDay_10
  | HourOfDay_11
  | HourOfDay_12
  | HourOfDay_13
  | HourOfDay_14
  | HourOfDay_15
  | HourOfDay_16
  | HourOfDay_17
  | HourOfDay_18
  | HourOfDay_19
  | HourOfDay_20
  | HourOfDay_21
  | HourOfDay_22
  | HourOfDay_23
  deriving (Eq, Ord, Show)

makeClassy ''HourOfDay
makeClassyPrisms ''HourOfDay

integralHourOfDay ::
  (Eq a, Num a) =>
  Prism' a HourOfDay
integralHourOfDay =
  prism'
    (\d ->  case d of
              HourOfDay_00 ->
               0
              HourOfDay_01 ->
               1
              HourOfDay_02 ->
               2
              HourOfDay_03 ->
               3
              HourOfDay_04 ->
               4
              HourOfDay_05 ->
               5
              HourOfDay_06 ->
               6
              HourOfDay_07 ->
               7
              HourOfDay_08 ->
               8
              HourOfDay_09 ->
               9
              HourOfDay_10 ->
               10
              HourOfDay_11 ->
               11
              HourOfDay_12 ->
               12
              HourOfDay_13 ->
               13
              HourOfDay_14 ->
               14
              HourOfDay_15 ->
               15
              HourOfDay_16 ->
               16
              HourOfDay_17 ->
               17
              HourOfDay_18 ->
               18
              HourOfDay_19 ->
               19
              HourOfDay_20 ->
               20
              HourOfDay_21 ->
               21
              HourOfDay_22 ->
               22
              HourOfDay_23 ->
               23)
    (\n ->  case n of
              0 ->
                Just HourOfDay_00
              1 ->
                Just HourOfDay_01
              2 ->
                Just HourOfDay_02
              3 ->
                Just HourOfDay_03
              4 ->
                Just HourOfDay_04
              5 ->
                Just HourOfDay_05
              6 ->
                Just HourOfDay_06
              7 ->
                Just HourOfDay_07
              8 ->
                Just HourOfDay_08
              9 ->
                Just HourOfDay_09
              10 ->
                Just HourOfDay_10
              11 ->
                Just HourOfDay_11
              12 ->
                Just HourOfDay_12
              13 ->
                Just HourOfDay_13
              14 ->
                Just HourOfDay_14
              15 ->
                Just HourOfDay_15
              16 ->
                Just HourOfDay_16
              17 ->
                Just HourOfDay_17
              18 ->
                Just HourOfDay_18
              19 ->
                Just HourOfDay_19
              20 ->
                Just HourOfDay_20
              21 ->
                Just HourOfDay_21
              22 ->
                Just HourOfDay_22
              23 ->
                Just HourOfDay_23
              _ ->
                Nothing)

instance AsHourOfDay Int where
  _HourOfDay =
    integralHourOfDay

instance AsHourOfDay Integer where
  _HourOfDay =
    integralHourOfDay

instance AsHourOfDay Float where
  _HourOfDay =
    integralHourOfDay

instance AsHourOfDay Double where
  _HourOfDay =
    integralHourOfDay

-- | Parse two digits to an hour of day (00-23).
--
-- >>> parse parseHourOfDay "test" "00"
-- Right HourOfDay_00
--
-- >>> parse parseHourOfDay "test" "01"
-- Right HourOfDay_01
--
-- >>> parse parseHourOfDay "test" "18"
-- Right HourOfDay_18
--
-- >>> parse parseHourOfDay "test" "22"
-- Right HourOfDay_22
--
-- >>> parse parseHourOfDay "test" "23"
-- Right HourOfDay_23
--
-- >>> isn't _Right (parse parseHourOfDay "test" "24")
-- True
--
-- >>> isn't _Right (parse parseHourOfDay "test" "9")
-- True
--
-- >>> parse (show <$> parseHourOfDay <|> string "25") "test" "25"
-- Right "25"
parseHourOfDay ::
  CharParsing f =>
  f HourOfDay
parseHourOfDay =
  let t = [
            (HourOfDay_00, "00")
          , (HourOfDay_01, "01")
          , (HourOfDay_02, "02")
          , (HourOfDay_03, "03")
          , (HourOfDay_04, "04")
          , (HourOfDay_05, "05")
          , (HourOfDay_06, "06")
          , (HourOfDay_07, "07")
          , (HourOfDay_08, "08")
          , (HourOfDay_09, "09")
          , (HourOfDay_10, "10")
          , (HourOfDay_11, "11")
          , (HourOfDay_12, "12")
          , (HourOfDay_13, "13")
          , (HourOfDay_14, "14")
          , (HourOfDay_15, "15")
          , (HourOfDay_16, "16")
          , (HourOfDay_17, "17")
          , (HourOfDay_18, "18")
          , (HourOfDay_19, "19")
          , (HourOfDay_20, "20")
          , (HourOfDay_21, "21")
          , (HourOfDay_22, "22")
          , (HourOfDay_23, "23")
          ]
  in  foldr (\(a, b) x -> try (a <$ string b) <|> x) empty t      
        <?> "hour of day"
data MinuteOfHour = 
  MinuteOfHour_00
  | MinuteOfHour_01
  | MinuteOfHour_02
  | MinuteOfHour_03
  | MinuteOfHour_04
  | MinuteOfHour_05
  | MinuteOfHour_06
  | MinuteOfHour_07
  | MinuteOfHour_08
  | MinuteOfHour_09
  | MinuteOfHour_10
  | MinuteOfHour_11
  | MinuteOfHour_12
  | MinuteOfHour_13
  | MinuteOfHour_14
  | MinuteOfHour_15
  | MinuteOfHour_16
  | MinuteOfHour_17
  | MinuteOfHour_18
  | MinuteOfHour_19
  | MinuteOfHour_20
  | MinuteOfHour_21
  | MinuteOfHour_22
  | MinuteOfHour_23
  | MinuteOfHour_24
  | MinuteOfHour_25
  | MinuteOfHour_26
  | MinuteOfHour_27
  | MinuteOfHour_28
  | MinuteOfHour_29
  | MinuteOfHour_30
  | MinuteOfHour_31
  | MinuteOfHour_32
  | MinuteOfHour_33
  | MinuteOfHour_34
  | MinuteOfHour_35
  | MinuteOfHour_36
  | MinuteOfHour_37
  | MinuteOfHour_38
  | MinuteOfHour_39
  | MinuteOfHour_40
  | MinuteOfHour_41
  | MinuteOfHour_42
  | MinuteOfHour_43
  | MinuteOfHour_44
  | MinuteOfHour_45
  | MinuteOfHour_46
  | MinuteOfHour_47
  | MinuteOfHour_48
  | MinuteOfHour_49
  | MinuteOfHour_50
  | MinuteOfHour_51
  | MinuteOfHour_52
  | MinuteOfHour_53
  | MinuteOfHour_54
  | MinuteOfHour_55
  | MinuteOfHour_56
  | MinuteOfHour_57
  | MinuteOfHour_58
  | MinuteOfHour_59
  deriving (Eq, Ord, Show)

makeClassy ''MinuteOfHour
makeClassyPrisms ''MinuteOfHour

-- | Parse two digits to a minute of hour (00-59).
--
-- >>> parse parseMinuteOfHour "test" "00"
-- Right MinuteOfHour_00
--
-- >>> parse parseMinuteOfHour "test" "01"
-- Right MinuteOfHour_01
--
-- >>> parse parseMinuteOfHour "test" "18"
-- Right MinuteOfHour_18
--
-- >>> parse parseMinuteOfHour "test" "58"
-- Right MinuteOfHour_58
--
-- >>> parse parseMinuteOfHour "test" "59"
-- Right MinuteOfHour_59
--
-- >>> isn't _Right (parse parseMinuteOfHour "test" "60")
-- True
--
-- >>> isn't _Right (parse parseMinuteOfHour "test" "9")
-- True
--
-- >>> parse (show <$> parseMinuteOfHour <|> string "61") "test" "61"
-- Right "61"
parseMinuteOfHour ::
  CharParsing f =>
  f MinuteOfHour
parseMinuteOfHour =
  let t = [
            (MinuteOfHour_00, "00")
          , (MinuteOfHour_01, "01")
          , (MinuteOfHour_02, "02")
          , (MinuteOfHour_03, "03")
          , (MinuteOfHour_04, "04")
          , (MinuteOfHour_05, "05")
          , (MinuteOfHour_06, "06")
          , (MinuteOfHour_07, "07")
          , (MinuteOfHour_08, "08")
          , (MinuteOfHour_09, "09")
          , (MinuteOfHour_10, "10")
          , (MinuteOfHour_11, "11")
          , (MinuteOfHour_12, "12")
          , (MinuteOfHour_13, "13")
          , (MinuteOfHour_14, "14")
          , (MinuteOfHour_15, "15")
          , (MinuteOfHour_16, "16")
          , (MinuteOfHour_17, "17")
          , (MinuteOfHour_18, "18")
          , (MinuteOfHour_19, "19")
          , (MinuteOfHour_20, "20")
          , (MinuteOfHour_21, "21")
          , (MinuteOfHour_22, "22")
          , (MinuteOfHour_23, "23")
          , (MinuteOfHour_24, "24")
          , (MinuteOfHour_25, "25")
          , (MinuteOfHour_26, "26")
          , (MinuteOfHour_27, "27")
          , (MinuteOfHour_28, "28")
          , (MinuteOfHour_29, "29")
          , (MinuteOfHour_30, "30")
          , (MinuteOfHour_31, "31")
          , (MinuteOfHour_32, "32")
          , (MinuteOfHour_33, "33")
          , (MinuteOfHour_34, "34")
          , (MinuteOfHour_35, "35")
          , (MinuteOfHour_36, "36")
          , (MinuteOfHour_37, "37")
          , (MinuteOfHour_38, "38")
          , (MinuteOfHour_39, "39")
          , (MinuteOfHour_40, "40")
          , (MinuteOfHour_41, "41")
          , (MinuteOfHour_42, "42")
          , (MinuteOfHour_43, "43")
          , (MinuteOfHour_44, "44")
          , (MinuteOfHour_45, "45")
          , (MinuteOfHour_46, "46")
          , (MinuteOfHour_47, "47")
          , (MinuteOfHour_48, "48")
          , (MinuteOfHour_49, "49")
          , (MinuteOfHour_50, "50")
          , (MinuteOfHour_51, "51")
          , (MinuteOfHour_52, "52")
          , (MinuteOfHour_53, "53")
          , (MinuteOfHour_54, "54")
          , (MinuteOfHour_55, "55")
          , (MinuteOfHour_56, "56")
          , (MinuteOfHour_57, "57")
          , (MinuteOfHour_58, "58")
          , (MinuteOfHour_59, "59")
          ]
  in  foldr (\(a, b) x -> try (a <$ string b) <|> x) empty t      
        <?> "minute of hour"

data SixFigureGroup =
  SixFigureGroup {
    _dayofmonth ::
      DayOfMonth
  , _hourofday ::
      HourOfDay
  , _minuteofhour ::
     MinuteOfHour
  }
  deriving (Eq, Ord, Show)

makeClassy ''SixFigureGroup

integralMinuteOfHour ::
  (Eq a, Num a) =>
  Prism' a MinuteOfHour
integralMinuteOfHour =
  prism'
    (\d ->  case d of
              MinuteOfHour_01 ->
               1
              MinuteOfHour_00 ->
               0
              MinuteOfHour_02 ->
               2
              MinuteOfHour_03 ->
               3
              MinuteOfHour_04 ->
               4
              MinuteOfHour_05 ->
               5
              MinuteOfHour_06 ->
               6
              MinuteOfHour_07 ->
               7
              MinuteOfHour_08 ->
               8
              MinuteOfHour_09 ->
               9
              MinuteOfHour_10 ->
               10
              MinuteOfHour_11 ->
               11
              MinuteOfHour_12 ->
               12
              MinuteOfHour_13 ->
               13
              MinuteOfHour_14 ->
               14
              MinuteOfHour_15 ->
               15
              MinuteOfHour_16 ->
               16
              MinuteOfHour_17 ->
               17
              MinuteOfHour_18 ->
               18
              MinuteOfHour_19 ->
               19
              MinuteOfHour_20 ->
               20
              MinuteOfHour_21 ->
               21
              MinuteOfHour_22 ->
               22
              MinuteOfHour_23 ->
               23
              MinuteOfHour_24 ->
               24
              MinuteOfHour_25 ->
               25
              MinuteOfHour_26 ->
               26
              MinuteOfHour_27 ->
               27
              MinuteOfHour_28 ->
               28
              MinuteOfHour_29 ->
               29
              MinuteOfHour_30 ->
               30
              MinuteOfHour_31 ->
               31
              MinuteOfHour_32 ->
               32
              MinuteOfHour_33 ->
               33
              MinuteOfHour_34 ->
               34
              MinuteOfHour_35 ->
               35
              MinuteOfHour_36 ->
               36
              MinuteOfHour_37 ->
               37
              MinuteOfHour_38 ->
               38
              MinuteOfHour_39 ->
               39
              MinuteOfHour_40 ->
               40
              MinuteOfHour_41 ->
               41
              MinuteOfHour_42 ->
               42
              MinuteOfHour_43 ->
               43
              MinuteOfHour_44 ->
               44
              MinuteOfHour_45 ->
               45
              MinuteOfHour_46 ->
               46
              MinuteOfHour_47 ->
               47
              MinuteOfHour_48 ->
               48
              MinuteOfHour_49 ->
               49
              MinuteOfHour_50 ->
               50
              MinuteOfHour_51 ->
               51
              MinuteOfHour_52 ->
               52
              MinuteOfHour_53 ->
               53
              MinuteOfHour_54 ->
               54
              MinuteOfHour_55 ->
               55
              MinuteOfHour_56 ->
               56
              MinuteOfHour_57 ->
               57
              MinuteOfHour_58 ->
               58
              MinuteOfHour_59 ->
               59)
    (\n ->  case n of
              0 ->
                Just MinuteOfHour_00
              1 ->
                Just MinuteOfHour_01
              2 ->
                Just MinuteOfHour_02
              3 ->
                Just MinuteOfHour_03
              4 ->
                Just MinuteOfHour_04
              5 ->
                Just MinuteOfHour_05
              6 ->
                Just MinuteOfHour_06
              7 ->
                Just MinuteOfHour_07
              8 ->
                Just MinuteOfHour_08
              9 ->
                Just MinuteOfHour_09
              10 ->
                Just MinuteOfHour_10
              11 ->
                Just MinuteOfHour_11
              12 ->
                Just MinuteOfHour_12
              13 ->
                Just MinuteOfHour_13
              14 ->
                Just MinuteOfHour_14
              15 ->
                Just MinuteOfHour_15
              16 ->
                Just MinuteOfHour_16
              17 ->
                Just MinuteOfHour_17
              18 ->
                Just MinuteOfHour_18
              19 ->
                Just MinuteOfHour_19
              20 ->
                Just MinuteOfHour_20
              21 ->
                Just MinuteOfHour_21
              22 ->
                Just MinuteOfHour_22
              23 ->
                Just MinuteOfHour_23
              24 ->
                Just MinuteOfHour_24
              25 ->
                Just MinuteOfHour_25
              26 ->
                Just MinuteOfHour_26
              27 ->
                Just MinuteOfHour_27
              28 ->
                Just MinuteOfHour_28
              29 ->
                Just MinuteOfHour_29
              30 ->
                Just MinuteOfHour_30
              31 ->
                Just MinuteOfHour_31
              32 ->
                Just MinuteOfHour_32
              33 ->
                Just MinuteOfHour_33
              34 ->
                Just MinuteOfHour_34
              35 ->
                Just MinuteOfHour_35
              36 ->
                Just MinuteOfHour_36
              37 ->
                Just MinuteOfHour_37
              38 ->
                Just MinuteOfHour_38
              39 ->
                Just MinuteOfHour_39
              40 ->
                Just MinuteOfHour_40
              41 ->
                Just MinuteOfHour_41
              42 ->
                Just MinuteOfHour_42
              43 ->
                Just MinuteOfHour_43
              44 ->
                Just MinuteOfHour_44
              45 ->
                Just MinuteOfHour_45
              46 ->
                Just MinuteOfHour_46
              47 ->
                Just MinuteOfHour_47
              48 ->
                Just MinuteOfHour_48
              49 ->
                Just MinuteOfHour_49
              50 ->
                Just MinuteOfHour_50
              51 ->
                Just MinuteOfHour_51
              52 ->
                Just MinuteOfHour_52
              53 ->
                Just MinuteOfHour_53
              54 ->
                Just MinuteOfHour_54
              55 ->
                Just MinuteOfHour_55
              56 ->
                Just MinuteOfHour_56
              57 ->
                Just MinuteOfHour_57
              58 ->
                Just MinuteOfHour_58
              59 ->
                Just MinuteOfHour_59
              _ ->
                Nothing)

instance AsMinuteOfHour Int where
  _MinuteOfHour =
    integralMinuteOfHour

instance AsMinuteOfHour Integer where
  _MinuteOfHour =
    integralMinuteOfHour

instance AsMinuteOfHour Float where
  _MinuteOfHour =
    integralMinuteOfHour

instance AsMinuteOfHour Double where
  _MinuteOfHour =
    integralMinuteOfHour

instance HasDayOfMonth SixFigureGroup where
  dayOfMonth =
    dayofmonth . dayOfMonth

instance HasHourOfDay SixFigureGroup where
  hourOfDay =
    hourofday . hourOfDay

instance HasMinuteOfHour SixFigureGroup where
  minuteOfHour =
    minuteofhour . minuteOfHour

-- | Parse six digits to a six figure group.
--
-- >>> parse parseSixFigureGroup "test" "302301"
-- Right (SixFigureGroup {_dayofmonth = DayOfMonth_30, _hourofday = HourOfDay_23, _minuteofhour = MinuteOfHour_01})
--
-- >>> parse parseSixFigureGroup "test" "090113"
-- Right (SixFigureGroup {_dayofmonth = DayOfMonth_09, _hourofday = HourOfDay_01, _minuteofhour = MinuteOfHour_13})
--
-- >>> isn't _Right (parse parseSixFigureGroup "test" "322301")
-- True
--
-- >>> isn't _Right (parse parseSixFigureGroup "test" "302401")
-- True
--
-- >>> isn't _Right (parse parseSixFigureGroup "test" "302360")
-- True
--
-- >>> isn't _Right (parse parseSixFigureGroup "test" "30230")
-- True
--
-- >>> parse (show <$> parseSixFigureGroup <|> string "302360") "test" "302360"
-- Right "302360"
parseSixFigureGroup ::
  CharParsing f =>
  f SixFigureGroup
parseSixFigureGroup =
  try (SixFigureGroup <$> parseDayOfMonth <*> parseHourOfDay <*> parseMinuteOfHour)
    <?> "six figure group"

-- | Parse six digits to a six figure group appended with 'Z'.
--
-- >>> parse parseSixFigureGroup "test" "302301Z"
-- Right (SixFigureGroup {_dayofmonth = DayOfMonth_30, _hourofday = HourOfDay_23, _minuteofhour = MinuteOfHour_01})
--
-- >>> parse parseSixFigureGroup "test" "090113Z"
-- Right (SixFigureGroup {_dayofmonth = DayOfMonth_09, _hourofday = HourOfDay_01, _minuteofhour = MinuteOfHour_13})
--
-- >>> isn't _Right (parse parseSixFigureGroup "test" "322301Z")
-- True
--
-- >>> isn't _Right (parse parseSixFigureGroup "test" "302401Z")
-- True
--
-- >>> isn't _Right (parse parseSixFigureGroup "test" "302360Z")
-- True
--
-- >>> isn't _Right (parse parseSixFigureGroup "test" "30230Z")
-- True
--
-- >>> parse (show <$> parseSixFigureGroup <|> string "302360Z") "test" "302360Z"
-- Right "302360Z"
parseSixFigureGroupz ::
  CharParsing f =>
  f SixFigureGroup
parseSixFigureGroupz =
  parseSixFigureGroup <* char 'Z'

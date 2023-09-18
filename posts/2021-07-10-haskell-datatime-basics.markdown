---
title: Quick guide to basic Date/Time operations in Haskell
description: Explaining how to consume frequently used date/time functions in Haskell
---

Recently I had to write end to end tests for my personal web project. I decided to write tests in Haskell so I don't need to debug runtime errors in tests and I can also utilize popular property testing libraries like [QuickCheck](https://www.cse.chalmers.se/~rjmh/QuickCheck) and [Hedgehog](https://hedgehog.qa) to test certain properties about each page and endpoint.

Many of the requests to the web application involved generating and sending timestamps in the universal ISO 8601 format. I'm documenting here the functions I've discovered for handling date and time in Haskell so I can refer to this later when required and also might help others.

## `time` package

[time](https://hackage.haskell.org/package/time) package is the de-facto standard for date/time representation and manipulation in Haskell. The types in the package are designed in such a way, that it will not allow meaning less operations. It is not uncommon for developers coming from mainstream programming languages like C#, JavaScript or Python to get frustated with `time` package.

The reason date and time is easy to deal with in those languages is because they allow invalid representations of date/time. For example, C# has only `DateTime` and `TimeSpan` types for all kinds of representation. Let's say you want to build an alarm app and would like to represent daily wake up time (say 7am), you still have to use `DateTime` or `TimeSpan`. Neither of that is accurate representation. Why? Because you've no clue if `DateTime` is your birthday in local timezone or an alarm time or end of the world in UTC. It depends on the context of the value's usage.

In fact that is why third party libraries like [NodaTime](https://nodatime.org/) or [Joda-Time](https://www.joda.org/joda-time/) exist for C# and Java respectively to replace the primitive and error prone types.

No wonder, `time` package follows Haskell's philosophy of correctness through types.

## Types

Here are the most commonly used date and time types provided by the `time` package under `Data.Time` module.

| Type              | Description                                                                                            |
| ----------------- | ------------------------------------------------------------------------------------------------------ |
| `UTCTime`         | An instant in UTC representing both date and time. This is the most common type you would want to use. |
| `NominalDiffTime` | Length of time or interval between two `UTCTime`, as measured by UTC.                                  |
| `Day`             | Represents only date without time and TimeZone info.                                                   |
| `TimeOfDay`       | Represents only time.                                                                                  |
| `LocalTime`       | Date with Time (Combination of `Day` and `TimeOfDay`) without TimeZone info.                           |
| `TimeZone`        | Whole number of minutes offset from UTC, together with a name.                                         |
| `ZonedTime`       | Date with Time (Combination of `Day` and `TimeOfDay`) with TimeZone info.                              |

\
These are only basic types. There are even richer types that you can find here from the official [quick start guide](https://hackage.haskell.org/package/time-1.12/docs/Data-Time.html).

## Get current Local Time

Most of the date and time functions in Haskell are pure functions. But getting current time in a function makes it non-deterministic and not referentially transparent. So `getZonedTime` produces an IO monad with the type `IO ZonedTime`.

`getZonedTime :: IO ZonedTime` is what you need to get the current local time along with local timezone information.

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getZonedTime
    print now                -- 2021-07-06 23:57:48.187103629 IST
```

## Get current Epoch (Unix) time

Epoch is an Unix time standard where time is represented as seconds elapsed since 1970-01-01 00:00 UTC. This format is easy to parse and transmit over wire since it is basically an integer. `getPOSIXTime :: IO POSIXTime` from `Data.Time.Clock.POSIX` can be used to get current epoch time.

```haskell
import Data.Time.Clock.POSIX

main :: IO ()
main = do
    epochNow <- getPOSIXTime
    print $ round epochNow   -- 1625594110
```

## Get current UTC Time

`getCurrentTime :: IO UTCTime` is what you need to get the current UTC time.

```haskell
import Data.Time

main :: IO ()
main = do
    utcNow <- getCurrentTime
    print utcNow            -- 2021-07-06 18:28:51.693103262 UTC
```

## Add to UTCTime

Arithmetic operations on UTCTime involves additional type called `NominalDiffTime` which is basically an integer type that reperesents length of time (for example, 3000 seconds). Let's say you want to add one hour to the UTCTime, first you've to construct `NominalDiffTime`. Fortunately, `NominalDiffTime` supports `Num` instance which means you can construct an instance using `fromInteger`. `fromInteger` treats given argument as seconds and produces a `NominalDiffTime` instance which can then be used to add to UTCTime.

`addUTCTime` function takes a `UTCTime` and `NominalDiffTime` and evaluates to the `UTCTime` with the time added.

```haskell
oneHour :: NominalDiffTime
oneHour = fromInteger 3600          -- secs

addOneHour :: UTCTime -> UTCTime
addOneHour x = addUTCTime oneHour x

```

##### Subtract from UTCTime

Just `negate` the `NominalDiffTime` to subtract.

```haskell
oneHour :: NominalDiffTime
oneHour = fromInteger 3600          -- secs

subtractOneHour :: UTCTime -> UTCTime
subtractOneHour x = addUTCTime (negate oneHour) x

```

## Formatting

Displaying or sending date/time over wire involves converting the type into a string format. So `time` package provides these facilities through `FormatTime` typeclass. Most of the date/time types support `FormatTime` instance which give us a function called `formatTime` to produce a string in the desired format.

```haskell
formatTime
:: FormatTime t
=> TimeLocale           -- Type that provides functionalities to produce name of weekday, month, AM/PM symbols, timezones etc. in a particular locale. We have also predefined `TimeLocale` such as `defaultTimeLocale` for English.
-> String               -- Format string like `"%H:%M"` or `"%Y-%m-%d"`. Complete specification is [documented here](https://hackage.haskell.org/package/time-1.12/docs/Data-Time-Format.html).
-> t                    -- One of the date/time types such as `UTCTime` or `LocalTime`.
-> String               -- Return the formatted string
```

**Example usage**

---

```haskell
import Data.Time

printTime :: UTCTime -> String
printTime x = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" x
              -- produces "2021-07-05 14:40"
```

---

### Formatting to ISO8601

Most applications need to serialize date/time values to the standard ISO8601 format. Though you can use the regular `formatTime` function, it involves more ceremony. Luckily, this facility is already available through `Format` typeclass from the `Data.Time.Format.ISO8601` module.

You can use either

```haskell
formatShow :: Format t -> t -> String
```

or

```haskell
formatShowM :: Format t -> t -> Maybe String
```

function depending on the level of type safety you need.

**Example usage**

```haskell
formatUTCToISO8601 :: UTCTime -> String
formatUTCToISO8601 x = formatShow iso8601Format x  -- produces "2021-07-05T14:40:25.436865727Z"
```

Note: ISO8601 string can be generated from not just `UTCTime` but also from `Day`, `LocalTime`, `ZonedTime` or any other date/time types. Because all these types comes with `Format t` instance.

## Parsing

Just like formatting, date/time types can be constructed from string representations. It's not uncommon for applications to construct Date type from a string like "2021-07-15". This ability comes from a typeclass called `ParseTime` which all common date/time types implement. Use the `parseTimeM` function to construct a type from string.

```haskell
parseTimeM
:: (MonadFail m, ParseTime t)
=> Bool                     -- Accept leading and trailing whitespace?
-> TimeLocale               -- Time locale like defaultTimeLocale
-> String                   -- Format string like "%Y-%-m-%-d"
-> String                   -- Input string like "2010-3-04"
-> m t                      -- Return the time value, or fail if the input could not be parsed using the given format.
```

**Example usage**

```haskell

stringToDay :: String -> Maybe Day
stringToDay s = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" "2020-09-04 12:30" :: Maybe LocalTime
                -- evaluates to Just 2020-09-04 12:30:00

```

### Parsing from ISO8601

If you have a ISO8601 timestamp as a string, it can be easily converted to `UTCTime` through `iso8601ParseM` function from `Data.Time.Format.ISO8601` module.

```haskell
iso8601ParseM
:: (MonadFail m, ISO8601 t)
=> String                       -- ISO8601 string. Example: "2021-07-05T14:40:25.436865727Z"
-> m t                          -- Return a monad that supports MonadFail. Example: Maybe
```

**Example usage**

```haskell
> import Data.Time.Format.ISO8601

> iso8601ParseM "2021-07-05T14:40:25.436865727Z" :: Maybe UTCTime  -- produces Just 2021-07-05 14:40:25.436865727 UTC

```

Hope this helped you. If you need guide to even more advanced functionalities, please check out these excellent blog posts.

[https://two-wrongs.com/haskell-time-library-tutorial](https://two-wrongs.com/haskell-time-library-tutorial)

[https://www.williamyaoh.com/posts/2019-09-16-time-cheatsheet.html](https://www.williamyaoh.com/posts/2019-09-16-time-cheatsheet.html)

---

**References**

1. [https://hackage.haskell.org/package/time-1.12/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.12/docs/Data-Time.html)
2. [https://two-wrongs.com/haskell-time-library-tutorial](https://two-wrongs.com/haskell-time-library-tutorial)
3. [https://www.williamyaoh.com/posts/2019-09-16-time-cheatsheet.html](https://www.williamyaoh.com/posts/2019-09-16-time-cheatsheet.html)
4. [https://wiki.haskell.org/Cookbook/Dates_And_Time](https://wiki.haskell.org/Cookbook/Dates_And_Time)

# Haskell type safety: a comprehensive guide for AI code assistants

Haskell's type system is the most powerful mainstream tool for preventing bugs at compile time. **The central philosophy is this: encode invariants in types so the compiler rejects invalid programs before they run.** This guide covers 17 interlocking techniques—from simple newtypes to type-level programming—that form a layered defense against bugs. Each technique is illustrated with concrete code examples and grounded in community best practices from Alexis King, Matt Parsons, Sandy Maguire, and production Haskell shops. An AI code assistant that masters these patterns can generate Haskell code that is correct by construction rather than correct by testing.

---

## 1. Make illegal states unrepresentable

This phrase, coined by Yaron Minsky in 2007, is the foundational principle of Haskell type design. The idea: **structure your data types so that invalid combinations of values cannot be constructed at all**. Compile-time enforcement replaces runtime validation.

A common antipattern uses `Maybe` fields that must be synchronized:

```haskell
-- BAD: shipmentInfo can be Just when status is Outstanding, or Nothing when PaidFor
data Order = Order
  { status       :: OrderStatus
  , shipmentInfo :: Maybe ShipmentInfo
  }
data OrderStatus = Outstanding | PaidFor
```

Four combinations of `(OrderStatus, Maybe ShipmentInfo)` exist, but only two are valid. The fix uses separate constructors:

```haskell
-- GOOD: invalid combinations are impossible to construct
data Order
  = OutstandingOrder OrderData
  | PaidOrder OrderData ShipmentInfo
```

Now a paid order always has shipment info, and an outstanding order never does. **No runtime check required.** The same principle applies to correlated optionals—replace `Maybe a -> Maybe b -> ...` with `Maybe (a, b) -> ...` when both must be present or absent together.

The principle extends to state machines encoded in types using GADTs (covered in section 4):

```haskell
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

data CheckoutState (s :: *) where
  NoItems      :: CheckoutState Empty
  HasItems     :: [Item] -> CheckoutState Filled
  CardSelected :: [Item] -> Card -> CheckoutState Ready
  OrderPlaced  :: OrderId -> CheckoutState Complete

-- Transition functions enforce valid state flow:
selectCard :: Card -> CheckoutState Filled -> CheckoutState Ready
selectCard card (HasItems items) = CardSelected items card

placeOrder :: CheckoutState Ready -> IO (CheckoutState Complete)
placeOrder (CardSelected items card) = OrderPlaced <$> processPayment items card

-- placeOrder (HasItems [...]) is a TYPE ERROR — cannot skip card selection
```

Alexis King distinguishes **intrinsic** vs. **extrinsic** safety. `data OneToFive = One | Two | Three | Four | Five` is intrinsically safe—illegal values are literally unutterable. A `newtype OneToFive = OneToFive Int` with a hidden constructor is extrinsically safe—it relies on the module boundary. Prefer intrinsic safety where practical; use extrinsic safety (smart constructors + opaque types) as the fallback.

---

## 2. Newtypes prevent argument transposition bugs

When functions accept multiple parameters of the same primitive type, arguments can be silently swapped with no compile-time error. This is one of the most common bug categories in any language:

```haskell
-- BAD: both arguments are Double — easy to swap
calculateArea :: Double -> Double -> Double
calculateArea width height = width * height

result = calculateArea myHeight myWidth  -- compiles, silently wrong
```

The fix wraps each concept in a `newtype`. Newtypes have **zero runtime overhead**—GHC erases the wrapper after type checking:

```haskell
newtype Width  = Width  Double deriving (Show)
newtype Height = Height Double deriving (Show)

calculateArea :: Width -> Height -> Double
calculateArea (Width w) (Height h) = w * h

-- calculateArea (Height 10) (Width 5)
-- ERROR: Couldn't match expected type 'Width' with actual type 'Height'
```

Real-world Haskell libraries frequently make this mistake with `type` aliases, which provide **zero type safety**:

```haskell
-- BAD: type aliases are just synonyms — all interchangeable!
type WorkerId     = UUID
type SupervisorId = UUID
type ProcessId    = UUID
-- A WorkerId can be passed where SupervisorId is expected
```

Use `newtype` instead. The `GeneralizedNewtypeDeriving` extension eliminates boilerplate by reusing the wrapped type's instances:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Dollars = Dollars { getDollars :: Int }
  deriving (Eq, Show, Num, Ord)

-- Arithmetic works without manual instances:
total = Dollars 5 + Dollars 3  -- Dollars 8
```

`DerivingVia` goes further, letting you choose *which* type's instance to reuse:

```haskell
{-# LANGUAGE DerivingVia #-}

newtype Salary = Salary Int
  deriving (Semigroup, Monoid) via (Sum Int)
```

---

## 3. Phantom types track state at the type level

A phantom type parameter appears in the type signature but not in the data constructor. It exists purely for compile-time tracking with zero runtime overhead:

```haskell
data Sanitized
data Unsanitized

newtype FormData a = FormData String  -- 'a' is phantom

userInput :: String -> FormData Unsanitized
userInput = FormData

sanitize :: FormData Unsanitized -> FormData Sanitized
sanitize (FormData s) = FormData (filter isAlpha s)

insertIntoDb :: FormData Sanitized -> IO ()
insertIntoDb (FormData s) = putStrLn ("Storing: " ++ s)

-- insertIntoDb (userInput "<script>alert('xss')</script>")
-- ERROR: Couldn't match type 'Unsanitized' with 'Sanitized'
-- The ONLY path to Sanitized is through the sanitize function.
```

The pattern works equally well for file handle permissions:

```haskell
data ReadMode
data WriteMode

newtype Handle mode = Handle FilePath

openRead  :: FilePath -> IO (Handle ReadMode)
openWrite :: FilePath -> IO (Handle WriteMode)

readData  :: Handle ReadMode -> IO String
writeData :: Handle WriteMode -> String -> IO ()

-- writeData readHandle "hello"
-- ERROR: Couldn't match type 'ReadMode' with 'WriteMode'
```

And for preventing unit confusion (the Mars Climate Orbiter problem):

```haskell
data USD
data EUR

newtype Amount a = Amount Double deriving (Show, Num)

-- (Amount 10 :: Amount USD) + (Amount 20 :: Amount EUR)
-- ERROR: Couldn't match type 'EUR' with 'USD'
```

Functions that don't care about the phantom parameter can be polymorphic over it, while functions that enforce a specific state constrain it. This gives you fine-grained compile-time control over which operations are valid in which states.

---

## 4. GADTs encode invariants in constructors

Generalized Algebraic Data Types let each constructor specify its own return type. This enables the compiler to **refine types during pattern matching**, unlocking type-safe expression trees, well-typed ASTs, and length-indexed vectors.

### Well-typed expression AST

```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
  LitInt  :: Int  -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  Equal   :: Expr Int -> Expr Int -> Expr Bool
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a

-- The evaluator is TOTAL — no runtime type errors possible:
eval :: Expr a -> a
eval (LitInt n)    = n           -- GHC knows a ~ Int
eval (LitBool b)   = b           -- GHC knows a ~ Bool
eval (Add e1 e2)   = eval e1 + eval e2
eval (Equal e1 e2) = eval e1 == eval e2
eval (If c t f)    = if eval c then eval t else eval f

-- Ill-typed expressions are IMPOSSIBLE to construct:
-- Add (LitBool True) (LitInt 3)
-- ERROR: Couldn't match type 'Bool' with 'Int'
```

When you pattern match on `LitInt n`, GHC learns `a ~ Int` for that branch. This **type refinement** is the key GADT power—it lets `eval` return `Int` in one branch and `Bool` in another while maintaining the unified return type `a`. A type signature is required for GADT pattern-matching functions; without it, GHC cannot perform refinement.

### Length-indexed vectors

```haskell
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies #-}

data Nat = Zero | Succ Nat

data Vector (n :: Nat) a where
  VNil  :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

-- Total head — cannot be called on empty vectors:
vhead :: Vector ('Succ n) a -> a
vhead (VCons x _) = x
-- vhead VNil is a TYPE ERROR, not a runtime crash

type family Plus (m :: Nat) (n :: Nat) :: Nat where
  Plus 'Zero     n = n
  Plus ('Succ m) n = 'Succ (Plus m n)

vappend :: Vector m a -> Vector n a -> Vector (Plus m n) a
vappend VNil         ys = ys
vappend (VCons x xs) ys = VCons x (vappend xs ys)
```

GADTs also power the order state machine pattern from section 1, where `Order 'PaidFor` carries different data than `Order 'Outstanding`, and functions like `refundOrder :: Order 'PaidFor -> m ()` are statically guaranteed to receive only paid orders.

---

## 5. Type-level programming with DataKinds and type families

The `DataKinds` extension promotes every data type to a kind and its value constructors to type constructors. This creates a richer kind system where type-level values carry domain meaning:

```haskell
{-# LANGUAGE DataKinds #-}

data Nat = Zero | Succ Nat
-- Without DataKinds: Zero and Succ are value constructors of kind *
-- With DataKinds: 'Zero :: Nat and 'Succ :: Nat -> Nat exist at the type level
```

The tick mark `'` disambiguates promoted constructors. Without `DataKinds`, type-level "naturals" like `data Ze; data Su n` have kind `*`, meaning nonsensical types like `Vec Int Char` are well-kinded. With `DataKinds`, `'Zero :: Nat` is a distinct kind, preventing such errors.

GHC also provides built-in type-level naturals via `GHC.TypeLits`:

```haskell
import GHC.TypeLits
import Data.Proxy

-- KnownNat bridges type-level to term-level:
showNat :: forall n. KnownNat n => Proxy n -> String
showNat p = show (natVal p)
-- showNat (Proxy :: Proxy 42) ==> "42"
```

This enables **sized vectors** backed by efficient runtime representations:

```haskell
newtype SizedVector (n :: Nat) a = SizedVector (V.Vector a)

append :: SizedVector n a -> SizedVector m a -> SizedVector (n + m) a
append (SizedVector x) (SizedVector y) = SizedVector (x V.++ y)

-- Safe construction requires runtime length to match type-level size:
tryMakeSized :: forall n a. KnownNat n => V.Vector a -> Maybe (SizedVector n a)
tryMakeSized v
  | V.length v == fromIntegral (natVal (Proxy :: Proxy n)) = Just (SizedVector v)
  | otherwise = Nothing
```

**Type-safe matrix multiplication** demonstrates the power: a `Matrix m n a` multiplied by `Matrix n p a` produces `Matrix m p a`. The shared dimension `n` must match—dimension mismatches are compile-time errors, not runtime crashes.

**Closed type families** define type-level functions with ordered equations, evaluated top-to-bottom:

```haskell
type family If (c :: Bool) (t :: k) (e :: k) :: k where
  If 'True  t _ = t
  If 'False _ e = e
```

The **singleton pattern** bridges type and term levels, letting runtime values carry type-level evidence:

```haskell
data SNat :: Nat -> * where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

replicate' :: SNat n -> a -> Vector n a
replicate' SZ     _ = VNil
replicate' (SS n) x = VCons x (replicate' n x)
```

| Extension | Purpose |
|-----------|---------|
| `DataKinds` | Promote data types to kinds; constructors become type constructors |
| `KindSignatures` | Annotate type parameters with their kind: `(n :: Nat)` |
| `TypeFamilies` | Define functions at the type level |
| `TypeOperators` | Use operators in types: `n + m` |
| `ScopedTypeVariables` | Let `forall`-bound variables scope into `where` clauses |

---

## 6. Exhaustive pattern matching catches missing cases

GHC's `-Wincomplete-patterns` (included in `-Wall`) warns when a function doesn't handle all constructors of a sum type. This is one of the most powerful features for maintaining large codebases:

```haskell
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r)      = pi * r * r
area (Rectangle w h) = w * h

-- Later, add a constructor:
data Shape = Circle Double | Rectangle Double Double | Triangle Double Double

-- GHC immediately warns:
-- Pattern match(es) are non-exhaustive
-- In an equation for 'area': Patterns not matched: Triangle _ _
```

**Every call site that handles `Shape` gets a compiler warning.** This is qualitatively superior to `if-else` chains, which give the compiler no structural knowledge about completeness.

Wildcard patterns defeat this protection and should be avoided:

```haskell
-- BAD: wildcard silently swallows new constructors
tasty :: Sweet -> Bool
tasty Cupcake = True
tasty Cookie  = True
tasty _       = False  -- Adding Cheesecake? No warning — silently "not tasty"

-- GOOD: explicit patterns for every constructor
tasty Cupcake   = True
tasty Cookie    = True
tasty Liquorice = False
tasty Raisins   = False
-- Adding Cheesecake now triggers a non-exhaustive warning
```

Use `-Werror=incomplete-patterns` in CI to turn these warnings into hard compile errors:

```cabal
ghc-options: -Wall -Werror=incomplete-patterns
```

---

## 7. Total functions eliminate runtime crashes

Partial functions—those that crash on certain inputs—are one of Haskell's worst legacy design decisions. **Every use of `head`, `tail`, `fromJust`, `read`, or `!!` is a potential runtime crash:**

| Function | Crash trigger | Safer alternative |
|----------|--------------|-------------------|
| `head` | Empty list | Pattern match or `Data.List.NonEmpty.head` |
| `tail` | Empty list | Pattern match or `Data.List.NonEmpty.tail` |
| `fromJust` | `Nothing` | `maybe`, `fromMaybe`, or pattern match |
| `read` | Parse failure | `readMaybe` from `Text.Read` |
| `!!` | Out of bounds | `Data.Vector.!?` or bounds-checked indexing |
| `maximum` | Empty list | `maximumMay` from `safe` or use `NonEmpty` |

Replace partial patterns with total ones:

```haskell
-- BAD:
processFirst xs = doSomething (head xs)

-- GOOD:
processFirst (x:_) = doSomething x
processFirst []    = handleEmpty

-- BETTER: require non-emptiness in the type
processFirst :: NonEmpty a -> Result
processFirst (x :| _) = doSomething x  -- total, no Maybe needed
```

The **`safe` package** by Neil Mitchell provides safe variants: `headMay`, `headDef`, `headNote`, `readMay`, `atMay`, `tailSafe`, and more. Each partial function gets up to five variants covering different failure-handling strategies.

```haskell
import Safe

headMay [1,2,3]  -- Just 1
headMay []       -- Nothing
headDef 0 []     -- 0
readMay "42" :: Maybe Int  -- Just 42
readMay "abc" :: Maybe Int -- Nothing
```

A subtler danger: **`fromIntegral` silently truncates or overflows** across some type pairs. `fromIntegral (256 :: Int) :: Word8` produces `0` with no error. Write explicit, named conversion functions for numeric types.

---

## 8. Smart constructors enforce invariants at construction

The smart constructor pattern hides raw data constructors and exposes only validated construction functions. The module system is the enforcement mechanism:

```haskell
module Email (Email, mkEmail, unEmail) where  -- Email type, NOT constructor

newtype Email = Email Text

mkEmail :: Text -> Either EmailError Email
mkEmail t
  | "@" `T.isInfixOf` t && "." `T.isInfixOf` t = Right (Email t)
  | otherwise = Left InvalidEmailFormat

unEmail :: Email -> Text
unEmail (Email t) = t
```

External code cannot write `Email "not-an-email"`. The **only** way to obtain an `Email` value is through `mkEmail`, which validates the input. This moves all validation to construction time; all downstream code can trust the invariant.

Key implementation details from the Kowainik patterns handbook:

- Don't define a record field on the newtype (`newtype Email = Email { unEmail :: Text }`) because record update syntax could bypass validation
- Use a separate accessor function instead
- Hiding the constructor also prevents `Data.Coerce.coerce` from bypassing validation
- Provide an `unsafeEmail` escape hatch if needed for testing, with the `unsafe` prefix making it visible in code review

For richer error reporting, return `Either ValidationError a`:

```haskell
data ValidationError
  = WrongNumberOfGroups Int
  | InvalidGroupLength Int Text
  | InvalidCharacters (HashSet Char)

makeSerialNumber :: Text -> Either ValidationError SerialNumber
```

**PatternSynonyms** can restore pattern matching without exposing the constructor:

```haskell
module NonZero (NonZero(), pattern NonZero, nonZero) where

newtype NonZero a = UnsafeNonZero a
pattern NonZero a <- UnsafeNonZero a  -- read-only pattern

nonZero :: (Num a, Eq a) => a -> Maybe (NonZero a)
nonZero 0 = Nothing
nonZero i = Just (UnsafeNonZero i)
```

---

## 9. Refinement types and Liquid Haskell

Liquid Haskell adds **refinement types**—types decorated with logical predicates checked at compile time via an SMT solver (Z3). Specifications are written in special comments `{-@ ... @-}` so they don't affect normal compilation:

```haskell
{-@ type Pos = {v:Int | v > 0} @-}

{-@ safeDiv :: Int -> {v:Int | v /= 0} -> Int @-}
safeDiv :: Int -> Int -> Int
safeDiv x y = x `div` y

-- safeDiv 10 0  →  LIQUID TYPE ERROR at compile time
```

Liquid Haskell can prove array indices are in bounds:

```haskell
{-@ (!!) :: xs:[a] -> {n:Int | 0 <= n && n < len xs} -> a @-}
```

And verify that functions preserve invariants:

```haskell
{-@ avg :: {xs:[Int] | len xs > 0} -> Int @-}
avg :: [Int] -> Int
avg xs = safeDiv (sum xs) (length xs)
-- LiquidHaskell verifies: len xs > 0 implies length xs /= 0
```

Liquid Haskell is now available as a **GHC plugin** rather than a standalone tool. It has verified over 10,000 lines of real-world Haskell code including `containers`, `bytestring`, and `text`. The limitation is tooling maturity—it requires Z3 and can need manual annotations for complex cases. For most production Haskell, the techniques in this guide's other sections provide sufficient safety without the Liquid Haskell overhead.

---

## 10. Opaque types and module boundaries

Haskell's module system is the **only** mechanism for building abstract data types. Export the type name but not its constructors:

```haskell
module Stack (Stack, empty, push, pop, top) where  -- Stack, not Stack(..)

newtype Stack a = Stk [a]

empty :: Stack a
empty = Stk []

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stk (x:xs)) = (x, Stk xs)
pop (Stk [])     = error "empty stack"
```

Since `Stk` is not exported, external code can only use the public API. The internal representation could change from `[a]` to `Vector` without affecting any consumer. This pattern is used throughout the Haskell ecosystem: `Data.Map` and `Data.Set` maintain balanced-tree invariants behind opaque constructors; `Data.Text` hides its internal `Array` representation.

The guarantee of an opaque type requires three pillars working together: **strong static types** (the compiler enforces type distinctions), **purity** (no side-channels to mutate internals), and the **module system** (hiding constructors prevents direct construction). If any pillar is missing, the guarantee breaks.

---

## 11. Purity eliminates entire bug categories

Pure functions always produce the same output for the same input and have no side effects. This makes them **trivially testable**—no mock objects, no setup, no dependency injection:

```haskell
testSort :: Bool
testSort = sort [3,1,2] == [1,2,3]  -- That's the whole test
```

**Referential transparency** means any expression can be replaced by its value. This eliminates aliasing bugs entirely—sharing references is always safe because values are immutable. It also enables **equational reasoning**: you can substitute equals for equals just like in mathematics, and prove properties of compositions like `map f . map g == map (f . g)`.

The **IO monad** enforces a strict boundary. A pure function with type `a -> b` literally cannot perform I/O—the type system prevents it:

```haskell
-- This WON'T COMPILE:
badPure :: Int -> Int
badPure x = do
    putStrLn "side effect!"  -- TYPE ERROR: IO () is not Int
    x + 1
```

**No race conditions in pure code.** Since pure functions don't access shared mutable state, they are inherently thread-safe. `parMap f xs` parallelizes a pure computation across threads with no locks, no mutexes, and no possibility of data races.

The correct architecture pushes IO to the edges: parse input in IO, process it with pure functions, then output results in IO. The pure core is where business logic lives, and it's the easiest code to test and reason about.

---

## 12. Strict data prevents space leaks

Haskell's laziness can cause **space leaks** when unevaluated thunks accumulate. The classic example:

```haskell
-- SPACE LEAK: accumulator builds up chain of unevaluated additions
badSum = go 0 [1..1000000]
  where
    go acc []     = acc
    go acc (x:xs) = go (acc + x) xs
    -- go (((0+1)+2)+3)... — millions of thunks in memory!
```

**BangPatterns** force evaluation at pattern-match time:

```haskell
{-# LANGUAGE BangPatterns #-}

goodSum = go 0 [1..1000000]
  where
    go !acc []     = acc       -- !acc forces evaluation immediately
    go !acc (x:xs) = go (acc + x) xs
    -- Constant memory: each addition evaluated before recursion
```

Measurements show this reduces memory from **~93 MB to ~5 MB** for summing one million integers.

**StrictData** makes all fields in data types strict by default within a module:

```haskell
{-# LANGUAGE StrictData #-}

-- Equivalent to: data Config = Config !Text !Int !Bool
data Config = Config
    { configName  :: Text
    , configPort  :: Int
    , configDebug :: Bool
    }
```

A subtler trap: `foldl'` is strict in the accumulator, but if the accumulator is a tuple, it only evaluates to WHNF (the tuple constructor), leaving the tuple's fields as thunks:

```haskell
-- STILL LEAKS: foldl' forces the pair, but not its contents
pairFold = foldl' (\(count, total) x -> (count+1, total+x)) (0,0) [1..1000000]

-- FIX: bang the fields
pairFold = foldl' (\(!count, !total) x -> (count+1, total+x)) (0,0) [1..1000000]
```

**Best practice**: enable `StrictData` as a default extension in your `.cabal` file. Use `~` (tilde) to opt individual fields back into laziness for infinite structures, streams, or "tying the knot" patterns.

---

## 13. Type-driven development treats the compiler as a partner

Typed holes (`_`) let you write incomplete programs and ask GHC what type goes in each gap:

```haskell
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f (x:xs) = case f x of
    Nothing -> _           -- GHC reports: Found hole '_' with type: [b]
    Just y  -> _           -- GHC reports: Found hole '_' with type: [b]
                           -- Relevant bindings: y :: b, f :: a -> Maybe b, xs :: [a]
```

Named holes (`_result`, `_combine`) let you track multiple unknowns. GHC reports the type and all in-scope bindings for each hole, effectively telling you what you can use to fill it.

The **type-driven development workflow**:

1. Write the type signature first
2. Stub the body with holes
3. Read GHC's reports about each hole's expected type and available bindings
4. Progressively refine, replacing holes with expressions guided by the types
5. When all holes are filled, the function type-checks

**Deferred type errors** (`-fdefer-typed-holes`) convert holes to runtime warnings, letting you compile and test other parts while leaving some functions incomplete. This is invaluable for incremental development.

GHCi provides interactive type exploration:

```
ghci> :type foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

ghci> :kind Maybe
Maybe :: * -> *

ghci> :type (>>=) @Maybe
(>>=) @Maybe :: Maybe a -> (a -> Maybe b) -> Maybe b
```

---

## 14. GHC warnings that catch real bugs

**`-Wall`** enables the most important warnings. These are the individual flags that matter most for correctness:

- **`-Wincomplete-patterns`**: Non-exhaustive pattern matches (included in `-Wall`)
- **`-Wincomplete-uni-patterns`**: Same for lambdas and pattern bindings (NOT in `-Wall`)
- **`-Wmissing-fields`**: Record construction with missing fields
- **`-Wunused-binds`**: Dead code detection
- **`-Wredundant-constraints`**: Unnecessary typeclass constraints (NOT in `-Wall`)
- **`-Wpartial-fields`**: Record fields that create partial accessor functions (NOT in `-Wall`)

Recommended production configuration in your `.cabal` file:

```cabal
ghc-options:
  -Wall
  -Wincomplete-uni-patterns
  -Wincomplete-record-updates
  -Wpartial-fields
  -Widentities
  -Wredundant-constraints
```

For CI, add `-Werror` to turn all warnings into hard compile errors. Some teams use `-Weverything` with explicit `-Wno-*` flags for the few warnings they disagree with—this ensures new GHC versions automatically enable new warnings.

---

## 15. Property-based testing leverages the type system

QuickCheck and Hedgehog generate hundreds of random test cases and automatically **shrink** failing inputs to minimal counterexamples. Haskell's type system powers this through the `Arbitrary` typeclass:

```haskell
import Test.QuickCheck

prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs

-- quickCheck prop_reverseReverse
-- +++ OK, passed 100 tests.
```

**Roundtrip properties** are the most natural pattern for typed code:

```haskell
prop_jsonRoundtrip :: Person -> Bool
prop_jsonRoundtrip p = decode (encode p) == Just p

prop_showRead :: Int -> Bool
prop_showRead x = read (show x) == x
```

Custom generators give fine-grained control:

```haskell
genAge :: Gen Int
genAge = choose (0, 120)

genPerson :: Gen Person
genPerson = Person <$> genName <*> genAge
```

**Shrinking** is the killer feature: when a property fails on a large input like `[45, -12, 99, 3, -7]`, QuickCheck automatically tries smaller inputs until it finds the minimal counterexample, often something like `[0, 1]`.

**Hedgehog** improves on QuickCheck with **integrated shrinking**—generators carry shrink trees, so shrinking always respects generator invariants by construction. Hedgehog uses explicit generator values instead of a typeclass, avoiding orphan instance problems:

```haskell
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_reverse :: Property
prop_reverse = property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
```

Algebraic properties make excellent properties: commutativity, associativity, identity, idempotence, and distributivity can all be tested generically over any type with the right structure.

---

## 16. Custom sum types cure boolean blindness

The term "boolean blindness," popularized by Robert Harper, describes the problem: **a `Bool` carries no information about its provenance.** `True` from `isAdmin user` looks identical to `True` from `isRed car`—the compiler cannot distinguish them.

```haskell
-- BAD: What does True mean?
handleRequest :: Bool -> Request -> IO Response
handleRequest True  req = processRequest req
handleRequest False req = denyRequest req
-- Nothing prevents passing (isRed car) where (isAdmin user) was intended
```

The fix uses a custom sum type:

```haskell
data Access = Granted | Denied

checkAccess :: User -> Access
checkAccess user
  | isAdmin user = Granted
  | otherwise    = Denied

handleRequest :: Access -> Request -> IO Response
handleRequest Granted req = processRequest req
handleRequest Denied  req = denyRequest req
```

Sum types also scale naturally. When you add a third access level, the compiler finds every site that needs updating—something impossible with `Bool`:

```haskell
data Access = Granted | Denied | RequiresMFA
-- GHC warns everywhere Access is pattern-matched without handling RequiresMFA
```

Other examples: `data Parity = Even | Odd` instead of `isEven :: Int -> Bool`; `data IOMode = ReadMode | WriteMode | AppendMode` instead of multiple booleans; `data Keep = Keep | Drop` instead of ambiguous filter predicates.

---

## 17. Parse, don't validate

Alexis King's 2019 blog post articulated the most influential Haskell design principle of the past decade. **A parser consumes less-structured input and produces more-structured output. A validator checks a property and throws away the result.**

```haskell
-- VALIDATION: checks but discards information
validateNonEmpty :: [a] -> IO ()
validateNonEmpty (_:_) = pure ()
validateNonEmpty []    = throwIO $ userError "list cannot be empty"

-- PARSING: checks AND preserves information in the type
parseNonEmpty :: [a] -> IO (NonEmpty a)
parseNonEmpty (x:xs) = pure (x :| xs)
parseNonEmpty []     = throwIO $ userError "list cannot be empty"
```

The practical difference is dramatic. With validation, downstream code must redundantly check or use partial functions:

```haskell
-- After validation, head might still fail — the type doesn't know the list is non-empty
main = do
  dirs <- getConfigDirs          -- returns [FilePath]
  validateNonEmpty dirs
  initCache (head dirs)          -- partial! could crash if validation is removed
```

With parsing, downstream code uses total functions on refined types:

```haskell
-- After parsing, NonEmpty guarantees non-emptiness
main = do
  dirs <- parseConfigDirs        -- returns NonEmpty FilePath
  initCache (NE.head dirs)       -- total! NE.head :: NonEmpty a -> a always succeeds
```

Matt Parsons extends this in "Type Safety Back and Forth": there are two ways to make a partial function total. **Weakening the return type** (`head :: [a] -> Maybe a`) pushes burden onto callers. **Strengthening the argument type** (`head :: NonEmpty a -> a`) pushes burden onto producers. Strengthening arguments is almost always superior because it eliminates redundant checks throughout the codebase.

Parsons' "Keep Your Types Small" post makes this precise using **cardinality**: `Maybe a` has `1 + |a|` inhabitants, expanding the output space. `NonEmpty a` is a strict subset of `[a]`, restricting the input space. **Smaller types = fewer invalid states = fewer bugs.** Use `Natural` instead of `Int` when values can't be negative. Use `NonEmpty` instead of `[]` when emptiness is invalid. Use `NonZero Int` instead of `Int` for divisors.

King identifies the antipattern of **shotgun parsing**: scattering validation throughout processing code, hoping one check or another catches all bad cases. The fix is to stratify your program: **parse at the boundary** (where invalid input is rejected with structured errors), then **execute internally** (where only well-typed data flows and failure modes are minimal).

King's practical guidelines:

- Let your datatypes inform your code, not the reverse
- Don't be afraid to parse data in multiple passes
- Avoid denormalized representations (duplicated data invites inconsistency)
- Use abstract datatypes to make validators "look like" parsers when full constructive modeling isn't practical

---

## The progression of type safety techniques

These 17 techniques form a **layered defense** from simplest to most advanced. Each layer catches bugs the previous layers miss:

1. **Newtypes** — prevent argument confusion at zero runtime cost
2. **Custom sum types** — cure boolean blindness, enable exhaustive matching
3. **Smart constructors + opaque modules** — enforce invariants at construction time
4. **Phantom types** — track state, permissions, and units at the type level
5. **GADTs** — encode state machines and complex invariants in constructors
6. **DataKinds + type families** — type-level computation for dimensions, sizes, and indices
7. **Liquid Haskell** — SMT-verified refinement types for mathematical properties
8. **Purity + strictness** — eliminate mutation bugs and space leaks
9. **`-Wall` + totality** — catch missing cases and partial functions
10. **Property-based testing** — validate algebraic properties with automatic shrinking

An AI code assistant generating Haskell should default to these patterns: use newtypes for all domain concepts, prefer `NonEmpty` over `[]` and `Natural` over `Int`, hide constructors behind smart constructors, enable `-Wall -Wincomplete-uni-patterns -Wredundant-constraints`, avoid all partial Prelude functions, and use sum types instead of booleans. The result is code where the compiler catches the bugs before the tests even run.
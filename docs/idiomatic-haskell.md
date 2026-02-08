# Writing idiomatic, terse Haskell: a comprehensive reference

An AI code assistant producing Haskell should default to concise, idiomatic patterns rather than verbose, beginner-style code. This means leveraging composition over explicit recursion, choosing the weakest sufficient abstraction (Functor before Applicative before Monad), using standard combinators and eliminators, and enabling the right GHC extensions. The patterns below represent community consensus drawn from the Kowainik style guide, Tibell's style guide, Tweag's guidelines, HLint rules, the STAN static analyzer, and common practice on Hackage. **Every section includes concrete before/after code examples** that demonstrate the transformation from verbose to idiomatic.

---

## Point-free style and the composition toolbox

Point-free (tacit) style defines functions without naming their arguments, building them instead by composing other functions. The canonical good case is a **simple composition chain of named functions**:

```haskell
-- Pointful
totalWordLengths s = sum (map length (words s))

-- Point-free (reads as a pipeline: words → map length → sum)
totalWordLengths = sum . map length . words
```

Point-free shines when every function in the chain is a well-known named function. It hurts when multi-argument functions force obscure combinator gymnastics — the `pointfree` tool (or Lambdabot's `@pl`) often produces unreadable output like `uncurry ((. return) . (:))`. **Rule of thumb**: if point-free requires `flip`, nested sections of `(.)`, or anonymous combinators, write it pointful.

The core operators for chaining, each with a distinct role:

| Operator | Direction | Typical use |
|---|---|---|
| `(.)` | Right-to-left | Composing pure functions: `f . g . h` |
| `($)` | Right-to-left | Final application, eliminating parens: `putStrLn $ show x` |
| `(&)` | Left-to-right | Pipeline style (from `Data.Function`): `xs & filter even & map (*2) & sum` |
| `(>>>)` | Left-to-right | Arrow/Category composition: `filter even >>> map (*2) >>> sum` |
| `(<$>)` | — | Mapping inside a functor: `(+1) <$> Just 5` |
| `(<*>)` | — | Applying functions inside a functor: `User <$> parseName <*> parseAge` |
| `(>>=)` | Left-to-right | Dependent monadic chaining: `getLine >>= putStrLn` |
| `(>>)` | Left-to-right | Sequencing, discarding first result: `putStr "Name: " >> getLine` |
| `(<>)` | — | Semigroup append: `"Hello" <> " " <> "World"` |

Several utility combinators appear constantly:

```haskell
-- on (Data.Function): project before comparing
sortBy (compare `on` length) xs       -- sort by length
groupBy ((==) `on` fst) pairs         -- group by first element

-- bool (Data.Bool): point-free conditional
classify = bool "odd" "even" . even    -- bool falseCase trueCase predicate

-- id: identity function, useful as default/no-op
applyIf True f = f
applyIf False _ = id

-- const: ignore second argument
map (const 0) [1,2,3]                 -- [0,0,0]
```

Prefer `(<>)` over `(++)` for polymorphic code — it works on any `Semigroup`, not just lists. Prefer `comparing` from `Data.Ord` over `compare `on``:

```haskell
sortBy (comparing length) xs           -- ascending by length
sortBy (comparing Down . length) xs    -- descending
```

---

## Do-notation: when to use it and when to drop it

**Single-statement do blocks are always redundant** — just remove `do`:

```haskell
-- ❌ greet name = do putStrLn ("Hello " ++ name)
-- ✅ greet name = putStrLn ("Hello " ++ name)
```

**`x <- action; return x`** simplifies to just `action`:

```haskell
-- ❌ getName = do { x <- getLine; return x }
-- ✅ getName = getLine
```

**Independent bindings** should use Applicative style, not do-notation:

```haskell
-- ❌                              -- ✅
do a <- getA                       (,) <$> getA <*> getB
   b <- getB                       -- or: liftA2 (,) getA getB
   return (a, b)
```

**Simple chains** work well with `(>>=)`:

```haskell
getLine >>= putStrLn . ("Hello, " ++)
lookup key m >>= parseValue >>= validate     -- Maybe chaining
```

**Kleisli composition** `(>=>)` creates point-free monadic pipelines:

```haskell
processAndSave = parseInput >=> validate >=> save
-- equivalent to: \x -> parseInput x >>= validate >>= save
```

Reserve `do` for **complex multi-step computations** with many intermediate bindings, control flow, or `let` bindings — where operator chaining would be harder to follow. Inside do-blocks, use `let` for pure bindings and `<-` for monadic actions. Modern Haskell **prefers `pure` over `return`** — it requires only `Applicative`, the weaker constraint.

---

## Pattern matching made terse with GHC extensions

**LambdaCase** (`\case`, included in GHC2021) eliminates throwaway variables:

```haskell
-- ❌ eitherToMaybe e = case e of { Left _ -> Nothing; Right x -> Just x }
-- ✅ eitherToMaybe = \case { Left _ -> Nothing; Right x -> Just x }

-- Especially powerful with >>= :
peekWord8' >>= \case
    0xF1 -> mtcQuarter
    0xF2 -> songPosition
    _    -> empty
```

GHC 9.4+ adds **`\cases`** for multi-argument lambda case. **MultiWayIf** provides guard-style syntax in expression position:

```haskell
classify x y = if | x == 0    -> "zero"
                  | y < 0     -> "negative y"
                  | otherwise -> "other"
```

**Pattern guards** allow failable pattern matches inside guards:

```haskell
addLookup env var1 var2
  | Just val1 <- lookup env var1
  , Just val2 <- lookup env var2
  = Just (val1 + val2)
addLookup _ _ _ = Nothing
```

**View patterns** (`ViewPatterns`, GHC2021) apply a function and match on the result:

```haskell
size (view -> Unit)        = 1
size (view -> Arrow t1 t2) = size t1 + size t2
```

**Use eliminators** (`maybe`, `either`, `bool`) instead of explicit case when the function is a simple transform:

```haskell
-- ❌ case mx of { Nothing -> 0; Just x -> x }
-- ✅ fromMaybe 0 mx

-- ❌ case result of { Left e -> handleErr e; Right v -> process v }
-- ✅ either handleErr process result
```

---

## Functor, Applicative, and Monad: choosing the right level

The fundamental rule: **use the weakest abstraction that works**. If effects are independent, use Applicative. If a later action depends on an earlier result, use Monad.

```haskell
-- Applicative: all fields parsed independently (enables error accumulation)
User <$> parseName <*> parseAge <*> parseEmail

-- Monadic: second action depends on first
do name <- getLine
   if null name then fail "empty" else lookupUser name
```

Key traverse/map/sequence patterns:

```haskell
traverse validate fields              -- Applicative traversal (preferred in general)
mapM processItem items                -- Monadic traversal (fine in IO context)
for_ items $ \item -> do              -- "for loop" style when action is multi-line
  process item; log item
mapM_ print items                     -- short one-liner: mapM_ reads naturally
sequence [getLine, getLine, getLine]  -- [IO String] -> IO [String]
```

**`for_`/`forM_`** (flipped argument order) reads like an imperative loop and is preferred when the action body is large. **`traverse_`/`mapM_`** is preferred when the function is already named.

Other essential combinators:

```haskell
when debug $ putStrLn "debug mode"           -- conditional action
unless (null errors) $ mapM_ report errors   -- negated conditional
void $ forkIO someAction                     -- discard return value
guard (age >= 18) $> ticket                  -- Maybe/list filter
join (Just (Just 3))                         -- Just 3 (flatten nested monad)
getUserName userId <&> Text.toUpper          -- <&> is flipped <$>, pipeline style
```

**`foldM`** is the monadic fold, **`filterM`** the monadic filter, and **`concatMapM f = fmap concat . mapM f`** (not in base but trivially composed).

---

## Lens and optics for nested data manipulation

The `lens` library's three core operations — **view** (`^.`), **set** (`.~`), **over** (`%~`) — compose with ordinary `(.)` and chain with `(&)`:

```haskell
person ^. address . city                     -- nested get
person & address . city .~ "NYC"             -- nested set
person & age %~ (+1)                         -- modify through function
molecule & atoms . traversed . point . x %~ (+1)  -- map over all atoms' x coords
```

Essential optics for collections:

```haskell
m ^. at "alice"                    -- Maybe value at key (can insert/delete)
m & at "key" .~ Just 42           -- insert
m & at "key" .~ Nothing           -- delete
m & ix "key" %~ (+1)              -- modify if present, no-op otherwise
[1..10] ^.. traversed . filtered even        -- [2,4,6,8,10]
(1,2) & both %~ (*10)                        -- (10, 20)
Just 5 & _Just %~ (+1)                       -- Just 6
```

**Stateful operations** (in `MonadState`) use `=` suffixed operators: `.=` for set, `%=` for modify, `+=` for increment. Use `use` to read from state:

```haskell
entities . ix 0 . position . x += 1         -- in a State/StateT context
score <- use (entities . to length)
```

Generate lenses with **`makeLenses`** (prefix fields with `_`), **`makeClassy`** (generates `HasFoo` typeclass for embedding), or **`makeFields`** (per-field typeclasses for overloaded access). Lens shines for **deeply nested updates and traversals**; for single-level record access, plain pattern matching is clearer.

The **optics** library is an alternative with clearer type errors and an opaque `Optic` newtype (uses `%` for composition instead of `.`), but `lens` remains the de facto standard on Hackage.

---

## Records, newtypes, and deriving strategies

**Modern record extensions** eliminate most boilerplate. The recommended combo for GHC 9.2+:

```haskell
{-# LANGUAGE NoFieldSelectors, OverloadedRecordDot, DuplicateRecordFields #-}

data Person = Person { name :: String, age :: Int }
data Company = Company { name :: String, owner :: Person }

main = print $ company.owner.name        -- OOP-style dot access
names = map (.name) people               -- section syntax
```

**RecordWildCards** and **NamedFieldPuns** enable concise construction and destructuring:

```haskell
greet User{..} = "Hello, " <> name                -- RecordWildCards: all fields in scope
greet User{name, age} = name <> " (" <> show age <> ")"  -- NamedFieldPuns: explicit subset
mkUser name age = User{..}                          -- construction from matching bindings
```

**DerivingStrategies** (GHC 8.2+) gives explicit control. Always specify the strategy:

```haskell
newtype Name = Name Text
  deriving stock    (Show, Generic)          -- compiler-generated
  deriving newtype  (Eq, IsString)           -- through the wrapped type (GND)
  deriving anyclass (ToJSON, FromJSON)        -- default methods (typically Generic-based)
```

**DerivingVia** (GHC 8.6+) enables deriving through *any* type with the same runtime representation:

```haskell
newtype Score = Score Int
  deriving (Semigroup, Monoid) via (Sum Int)   -- monoidal addition

data FilmRating = U | PG | R
  deriving stock (Bounded, Eq, Ord)
  deriving (Semigroup, Monoid) via (Max FilmRating)  -- "most restrictive" monoid
```

Common DerivingVia targets include **`Sum`**, **`Product`**, **`Ap`/`App`** (lift Monoid through Applicative), **`Alt`** (Monoid from Alternative), and **`Endo`** (composition monoid). Newtypes are **zero-cost at runtime** and should be used liberally for type safety:

```haskell
newtype UserId    = UserId Int    deriving newtype (Eq, Ord, Show)
newtype ProductId = ProductId Int deriving newtype (Eq, Ord, Show)
-- getUser (ProductId 5) is now a type error, not a silent bug
```

---

## Strings, text, and the five string types

| Type | Backed by | Use for |
|---|---|---|
| `String` (`[Char]`) | Linked list | Show/Read, compatibility only |
| `Data.Text` (strict) | UTF-8 array | **Default for all text** |
| `Data.Text.Lazy` | Chunks | Streaming, builders |
| `Data.ByteString` (strict) | Byte array | Binary data, network I/O |
| `Data.ByteString.Lazy` | Chunks | Large binary, streaming |

**Always enable `OverloadedStrings`** so string literals work polymorphically. Convert between types explicitly:

```haskell
T.pack / T.unpack                   -- String ↔ Text
TE.encodeUtf8 / TE.decodeUtf8'     -- Text ↔ ByteString (UTF-8, safe version)
TL.toStrict / TL.fromStrict        -- Lazy ↔ Strict Text
```

For efficient text construction, use **`Data.ByteString.Builder`** or **`Data.Text.Lazy.Builder`** rather than repeated `(<>)`. Libraries like **PyF** (`[fmt|Hello {name}|]`), **string-interpolate** (`[i|Hello #{name}|]`), and **fmt** (`"Hello "+|name|+""`) provide string interpolation.

---

## Lists, maps, and collection patterns

**Prefer higher-order functions over explicit recursion.** The choice of fold matters:

- **`foldr`**: building data structures, short-circuiting (e.g., `any`, `all`), infinite lists
- **`foldl'`** (strict): reducing to a value (sums, counts). **Never use lazy `foldl`** — it builds thunks and causes space leaks
- **`foldMap`**: when elements map into a Monoid

Essential one-liners:

```haskell
mapMaybe readMaybe ["1","x","3"] :: [Int]    -- [1, 3] — map + filter in one pass
catMaybes [Just 1, Nothing, Just 3]          -- [1, 3]
partitionEithers results                      -- ([errors], [successes])
concatMap expand items                        -- flatMap equivalent
sortOn length xs                              -- Schwartzian transform, O(n log n)
```

**`NonEmpty`** from `Data.List.NonEmpty` guarantees non-emptiness at the type level — `NE.head` is total. Use it wherever an empty collection is invalid.

**Map idioms** center on `fromListWith`, `insertWith`, `unionWith` for combining duplicate keys, and the **two-line import pattern**:

```haskell
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
```

Avoid `nub` on large lists (**O(n²)**) — use `Set.toList . Set.fromList` or `nubOrd` from the `extra` package.

---

## Error handling: the hierarchy from Maybe to exceptions

| Approach | When to use |
|---|---|
| `Maybe` | Simple absence, no error info needed |
| `Either e` | Expected failures with structured error types (parsers, validation) |
| `ExceptT e m` (non-IO base) | Threading typed errors through pure transformer stacks |
| IO exceptions (`throwIO`/`catch`) | Unexpected IO failures, resource cleanup |

**Do not wrap `ExceptT` around IO** — this is a well-known anti-pattern (documented by Michael Snoyman). It creates a third error channel on top of synchronous and asynchronous IO exceptions.

The **`note`** pattern converts Maybe to Either: `note "not found" (lookup key m)`. The **`safe`** library provides total versions of partial functions: `headMay`, `readMay`, `atMay`. Use `bracket` for exception-safe resource management and `safe-exceptions` for production code that properly distinguishes sync vs async exceptions.

```haskell
-- Pure code: return Either
parseConfig :: String -> Either ConfigError Config

-- IO code: throw exceptions, catch at boundaries
loadConfig :: FilePath -> IO Config
loadConfig path = do
  contents <- readFile path
  either (throwIO . ConfigException) pure (parseConfig contents)
```

---

## Type-level idioms that make code more concise

**TypeApplications** (GHC2021) eliminates the need for type annotations and `Proxy`:

```haskell
read @Int "42"               -- instead of (read "42" :: Int)
maxBound @Int                -- instead of maxBound :: Int
show @Int                    -- monomorphic show
```

**ScopedTypeVariables** (GHC2021, requires explicit `forall`) lets type variables scope into `where` clauses:

```haskell
f :: forall a. [a] -> [a]
f xs = ys ++ ys
  where ys :: [a]        -- same 'a' as the top-level signature
        ys = reverse xs
```

**RankNTypes** (GHC2021) enables higher-rank polymorphism — the argument itself must be polymorphic:

```haskell
runST :: (forall s. ST s a) -> a       -- s cannot escape, enforcing purity
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
```

---

## GHC extensions every AI assistant should enable

The most impactful extensions for terse, modern Haskell:

- **`OverloadedStrings`**: string literals work as `Text`, `ByteString`, etc.
- **`LambdaCase`**: `\case` eliminates throwaway pattern match variables
- **`BlockArguments`** (GHC2024): `when condition do ...` without `$`
- **`TupleSections`**: `(,3)` instead of `\x -> (x, 3)`
- **`TypeApplications`**: `read @Int` instead of type annotations
- **`ScopedTypeVariables`**: type variables scope into where clauses
- **`DerivingStrategies`**: explicit `stock`/`newtype`/`anyclass`/`via`
- **`RecordWildCards`**: `MyRecord{..}` for destruction and construction
- **`OverloadedRecordDot`** (GHC 9.2+): `record.field` syntax
- **`NumericUnderscores`**: `1_000_000` for readability
- **`ApplicativeDo`**: desugars independent do-bindings to Applicative

The **GHC2021** language edition bundles most of these. **GHC2024** adds `BlockArguments`.

---

## Import patterns and module organization

Follow the **two-line import pattern** for containers and text:

```haskell
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Text (Text)
import qualified Data.Text as T           -- or 'Text' per Kowainik

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
```

Group imports: (1) unqualified external, (2) unqualified internal, (3) qualified external, (4) qualified internal, separated by blank lines. Use **explicit import lists** for small imports and **qualified imports** when a module is designed for it (containers, text, bytestring). Consider a **custom prelude** like `relude` (no partial functions, `Text` as default string type) or `protolude` for production projects. The **`ImportQualifiedPost`** extension allows `import Data.Map qualified as Map` for cleaner alignment.

---

## Where vs let: scoping auxiliary definitions

**Use `where`** for function-level helpers that support guards and keep main logic first:

```haskell
bmiTell weight height
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal"
  | otherwise   = "Overweight"
  where bmi = weight / height ^ 2
```

**Use `let`** for intermediate pure bindings in do-blocks:

```haskell
main = do
  input <- getLine
  let parsed = read @Int input
  print (parsed * 2)
```

Avoid deeply nesting `where` inside `where` — extract helpers to the top level instead.

---

## The anti-pattern catalogue: verbose patterns to replace

These are the most common mistakes that beginners and AI models produce. Each should be recognized and rewritten:

**Explicit recursion instead of standard combinators:**
```haskell
-- ❌ sumList [] = 0; sumList (x:xs) = x + sumList xs
-- ✅ sumList = foldl' (+) 0
```

**Unnecessary do-notation for pure code:**
```haskell
-- ❌ greet name = do putStrLn ("Hello " ++ name)
-- ✅ greet name = putStrLn ("Hello " ++ name)
```

**Pattern matching on Maybe/Either just to re-wrap:**
```haskell
-- ❌ case mx of { Nothing -> Nothing; Just x -> Just (x+1) }
-- ✅ fmap (+1) mx
```

**`length xs == 0` instead of `null xs`** — `null` is O(1), `length` is O(n).

**`concat . map f` instead of `concatMap f`** — a single, clearer function.

**Nested if-then-else instead of guards:**
```haskell
-- ❌ if n < 0 then "neg" else if n == 0 then "zero" else "pos"
-- ✅ | n < 0 = "neg" | n == 0 = "zero" | otherwise = "pos"
```

**Lazy `foldl` instead of strict `foldl'`** — causes space leaks on large inputs.

**`String` instead of `Text`** in production code — **40+ bytes per character** vs packed UTF-8.

**Using `head`/`tail`/`fromJust`/`read`** — partial functions that crash. Use `listToMaybe`, pattern matching, `readMaybe`, and `fromMaybe`.

**Orphan instances** — break global instance uniqueness. Use newtype wrappers.

**Lazy IO (`readFile`)** — unpredictable resource management. Use strict `Data.Text.IO.readFile` or streaming libraries.

**The existential typeclass anti-pattern** — wrapping values in `forall a. Show a => AnyShow a` when `[String]` suffices.

---

## Conclusion

Idiomatic Haskell achieves concision not through clever tricks but through **choosing the right abstraction level**: `fmap` over pattern matching, Applicative over Monad, `foldl'` over explicit recursion, eliminators (`maybe`, `either`, `bool`) over `case`, and composition over intermediate variable naming. The critical meta-principle is to **use the weakest sufficient tool** — this produces code that is both shorter and more general.

Three practices have the single highest impact on code quality: enabling `OverloadedStrings` and using `Text` by default, compiling with `-Wall -Werror`, and using `DerivingStrategies` to make instance derivation explicit. An AI assistant that consistently applies the patterns in this reference — from `\case` and `<$>`/`<*>` chains to lens pipelines and `mapMaybe` — will produce Haskell that experienced developers recognize as fluent rather than translated from another language.
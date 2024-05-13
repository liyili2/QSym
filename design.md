# Top-level functions

- `interpret :: Expr -> QSym ()`
- `runQSym :: QEnv -> QState -> QSym () -> QState`

# Types

- `Expr`: "Expression" type for VQO language (NOTE: These are really more like commands than expressions)
- `Value`: This is a pair of `RzValue`s which is either a `QVal` or an `NVal`
- `RzValue`: This is a bit-vector of arbitrary length
- `QSym`: Reader + state monad. This has a read-only `QEnv` and a read-write `QState`
- `QState`: This maps `Var`s to `Value`s (?)
- `QEnv`: This maps `Var`s to `Value`s

# General description

`interpret` will receive an `Expr`. It will then update the `QState` based on the instruction that this `Expr` represents. `interpret`
also has access to a read-only `QEnv` which it can use to do this.

# Utility functions

## Operations on `RzValue`s

- Standard numeric functions such as `+` work on `RzValue`s
- `rzValue :: Natural -> QSym RzValue`: Converts a `Natural` to an `RzValue`. Note that this is in the `QSym` monad: we need access to the `QEnv` to know how long the bit vector should be (including trailing zeroes)
- `rotate :: Int -> RzValue -> RzValue`: We rotate `v` by `n` bits when we have `rotate n v`
- `v ! i` gets the `i`th bit of the `RzValue` called `v`
- `rzSetBit :: RzValue -> Int -> Bool -> RzValue`
- `nOnes :: Int -> QSym RzValue`. This produces an `RzValue` consisting of `n` ones: `nOnes n`
- `rzNatural :: RzValue -> Natural`

## Operations related to `QEnv` and `QState`

### Monadic `QSym` actions

- `update :: Var -> Value -> QSym ()`: updates the `QState` that is "tracked" by `QSym`
- `getState :: Var -> QSym Value`: gets the `Value` associated to the given `Var` (from the `QState`)
- `getEnv :: Var -> QSym Value`: gets the `Value` associated to the given `Var` (from the `QEnv`)

### Building `QEnv`s and `QState`s

- `emptyEnv :: QEnv`
- `mkState :: QEnv -> [(Var, Value)] -> QState`: Makes a `QState` from the list of pairs (values assigned to variables) and a given `QEnv`

# Selected details

```haskell
data Value
  = QVal RzValue RzValue
  | NVal RzValue RzValue
```


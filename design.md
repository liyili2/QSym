# Top-level functions

- `interpret :: Expr -> QSym ()`
- `runQSym :: QEnv -> QState -> QSym () -> QState`

# Types

- `Expr`: "Expression" type for VQO language (NOTE: These are really more like commands than expressions)
- `Value`: This is a pair of `RzValue`s which is either a `QVal` or an `NVal`
- `RzValue`: This is a bit-vector of arbitrary length
- `QSym`: Reader + state monad. This has a read-only `QEnv` and a read-write `QState`
- `QState`: This maps `Var`s to `Natural`s (?)
- `QEnv`: This maps `Var`s to `Value`s

# General description

`interpret` will receive an `Expr`. It will then update the `QState` based on the instruction that this `Expr` represents. `interpret`
also has access to a read-only `QEnv` which it can use to do this.


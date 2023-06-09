# envelopes

[![GitHub CI](https://github.com/pmihaly/envelopes/workflows/CI/badge.svg)](https://github.com/pmihaly/envelopes/actions)
[![Hackage](https://img.shields.io/hackage/v/envelopes.svg?logo=haskell)](https://hackage.haskell.org/package/envelopes)
[![Stackage Lts](http://stackage.org/package/envelopes/badge/lts)](http://stackage.org/lts/package/envelopes)
[![Stackage Nightly](http://stackage.org/package/envelopes/badge/nightly)](http://stackage.org/nightly/package/envelopes)
[![AGPL-3.0-only license](https://img.shields.io/badge/license-AGPL--3.0--only-blue.svg)](LICENSE)

See README for more info


## Usage

```shell
envelopes <finances.yaml | sponge finances.yaml
```


value objects:
- [x] Text50
- [x] Positive
- [x] Id
- [x] ShorthandNumber
- [x] Money ignoring currency
- [x] Money storing currency as string

state:
- [x] Envelope (Semigroup 'x and y', (+), Monoid '' Money.mempty)

event schema & apply events:
- [x] Transaction
    - [x] Spending
    - [x] Refill
- [x] State (envelopes :: HashMap (Id Envelope) Envelope, appliedTransactions :: HashSet (Id Envelope))
- [x] `applyTransactions :: Foldable f => State -> f Transaction -> Either TransactionError State`
- [x] delete envelopes if it contains ~0 money

- [x] read & write input file

nice to have:
- [x] Revolut export month -> events CLI
- [ ] Transaction date is an actual date
- [ ] Spending Tags is a `NonEmpty [Text50]`
- [ ] CLI --from-json --to-json
- [ ] exchange currencies https://stackoverflow.com/questions/27408873/how-to-model-a-currencies-money-and-banks-that-exchange-money-between-currenci

# envelopes

[![GitHub CI](https://github.com/pmihaly/envelopes/workflows/CI/badge.svg)](https://github.com/pmihaly/envelopes/actions)
[![Hackage](https://img.shields.io/hackage/v/envelopes.svg?logo=haskell)](https://hackage.haskell.org/package/envelopes)
[![Stackage Lts](http://stackage.org/package/envelopes/badge/lts)](http://stackage.org/lts/package/envelopes)
[![Stackage Nightly](http://stackage.org/package/envelopes/badge/nightly)](http://stackage.org/nightly/package/envelopes)
[![AGPL-3.0-only license](https://img.shields.io/badge/license-AGPL--3.0--only-blue.svg)](LICENSE)

See README for more info

value objects:
- [x] Text50
- [x] Positive
- [x] Id
- [ ] Money (Semigroup sum (ignore currency), Monoid 0)

state:
- [ ] Envelope (Semigroup 'x and y', (+), Monoid '' Money.mempty)

event schema & apply events:
- [ ] Spending
- [ ] Refill

- [ ] read & write input file

nice to have:
- [ ] CLI --from-json --to-json
- [ ] currencies

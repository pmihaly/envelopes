#/bin/sh
#
# revolut.sh - append spendings from revolut CSV to transactions
# Syntax: revolut.sh finances.yaml <account-statement.csv

yq -p csv '
filter(.Amount < 0) |
filter(.Description | test("^To [A-Z]{3}$") | not) |
filter(.Description | test("^Top-Up by \*\d{4}$") | not) |
map({
  "id": "revo-" + ("" + .["Started Date"] | @base64),
  "type": "spending",
  "envelope": "???",
  "amount": (.Amount * -1) + " " + (.Currency | downcase),
  "date": .["Completed Date"],
  "tags": [ .Description ]
})
' | yq '.transactions += load("/dev/stdin")' $1

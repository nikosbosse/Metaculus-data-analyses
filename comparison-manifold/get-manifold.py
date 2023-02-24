from manifoldpy import api
import json
from os import path
import requests
import pandas as pd

refetch_from_manifold = True

# Get all basic info on all markets from Manifold if we don't have them already
if refetch_from_manifold:
    markets = api._get_all_markets()
    with open(
        "comparison-manifold/manifold-data/all-manifold-markets.json",
        "w",
        encoding="utf-8",
    ) as f:
        json.dump(markets, f, ensure_ascii=False, indent=4)


## --------------------------------------------------------- ##
## get full information for all resolved binary markets created by MetaculusBot

# Load basic info on all markets from Manifold previously stored
with open(
    "comparison-manifold/manifold-data/all-manifold-markets.json", "r", encoding="utf-8"
) as f:
    markets = json.load(f)

# filtering:
# Discard all markets that weren't created by MetaculusBot
user_id = "jn8ZKjXgwgfbwJqHRNbemA3Epw52"  # MetaculusBot user id
markets = [market for market in markets if market["creatorId"] == user_id]
# also discard all markets that are not binary
markets = [market for market in markets if market["outcomeType"] == "BINARY"]
# only keep if market is resolved and resolution is not CANCEL
markets = [
    market
    for market in markets
    if market["isResolved"] is True  # & market["outcomeType"] == "BINARY"
]
markets = [market for market in markets if market["resolution"] != "CANCEL"]

# get full information for all resolved binary markets
market_ids = [market["id"] for market in markets]
full_markets = []
for id in market_ids:
    url = "https://manifold.markets/api/v0/market/" + id
    market_info = requests.get(url, timeout=20).json()
    full_markets.append(market_info)

## --------------------------------------------------------- ##
## store full information on binary metaculus markets as csv file
df = pd.DataFrame(full_markets)
df = df[
    [
        "id",
        "createdTime",
        "closeTime",
        "probability",
        "totalLiquidity",
        # question,
        "resolution",
        "resolutionTime",
        "resolutionProbability",
        "lastUpdatedTime",
        "description",
    ]
]
df.to_csv("comparison-manifold/manifold-data/metaculus-binary-markets.csv", index=False)

## --------------------------------------------------------- ##
# get all bets for all binary markets and save as csv
binary_ids = [market["id"] for market in markets]
bets = []
for id in binary_ids:
    url = "https://manifold.markets/api/v0/bets?contractId=" + id
    market_bets = requests.get(url, timeout=20).json()
    bets.extend(market_bets)

bets = pd.DataFrame(bets)
bets = bets[
    [
        "userId",
        "probAfter",
        "probBefore",
        "contractId",
        "createdTime",
    ]
]
bets.to_csv("comparison-manifold/manifold-data/metaculus-binary-bets.csv", index=False)

# Problem
When I restore my wallet, the application freezes.
## Locale
en
## Solution
This is not a bug, it is actually trying fetch whole transaction history. The reason why it takes such a long time is because it is traversing through the whole blockchain to get the wallet synced.
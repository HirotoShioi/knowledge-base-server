# Question
What is "transaction assurance level"? What is the difference between "normal" and "strict"?
#### Locale
en
## Answer
Transaction assurance level is a guarantee that a transaction will not be cancelled by some fork in case of the attack by an adversary. There is a complex formula that depends on transaction depth in the blockchain (how many blocks before the most recent one were included into the block, the number of confirmations) and the amount of adversarial stake. 
Things are much simpler during the first 3 months of operation of mainnet since all the stake that participate in the process of staking (block generation in proof of stake system) is controlled by IOHK. Same goes for the testnet. 
""Normal"" and ""strict"" are different user settings as shown below.

Normal setting:
* Low assurance: < 3 confirmations
* Medium assurance: 3 to 6 confirmations
* High assurance: 7+ confirmations

Strict setting:
* Low assurance: < 5 confirmations
* Medium assurance: 5 to 10 confirmations
* High assurance: 11+ confirmations
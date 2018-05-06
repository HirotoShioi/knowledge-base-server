# Question
Inability to send Ada to addresses that are in rare cases 101 characters
#### Locale
en
## Answer
There is a bug in the current wallet code where, for certain rare addresses, it computes the transaction fee incorrectly, preventing the transaction from being sent.

The error reported is: ""Not enough Ada for fees. Try sending a smaller amount.""

These addresses are distinguished by being 3 characters shorter than normal addresses, i.e. 101 rather than 104. 
The workaround is to request a new address from the receiver if the address you are sending Ada to is 101 characters long.

The workaround for withdrawals from the exchanges is to generate a new address in Daedalus and use it for the withdrawal instead of the shorter address.
#!/usr/bin/expect -f

spawn ./scripts/clone.sh

expect "Username for 'https://git.ccfe.ac.uk':"

send -- "gitlab+deploy-token-17\r"

expect "Password for 'https://gitlab+deploy-token-17@git.ccfe.ac.uk':"

send -- "kcVbZnz9AHLZytz5gTRG\r"

expect eof
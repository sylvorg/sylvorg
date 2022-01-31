(import sys [argv])
(import toolz [drop])
(print (drop argv))
(print (cut argv 1 (len argv)))

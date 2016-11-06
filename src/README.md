#Examples

Name | Description | :arrow_forward: Start address | How to use | Notes
---- | ----------- | ----------------------------- | ---------- | -----
hello-c64 | Prints "COMMODORE 64" 5 times on the screen | $02a8 = 680 | ```LOAD"HELLO-C64",8,1```<br>```SYS680``` | How to use a loop
input | Gets a string input from the user and prints it | $02a8 = 680 | ```LOAD"INPUT",8,1```<br>```SYS680``` | 
variables | Creates BASIC integer, floating point and string variables and prints their values | $c000 = 49152 | ```LOAD"VARIABLES",8,1```<br>```NEW```<br>```SYS49152``` | Need to use ```NEW``` after loading as the ```LOAD``` command messes up the BASIC memory pointers.
faddh | Prints the numbers from 1 to 10 by 0.5 | $033e = 830 | ```LOAD"FADDH",8,1```<br>```SYS830``` | :new:

#Utilities
Name | Description | :arrow_forward: Start address | How to use | Notes
---- | ----------- | ----------------------------- | ---------- | -----
str2fp | Converts a string containing a floating point number to its 5 byte FAC exponent-mantissa representation | $0810 = 2064 | ```LOAD"STR2FP",8```<br>```RUN``` | This one has a short BASIC program in front beginning at $0801 (```10 SYS 2064```) so ```,8``` and ```RUN``` can be used.

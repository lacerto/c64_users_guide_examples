# Utilities

Small utilities and experiments with code.

Name | Description | :arrow_forward: Start address | How to use | Notes
---- | ----------- | ----------------------------- | ---------- | -----
decstr | Reads a byte parameter, converts it to string and prints the result. A second conversion routine stores the result as a null-terminated right-aligned string in memory. | $0334 = 820 | ```LOAD"DECSTR",8,1```<br>```SYS820,X``` | X = [0;255] The original conversion routine can be found here: https://www.c64-wiki.de/wiki/Assembler_Beispiel_Division
decstr2 | Reads a byte parameter, converts it to string and prints the result. | $0334 = 820 | ```LOAD"DECSTR2",8,1```<br>```SYS820,X``` | X = [0;255] This one is the modified version of https://codebase64.org/doku.php?id=base:tiny_.a_to_ascii_routine
str2fp | Converts a string containing a floating point number to its 5 byte FAC exponent-mantissa representation | $0810 = 2064 | ```LOAD"STR2FP",8```<br>```RUN``` | This one has a short BASIC program in front beginning at $0801 (```10 SYS 2064```) so ```,8``` and ```RUN``` can be used.
strinput | Displays a prompt and waits for user input. The input is filtered using a list of allowed characters. The maximum length of the input is limited. | $C000 = 49152 | ```LOAD"STRINPUT",8,1```<br>```SYS49152``` | This is a modified version of the [Robust String Input](https://codebase64.org/doku.php?id=base:robust_string_input).

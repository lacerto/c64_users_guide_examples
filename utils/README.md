# Utilities

Small utilities and experiments with code.

Name | Description | :arrow_forward: Start address | How to use | Notes
------- | ----------------------------- | -------------- | ---------- | -----------------------
chargen | Switch to VIC bank #3, copy character generator ROM to RAM and print the first 128 characters. | $c000 = 49152 | `LOAD"CHARGEN",8,1`<br>`SYS49152` |
decstr | Reads a byte parameter, converts it to string and prints the result. A second conversion routine stores the result as a null-terminated right-aligned string in memory. | $0334 = 820 | `LOAD"DECSTR",8,1`<br>`SYS820,X` | X = [0;255] The original conversion routine can be found here: https://www.c64-wiki.de/wiki/Assembler_Beispiel_Division
decstr2 | Reads a byte parameter, converts it to string and prints the result. | $0334 = 820 | `LOAD"DECSTR2",8,1`<br>`SYS820,X` | X = [0;255] This one is the modified version of https://codebase64.org/doku.php?id=base:tiny_.a_to_ascii_routine
extcolormode | Switch to extended background color text mode and print the available 64 characters using all 4 background colors. | $033e = 830 | `LOAD"EXTCOLORMODE",8,1`<br>`SYS830` | 
mcspritetest | Multi-color sprite test. | $0825 = 2085 | `LOAD"MCSPRITETEST",8`<br>`RUN` | Displays a multi-color sprite and its expanded version.
pixie | Very basic sprite editor. | $080d = 2061 | `LOAD"PIXIE",8`<br>`RUN` | Saves the sprite data in a *SEQ* file which can be readily included in an assembly program as it uses the *.byte* data pseudo-op notation of tmpx.
showchar | Print a character from chargen as an 8x8 matrix and exit. | $c000 = 49152 | `LOAD"SHOWCHAR",8,1`<br>`SYS49152,X` | X = [0;255] character index. This utility cannot show the characters with indices 256-511.
spritetest | Displays a sprite. | $0825 = 2085 | `LOAD"SPRITETEST",8`<br>`RUN` | Paste the 63 bytes of your sprite at the end of the source code to see it on the screen.
str2fp | Converts a string containing a floating point number to its 5 byte FAC exponent-mantissa representation | $0810 = 2064 | `LOAD"STR2FP",8`<br>`RUN` | This one has a short BASIC program in front beginning at $0801 (`10 SYS 2064`) so `,8` and `RUN` can be used.
strinput | Displays a prompt and waits for user input. The input is filtered using a list of allowed characters. The maximum length of the input is limited. | $C000 = 49152 | `LOAD"STRINPUT",8,1`<br>`SYS49152` | This is a modified version of the [Robust String Input](https://codebase64.org/doku.php?id=base:robust_string_input).
viewchars | Displays all of the characters in CHARGEN as a 8x8 matrix of differently colored spaces. | $0820 = 2080 | `LOAD"VIEWCHARS",8`<br>`RUN` | Next character: D, W, up or right; previous character: A, S, down or left. Exit by pressing return or space.

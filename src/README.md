# Examples

Name | Description | :arrow_forward: Start address | How to use | Notes
---- | ----------- | ----------------------------- | ---------- | -----
hello-c64 | Prints "COMMODORE 64" 5 times on the screen | $02a8 = 680 | ```LOAD"HELLO-C64",8,1```<br>```SYS680``` | How to use a loop
input | Gets a string input from the user and prints it | $02a8 = 680 | ```LOAD"INPUT",8,1```<br>```SYS680``` | 
variables | Creates BASIC integer, floating point and string variables and prints their values | $c000 = 49152 | ```LOAD"VARIABLES",8,1```<br>```NEW```<br>```SYS49152``` | Need to use ```NEW``` after loading as the ```LOAD``` command messes up the BASIC memory pointers.
faddh | Prints the numbers from 1 to 10 by 0.5 | $0334 = 820 | ```LOAD"FADDH",8,1```<br>```SYS820``` | Load, save, increment and compare floating point numbers in FAC1
color-bars | Prints randomly colored bars | $02a8 = 680 | ```LOAD"COLOR-BARS",8,1```<br>```SYS680``` | How to use RND from assembly
bouncing-ball | Shows a moving ball that bounces back if it hits the screen border | $0334 = 820 | ```LOAD"BOUNCING-BALL",8,1```<br>```SYS820``` | Get screen size, change border and background colors, write screen codes directly to screen memory
bouncing-ball2 | Shows a moving ball that bounces back if it hits the screen border or some randomly placed obstacles | $080d = 2061 | ```LOAD"BOUNCING-BALL2",8```<br>```RUN``` | Has a BASIC stub in front of it and is placed in BASIC memory as it outgrew the cassette buffer
balloon | Sets sprite #2 to a balloon shape and moves it diagonally on the screen in an endless loop. | $0825 = 2085 | ```LOAD"BALLOON",8```<br>```RUN``` | Use RUN/STOP+RESTORE to stop the animation.

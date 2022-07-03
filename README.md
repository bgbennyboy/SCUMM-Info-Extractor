# SCUMM-Info-Extractor
Extracts version information from SCUMM interpreters and scripts.

It is possible to get interpreter information in most DOS SCUMM games by running the executable with an invalid command line parameter like /? You can also get different version information ingame by pressing ctrl+v. This program automatically extracts both types of information. This means that it extracts interpreter information from the DOS versions and version information from the scripts on all platforms.

### Why does it take so long when running the interpreters?
It runs each interpreter in dosbox and pipes its output back to the program. It runs a number of Dosbox instances in parallel to speed it up but much of the overhead is in starting and stopping the dosbox processes. It would be much quicker to have one Dosbox instance but this isnt possible because some executables crash or hang, the program monitors the Dosbox processes and terminates any that have hung. It takes a brute force approach and tries to run every .exe it finds in Dosbox so some will inevitably hang and have to be terminated.

### How does it get the version information from the scripts?
It searches for every resource file in the given directories. It then gets the  file that contains the version+date information, for SCUMM v7 and up thats .LA0 files, for V5 and up thats .001 files and for the rest it could be in any DISK.LEC or .LFL file so it tries them all. It searches the file for the hex values that denote the start of strings in scripts and uses regex to find the particular version+date strings that we want. If that fails then it tries again with the different XOR values used in SCUMM games (0xFF and 0x69). Apart from some rogue characters on the end of the extracted strings it works pretty well.

### What about Maniac Mansion and Zak McKracken?
They dont have a version string in their interpreter, or at least not one that gets output to the command line.

# Change the creation date of a file by an offset (macOS)

## How to use

With this little bash command you can change the creation date of multiple files. Only tested on macOS.

`./change-creation-datetime 3600 file.txt`

The first parameter is the offset that should be applied in seconds. In the top case this would be add 1h. The reduce just use negative numbers (like -1800 for subtract 30min). The second parameter is one or multiple files that the offset should be applied to.

`./change-creation-datetime -1800 *.txt` remove 30min from all txt files

## Develop

`ghci change-creation-datetime.hs` to load the program

`> :main 3600 file.txt` to run the main function

`> :q` to quit
`> :l` to load a program
`> :r` to reload a program
`> :i` information about a operator

## Compile

`ghc change-creation-datetime.hs`

## Usage
```
Usage: asca [-v | --version] [-h | --help] <command>

Commands:
    run     Run basic cli
    seq     Run an asca config
    conv    Convert between an asca-web json file and the wsca/rsca format
    tui     (Coming Soon)
```
### Run command
```
usage: asca run ([-j | --from-json <path>] | [-r | --rules <path>]) [-w | --words <path>] 
                 [-c | --compare <path>]     [-o | --output <path>] [-h | --help]
Options:
    -j  <path>  Path to an asca-web json file.
                - Mutually exclusive with -r.
                - If -w is supplied, those words will be used instead of those defined in the json.
    -r  <path>  Path to a rsca file containing the rules to be applied.
                - Mutually exclusive with -j.
                - If neither -j nor -r is provided, asca will look for a file in the current directory.
    -w  <path>  Path to a wsca file containing the input words
                - If this and -j are not provided, asca will look for a file in the current directory
    -c  <path>  Path of a wsca file to compare with the result
    -o  <path>  Desired path of the output file
                - If a directory is provided, asca will create an out.wsca file in that directory
                - If not provided, asca will not write to any file
    -h          Print help
```
### Seq command
```
usage: asca seq [PATH] [-t | --tag <tag>] [-w | --words <path>] [-o | --output]
                       [-y | --overwrite] [-n | --no-overwrite] [-l | --last-only]
                       [-h | --help]
Arguments
    [PATH]  Path to a directory containing an asca config file.
Options:
    -t  <tag>   Run a given tag in the config file.
                - If not provided, all tags in the config will be run
    -w  <path>  Path to a wsca file.
                - If provided, these will be used instead of the word files defined in the config.
    -a          Print all intermediate steps
    -o          When given, asca will create an out folder within the path directory.
    -y          Accept cases where an output file would be overwritten.
    -n          Reject cases where an output file would be overwritten.
    -l          Only the final iteration will be saved (i.e. no intermediate steps).
    -h          Print help
```
### Conv command
```
usage: asca conv [-h | --help] <command>

Commands:
    asca    Convert a word file and rule file into an asca-web json file
    json    Convert a json file into separate word and rule files


usage: asca conv asca [-w | --words <path>] [-r | --rules <path>] [-o | --output <path>]
                      [-h | --help]
Options:
    -w  <path>  The path of the word file to convert
                - If not provided, asca will look for a file in the current directory
    -r  <path>  The path of the rule file to convert
                - If not provided, asca will look for a file in the current directory
    -o  <path>  The desired path of the output json file
                - If not provided, asca will create a file in the current directory
    -h          Print help


usage: asca conv json [-p | --path <path>] [-w | --words <path>] [-r | --rules <path>]
                      [-h | --help]
Options:
    -p  <path>  The path of the Json file to convert
                - If not provided, asca will look for a file in the current directory
    -w  <path>  The desired path of the output word file
                - If not provided, asca will create a file in the current directory
    -r  <path>  The desired path of the output rule file
                - If not provided, asca will create a file in the current directory
    -h          Print help
```

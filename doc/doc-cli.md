# ASCA Command Line Documentation

### Quick Links
* [File Formats](#file-formats)
* [Sequences](#sequences)
* [Usage](#usage)
* [Shell Completions](#shell-completions)

## File Formats
Asca-cli differs from the web implementation by using three file formats to each define a set of input words, a set of rules, and a set of romanisations.
An existing web json file can be converted into these formats (and vice-versa) using the [conv command](#conv-command).

### Word file (.wsca)
Words are defined similarly as to the input field on [asca-web](doc.md). That is, each word is declared on a new line in ipa form.
Unlike the current web format, wsca files allow for comments marked by `#`.

See [pie-uvular](../examples/indo-european/pie-uvular-common.wsca) for an example.

### Rule file (.rsca)

Each rule group is defined as follows:

- A rule title, preceded by `@`.
- A list of sub rules.
    - Can be indented for clarity
    - Empty lines are allowed
- A rule description, preceded by `#`
    - Can be multiple lines, with each line starting with `#`.

Example from a [germanic](../examples/indo-european/germanic/pgmc/early.rsca) implementation.
``` diff
@ Grimms Law 
    [+cons, -son, -cont, -voice] > [+cont]
    [+cons, -son, -cont, +voice, -sg] > [-voice]
    [+cons, +voice, +sg] > [-sg]
    
    [+cons, -son, +voice] > [+cont] / {V:[-red], G}_{V, G}
# Chain shift of the three series of plosives.
# Voiceless plosives become fricatives
# Voiced plosives are devoiced
# Aspirated plosives become voiced plosives or fricatives.

@ Verners Law
    [-voice, +cont] > [+voice] / V:[-stress]([+son])_
# Voiceless fricatives are voiced when preceded by an unaccented vowel.
# Including cases where the vowel and fricative are separated by a sonorant.
```

### Romanisation file (.alias)
An alias file has two sections, `into` and `from`. These describe deromanisation and romanisation, respectively.

Each section is introduced with a tag preceded by '@', then followed by a new line delimited list of alias rules (similiar to a rule file).

Example:
```
@into
    汉 > xan:[tone: 51] 
    语 >   y:[tone:214]
    
@from
    xan:[tone: 51] => 汉
      y:[tone:214] => 语
    $ > *
```

### Config file (.asca)
See the [sequences section](#the-config-file) for more info.

## Sequences
The seq command allows you to run language family projects, defined within a `.asca` file. 
This lets you create a pipeline from parent to daughter languages, generating an output lexicon at each stage.

For an example project, see the indo-european example folder [here](../examples/indo-european/germanic).

### The Config File

Each transformation is called a sequence.

A sequence tag is defined with `@`. 
A single specified tag can be run with `seq -t <tagname>`. 
On output, the results will be stored in the directory `./out/<tagname>/*`.

After a tag, paths to multiple wsca files can be specified between square brackets. 
These word files will be appended together and used as input. 
The path is relative to the directory containing the config file, an extension is not necessary.
The word file list is optional, but they then must be passed by the user with `seq -w <path>`.

This is followed by a colon, after which follows a comma-delimited list of paths to rsca files. 
Again, the path is relative to the config file and the extension does not need to be specified.
``` diff
# This is a comment
# The following sequence has been one-lined, but they can be span multiple lines as well

@alpha ["foo", "bar"]: "rules1", "rules2"

# This means "With the name 'alpha',
# Apply the sound changes within ./rules1.rsca and then ./rules2.rsca
# on the words within ./foo.wsca and ./bar.wsca"
```

#### Filters
A filter can be placed after each rule file to select rules within them. 
Filters are case insensitive, but they must elsewise be identical in name to a rule defined within the file. 
Multiple filters can be specified, separated by commas, and between curly brackets. 

`!` means to exclude the following rules from the sequence. 

`~` means to select only the following rules from the sequence (NOTE: They will be applied in the order they appear in the filter, not as they are within the rule file). 
This is useful for defining commonly used sound changes within a 'global' sound change file and selecting them when needed. 

```diff
@beta ["foo", "bar"]:
    "rules1" ! {"Glottal Deletion"},
    "rules2" ~ {"Cluster Simplification", "Hap(lo)logy"},

# The above sequence tagged "beta" means:
# - Using the words defined in ./foo.wsca and ./bar.wsca,
# - Apply ./rules1.wsca WITHOUT "Glottal Deletion",
# - Apply ONLY "Cluster Simplification" and "Hap(lo)logy", in that order, from ./rules2.wsca.
```

#### Pipelines

Instead of a word list, another defined sequence can be referenced with `%`. This will run the referenced sequence and use the resulting words as input. 
This is useful for defining daughter languages or branches.

``` diff
@latin ["example-lex"]:
    "foo", "bar"

@old-spanish %latin:
    "foo", "bar", "baz",

@spanish %old-spanish:
    "baz", "bar", "foo"
```

A word list can be optionally added after a pipeline tag, with its contents being appended to tag's input. 
This allows for you to add forms at an intermediate sequence (i.e. loanwords or neologisms).

```
@gamma %alpha ["baz"]:
    "rules1" ~ {"Low Vowel Reduction"},
```

#### Using Alias Files

A romanisation file can be added to a sequence similarly to a pipeline by using `$`.

```
@delta %gamma $romanisation:
    "rules3"
```

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
                 [-c | --compare <path>]     [-o | --output <path>] [-l | --alias <path>]
                 [-h | --help]
Options:
    -j  <path>  Path to an asca-web json file.
                - Mutually exclusive with -r.
                - If -w is supplied, those words will be used instead of those defined in the json.
    -r  <path>  Path to a rsca file containing the rules to be applied.
                - Mutually exclusive with -j.
                - If neither -j nor -r is provided, asca will look for a file in the current directory.
    -w  <path>  Path to a wsca file containing the input words
                - If this and -j are not provided, asca will look for a file in the current directory
    -l  <path>  Path to an alias file containing romanisations to and from.
    -c  <path>  Path of a wsca file to compare with the result
    -o  <path>  Desired path of the output file
                - If a directory is provided, asca will create an out.wsca file in that directory
                - If not provided, asca will not write to any file
    -h          Print help
```
### Seq command
```
usage: asca seq [PATH] [-t | --tag <tag>]  [-w | --words <path>] [-a | --all-steps] 
                       [-y | --overwrite]  [-n | --no-overwrite] [-o | --output]   
                       [-i | --output-all] [-h | --help]
Arguments:
    [PATH]  Path to a directory containing an asca config file.
Options:
    -t  <tag>   Run a given tag in the config file.
                - If not provided, all tags in the config will be run.
    -w  <path>  Path to a wsca file.
                - If provided, these will be used instead of the word files defined in the config.
    -a          Print all intermediate steps.
    -o          When given, asca will create an out folder within the path directory.
    -y          Accept cases where an output file would be overwritten.
    -n          Reject cases where an output file would be overwritten.
    -i          Save all intermediate steps.
    -h          Print help
```
### Conv command
```
usage: asca conv [-h | --help] <command>

Commands:
    asca    Convert a word file and rule file into an asca-web json file
    json    Convert a json file into separate word and rule files
    tag     Convert a tag within a config file into an asca-web json file


usage: asca conv asca [-w | --words <path>] [-r | --rules <path>] 
                      [-a | --alias <path>] [-o | --output <path>]
                      [-h | --help]
Options:
    -w  <path>  Path to the word file to convert
                - If not provided, asca will look for a file in the current directory
    -r  <path>  Path to the rule file to convert
                - If not provided, asca will look for a file in the current directory
    -a  <path>  Path to an optional alias file to convert.
    -o  <path>  The desired path of the output json file
                - If not provided, asca will create a file in the current directory
    -h          Print help


usage: asca conv json [-p | --path <path>]  [-r | --rules <path>]
                      [-w | --words <path>] [-a | --alias <path>]
                      [-h | --help]
Options:
    -p  <path>  Path to the Json file to convert
                - If not provided, asca will look for a file in the current directory
    -w  <path>  The desired path of the output word file
                - If not provided, asca will create a file in the current directory
    -a  <path>  The desired path of the output alias file, if applicable.
                - If not provided, asca will create a file in the current directory.
    -r  <path>  The desired path of the output rule file
                - If not provided, asca will create a file in the current directory
    -h          Print help


Usage: asca conv tag [TAG]  [-p | --path <path>] [-o | --output <path>] [-r | --recurse]
                            [-h | --help]
Arguments:
    [TAG]  The tag within the config file to be converted
Options:
    -p  <path>  Path to the config file or the directory it is within
                - If not provided, asca will look for a config in the current directory.
    -o  <path>  The desired path of the output rule file.
                - If not provided, asca will create a file in the current directory.
    -r          Follow a pipeline back to its root tag and generate a full rule history
                - Additional words added after the start of the pipeline will not be included
    -h          Print help
```

## Shell Completions

ASCA provides completion script generation for Bash, Zsh, Fish, Powershell, & Elvish.

### Bash
``` bash
mkdir -p ~/.local/share/bash-completion/completions
asca --generate=bash > ~/.local/share/bash-completion/completions/asca
``` 
macOS with Homebrew:
```bash
mkdir -p $(brew --prefix)/etc/bash_completion.d
asca --generate=bash > $(brew --prefix)/etc/bash_completion.d/asca.bash-completion
```

### Zsh
```zsh
mkdir ~/.zfunc
asca --generate=zsh > ~/.zfunc/_asca
```
Add `fpath+=~/.zfunc` to your `~/.zshrc` before `compinit`
### Fish
```fish
asca --generate=fish > ~/.config/fish/completions/asca.fish
```

### PowerShell
```powershell
asca --generate=powershell >> $PROFILE.CurrentUserCurrentHost
```

### Elvish
```Elvish
asca --generate=elvish >> ~/.config/elvish/lib/completers.elv
```
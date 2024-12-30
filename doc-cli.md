# ASCA Command Line Documentation

### Quick Links
* [File Formats](#file-formats)
* [Converting between file formats](#converting-between-file-formats)
* [Sequences](#sequences)
* [Usage](#usage)

## File Formats
Asca-cli differs from the web implementation by using two file formats to each define a set of input words and a set of rules.
An existing web json file can be converted into these formats (and vice-versa) using the [conv command](#converting-between-file-formats).

### Word file (.wsca)
Words are defined similarly as to the input field on [asca-web](doc.md). That is, each word is declared on a new line in ipa form.
Unlike the current web format however, wsca files allow for comments delimited by `#`.

See [pie-uvular](./examples/indo-european/pie-uvular.wsca) for an example.

### Rule file (.rsca)

Each rule group is defined as follows:

- A rule title, preceded by `@`.
- A list of sub rules.
    - Can be indented for clarity
- A rule description, preceded by `#`
    - Can be multiple lines, with each line starting with `#`.

Example from a [germanic](./examples/indo-european/germanic/early-pgmc.rsca) implementation.
```
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

### Config file (.asca)
See the [sequences section](#the-config-file) for more info.

## Converting between file formats


## Sequences
The seq command allows for whole language families to be defined as a project within a `.asca` file and ran at once. 
This lets you create a pipeline from parent to daughter languages, generating an output lexicon at each stage.

For an example project, see the indo-european example folder [here](./examples/indo-european/).

### The Config File

Each transformation is called a sequence.

A sequence tag is defined with `@`. 
This tag can be called with `-t` to run only that specified sequence. 
On output, the results will be stored in the directory `./out/<tag>/*`.

After a tag, paths to multiple wsca files can be specified between square brackets. 
These word files will be joined and used as input. 
The path is relative to the directory containing the config file, an extension is not necessary. 
The word list is optional, but they then must be passed by the user at runtime with `-w`.

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
Filters are case insensitive, but they must elsewise be identical in name. 
Multiple filters can be specified, separated by commas, and between curly brackets. 

`!` means to exclude the following rules from the sequence. 

`~` means to select only the following rules from the sequence (NOTE: Will be applied in the order they appear in the filter, not as they are in the rule file). 
This could be useful for defining commonly used sound changes within a 'global' sound change file and selecting them when needed. 

```diff
@beta ["foo", "bar"]:
    "rules1" ! {"Glottal Deletion"},
    "rules2" ~ {"Cluster Simplification", "Hap(lo)logy"},

# The above sequence tagged "beta" means:
# - Using the words at ./foo.wsca and ./bar.wsca,
# - Apply ./rules1.wsca WITHOUT "Glottal Deletion",
# - Apply ONLY "Cluster Simplification" and "Hap(lo)logy", in that order, from ./rules2.wsca.
```

#### Pipelines

Instead of a word list, another defined sequence can be referenced with `%`. This will run the referenced sequence and use the resulting words as input. 
This can be useful for defining daughter languages or branches without having to manually redefine the words.

``` diff
@gamma %alpha:
    "rules1" ~ {"Low Vowel Reduction"},

# The above sequence tagged "gamma" means:
# - Using the result from @alpha as input,
# - Apply "Low Vowel Reduction" from ./rules1.rsca
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
                       [-y | --overwrite] [-n | --no-overwrite] [-i | --output-all]
                       [-h | --help]
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
    -i          Output all intermediate steps.
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

```
-w      specify word file (.txt) as input
-r      specify rule file (.asca) to run
-o      specify path of output (.txt)
-c      compare output to a given file (.txt)
-h      this message

```


```
.asca format

project-file-tree
|
|- rules
|   |
|   |- rule1.asca
|   |- rule2.asca
|   |- rule3.asca
|   |- rule4.asca
|   |_ rule5.asca
|
|- family file



family file 

start: proto
rule1
rule2
rule3
    branch: SubFamily1
    rule4
    rule5
        branch: Sub1-1
        Rule9
        Rule10
        branch: Sub1-2
        Rule11
    branch: SubFamily2
    Rule6
    Rule7
    Rule8
        branch:Sub2-1
        Rule12



```

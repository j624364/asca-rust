
```
-w      specify word file (.wasca) as input
-r      specify rule file (.rasca) to run
-o      specify path of output (.txt)
-c      compare output to a given file (.wasca)
-h      this message

```


```
dir
|
|- x to y.rasca
|- y to z.rasca
|
|- config.asca
|
|- out
    |-configA
    |   |- 1-x_to_y
    |   |- 2-y_to_x
    |-configB
        |- 1-y_to_x
        |- 2-x_to_y
```

``` CONFIG.ASCA

@ configA
    x to y
    y to z
@ configB
    y to z
    x to y
```

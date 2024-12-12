
```
-w      specify word file (.wsca) as input
-r      specify rule file (.rsca) to run
-o      specify path of output (.txt)
-c      compare output to a given file (.wsca)
-h      this message

```


```
<dir>
├── out
│   ├── configA
│   │   ├── 1-x_to_y
│   │   └── 2-y_to_x
│   └── configB
│       ├── 1-y_to_x
│       └── 2-x_to_y
│
├── config.asca
├── words.wsca
├── x to y.rsca
└── y to z.rsca

```

``` CONFIG.ASCA

@ configA
    x to y
    y to z
@ configB
    y to z
    x to y
```

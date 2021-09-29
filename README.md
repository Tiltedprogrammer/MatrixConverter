# haskell2hardware

`stack build && stack run -- [params]`

`stack run -- [options] file
  -q[bool]  --qtree[=bool] Generate qtree representation of .mtx                      file, if 'bool' is specified Boolean qtree is dumped
`

`
  -m        --mask                     Generate a mask from .mtx
`

`
--backend=haskell|=poitin  Choose backend: haskell for Haskell, poitin for distiller language
`
### Examples
----
- `stack run -- -q=bool --backend=haskell data/test.mtx` will generate

  `(QNode (QNode (QVal True) QNone QNone (QVal True)) QNone QNone (QNode (QVal True) QNone QNone (QVal True)))` 

- `stack run -- -q --backend=haskell data/test.mtx` will yield

  `(QNode (QNode (QVal 1) QNone QNone (QVal 1)) QNone QNone (QNode (QVal 1) QNone QNone (QVal 1)))`

- `stack run -- -q --backend=poitin data/test.mtx` will yield 

  `QNode (QNode (QVal (1),QNone,QNone,QVal (1)),QNone,QNone,QNode (QVal (1),QNone,QNone,QVal (1)))`

- `stack run -- -q=bool --backend=poitin data/test.mtx `

  `QNode (QNode (QVal (True),QNone,QNone,QVal (True)),QNone,QNone,QNode (QVal (True),QNone,QNone,QVal (True)))`


### Notes
---

Only matrices of power of 2 size are supported. In case of .mtx format one could simple change rows and columns to be power of 2, e.g. for

```
%%MatrixMarket matrix coordinate integer general

5  5  8
    1     1   1
    2     2   1
    3     3   1
    1     4   6
    4     2   2
    4     4  -2
    4     5   3
    5     5   1

```

make 

```
%%MatrixMarket matrix coordinate integer general

8  8  8
    1     1   1
    2     2   1
    3     3   1
    1     4   6
    4     2   2
    4     4  -2
    4     5   3
    5     5   1

```
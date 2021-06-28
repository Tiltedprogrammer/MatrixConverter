# haskell2hardware

`stack build && stack run [params]`

`params := ["qtree"|"mask"] "path-to-file-relative-to-data-folder"`, where "qtree" generates qtree represenation and mask converts a matrix to a mask.

### Example
----
`stack build && stack run "qtree" "exampleQTree1.mtx"` will generate `QNode (QVal 1) (QVal 1) (QVal 5) QNone` 

and

`stack build && stack run "qtree" "exampleQTree1.mtx"` will yield 

`MNode MVal MVal MVal MNone` 


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
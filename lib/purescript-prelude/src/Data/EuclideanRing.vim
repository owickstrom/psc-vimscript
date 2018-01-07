let g:Data#EuclideanRing#intDegree = {x -> min(abs(x), 2147483647)}

let g:Data#EuclideanRing#intDiv = {x -> {y -> (x / y)}}

let g:Data#EuclideanRing#intMod = {x -> {y -> (x % y)}}

let g:Data#EuclideanRing#numDiv = {x -> {y -> (x / y)}}

#lang dcc019/let

let z = 5
   in let x = 3
      in let y = -(x,1)
         in let x = 4
            in -(z,-(x,y))
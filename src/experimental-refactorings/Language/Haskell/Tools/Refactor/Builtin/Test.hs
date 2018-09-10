module Test where

f = id
g = id

x = f (f (g 2))

h y = 2 * y * x

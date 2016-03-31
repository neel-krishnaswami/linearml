# Linear ML

This is a toy implementation of intuitionistic linear type theory. The
idea is to support IMELL -- intuitionistic multiplicative exponential
linear logic -- in a way that is convenient enough that some small but
interesting programs can be written in it. 

Basically, the idea is to enrich IMELL with labelled Cartesian
products (i.e., lazy records), labelled sums, recursive types, and
recursive functions, and integers. I am also adding polymorphism and
type definitions, along the lines of my 2013 paper with Joshua
Dunfield, [*Complete and Easy Bidirectional Typechecking for
Higher-Rank
Polymorphism*](http://www.cs.cmu.edu/~joshuad/papers/bidir/).

This should be enough to demonstrate some nontrivial programs. As
features get implemented I'll add them to the readme. 


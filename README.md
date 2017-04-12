# Linear ML

This is a small implementation of a linear type theory in the style of
the Benton-Wadler adjoint calculus
([*Linear logic, monads, and the lambda calculus*](http://homepages.inf.ed.ac.uk/wadler/topics/linear-logic.html#linearmonad)),
which shows how to extend ordinary functional programming with support 
for the connectives of linear logic.

In addition, the linear type theory has support for logical
connectives drawn from temporal logic. This gives a logical
interpretation for a reactive, event-based programming style, and so
the language should be compilable into Javascript in a way that
integrates cleanly with the event loop and DOM.

The language is intended to support polymorphism using an algorithm
extending the one in my 2013 paper with Joshua
Dunfield,
[*Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism*](http://www.cs.cmu.edu/%7Ejoshuad/papers/bidir/)



Pearls of functional algorithm design are written using the functional language Haskell. For those that want to learn Scala and functional programming, it is worthwhile to go through this book and try to code-up the individual pearls in Scala.
There are some significant differences between Haskell and Scala. I am going out point out here some of the most important differences.

List-comprehensions in Haskell => Scala has equivalent syntax in the for-yield statement, it behaves the same way, you can add if statements inside for expression and result is a single collection built up iteratively
Immutable array in Haskell => Scala's immutable array is represented by the Vector, be aware that Scala's Array is mutable and comes from Java. So use vector if you want immutable array.
Higher-order functions in Haskell => Scala has mostly higher-order methods, not functions.
Pattern matching in Scala is done through match and case keywords, obviously not as clean as in Haskell.
# Sized Lists

`elm-sized-list` implements a list that caches its size. This means `SizedList.length lst` runs in O(1) time, while `List.length lst` is an O(n) operation. All else behaves similarly.


## Pros/cons when compared with the regular List module

Use this module only if you are constantly reaching for the size of a list. See the list of pros and cons to decide if you should use regular list 

Pros:

* O(1) access to list length.

Cons:

* No specialized syntax.
* The need of constant wrapping, unwrapping for basic pattern matching.
* Wrapping regular lists require a possibly unnecessary computation of its length.
* Many algorithms require to use an unsafe operation, `unsafeFromList` to have good performance.
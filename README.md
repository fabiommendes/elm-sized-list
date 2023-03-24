# Tape for ELM 

`elm-sized-list` implements a list that caches its size. This means `SizedList.length lst` runs in O(1) time, while `List.length lst` is an O(n) operation. All else behaves similarly.

Use this module if you are constantly reaching for the size of a list.
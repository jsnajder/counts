counts
======

Data.Counts is a simple data structure for counting values (similar to Data.Multiset).

Example:

    > import Data.Counts
    >
    > let c = fromList "The quick brown fox jumps over the lazy dog"
    > countOf 'e' c
    > 3

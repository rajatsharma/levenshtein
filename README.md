## Lavenshtein

_Lavenshtein Distance in Haskell_

#### About

The Levenshtein distance is a string metric for measuring the difference between two sequences. Informally, the Levenshtein distance between two words is the minimum number of single-character edits (insertions, deletions or substitutions) required to change one word into the other.

#### Usage

```haskell
import Data.Lavenshtein (levenshtein)

-- gives 1
distance = levenshtein "levenshtein" "lavenshtein"
```

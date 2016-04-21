# wordcut-clj 

A word segmentation tool for ASEAN languages written in Clojure

## Example

     (:require [wordcut.tokenizer :refer :all])

### Khmer (Cambodian)

     ((khmer-tokenizer) "ភាសាខ្មែរ")

### Lao

     ((lao-tokenizer) "ພາສາລາວມີ")

### Thai

     ((thai-tokenizer) "ภาษาไทย")

## Leiningen

     [wordcut "0.1.2"]

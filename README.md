# wordcut-clj 

A word segmentation tool for ASEAN languages written in Clojure

## Example

### Khmer (Cambodian)

     ((tokenizer (read-default-khmer-dict)) "ភាសាខ្មែរ")

### Lao

     ((tokenizer (read-default-lao-dict)) "ພາສາລາວມີ")

### Thai

     ((tokenizer (read-default-thai-dict)) "ภาษาไทย")

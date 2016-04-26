<!-- README.md is generated from README.Rmd. Please edit that file -->
thainltk
--------

Thai National Language Toolkit in R

This package is intended to provide utility functions for Thai NLP. The first release includes a feature-based Thai word tokenizer, trained on publicly available BEST corpus. Additional functions may be added to later releases.

The tokenizer can be obtained from the factory function thaiTokenizer(). It accepts one parameter 'skipSpace'. If set to true, which is the default, the obtained tokenizer will filter out whitespaces before returning the character vector. This behavior should be desirable in common use cases.

``` r
library(thainltk)
tt <- thaiTokenizer()
tt('ทดสอบการแบ่งคำภาษาไทย')
#> [1] "ทดสอบ" "การ"   "แบ่ง"   "คำ"    "ภาษา"  "ไทย"
```

When multiple strings were supplied through a character vector of length more than one, the strings will be concatenated (pasted) with a new line character('\\n'), and tokenized. This behavior is designed to work with the readLines function. To tokenize multiple strings separately, use the usual lapply or related functions.

``` r
lines <- c('บรรทัดที่ 1', 'บรรทัดที่ 2')
tt(lines)
#> [1] "บรรทัด" "ที่"     "1"     "บรรทัด" "ที่"     "2"
lapply(lines, tt)
#> [[1]]
#> [1] "บรรทัด" "ที่"     "1"    
#> 
#> [[2]]
#> [1] "บรรทัด" "ที่"     "2"
```

The tokenizer uses a feature-based, discriminative approach. The used features do not include handling of different kinds of special strings, such as URL, emails, date time, etc. So the tokenizer may not handle those type of strings well. English alphabets are abstracted to one character class. The tokenizer should split English text at whitespace, but this is not guaranteed. As such, preprocessing of input text may be needed when the input text contains many non-Thai tokens. Otherwise, the tokenizer should handle normal Thai text without the need for preprocessing.

``` r
tt('ประโยคที่มีEnglishและไทยปนกัน')
#> [1] "ประโยค"  "ที่"       "มี"       "English" "และ"     "ไทย"     "ปน"     
#> [8] "กัน"
```

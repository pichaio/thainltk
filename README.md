# thainltk
Thai National Language Toolkit in R

This package is intended to provide utility functions for Thai NLP. The first release includes a feature-based Thai word tokenizer, trained on publicly available BEST corpus. Additional functions may be added to later releases. 

## The Tokenizer

The tokenizer can be obtained from the factory function thaiTokenizer(). It accepts one parameter 'skipSpace'. If set to true, which is the default, the obtained tokenizer will filter out whitespaces before returning the character vector. This behavior should be desirable in common use cases. 

When multiple strings were supplied through a character vector of length more than one, the strings will be concatenated (pasted) with a new line character('\\n'), and tokenized. This behavior is designed to work with the readLines function. To tokenize multiple strings separately, use the usual lapply or related functions. 

The tokenizer in this package was trained on the BEST corpus. The estimated F1 measure is around 0.978. The estimation was done on a holdout test set of 126 documents. Although the performance number cannot be compared directly with other models, the tokenizer in this package should be competitive with other modern tokenizers.

The algorithm behind the tokenizer is a linear SVM. More information can be obtained from the help of thaiTokenizer function.

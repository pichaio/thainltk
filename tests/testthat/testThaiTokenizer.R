context('Tokenize string')
testString <- stringi::stri_unescape_unicode("\\u0e17\\u0e14\\u0e2a\\u0e2d\\u0e1a\\u0e01\\u0e32\\u0e23\\u0e41\\u0e1a\\u0e48\\u0e07\\u0e04\\u0e33")
testStrings <- c(testString,
                 stringi::stri_unescape_unicode('\\u0e14\\u0e49\\u0e27\\u0e22\\u0e27\\u0e34\\u0e18\\u0e35\\u0e01\\u0e32\\u0e23\\u0e2b\\u0e25\\u0e32\\u0e01\\u0e2b\\u0e25\\u0e32\\u0e22'))

t1 <- thaiTokenizer()
t2 <- thaiTokenizer(skipSpace = F)

test_that('0-length string returns 0-length string', {
  expect_equal(t1(character(0)), character(0))
  expect_equal(t2(character(0)), character(0))
})

test_that('NA returns NA', {
  expect_equal(t1(NA), NA_character_)
  expect_equal(t2(NA), NA_character_)
})

test_that('Single characters returns the same', {
  expect_equal(t1("a"), "a")
  expect_equal(t1("."), ".")
  expect_equal(t2("a"), "a")
  expect_equal(t2("."), ".")
})

test_that('Not skiping space should return spaces', {
  expect_equal(t2('cat dog'), c('cat', ' ', 'dog'))
  expect_equal(t2('cat dog '), c('cat', ' ', 'dog', ' '))
  expect_equal(t2('cat\ndog'), c('cat', '\n', 'dog'))
  expect_equal(t2('cat\tdog'), c('cat', '\t', 'dog'))
})

test_that('Skiping space should return no space', {
  expect_equal(t1('cat dog'), c('cat', 'dog'))
  expect_equal(t1('cat dog '), c('cat', 'dog'))
  expect_equal(t1('cat\ndog'), c('cat', 'dog'))
  expect_equal(t1('cat\tdog'), c('cat', 'dog'))
})

test_that('Test string should be tokenized to several tokens',{
  expect_true(length(t1(testString)) > 3)
})

test_that('Tokenized and combined back should yield the original string', {
  expect_equal(paste0(t1(testString), collapse = ""), testString)
  expect_equal(paste0(t2(testString), collapse = ""), testString)
})

test_that('Tokenized multiple characters should be the same as joining them with a new line.',{
  expect_equal(t2(testStrings), t2(paste(testStrings[[1]], testStrings[[2]], sep = "\n")))
})

context('sbest dataset')

sbest <- unescapeSbest()

test_that('the number of document is 3',{
  expect_equal(length(sbest), 3)
})

test_that('Encoding is UTF-8',{
  expect_equal(Encoding(sbest[[1]]$content), 'UTF-8')
  expect_equal(Encoding(sbest[[2]]$content), 'UTF-8')
  expect_equal(Encoding(sbest[[3]]$content), 'UTF-8')
})

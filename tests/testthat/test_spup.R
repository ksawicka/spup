context("spup")


# dummy test to start writing tests
test_that("equality holds", {
    expect_equal(sin(pi / 4), 1 / sqrt(2))
    expect_warning(sqrt(-1))
})

test_that("warning emitted when acf0 outside (0,1) interval gives warning", {
	expect_warning(
	  crm <- makeCRM(acf0 = -0.8, range = 300, model = "Exp"),
		"For standardized residuals acf0 argument should be between 0 and 1."
	)
})

test_that("error returned when correlation matrix eigenvalues < 0 in defineMUM()", {
  a <- matrix(c(1,-0.7,0.2,0.7,1,0.5,0.2,0.5,1), nrow = 3, ncol = 3)
  expect_error(stopifnot(min(eigen(a)$values) > 0))
})

test_that("distribution sampling works", {
  set.seed(1234567)
  a <- distribution_sampling(5,"beta",c(0.5,0.6,2))
  expect_output(str(a), " num [1:5] 0.686 0.977 0.934 0.828 0.867", fixed = TRUE)
})

test_that("", {
  set.seed(1234567)
  a <- distribution_sampling_raster()
  a <- raster::overlay(parameters_stack, 
                       fun = function(shape1, shape2, ncp) Vectorize(rbeta(shape1, shape2, ncp)))
})


# # template
# test_that("", {
#   
# })



# # this should pass but R has problem with double quotation marks?
# test_that("makecrm() is deprecated", {
#   expect_warning(
#     crm <- makecrm(acf0 = 0.8, range = 300, model = "Exp"),
#     "'makecrm' is deprecated.\nUse 'makeCRM' instead.\nSee help("Deprecated")")
# })



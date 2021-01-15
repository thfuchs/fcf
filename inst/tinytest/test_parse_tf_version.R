suppressWarnings(suppressPackageStartupMessages(library(mockery)))

expect_silent(parse_tf_version())

# Mock to TF version 2.2
stub(parse_tf_version, "tensorflow::tf_version", "2.2")
expect_identical(parse_tf_version(), 2.2)

# Mock to "complex" TF version
stub(parse_tf_version, "tensorflow::tf_version", "20.201-1.3")
expect_identical(parse_tf_version(), 20.201)

stub(parse_tf_version, "tensorflow::tf_version", "20.201.1.3")
expect_identical(parse_tf_version(), 20.201)

stub(parse_tf_version, "tensorflow::tf_version", "0-2-1-3")
expect_identical(parse_tf_version(), 0.2)

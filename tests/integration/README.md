# Integration Tests

All integration tests for the _Hornvale_ (top-level) library should be placed within this folder:

>Each integration test results in a separate executable binary, and cargo test will run them serially. In some cases this can be inefficient, as it can take longer to compile, and may not make full use of multiple CPUs when running the tests. If you have a lot of integration tests, you may want to consider creating a single integration test, and split the tests into multiple modules. The libtest harness will automatically find all of the `#[test]` annotated functions and run them in parallel. You can pass module names to cargo test to only run the tests within that module.

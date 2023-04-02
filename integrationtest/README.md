#Integration testing

Run `make` to execute integration test. Podman installation required.

The test loads all index entries, generates asserts for its identifiers (eg. `(test-assert (procedure? list?))`), and runs such asserts for each library with specified implementation (as written in testlist.scm). The main intent is to catch typos.

FIXME: currently the tests fail, in part because of deficiencies of implementations. 

# Run all ERT test batteries
test:
    #!/usr/bin/env bash
    set -e
    for test_file in test-*.el; do
        echo "Running $test_file..."
        emacs --batch -Q -L . -l "$test_file" -f ert-run-tests-batch-and-exit
    done
    echo ""
    echo "All tests passed!"

# Run a specific test file
test-run name:
    emacs --batch -Q -L . -l "test-{{name}}.el" -f ert-run-tests-batch-and-exit

# List all available test files
test-list:
    @ls test-*.el 2>/dev/null || echo "No test files found"

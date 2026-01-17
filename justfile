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
    #!/usr/bin/env bash
    if [ -f "test-{{name}}.el" ]; then
        emacs --batch -Q -L . -l "test-{{name}}.el" -f ert-run-tests-batch-and-exit
    else
        echo "Test file test-{{name}}.el not found"
        exit 1
    fi

# List all available test files
test-list:
    @echo "Available ERT test files:"
    @ls -1 test-*.el 2>/dev/null | sed 's/^/  - /' || echo "  (none found)"

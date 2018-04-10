ci:
    cabal new-test
    yamllint stack.yaml
    yamllint .travis.yml
    hlint src test
    stack build --test --no-run-tests
    weeder .

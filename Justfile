# Sync all hpack files and build
build:
    @find . -name "package.yaml" -execdir hpack \;
    cabal build all

# Just sync hpack without building
sync:
    @find . -name "package.yaml" -execdir hpack \;

# Open a REPL for a specific sub-library (e.g., 'just repl foo')
repl package:
    @hpack {{package}}
    cabal repl {{package}}:internal

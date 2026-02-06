build:
    spago build

bundle:
    spago bundle

dev:
    spago build --watch

clean:
    rm -rf output/ dist/index.js

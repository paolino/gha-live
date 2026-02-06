build:
    spago build

bundle:
    spago bundle

dev:
    spago build --watch

format:
    purs-tidy format-in-place src/**/*.purs

lint:
    purs-tidy check src/**/*.purs

ci: lint build bundle

serve: bundle
    python3 -m http.server 10000 -d dist

restart: bundle
    -pkill -f 'http.server 10000'
    sleep 1
    python3 -m http.server 10000 -d dist

clean:
    rm -rf output/ dist/index.js
